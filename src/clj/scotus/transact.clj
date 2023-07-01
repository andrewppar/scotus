(ns scotus.transact
  (:require [scotus.database.add        :as dba]
            [scotus.database.assert-map :as am]
            [scotus.database.remove     :as dbr]
            [scotus.formula.formula     :as formula]
            [scotus.formula.semantic    :as semantic]
            [scotus.state               :as state]
            [scotus.transitive          :as transitive]
            [scotus.xnf                 :as xnf]))

(defn split-cnf [cnf]
  (cond (formula/junction? cnf)
        (reduce
         (fn [acc formula]
           (let [update-key (cond (formula/atomic? formula) :atomic
                                  (formula/negation? formula) :neg
                                  :else :rule)]
             (update acc update-key (fnil conj #{}) formula)))
         {}
         (formula/juncts cnf))
        (formula/atomic? cnf)
        {:atomic [cnf]}
        (formula/negation? cnf)
        {:neg [cnf]}
        :else
        (ex-info (format "Formula %s is not CNF" cnf)
                 {})))

(defn add-context-asserts [assertion-groups]
  (if-let [instances (get assertion-groups "instance")]
    (let [specs (->> instances
                     (filter
                      #(= (second (formula/args %)) "context"))
                     (map #(first (formula/args %)))
                     (map
                      (fn [context]
                        `["subcontext_of" "universal" ~context])))]
      (update assertion-groups "subcontext_of" (fnil concat []) specs))
    assertion-groups))

(defmulti add-rows
  {:arglists '([predicate context asserter negated? specs])}
  (fn [predicate _ _ _ _]
    predicate))

(defmethod add-rows :default
  [predicate context asserter negated? specs]
  (dba/add-rows predicate context asserter negated? specs))

(defmethod add-rows "arg_instance"
  [_ context asserter negated? specs]
  (let [contexts (transitive/subcontext context)]
    (apply am/merge-assert-maps
           (conj
            (mapv
             (fn [spec]
               (let [pred     (formula/predicate spec)
                     column   (formula/arg spec 1)
                     arg-type (formula/arg spec 2)]
                 (if (transitive/instance? arg-type "arg-type" contexts)
                   (dba/update-column-type pred column arg-type)
                   am/empty-assert-map)))
             specs)
            (dba/add-rows "arg_instance" context asserter negated? specs)))))

(defn add-assertions [formulas polarity context asserter]
  ;; we don't support rules yet
  (when (or (= polarity :atomic)
            (= polarity :neg))
    (let [atoms     (case polarity
                      :atomic formulas
                      :neg  (mapv formula/negatum formulas))
          groups   (add-context-asserts
                    (group-by formula/predicate atoms))
          negated? (case polarity :atomic false :neg true)]
      (reduce-kv
       (fn [acc predicate to-assert]
         (let [specs (mapv formula/args to-assert)]
           (am/merge-assert-maps
            acc
            (add-rows predicate context asserter negated? specs))))
       am/empty-assert-map
       groups))))


(defn assert-for-wtf!
  "Given a wtf error - add assertions to make
  the formula a wtf."
  [error context]
  (let [[well-formed? retry? new-error] (semantic/wtf-assert cnf context)
        assert-map                  (atom am/empty-assert-map)]
    (when (and (not retry?) (not well-formed?))
      (throw
       (ex-info "Cannot make assertion"
                {:caused-by new-error})))
    (when-not well-formed?
      (swap! assert-map am/merge-assert-maps
             (assert-for-wtf! new-error context)))
    (let [assert-specs (map
                        (fn [{:keys [formula justification]}]
                          (let [args (formula/maybe-nested-args formula)]

                            (dba/make-row-spec justification context args))))
          groups       (group-by formula/maybe-nested-predicate formula)]
      (reduce-kv
       (fn [result predicate specs]
         (am/merge-assert-maps
          result (dba/add-rows-fine-grained predicate false specs)))
       @assert-map
       groups))))

(defn assert!
  [formula asserter context &
   {:keys [with-semantic-check? assert-requirements?]
    :or {with-semantic-check? true assert-requirements? true}}]
  (let [cnf  (xnf/cnf formula)
        assert-map (atom {:assert-count 0})
        {:keys [atomic neg rule]} (split-cnf cnf)]
    (when with-semantic-check?
      (let [{:keys [well-formed? retry? error]}
            (semantic/wtf-assert cnf context)]
        (when (and (not well-formed?) retry?)
          (if assert-requirements?
            (->> context
                 (assert! (apply formula/and
                                 (map
                                  (fn [atomic]
                                    (formula/arg atomic 1))
                                  (formula/juncts error))) asserter)
                 (swap! assert-map am/merge-assert-maps))
            (throw
             (ex-info
              (format "Formula %s is not well-formed" cnf)
              {:caused-by error}))))))
    (state/with-refreshed-index
      (reduce
       (fn [acc {:keys [assert-count]}]
         (update acc :assert-count + assert-count))
       @assert-map
       [(add-assertions atomic :atomic context asserter)
        (add-assertions neg :neg  context asserter)
        ;; We don't support rules yet
        #_(add-assertions rule :rule context asserter)
        ]))))

(defn create-predicate!
  [predicate args asserter]
  (apply dba/create-table! predicate args)
  (assert! ["instance" predicate "predicate"] asserter "universal"))

(defmulti retract-instance!
  {:arglists '([formula contexts])}
  (fn [[_ _ retract-class] _]
    retract-class))

(defmethod retract-instance! "predicate"
  [formula contexts]
  (mapv
   ;; TODO: This is inefficient
   ;; TODO: This is also crappy since it does not
   ;; get rid of any other places the predicate might exist.
   ;; Probably it's better to have a `delete-predicate!` function
   ;; that does the expected cleanup
   (fn [context]
     (dbr/delete-rows "instance" context [(formula/args formula)] false))
   contexts)
  (dbr/drop-table! (second formula)))

(defmethod retract-instance! :default
  [formula contexts]
  (mapv
   ;; TODO: This is inefficient
   (fn [context]
     (dbr/delete-rows "instance" context [(formula/args formula)] false))
   contexts))

(defmulti retract-atom!
  {:arglists '([formula contexts])}
  (fn [formula _]
    (formula/predicate formula)))

(defmethod retract-atom! "instance"
  [formula contexts]
  (retract-instance! formula contexts))

(defmethod retract-atom! :default
  [[predicate & args] contexts]
  (map
   (fn [context]
     (dbr/delete-rows predicate context [args] false))
   contexts))

(defn retract! [formula contexts]
  (let [formula-type (formula/formula-type formula)]
    (if (contains? #{:atomic :neg} formula-type)
      (let [predicate (formula/literal-predicate formula)
            args      [(formula/literal-args formula)]
            negated?  (= formula-type :neg)]
        (if negated?
          (map
           (fn [context]
             (dbr/delete-rows predicate context [args] negated?))
           contexts)
          ;; TODO: make this output look like the output of assert!
          (retract-atom! formula contexts)))
      ;; We don't support rules yet
      nil)))

(defn delete-predicate!
  [predicate]
  (retract! ["instance" predicate "predicate"] ["universal"]))



(comment
  (assert! (formula/and
            ["subclass_of" "human" "person"])
           "anparisi" "universal")

  (assert! ["transitive_arg" "subclass_of" "superclass" "subclass_of" "subclass" "superclass"] "anparisi" "universal")
  (assert! ["transitive_arg" "instance" "class" "subclass_of" "subclass" "superclass"] "anparisi" "universal")

  )
