(ns scotus.transact
  (:require [scotus.database         :as db]
            [scotus.formula.formula  :as formula]
            [scotus.formula.semantic :as semantic]
            [scotus.xnf              :as xnf]))

(defn split-cnf [cnf]
  (cond (formula/junction? cnf)
        (reduce
         (fn [acc formula]
           (let [update-key (cond (formula/atomic? formula) :atomic
                                  (formula/negation? formula) :not
                                  :else :rule)]
             (update acc update-key (fnil conj #{}) formula)))
         {}
         (formula/juncts cnf))
        (formula/atomic? cnf)
        {:atomic [cnf]}
        (formula/negation? cnf)
        {:not [cnf]}
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
                        `["subcontext_of" ~context "universal"])))]
      (update assertion-groups "instance" concat specs))
    assertion-groups))


(defn add-assertions [formulas polarity context asserter]
  ;; we don't support rules yet
  (when (or (= polarity :atomic)
            (= polarity :not))
    (let [atoms     (case polarity
                      :atomic formulas
                      :not  (mapv formula/negatum formulas))
          groups   (add-context-asserts
                    (group-by formula/predicate atoms))
          negated? (case polarity :atomic false :not true)]
      (reduce-kv
       (fn [_ predicate to-assert]
         (let [specs    (mapv formula/args to-assert)]
           (db/add-rows predicate context asserter negated? specs)))
       nil
       groups))))

(defn merge-assert-maps [map-one map-two]
  (let [old-count (get map-one :assert-count)
        new-count (get map-two :assert-count)]
    {:assert-count (+ old-count new-count)}))

(defn assert!
  [formula asserter context &
   {:keys [with-semantic-check? assert-requirements?]
    :or {with-semantic-check? true assert-requirements? true}}]
  (let [cnf  (xnf/cnf formula)
        assert-map (atom {:assert-count 0})
        {:keys [atomic not rule]} (split-cnf cnf)]
    (when with-semantic-check?
      (let [{:keys [well-formed? error]}
            (semantic/well-formed-assert cnf context)]
        (when-not well-formed?
          (if (and assert-requirements?
                   (formula/conjunction? error))
            (->> context
                 (assert! (apply formula/and
                                 (map
                                  (fn [atomic]
                                    (formula/arg atomic 1))
                                  (formula/juncts error))) asserter)
                 (swap! assert-map merge-assert-maps))
            (throw
             (ex-info
              (format "Formula %s is not well-formed" cnf)
              {:caused-by error}))))))
    (reduce
     (fn [acc item]
       (if item
         (update acc :assert-count
                 + (get (first (first item)) :next.jdbc/update-count))
         acc))
     @assert-map
     [(add-assertions atomic :atomic context asserter)
      (add-assertions not :not  context asserter)
      ;; We don't support rules yet
      #_(add-assertions rule :rule context asserter)
      ])))

(defn create-predicate!
  [predicate args asserter]
  (apply db/create-table! predicate args)
  (assert! ["instance" predicate "predicate"] asserter "universal"))

(defmulti retract-instance!
  {:arglists '([formula contexts])}
  (fn [[_ _ retract-class] _]
    retract-class))

(defmethod retract-instance! "predicate"
  [formula contexts]
  (mapv
   ;; TODO: This is inefficient
   (fn [context]
     (db/delete-rows "instance" context [(formula/args formula)] false))
   contexts)
  (db/drop-table! (second formula)))

(defmethod retract-instance! :default
  [formula contexts]
  (mapv
   ;; TODO: This is inefficient
   (fn [context]
     (db/delete-rows "instance" context [(formula/args formula)] false))
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
     (db/delete-rows predicate context [args] false))
   contexts))

(defn retract! [formula contexts]
  (let [formula-type (formula/formula-type formula)]
    (if (contains? #{:atomic :not} formula-type)
      (let [predicate (formula/literal-predicate formula)
            args      [(formula/literal-args formula)]
            negated?  (= formula-type :not)]
        (if negated?
          (map
           (fn [context]
             (db/delete-rows predicate context [args] negated?))
           contexts)
          ;; TODO: make this output look like the output of assert!
          (retract-atom! formula contexts)))
      ;; We don't support rules yet
      nil)))

(comment
  (assert! (formula/and
            ["subclass_of" "human" "person"])
           "anparisi" "universal")

  (assert! ["transitive_arg" "subclass_of" "superclass" "subclass_of" "subclass" "superclass"] "anparisi" "universal")
  (assert! ["transitive_arg" "instance" "class" "subclass_of" "subclass" "superclass"] "anparisi" "universal")

  )
