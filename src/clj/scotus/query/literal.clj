(ns scotus.query.literal
  (:require
   [clojure.string :as string]
   [scotus.database.query :as dbq]
   [scotus.query.assert-spec :as assert-spec]
   [scotus.query.binding :as binding]
   [scotus.query.resolution :as resolution]
   [scotus.semantic.literal :as lit]
   [scotus.semantic.resolution :as r]
   [scotus.syntax.formula :as f]
   [scotus.logic.closure :as cl]))

;;; Simple Lookup

(defn args->spec [args]
  (mapv (fn [arg] (when-not (f/variable? arg) arg)) args))

;; needs to filter bindings
(defn simple-lookup
  [bindings original-formula context justification?]
  (let [negated? (f/negation? original-formula)
        subformula (if negated? (f/negatum original-formula) original-formula)
        formulas  (binding/formula-apply-all subformula bindings)
        specs (map (comp args->spec f/args) formulas)
        contexts (r/->context context)
        pred (f/predicate subformula)
        base-results (if (f/variable? pred)
                       (dbq/lookup-asserts-for-all-preds contexts negated? specs)
                       (dbq/lookup-rows pred contexts negated? specs))]
    (->> base-results
         (map
          (fn [assert]
            (assert-spec/->binding
             original-formula assert :justification? justification?)))
         (binding/extend-all-filtering bindings)
         set)))

;;; Transitivity

(defn get-transitive-asserts [formula context]
  (let [pred (f/predicate formula)]
    (if (f/variable? pred)
      '()
      (let [spec [[pred nil nil nil nil]]]
        (seq (dbq/lookup-rows "transitive_arg" [context] false spec))))))

(defn ^:private bindings-for-upward-closure
  [db-result
   query
   arg-name
   to-arg
   justification?
   closure]
  (let [start-binding (assert-spec/->binding
                       query db-result :justification? justification?)]
    (conj
     (map-indexed
      (fn [idx assert-spec]
        (let [new-value (assert-spec/lookup assert-spec to-arg)
              new-spec (assert-spec/put db-result arg-name new-value)
              new-binding (assert-spec/->binding query new-spec)]
          (if justification?
            (let [justification-specs (conj (subvec closure 0 (inc idx)) db-result)
                  justification (mapv assert-spec/->formula justification-specs)]
              (assoc new-binding :justification justification))
            new-binding)))
      closure)
     (assert-spec/->binding query db-result :justification? justification?))))

(defn transitive-up
  [bindings
   query
   {:transitive_arg/keys [arg_name transitive_pred from_arg to_arg]}
   context
   justification?]
  (let [pred (f/literal-predicate query)
        db-results (->> bindings
                        (binding/formula-apply-all query)
                        (map (comp args->spec f/literal-args))
                        (dbq/lookup-rows pred (r/->context context) false))
        start-args (map
                    (fn [spec] (assert-spec/lookup spec arg_name))
                    db-results)
        closures (cl/closure start-args transitive_pred from_arg to_arg :context context)
        from-key (keyword transitive_pred from_arg)
        arg->closures (group-by
                       (fn [closure] (get (first closure) from-key))
                       closures)]
    (reduce
     (fn [result spec]
       (let [arg (assert-spec/lookup spec arg_name)
             spec-closures (get arg->closures arg)]
         (into result (mapcat (partial bindings-for-upward-closure
                                    spec query arg_name to_arg justification?)
                           spec-closures))))
     #{}
     db-results)))

(defn ^:private binding-for-assert-spec
  [query
   closures
   {:transitive_arg/keys [arg_name transitive_pred from_arg to_arg]}
   justification?
   db-result]
  (let [from-key (keyword transitive_pred from_arg)
        to-match (assert-spec/lookup db-result arg_name)]
    (or
     (some
      (fn [closure]
        (let [justification (->> to-match
                                 (cl/slice closure from-key)
                                 (map assert-spec/->formula)
                                 vec)]
          (when (seq justification)
            (let [start-arg (assert-spec/lookup (first closure) to_arg)
                  arg-key (keyword (f/literal-predicate query) arg_name)
                  updated-db-spec (assoc db-result arg-key start-arg)
                  binding  (assert-spec/->binding query updated-db-spec)]
              (if justification?
                (assoc binding
                       :justification
                       (conj justification
                             (assert-spec/->formula db-result)))
                binding)))))
      closures)
     (cond-> (assert-spec/->binding query db-result)
       justification? (update :justification (fnil conj [])
                              (assert-spec/->formula db-result))))))

(defn transitive-down
  [bindings
   query
   query-args
   {:transitive_arg/keys [arg_name transitive_pred from_arg to_arg] :as assert}
   context
   justification?]
  (let [pred (f/literal-predicate query)
        queries (binding/formula-apply-all query bindings)
        closures (cl/closure query-args transitive_pred to_arg from_arg :context context)
        query-arg->closure (group-by
                            (fn [closure]
                              (assert-spec/lookup (first closure) to_arg))
                            closures)
        new-specs (reduce
                   (fn [result query-lit]
                     (let [query-spec (args->spec (f/literal-args query-lit))]
                       (conj (->> (lit/arg-get query-lit arg_name)
                                  (get query-arg->closure)
                                  ;; transduce or a single map?
                                  (mapcat (partial cl/map-arg from_arg))
                                  (map (partial lit/put query-lit arg_name))
                                  (map (comp args->spec f/literal-args))
                                  (into result))
                             query-spec)))
                   #{}
                   queries)
        ;; why are we dointg a db lookup here again? If you remember or
        ;; figure it out write it down here
        new-results (dbq/lookup-rows pred (r/->context context) false new-specs)]
    (->> new-results
         (mapv
          (partial
           binding-for-assert-spec query closures assert justification?))
         (binding/extend-all-with-all bindings)
         (map (fn [binding]
                (if (get binding :justification)
                  (update binding :justification set)
                  binding))))))

(defn transitivity
  [bindings query transitive-asserts context justification?]
  (let [queries (binding/formula-apply-all query bindings)]
    (set
     (reduce
      (fn [result {:transitive_arg/keys [arg_name] :as assert}]
        (let [query-args (map
                          (fn [literal] (lit/arg-get literal arg_name))
                          queries)]
          ;; Does using result here mean that different order of transitive asserts
          ;; could mean different results?
          (cond (every? f/variable? query-args)
                (transitive-up result query assert context justification?)

                (every? (complement f/variable?) query-args)
                (transitive-down result query query-args assert context justification?)

                :else
                (throw
                 (ex-info (format "Bindings mismatch on %s" query-args)
                          {:caused-by bindings})))))
      bindings
      transitive-asserts))))

;;; These should have their own namespace

(defn ^:private add-reflexivity [predicate justification? binding]
  (let [binding-length (count binding)
        justification (get binding :justification)
        values (dissoc binding :justification)]
    (conj
     (map
      (fn [value]
        (let [new-binding (update-vals binding (fn [_] value))]
          (if justification?
            (assoc new-binding
                   :justification (->> value
                                       (repeat binding-length)
                                       (into [predicate])
                                       (conj justification)))
            new-binding)))
      (vals values))
     ;; should the justification on the old binding be updated?
     binding)))

(defn resolve-subclass-of [bindings formula context justification?]
  (let [args (f/args formula)]
    (if (not= (count args) 2)
      (throw (ex-info "malformed query literal (how'd this get here?)"
                      {:caused-by formula}))
      (let [transitive-asserts (get-transitive-asserts formula context)
            new-bindings (transitivity
                          bindings formula transitive-asserts context justification?)]
        (if (and (f/variable? (first args))
                 (f/variable? (second args)))
          (set (mapcat (partial add-reflexivity "subclass_of" justification?) new-bindings))
          (let [var (some (fn [arg] (when (f/variable? arg) arg)) args)
                value (some (fn [arg] (when-not (f/variable? arg) arg)) args)
                new-justification ["subclass_of" value value]
                new-binding (if justification?
                              {var value :justification #{new-justification}}
                              {var value})
                result-bindings (conj new-bindings new-binding)]
            (set
             (if justification?
               (map
                (fn [binding]
                  (update binding :justification conj
                          new-justification))
                result-bindings)
               result-bindings))))))))

(defn get-resolution-fn [formula]
  (let [pred (f/predicate formula)]
    nil
    (cond (= pred "subclass_of")
          #'resolve-subclass-of
          :else nil)))

(defn query
  [atomic-formula bindings &
   {:keys [context justification?]
    :or {context "universal" justification? false}}]
  (if (resolution/? atomic-formula bindings)
    ;; Resolution Module Available
    (resolution-fn bindings atomic-formula context justification?)
    (if-let [transitive-asserts (seq (get-transitive-asserts atomic-formula context))]
      ;; Some Formula args are transitive
      (transitivity bindings atomic-formula transitive-asserts context justification?)
      ;; Just see if we find anything
      (simple-lookup bindings atomic-formula context justification?))))
