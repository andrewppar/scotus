(ns scotus.query.literal
  (:require
   [scotus.database.query :as dbq]
   [scotus.query.assert-spec :as assert-spec]
   [scotus.query.binding :as binding]
   [scotus.query.resolution :as resolution]
   [scotus.query.transitivity :as transitivity]
   [scotus.heuristic.query :as hq]
   [scotus.semantic.resolution :as r]
   [scotus.syntax.formula :as f]))

;;; These should have their own namespace

(defn resolve-formula [bindings formula context justification?]
  (reduce
   (fn [acc {:keys [resolve-fn]}]
     (resolve-fn formula acc context justification?))
   bindings
   (resolution/get-fns formula bindings)))

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
                       ;; should this inlude nlp preds? probably not (for now)
                       (dbq/lookup-asserts-for-all-preds contexts negated? specs)
                       (hq/lookup-rows pred contexts negated? specs))]
    (->> base-results
         (map
          (fn [assert]
            (assert-spec/->binding
             original-formula assert :justification? justification?)))
         (binding/expand-all-filtering bindings)
         set)))

(defn query
  [atomic-formula bindings &
   {:keys [context justification?]
    :or {context "universal" justification? false}}]
  (if (resolution/? atomic-formula bindings)
    ;; Resolution Module Available
    (resolve-formula bindings atomic-formula context justification?)
    (if-let [transitive-asserts (seq (transitivity/get-transitive-asserts atomic-formula context))]
      ;; Some Formula args are transitive
      (transitivity/transitivity bindings atomic-formula transitive-asserts context justification?)
      ;; Just see if we find anything
      (simple-lookup bindings atomic-formula context justification?))))
