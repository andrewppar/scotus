(ns scotus.query.spec
  (:require
   [scotus.state :as state]
   [scotus.semantics.literal :as literal]
   [scotus.syntax.formula :as f]))

(defn lookup
  "Get the item at `pred/column` in `spec`."
  [spec pred column]
  (get spec (keyword pred column)))

(defn ->formula
  "Convert `assert-spec` into a formula, i.e. a vector whose first
  element is `assert-spec`s predicate and whose other elements are
  its values. "
  [assert-spec]
  (let [predicate (namespace (first (keys assert-spec)))]
    (->> (state/table-args predicate)
         (mapv
          (fn [arg]
            (get assert-spec (keyword predicate arg))))
         (into [predicate]))))

(defn ->binding
  "Convert `spec` into a binding for `query`.

   Each variable in `query` is bound to the corresponding value in
   `spec`.

   If `justification?` is non-nil then the vector containing `spec`
   is used as the value for the `:justification` keyword."
  [query spec & {:keys [justification?]}]
  (let [pred (f/literal-predicate query)
        args (f/args (if (f/negation? query) (f/negatum query) query))
        pred-arg-names (state/table-args pred)
        arg-name->query-value (zipmap pred-arg-names args)]
    (reduce-kv
     (fn [result arg-name query-value]
       (let [spec-value (lookup spec pred arg-name)]
         (if (f/variable? query-value)
           (assoc result query-value spec-value)
           result)))
     (if justification? {:justification [(->formula spec)]} {})
     arg-name->query-value)))
