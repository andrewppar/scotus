(ns scotus.query.assert-spec
  (:require
   [scotus.state :as state]
   [scotus.syntax.formula :as f]))

(defn lookup [spec column]
  (let [pred (-> spec keys first namespace)]
    (get spec (keyword pred column))))

(defn put [spec column value]
  (let [pred (-> spec keys first namespace)]
    (assoc spec (keyword pred column) value)))

(defn ->formula [assert-spec]
  (let [predicate (namespace (first (keys assert-spec)))]
    (->> (state/table-args predicate)
         (mapv
          (fn [arg]
            (get assert-spec (keyword predicate arg))))
         (into [predicate]))))

(defn ->binding [query spec & {:keys [justification?]}]
  (let [pred (f/literal-predicate query)
        args (f/args (if (f/negation? query) (f/negatum query) query))
        pred-arg-names (state/table-args pred)
        arg-name->query-value (zipmap pred-arg-names args)]
    (reduce-kv
     (fn [result arg-name query-value]
       (let [spec-value (lookup spec arg-name)]
         (if (f/variable? query-value)
           (assoc result query-value spec-value)
           result)))
     (if justification? {:justification [(->formula spec)]} {})
     arg-name->query-value)))
