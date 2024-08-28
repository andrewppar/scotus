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
  (let [query-pred (f/literal-predicate query)
        pred (if (f/variable? query-pred)
               (first (map namespace (keys spec)))
               query-pred)
        args (f/args (if (f/negation? query) (f/negatum query) query))
        pred-arg-names (state/table-args pred)
        args-arg-name->query-value (zipmap pred-arg-names args)
        arg-name->query-value (if (f/variable? query-pred)
                                (assoc args-arg-name->query-value pred query-pred)
                                args-arg-name->query-value)]
    (reduce-kv
     (fn [result arg-name query-value]
       (let [spec-value (lookup spec arg-name)]
         (if (f/variable? query-value)
           (if (= query-value query-pred)
             (assoc result query-value pred)
             (assoc result query-value spec-value))
           result)))
     (if justification? {:justification [(->formula spec)]} {})
     arg-name->query-value)))
