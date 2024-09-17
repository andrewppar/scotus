(ns scotus.query.assert-spec
  (:require
   [scotus.state :as state]
   [scotus.syntax.formula :as f]))

(defn predicate [spec]
  (->> spec
       keys
       (some (fn [k] (when-let [ns (namespace k)] ns)))))

(defn lookup [spec column]
  (get spec (keyword (predicate spec) column)))

(defn put [spec column value]
  (assoc spec (keyword (predicate spec) column) value))

(defn ->formula [assert-spec]
  (let [pred (predicate assert-spec)]
    (->> (state/table-args pred)
         (mapv
          (fn [arg]
            (get assert-spec (keyword pred arg))))
         (into [pred]))))

(defn ->binding [query spec & {:keys [justification?]}]
  (let [query-pred (f/literal-predicate query)
        pred (if (f/variable? query-pred)
               (predicate spec)
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
