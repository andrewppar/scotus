(ns scotus.query.query
  (:require
   [scotus.query.canon :as canon]
   [scotus.query.literal :as literal]
   [scotus.syntax.formula :as f]))


(defn query-conjunction [context justification? conjunction]
  (reduce
   (fn [result literal]
     (literal/query
      literal result :context context :justification? justification?))
   {}

   (canon/sort-conjuncts
    (if (f/literal? conjunction)
      [conjunction]
      (f/args conjunction)))))

(defn query
  [query &
   {:keys [context justification?] :or
    {context "universal"
     justification? false}}]
  (let [normalized-query (canon/dnf query)]
    (cond
      (f/literal? normalized-query)
      (literal/query normalized-query #{}
                     :context context :justification? justification?)

      (f/conjunction? normalized-query)
      (query-conjunction context justification? normalized-query)

      :else
      (->> normalized-query
           f/args
           (map
            (partial query-conjunction context justification?))
           (reduce into #{})))))
