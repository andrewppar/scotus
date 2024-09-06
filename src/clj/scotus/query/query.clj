(ns scotus.query.query
  (:require
   [scotus.query.canon :as canon]
   [scotus.query.binding :as binding]
   [scotus.query.literal :as literal]
   [scotus.syntax.formula :as f]))


(defn query-conjunction [context justification? conjunction]
  (reduce
   (fn [result literal]
     (let [new-results (literal/query
                        literal result
                        :context context
                        :justification? justification?)]
       (if (= new-results #{})
         (reduced new-results)
         new-results)))
   #{}
   (canon/sort-conjuncts
    (if (f/literal? conjunction)
      [conjunction]
      (f/args conjunction)))))

(defn query-internal
  [query context justification?]
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

(defn query
[query &
   {:keys [context justification?] :or
    {context "universal"
     justification? false}}]
  (->> (query-internal query context justification?)
       (mapv binding/remove-unwanted-vars)
       set))
