(ns scotus.natural-language.database.query
  (:require
   [scotus.natural-language.database.index :as index]
   [scotus.database.utils :as utils]
   [scotus.state :as state])
  (:import
   (org.apache.lucene.index Term)
   (org.apache.lucene.search
    IndexSearcher BooleanClause$Occur BooleanQuery$Builder ScoreDoc TopDocs TermQuery)))

(defn builder-add [builder term-query must?]
  (.add builder term-query
        (if must? BooleanClause$Occur/MUST BooleanClause$Occur/SHOULD)))

(defn term-query [key value]
  (TermQuery. (Term. key (format "%s" value))))

(defn add-disjunction-terms [builder term values]
  (reduce
   (fn [builder* value]
     (doto builder*
       (builder-add (term-query term value) false)))
   builder
   values))

(defn ->named-key [predicate key]
  (if (contains? #{:score :id} key)
    key
    (keyword predicate (name key))))

(defn ->document
  [searcher score-doc]
  (let [raw-results (reduce
                     (fn [acc field]
                       (let [field-key (utils/to-keyword (.name field))]
                         (assoc acc field-key (.stringValue field))))
                     {:score (.score score-doc)}
                     (.getFields (.doc searcher (.doc score-doc))))
        predicate (get raw-results :predicate)]
    (dissoc
     (update-keys
      raw-results
      (partial ->named-key predicate))
     :predicate)))


(defn score-search-results
  [searcher search-results]
  (map
   (partial ->document searcher)
   (.scoreDocs search-results)))

(defn lookup-assertion
  [assertion-id predicate]
  (let [query (.build
               (doto (BooleanQuery$Builder.)
                 (builder-add (term-query "id" assertion-id) true)
                 (builder-add (term-query "predicate" predicate) true)))
        searcher (index/searcher (state/nlp-store))]
    (score-search-results searcher (.search searcher query 10000))))

(defn lookup [pred contexts specs]
  (let [args (state/table-args pred)
        spec-maps (map (partial zipmap args) specs)
        grouped-args (assoc
                      (reduce
                       (fn [acc m]
                         (reduce-kv
                          (fn [acc* k v]
                            (update acc* k (fnil conj #{}) v))
                          acc m))
                       spec-maps)
                      :context contexts)
        nlp-args (state/table-nlp-args pred)
        query (.build
               (reduce-kv
                (fn [builder term values]
                  (builder-add (disjoint-term-query term values) true))
                (doto (BooleanQuery$Builder.)
                  (builder-add (term-query "predicate" predicate) true))
                grouped-args))



        ]


    ))



 {}
 [{:one 1 :two 2} {:one 2 :two 3}])
