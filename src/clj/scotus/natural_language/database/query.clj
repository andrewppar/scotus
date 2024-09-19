(ns scotus.natural-language.database.query
  (:require
   [scotus.natural-language.database.index :as index]
   [scotus.database.utils :as utils]
   [scotus.state :as state])
  (:import
   (org.apache.lucene.index Term)
   (org.apache.lucene.queryparser.classic QueryParser)
   (org.apache.lucene.search
    FuzzyQuery
    #_IndexSearcher
    BooleanClause$Occur BooleanQuery$Builder #_ScoreDoc #_TopDocs
    TermQuery)))

(defn builder-add [builder query must?]
  (.add builder query
        (if must? BooleanClause$Occur/MUST BooleanClause$Occur/SHOULD)))

(defn term-query [key value]
  (TermQuery. (Term. key (format "%s" value))))

(defn parsed-query [term value analyzer]
  (.parse (QueryParser. term analyzer) value))

(defn disjunction-query [term values]
  (.build
   (reduce
    (fn [builder value]
        (doto builder
          (builder-add (term-query term value) false)))
    (BooleanQuery$Builder.)
    values)))

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

(defn lookup-spec [pred contexts spec]
  (let [analyzer (state/nlp-analyzer)
        nlp-args (set (state/table-nlp-args pred))
        context-subquery (.build
                          (reduce
                           (fn [builder context] (builder-add builder (term-query "context" context) false))
                           (BooleanQuery$Builder.)
                           contexts))
        query (.build
               #_(doto (BooleanQuery$Builder.)
                   (builder-add (term-query "predicate" pred) true)
                   (builder-add context-subquery true))
               (reduce-kv
                (fn [builder term value]
                  (if (nil? value)
                    builder
                    (if (contains? nlp-args term)
                      (builder-add builder (parsed-query term value analyzer) true)
                      (builder-add builder (term-query term value) true))))
                (doto (BooleanQuery$Builder.)
                  (builder-add (term-query "predicate" pred) true)
                  (builder-add context-subquery true))
                (zipmap (state/table-args pred) spec)))
        searcher (index/searcher (state/nlp-store))]
    (->>
     (.search searcher query 100000)
     (score-search-results searcher)
     (sort-by :score >=)
     #_(take 5))))






(defn ^:private merge-original [args spec results]
  (map
   (fn [result]
     (reduce-kv
      (fn [acc term original-value]
        (if (nil? original-value)
          acc
          (assoc acc (keyword "original" term) original-value)))
      result
      (zipmap args spec)))
   results))

(defn lookup [pred contexts specs]
  (let [args (state/table-args pred)]
    (reduce into #{}
            (map
             (fn [spec]
               (merge-original args spec (lookup-spec pred contexts spec)))
             specs))))
