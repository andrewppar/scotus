(ns scotus.natural-language.database.index
  (:import (org.apache.lucene.store NIOFSDirectory)
           (org.apache.lucene.analysis.standard StandardAnalyzer)
           (org.apache.lucene.analysis.ngram NGramTokenFilter)
           (org.apache.lucene.analysis.custom CustomAnalyzer)
           (org.apache.lucene.index
            DirectoryReader IndexWriterConfig IndexWriter)
           (org.apache.lucene.search IndexSearcher)
           java.nio.file.Paths))

(defn store [location]
  (NIOFSDirectory. (Paths/get "." (into-array [location]))))

(defn close [store]
  (when store
    (.close store)))

(defn analyzer []
  (.build
   (doto (CustomAnalyzer/builder)
     (.withTokenizer  "standard" (into-array String []))
     (.addTokenFilter "lowercase" (into-array String []))
     (.addTokenFilter "stop" (into-array String []))
     (.addTokenFilter "englishMinimalStem" (into-array String [])))))

(defn writer
  "The `with-writer` macro should always be used since it does the right cleanup."
  [store analyzer]
  (loop [iteration 0]
    (let [writer (try
                     (IndexWriter. store (IndexWriterConfig. analyzer))
                     (catch Exception e nil))]
      (cond writer writer
            (> iteration 5) (throw
                             (ex-info "Could not get write access to store"
                                      {}))
            :else
            (do
              (Thread/sleep 5000)
              (recur (inc iteration)))))))

(defmacro with-writer [[var {:keys [store analyzer]}] & body]
  "Create a writer from `_spec`, do `body`, and close it.
  Where `_spec` is a vector whose first element is a varaible
  for the writer, and whose second element is a map specifying
  the store and analyzer to create the writer with."
  `(let [~var (writer ~store ~analyzer)]
     (try
       (do ~@body)
       (catch Exception e# (throw e#))
       (finally
         (doto ~var
           (.commit)
           (.close))))))

(defn searcher [store]
  (IndexSearcher. (DirectoryReader/open store)))
