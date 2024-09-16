(ns scotus.natural-language.database.add
  (:require
   [scotus.database.assertion :as assertion]
   [scotus.database.justification :as justification]
   [scotus.database.utils :as utils]
   [scotus.natural-language.database.index :as index]
   [scotus.state :as state])
  (:import (org.apache.lucene.document
            Document TextField StringField Field$Store FieldType)))

(defn field [field-name value field-type]
  (case field-type
    :string (StringField. field-name value Field$Store/YES)
    :text (TextField. field-name value Field$Store/YES)))

(defn assert-spec->doc
  [asserter context assert-spec]
  (let [table (first assert-spec)
        arg-name->arg (->> assert-spec
                           rest
                           (zipmap (state/table-args table)))
        id (utils/->assertion-id (update-keys arg-name->arg utils/to-keyword) context)
        text-args (set (state/table-nlp-args table))]
    (reduce-kv
     (fn [doc arg-name arg]
       (let [arg-type (if (contains? text-args arg-name) :text :string)]
         (doto doc
           (.add (field arg-name arg arg-type)))))
     (doto (Document.)
       (.add (field "id" (format "%s" id) :string))
       (.add (field "asserter" asserter :string))
       (.add (field "context" context :string))
       (.add (field "predicate" (first assert-spec) :string)))
     arg-name->arg)))

;; TODO: negation here?
(defn add-docs
  "Add a documents for `assert-specs` by `asserter` in `context`."
  [assert-specs asserter context]
  (index/with-writer [writer {:store (state/nlp-store)
                              :analyzer (state/nlp-analyzer)}]
    (let [docs (map
                (partial assert-spec->doc asserter context)
                assert-specs)
          predicate->ids (reduce
                          (fn [acc doc]
                            (let [id (parse-uuid (.stringValue (.getField doc "id")))
                                  pred  (.stringValue (.getField doc "predicate"))]
                              (update acc pred (fnil conj #{}) id)))
                          {}
                          docs)
          ids (reduce-kv
               (fn [acc pred ids]
                 (assertion/add-lookup-rows ids pred)
                 (into acc ids))
               #{}
               predicate->ids)]
      (justification/add ids asserter)
      {:assert-count
      (->> docs
         (mapv (fn [doc] (.addDocument writer doc)))
         (apply +))})))
