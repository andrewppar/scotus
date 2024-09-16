(ns scotus.database.assertion
  (:require
   [honey.sql.helpers :as h]
   [scotus.database.utils :as utils]))

(defn create-predicate-lookup-table []
  (-> (h/create-table :assertion_predicate_lookup)
      (h/with-columns [[:id :uuid] [:predicate [:varchar 500]]])
      utils/execute!))

(defn add-lookup-rows [assertion-ids predicate]
  (let [rows (map
              (partial assoc {:predicate predicate} :id)
              assertion-ids)]
    (-> (h/insert-into :assertion_predicate_lookup)
        (h/values rows)
        (h/on-conflict :id)
        h/do-nothing
        utils/execute!)))
