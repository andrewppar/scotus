(ns scotus.database.remove
  (:require
   [honey.sql             :as sql]
   [honey.sql.helpers     :as h]
   [scotus.assert         :as assert]
   [scotus.database.utils :as utils]))

(defn drop-table!
  "Delete a table from the database."
  [table]
  (->> table
       utils/to-keyword
       h/drop-table
       sql/format
       utils/execute!))

(defn delete-rows
  "Given a vector of row-specifications, a table, and a context,
  delete the corresponding rows from the table."
  [table context row-specs negated?]
  (let [ids (map (fn [spec] (assert/id table spec context)) row-specs)]
    (-> (h/delete-from (utils/to-keyword table))
        (h/where
         [:in :id ids]
         [:= :negative negated?]
         [:= :context context])
        (sql/format {:inline true})
        utils/execute!)))
