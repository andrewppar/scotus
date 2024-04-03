(ns scotus.database.remove
  (:require
   [honey.sql             :as sql]
   [honey.sql.helpers     :as h]
   [scotus.database.utils :as utils]
   [scotus.state          :as state]))


(defn drop-table!
  "Delete a table from the database."
  [table]
  (state/with-refreshed-index
    (->> table
         utils/to-keyword
         (h/drop-table :if-exists)
         utils/execute!
         utils/ddl-success?)))

(defn delete-rows
  "Given a vector of row-specifications, a table, and a context,
  delete the corresponding rows from the table."
  [table context negated? row-spec]
  (let [args (map utils/to-keyword (state/table-args table))
        arg-clauses (reduce-kv
                     (fn [acc arg value]
                       (if (= value "")
                         acc
                         (conj acc [:= arg value])))
                     []
                     (zipmap args row-spec))
        where-clause (fn [sql-clause]
                       (apply h/where sql-clause (conj arg-clauses
                                                       [:= :negative negated?]
                                                       [:= :context context])))]
    (-> (h/delete-from (utils/to-keyword table))
        where-clause
        utils/execute!)))
