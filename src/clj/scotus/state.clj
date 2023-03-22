(ns scotus.state
  (:require
   [integrant.core :as ig]
   [next.jdbc      :as jdbc]
   [honey.sql      :as sql]
   [honey.sql.helpers :as h]
   [scotus.config  :as cfg]))

(defmacro dpf [form]
  `(let [result# ,form]
     (println (format "%s => %s" ',form result#))
     result#))

(def state (atom nil))

(defmethod ig/init-key :index/predicate [_ {:keys [db-connection]}]
  (let [excluded-tables ["information_schema" "pg_catalog" "views"]
        excluded-cols ["id" "negative" "justification" "context"]
        raw-results (-> (h/select-distinct
                         :table-name
                         :table-schema
                         :column-name
                         :ordinal-position)
                        (h/from :information-schema/columns)
                        (h/where
                         [:not-in :table-schema excluded-tables]
                         [:not-in :column-name  excluded-cols])
                        (h/order-by :table-name :ordinal-position)
                        sql/format
                        ((partial jdbc/execute! db-connection)))
        tables      (distinct (map :columns/table_name raw-results))]
    (reduce
     (fn [result table]
       (assoc result table
              (->> raw-results
                   (filter
                    (fn [{:columns/keys [table_name]}]
                      (= table_name table)))
                   (sort-by :columns/ordinal_position)
                   (map :columns/column_name))))
     {}
     tables)))

(defmethod ig/init-key :database/connection [_ _]
  (let [dbtype "postgres"
        dbuser (cfg/get-item :db :user)
        dbpass (cfg/get-item :db :pass)
        dbhost (cfg/get-item :db :host)
        dbport (cfg/get-item :db :port)
        dbname (cfg/get-item :db :name)]
    (jdbc/get-datasource
     {:dbtype   dbtype
      :user     dbuser
      :password dbpass
      :host     dbhost
      :port     dbport
      :dbname dbname})))

(def config
  {:database/connection []
   :index/predicate {:db-connection (ig/ref :database/connection)}})

(defn init!
  "Initialize all the state for scotus."
  []
  (clojure.core/reset! state (ig/init config)))

(defn reset!
  "Reset the state for scotus."
  []
  (clojure.core/reset! state nil)
  (init!))

(defn db-connection
  "Get the current database connection."
  []
  (get @state :database/connection))

(defn get-table-args
  "Get the arguments associated with a table"
  [table]
  (get-in @state [:index/predicate table]))

(defn refresh-index []
  (let [new-state (ig/init config [:index/predicate])]
    (clojure.core/reset! state new-state)))

(comment
  (init!))

@state
