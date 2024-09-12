(ns scotus.state
  (:require
   [integrant.core :as ig]
   [next.jdbc      :as jdbc]
   [honey.sql      :as sql]
   [honey.sql.helpers :as h]
   [scotus.config  :as cfg]
   [scotus.natural-language.database.index :as nlp.index]))

(def state (atom nil))

(defn set-table-args
  [all-table-info table]
  (->> all-table-info
       (filter
        (fn [{:columns/keys [table_name]}]
          (= table_name table)))
       (sort-by :columns/ordinal_position)
       (map :columns/column_name)))

(defn set-table-column-type
  [all-table-info table]
  (->> all-table-info
       (filter
        (fn [{:columns/keys [table_name]}]
          (= table_name table)))
       (reduce
        (fn [result {:columns/keys [column_name data_type]}]
          (assoc result column_name data_type))
        {})))

(defn set-table-count
  [table db-connection]
  (-> (h/select [[:count :*]])
      (h/from (keyword table))
      sql/format
      ((partial jdbc/execute! db-connection))
      first
    (get :count)))

(defmethod ig/init-key :index/predicate [_ {:keys [db-connection]}]
  (let [excluded-tables ["information_schema" "pg_catalog" "views"]
        excluded-cols ["id" "negative" "justification" "context"]
        raw-results (-> (h/select-distinct
                         :table-name
                         :table-schema
                         :data_type
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
       (-> result
           (assoc-in [table :args]
                     (set-table-args raw-results table))
           (assoc-in [table :count]
                     (set-table-count table db-connection))
           (assoc-in [table :column-types]
                     (set-table-column-type raw-results table))))
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

(defmethod ig/init-key :nlp/store [_ _]
  (nlp.index/store (cfg/get-item :nlp :store-location)))

(defmethod ig/init-key :nlp/analyzer [_ _]
  (nlp.index/analyzer))

(defmethod ig/init-key :nlp/writer [_ {:keys [store analyzer]}]
  (nlp.index/writer store analyzer))

(defmethod ig/init-key :nlp/searcher [_ {:keys [store]}]
  (nlp.index/searcher store))

(def config
  {:database/connection []
   :index/predicate {:db-connection (ig/ref :database/connection)}
   :nlp/store []
   :nlp/analyzer []
   :nlp/writer {:store (ig/ref :nlp/store) :analyzer (ig/ref :nlp/analyzer)}
   #_#_:nlp/searcher {:store (ig/ref :nlp/store)}})

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

(defn table-args
  "Get the arguments associated with a table"
  [table]
  (into [] (get-in @state [:index/predicate table :args])))

(defn tables
  "Get all the tables in the database."
  []
  (keys (get @state :index/predicate)))

(defn table-count
  "Get the number of rows in a table"
  [table]
  (get-in @state [:index/predicate table :count]))

(defn refresh-index! []
  (let [new-state (ig/init config [:index/predicate])]
    (clojure.core/reset! state new-state)))

(defn predicate-index  []
  (get @state :index/predicate))

(defn table-arg-types [table]
  (get-in (predicate-index) [table :column-types]))

(defmacro with-refreshed-index [& body]
  {:style/indent 1}
  `(let [result# (do ~@body)]
     (refresh-index!)
     result#))

(comment
  (init!)
  (refresh-index!)


  )
