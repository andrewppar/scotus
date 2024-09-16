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

(defn set-table-nlp-args [table tables db-connection]
  (mapv
   (fn [{:nlp_args/keys [arg]}] arg)
   (-> (h/select-distinct :arg)
       (h/from :nlp-args)
       (h/where [:= :predicate table])
       sql/format
       ((partial jdbc/execute! db-connection)))))

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
        tables      (distinct (map :columns/table_name raw-results))
        nlp-ready? (contains? (set tables) "nlp_args")]
    (reduce
     (fn [result table]
       (-> result
           (assoc-in [table :args]
                     (set-table-args raw-results table))
           (assoc-in [table :count]
                     (set-table-count table db-connection))
           (assoc-in [table :column-types]
                     (set-table-column-type raw-results table))
           (cond-> nlp-ready?
             (assoc-in
              [table :nlp-args]
              (set-table-nlp-args table tables db-connection)))))
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

(def config
  {:database/connection []
   :index/predicate {:db-connection (ig/ref :database/connection)}
   :nlp/store []
   :nlp/analyzer []})

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

(defn table-nlp-args
  "Get the nlp args associated with a table."
  [table]
  (into [] (get-in @state [:index/predicate table :nlp-args])))

(defn tables
  "Get all the tables in the database."
  []
  (keys (get @state :index/predicate)))

(defn table-count
  "Get the number of rows in a table"
  [table]
  (get-in @state [:index/predicate table :count]))

(defn nlp-store
  "Get the nlp store."
  []
  (get @state :nlp/store))

(defn nlp-analyzer
  "Get the nlp analyzer."
  []
  (get @state :nlp/analyzer))

(defn refresh-index! []
  (let [new-index (get
                   (ig/init config [:index/predicate])
                   :index/predicate)]
    (clojure.core/swap! state assoc :index/predicate new-index)))

(defn predicate-index  []
  (get @state :index/predicate))

(defn table-arg-types [table]
  (get-in (predicate-index) [table :column-types]))

(defmacro with-refreshed-index [& body]
  {:style/indent 1}
  `(let [result# (do ~@body)]
     (refresh-index!)
     result#))

(defn close-store []
  (nlp.index/close (get @state :nlp/store)))

(comment
  (init!)
  (refresh-index!)


  )
