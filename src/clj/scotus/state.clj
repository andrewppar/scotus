(ns scotus.state
  (:require
   [integrant.core :as ig]
   [next.jdbc      :as jdbc]
   [scotus.config  :as cfg]))

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
  {:database/connection []})

(def state (atom nil))

(defn init!
  []
  (reset! state (ig/init config)))

(defn db-connection
  "Get the current database connection."
  []
  (get @state :database/connection))

(comment
  (init!)

  )
