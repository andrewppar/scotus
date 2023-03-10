(ns scotus.state
  (:require
   [integrant.core :as ig]
   [next.jdbc      :as jdbc]
   [scotus.config  :as cfg]))

;;; Make a db connection here

(defmethod ig/init-key :db/database-connection
  []
  (let [dbtype "postgres"
        dbuser (cfg/get :db :db_user)
        dbpass (cfg/get :db :db_pass)
        dbhost (cfg/get :db :db_host)
        dbport (cfg/get :db :db_port)
        dbname (cfg/get :db :db_name)]
    (jdbc/get-datasource
     {:dbtype   dbtype
     :user     dbuser
     :password dbpass
     :host     dbhost
     :port     dbport
     :dbname dbname})))

(def config
  {:db/database-connection []})

(def state (atom nil))

(defn init!
  []
  (reset! state (ig/init config)))

(comment
  (init!)

  )

(ig/init config)
