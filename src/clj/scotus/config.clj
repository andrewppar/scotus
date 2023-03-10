(ns scotus.config
  (:require [omniconf.core :as cfg]))

(cfg/define
  {:db {:nested
        {:db_name {:description
                   "The name of the database that backs scotus."
                   :type :string
                   ;; Maybe relax this for first time setup
                   :required true
                   :default "kb"}
         :db_host {:description
                   "The host where the database is kept."
                   :type :string
                   :required true
                   :default "localhost"}
         :db_user {:description
                   "The name of the user accessing the database."
                   :type :string
                   :required true
                   :default "postgres"}
         :db_port {:desccription
                   "The port to use when accessing the database."
                   :type :number
                   :required true
                   :default 5432}}}})


(defn config-init!
  "Populate scotus's configuration"
  []
  (cfg/populate-from-env))

#_{:clj-kondo/ignore [:redefined-var]}
(defn get
  "Get an item from the configuration."
  [& args]
  (apply cfg/get args))

(comment
  (config-init!)
  )
