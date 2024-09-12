(ns scotus.config
  (:require [omniconf.core :as cfg]))

(cfg/define
  {:db {:nested
        {:name {:description
                   "The name of the database that backs scotus."
                   :type :string
                   ;; Maybe relax this for first time setup
                   :required true
                   :default "kb"}
         :host {:description
                   "The host where the database is kept."
                   :type :string
                   :required true
                   :default "localhost"}
         :user {:description
                   "The name of the user accessing the database."
                   :type :string
                   :required true
                   :default "postgres"}
         :port {:description
                   "The port to use when accessing the database."
                   :type :number
                   :required true
                :default 5432}}}
   :nlp {:nested
         {:store-location
          {:description "The location of the NLP Store"
           :type :string
           :default "resources/scotus"}}}})

(defn init!
  "Populate scotus's configuration"
  []
  (cfg/populate-from-env))

(defn get-item
  "Get an item from the configuration."
  [& args]
  (apply cfg/get args))

(defn set-item
  "Set a configuration item to a different value"
  [ks value]
  (cfg/set ks value))



(comment
  (init!)
  )
