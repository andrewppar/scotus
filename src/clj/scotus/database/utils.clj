(ns scotus.database.utils
  (:require
   [clojure.string :as string]
   [next.jdbc    :as jdbc]
   [scotus.state :as state]))

(defn execute! [stmt]
  (jdbc/execute! (state/db-connection) stmt))

(defn clean-object-name
  [item]
  (string/replace item #"[- ]" "_"))

(defn to-keyword
  [item]
  (-> item clean-object-name keyword))
