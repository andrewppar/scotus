(ns scotus.database.utils
  (:require
   [clojure.string :as string]
   [honey.sql :as sql]
   [next.jdbc :as jdbc]
   [scotus.state :as state])
  (:import (java.util UUID)))

(defn ddl-success? [result]
  (= result [#:next.jdbc{:update-count 0}]))

(defn run
  [stmt]
  (jdbc/execute! (state/db-connection) [stmt]))

(defn execute! [stmt]
  (jdbc/execute! (state/db-connection) (sql/format stmt)))

(defn format [stmt]
  (sql/format stmt))

(defn clean-object-name
  [item]
  (string/replace item #"[- ]" "_"))

(defn to-keyword
  [item]
  (-> item clean-object-name keyword))

(defn ->uuid
  [item]
  (UUID/nameUUIDFromBytes (.getBytes (str item))))
