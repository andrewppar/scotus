(ns scotus.database.utils
  (:require
   [clojure.string :as string]
   [honey.sql :as sql]
   [next.jdbc :as jdbc]
   [scotus.state :as state])
  (:import (java.util UUID)))

(defn ddl-success? [result]
  (= result [#:next.jdbc{:update-count 0}]))

(defn something-inserted? [result]
  (< 0 (get (first result) :next.jdbc/update-count)))

(defn insert-count [result]
  (get (first result) :next.jdbc/update-count))

(defn run
  [stmt]
  (jdbc/execute! (state/db-connection) [stmt]))

(defn execute! [stmt]
  (jdbc/execute! (state/db-connection) (sql/format stmt)))

(defn format [stmt]
  (sql/format stmt))

(defn clean-object-name
  [item]
  (string/replace item #"[- :#@]" "_"))

(defn clean-table-name
  [item]
  (string/replace item #"[- ]" "_"))

(defn to-keyword
  [item]
  (-> item clean-table-name keyword))

(defn ->uuid
  [item]
  (UUID/nameUUIDFromBytes (.getBytes (str item))))

(defn ->assertion-id [args context]
  (->uuid (sort (assoc args :context context))))
