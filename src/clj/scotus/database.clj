(ns scotus.database
  (:require [honey.sql :as sql]
            [honey.sql.helpers :as h]
            [next.jdbc :as jdbc]))


(defn create-table
  [table-name & columns]
  (let [required-cols ["id" "negative" "justification"]
        all-cols     (concat required-cols columns)]
    ))
