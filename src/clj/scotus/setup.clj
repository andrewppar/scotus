(ns scotus.setup
  (:require [scotus.database :as db]
            [scotus.state :as state]))

(defn setup!
  []
  (db/create-table! "instance" "thing" "class")
  (db/create-table! "subcontext_of" "subcontext" "supercontext")
  (db/add-rows
   "instance" "universal" "anparisi" false [["universal" "context"]]))
