(ns scotus.setup
  (:require [scotus.database :as db]
            [scotus.state :as state]))

(def required-tables
  [["instance" "thing" "class"]
   ["subcontext_of" "subcontext" "supercontext"]])

(def required-rows
  {"instance"
   [["universal"     "context"]
    ["instance"      "predicate"]
    ["subcontext_of" "predicate"]]})

(defn setup? []
  (let [row-count (reduce-kv (fn [num _ rows]
                               (+ num (count rows)))
                             0 required-rows)
        vals (try (db/lookup-rows
                   "instance" ["universal"]
                   [["instance" "predicate"]
                    ["subcontext_of" "predicate"]
                    ["universal" "context"]])
               (catch Exception _
                 false))]
    (boolean
     (when vals
       (= (count vals) row-count)))))


(defn setup!
  []
  (mapv (fn [spec] (apply db/create-table! spec)) required-tables)
  (reduce-kv
   (fn [_ predicate specs]
     (db/add-rows predicate "universal" "anparisi" false specs))
   nil
   required-rows)
  :done)
