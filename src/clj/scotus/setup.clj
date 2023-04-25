(ns scotus.setup
  (:require [scotus.database :as db]
            [scotus.state :as state]))

(def required-tables
  [["instance" "thing" "class"]
   ["subcontext_of" "subcontext" "supercontext"]
   ["subclass_of" "subclass" "superclass"]
   ["transitive_arg"
    "predicate" "arg" "transitive-predicate" "start-arg" "transitive-arg"]])

(def required-rows
  {"instance"
   [["universal"            "context"]
    ["predicate"            "class"]
    ["transitive-predicate" "class"]
    ["reflexive-predicate"  "class"]
    ["instance"             "predicate"]
    ["subcontext_of"        "transitive-predicate"]
    ["subclass_of"          "transitive-predicate"]
    ["subclass_of"          "reflexive-predicate"]]
   "transitive_arg"
   [["subcontext_of"
     "supercontext" "subcontext_of" "subcontext" "supercontext"]
    ["subclass_of"
     "superclass" "subclass_of" "subclass" "superclass"]
    ["instance"
     "class" "subclass_of" "subclass" "superclass"]]
   "subclass_of"
   [["transitive-predicate" "predicate"]
    ["reflexive-predicate"  "predicate"]]})

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
