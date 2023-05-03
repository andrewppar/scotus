(ns scotus.setup
  (:require [scotus.database :as db]
            [scotus.state :as state]))

(def required-tables
  [["instance" "thing" "class"]
   ["arg_instance" "predicate" "argument" "class"]
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
    ["arg_instance"         "predicate"]
    ["instance"             "transitive-predicate"]
    ["subcontext_of"        "transitive-predicate"]
    ["subcontext_of"        "reflexive-predicate"]
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
    ["reflexive-predicate"  "predicate"]]
   "arg_instance"
   [["instance" "class" "class"]
    ["arg_instance" "predicate" "predicate"]
    ["arg_instance" "class" "class"]
    ["subcontext_of" "subcontext" "context"]
    ["subcontext_of" "supercontext" "context"]
    ["subclass_of" "subclass" "class"]
    ["subclass_of" "superclass" "class"]
    ["transitive_arg" "predicate" "predicate"]
    ["transitive_arg" "transitive_predicate" "predicate"]]})

(defn setup? []
  (let [row-count (reduce-kv (fn [num _ rows]
                               (+ num (count rows)))
                             0 required-rows)
        vals (try (reduce-kv
                   (fn [results pred specs]
                     (concat
                      (db/lookup-rows pred :universal specs)
                      results))
                   []
                   required-rows)
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

(defn tear-down!
  []
  (state/refresh-index!)
  (mapv
   (fn [predicate]
     (db/drop-table! predicate))
   (state/tables)))


(comment
  (setup!)
  (tear-down!)
  )
