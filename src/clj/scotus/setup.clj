(ns scotus.setup
  (:require [scotus.database.query      :as dbq]
            [scotus.database.assert-map :as am]
            [scotus.database.add        :as dba]
            [scotus.database.remove     :as dbr]
            [scotus.state               :as state]))

(def required-tables
  [["instance" "thing" "class"]
   ["arg_instance" "predicate" "argument" "class"]
   ["subcontext_of" "subcontext" "supercontext"]
   ["subclass_of" "subclass" "superclass"]
   ["description" "thing" "description"]
   ["disjoint" "class_one" "class_two"]
   ["transitive_arg"
    "predicate" "arg" "transitive-predicate" "start-arg" "transitive-arg"]])

(def required-positive-rows
  {"instance"
   [["universal"            "context"]
    ["class"                "class"]
    ["context"              "class"]
    ["predicate"            "class"]
    ["arg_instance"         "predicate"]
    ["unassertible"         "class"]
    ["asserted"             "unassertible"]
    ["unknown"              "unassertible"]
    ["instance"             "transitive-predicate"]
    ["subcontext_of"        "transitive-predicate"]
    ["subcontext_of"        "reflexive-predicate"]
    ["subclass_of"          "transitive-predicate"]
    ["subclass_of"          "reflexive-predicate"]
    ["disjoint"             "symmetric-predicate"]
    ["enumeration"          "class"]
    ["arg-type"             "class"]
    ["arg-type"             "enumeration"]
    ["time"                 "arg-type"]
    ["date"                 "arg-type"]]
   "transitive_arg"
   [["subcontext_of"
     "supercontext" "subcontext_of" "subcontext" "supercontext"]
    ["subclass_of"
     "superclass" "subclass_of" "subclass" "superclass"]
    ["instance"
     "class" "subclass_of" "subclass" "superclass"]]
   "subclass_of"
   [["transitive-predicate" "predicate"]
    ["reflexive-predicate"  "predicate"]
    ["symmetric-predicate"  "predicate"]
    ["unassertible"         "predicate"]
    ;; One day support query restrictions on
    ;; predicates that must be fully bound at query time
    #_["closed-prediate"      "predicate"]]
   "arg_instance"
   [["instance" "class" "class"]
    ["arg_instance" "predicate" "predicate"]
    ["arg_instance" "class" "class"]
    ["disjoint" "class_one" "class"]
    ["disjoint" "class_two" "class"]
    ["subcontext_of" "subcontext" "context"]
    ["subcontext_of" "supercontext" "context"]
    ["subclass_of" "subclass" "class"]
    ["subclass_of" "superclass" "class"]
    ["transitive_arg" "predicate" "predicate"]
    ["transitive_arg" "transitive_predicate" "predicate"]]})

(def required-negative-rows
  {"instance"
   [["disjoint" "reflexive-predicate"]]})

(defn setup? []
  (let [row-count (reduce-kv (fn [num _ rows]
                               (+ num (count rows)))
                             0 required-positive-rows)
        vals (try (reduce-kv
                   (fn [results pred specs]
                     (concat
                      (dbq/lookup-rows pred :universal false specs)
                      results))
                   []
                   required-positive-rows)
               (catch Exception _
                 false))]
    (boolean
     (when vals
       (= (count vals) row-count)))))

(defn add-setup-asserts
  [required-rows polarity]
  (reduce-kv
   (fn [acc predicate specs]
     (am/merge-assert-maps
      acc
      (dba/add-rows predicate "universal" "anparisi" polarity specs)))
   {:assert-count 0}
   required-rows))

(defn setup!
  []
  (mapv (fn [spec] (apply dba/create-table! spec)) required-tables)
  (am/merge-assert-maps
   {:assert-count 0}
   (add-setup-asserts required-positive-rows false)
   #_(add-setup-asserts required-negative-rows true)))

(defn tear-down!
  []
  (state/refresh-index!)
  (mapv
   (fn [predicate]
     (dbr/drop-table! predicate))
   (state/tables)))

(comment
  (setup!)
  (tear-down!)
  )
