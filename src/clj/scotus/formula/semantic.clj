(ns scotus.formula.semantic
  (:require [scotus.database :as db]
            [scotus.formula.formula :as formula]
            [scotus.state :as state]
            [scotus.transitive :as transitive]))

(def semantic-predicates
  ["arg_instance"
   ;; I'm not sure how I feel about this predicate
   "require_arg"])

(defn predicate?
  "A string that is represented as a predicate in the knowledge base."
  [object]
  (.contains (keys (state/predicate-index)) object))

(defn well-formed-atomic-assert
  [formula context]
  (let [contexts (transitive/subcontext context)
        pred (formula/predicate formula)]
    (if-not (predicate? pred)
      false
      (let [arg-names (state/table-args pred)
            name->type (reduce
                        (fn [acc {:arg_instance/keys [class argument]}]
                          (assoc acc argument class))
                        {} (db/lookup-rows
                            "arg_instance" contexts [[pred nil nil]]))
            arg->name (zipmap (formula/args formula) arg-names)
            arg->type (reduce-kv
                       (fn [acc arg name]
                         (if-let [type (get name->type name)]
                           (assoc acc arg type)
                           acc))
                       {} arg->name)]
        (every? (fn [[arg type]] (transitive/instance? arg type contexts)) arg->type)))))


(transitive/instance? "sandbox" "class" :universal)

(well-formed-atomic-assert
 ["instance" "312" "sandbox"] "universal")








(defn well-formed-assert [formula]
  )
