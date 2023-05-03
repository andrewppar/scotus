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
        pred (formula/predicate formula)
        arg-names (state/table-args pred)
        arg-instance (db/lookup-rows
                      "arg_instance" contexts [[pred nil nil]])
        arg->name (zipmap (formula/args formula) arg-names)

        ]
    [arg-names
     arg-instance
     arg->name]

    ))


(well-formed-atomic-assert
 ["subclass_of" "cat" "dog"] "universal")






(defn well-formed-assert [formula]
  )
