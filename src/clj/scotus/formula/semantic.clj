(ns scotus.formula.semantic
  (:require [clojure.string :as string]
            [scotus.database :as db]
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
  [formula contexts]
  (let [pred (formula/predicate formula)]
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
                           (if-not (formula/variable? arg)
                             (assoc acc arg type)
                             acc)
                           acc))
                       {} arg->name)]
        (if (every?
             (fn [[arg type]] (transitive/instance? arg type contexts))
             arg->type)
          {:well-formed true :error nil}
          {:well-formed false
           :error (string/join
                   "\n"
                   (->> arg->type
                        (map (fn [[arg type]]
                               [:arg arg
                                :type type
                                :value
                                (transitive/instance? arg type contexts)]))
                        (filter :value)
                        (map (fn [{:keys [arg type]}]
                               (format "%s is not a %s" arg type)))))})))))




(defmulti well-formed-assert-internal
  {:arglists '([formula contexts])}
  (fn [formula _]
    (formula/formula-type formula))
  :hierarchy formula/hierarchy)

(defmethod well-formed-assert-internal :atomic
  [formula contexts]
  (let [well-formed-map (well-formed-atomic-assert formula contexts)]
    well-formed-map))

(defmethod well-formed-assert-internal :not
  ;;This could be a somewhat difficult question. I want to distinguish
  ;; between two types of negation - a serious negation and a meta negation.
  ;; A formula is seriously negated when it's negatum is well-formed and false.
  ;; A formula is meta-negated when its negatum is not well-formed.

  ;;I don't think this needs to be made explicit in the system now.
  ;; When it comes to querying a negation, meta-negation should also
  ;; be considered as a way to return a result.
  [formula contexts]
  (-> formula
      formula/negatum
      (well-formed-assert-internal contexts)))

(defmethod well-formed-assert-internal :junction
  [formula contexts]
  (every?
   (fn [junct]
     (well-formed-assert-internal junct contexts))
   (formula/juncts formula)))

(defmethod well-formed-assert-internal :implies
  [formula contexts]
  (let [ant  (formula/antecedent formula)
        conq (formula/consequent formula)]
    (and
     (well-formed-assert-internal ant contexts)
     (well-formed-assert-internal conq contexts))))

(defn well-formed-assert
  [formula context]
  (let [contexts (transitive/subcontext context)]
    (well-formed-assert-internal formula contexts)))
