(ns scotus.formula.semantic
  (:require [clojure.string :as string]
            [scotus.database.query :as dbq]
            [scotus.assert :as assert]
            [scotus.formula.formula :as formula]
            [scotus.state :as state]
            [scotus.transitive :as transitive]))

(def semantic-predicates
  ["arg_instance"
   ;; I'm not sure how I feel about this predicate
   "require_arg"
   "unassertible"])

(defn predicate?
  "A string that is represented as a predicate in the knowledge base."
  [object]
  (.contains (keys (state/predicate-index)) object))

(defn generate-semantic-error [formula arg->name arg->type contexts]
  (let [predicate (formula/predicate formula)
        instance?  (transitive/instance?* arg type contexts)
    (->> arg->type
         (map (fn [[arg type]]
                {:arg arg
                 :type type
                 :value

                 :justification
                 `[(assert/make "arg-instance"
                                [~predicate ~(get arg->name arg) ~type])

                   ~formula]}))
         (filter (comp not :value))
         (reduce
          (fn [error {:keys [justification arg type]}]
            (conj error {:formula ["unknown" ["instance" arg type]]
                         :justification justification}))
          []))))

;;; Assert

(defn unassertible?
  [predicate]
  (boolean
   (seq
    (dbq/lookup-rows
     "instance" :universal false `[[~predicate "unassertible"]]))))

(defn ^:private wtf-error-message
  [well-formed? retry? error]
  {:well-formed? well-formed?
   :retry? retry?
   :error error})

(defn wtf-atomic-assert
  [formula contexts]
  (let [pred (formula/predicate formula)]
    (cond (not (predicate? pred))
          (wtf-error-message
           false false  `[:not ["instance" ~pred "predicate"]])
          (unassertible? pred)
          (wtf-error-message
           false false `["asserted" ["instance" ~pred "unassertible"]])
          :else
          (let [arg-names (state/table-args pred)
                name->type (reduce
                            (fn [acc {:arg_instance/keys [class argument]}]
                              (assoc acc argument class))
                            {} (dbq/lookup-rows
                                "arg_instance" contexts false [[pred nil nil]]))
                ;; This won't work if we have the same arg in two places....
                arg->name (zipmap (formula/args formula) arg-names)
                arg->type (reduce-kv
                           (fn [acc arg name]
                             (if-let [type (get name->type name)]
                               (if-not (formula/variable? arg)
                                 (assoc acc arg type)
                                 acc)
                               acc))
                           {} arg->name)]
            ;; We do transitive instance twice - here and in generate-semantic-erorr
            (if (every?
                 (fn [[arg type]] (transitive/instance? arg type contexts))
                 arg->type)
              (wtf-error-message true false nil)
              (wtf-error-message
               false true (generate-semantic-error formula arg->name arg->type contexts)))))))

(defn wtf-atomic-query
  [formula contexts]
;;  (let [predicate (formula/predicate formula)]
;;    (when (and
;;           (transitive/instance? predicate "closed-predicate" contexts)
;;           ;; Need groundable?
;;           (not (formula/ground? formula)))
;;      (

  true)

(defmulti wtf-internal
  {:arglists '([formula contexts wtf-type])}
  (fn [formula _ _]
    (formula/formula-type formula))
  :hierarchy formula/hierarchy)

(defmethod wtf-internal :atomic
  [formula contexts wtf-type]
  (case wtf-type
    :assert (wtf-atomic-assert formula contexts)
    :query  (wtf-atomic-query formula contexts)))

(defmethod wtf-internal :not
  ;;This could be a somewhat difficult question. I want to distinguish
  ;; between two types of negation - a serious negation and a meta negation.
  ;; A formula is seriously negated when it's negatum is well-formed and false.
  ;; A formula is meta-negated when its negatum is not well-formed.

  ;;I don't think this needs to be made explicit in the system now.
  ;; When it comes to querying a negation, meta-negation should also
  ;; be considered as a way to return a result.
  [formula contexts wtf-type]
  (-> formula
      formula/negatum
      (wtf-internal contexts wtf-type)))

(defmethod wtf-internal :junction
  [formula contexts wtf-type]
  (reduce
   (fn [acc junct]
     (let [{:keys [well-formed? error]}
           (wtf-internal junct contexts wtf-type)]
       (if well-formed?
         acc
         (-> acc
            (assoc :well-formed? false)
            (update :error (fnil formula/add-juncts [:and]) error)))))
   {:well-formed? true :error nil}
   (formula/juncts formula)))

(defmethod wtf-internal :implies
  [formula contexts wtf-type]
  (let [ant  (formula/antecedent formula)
        conq (formula/consequent formula)]
    (and
     (wtf-internal ant contexts wtf-type)
     (wtf-internal conq contexts wtf-type))))

(defn wtf-assert
  [formula context]
  (let [contexts (transitive/subcontext context)]
    (wtf-internal formula contexts :assert)))

;;; Query

(defn wtf-query
  [formula context]
  (let [contexts (transitive/subcontext context)]
    (wtf-internal formula contexts :query)))
