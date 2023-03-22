(ns scotus.formula
  (:require [scotus.state :as state]))

(defn predicate?
  "A string that represents a predicate of the system."
  [object]
  (and
   (string? object)
   (-> @state/state
       (get :index/predicate)
       keys
       (.contains object))))

(defn main-operator
  [formula]
  (first formula))

(defn ^:private maybe-accessor-error
  [object test-fn object-type accessor-type]
  (when-not (test-fn object)
    (throw
     (ex-info (format "%s is not %s" object object-type)
              {:caused-by
               "Only %s has %s." object-type accessor-type}))))


;; Atomics

(defn atomic?
  "A formula with no logical complexity.

  NOTE: This namespace is purely syntactic so there is no check here
  that the predicate is followed by a right number of arguments."
  [object]
  (and
   (coll? object)
   (predicate? (main-operator object))))

(defn atomic
  "Create an atomic formula"
  [predicate & args]
  `[predicate ~@args])

(defn predicate
  "Gets the predicate of an atomic formula. If the formula is not
  atomic it throws an error."
  [formula]
  (maybe-accessor-error formula atomic? "atomic formula" "predicate")
  (main-operator formula))

(defn args
  "Get the args of an atomic formula. If the formula is not
  atomic it throws an error."
  [formula]
  (maybe-accessor-error formula atomic? "atomic formula" "args")
  (rest formula))

(declare formula?)

;; Negation

(defn negation?
  "A formula whose main operator is `:not`"
  [object]
  (and
   (= (main-operator object) :not)
   (formula? (second object))))

(defn lnot
  "Create a negation from a formula."
  [formula]
  [:not formula])

(defn negatum
  "The negated formula of a negation."
  [formula]
  (maybe-accessor-error formula negation? "negation" "negatum")
  (second formula))

;; Conjunction

(defn conjunction?
  "A formula whose main operator is `:and`"
  [object]
  (and
   (= (main-operator object) :and)
   (every? formula? (rest object))))


(defn land
  "Create a conjunction from conjuncts."
  [& conjuncts]
  `[:and ~@conjuncts])

(defn conjuncts
  "The conjuncts of a conjunction"
  [formula]
  (maybe-accessor-error formula conjunction? "conjunction" "conjuncts")
  (rest formula))

;; Disjunction

(defn disjunction?
  "A formula whose main operator is `:or`"
  [object]
  (and
   (= (main-operator object) :or)
   (every? formula? (rest object))))


(defn lor
  "Create a conjunction from disjuncts."
  [& disjuncts]
  `[:or ~@disjuncts])

(defn disjuncts
  "The conjuncts of a conjunction"
  [formula]
  (maybe-accessor-error formula disjunction? "disjunction" "disjuncts")
  (rest formula))

;; Junction
(defn junction?
  "Check if a formula is a junction"
  [formula]
  (or
   (conjunction? formula)
   (disjunction? formula)))

(defn junction
  [kind & juncts]
  (case kind
    :and (apply land juncts)
    :or  (apply lor juncts)))

(defn juncts
  [formula]
  (maybe-accessor-error formula junction? "junction" "juncts")
  (rest formula))


;; Implication

;; TODO: Should we not allow rules to conclude to disjunctions?
(defn rule?
  "Check if an object is a rule."
  [object]
  (and
   (= (main-operator object) :implies)
   (= (count object) 3)
   (formula? (nth object 1))
   (formula? (nth object 2))))

(defn lif
  "Create a rule."
  [antecedent consequent]
  [:implies antecedent consequent])

(defn antecedent
  "Get the antecedent of a rule"
  [rule]
  (maybe-accessor-error rule rule? "rule" "antecedent")
  (nth rule 1))

(defn consequent
  "Get the consequent of a rule"
  [rule]
  (maybe-accessor-error rule rule? "rule" "consequent")
  (nth rule 2))

(defn formula?
  [object]
  (or
   (atomic? object)
   (negation? object)
   (conjunction? object)
   (disjunction? object)
   (rule? object)))

(defn kind
  [formula]
  (let [operator (main-operator formula)]
      (if (predicate? operator)
        :atomic
        operator)))

(def hierarchy (atom (make-hierarchy)))

(swap! hierarchy derive :atomic   :formula)
(swap! hierarchy derive :not      :junction)
(swap! hierarchy derive :and      :junction)
(swap! hierarchy derive :or       :formula)
(swap! hierarchy derive :junction :formula)
(swap! hierarchy derive :implies  :formula)
