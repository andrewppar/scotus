(ns scotus.syntax.formula
  (:require
   [clojure.string :as string]))

(defn variable? [object]
  (and (symbol? object)
       (string/starts-with? (name object) "?")))

(defn predicable? [object]
  (or (string? object)
      (variable? object)))

(defmulti formula?
  {:arglists '([object])}
  (fn [object]
    (first object)))

(defmethod formula? :default
  [object]
  (and
   (vector? object)
   (predicable? (first object))))

(defmethod formula? :not
  [object]
  (formula? (second object)))

(defmethod formula? :and
  [object]
  (every? formula? (rest object)))

(defmethod formula? :or
  [object]
  (every? formula? (rest object)))

(defmethod formula? :implies
  [object]
  (and
   (formula? (nth object 1))
   (formula? (nth object 2))))

(defn kind [object]
  (when (formula? object)
    (if (keyword? (first object))
      (first object)
      :atom)))

(defn atom? [object]
  (and (vector? object)
       (predicable? (first object))))

(defn predicate [formula]
  (when (atom? formula)
    (first formula)))

(defn ->atom
  "Create an atom from `pred` and `args`.

  If `args` has only one item that is also a vector then
  that vector is treated as containing the args. Otherwise, args
  is treated as a `&rest` argument."
  [pred & args]
  (if (and (= (count args) 1) (seq? (first args)))
    (into [pred] (first args))
    (into [pred] args)))

(defn negatum [negation]
  (second negation))

(defn negation? [object]
  (and
   (vector? object)
   (= (first object) :not)
   (formula? (negatum object))))

(defn negate [formula]
  [:not formula])

(defn literal? [formula]
  (or (atom? formula)
      (and (negation? formula)
           (atom? (negatum formula)))))

(defn literal-predicate [formula]
  (when (literal? formula)
    (if (atom? formula)
      (predicate formula)
      (predicate (negatum formula)))))

(defn args
  "Assumes that the thing given is a `formula?`."
  [formula]
  (rest formula))

(defn literal-args
  "Get the args from `literal`"
  [literal]
  (args
   (if (negation? literal)
     (negatum literal)
     literal)))

;;; This needs to be really fleshed out when we have quantifiers...
(defn fully-bound? [literal]
  (every? (complement variable?) (literal-args literal)))

(defn disjunction? [object]
  (and
   (vector? object)
   (= (first object) :or)
   (every? formula? (args object))))

(defn disjoin [formulas]
  (into [:or] formulas))

(defn conjunction? [object]
  (and
   (vector? object)
   (= (first object) :and)
   (every? formula? (args object))))

(defn conjoin [formulas]
  (into [:and] formulas))

(defn kind-pred [kind]
  (case kind
    :atom atom?
    :not negation?
    :and conjunction?
    :or disjunction?
    #_#_:implies implication?))


(defn dual [formula-kind]
  (case formula-kind
    :atom :atom
    :not :not
    :and :or
    :or :and
    :implies :implies))

(defn implication? [object]
  (and
   (vector? object)
   (= (count object) 3)
   (= (first object) :implies)
   (formula? (nth object 1))
   (formula? (nth object 2))))

(defn antecedent [formula]
  (nth formula 1))

(defn consequent [formula]
  (nth formula 2))

(defn same? [formula-one formula-two]
  (or (= formula-one formula-two)
      (cond (and (atom? formula-one)
                 (atom? formula-two))
            (= formula-one formula-two)

            (and (negation? formula-one)
                 (negation? formula-two))
            (same? (negatum formula-one) (negatum formula-two))

            (or (and (conjunction? formula-one)
                     (conjunction? formula-two))
                (and (disjunction? formula-one)
                     (disjunction? formula-two)))
            (let [args-one (set (args formula-one))
                  args-two (set (args formula-two))]
              (and (= (count args-one) (count args-two))
                   (every?
                    (fn [arg]
                      (some
                       (fn [arg-two]
                         (same? arg arg-two))
                       args-two))
                    args-one)
                   (every?
                    (fn [arg-two]
                      (some
                       (fn [arg]
                         (same? arg-two arg))
                       args-one))
                    args-two)))

            (and (implication? formula-one)
                 (implication? formula-two))

            (and (same? (antecedent formula-one)
                        (antecedent formula-two))
                 (same? (consequent formula-one)
                        (consequent formula-two))))))

(defn gather [formula gather-fn]
  (cond
    (atom? formula)
    (if-let [to-keep (gather-fn formula)]
      (into [to-keep]
            (filter gather-fn formula))
      (filter gather-fn formula))

    (negation? formula)
    (into (gather-fn formula) (gather (negatum formula) gather-fn))

    (or (conjunction? formula)
        (disjunction? formula))
    (into (gather-fn formula)
          (mapcat (fn [subformula]
                 (gather subformula gather-fn))
               (args formula)))

    (implication? formula)
    (into (gather-fn formula)
          (into (gather (antecedent formula) gather-fn)
                (gather (consequent formula) gather-fn)))

    :else
    (gather-fn formula)))

(defn substitute [formula expression replacement]
  (let [sub-internal-fn (fn [subformula]
                          (substitute subformula expression replacement))]
    (if (= formula expression)
      replacement
      (cond (atom? formula)
            (mapv sub-internal-fn formula)

            (negation? formula)
            (negate (substitute (negatum formula) replacement expression))

            (conjunction? formula)
            (into [:and] (mapv sub-internal-fn (args formula)))

            (disjunction? formula)
            (into [:or] (mapv sub-internal-fn (args formula)))

            (implication? formula)
            [:implies
             (substitute (antecedent formula) expression replacement)
             (substitute (consequent formula) expression replacement)]

            :else formula))))

(defn variables [formula]
  (gather formula variable?))
