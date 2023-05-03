(ns scotus.formula.formula
  (:require
   [clojure.string :as str]
   [scotus.state :as state]))

(def hierarchy (atom (make-hierarchy)))


;; If literal means ground we'll have to update this when we start
;; adding rules in.
(swap! hierarchy derive :atomic   :literal)
(swap! hierarchy derive :not      :literal)
(swap! hierarchy derive :literal  :formula)
(swap! hierarchy derive :and      :junction)
(swap! hierarchy derive :or       :junction)
(swap! hierarchy derive :junction :formula)
(swap! hierarchy derive :implies  :formula)

(defn main-operator
  [formula]
  (first formula))

(defn operator-type
  [operator]
  (if (string? operator) :atomic operator))

(defn formula-type
  [formula]
  (operator-type (main-operator formula)))

(defn dual
  "Get the dual formula kind of `kind`"
  [type]
  (case type
    :atomic nil
    :not    :not
    :and    :or
    :or     :and
    :implies nil))

(defn ^:private maybe-accessor-error
  [object test-fn object-type accessor-type formula-kind]
  (when (or (not
             (isa?
              @hierarchy (formula-type object) formula-kind))
            (not (test-fn object)))
    (throw
     (ex-info (format "Syntactic Access Error: %s is not %s"
                      object object-type)
              {:caused-by
               (format "Only %s has %s." object-type accessor-type)}))))

(defmacro defaccessor [accessor-name args formula-kind & body]
  (let [formula-arg          (first args)
        object-name          (-> formula-arg
                                 name
                                 (str/replace "-" " "))
        accessor-name-string (-> accessor-name
                                 name
                                 (str/replace "-" " "))]
    `(defn ~accessor-name
       ~args
       (maybe-accessor-error
        ~formula-arg formula? ~object-name ~accessor-name-string ~formula-kind)
       (do
         ~@body))))

(defn variable?
  [object]
  (clojure.core/and
   (symbol? object)
   (str/starts-with? (name object) "?")))

(defmulti formula?
  {:arglists '([formula])}
  (fn [formula]
    (formula-type formula))
  :hierarchy hierarchy)

(defmacro def-formula-predicate [pred-name formula-kind]
  `(defn ~pred-name [formula#]
     (clojure.core/and (isa? @hierarchy (formula-type formula#) ~formula-kind)
      #_(= (formula-type formula#) ~formula-kind)
          (formula? formula#))))

(defmacro make-predicate [predicate args]
  (let [pred-symbol (symbol (str/replace predicate "_" "-"))
        arg-symbols (mapv symbol args)]
    `(defn ~pred-symbol ~arg-symbols
       [~predicate ~@arg-symbols])))

(comment "This is cool, but not useful unless we can map it over
          the predicate index (see commented form in scotus.core"
         (make-predicate "father_of" ["father" "child"])

         (father-of "Andrew" "Anthony")
         (father-of "Andrew" "George")
         )

;;; atoms

(defn predicate?
  "A string"
  [object]
  (string? object))

(defmethod formula? :atomic
  [formula]
  (clojure.core/and
   (predicate? (main-operator formula))
   (every?
    (some-fn string? variable?)
    (rest formula))))

(defaccessor args [atomic-formula] :atomic
  (rest atomic-formula))

(defaccessor predicate [atomic-formula] :atomic
  (main-operator atomic-formula))

(defn arg [atomic-formula argnum]
  (get atomic-formula argnum))

(defn arg-by-name [arg-name [predicate & args]]
  (-> predicate
      state/table-args
      (zipmap args)
      (get arg-name)))

(defn signature [atomic-formula]
  {:predicate (predicate atomic-formula)
   :arg-signature (reduce
                   (fn [acc arg]
                     (if (variable? arg) (conj acc arg) (conj acc nil)))
                   []
                   (args atomic-formula))})

(def-formula-predicate atomic? :atomic)

;; negation

(defn not [negatum]
  (conj [:not] negatum))

(defmethod formula? :not
  [formula]
  (formula? (second formula)))

(defaccessor negatum [negation] :not
  (second negation))

(def-formula-predicate negation? :not)

(defn variables [formula]
  (case (formula-type formula)
    :atomic (filter variable? (args formula))
    :not    (variables (negatum formula))
    :else   nil))

;; literals

(defn literal-accessor [formula accessor-fn]
  (case (formula-type formula)
    :atomic (accessor-fn formula)
    :neg  (accessor-fn (negatum formula))
    :else nil))

(defn literal-predicate
  [formula]
  (literal-accessor formula predicate))

(defn literal-args
  [formula]
  (literal-accessor formula args))

;; junction

(defmethod formula? :junction
  [formula]
  (every? formula? (rest formula)))

(defaccessor juncts [junction] :junction
  (rest junction))

(defn and [& formulas]
  `[:and ~@formulas])

(defn or [& formulas]
  `[:or ~@formulas])

(defn junction [junction-type formulas]
  (case junction-type
    :and (apply and formulas)
    :or  (apply or formulas)))

(def-formula-predicate junction? :junction)
(def-formula-predicate conjunction? :and)
(def-formula-predicate disjunction? :or)

;; conditional

(defmethod formula? :implies
  [formula]
  (clojure.core/and
   (formula? (nth formula 1))
   (formula? (nth formula 2))))

(defaccessor antecedent [conditional] :implies
  (nth conditional 1))

(defaccessor consequent [conditional] :implies
  (nth conditional 2))

(defn implies [antecedent consequent]
  [:implies antecedent consequent])

(def-formula-predicate rule? :implies)

;;; Other Predicates

(defn literal?
  "A formula that is either atomic or the negation of an atomic."
  [formula]
  (clojure.core/or
   (atomic? formula)
   (clojure.core/and
    (negation? formula)
    (atomic? (negatum formula)))))
