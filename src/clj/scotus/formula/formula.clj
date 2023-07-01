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

(defn args [formula]
  (into [] (rest formula)))

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
  (predicate? (main-operator formula)))

(defaccessor predicate [atomic-formula] :atomic
  (main-operator atomic-formula))

(defmulti maybe-nested-args
  {:arglists '([formula])}
  (fn [formula]
    (predicate formula)))

(defmethod maybe-nested-args :default
  [formula]
  (args formula))

(defmethod maybe-nested-args "asserted"
  [formula]
  (-> formula second args))

(defmethod maybe-nested-args "unknown"
  [formula]
  (-> formula second args))

(defmulti maybe-nested-predicate
  {:arglists '([formula])}
  (fn [formula]
    (predicate formula)))

(defmethod maybe-nested-predicate :default
  [formula]
  (predicate formula))

(defmethod maybe-nested-predicate "asserted"
  [formula]
  (-> formula second predicate))

(defmethod maybe-nested-predicate "unknown"
  [formula]
  (-> formula second predicate))

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

(defn ground?
  [formula]
  (and
   (atomic? formula)
   (every?
    (fn [arg] (not (variable? arg)))
    (args formula))))

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

(defn add-juncts [base-junction new-junction]
  (let [base-type (formula-type base-junction)
        new-type  (formula-type new-junction)]
    (when-not (= base-type new-type)
      (throw
       (ex-info
        (format "Cannot add %s juncts to %s" new-type base-type)
        {:caused-by
         `(= (formula-type ~base-junction) (formula-type ~new-junction))})))
    (reduce
     (fn [acc junct] (conj acc junct))
     base-junction
     (juncts new-junction))))

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

(comment
  ;; Experimental Stuff

  (defn simple-recursive-property [formula property]
    (case (formula-type formula)
      :atomic   (property formula)
      :not      (property (negatum formula))
      :junction (every? property (juncts formula))
      :implies  (and
                 (property (antecedent formula))
                 (property (consequent formula)))))

  (defn fully-bound?
    "Check whether a formula is fully bound."
    [formula]
    (simple-recursive-property
     formula
     (fn [expression]
       (or (not (atomic? expression))
           (every? (fn [arg] (not (variable? arg))) (args formula))))))


;;; I Think there's a way to use the above function here.
;;; TEST THIS
  (defn gather [gathered test-fn gather-fn expression]
    (let [acc (if (test-fn expression)
                (conj gathered (gather-fn expression))
                gathered)]
      (case (formula-type formula)
        :atomic   (reduce
                   (fn [result other-expression]
                     (gather result test-fn gather-fn other-exprssion))
                   acc
                   formula)
        :not      (gather acc test-fn gather-fn (negatum formula))
        :junction (reduce
                   (fn [result subfomula]
                     (gather result test-fn gather-fn subformula))
                   acc
                   (junct formula))
        :implies (-> acc
                     (gather test-fn gather-fn (antecedent expression))
                     (gather test-fn gather-fn (consequent expression))))))
  )
