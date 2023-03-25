(ns scotus.formula
  (:require
   [clojure.string :as string]
   [scotus.state :as state]))

(def hierarchy (atom (make-hierarchy)))

(swap! hierarchy derive :atomic   :formula)
(swap! hierarchy derive :not      :junction)
(swap! hierarchy derive :and      :junction)
(swap! hierarchy derive :or       :formula)
(swap! hierarchy derive :junction :formula)
(swap! hierarchy derive :implies  :formula)

(defn main-operator
  [formula]
  (first formula))

(defn operator-type
  [operator]
  (if (string? operator) :atomic operator))

(defn kind
  [formula]
  (operator-type (main-operator formula)))

(defn dual
  "Get the dual formula kind of `kind`"
  [kind]
  (case kind
    :atomic nil
    :not    :not
    :and    :or
    :or     :and
    :implies nil))

(defn ^:private maybe-accessor-error
  [object test-fn object-type accessor-type formula-kind]
  (when (or (not (= (kind object) formula-kind))
            (not (test-fn object)))
    (throw
     (ex-info (format "%s is not %s" object object-type)
              {:caused-by
               (format "Only %s has %s." object-type accessor-type)}))))

(defmacro defaccessor [accessor-name args formula-kind & body]
  (let [formula-arg          (first args)
        object-name          (-> formula-arg
                                 name
                                 (string/replace "-" " "))
        accessor-name-string (-> accessor-name
                                 name
                                 (string/replace "-" " "))]
    `(defn ~accessor-name
       ~args
       (maybe-accessor-error
        ~formula-arg formula? ~object-name ~accessor-name-string ~formula-kind)
       (do
         ~@body))))

(defmulti make
  {:arglists '([operator & args]
               [operator predicate & args]
               [operator formula]
               [operator antecedent consequent])}
  (fn [operator & _]
    (operator-type operator))
  :hierarchy hierarchy)

(defmulti formula?
  {:arglists '([formula])}
  (fn [formula]
    (kind formula))
  :hierarchy hierarchy)

(defmacro def-formula-predicate [pred-name formula-kind]
  `(defn ~pred-name [formula#]
     (and (= (kind formula#) ~formula-kind)
          (formula? formula#))))

;;; atoms

(defmethod make :atomic
  [_ predicate & args]
  `[~predicate ~@args])

(defn predicate?
  "A string that represents a predicate of the system."
  [object]
  (and
   (string? object)
   (.contains (keys (state/predicate-index)) object)))

(defmethod formula? :atomic
  [formula]
  (and
   (predicate? (main-operator formula))
   (every? string? (rest formula))))

(defaccessor args [atomic-formula] :atomic
  (rest atomic-formula))

(defaccessor predicate [atomic-formula] :atomic
  (main-operator atomic-formula))

(def-formula-predicate atomic? :atomic)

;; negation

(defmethod make :not
  [_ formula]
  [:not formula])

(defmethod formula? :not
  [formula]
  (formula? (second formula)))

(defaccessor negatum [negation] :not
  (second negation))

(def-formula-predicate negation? :not)

;; junction

(defmethod make :junction
  [operator & formulas]
  `[~operator ~@formulas])

(defmethod formula? :and
  [formula]
  (and
   (= (kind formula) :and)
   (every? formula? (rest formula))))

(defmethod formula? :junction
  [formula]
  (every? formula (rest formula)))

(defaccessor juncts [junction] :junction
  (rest junction))

(def-formula-predicate conjunction? :and)
(def-formula-predicate disjunction? :or)

;; conditional

(defmethod make :if
  [_ antecedent consequent]
  [:implies antecedent consequent])

(defmethod formula? :if
  [formula]
  (and
   (formula? (nth formula 1))
   (formula? (nth formula 2))))

(defaccessor antecedent [conditional] :if
  (nth conditional 1))

(defaccessor consequent [conditional] :if
  (nth conditional 2))

(def-formula-predicate rule? :if)
