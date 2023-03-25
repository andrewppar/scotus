(ns scotus.xnf
  (:require [scotus.formula :as form]))

;; Implication Out
(defmulti implication-out
  {:arglists '([formula])}
  (fn [formula]
    (form/kind formula))
  :hierarchy form/hierarchy)

(defmethod implication-out :atomic
  [formula]
  formula)

(defmethod implication-out :not
  [formula]
  (-> formula
      form/negatum
      implication-out
      form/lnot))

(defmethod implication-out :junction
  [formula]
  (let [operator (form/main-operator formula)]
    (->> formula
         form/juncts
         (map implication-out)
         (form/junction operator))))

(defmethod implication-out :implies
  [formula]
  (let [ant (implication-out (form/antecedent formula))
        con (implication-out (form/consequent formula))]
    (form/lor (form/lnot ant) con)))

;; Negation In

(defmulti negation-in
  {:arglists '([formula])}
  (fn [formula]
    (form/kind formula)))

(defmethod negation-in :atomic
  [formula]
  formula)

(defmethod negation-in :not
  [formula]
  (let [neg (form/negatum formula)]
    (if (form/negation? neg)
      (negation-in (form/negatum neg))
      (negation-in neg))))

(defmethod negation-in :junction
  [formula]
  (let [operator (form/main-operator formula)]
    (->> formula
         form/juncts
         (map negation-in)
         (form/junction (form/dual operator)))))

(defmethod negation-in :implies
  [formula]
  (let [ant (form/antecedent formula)
        con (form/consequent formula)]
    (form/land ant (form/lnot con))))

(defmulti conjunction-in
  {:arglists '([formula])}
  (fn [formula]
    (form/kind formula)))

(defmethod conjunction-in :atom
  [formula]
  formula)

(defmethod conjunction-in :not
  [formula]
  (-> formula
      form/negatum
      conjunction-in
      form/lnot))

;;(defmethod conjunction-in :and
;;  [formula]
;;  (
