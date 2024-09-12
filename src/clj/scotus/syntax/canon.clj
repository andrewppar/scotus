(ns scotus.syntax.canon
  (:require [scotus.syntax.formula :as f]
            [scotus.syntax.xnf :as xnf]))

(def purposes #{:query :assert})

(defn ^:private id-literal-gather [subformula]
  (when (and (not (f/negation? subformula))
                (= (f/literal-predicate subformula) "="))
       subformula))

(defn ^:private id-literals
  [formula]
  (f/gather formula id-literal-gather))

;;(defn replace-equals [xnf]
;;  (if (xnf/dnf? xnf)
;;    (into [:or]
;;          (mapv
;;           (fn [conjunction]
;;             (let [id-lits
;;                   (filter
;;                    (fn [literal]
;;                      (and (not (f/negation? literal))
;;                           (= (f/literal-predicate literal) "=")))
;;                    (f/args conjunction))]




(defn enact [formula purpose]
  (case purpose
    :assert (xnf/cnf formula)
    :query (xnf/dnf formula)))
