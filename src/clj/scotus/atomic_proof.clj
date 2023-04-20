(ns scotus.atomic-proof
  (:require [scotus.database   :as db]
            [scotus.formula    :as formula]
            [scotus.transitive :as transitive]))


(defmulti atomic-proof
  "Prove an atomic formula"
  {:arglists '([predicate formulas])}
  (fn [predicate _]
    (if (formula/predicate? predicate)
      predicate
      (throw
       (ex-info (format "%s is not a predicate" predicate)
                {:caused-by `(not (formula/predicate? ~predicate))})))))

;;(defmulti
