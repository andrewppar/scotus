(ns scotus.transact
  (:require [scotus.database :as db]
            [scotus.formula  :as formula]
            [scotus.xnf      :as xnf]))

(defn split-cnf [cnf]
  (cond (formula/junction? cnf)
        (reduce
         (fn [acc formula]
           (let [update-key (cond (formula/atomic? formula) :atom
                                  (formula/negation? formula) :neg
                                  :else :rule)]
             (update acc update-key (fnil conj #{}) formula)))
         {}
         (formula/juncts cnf))
        (formula/atomic? cnf)
        {:atom cnf}
        (formula/negation? cnf)
        {:neg cnf}
        :else
        (ex-info (format "Formula %s is not CNF" cnf)
                 {})))

(defn add-assertions [formulas polarity context asserter]
  ;; we don't support rules yet
  (when (or (= polarity :atom)
            (= polarity :neg))
    (let [atoms     (case polarity
                      :atom formulas
                      :neg  (mapv formula/negatum formulas))
          groups   (group-by formula/predicate formulas)
          negated? (case polarity :atom false :neg true)]
      (reduce-kv
       (fn [_ predicate to-assert]
         (let [specs    (mapv formula/args to-assert)]
           (db/add-rows predicate context asserter negated? specs)))
       nil
       groups))))

(defn assert!
  [formula asserter context]
  (let [cnf  (xnf/cnf formula)
        {:keys [atom neg rule]} (split-cnf cnf)]
    (add-assertions atom :atom context asserter)
    (add-assertions neg  :neg  context asserter)
    ;; We don't support rules yet
    #_(add-assertions rule :rule context asserter)))

(defn create-predicate!
  [predicate args asserter]
  (apply db/create-table! predicate args)
  (assert! ["instance" predicate "predicate"] asserter "universal"))

(comment
  (assert! (formula/and
            ["subclass_of" "baby" "human"])
           "anparisi"))
