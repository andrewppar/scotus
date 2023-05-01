(ns scotus.transact
  (:require [scotus.database         :as db]
            [scotus.formula.formula  :as formula]
            [scotus.xnf              :as xnf]))

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
        {:atom [cnf]}
        (formula/negation? cnf)
        {:neg [cnf]}
        :else
        (ex-info (format "Formula %s is not CNF" cnf)
                 {})))

(defn add-context-asserts [assertion-groups]
  (if-let [instances (get assertion-groups "instance")]
    (let [specs (->> instances
                     (filter
                      #(= (second (formula/args %)) "context"))
                     (map #(first (formula/args %)))
                     (map
                      (fn [context]
                        `["subcontext_of" ~context "universal"])))]
      (update assertion-groups "instance" concat specs))
    assertion-groups))


(defn add-assertions [formulas polarity context asserter]
  ;; we don't support rules yet
  (when (or (= polarity :atom)
            (= polarity :neg))
    (let [atoms     (case polarity
                      :atom formulas
                      :neg  (mapv formula/negatum formulas))
          groups   (add-context-asserts
                    (group-by formula/predicate formulas))
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

(defn retract! [formula contexts]
  (let [formula-type (formula/formula-type formula)]
    (if (contains? #{:atom :neg} formula-type)
      (let [predicate (formula/literal-predicate formula)
            args      [(formula/literal-args formula)]
            negated?  (= formula-type :neg)]
        ;;TODO: This makes too many io calls - fix it
        (map
         (fn [context]
           (db/delete-rows predicate context args negated?))
         contexts))
      ;; We don't support rules yet
      nil
      )))





(comment
  (assert! (formula/and
            ["subclass_of" "human" "person"])
           "anparisi" "universal")

  (assert! ["transitive_arg" "subclass_of" "superclass" "subclass_of" "subclass" "superclass"] "anparisi" "universal")
  (assert! ["transitive_arg" "instance" "class" "subclass_of" "subclass" "superclass"] "anparisi" "universal")

  )
