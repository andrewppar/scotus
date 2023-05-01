(ns scotus.query
  (:require
   [clojure.set                       :as set]
   [scotus.atomic-proof               :as ap]
   [scotus.formula.formula            :as formula]
   [scotus.query.canonicalize         :as cz]
   [scotus.transitive                 :as transitive]
   [scotus.xnf                        :as xnf]))

(defn conjunction [conjuncts contexts]
  (reduce
   (fn [bindings formula]
     (ap/proof formula bindings contexts))
   #{}
   (cz/sort-conjuncts conjuncts)))

(defn query [formula context]
  (let [contexts (transitive/subcontext context)
        ;; canonicalize?
        dnf      (xnf/dnf formula)]
    ;;; need wff to error if there's a problem
    ;;; the errors now are not useful
    (cond
      (or (formula/atomic? dnf)
          (formula/negation? dnf))
      (ap/proof dnf [] contexts)

      (formula/conjunction? dnf)
      (conjunction (formula/juncts dnf) contexts)

      ;; disjunction
      :else
      (->> dnf
           formula/juncts
           ;; pmap
           (map
            (fn [subformula]
              (conjunction (formula/juncts subformula) contexts)))
           (apply set/union)))))
