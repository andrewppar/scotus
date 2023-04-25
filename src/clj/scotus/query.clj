(ns scotus.query
  (:require
   [clojure.set :as set]
   [scotus.atomic-proof :as ap]
   [scotus.formula      :as formula]
   [scotus.transitive   :as transitive]
   [scotus.xnf :as xnf]))


(defn conjunction [conjuncts contexts]
  (let [conjunction-groups (vals (group-by formula/signature conjuncts))]
    (reduce
     (fn [results group]
       (join-results results (ap/atomic-proof group contexts)))
     #{}
     conjunction-groups)))

(defn query [formula context]
  (let [contexts (transitive/subcontext context)
        ;; canonicalize?
        dnf      (xnf/dnf formula)]
    (cond
      (or (formula/atomic? dnf)
          (formula/negation? dnf))
      (ap/atomic-proof [dnf] contexts)

      (formula/conjunction? dnf)
      (conjunction (formula/juncts dnf) contexts)

      ;; disjunction
      :else
      (->> dnf
           formula/juncts
           (map (comp conjunction formula/juncts))
           (apply set/union)))))
