(ns scotus.query
  (:require
   [clojure.set         :as set]
   [scotus.atomic-proof :as ap]
   [scotus.formula      :as formula]
   [scotus.transitive   :as transitive]
   [scotus.xnf          :as xnf]))


(defn join-results
  [previous-bindings new-bindings]
  (if (seq previous-bindings)
  (let [common-bindings (set/intersection (set (keys (first previous-bindings)))
                                          (set (keys (first new-bindings))))]
    (reduce
     (fn [result previous-binding]
       (let [join-map (select-keys previous-binding common-bindings)
             matches  (filter (fn [binding]
                                (= (select-keys binding common-bindings)
                                   join-map)) new-bindings)
             join     (map (partial merge previous-binding) matches)]
         (concat result join)))
     []
     previous-bindings))
  new-bindings))

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


(query '[:and ["subclass_of" ?x "pet"] ["subclass_of" ?x "mammal"]] "universal")

(query '["subclass_of" "pet" "mammal"] "universal")
