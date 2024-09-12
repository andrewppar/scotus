(ns scotus.syntax.xnf
  (:require
   [clojure.set :as set]
   [scotus.syntax.formula :as f]))



(defn other-side [side]
  (case side
    :right :left
    :left :right))

(defn particular-junction [sequent test-fn side]
  #{(update sequent side
            (fn [formulas]
              (set
               (mapcat
                (fn [formula]
                  (if (test-fn formula)
                    (f/args formula)
                    [formula]))
                formulas))))})

(defn conjunction-left [sequent]
  (particular-junction sequent f/conjunction? :left))

(defn disjunction-right [sequent]
  (particular-junction sequent f/disjunction? :right))

(defn universal-junction [sequent test-fn side]
  (let [{junctions true others false} (group-by test-fn (get sequent side))]
    (->> junctions
         (reduce
          (fn [acc junction]
            (for [formulas acc
                  junct (f/args junction)]
              (conj formulas junct)))
          #{(into #{} others)})
         (map (fn [new-side] (assoc sequent side new-side)))
         set)))

(defn conjunction-right [sequent]
  (universal-junction sequent f/conjunction? :right))

(defn disjunction-left [sequent]
  (universal-junction sequent f/disjunction? :left))

(defn negation-left [{:keys [left right]}]
  (let [{negations true others false} (group-by f/negation? left)]
    #{{:left others :right (into right (map f/negatum negations))}}))

(defn negation-right [{:keys [left right]}]
  (let [{negations true others false} (group-by f/negation? right)]
    #{{:left (into left (map f/negatum negations)) :right others}}))

(defn transform [sequents rule]
  (mapcat rule sequents))

(defn sequent-done? [{:keys [left right]}]
  (and
   (every? f/atom? left)
   (every? f/atom? right)))

(defn proof-done? [sequents]
  (every? sequent-done? sequents))

(defn identity? [{:keys [left right]}]
  (boolean (seq (set/intersection (set left) (set right)))))

(defn prove [formula side]
  (let [start (assoc {:left #{} :right #{}} side #{formula})
        rules [conjunction-left
               conjunction-right
               disjunction-left
               disjunction-right
               negation-left
               negation-right]]
    (loop [sequents #{start}]
      (if (proof-done? sequents)
        sequents
        (recur (->> rules
                    (reduce transform sequents)
                    (filter (complement identity?))))))))

(defn implication-out [formula]
  (cond (f/atom? formula)
        formula

        (f/negation? formula)
        [:not (implication-out (f/negatum formula))]

        (f/conjunction? formula)
        (into [:and] (map implication-out (f/args formula)))

        (f/disjunction? formula)
        (into [:or] (map implication-out (f/args formula)))

        (f/implication? formula)
        [:or [:not (implication-out (f/antecedent formula))]
         (implication-out (f/consequent formula))]))

(defn lit-true-or-false-in-every? [literal sequents]
  (let [containment-maps (map
                          (fn [{:keys [left right]}]
                            {:left  (contains? (set left) literal)
                             :right (contains? (set right) literal)})
                          sequents)]
    (and
     (some (fn [{:keys [left]}] left) containment-maps)
     (some (fn [{:keys [right]}] right) containment-maps))))

(defn some-lit-true-and-false-in-every? [sequents]
  (let [{:keys [left right]} (first sequents)]
    (some
     (fn [literal]
       (lit-true-or-false-in-every? literal sequents))
     (concat left right))))

(defn xnf [formula xnf-type]
  (let [one (implication-out formula)
        sequents (prove one (case xnf-type :dnf :left :cnf :right))]
    (cond (some-lit-true-and-false-in-every? sequents)
          (case xnf-type
            :dnf :tautology
            :cnf :contradiction)

          (seq sequents)
          (let [junctions (map
                           (fn [{:keys [left right]}]
                             (let [negations (map
                                              f/negate
                                              (case xnf-type :dnf right :cnf left))
                                   forms (concat negations (case xnf-type
                                                             :dnf left
                                                             :cnf right))]
                               (if (= (count forms) 1)
                                 (first forms)
                                 (case xnf-type
                                   :dnf (f/conjoin forms)
                                   :cnf (f/disjoin forms)))))
                           sequents)]
            (if (= (count junctions) 1)
              (first junctions)
              (case xnf-type
                :dnf (f/disjoin junctions)
                :cnf (f/conjoin junctions))))

          :else
          (case xnf-type
            :dnf :contradiction
            :cnf :tautology))))

(defn dnf [formula]
  (xnf formula :dnf))

(defn dnf? [formula]
  (and (f/disjunction? formula)
       (every? (fn [subformula]
                 (and (f/conjunction? subformula)
                      (every?
                       (fn [subsubformula]
                         (f/literal? subsubformula))
                       (f/args subformula))))
               (f/args formula))))

(defn cnf [formula]
  (xnf formula :cnf))

(defn cnf? [formula]
  (and (f/conjunction? formula)
       (every? (fn [subformula]
                 (and (f/disjunction? subformula)
                      (every?
                       (fn [subsubformula]
                         (f/literal? subsubformula))
                       (f/args subformula))))
               (f/args formula))))


(comment
  (cnf '[:implies ["subclass_of" ?x ?y] [:and ["instance" ?x "class"]
                                         ["instance" ?y "class"]]])
)
