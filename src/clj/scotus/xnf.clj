(ns scotus.xnf
  (:require [scotus.formula :as form]))

;; Implication Out
(defmulti implication-out
  {:arglists '([formula])}
  (fn [formula]
    (form/formula-type formula))
  :hierarchy form/hierarchy)

(defmethod implication-out :atomic
  [formula]
  formula)

(defmethod implication-out :not
  [formula]
  (form/not (->> formula form/negatum implication-out)))

(defmethod implication-out :junction
  [formula]
  (->> formula
       form/juncts
       (map implication-out)
       (form/junction (form/main-operator formula))))

(defmethod implication-out :implies
  [formula]
  (let [ant (implication-out (form/antecedent formula))
        con (implication-out (form/consequent formula))]
    (form/or (form/not ant) con)))

;; Negation In

(defmulti negation-in
  {:arglists '([formula])}
  (fn [formula]
    (form/formula-type formula))
  :hierarchy form/hierarchy)

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
  (->> formula
       form/juncts
       (map negation-in)
       (form/junction (form/main-operator formula))))

(defmethod negation-in :if
  [formula]
  (let [ant (form/antecedent formula)
        con (form/consequent formula)]
    (form/and ant (form/not con))))

(defn add-each-to-each
  [blocks to-add]
  (reduce
   (fn [result item]
     (apply conj result (map (fn [block] (conj block item)) blocks)))
   []
   to-add))

(defmulti junction-in
  {:arglists '([transform-type formula])}
  (fn [transform-type formula]
    [transform-type (form/formula-type formula)])
  :hierarchy form/hierarchy)

(defmethod junction-in [:junction :atomic]
  [_ formula]
  formula)

(defmethod junction-in [:junction :not]
  [transform-type formula]
  (->> formula
       form/negatum
       (junction-in transform-type)
       form/not))

(defn junction-in-same-polarity
  [transform-type formula]
  (let [formula-type (form/formula-type formula)
        subformulas  (map (fn [subformula]
                            (junction-in transform-type subformula))
                          (form/juncts formula))
        dual-test-fn (case formula-type
                       :or  form/conjunction?
                       :and form/disjunction?)
        [same-juncts dual-juncts] (reduce
                                   (fn [[same dual] formula]
                                     (if (dual-test-fn formula)
                                       [same (conj dual formula)]
                                       [(conj same formula) dual]))
                                   [#{} #{}]
                                   subformulas)]
    (if (seq dual-juncts)
      (->> dual-juncts
           (map form/juncts)
           (reduce add-each-to-each [same-juncts])
           (map
            (fn [dual-junct] (form/junction formula-type dual-junct)))
           (form/junction (form/dual formula-type)))
      (form/junction formula-type same-juncts))))

(defmethod junction-in [:and :and]
  [transform-type formula]
  (junction-in-same-polarity transform-type formula))

(defmethod junction-in [:or :or]
  [transform-type formula]
  (junction-in-same-polarity transform-type formula))

(defn junction-in-dual-polarity
  [transform-type formula]
  (->> formula
       form/juncts
       (map
        (fn [subformula] (junction-in transform-type subformula)))
       (form/junction (form/formula-type formula))))

(defmethod junction-in [:and :or]
  [transform-type formula]
  (junction-in-dual-polarity transform-type formula))

(defmethod junction-in [:or :and]
  [transform-type formula]
  (junction-in-dual-polarity transform-type formula))

(defmethod junction-in [:junction :implies]
  [transform-type formula]
  (let [ant (form/antecedent formula)
        con (form/consequent formula)]
    (form/implies
     (junction-in transform-type ant)
     (junction-in transform-type con))))

(defn conjunction-in
  [formula]
  (junction-in :and formula))

(defn disjunction-in
  [formula]
  (junction-in :or formula))

(defmulti collapse-juncts
  {:arglists '([formula])}
  (fn [formula]
    (form/formula-type formula))
  :hierarchy form/hierarchy)

(defmethod collapse-juncts :atomic
  [formula]
  formula)

(defmethod collapse-juncts :not
  [formula]
  (->> formula form/negatum collapse-juncts form/not))

(defn collapse-juncts-for-junction
  [formula junction-type test-fn]
  ;; TODO: Do this instead of a reduce aboce
  (let [groups (->> formula
                    form/juncts
                    (map collapse-juncts)
                    (group-by test-fn))
        collapse-juncts (get groups true)
        new-juncts      (concat (map form/juncts collapse-juncts)
                                (get groups false))]
    (form/junction junction-type new-juncts)))

(defmethod collapse-juncts :junction
  [formula]
  (let [formula-type (form/formula-type formula)
        test-fn      (case formula-type
                       :and form/conjunction?
                       :or  form/disjunction?)]
    (collapse-juncts-for-junction formula formula-type test-fn)))

(defmethod collapse-juncts :implies
  [formula]
  (form/implies (collapse-juncts (form/antecedent formula))
                (collapse-juncts (form/consequent formula))))
