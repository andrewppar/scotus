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
  (->> formula
      form/negatum
      implication-out
      form/make :not))

(defmethod implication-out :junction
  [formula]
  (let [operator (form/main-operator formula)]
    (->> formula
         form/juncts
         (map implication-out)
         (apply form/make operator))))

(defmethod implication-out :if
  [formula]
  (let [ant (implication-out (form/antecedent formula))
        con (implication-out (form/consequent formula))]
    (form/make :or (form/make :not ant) con)))

;; Negation In

(defmulti negation-in
  {:arglists '([formula])}
  (fn [formula]
    (form/kind formula))
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
  (let [operator (form/main-operator formula)]
    (->> formula
         form/juncts
         (map negation-in)
         (form/make (form/dual operator)))))

(defmethod negation-in :if
  [formula]
  (let [ant (form/antecedent formula)
        con (form/consequent formula)]
    (form/make :and ant (form/make :not con))))

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
    [transform-type (form/kind formula)])
  :hierarchy form/hierarchy)

(defmethod junction-in [:junction :atomic]
  [_ formula]
  formula)

(defmethod junction-in [:junction :not]
  [transform-type formula]
  (->> formula
       form/negatum
       (junction-in transform-type)
       (form/make :not)))

(defn junction-in-same-polarity
  [transform-type formula]
  (let [formula-type (form/kind formula)
        subformulas  (map (fn [subformula]
                           (junction-in transform-type subformula))
                         (form/juncts formula))
        dual-test-fn (case formula-type
                       :or  form/conjunction?
                       :and form/disjunction?)
        [same-juncts dual-juncts] (reduce
                                   (fn [[same dual] formula]
                                     ;; generalize
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
            (fn [dual-junct]
              (apply form/make formula-type dual-junct)))
           (apply form/make (form/dual formula-type)))
      (apply form/make formula-type same-juncts))))

(defmethod junction-in [:and :and]
  [transform-type formula]
  (junction-in-same-polarity transform-type formula))

(defmethod junction-in [:or :or]
  [transform-type formula]
  (junction-in-same-polarity transform-type formula))

(defn junction-in-dual-polarity
  [transform-type formula]
  (let [formula-type (form/kind formula)]
    (->> formula
         form/juncts
         (map
          (fn [subformula]
            (junction-in transform-type subformula)))
         (apply form/make formula-type))))

(defmethod junction-in [:and :or]
  [transform-type formula]
  (junction-in-dual-polarity transform-type formula))

(defmethod junction-in [:or :and]
  [transform-type formula]
  (junction-in-dual-polarity transform-type formula))

(defmethod junction-in [:junction :if]
  [transform-type formula]
  (let [ant (form/antecedent formula)
        con (form/consequent formula)]
    (form/make :if
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
    (form/kind formula))
  :hierarchy form/hierarchy)

(defmethod collapse-juncts :atomic
  [formula]
  formula)

(defmethod collapse-juncts :not
  [formula]
  (->> formula
      form/negatum
      collapse-juncts
      (form/make :not)))

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
    (apply form/make junction-type new-juncts)))

(defmethod collapse-juncts :junction
  [formula]
  (let [formula-type (form/kind formula)
        test-fn      (case formula-type
                       :and form/conjunction?
                       :or  form/disjunction?)]
    (collapse-juncts-for-junction formula formula-type test-fn)))

(defmethod collapse-juncts :if
  [formula]
  (form/make :if (collapse-juncts (form/antecedent formula))
             (collapse-juncts (form/consequent formula))))
