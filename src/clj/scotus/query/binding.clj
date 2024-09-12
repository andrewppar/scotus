(ns scotus.query.binding
  (:require
   [clojure.string :as string]
   [scotus.syntax.formula :as f]))

(defn formula-apply [binding-map formula]
 ;; (when (f/formula? formula) ;; add the ability to optinally type check
  (cond
    (f/atom? formula)
    (mapv (fn [arg] (get binding-map arg arg)) formula)

    (f/negation? formula)
    (into [:not] (formula-apply binding-map (f/negatum formula)))

    (f/conjunction? formula)
    (into [:and]
          (map (partial formula-apply binding-map) (f/args formula)))

    (f/disjunction? formula)
    (into [:or]
          (map (partial formula-apply binding-map) (f/args formula)))

    (f/implication? formula)
    [:implies
     (formula-apply binding-map (f/antecedent formula))
     (formula-apply binding-map (f/consequent formula))]))

(defn formula-apply-all [formula binding-maps]
  (->> (or (seq binding-maps) [{}])
       (into [])
       (mapv
        (fn [binding-map] (formula-apply binding-map formula)))))

(defn combine [{justification-one :justification :as binding-map-one}
               {justification-two :justification :as binding-map-two}]
  (let [new-justification (into justification-one justification-two)
        result (merge binding-map-one binding-map-two)]
    (if new-justification
      (assoc result :justification new-justification)
      result)))

(defn without-justification [binding]
  (dissoc binding :justification))

(defn find [binding-maps search-key search-value]
  (some
   (fn [m] (when (= (get m search-key) search-value) m))
   binding-maps))

(defn find-all [binding-maps search-key search-value]
  (filter
   (fn [m] (= (get m search-key) search-value))
   binding-maps))

(defn expansion? [binding-one binding-two]
  (let [binding-one-test (dissoc binding-one :justification)
        binding-two-test (dissoc binding-two :justification)]
    (every?
     (fn [[var one-value]]
       (let [two-value (get binding-two-test var)]
         (or (not two-value)
             #_(not= var :justification)
             (= two-value one-value))))
     binding-one-test)))

(defn expand [binding-one binding-two]
  (when (expansion? binding-one binding-two)
    (combine binding-one binding-two)))

(defn expand-all [binding-map binding-maps]
  (set
   (or (seq (keep (partial expand binding-map) binding-maps))
       (into #{binding-map} binding-maps))))

(defn expand-all-with-all-internal
  [binding-maps-one binding-maps-two]
  (set
   (mapcat
    (fn [binding-map]
      (expand-all binding-map binding-maps-one))
    binding-maps-two)))

(defn expand-all-with-all
  [binding-maps-one binding-maps-two]
  (set
  (if (not (seq binding-maps-two))
    binding-maps-one
    (expand-all-with-all-internal binding-maps-one binding-maps-two))))

(defn expand-all-filtering
  [original-bindings filtering-bindings]
  (if (seq original-bindings)
    (reduce
     (fn [result original]
       (->> filtering-bindings
            (keep (partial expand original))
            (into result)))
     #{}
     original-bindings)
    filtering-bindings))

(defn remove-unwanted-vars [binding]
  (reduce-kv
   (fn [acc var value]
     (if (string/starts-with? (name var) "??")
       acc
       (assoc acc var value)))
   {}
   binding))
