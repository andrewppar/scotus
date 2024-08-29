(ns scotus.query.resolution
  (:require
   [scotus.logic.class :as subclass]
   [scotus.query.binding :as binding]
   [scotus.query.binding :as binding]
   [scotus.query.transitivity :as transitivity]
   [scotus.syntax.formula :as f]))

(defn r-inequality [literal bindings _ compare-fn justification?]
  (set
   (keep
    (fn [binding]
      (let [formula (binding/formula-apply binding literal)
            args (f/literal-args formula)]
        (when (apply compare-fn args)
          (if justification?
            (update binding :justification (fnil conj #{}) formula)
          binding))))
    bindings)))

(defn r-<= [literal bindings context justification?]
  (r-inequality literal bindings context <= justification?))

(defn r->= [literal bindings context justification?]
  (r-inequality literal bindings context >= justification?))

(defn r-< [literal bindings context justification?]
  (r-inequality literal bindings context < justification?))

(defn r-> [literal bindings context justification?]
  (r-inequality literal bindings context > justification?))

(defn r-subclass-of-traverse [bindings variable value direction context justification?]
  (let [classes (case direction
                  :subclass
                  (subclass/subclasses
                   value
                   :context context
                   :justification? justification?)
                  :superclass
                  (subclass/superclasses
                   value
                   :context context
                   :justification? justification?))]
    (reduce
     (fn [acc class-value]
       (cond->  {variable class-value}
         justification? (assoc :justification
                               (-> classes
                                   meta
                                   (get-in [:justification class-value])
                                   first))
         true (binding/expand-all acc)))
     bindings
     classes)))

(defn r-formula-subclass-of [formula query bindings context justification?]
  (let [args (f/args formula)
        subclass (first args)
        superclass (second args)]
    (cond (f/fully-bound? formula)
          (let [found? (subclass/subclass?
                        subclass superclass
                        :context context
                        :justification? justification?)]
            (when (seq found?)
              (keep
               (fn [binding]
                 (when (= formula (binding/formula-apply binding query))
                   (if justification?
                     (update binding :justification (fnil into #{})
                             (get (meta found?) :justification))
                     binding)))
               bindings)))

          (and (f/variable? subclass)
               (f/variable? superclass))
          (let [asserts (transitivity/get-transitive-asserts formula context)]
            ;; todo we need some logging
            (transitivity/transitivity bindings formula asserts context justification?))

          (f/variable? subclass)
          (r-subclass-of-traverse
           bindings subclass superclass :subclass context justification?)

          (f/variable? superclass)
          (r-subclass-of-traverse
           bindings superclass subclass :superclass context justification?))))

(defn r-subclass-of [literal bindings context justification?]
  (let [formulas (binding/formula-apply-all literal bindings)]
    (reduce
     (fn [acc formula]
       (into acc
             (r-formula-subclass-of formula literal bindings context justification?)))
     #{}
     formulas)))


(def resolution-modules
  [{:name :<=
    :test-fn (fn [literal bindings]
               (and (= (f/predicate literal) "<=")
                    (every?
                     f/fully-bound?
                     (binding/formula-apply-all literal bindings))))
    :resolve-fn r-<=}
   {:name :>=
    :test-fn (fn [literal bindings]
               (and (= (f/predicate literal) ">=")
                    (every?
                     f/fully-bound?
                     (binding/formula-apply-all literal bindings))))
    :resolve-fn r->=}
   {:name :<
    :test-fn (fn [literal bindings]
               (and (= (f/predicate literal) "<")
                    (every?
                     f/fully-bound?
                     (binding/formula-apply-all literal bindings))))
    :resolve-fn r-<}
   {:name :>
    :test-fn (fn [literal bindings]
               (and (= (f/predicate literal) ">")
                    (every?
                     f/fully-bound?
                     (binding/formula-apply-all literal bindings))))
    :resolve-fn r->}
   {:name :subclass_of
    :test-fn (fn [literal _]
               (= (f/predicate literal) "subclass_of"))
    :resolve-fn r-subclass-of}])

(defn ? [literal bindings]
  (->> resolution-modules
       (some (fn [{:keys [test-fn]}] (test-fn literal bindings)))
       boolean))

(defn get-fns [literal bindings]
  (filter
   (fn [{:keys [test-fn]}] (test-fn literal bindings))
   resolution-modules))
