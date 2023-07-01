(ns scotus.transitive
  (:require [clojure.set :as set]
            [scotus.assert  :as assert]
            [scotus.database.query :as dbq]
            [scotus.formula.formula :as formula]
            [scotus.state :as state]))

(defn matching-paths [paths value]
  (filter (fn [path] (= (peek path) value)) paths))

(defn closure-internal-return [paths]
  {:result (set (apply set/union paths))
   :justification paths})

(defn closure-internal
  "Find paths that close `start-values`for traversing `predicate`
  from `start-arg` over `transitive-arg`for `contexts`."
  [start-values start-arg predicate transitive-arg contexts]
  (let [args           (state/table-args predicate)
        start-idx      (.indexOf args start-arg)
        template       (into [] (repeat (count args) nil))
        start-key      (keyword predicate start-arg)
        transitive-key (keyword predicate transitive-arg)]
    (loop [paths        (set (map (comp vec list) start-values))]
      (let [next-specs (mapv (fn [arg] (assoc template start-idx arg)) (map last paths))
            next-vals  (dbq/lookup-rows predicate contexts false next-specs)]
        (if-not (seq next-vals)
          (closure-internal-return paths)
          (let [assert-groups (group-by start-key next-vals)
                new-paths  (set (mapcat
                                 (fn [path]
                                   (let [path-end (peek path)
                                         assert-specs (get assert-groups path-end)]
                                     (if (seq assert-specs)
                                       (reduce
                                        (fn [new-ps assert]
                                          (->> transitive-key
                                               (get assert)
                                               (conj path)
                                               (conj new-ps)))
                                        #{}
                                        assert-specs)
                                       #{path})))
                                 paths))]
            (if (= new-paths paths)
              (closure-internal-return paths)
              (recur new-paths))))))))

(defn closure
  "Find the forward transitive closure of `start-values`
  for traversing `predicate` from `start-arg` over `transitive-arg`
  for `contexts`"
  [start-values start-arg predicate transitive-arg contexts]
  (get
   (closure-internal start-values start-arg predicate transitive-arg contexts)
   :result))

(defn closure*
  "Like closure, except that justifications are preserved"
  [start-values start-arg predicate transitive-arg contexts]
  (closure-internal start-values start-arg predicate transitive-arg contexts))

(defn subcontext [context]
  (if (= context "universal")
    :universal
    (-> []
        (conj context)
        (closure
         "supercontext" "subcontext_of" "subcontext" ["universal"])
        (conj "universal"))))

(defn ^:private matching-path [assert-paths terminus]
  (reduce
   (fn [acc path]
     ;; these are all subclass assertions
     (if (= (assert/arg (last path) 2) terminus)
       (reduced path)
       acc))
   []
   assert-paths))

(defn subclass-of-internal [subclass-assert arg-type]
  (if (and (assert/assert? subclass-assert)
           (= (assert/predicate subclass-assert) "subclass_of"))
    (assert/arg subclass-assert (case arg-type :subclass 1 :superclass 2))
    (throw
     (ex-info (format "Cannot get %s from a non-subclass assert" arg-type)
              {:caused-by subclass-assert}))))

(defn subclass [subclass-assert]
  (subclass-of-internal :subclass))

(defn superclass [subclass-assert]
  (subclass-of-internal :superclass))

(defn instance?-internal
  "Check whether `item` is an instance of `scotus-class`

  Optionally return a justification."
  [item scotus-class contexts justification?]
  (let [start (->> (dbq/lookup-rows "instance" contexts false [[item nil]])
                   (mapv (fn [row] (get row :instance/class))))]
    (if (contains? (set start) scotus-class)
      (cond-> {:result true}
        justification?
        (assoc :justification [["instance" item scotus-class]]))
      (loop [todo [start]
             seen #{}]
        ;; TODO: Pull this out into its own function
        (let [new-paths (->> todo
                             (mapv (fn [path] [(superclass (last path)) nil]))
                             (dbq/lookup-rows "subclass_of" contexts true)
                             (reduce
                              (fn [acc {:subclass_of/keys [subclass superclass context justification] :as row }]
                                (if (contains? seen superclass)
                                  acc
                                  (let [path (matching-path todo subclass)
                                        assert (assert/make "subclass_of" [subclass superclass] context false justification)]
                                    (conj acc (conj path assert)))))
                              #{}))
              new-classes (into #{} (map last new-paths))
              found?     (contains? new-classes scotus-class)]
          (if found?
            (cond-> {:result true}
              justification?
              (assoc :justification (matching-path new-paths scotus-class)))
            (let [new-todo (remove
                            (fn [path] (contains? seen (last path)))
                            new-paths)]
              (if (seq new-todo)
                (recur new-todo (set (concat seen new-classes)))
                (cond-> {:result false}
                  justification? (assoc :justification? []))))))))))

(defn instance? [item scotus-class contexts]
  (get
   (instance?-internal item scotus-class contexts false)
   :result))

(defn instance?* [item scotus-class contexts]
  (instance?-internal item scotus-class contexts true))









(comment

  (subcontext "universal")

  (dbq/lookup-rows "subclass_of" ["animals"] false [["mammal" nil]])
  (closure ["mammal"]  "superclass" "subclass_of" "subclass" ["animals"]);; => #{"persian" "dog" "chihuahua" "mammal" "golden retriever" "cat"}

  (instance?* "Ody" "mammal" :universal)
)
