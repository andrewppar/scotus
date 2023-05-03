(ns scotus.transitive
  (:require [clojure.set :as set]
            [scotus.database :as db]
            [scotus.state :as state]))


(defn closure "Find the forward transitive closure of `start-values`
  for traversing `predicate` from `start-arg` over `transitive-arg`
  for `contexts`"
  [start-values start-arg predicate transitive-arg contexts]
  (let [args           (state/table-args predicate)
        start-idx      (.indexOf args start-arg)
        template       (into [] (repeat (count args) nil))
        transitive-key (keyword predicate transitive-arg)]
  (loop [todo        start-values
         result      #{}]
    (let [next-specs (mapv (fn [arg] (assoc template start-idx arg)) todo)
          next-vals  (db/lookup-rows predicate contexts next-specs)
          ;; consider making this a transduce
          new-todo   (->> next-vals
                          (map
                           (fn [assert]
                             (get assert transitive-key)))
                          (filter
                           (fn [value]
                             (not (contains? result value))))
                          set)
          new-result (set/union result todo)]
      (if (seq new-todo) (recur new-todo new-result) new-result)))))

(defn subcontext [context]
  (if (= context "universal")
    :universal
    (closure
     [context] "supercontext" "subcontext_of" "subcontext" ["universal"])))


(defn instance?
  "Check whether `item` is an instance of `scotus-class`"
  [item scotus-class contexts]
  (let [start (map (fn [row]
                     (get row :instance/class))
                   (db/lookup-rows "instance" contexts [[item nil]]))]
    start))

(instance? "311" "sensor" :universal)


(comment

  (subcontext "universal")

  (db/lookup-rows "subclass_of" ["household" "nature"] [[]])
  (closure ["mammal"]  "superclass" "subclass_of" "subclass" ["household" "nature"]);; => #{"persian" "dog" "chihuahua" "mammal" "golden retriever" "cat"}
)
