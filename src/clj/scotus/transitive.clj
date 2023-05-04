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
          new-todo   (->> next-vals
                        (map
                         (fn [assert]
                           (get assert transitive-key)))
                        (filter
                         (fn [value]
                           (not (contains? result value))))
                        set)
          new-result (set (set/union result todo))]
      (if (seq new-todo) (recur new-todo new-result) new-result)))))

(defn subcontext [context]
  (if (= context "universal")
    :universal
    (closure
     [context] "supercontext" "subcontext_of" "subcontext" ["universal"])))


(defn instance?
  "Check whether `item` is an instance of `scotus-class`"
  [item scotus-class contexts]
  (let [start (->> (db/lookup-rows "instance" contexts [[item nil]])
                 (map (fn [row] (get row :instance/class)))
                 set)]
    (if (contains? start scotus-class)
      true
      (loop [todo start
             seen #{}]
        (let [new-classes (->> todo
                             (mapv (fn [subclass] [subclass nil]))
                             (db/lookup-rows "subclass_of" contexts)
                             (map (fn [row]
                                    (get row :subclass_of/superclass)))
                             set)
              found?     (contains? new-classes scotus-class)]
          (if found?
            true
            (let [new-todo (remove
                            (fn [item] (contains? seen item))
                            new-classes)]
              (if (seq new-todo)
                (recur new-todo (set (concat seen new-todo)))
                false))))))))







(comment

  (subcontext "universal")

  (db/lookup-rows "subclass_of" ["household" "nature"] [[]])
  (closure ["mammal"]  "superclass" "subclass_of" "subclass" ["household" "nature"]);; => #{"persian" "dog" "chihuahua" "mammal" "golden retriever" "cat"}
)
