(ns scotus.transitive
  (:require [clojure.set :as set]
            [scotus.database :as db]
            [scotus.state :as state]))


;; predicate must be a two place predicate; should we enforce that here?
;; That seems like a caller's responsibility at this point
(defn transitive-closure
  "Find the forward transitive closure of `start-values`
  for traversing `predicate` from `start-arg` over `transitive-arg`
  for `contexts`"
  [start-values start-arg predicate transitive-arg contexts]
  (let [args           (state/get-table-args predicate)
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


(comment

  (db/lookup-rows "subclass_of" ["household" "nature"] [[]])
  (transitive-closure-up ["pet"]  "superclass" "subclass_of" "subclass" ["household" "nature"]))
