(ns scotus.transitive
  (:require [clojure.set :as set]
            [scotus.database :as db]
            [scotus.state :as state]))


;; predicate must be a two place predicate; should we enforce that here?
;; That seems like a caller's responsibility at this point
(defn transitive-closure-up
  "Find the forward transitive closure of `arg` under `predicate`
  for `contexts`.

  NOTE: This assumes that `predicate` is a two-place predicate and
  that the transitivity moves from the first to the second arg."
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
  (transitive-closure-up ["persian"] "subclass" "subclass_of" "superclass" ["household" "nature"]))
