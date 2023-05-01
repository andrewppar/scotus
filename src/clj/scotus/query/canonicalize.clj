(ns scotus.query.canonicalize
  (:require [scotus.formula.formula :as formula]
            [scotus.state           :as state]))

(defn sort-formulas-by [group-and-sort-functions formulas]
  (let [[group-fn sort-fn] (first group-and-sort-functions)
        groups             (group-by group-fn formulas)
        sorted-keys        (sort sort-fn (keys groups))]
    (if-let [next-fns (seq (rest group-and-sort-functions))]
      (mapcat
       (fn [group-key] (sort-formulas-by next-fns (get groups group-key)))
       sorted-keys)
      (mapcat (fn [group-key] (get groups group-key)) sorted-keys))))

(defn sort-conjuncts
  [conjuncts]
  (sort-formulas-by
   [[(comp count formula/variables) <]
    [(comp state/table-count formula/predicate) <]]
   conjuncts))
