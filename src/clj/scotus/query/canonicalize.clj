(ns scotus.query.canonicalize
  (:require [scotus.formula.formula :as formula]
            [scotus.state           :as state]))

(defn sort-formulas-by
  "Group formulas successively into sub-groups and sort those
   according to the corresponding keys of the groups."
  [group-and-sort-functions formulas]
  (let [[group-fn sort-fn] (first group-and-sort-functions)
        groups             (group-by group-fn formulas)
        sorted-keys        (sort sort-fn (keys groups))]
    (if-let [next-fns (seq (rest group-and-sort-functions))]
      (mapcat
       (fn [group-key] (sort-formulas-by next-fns (get groups group-key)))
       sorted-keys)
      (mapcat (fn [group-key] (get groups group-key)) sorted-keys))))

;; should this also be informed by a graph on the variables?
(defn sort-conjuncts
  [conjuncts]
  (sort-formulas-by
   [;;; Looking for assertions should always come first
    [(comp (fn [pred] (not= pred "asserted")) formula/predicate) nil]
    ;;; Looking for unknown things should alwasy come last
    [(comp (fn [pred] (= pred "unknown")) formula/predicate) nil]
    ;;; We should prioritize formulas with fewer variables
    [(comp count formula/variables) <]
    ;;; We should prioritize restricting search space with smaller tables
    [(comp state/table-count formula/predicate) <]]
   conjuncts))
