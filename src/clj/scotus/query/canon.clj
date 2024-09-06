(ns scotus.query.canon
  (:require [scotus.syntax.formula :as f]
            [scotus.syntax.canon :as canon]
            [scotus.state :as state]))

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

(defn ^:private <-with-nils-low [object-one object-two]
  (cond (and object-one object-two)
        (< object-one object-two)

        object-one
        false

        object-two
        true

        :else
        true))

;; should this also be informed by a graph on the variables?
(defn sort-conjuncts
  [conjuncts]
  (sort-formulas-by
   [;;; Looking for assertions should always come first
    [(comp (fn [pred] (not= pred "asserted")) f/predicate) nil]
    ;;; Looking for unknown things should alwasy come last
    [(comp (fn [pred] (= pred "unknown")) f/predicate) nil]
    ;;; resolution predicates
    [(comp (fn [pred] (= pred "<=")) f/predicate) nil]
    [(comp (fn [pred] (= pred ">=")) f/predicate) nil]
    [(comp (fn [pred] (= pred "<")) f/predicate) nil]
    [(comp (fn [pred] (= pred ">")) f/predicate) nil]

    ;;; We should prioritize formulas with fewer variables
    [(comp count f/variables) <]
    ;;; We should prioritize restricting search space with smaller tables
    [(comp state/table-count f/predicate) <-with-nils-low]]
   conjuncts))

;;; I'm no longer sure this is a good idea
;;; what about cases like [:and ["=" ?x "one] ["=" ?x "two"]]
;;; or cases like [:and ["=" "a" "b"] PHI] - do we want to treat
;;; "=" as a robust identity predicate (so we prove that ["=" "a" "b"]
;;; in addition to proving PHI?
(defn ^:private conjunction-subsitute-identity
  "Assuming that a conjunction of literals has been passed.

  Use any `=` statements to replace args with others and generate
  bindings for variable to value identities."
  [conjunction-of-literals]
  (let [{:keys [ids non-ids]} (reduce
                               (fn [acc literal]
                                 (if (and (f/atom? literal)
                                          (= (f/predicate literal) "="))
                                   (update acc :ids conj literal)
                                   (update acc :non-ids conj literal)))
                               {:ids #{} :non-ids #{}}
                               (f/args conjunction-of-literals))]
    (reduce
     (fn [{:keys [formula bindings]} id-lit]
       (let [args (sort-by (complement f/variable?) (f/args id-lit))
             arg (first args)
             replacement (second args)
             new-conjunction (f/substitute formula arg replacement)])
     (f/conjoin non-ids)
     ids))))

(defn conjunction-substitute-identity [dnf]
  (f/disjoin
   (mapv conjunction-substitute-identity (f/args dnf))))

(defn dnf [formula]
  (canon/enact formula :query))
