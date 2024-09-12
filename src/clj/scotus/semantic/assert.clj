(ns scotus.semantic.assert
  (:require
   [scotus.heuristic.assert :as ha]
   [scotus.database.add :as add]
   [scotus.semantic.well-formed :as wf]
   [scotus.syntax.formula :as f]
   [scotus.syntax.xnf :as xnf]))


(defn !
  [formula & {:keys [context asserter] :or {context "universal"}}]
  ;; TODO: Not mistyped
  (let [cnf (xnf/cnf formula)]
    (cond (f/atom? cnf)
          (ha/add-literals!
           (f/predicate cnf) [(f/args cnf)] false context asserter)

          (f/negation? formula)
          (let [atom (f/negatum cnf)]
            (ha/add-literals!
             (f/predicate atom) [(f/args atom)] true context asserter))

          :else
          (let [{literals true} (group-by f/literal? (f/args cnf))
                {atoms false negations true} (group-by f/negation? literals)
                atoms-by-predicate (group-by f/predicate atoms)
                negations-by-predicate (group-by
                                        (fn [negation]
                                          (f/predicate (f/negatum negation)))
                                        negations)
                atom-asserts (reduce-kv
                              (fn [acc predicate atoms]
                                (let [{:keys [assert-count]} (ha/add-literals!
                                                              predicate
                                                              (mapv f/args atoms)
                                                              false
                                                              context
                                                              asserter)]
                                  (update acc :assert-count (fnil + 0) assert-count)))
                              {}
                              atoms-by-predicate)]
            (reduce-kv
             (fn [acc predicate negations]
               (let [{:keys [assert-count]} (ha/add-literals!
                                             predicate
                                             (mapv (comp f/args f/negatum) negations)
                                             false
                                             context
                                             asserter)]
                 (update acc :assert-count (fnil + 0) assert-count)))
             atom-asserts
             negations-by-predicate)))))

(defn predicate!
  [predicate & {:keys [args asserter]}]
  (when-not  (wf/predicate? predicate)
    (! ["instance" predicate "predicate"]
       :context "universal"
       :asserter asserter)
    (apply add/create-table! predicate args)))
