(ns scotus.semantic.assert
  (:require
   [scotus.database.add :as add]
   [scotus.syntax.formula :as f]
   [scotus.syntax.xnf :as xnf]))


(defn !
  [formula & {:keys [context asserter] :or {context "universal"}}]
  ;; TODO: Not mistyped
  (let [cnf (xnf/cnf formula)]
    (cond (f/atom? cnf)
          (add/add-rows-by-table
           (f/predicate cnf) context asserter false [(f/args cnf)])

          (f/negation? formula)
          (let [atom (f/negatum cnf)]
            (add/add-rows-by-table
             (f/predicate atom) context asserter true [(f/args atom)]))

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
                                (let [{:keys [assert-count]} (add/add-rows-by-table
                                                              predicate
                                                              context
                                                              asserter
                                                              false
                                                              (mapv f/args atoms))]
                                  (update acc :assert-count (fnil + 0) assert-count)))
                              {}
                              atoms-by-predicate)]
            (reduce-kv
             (fn [acc predicate negations]
               (let [{:keys [assert-count]} (add/add-rows-by-table
                                             predicate
                                             context
                                             asserter
                                             false
                                             (mapv (comp f/args f/negatum) negations))]
                 (update acc :assert-count (fnil + 0) assert-count)))
             atom-asserts
             negations-by-predicate)))))
