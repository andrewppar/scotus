(ns scotus.heuristic.assert
  (:require
   [scotus.database.add :as dba]))

(defn add-literals*
  [predicate args negated? context justification]
  (dba/add-rows-by-table predicate context justification negated? args))

(defmulti add-literals!
  "Determines whether other heuristics need to be added to the
  database as the result of an assertion"
  {:arg-lists '([predicate arg-lists negated? context justification])}
  (fn [predicate _ _ _ _]
    predicate))

(defmethod add-literals! :default
  [predicate arg-lists negated? context justification]
  (add-literals* predicate arg-lists negated? context justification))

(defn ^:private update-arg-instance-table [[predicate arg arg-type]]
    (dba/update-column-type predicate arg arg-type))

(defmethod add-literals! "arg_instance"
  [predicate arg-lists negated? context justification]
  (when-not negated?
    (mapv update-arg-instance-table arg-lists))
  (add-literals* predicate arg-lists negated? context justification))
