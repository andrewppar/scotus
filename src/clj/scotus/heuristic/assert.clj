(ns scotus.heuristic.assert
  (:require
   [scotus.database.assert-map :as assert-map]
   [scotus.database.add :as dba]
   [scotus.database.query :as dbq]))

(defn ^:private add-inferred-context
  [predicate context negated? args]
  (let [id (-> predicate
               (dbq/lookup-rows [context] negated? args {:include-meta? true})
               first
               (get (keyword predicate "id")))]
    (dba/add-rows-by-table "instance" "universal" [id] false [[context "context"]])))

(defn add-literals*
  [predicate args negated? context justification]
  ;; maybe cache this lookup?
  (let [contexts (->> (dbq/lookup-rows-serial
                       {:table "instance" :negated? false}
                       [[nil "context"]])
                      (mapv :instance/thing)
                      set)
        asserted-map (dba/add-rows-by-table predicate context justification negated? args)
        inferred-map (if (contains? contexts context)
                       assert-map/empty-assert-map
                       (add-inferred-context predicate context negated? args))]
    (assert-map/merge-assert-maps asserted-map inferred-map)))

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
