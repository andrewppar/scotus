(ns scotus.database.query
  (:require
   [honey.sql              :as sql]
   [honey.sql.helpers      :as h]
   [scotus.assert          :as assert]
   [scotus.database.utils  :as utils]
   [scotus.formula.formula :as formula]
   [scotus.state           :as state]))

(defn ^:private row-spec->conjunction
  [contexts columns negated? row-spec]
  (when-not (>= (count columns) (count row-spec))
    (throw
     (ex-info
      (format "Row specification \"%s\" cannot be used with columns: %s"
              row-spec columns)
      {:caused-by `(>= (count ~columns) (count ~row-spec))})))
  (let [base-conjunct (cond-> [:and]
                        (not= contexts :universal) (conj [:in :context contexts])
                        true (conj [:= :negative negated?]))]
    (->> row-spec
         (zipmap columns)
         (reduce-kv
          (fn [result col value]
            (if (nil? value)
              result
              (conj result [:= col value])))
          base-conjunct))))

(defn ^:private row-specs->where-body
  [{:keys [contexts columns negated?]} row-specs]
  (reduce
   (fn [acc row-spec]
     (conj acc (row-spec->conjunction contexts columns negated? row-spec)))
   [:or]
   row-specs))

(defn empty-spec? [row-spec]
  (every? nil? row-spec))

#dbg
(defn lookup-rows-serial
  [{:keys [table contexts negated? include-meta?]
    :as options}
   row-specs]
  (let [table-columns (map utils/to-keyword (state/table-args table))
        select-args   (if include-meta? [:*] table-columns)]
    (utils/execute!
     (cond-> (apply h/select select-args)
       true (h/from (utils/to-keyword table))

       (not (every? empty-spec? row-specs))
       (h/where
        (let [new-options (assoc options :columns table-columns)]
          (row-specs->where-body new-options row-specs)))
       true sql/format))))

(defn lookup-rows
  [table contexts negated? row-specs & {:keys [include-meta?] :or
                                        {include-meta? false}}]
  (let [options {:table table
                 :contexts contexts
                 :negated? negated?
                 :include-meta? include-meta?}]
  (->> row-specs
       (partition-all 10000)
       (pmap (partial lookup-rows-serial options))
       (apply concat))))

(defn db-row->assert [predicate row]
  (let [clean-row (reduce-kv
                   (fn [acc k v]
                     (assoc acc (keyword (name k)) v))
                   {}
                   row)
        {:keys [context negative justification]} clean-row
        args (vals
              (dissoc clean-row :context :negative :justification :id))]
    (assert/make predicate args context negative justification)))

(defn formulas->assertions [formulas contexts]
  (reduce-kv
   (fn [asserts predicate formulas]
     (let [specs (mapv formula/args formulas)
           rows  (lookup-rows
                  predicate
                  contexts
                  ;; only positive assertions now
                  false
                  specs
                  :include-meta? true)]
       (concat asserts (map (partial db-row->assert predicate) rows))))
   []
   (group-by formula/predicate formulas)))

(defn lookup-assertion-by-id [table id]
  (let [results (-> (h/select :*)
                    (h/from (utils/to-keyword table))
                    (h/where [:= :id id])
                    sql/format
                    utils/execute!)]
    (when (seq results)
      (if (> (count results) 1)
        (throw
         (ex-info (format
                   "More than one assertion with id: %s"
                   id)
                  {:caused-by id}))
        (let [raw-assert (first results)
              keyfn    (partial keyword table)
              context  (get raw-assert (keyfn "context"))
              negative (get raw-assert (keyfn "negative"))
              justification (get raw-assert (keyfn "justification"))
              args (vals (dissoc raw-assert (keyfn "id")
                                 (keyfn "negative")
                                 (keyfn "justification")
                                 (keyfn "context")))
              assert (assert/make
                      table args context negative justification)
              generated-id (get assert :id)]
          (if (not= id generated-id)
            (throw
             (ex-info (format "assertion with id: %s was saved with id %s"
                              generated-id id)
                      {:caused-by `(not= ~id ~generated-id)}))
            assert))))))
