(ns scotus.database.query
  (:require
   [honey.sql              :as sql]
   [honey.sql.helpers      :as h]
   [scotus.database.utils  :as utils]
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
                        (not= contexts :universal)
                        (conj [:in :context (vec contexts)])

                        true
                        (conj [:= :negative negated?]))]
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

(defn lookup-rows-serial
  [{:keys [table contexts negated? include-meta?]
    :as options}
   row-specs]
  (let [table-columns (map utils/to-keyword (state/table-args table))
        select-args   (if include-meta? [:*] table-columns)
        query-start (-> (apply h/select select-args)
                        (h/from (utils/to-keyword table)))]
    (if (not (every? empty-spec? row-specs))
      (-> query-start
          (h/where (row-specs->where-body
                    (-> options
                        (assoc :columns table-columns)
                        (update :contexts (fnil conj #{}) "universal"))
                    row-specs))
          utils/execute!)
      (utils/execute!
       (-> query-start
           (h/where
            [:in :context (or contexts ["universal"])]))))))

(defn lookup-rows
  [table contexts negated? row-specs & {:keys [include-meta?] :or
                                        {include-meta? false}}]
  (let [options {:table table
                 :contexts contexts
                 :negated? negated?
                 :include-meta? include-meta?}]
    (if (seq row-specs)
      (->> row-specs
           (partition-all 10000)
           (pmap (partial lookup-rows-serial options))
           (apply concat))
      (lookup-rows-serial options row-specs))))

(defn lookup-assertion
  [assertion-id]
  (let [table (-> (h/select :predicate)
                  (h/from :assertion_predicate_lookup)
                  (h/where [:= [:cast assertion-id :uuid] :id])
                  utils/execute!
                  first
                  (get :assertion_predicate_lookup/predicate)
                  keyword)]
    (-> (h/select :*)
        (h/from table)
        (h/where [:= :id [:cast assertion-id :uuid]])
        utils/execute!
        first)))

(defn lookup-for-all-preds
  [contexts negated? row-specs
   & {:keys [preds include-meta?] :or
      {include-meta? false
       preds (state/tables)}}]
  (mapcat
   (fn [table]
     (lookup-rows
      table contexts negated? row-specs :include-meta? include-meta?))
   preds))

(defn lookup-asserts-for-all-preds
  [contexts negated? row-specs
   & {:keys [preds include-meta?] :or
      {include-meta? false
       preds (state/tables)}}]
  (let [min-row-spec-size (apply min (map count row-specs))
        applicable-preds  (->> preds
                               (filter
                                (fn [predicate]
                                  (<= min-row-spec-size
                                      (count (state/table-args predicate)))))
                               (remove
                                (fn [pred]
                                  (contains? #{"assertion_predicate_lookup"
                                               "assertion_justification"
                                               "justification"}
                                             pred))))]
    (when (seq applicable-preds)
      (lookup-for-all-preds
       contexts negated? row-specs
       :preds applicable-preds :include-meta? include-meta?))))



(defn context [context]
  (let [preds (remove
               (fn [pred]
                 (contains? #{"assertion_predicate_lookup"
                              "assertion_justification"
                              "justification"}
                            pred))
               (state/tables))]
    (lookup-for-all-preds [context] false {} :include-meta? true :preds preds)))
