(ns scotus.database.add
  (:require
   [clojure.set :as set]
   [clojure.string :as string]
   [honey.sql.helpers :as h]
   [next.jdbc :as jdbc]
   [scotus.database.utils :as utils]
   [scotus.json :as json]
   [scotus.state :as state]))

;; TODO: Maybe keep an object index table?
;; TODO: Set things up for rules
;; create table

(defn ^:private create-index
  "Create an index for `table` on `column` using `opts`"
  [table column {:keys [unique] :as _opts}]
  (let [table-name  (utils/clean-object-name table)
        column-name (utils/clean-object-name column)
        unique     (if unique "UNIQUE" "")
        spec [(format
               "CREATE %s INDEX %s_%s ON %s (%s)"
               unique table-name column-name table-name column-name)]]
    (utils/ddl-success? (jdbc/execute! (state/db-connection) spec))))

(defn create-assertion-predicate-lookup []
  (-> (h/create-table :assertion_predicate_lookup)
      (h/with-columns [[:id :uuid] [:predicate [:varchar 500]]])
      utils/execute!)
  (create-index "assertion_predicate_lookup" "id" {:unique true}))

(defn create-justification-table []
  (and
   (every?
    utils/ddl-success?
    [
     (-> (h/create-table :justification)
         (h/with-columns [[:id :uuid] [:derived :bool] [:justification :jsonb]])
         utils/execute!)

     (-> (h/create-table :assertion-justification)
         (h/with-columns [[:assertion-id :uuid] [:justification-id :uuid]])
         utils/execute!)

     (-> (h/alter-table :assertion-justification)
         (h/add-index :primary-key :assertion-id :justification-id)
         utils/execute!)])
   (create-index "justification" "id" {:unique true})))

(defn create-table!
  "Create a table with `table-name` and `columns`."
  [table-name & columns]
  (state/with-refreshed-index
    (let [table-key     (utils/to-keyword table-name)
          required-cols [[:negative :boolean]
                         [:context [:varchar 50]]]
          clean-cols   (mapv
                        (fn [col] [(utils/to-keyword col) [:varchar 500]])
                        columns)
          all-cols      (concat [[:id :uuid [:not nil]]]
                                clean-cols
                                required-cols)
          table-spec    (-> (h/create-table table-key :if-not-exists)
                            (h/with-columns all-cols))
          ;; create table
          table-success? (utils/ddl-success? (utils/execute! table-spec))
          ;; create indexes
          index-success? (every? (fn [column] (create-index table-name column {})) columns)
          ;; add a unique constraint for id
          id-index-success? (create-index table-name "id" {:unique true})]
      (and table-success? index-success? id-index-success?))))

;;; add rows

(defn make-table-row [context negated? arg-columns spec]
  (let [args (into {} (map (fn [arg-spec arg]
                             (if (vector? arg-spec)
                               [(first arg-spec) [:cast arg (second arg-spec)]]
                               [arg-spec arg]))
                           arg-columns
                           spec))]
    (assoc args
           :id (utils/->uuid (sort (assoc args :context context)))
           :context context
           :negative negated?)))



(defn make-table-headers [table]
  (reduce-kv
   (fn [acc column data-type]
     (let [cast-type (case data-type
                       "date" :date
                       "time without time zone" :time
                       "uuid" :uuid
                       nil)]
       (if cast-type
         (conj acc [(utils/to-keyword column) cast-type])
         (conj acc (utils/to-keyword column)))))
   []
   (state/table-arg-types table)))

(defn add-rows-by-table
  "Add rows to a table all with the same justification, context, and negated value."
  [table context justification negated? specs]
  (let [table-arg-columns (make-table-headers table)
        rows (map (partial make-table-row context negated? table-arg-columns) specs)
        justification-id (if (string? justification)
                           (utils/->uuid justification)
                           (utils/->uuid (set justification)))
        justification-row {:id justification-id :justification [:cast (json/encode justification) :jsonb]}
        just-join-rows (map
                        (fn [{:keys [id]}]
                          {:assertion-id id
                           :justification-id justification-id})
                        rows)
        assert-predicate-rows (map
                               (fn [{:keys [id]}]
                                 {:id id
                                  :predicate table})
                               rows)]
    (-> (h/insert-into :assertion_predicate_lookup)
        (h/values assert-predicate-rows)
        (h/on-conflict :id)
        h/do-nothing
        utils/execute!)
    (-> (h/insert-into :assertion-justification)
        (h/values just-join-rows)
        (h/on-conflict :assertion-id :justification-id)
        h/do-nothing
        utils/execute!)
    (-> (h/insert-into :justification)
        (h/values [justification-row])
        (h/on-conflict :id)
        h/do-nothing
        utils/execute!)
    {:assert-count
     (get (first (-> (h/insert-into (utils/to-keyword table))
                     (h/values rows)
                     (h/on-conflict :id)
                     h/do-nothing
                     utils/execute!))
          :next.jdbc/update-count)}))

;;;;;;;;;;;;;;;;;;
;;; Update Columns

(defn update-column-type [table column new-type]
  (let [column-type-string (case new-type
                             "date" "::date"
                             "time" "::time"
                             "integer" "::integer"
                             "boolean" "::boolean"
                             "ip" "::cidr"
                             "uuid" "::uuid")]
    (when column-type-string
      (state/with-refreshed-index
        (-> (h/alter-table (utils/to-keyword table))
            (h/alter-column (utils/to-keyword column) :type (keyword new-type))
            (h/using
             [[:raw (-> column
                        utils/to-keyword
                        str
                        (subs 1)
                        (str column-type-string))]])
            utils/execute!)))))
