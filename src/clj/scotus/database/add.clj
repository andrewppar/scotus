(ns scotus.database.add
  (:require
   [honey.sql.helpers :as h]
   [next.jdbc :as jdbc]
   [scotus.database.assertion :as assertion]
   [scotus.database.justification :as justification]
   [scotus.database.utils :as utils]
   [scotus.json :as json]
   [scotus.state :as state]))

;; TODO: Maybe keep an object index table?
;; TODO: Set things up for rules
;; create table

(defn ^:private create-index
  "Create an index for `table` on `column` using `opts`"
  [table column {:keys [unique] :as _opts}]
  (let [table-name-for-idx  (utils/clean-object-name table)
        table-name (name (utils/to-keyword table))
        column-name (utils/clean-object-name column)
        unique     (if unique "UNIQUE" "")
        spec [(format
               "CREATE %s INDEX %s__%s ON \"%s\" (%s)"
               unique table-name-for-idx column-name table-name column-name)]]
    (utils/ddl-success? (jdbc/execute! (state/db-connection) spec))))

(defn create-assertion-predicate-lookup []
  (and
   (assertion/create-predicate-lookup-table)
   (create-index "assertion_predicate_lookup" "id" {:unique true})))

(defn create-justification-table []
  (and
   (justification/create-table)
   (create-index "justification" "id" {:unique true})))

(defn create-nlp-args-table []
  (and
   (-> (h/create-table :nlp-args)
       (h/with-columns [[:predicate :text] [:arg :text]])
       utils/execute!
       utils/ddl-success?)
   (create-index "nlp_args" "predicate" {})))

(defn create-table!
  "Create a table with `table-name` and `columns`."
  [table-name columns nlp-columns]
  (state/with-refreshed-index
    (let [table-key     (utils/to-keyword table-name)
          required-cols [[:negative :boolean]
                         [:context [:varchar 50]]]
          clean-cols   (mapv
                        (fn [col] [(utils/to-keyword col) :text])
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
          id-index-success? (create-index table-name "id" {:unique true})
          ;; add any nlp columns
          clean-name (utils/clean-table-name table-name)
          nlp-success? (or (not (seq nlp-columns))
                         (-> (h/insert-into :nlp-args)
                             (h/values (mapv (partial conj [clean-name]) nlp-columns))
                             utils/execute!
                             utils/something-inserted?))]
      (and table-success? index-success? id-index-success? nlp-success?))))

;;; add rows

(defn make-table-row [context negated? arg-columns spec]
  (let [args (into {} (map (fn [arg-spec arg]
                             (if (vector? arg-spec)
                               [(first arg-spec) [:cast arg (second arg-spec)]]
                               [arg-spec arg]))
                           arg-columns
                           spec))]
    (assoc args
           :id (utils/->assertion-id args context)
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
        assertion-ids (map (fn [{:keys [id]}] id) rows)]
    (justification/add assertion-ids justification)
    (assertion/add-lookup-rows assertion-ids table)
    {:assert-count
     (-> (h/insert-into (utils/to-keyword table))
         (h/values rows)
         (h/on-conflict :id)
         h/do-nothing
         utils/execute!
         utils/insert-count)}))

;;;;;;;;;;;;;;;;;;
;;; Update Columns

(defn update-column-type [table column new-type]
  (let [column-type-string (case new-type
                             "date" "::date"
                             "time" "::time"
                             "integer" "::integer"
                             "boolean" "::boolean"
                             "ip" "::cidr"
                             "uuid" "::uuid"
                             #_#_"assertion" "::uuid"
                             nil)]
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
