(ns scotus.database.add
  (:require
   [clojure.set                :as set]
   [clojure.string             :as str]
   [honey.sql                  :as sql]
   [honey.sql.helpers          :as h]
   [scotus.assert              :as assert]
   [scotus.database.assert-map :as am]
   [scotus.database.utils      :as utils]
   [scotus.formula.formula     :as formula]
   [scotus.state               :as state]))

;; TODO: Maybe keep an object index table?
;; TODO: Set things up for rules
;; create table

(defn ^:private create-index
  "Create an index for `table` on `column` using `opts`"
  [table column opts]
  (let [table-name  (utils/clean-object-name table)
        column-name (utils/clean-object-name column)
        unique     (if (get opts :unique) "UNIQUE" "")
        spec [(format
               "CREATE %s INDEX %s_%s ON %s (%s)"
               unique table-name column-name table-name column-name)]]
    (utils/execute! spec)))

(defn create-table!
  "Create a table with `table-name` and `columns`."
  [table-name & columns]
  (let [table-key     (utils/to-keyword table-name)
        required-cols [[:negative :boolean]
                       [:justification [:varchar 50]]
                       [:context [:varchar 50]]]
        clean-cols   (mapv
                      (fn [col] [(utils/to-keyword col) [:varchar 500]])
                      columns)
        all-cols      (concat [[:id :varchar [:not nil]]]
                              clean-cols
                              required-cols)
        table-spec    (-> (h/create-table table-key)
                          (h/with-columns all-cols)
                          sql/format)]
    ;; create table
    (utils/execute! table-spec)
    ;; create indexes
    (mapv (fn [column] (create-index table-name column {})) columns)
    ;; add a unique constraint for id
    (create-index table-name "id" {:unique true}))
  (state/refresh-index!))

;;; add justification
(defn add-justification
  [assert-id assertions])

;;; add rows
(defn lookup-arg-type [column->pg-type column]
  (let [arg-type (get column->pg-type column)]
    (case arg-type
      "character varying" nil
      "time without time zone" :time
      nil)))

(defn add-types-to-column-args [columns column->pg-type spec]
  (let [column-map (zipmap columns spec)]
    (reduce-kv
     (fn [result column arg]
       (if-let [arg-type (lookup-arg-type column->pg-type column)]
         (conj result [:cast arg arg-type])
         (conj result arg)))
     []
     column-map)))

(defn ^:prvate create-row
  [table columns negated? column->pg-type
   {:keys [args context justification]}]
  (let [id (assert/id table args context)
        columns-with-type (add-types-to-column-args
                           columns column->pg-type args)]
    `[~id ~@columns-with-type ~negated? ~justification ~context]))

(defn add-partition
  [table columns args negated? column->pg-type specs]
  (let [create-row-fn (partial
                       create-row table args negated? column->pg-type)]
    (-> (h/insert-into (utils/to-keyword table) columns)
        (h/values (map create-row-fn specs))
        sql/format
        utils/execute!)))

(defn db-result->assert-map [db-result]
  (->> db-result
       (map (fn [result]
              (set/rename-keys
               result {:next.jdbc/update-count :assert-count})))
       (apply am/merge-assert-maps)))

(defn add-rows-fine-grained
  " Given a vector of row specifications and a table name
  add those rows to the database."
  [table negated? row-specs]
  ;; Use the args for validation
  (let [args            (map utils/to-keyword (state/table-args table))
        column->pg-type (reduce-kv
                         (fn [acc k v]
                           (assoc acc (utils/to-keyword k) v))
                         {}
                         (state/table-arg-types table))

        columns        `[:id ~@args :negative :justification :context]]
    (->> row-specs
         (partition-all 10000)
         (pmap
          (partial
           add-partition table columns args negated? column->pg-type))
         (reduce
          (fn [acc result]
            (let [assert-map (db-result->assert-map result)]
              (am/merge-assert-maps acc assert-map)))
          am/empty-assert-map))))

(defn make-row-spec [context justification args]
  {:args args :justification justification :context context})

(defn add-rows
  [table context justification negated? specs]
  (->> specs
       (map (partial make-row-spec context justification)
       (add-rows-fine-grained table negated?))))

;;;;;;;;;;;;;;;;;;
;;; Update Columns

(defn update-column-type [table column new-type]
  (let [column-type-string (case new-type
                             "date" "::date"
                             "time" "::time")]
    (-> (h/alter-table (utils/to-keyword table))
        (h/alter-column (utils/to-keyword column) :type (keyword new-type))
        (h/using
         [[:raw (-> column
                    utils/to-keyword
                    str
                    (subs 1)
                    (str column-type-string))]])
        sql/format
        utils/execute!
        db-result->assert-map)))

(comment
  :testing

  (create-table! "subclass_of" "subclass" "superclass")
  (add-rows-fine-grained "subclass_of"
            "nature" "anparisi" false [{:args ["cat" "mammal"]
                                        :context "nature"
                                        :justification "anparisi"}
                                       {:args ["dog" "mammal"]
                                        :context "nature"
                                        :justification "anparisi"}])
  (add-rows "subclass_of"
               "nature" "anparisi" false [["chihuahua" "dog"]
                                          ["persian" "cat"]
                                          ["golden retriever" "dog"]
                                          ])
  (add-rows "subclass_of"
            "household" "anparisi" false [["cat" "pet"] ["dog" "pet"]])

  (add-rows "subclass_of"
            "nature" "anparisi" false [["cat" "feline"] ["mammal" "chordate"]])

  (add-rows "subclass_of"
            "household" "anparisi" false [["lizard" "pet"] ["snake" "pet"]])

  (add-rows "subclass_of"
            "nature" "anparisi" false [["lizard" "reptile"] ["snake" "reptile"] ["reptile" "animal"] ["mammal" "animal"]])

  (create-table! "instance" "thing" "class"))
