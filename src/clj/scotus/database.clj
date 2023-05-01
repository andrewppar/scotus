(ns scotus.database
  (:require
   [clojure.string    :as str]
   [honey.sql         :as sql]
   [honey.sql.helpers :as h]
   [next.jdbc         :as jdbc]
   [scotus.state      :as state]))

;; TODO: Maybe keep an object index table?
;; TODO: Set things up for rules

;; create table

(defn ^:private, clean-object-name
  [item]
  (str/replace item #"[- ]" "_"))

(defn ^:private to-keyword
  [item]
  (-> item clean-object-name keyword))

(defn ^:private create-index
  "Create an index for `table` on `column` using `opts`"
  [table column opts]
  (let [table-name  (clean-object-name table)
        column-name (clean-object-name column)
        unique     (if (get opts :unique) "UNIQUE" "")
        spec [(format
               "CREATE %s INDEX %s_%s ON %s (%s)"
               unique table-name column-name table-name column-name)]]
    (jdbc/execute! (state/db-connection) spec)))

(defn create-table!
  "Create a table with `table-name` and `columns`."
  [table-name & columns]
  (let [table-key     (to-keyword table-name)
        required-cols [[:negative :boolean]
                       [:justification [:varchar 50]]
                       [:context [:varchar 50]]]
        clean-cols   (mapv
                      (fn [col] [(to-keyword col) [:varchar 500]])
                      columns)
        table-spec    (-> (h/create-table table-key
                                          (h/with-columns
                                            (concat [[:id :int [:not nil]]]
                                                    clean-cols
                                                    required-cols)))
                          sql/format)]
    ;; create table
    (jdbc/execute! (state/db-connection) table-spec)
    ;; create indexes
    (mapv (fn [column] (create-index table-name column {})) columns)
    ;; add a unique constraint for id
    (create-index table-name "id" {:unique true}))
  (state/refresh-index))

;;; add rows

(defn ^:prvate add-computed-args
  [context justification negated? spec]
  (let [id        (hash (reduce str "" spec))]
    `[~id ~@spec ~negated? ~justification ~context]))

(defn ^:private add-partition
  [table columns context justification negated? specs]
  (let [create-row-fn (partial add-computed-args
                               context justification negated?)]
    (jdbc/execute!
     (state/db-connection)
     (-> (h/insert-into (to-keyword table) columns)
         (h/values (map create-row-fn specs))
         sql/format))))

(defn add-rows
  "Given a vector of row specifications and a table name
  add those rows to the database."
  [table context justification negated? row-specs]
  ;; Use the args for validation
  (let [args    (map to-keyword (state/table-args table))
        columns `[:id ~@args :negative :justification :context]
        parts (partition-all 10000 row-specs)]
    (pmap
     (partial add-partition table columns context justification negated?)
     parts)))

;;; delete rows
(defn delete-rows
  "Given a vector of row-specifications, a table, and a context,
  delete the corresponding rows from the table."
  [table context row-specs negated?]
  (let [ids (map (fn [spec] (hash (reduce str "" spec))) row-specs)]
    (jdbc/execute!
     (state/db-connection)
     (-> (h/delete-from (to-keyword table))
         (h/where
          [:in :id ids]
          [:= :negated? negated?]
          [:= :context context])
         (sql/format {:inline true})))))

;;; lookup rows
(defn ^:private row-spec->conjunction
  [contexts columns row-spec]
  (when-not (>= (count columns) (count row-spec))
    (throw
     (ex-info
      (format "Row specification \"%s\" cannot be used with columns: %s"
              row-spec columns)
      {:caused-by `(>= (count ,columns) (count ,row-spec))})))
  (->> row-spec
       (zipmap columns)
       (reduce-kv
        (fn [result col value]
          (if (nil? value)
            result
            (conj result [:= col value])))
        (if (= contexts :universal)
          [:and]
          [:and [:in :context contexts]]))))

(defn ^:private row-specs->where-body
  [contexts columns row-specs]
  (reduce
   (fn [acc row-spec]
     (conj acc (row-spec->conjunction contexts columns row-spec)))
   [:or]
   row-specs))

(defn empty-spec? [row-spec]
  (every? nil? row-spec))

(defn lookup-rows-serial
  [table contexts row-specs]
  (let [table-columns (map to-keyword (state/table-args table))]
    (jdbc/execute!
     (state/db-connection)
     (cond-> (apply h/select table-columns)
       true
       (h/from (to-keyword table))

       (not (every? empty-spec? row-specs))
       (h/where
        (row-specs->where-body contexts table-columns row-specs))

       true
       sql/format))))

(defn lookup-rows
  [table contexts row-specs]
  (->> row-specs
       (partition-all 10000)
       (pmap (partial lookup-rows-serial table contexts))
       (apply concat)))

(comment
  :testing

  (create-table! "subclass_of" "subclass" "superclass")
  (add-rows "subclass_of"
            "nature" "anparisi" false [["cat" "mammal"] ["dog" "mammal"]])
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

  (delete-rows "subclass_of"
               "nature" [["cat" "mammal"] ["dog" "mammal"]] false)

  (lookup-rows "subclass_of" ["nature" "household"] [["cat"]])
  (lookup-rows "subclass_of" ["nature" "household"] [[nil "dog"]])



  (create-table! "instance" "thing" "class")

  )
