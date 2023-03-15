(ns scotus.database
  (:require
   [clojure.string    :as str]
   [honey.sql         :as sql]
   [honey.sql.helpers :as h]
   [next.jdbc         :as jdbc]
   [scotus.state      :as state]))

;; TODO: Maybe keep an object index table?
;; TODO: Set things up for rules

(defn ^:private clean-object-name
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

(defn create-table
  "Create a table with `table-name` and `columns`."
  [table-name & columns]
  (let [table-key     (to-keyword table-name)
        required-cols [[:negative :boolean]
                       [:justification :jsonb]
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
    (create-index table-name "id" {:unique true})))

(create-table "subclass-of" "subclass" "superclass")

(defn add-rows
  "Given a vector of row specifications and a table name
  add those rows to the database.")
