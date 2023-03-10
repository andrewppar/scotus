(ns scotus.database
  (:require
   [clojure.string :as str]
   [honey.sql :as sql]
   [honey.sql.helpers :as h]
   [next.jdbc :as jdbc]))


(defn create-table
  [table-name & columns]
  (let [required-cols [[:negative :boolean]
                       [:justification :jsonb]
                       [:context [:varchar 50]]]
        to-keyword   (fn [item]
                       (keyword (str/replace item #"[- ]" "_")))
        clean-cols   (mapv
                      (fn [col] [(to-keyword col) [:varchar 500]])
                      columns)

        create-table (-> (h/create-table
                          (to-keyword table-name)
                          (h/with-columns
                            (concat [[:id :int [:not nil]]]
                                    clean-cols
                                    required-cols)))
                         sql/format)]
    create-table))


(create-table "subclass-of" "subclass" "superclass")
