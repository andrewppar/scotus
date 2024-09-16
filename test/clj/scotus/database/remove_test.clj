(ns scotus.database.remove-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [honey.sql.helpers :as h]
   [scotus.database.test-utils :refer [each-fixture once-fixture]]
   [scotus.database.add :as dba]
   [scotus.database.remove :as dbr]
   [scotus.database.utils :as utils]
   [scotus.state :as state]))

(use-fixtures :each each-fixture)
(use-fixtures :once once-fixture)

(deftest ^:integration t-drop-table
  (testing "Ensure that we can drop tables."
    (dba/create-table! "test-table" ["one" "two" "three"] [])
    (is (contains? (set (state/tables)) "test_table"))
    (dbr/drop-table! "test-table")
    (is (not (contains? (set (state/tables)) "test_table")))))

(deftest ^:integration t-delete-rows
  (testing "Ensure that we can delete rows from tables."
    (dba/add-rows-by-table
     "instance" "universal" "anparisi" false
     [["dog" "mammal"] ["cat" "mammal"]])
    (let [query-results (-> (h/select :thing)
                            (h/from :instance)
                            (h/where [:= :class "mammal"])
                            utils/execute!)
          instances (set (map :instance/thing query-results))]
      (is (= instances #{"cat" "dog"})))
    (testing "Delete a fully specified row."
      (dbr/delete-rows "instance" "universal" false ["dog" "mammal"])
      (let [query-results (-> (h/select :thing)
                              (h/from :instance)
                              (h/where [:= :class "mammal"])
                              utils/execute!)
            instances (set (map :instance/thing query-results))]
        (is (= instances #{"cat"}))))
    (testing "Delete all mammals"
      (dba/add-rows-by-table
       "instance" "universal" "anparisi" false
       [["dog" "mammal"] ["cat" "mammal"]])
      (let [query-results (-> (h/select :thing)
                              (h/from :instance)
                              (h/where [:= :class "mammal"])
                              utils/execute!)
            instances (set (map :instance/thing query-results))]
        (is (= instances #{"cat" "dog"})))
      (dbr/delete-rows "instance" "universal" false ["" "mammal"])
      (let [query-results (-> (h/select :thing)
                              (h/from :instance)
                              (h/where [:= :class "mammal"])
                              utils/execute!)
            instances (set (map :instance/thing query-results))]
        (is (= instances #{}))))))
