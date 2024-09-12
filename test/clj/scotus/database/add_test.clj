(ns scotus.database.add-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [honey.sql.helpers :as h]
   [scotus.database.test-utils :refer [each-fixture once-fixture]]
   [scotus.database.add :as dba]
   [scotus.state :as state]
   [scotus.database.utils :as utils]))

(use-fixtures :each each-fixture)
(use-fixtures :once once-fixture)

(deftest ^:integration t-create-table
  (testing "Ensure that we can create tables"
    (dba/create-table! "test-table" "one" "two" "three")
    (is (contains? (set (state/tables)) "test_table"))
    (is (= (state/table-args "test_table") ["one" "two" "three"]))))

(deftest ^:integration t-add-rows-by-table
  (testing "add rows to table"
    (dba/add-rows-by-table
     "instance" "universal" "anparisi" false
     [["dog" "mammal"] ["cat" "mammal"]])
    (let [query-results (-> (h/select :thing)
                            (h/from :instance)
                            (h/where [:= :class "mammal"])
                            utils/execute!)
          instances (set (map :instance/thing query-results))]
      (is (= instances #{"cat" "dog"})))))
