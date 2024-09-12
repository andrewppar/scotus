(ns scotus.database.query-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [scotus.database.test-utils :refer [each-fixture once-fixture]]
   [scotus.database.add :as dba]
   [scotus.database.query :as dbq]))

(use-fixtures :each each-fixture)
(use-fixtures :once once-fixture)

(deftest ^:integration t-lookup-rows
  (testing "Ensure that queries work"
    (dba/add-rows-by-table "instance" "universal" "anparisi" false
                           [["dog" "mammal"] ["cat" "mammal"]])
    (is (= []
           (dbq/lookup-rows "instance" ["universal"] false [[nil "dog"]])))
    (is (= #{["cat" "mammal"] ["dog" "mammal"]}
           (->> (dbq/lookup-rows "instance" ["universal"] false [[nil "mammal"]])
                (map (comp vec vals))
                set)))
    (is (= #{["cat" "mammal"]}
           (->> (dbq/lookup-rows "instance" ["universal"] false [["cat" "mammal"]])
                (map (comp vec vals))
                set)))
    (is (= #{["cat" "mammal"]}
           (->> (dbq/lookup-rows "instance" ["universal"] false [["cat" nil]])
                (map (comp vec vals))
                set)))))

;;; Lookup assertion test?? This one would be a little windy...
