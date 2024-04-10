(ns scotus.semantic.retract-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [scotus.semantic.assert :as assert]
   [scotus.semantic.retract :as retract]
   [scotus.query.query :as q]
   [scotus.database.test-utils :refer [each-fixture once-fixture]]))

(use-fixtures :each each-fixture)
(use-fixtures :once once-fixture)

(deftest ^:integration t-simple-retract
  (testing "simple retract an assertion"
    (let [statement '["instance" "Ody" "dog"]]
      (is (= #{} (q/query statement)))
      (assert/! statement)
      (is (= #{{}} (q/query statement)))
      (retract/! statement)
      (is (= #{} (q/query statement))))))
