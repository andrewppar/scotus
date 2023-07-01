(ns scotus.database.add-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [scotus.database.add :as add]
   [scotus.assert :as assert]
   [scotus.database.utils :as utils]))

(deftest t-add-rows
  (testing "simple add row"
    (with-redefs [utils/execute! identity
                  assert/id      (constantly "id")]
      (let [columns [:id :thing :class :negative :justification :context]
            args    [:thing :class]
            column->type {:thing "character varying", :class "character varying"}]
        (is (= ["INSERT INTO instance (id, thing, class, negative, justification, context) VALUES (?, ?, ?, FALSE, ?, ?), (?, ?, ?, FALSE, ?, ?)"
                "id" "dog" "mammal" "anparisi" "testing-animal"
                "id" "cat" "pet" "anparisi" "testing-pet"]
             (add/add-partition
              "instance" columns args false column->type
              [{:args ["dog" "mammal"]
                :context "testing-animal"
                :justification "anparisi"}
               {:args ["cat" "pet"]
                :context "testing-pet"
                :justification "anparisi"}])))))))
