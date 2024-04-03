(ns scotus.semantic.assert-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [honey.sql.helpers :as h]
   ;; TODO: Make this a util outside of database
   [scotus.semantic.assert :as assert]
   [scotus.database.test-utils :refer [each-fixture once-fixture]]
   [scotus.database.utils :as utils]))

(use-fixtures :each each-fixture)
(use-fixtures :once once-fixture)

(deftest ^:integration t-simple-assert
  (testing "simple atomic assert"
      (is (= 0 (-> (h/select :subclass :superclass)
                   (h/from :subclass_of)
                   (h/where [:and
                             [:= :subclass "cat"]
                             [:= :superclass "mammal"]])
                   utils/execute!
                   count)))
      (assert/! ["subclass_of" "cat" "mammal"] :asserter "anparisi")
      (is (= 1 (-> (h/select :subclass :superclass)
                   (h/from :subclass_of)
                   (h/where [:and
                             [:= :subclass "cat"]
                             [:= :superclass "mammal"]])
                   utils/execute!
                   count)))))

(deftest ^:integration t-simple-negative-assert
  (testing "simple negation assert"
      (is (= 0 (-> (h/select :subclass :superclass)
                   (h/from :subclass_of)
                   (h/where [:and
                             [:= :subclass "cat"]
                             [:= :superclass "mammal"]])
                   utils/execute!
                   count)))
      (assert/! [:not ["subclass_of" "cat" "mammal"]] :asserter "anparisi")
      (is (= 1 (-> (h/select :subclass :superclass)
                   (h/from :subclass_of)
                   (h/where [:and
                             [:= :subclass "cat"]
                             [:= :superclass "mammal"]
                             [:= :negative true]])
                   utils/execute!
                   count)))))

(deftest ^:integration t-assert-conjunction
  (testing "assert a conjunction and get both conjuncts"
    (is (= 0 (-> (h/select :subclass :superclass)
                 (h/from :subclass_of)
                 (h/where [:= :superclass "mammal"])
                 utils/execute!
                 count)))
    (assert/! [:and
                ["subclass_of" "cat" "mammal"]
                ["subclass_of" "dog" "mammal"]]
               :asserter "anparisi")
    (is (= 2 (-> (h/select :subclass :superclass)
                 (h/from :subclass_of)
                 (h/where [:= :superclass "mammal"])
                 utils/execute!
                 count)))))
