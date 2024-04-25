(ns scotus.semantic.assert-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [honey.sql.helpers :as h]
   ;; TODO: Make this a util outside of database
   [scotus.semantic.assert :as assert]
   [scotus.query.query :as q]
   [scotus.semantic.retract :as retract]
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


(defn ^:private column-data-type [table column]
  (->
   (h/select :data-type)
   (h/from :information-schema.columns)
   (h/where
    [:and
     [:= :table-name table]
     [:= :column-name column]])
   utils/execute!
   first
   (get :columns/data_type)))

(deftest ^:integration t-assert-arg-instance
  (testing "integer with arg at time of assert"
    (retract/predicate! "age")
    (assert/predicate! "age" :args ["name" "age"])
    (assert/! ["arg_instance" "age" "age" "integer"])
    (is (= "integer" (column-data-type "age" "age")))
    (assert/! ["age" "george" 1] :asserter "anparisi")
    (is (= '#{{?age 1}} (q/query '["age" "george" ?age]))))

  ;;; This is cool of postgres, but it means we have to be careful
  ;; with adding types that could reconvert a lot of the table
  (testing "integer with arg after assert"
    (retract/predicate! "age")
    (assert/predicate! "age" :args ["name" "age"])
    (assert/! ["age" "anthony" "1"] :asserter "anparisi")
    (assert/! ["arg_instance" "age" "age" "integer"])
    (is (= "integer" (column-data-type "age" "age")))
    (is (= '#{{?age 1}} (q/query '["age" "anthony" ?age]))))

  (testing "integer with arg after assert"
    (retract/predicate! "age")
    (assert/predicate! "age" :args ["name" "age"])
    (assert/! ["age" "anthony" "1"] :asserter "anparisi")
    (assert/! ["arg_instance" "age" "age" "integer"])
    (is (= "integer" (column-data-type "age" "age")))
    (is (thrown?
         Exception
         (assert/! ["age" "george" "one"] :asserter "anparisi")))))

;;  )
;;(testing "date")
;;(testing "id")
;;(testing "nothing interesting to do")
;;
;;)
