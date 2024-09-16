(ns scotus.semantic.assert-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [honey.sql.helpers :as h]
   [scotus.heuristic.query :as hq]
   [scotus.database.test-utils :refer [each-fixture once-fixture]]
   [scotus.database.utils :as utils]
   [scotus.query.query :as q]
   ;; TODO: Make this a util outside of database
   [scotus.semantic.assert :as assert]
   [scotus.semantic.retract :as retract]))

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

(deftest ^:integration t-assert-natural-language
  (testing "assert nlp data."
    (assert/predicate! "test_label"
                       :args ["item" "label"]
                       :nlp-args ["label"]
                       :asserter "anparisi")
    (assert/! ["test_label" "odysseus" "bobo"]
              :context "universal"
              :asserter "anparisi")
    (let [id (utils/->assertion-id
              {:item "odysseus" :label "bobo"} "universal")]
    (is (= (str id) (get (first (hq/lookup-assertion id)) :id))))))

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

(deftest ^:integration t-assert-new-context
  (testing "making assertion with new context creates it."
    (let [ctxt (str (random-uuid))]
      (retract/predicate! "age")
      (assert/predicate! "age" :args ["name" "age"])
      (assert/! ["age" "anparisi" "36"] :context ctxt :asserter "anparisi")
      (is (seq (q/query '["age" "anparisi" "36"] :context ctxt)))
      ;; justification isn't wired in for this yet...
      (is (seq (q/query `["instance" ~ctxt "context"] :justification? true))))))
