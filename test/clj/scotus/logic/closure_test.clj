(ns scotus.logic.closure-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [scotus.database.test-utils :refer [each-fixture once-fixture]]
   [scotus.semantic.assert :as assert]
   [scotus.logic.closure :as cl]))


(use-fixtures :each each-fixture)
(use-fixtures :once once-fixture)

(deftest ^:integration t-one-step
  (let [hierarchy [:and
                   ["subclass_of" "chihuahua" "dog"]
                   ["subclass_of" "dog" "mammal"]
                   ["subclass_of" "cat" "mammal"]
                   ["subclass_of" "persian" "cat"]]]
    (assert/! hierarchy :asserter "anparisi")
    (testing "simple one step"
      (is (= {"chihuahua"
              #{{:subclass_of/subclass "chihuahua" :subclass_of/superclass "dog"}}
              "persian"
              #{{:subclass_of/subclass "persian" :subclass_of/superclass "cat"}}}
             (cl/one-step ["chihuahua" "persian"] "subclass_of" "subclass" ["universal"]))))))

(deftest ^:integration t-closure-loops
  (testing "small loop"
    (let [hierarchy ["subclass_of" "chihuahua" "chihuahua"]]
      (assert/! hierarchy :asserter "anparisi")
      (is (= #{[{:subclass_of/subclass "chihuahua" :subclass_of/superclass "chihuahua"}
                {:subclass_of/subclass "chihuahua" :subclass_of/superclass "chihuahua"}]}
             (cl/closure ["chihuahua"] "subclass_of" "subclass" "superclass")))))

  (testing "larger loop"
    (let [hierarchy [:and
                     ["subclass_of" "retriever" "dog"]
                     ["subclass_of" "dog" "mammal"]
                     ["subclass_of" "mammal" "excellent"]
                     ["subclass_of" "excellent" "retriever"]]]
      (assert/! hierarchy :asserter "anparisi")
      (is (= #{[{:subclass_of/subclass "retriever" :subclass_of/superclass "dog"}
                {:subclass_of/subclass "dog" :subclass_of/superclass "mammal"}
                {:subclass_of/subclass "mammal" :subclass_of/superclass "excellent"}
                {:subclass_of/subclass "excellent" :subclass_of/superclass "retriever"}
                {:subclass_of/subclass "retriever" :subclass_of/superclass "dog"}]}
             (cl/closure ["retriever"] "subclass_of" "subclass" "superclass"))))))

(deftest ^:integration t-closure-splits
  (testing "simple split"
    (let [hierarchy [:and
                     ["subclass_of" "amphibious vehicle" "water transport"]
                     ["subclass_of" "amphibious vehicle" "land transport"]
                     ["subclass_of" "duckboat" "amphibious vehicle"]]]
      (assert/! hierarchy :asserter "anparisi")
      (is (= #{[{:subclass_of/subclass "duckboat" :subclass_of/superclass "amphibious vehicle"}
                {:subclass_of/subclass "amphibious vehicle" :subclass_of/superclass "water transport"}]
               [{:subclass_of/subclass "duckboat" :subclass_of/superclass "amphibious vehicle"}
                {:subclass_of/subclass "amphibious vehicle" :subclass_of/superclass "land transport"}]}
             (cl/closure ["duckboat"] "subclass_of" "subclass" "superclass"))))))

(deftest ^:integration t-closure-merge
  (testing "simple merge"
    (let [hierarchy [:and
                     ["subclass_of" "retriever" "dog"]
                     ["subclass_of" "persian" "cat"]
                     ["subclass_of" "dog" "mammal"]
                     ["subclass_of" "cat" "mammal"]]]
      (assert/! hierarchy :asserter "anparisi")
      (is (= #{[{:subclass_of/subclass "retriever" :subclass_of/superclass "dog"}
                {:subclass_of/subclass "dog" :subclass_of/superclass "mammal"}]
               [{:subclass_of/subclass "persian" :subclass_of/superclass "cat"}
                {:subclass_of/subclass "cat" :subclass_of/superclass "mammal"}]}
             (cl/closure ["retriever" "persian"] "subclass_of" "subclass" "superclass"))))))

(deftest ^:integration t-downward-closure
  (testing "downward closure"
    (let [hierarchy [:and
                     ["subclass_of" "retriever" "dog"]
                     ["subclass_of" "dog" "animal"]
                     ["subclass_of" "animal" "excellent"]]]
      (assert/! hierarchy :asserter "anparisi")
      (is (= #{[{:subclass_of/subclass "animal" :subclass_of/superclass "excellent"}
                {:subclass_of/subclass "dog" :subclass_of/superclass "animal"}
                {:subclass_of/subclass "retriever" :subclass_of/superclass "dog"}]}
             (cl/closure ["excellent"] "subclass_of" "superclass" "subclass"))))))
