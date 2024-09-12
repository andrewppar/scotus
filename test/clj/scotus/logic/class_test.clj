(ns scotus.logic.class-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [scotus.database.test-utils :refer [each-fixture once-fixture]]
   [scotus.logic.class :as scl]
   [scotus.semantic.assert :as assert]))

(use-fixtures :once once-fixture)
(use-fixtures :each each-fixture)

(deftest ^:integration t-subclass
  (let [hierarchy [:and
                   ["subclass_of" "dog" "mammal"]
                   ["subclass_of" "mammal" "animal"]]]
    (assert/! hierarchy :asserter "anparisi")
    (testing "reflexivity"
      (is (contains? (scl/superclasses "dog") "dog"))
      (is (contains? (scl/subclasses "dog") "dog"))
      (is (contains? (scl/superclasses "cat") "cat"))
      (is (contains? (scl/subclasses "cat") "cat")))

    (testing "with justifications"
      (let [result (scl/superclasses "dog" :justification? true)]
        (is (= #{"dog" "mammal" "animal"} result))
        (is (= {"mammal" #{#{["subclass_of" "dog" "mammal"]}},
	        "animal"
	        #{#{["subclass_of" "dog" "mammal"]
	            ["subclass_of" "mammal" "animal"]}},
	        "dog" #{#{["subclass_of" "dog" "dog"]}}}
               (get (meta result) :justification)))))))


(deftest ^:integration t-disjoint
  (let [hierarchy [:and
                   ["subclass_of" "dog" "mammal"]
                   ["subclass_of" "lizard" "reptile"]
                   ["disjoint" "mammal" "reptile"]]]
    (assert/! hierarchy :asserter "anparisi")

    (testing "simple disjointness"
      (is (scl/disjoint? "dog" "lizard")))

    (testing "simple failed disjunction"
      (assert/! ["subclass_of" "cat" "mammal"] :asserter "anparisi")
      (is (not (scl/disjoint? "dog" "cat"))))

    (testing "deeper disjoint"
      (assert/! [:and
                 ["subclass_of" "chihuahua" "dog"]
                 ["subclass_of" "oaxaca chihuahua" "chihuahua"]
                 ["subclass_of" "iguana" "lizard"]])
      (is (scl/disjoint? "iguana" "oaxaca chihuahua")))

    (testing "symmetry"
      (is (scl/disjoint? "chihuahua" "reptile"))
      (is (scl/disjoint? "reptile" "chihuahua")))

    (testing "justification"
      (let [result (scl/disjoint? "chihuahua" "reptile" :justification? true)]
        (is (= result ["disjoint" "mammal" "reptile"]))
        (is (= [["subclass_of" "chihuahua" "dog"]
                ["subclass_of" "dog" "mammal"]
                ;; fixing the disjoint justification
                ;; to use maps should fix this.
                ["subclass_of" "reptile" "reptile"]
                ["disjoint" "mammal" "reptile"]]
               (get (meta result) :justification)))))))
