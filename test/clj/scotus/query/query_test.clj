(ns scotus.query.query-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [scotus.database.add :as dba]
   [scotus.database.remove :as dbr]
   [scotus.database.test-utils :refer [each-fixture once-fixture]]
   [scotus.query.query :as q]
   [scotus.semantic.assert :as assert]))


(use-fixtures :each each-fixture)
(use-fixtures :once once-fixture)

(deftest t-query
  (dbr/drop-table! "has_disease")
  (dbr/drop-table! "label")
  (dba/create-table! "has_disease" "patient" "disease")
  (dba/create-table! "label" "thing" "english")
  (assert/!
   [:and
    ["transitive_arg"
     "has_disease" "disease"
     "subclass_of" "subclass" "superclass"]
    ["label" "C666" "CHF"]
    ["label" "C667" "Heart Disease"]
    ["label" "C01" "Disease"]
    ["label" "C13" "NSCLC"]
    ["label" "C14" "Lung Cancer"]
    ["label" "C15" "Cancer"]
    ["has_disease" "pat_01" "C666"]
    ["subclass_of" "C666" "C667"]
    ["subclass_of" "C667" "C01"]
    ["has_disease" "pat_02" "C13"]
    ["subclass_of" "C13"  "C14"]
    ["subclass_of" "C14" "C15"]
    ["subclass_of" "C15" "C01"]])
  (testing "simple lookup"
    (is (= #{'{?patient "pat_01"}}
           (q/query '["has_disease" ?patient "C666"]))))

  (testing "simple conjunction"
    (is (= #{'{?patient "pat_01" ?disease "C666"}}
           (q/query '[:and
                      ["has_disease" ?patient ?disease]
                      ["label" ?disease "CHF"]]))))

  (testing "simple conjunction"
    (is (= #{'{?patient "pat_01" ?disease "C667"}}
           (q/query '[:and
                      ["has_disease" ?patient ?disease]
                      ["label" ?disease "Heart Disease"]]))))


  (testing "simple conjunction with transitivity-down"
    (is (= #{'{?patient "pat_01" ?disease "C667"}}
           (q/query '[:and
                      ["has_disease" ?patient ?disease]
                      ["label" ?disease "Heart Disease"]]))))

  (testing "more transitivity down"
    (is (= #{'{?patient "pat_01" ?disease "C01"}
             '{?patient "pat_02" ?disease "C01"}}
           (q/query
            '[:and
              ["has_disease" ?patient ?disease]
              ["label" ?disease "Disease"]]))))

  (testing "simple conjunction with transitivity-up"
    (is (= #{'{?disease "C13"}
             '{?disease "C14"}
             '{?disease "C15"}
             '{?disease "C01"}}
           (q/query '[:and
                      ["has_disease" "pat_02" ?disease]]))))

  (testing "disjunction with transitivity"
    (is (= #{'{?patient "pat_01" ?disease "C667"}
             '{?patient "pat_02" ?disease "C15"}}
           (q/query '[:and
                      ["has_disease" ?patient ?disease]
                      [:or
                       ["label" ?disease "Cancer"]
                       ["label" ?disease "Heart Disease"]]]))))

  (testing "disjunction with transitivity and justification too"
    (is (= #{'{?patient "pat_01" ?disease "C667" :justification
               #{["has_disease" "pat_01" "C666"]
                 ["subclass_of" "C666" "C667"]
                 ["label" "C667" "Heart Disease"]}}
             '{?patient "pat_02" ?disease "C15" :justification
               #{["has_disease" "pat_02" "C13"]
                 ["subclass_of" "C13" "C14"]
                 ["subclass_of" "C14" "C15"]
                 ["label" "C15" "Cancer"]}}}
           (q/query '[:and
                      ["has_disease" ?patient ?disease]
                      [:or
                       ["label" ?disease "Cancer"]
                       ["label" ?disease "Heart Disease"]]]
                    :justification? true)))))
