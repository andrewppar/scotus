(ns scotus.query.query-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [scotus.database.add :as dba]
   [scotus.database.remove :as dbr]
   [scotus.database.test-utils :refer [each-fixture once-fixture]]
   [scotus.query.query :as q]
   [scotus.semantic.assert :as assert]
   [scotus.semantic.retract :as retract]))


(use-fixtures :each each-fixture)
(use-fixtures :once once-fixture)

(deftest t-query
  (retract/predicate! "has_disease")
  (retract/predicate! "label")
  (assert/predicate! "has_disease" :args ["patient" "disease"])
  (assert/predicate! "label" :args ["thing" "english"])
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
                    :justification? true))))
  (testing "second order query"
    (is (= '#{{:justification [["subclass_of" "C15" "C01"]]
	       ?value "C01"
	       ?predicate "subclass_of"}
	      {:justification [["label" "C15" "Cancer"]]
	       ?value "Cancer"
	       ?predicate "label"}}
           (q/query '[?predicate "C15" ?value]
                    :justification? true)))))

(deftest t-query-contexts
  (retract/predicate! "has_condition")
  (assert/predicate! "has_condition" :args ["patient" "disease"])
  (assert/! ["subclass_of" "heart_disease" "disease"]
            :asserter "anparisi" :context "health")
  (assert/! ["subclass_of" "disease" "biological_state"]
            :asserter "anparisi" :context "biology")
  (assert/! [:and
             ["has_condition" "pat_01" "heart_disease"]
             ["transitive_arg" "has_condition" "disease"
              "subclass_of" "subclass" "superclass"]]
            :asserter "anparisi" :context "hospital")
  (assert/! [:and ["subcontext_of" "health" "hospital"]
             ["subcontext_of" "biology" "health"]]
            :asserter "anparisi")
  (testing "Querying with contexts works"
    (is (= '#{}
           (q/query '["has_condition" "pat_01" ?condition] :context "health")))

    (is (= '#{{?condition "heart_disease"}
              {?condition "disease"}
              {?condition "biological_state"}}
           (q/query '["has_condition" "pat_01" ?condition] :context "hospital")))))

(deftest t-resolution-lookup
  (retract/predicate! "age")
  (assert/predicate! "age" :args ["person" "age"])
  (assert/!
   [:and
    ["arg_instance" "age" "age" "integer"]]
   :asserter "anparisi")
  (assert/!
   [:and
    ["age" "andrew" 36]
    ["age" "george" 1]
    ["age" "anthony" 1]
    ["age" "xenia" 34]]
   :asserter "anparisi")
  (is (= '#{{?person "xenia" ?age 34}
            {?person "george" ?age 1}
            {?person "anthony" ?age 1}}
         (q/query '[:and
                    ["age" ?person ?age]
                    ["<=" ?age 35]])))
  (is (= '#{{?person "george" ?age 1}
            {?person "anthony" ?age 1}}
         (q/query '[:and
                    ["age" ?person ?age]
                    ["<=" ?age 1]])))

  (is (= '#{{?person "xenia" ?age 34}
            {?person "andrew" ?age 36}}
         (q/query '[:and
                    ["age" ?person ?age]
                    [">=" ?age 2]])))

  (is (= '#{{?person "andrew" ?age 36}}
         (q/query '[:and
                    ["age" ?person ?age]
                    [">=" ?age 35]])))

  (is (= '#{}
         (q/query '[:and
                    ["age" ?person ?age]
                    [">=" ?age 38]])))

  (is (= '#{}
         (q/query '[:and
                    ["age" ?person ?age]
                    ["<=" ?age 0]]))))
