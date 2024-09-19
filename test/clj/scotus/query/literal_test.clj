(ns scotus.query.literal-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [scotus.database.add :as dba]
   [scotus.database.remove :as dbr]
   [scotus.database.test-utils :refer [each-fixture once-fixture]]
   [scotus.query.literal :as literal]
   [scotus.semantic.assert :as assert]))

(use-fixtures :each each-fixture)
(use-fixtures :once once-fixture)

(deftest ^:integration t-simple-lookup
  ;;more tests -- with justifications, without justifications, and negations
  (dbr/drop-table! "another_subclass")
  (dba/create-table! "another_subclass" ["thing" "class"] [])
  (assert/! [:and
             ["another_subclass" "dog" "mammal"]
             ["another_subclass" "cat" "mammal"]])
  (testing "simple lookup one arg one answer"
    (is (= #{'{?x "mammal"}}
           (literal/simple-lookup [] '["another_subclass" "dog" ?x] "universal" false))))

  (testing "simple lookup one arg two answers"
    (is (= #{'{?x "cat"}
             '{?x "dog"}}
           (literal/simple-lookup [] '["another_subclass" ?x "mammal"] "universal" false))))

  (testing "simple lookup two arg two answers"
    (is (= #{'{?x "cat" ?y "mammal"}
             '{?x "dog" ?y "mammal"}}
           (literal/simple-lookup [] '["another_subclass" ?x ?y] "universal" false))))

  (testing "simple lookup with original maps"
    (is (= #{'{?x "cat" ?y "mammal"}
             '{?x "dog" ?y "mammal"}}
           (literal/simple-lookup '[{?y "mammal"}] '["another_subclass" ?x ?y] "universal" false))))

  (testing "simple lookup no answers"
    (is (= #{}
           (literal/simple-lookup '[{?x "dog"}] '["another_subclass" ?x "reptile"] "universal" false))))

  (testing "simple lookup one arg one answer with justification"
    (let [result (literal/simple-lookup [] '["another_subclass" "dog" ?x] "universal" true)]
      (is (= (count result) 1))
      (let [binding (first result)]
        (is (= '#{?x :justification} (set (keys binding))))
        (is (= "mammal" (get binding '?x)))
        (is (= [["another_subclass" "dog" "mammal"]]
               (get binding :justification))))))

  (testing "simple negated lookup"
    (assert/! [:not ["another_subclass" "dog" "cat"]] :asserter "anparisi")
    (is (= #{'{?x "cat"}}
           (literal/simple-lookup [] '[:not ["another_subclass" "dog" ?x]] "universal" false))))

  (testing "simple negated lookup with bindings"
    (assert/! [:not ["another_subclass" "dog" "cat"]] :asserter "anparisi")
    (is (= #{'{?x "cat" ?y "dog"}}
           (literal/simple-lookup ['{?y "dog"}] '[:not ["another_subclass" ?y ?x]] "universal" false)))))

(deftest ^:integration query
  (dbr/drop-table! "has_disease")
  (dba/create-table! "has_disease" ["patient" "disease"] [])
  (assert/! [:and
             ["has_disease" "pat_x" "nsclc"]
             ["has_disease" "pat_y" "chf"]]
            :asserter "anparisi")

  (is (= '#{{?x "pat_x"}}
         (literal/query '["has_disease" ?x "nsclc"] #{})))

  (is (= '#{{?x "pat_x" ?y "nsclc"}
            {?x "pat_y" ?y "chf"}}
         (literal/query '["has_disease" ?x ?y] #{})))

  (assert/! [:and
             ["transitive_arg" "has_disease" "disease" "subclass_of" "subclass" "superclass"]
             ["subclass_of" "nsclc" "lung cancer"]
             ["subclass_of" "chf" "heart disease"]
             ["subclass_of" "lung cancer" "cancer"]
             ["subclass_of" "cancer" "disease"]
             ["subclass_of" "heart disease" "disease"]]
            :asserter "anparisi")

  (is (= '#{{?x "pat_x"}}
         (literal/query '["has_disease" ?x "cancer"] #{})))

  (is (= '#{{?x "pat_x"} {?x "pat_y"}}
         (literal/query '["has_disease" ?x "disease"] #{})))

  (is (= '#{{?disease "nsclc"}
            {?disease "lung cancer"}
            {?disease "cancer"}
            {?disease "disease"}}
         (literal/query '["has_disease" "pat_x" ?disease] #{})))

  (is (= '#{{?disease "chf"}
            {?disease "heart disease"}
            {?disease "disease"}}
         (literal/query '["has_disease" "pat_y" ?disease] #{})))

  (is (= '#{{?pat "pat_y" ?disease "chf"}
            {?pat "pat_y" ?disease "heart disease"}
            {?pat "pat_y" ?disease "disease"}
            {?pat "pat_x" ?disease "nsclc"}
            {?pat "pat_x" ?disease "lung cancer"}
            {?pat "pat_x" ?disease "cancer"}
            {?pat "pat_x" ?disease "disease"}}
         (literal/query '["has_disease" ?pat ?disease] #{})))

  (is (= '#{{?pat "pat_x" ?disease "cancer"}}
         (literal/query '["has_disease" ?pat ?disease]
                        '#{{?disease "cancer"}})))

  (is (= '#{{?subclass "disease"}
            {?subclass "heart disease"}
            {?subclass "chf"}
            {?subclass "cancer"}
            {?subclass "lung cancer"}
            {?subclass "nsclc"}}
         (literal/query '["subclass_of" ?subclass "disease"]
                        '#{})))

  (is (= '#{{?subclass "disease"
             :justification #{["subclass_of" "disease" "disease"]}}
            {?subclass "heart disease"
             :justification #{["subclass_of" "heart disease" "disease"]}}
            {?subclass "chf"
             :justification #{["subclass_of" "chf" "heart disease"]
                              ["subclass_of" "heart disease" "disease"]}}
            {?subclass "cancer"
             :justification #{["subclass_of" "cancer" "disease"]}}
            {?subclass "lung cancer"
             :justification #{["subclass_of" "lung cancer" "cancer"]
                              ["subclass_of" "cancer" "disease"]}}
            {?subclass "nsclc"
             :justification #{["subclass_of" "nsclc" "lung cancer"]
                              ["subclass_of" "lung cancer" "cancer"]
                              ["subclass_of" "cancer" "disease"]}}}
         (literal/query '["subclass_of" ?subclass "disease"]
                        '#{} :justification? true)))

  (is (= '#{{?subclass "chf" ?superclass "heart disease"}
            {?subclass "heart disease" ?superclass "disease"}
	    {?subclass "lung cancer" ?superclass "cancer"}
	    {?subclass "nsclc" ?superclass "lung cancer"}
	    {?subclass "lung cancer" ?superclass "disease"}
	    {?subclass "nsclc" ?superclass "disease"}
            {?subclass "nsclc" ?superclass "cancer"}
            {?subclass "cancer" ?superclass "disease"}
	    {?subclass "chf" ?superclass "disease"}}
         (literal/query '["subclass_of" ?subclass ?superclass]
                        '#{}))))
