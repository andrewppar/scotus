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
  (dba/create-table! "another_subclass" "thing" "class")
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

(deftest ^:integration t-transitive-down
  (dbr/drop-table! "pet_type")
  (dba/create-table! "pet_type" "person" "animal_type")
  (assert/! [:and
             ["transitive_arg" "pet_type" "animal_type" "subclass_of" "subclass" "superclass"]
             ["subclass_of" "golden_retriever" "dog"]
             ["subclass_of" "dog" "mammal"]
             ["subclass_of" "mammal" "animal"]
             ["pet_type" "anparisi" "golden_retriever"]]
            "anparisi")
  (is (= '#{{?x "anparisi"}}
         (literal/transitivity '[]
                               '["pet_type" ?x "animal"]
                               '[{:transitive_arg/predicate "pet_type"
                                  :transitive_arg/arg_name "animal_type"
                                  :transitive_arg/transitive_pred "subclass_of"
                                  :transitive_arg/from_arg "subclass"
                                  :transitive_arg/to_arg "superclass"}]
                               "universal"
                               false)))
  (is (= '#{{?x "anparisi"
             :justification
             #{["pet_type" "anparisi" "golden_retriever"]
               ["subclass_of" "mammal" "animal"]
               ["subclass_of" "dog" "mammal"]
               ["subclass_of" "golden_retriever" "dog"]}}}
         (literal/transitivity '[]
                               '["pet_type" ?x "animal"]
                               '[{:transitive_arg/predicate "pet_type"
                                  :transitive_arg/arg_name "animal_type"
                                  :transitive_arg/transitive_pred "subclass_of"
                                  :transitive_arg/from_arg "subclass"
                                  :transitive_arg/to_arg "superclass"}]
                               "universal"
                               true)))
  (is (= '#{{?x "anparisi" ?y "animal"}}
         (literal/transitivity '[{?y "animal"}]
                               '["pet_type" ?x ?y]
                               '[{:transitive_arg/predicate "pet_type"
                                  :transitive_arg/arg_name "animal_type"
                                  :transitive_arg/transitive_pred "subclass_of"
                                  :transitive_arg/from_arg "subclass"
                                  :transitive_arg/to_arg "superclass"}]
                               "universal"
                               false)))
  (is (= '#{{?x "anparisi" ?y "animal"
             :justification
             #{["pet_type" "anparisi" "golden_retriever"]
               ["subclass_of" "mammal" "animal"]
               ["subclass_of" "dog" "mammal"]
               ["subclass_of" "golden_retriever" "dog"]}}}
         (literal/transitivity '[{?y "animal"}]
                               '["pet_type" ?x ?y]
                               '[{:transitive_arg/predicate "pet_type"
                                  :transitive_arg/arg_name "animal_type"
                                  :transitive_arg/transitive_pred "subclass_of"
                                  :transitive_arg/from_arg "subclass"
                                  :transitive_arg/to_arg "superclass"}]
                               "universal"
                               true))))

(deftest ^:integration t-transitive-up
  (dbr/drop-table! "pet_type")
  (dba/create-table! "pet_type" "person" "animal_type")
  (assert/! [:and
             ["transitive_arg" "pet_type" "animal_type" "subclass_of" "subclass" "superclass"]
             ["subclass_of" "golden_retriever" "dog"]
             ["subclass_of" "dog" "mammal"]
             ["subclass_of" "mammal" "animal"]
             ["pet_type" "anparisi" "golden_retriever"]]
            "anparisi")
  (is (= '#{{?x "anparisi" ?y "dog"}
            {?x "anparisi" ?y "golden_retriever"}
            {?x "anparisi" ?y "mammal"}
            {?x "anparisi" ?y "animal"}}
         (literal/transitivity '[]
                               '["pet_type" ?x ?y]
                               [{:transitive_arg/predicate "pet_type"
                                 :transitive_arg/arg_name "animal_type"
                                 :transitive_arg/transitive_pred "subclass_of"
                                 :transitive_arg/from_arg "subclass"
                                 :transitive_arg/to_arg "superclass"}]
                               "universal" false)))

  (is (= '#{{?x "anparisi" ?y "golden_retriever"
             :justification
             [["pet_type" "anparisi" "golden_retriever"]]}
            {?x "anparisi" ?y "dog"
             :justification
             [["subclass_of" "golden_retriever" "dog"]
              ["pet_type" "anparisi" "golden_retriever"]]}
            {?x "anparisi" ?y "mammal"
             :justification
             [["subclass_of" "golden_retriever" "dog"]
              ["subclass_of" "dog" "mammal"]
              ["pet_type" "anparisi" "golden_retriever"]]}
            {?x "anparisi" ?y "animal"
             :justification
             [["subclass_of" "golden_retriever" "dog"]
              ["subclass_of" "dog" "mammal"]
              ["subclass_of" "mammal" "animal"]
              ["pet_type" "anparisi" "golden_retriever"]]}}

         (literal/transitivity '[]
                               '["pet_type" ?x ?y]
                               [{:transitive_arg/predicate "pet_type"
                                 :transitive_arg/arg_name "animal_type"
                                 :transitive_arg/transitive_pred "subclass_of"
                                 :transitive_arg/from_arg "subclass"
                                 :transitive_arg/to_arg "superclass"}]
                               "universal" true)))

  (is (= '#{{?x "anparisi" ?y "dog"}
            {?x "anparisi" ?y "golden_retriever"}
            {?x "anparisi" ?y "mammal"}
            {?x "anparisi" ?y "animal"}}
         (literal/transitivity '[{?x "anparisi"}]
                               '["pet_type" ?x ?y]
                               [{:transitive_arg/predicate "pet_type"
                                 :transitive_arg/arg_name "animal_type"
                                 :transitive_arg/transitive_pred "subclass_of"
                                 :transitive_arg/from_arg "subclass"
                                 :transitive_arg/to_arg "superclass"}]
                               "universal" false)))

  (is (= '#{{?x "anparisi" ?y "golden_retriever"
             :justification
             [["pet_type" "anparisi" "golden_retriever"]]}
            {?x "anparisi" ?y "dog"
             :justification
             [["subclass_of" "golden_retriever" "dog"]
              ["pet_type" "anparisi" "golden_retriever"]]}
            {?x "anparisi" ?y "mammal"
             :justification
             [["subclass_of" "golden_retriever" "dog"]
              ["subclass_of" "dog" "mammal"]
              ["pet_type" "anparisi" "golden_retriever"]]}
            {?x "anparisi" ?y "animal"
             :justification
             [["subclass_of" "golden_retriever" "dog"]
              ["subclass_of" "dog" "mammal"]
              ["subclass_of" "mammal" "animal"]
              ["pet_type" "anparisi" "golden_retriever"]]}}
         (literal/transitivity '[{?x "anparisi"}]
                               '["pet_type" ?x ?y]
                               [{:transitive_arg/predicate "pet_type"
                                 :transitive_arg/arg_name "animal_type"
                                 :transitive_arg/transitive_pred "subclass_of"
                                 :transitive_arg/from_arg "subclass"
                                 :transitive_arg/to_arg "superclass"}]
                               "universal" true))))


(deftest ^:integration query
  (dbr/drop-table! "has_disease")
  (dba/create-table! "has_disease" "patient" "disease")
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
             :justification #{["subclass_of" "heart disease" "disease"]
                              ["subclass_of" "disease" "disease"]}}
            {?subclass "chf"
             :justification #{["subclass_of" "chf" "heart disease"]
                              ["subclass_of" "heart disease" "disease"]
                              ["subclass_of" "disease" "disease"]}}
            {?subclass "cancer"
             :justification #{["subclass_of" "cancer" "disease"]
                              ["subclass_of" "disease" "disease"]}}
            {?subclass "lung cancer"
             :justification #{["subclass_of" "lung cancer" "cancer"]
                              ["subclass_of" "cancer" "disease"]
                              ["subclass_of" "disease" "disease"]}}
            {?subclass "nsclc"
             :justification #{["subclass_of" "nsclc" "lung cancer"]
                              ["subclass_of" "lung cancer" "cancer"]
                              ["subclass_of" "cancer" "disease"]
                              ["subclass_of" "disease" "disease"]}}}
         (literal/query '["subclass_of" ?subclass "disease"]
                        '#{} :justification? true)))

  (is (= '#{{?subclass "disease" ?superclass "disease"}
	    {?subclass "chf" ?superclass "heart disease"}
	    {?subclass "lung cancer" ?superclass "cancer"}
	    {?subclass "cancer" ?superclass "cancer"}
	    {?subclass "heart disease" ?superclass "heart disease"}
	    {?subclass "nsclc" ?superclass "lung cancer"}
	    {?subclass "nsclc" ?superclass "nsclc"}
	    {?subclass "lung cancer" ?superclass "disease"}
	    {?subclass "nsclc" ?superclass "disease"}
	    {?subclass "nsclc" ?superclass "cancer"}
	    {?subclass "chf" ?superclass "chf"}
	    {?subclass "lung cancer" ?superclass "lung cancer"}
	    {?subclass "chf" ?superclass "disease"}}
         (literal/query '["subclass_of" ?subclass ?superclass]
                        '#{})))
  )
