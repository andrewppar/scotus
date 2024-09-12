(ns scotus.query.transitivity-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [scotus.database.add :as dba]
   [scotus.database.remove :as dbr]
   [scotus.query.transitivity :as transitivity]
   [scotus.database.test-utils :refer [each-fixture once-fixture]]
   [scotus.semantic.assert :as assert]))

(use-fixtures :each each-fixture)
(use-fixtures :once once-fixture)

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
         (transitivity/transitivity '[]
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
         (transitivity/transitivity '[]
                                    '["pet_type" ?x "animal"]
                                    '[{:transitive_arg/predicate "pet_type"
                                       :transitive_arg/arg_name "animal_type"
                                       :transitive_arg/transitive_pred "subclass_of"
                                       :transitive_arg/from_arg "subclass"
                                       :transitive_arg/to_arg "superclass"}]
                                    "universal"
                                    true)))
  (is (= '#{{?x "anparisi" ?y "animal"}}
         (transitivity/transitivity '[{?y "animal"}]
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
         (transitivity/transitivity '[{?y "animal"}]
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
         (transitivity/transitivity '[]
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

         (transitivity/transitivity '[]
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
         (transitivity/transitivity '[{?x "anparisi"}]
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
         (transitivity/transitivity '[{?x "anparisi"}]
                                    '["pet_type" ?x ?y]
                                    [{:transitive_arg/predicate "pet_type"
                                      :transitive_arg/arg_name "animal_type"
                                      :transitive_arg/transitive_pred "subclass_of"
                                      :transitive_arg/from_arg "subclass"
                                      :transitive_arg/to_arg "superclass"}]
                                    "universal" true))))
