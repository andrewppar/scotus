(ns scotus.query.assert-spec-test
  (:require
   [clojure.test :refer [testing is deftest use-fixtures]]
   [scotus.database.test-utils :refer [each-fixture once-fixture]]
   [scotus.query.assert-spec :as aspec]))

(use-fixtures :each each-fixture)
(use-fixtures :once once-fixture)

(deftest ^:integration t->binding
  (is (= '{?x "dog"}
         (aspec/->binding
          '["subclass_of" ?x "mammal"]
          {:subclass_of/subclass "dog" :subclass_of/superclass "mammal"})))
  (is (= '{?x "dog" ?y "mammal"}
         (aspec/->binding
          '["subclass_of" ?x ?y]
          {:subclass_of/subclass "dog" :subclass_of/superclass "mammal"})))
  (is (= '{?x "dog" :justification [["subclass_of" "dog" "mammal"]]}
         (aspec/->binding
          '["subclass_of" ?x "mammal"]
          {:subclass_of/subclass "dog" :subclass_of/superclass "mammal"}
          :justification? true)))
  (is (= '{?x "dog" ?y "mammal" :justification [["subclass_of" "dog" "mammal"]]}
         (aspec/->binding
          '["subclass_of" ?x ?y]
          {:subclass_of/subclass "dog" :subclass_of/superclass "mammal"}
          :justification? true))))
