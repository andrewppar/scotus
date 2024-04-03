(ns  scotus.query.assert-spec-test
  (:require
   [clojure.test :refer [testing is deftest]]
   [scotus.query.assert-spec :as aspec]))

(deftest t->binding
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
