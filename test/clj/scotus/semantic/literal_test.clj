(ns scotus.semantic.literal-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [scotus.semantic.literal :as literal]
   [scotus.database.test-utils :refer [each-fixture once-fixture]]))

(use-fixtures :each each-fixture)
(use-fixtures :once once-fixture)

(deftest ^:integration t-variable-arg?
  (is (literal/variable-arg? '["subclass_of" ?x ?y] "subclass"))
  (is (literal/variable-arg? '["subclass_of" ?x ?y] "superclass"))
  (is (not (literal/variable-arg? '["subclass_of" "dog" ?y] "subclass")))
  (is (not (literal/variable-arg? '["subclass_of" ?x "mammal"] "superclass")))
  (is (not (literal/variable-arg? '["subclass_of" "dog" "mammal"] "superclass"))))
