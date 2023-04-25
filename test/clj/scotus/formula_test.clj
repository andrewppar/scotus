(ns scotus.formula-test
  (:require [clojure.test       :refer [is deftest]]
            [scotus.formula     :as  f]
            [scotus.test-config :refer [deftest-simple-index]]))


(deftest-simple-index predicate-test
  (is (f/predicate? "subclass_of"))
  (is (not (f/predicate? "instance"))))

(deftest-simple-index formula?-test
  (let [p ["subclass_of" "cat" "mammal"]
        q ["subclass_of" "dog" "mammal"]
        r ["subclass_of" "rat" "mammal"]
        t ["subclass_of" "frog" "amphibian"]
        b ["instance" "penelope" "dog"]]
    (is (f/formula? (f/implies
                     t (f/and
                        p (f/or (f/not q) r)))))
    (is (not (f/formula? (f/implies p b))))
    (is (not (f/formula? b)))))

(deftest t-signature
  (is (= (f/signature
          '["subclass_of" ?x "dog"])
         {:predicate "subclass_of"
          :arg-signature
          '[?x nil]}))
  (is (= (f/signature '["subclass_of" ?x ?y])
         {:predicate "subclass_of"
          :arg-signature
          '[?x ?y]}))
  (is (= (f/signature '["subclass_of" "dog" "mammal"])
         {:predicate "subclass_of"
          :arg-signature
          [nil nil]})))
