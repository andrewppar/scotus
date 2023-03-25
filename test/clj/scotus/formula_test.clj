(ns scotus.formula-test
  (:require [clojure.test       :refer [is]]
            [scotus.formula     :as  f]
            [scotus.test-config :refer [deftest-simple-index]]
            ))



(deftest-simple-index predicate-test
  (is (f/predicate? "subclass_of"))
  (is (not (f/predicate? "instance"))))

(deftest-simple-index formulap-test
  (let [p (f/atomic "subclass_of" "cat" "mammal")
        q (f/atomic "subclass_of" "dog" "mammal")
        r (f/atomic "subclass_of" "rat" "mammal")
        t (f/atomic "subclass_of" "frog" "amphibian")
        b (f/atomic "instance" "penelope" "dog")
        ]
    (is (f/formula? (f/lif t (f/land p (f/lor (f/lnot q) r)))))
    (is (not (f/formula? (f/lif p b))))
    (is (not (f/formula? b)))))
