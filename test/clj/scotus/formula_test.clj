(ns scotus.formula-test
  (:require [clojure.test       :refer [is]]
            [scotus.formula     :as  f]
            [scotus.test-config :refer [deftest-simple-index]]))


(deftest-simple-index predicate-test
  (is (f/predicate? "subclass_of"))
  (is (not (f/predicate? "instance"))))

(deftest-simple-index formula?-test
  (let [p (f/make "subclass_of" "cat" "mammal")
        q (f/make "subclass_of" "dog" "mammal")
        r (f/make "subclass_of" "rat" "mammal")
        t (f/make "subclass_of" "frog" "amphibian")
        b (f/make "instance" "penelope" "dog")]
    (is (f/formula? (f/make :if t
                            (f/make :and
                                    p
                                    (f/make :or
                                            (f/make :not q)
                                            r)))))
    (is (not (f/formula? (f/make :if p b))))
    (is (not (f/formula? b)))))
