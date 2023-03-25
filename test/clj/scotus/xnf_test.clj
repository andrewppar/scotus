(ns scotus.xnf-test
  (:require [clojure.test        :refer [is]]
            [scotus.formula      :as f]
            [scotus.test-config  :refer [deftest-simple-index]]
            [scotus.xnf          :as xnf]))

(deftest-simple-index implication-out-test
  (let [p (f/atomic "subclass_of" "cat" "mammal")
        q (f/atomic "subclass_of" "dog" "mammal")]
    (is (= (xnf/implication-out (f/lif p q))
           (f/lor (f/lnot p) q)))
    (is (= (xnf/implication-out (f/land p (f/lif p q)))
           (f/land p (f/lor (f/lnot p) q))))))

(deftest-simple-index negation-in-test
  (let [p (f/atomic "subclass_of" "cat" "mammal")
        q (f/atomic "subclass_of" "dog" "mammal")
        r (f/atomic "subclass_of" "human" "mammal")]
    (is (= (xnf/ngeation-in (f/lnot (f/lnot p)))
           p))))
