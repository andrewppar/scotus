(ns scotus.xnf-test
  (:require [clojure.test        :refer [are deftest is]]
            [scotus.formula      :as f]
            [scotus.test-config  :refer [deftest-simple-index] :as cfg]
            [scotus.xnf          :as xnf]))

(deftest-simple-index implication-out-test
  (let [p (f/make "subclass_of" "cat" "mammal")
        q (f/make "subclass_of" "dog" "mammal")]
    (is (= (xnf/implication-out (f/make :if p q))
           (f/make :or (f/make :not p) q)))
    (is (= (xnf/implication-out (f/make :and p (f/make :if p q)))
           (f/make :and p (f/make :or (f/make :not  p) q))))))

(deftest-simple-index negation-in-test
  (let [p (f/make "subclass_of" "cat" "mammal")
        q (f/make "subclass_of" "dog" "mammal")
        r (f/make "subclass_of" "human" "mammal")]
    (is (= (xnf/negation-in (f/make :not (f/make :not p)))
           p))))

(deftest add-each-to-each-test
  (are [blocks to-add result]
      (= result (xnf/add-each-to-each blocks to-add))
    ;; blocks          to-add  result
    [[1 2]]            [3 4]   [[1 2 3] [1 2 4]]

    [[1 2] [3 4]]      [5 6]   [[1 2 5] [3 4 5]
                                [1 2 6] [3 4 6]]

    [[1 2] [3 4]]      [5 6 7] [[1 2 5] [3 4 5]
                                [1 2 6] [3 4 6]
                                [1 2 7] [3 4 7]]
    [[1 2 5] [3 4 5]
     [1 2 6] [3 4 6]
     [1 2 7] [3 4 7]] [8 9]     [[1 2 5 8] [3 4 5 8]
                                 [1 2 6 8] [3 4 6 8]
                                 [1 2 7 8] [3 4 7 8]
                                 [1 2 5 9] [3 4 5 9]
                                 [1 2 6 9] [3 4 6 9]
                                 [1 2 7 9] [3 4 7 9]]))

(deftest-simple-index conjunction-in-test
  (let [p (f/make "subclass_of" "cat" "mammal")
        q (f/make "subclass_of" "dog" "mammal")
        r (f/make "subclass_of" "human" "mammal")]
    (are [formula expectation]
        (= expectation (xnf/conjunction-in formula))
      ;; simple case
      (f/make :and (f/make :or p q) r)
      (f/make :or (f/make :and p r) (f/make :and q r))

      ;; noop cases
      (f/make :and p)     (f/make :and p)
      (f/make :and p q)   (f/make :and p q)
      (f/make :or p q)    (f/make :or p q)

      ;; embedded cases
      (f/make :or (f/make :and p (f/make :or q r)) p)
      (f/make :or (f/make :or
                          (f/make :and p q)
                          (f/make :and p r))
              p))))
