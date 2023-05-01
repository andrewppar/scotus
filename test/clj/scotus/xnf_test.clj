(ns scotus.xnf-test
  (:require [clojure.test           :refer [are deftest is]]
            [scotus.formula.formula :as f]
            [scotus.test-config     :refer [deftest-simple-index] :as cfg]
            [scotus.xnf             :as xnf]))

(deftest-simple-index implication-out-test
  (let [p ["subclass_of" "cat" "mammal"]
        q ["subclass_of" "dog" "mammal"]]

    (is (= (xnf/implication-out (f/implies p q))
           (f/or (f/not p) q)))
    (is (= (xnf/implication-out (f/and p (f/implies p q)))
           (f/and p (f/or (f/not  p) q))))))

(deftest-simple-index simple-triviality?-test
  (let [p      ["subclass_of" "cat" "mammal"]
        q      ["subclass_of" "dog" "mammal"]
        not-p  (f/not p)]
    (is (not (xnf/simple-triviality? (f/or p q))))
    (is (xnf/simple-triviality? (f/or p not-p)))
    (is (xnf/simple-triviality? (f/or p q not-p)))
    (is (xnf/simple-triviality? (f/or not-p p q)))))

(deftest-simple-index negation-in-test
  (let [p ["subclass_of" "cat" "mammal"]
        q ["subclass_of" "dog" "mammal"]
        r ["subclass_of" "human" "mammal"]]
    (is (= p (xnf/negation-in (f/not (f/not p)))))
    (is (= (f/not p)
           (xnf/negation-in (f/not (f/not (f/not p))))))
    (is (= (f/or (f/not p) (f/not q))
           (xnf/negation-in (f/not (f/and p q)))))
    (is (= (f/or (f/not p) (f/not q))
           (xnf/negation-in
            (f/or (f/not p) (f/not q)))))
    (is (= (f/and (f/not p) q)
           (xnf/negation-in (f/not (f/or p (f/not q))))))
    (is (= (f/implies p q)
           (xnf/negation-in (f/implies p (f/not (f/not q))))))
    (is (= (f/and p q)
           (xnf/negation-in (f/not (f/implies p (f/not q))))))))

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
  (let [p ["subclass_of" "cat" "mammal"]
        q ["subclass_of" "dog" "mammal"]
        r ["subclass_of" "human" "mammal"]]
    (are [formula expectation]
         (= expectation (xnf/conjunction-in formula))
      ;; simple case
      (f/and (f/or p q) r)
      (f/or (f/and p r) (f/and q r))

        ;; noop cases
      (f/and p)     (f/and p)
      (f/and p q)   (f/and p q)
      (f/or p q)    (f/or p q)

        ;; embedded cases
      (f/or (f/and p (f/or q r)) p)
      (f/or (f/or (f/and p q) (f/and p r)) p))))

(deftest-simple-index cnf-test
  (let [p ["subclass_of" "cat" "mammal"]
        q ["subclass_of" "dog" "mammal"]
        r ["subclass_of" "human" "mammal"]]
    (are [formula expectation]
        (= expectation (xnf/cnf formula))

      (f/not (f/and (f/implies p q) (f/not r)))
      (f/and (f/or p r) (f/or (f/not q) r))

      (f/implies (f/implies p q) (f/and (f/not r) q))
      (f/and
       (f/or (f/not r) p)
       (f/or (f/not r) (f/not q))
       (f/or p q)
       ;; TODO: Remove tautologies
       ;;  (f/or (f/not q) q)
       )

      (f/implies p (f/and q r))
      (f/and
       (f/or (f/not p) q)
       (f/or (f/not p) r))

      p
      p

      (f/and p q)
      (f/and p q)

      (f/or p q)
      (f/or p q)

      (f/and p (f/and q r))
      (f/and q r p)

      (f/and p (f/not q))
      (f/and p (f/not q))

      (f/and p (f/not (f/or q r)))
      (f/and (f/not q) (f/not r) p)
      )))
