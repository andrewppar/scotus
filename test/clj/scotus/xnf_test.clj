(ns scotus.xnf-test
  (:require [clojure.test        :refer [are deftest is]]
            [scotus.formula      :as f]
            [scotus.test-config  :refer [deftest-simple-index] :as cfg]
            [scotus.xnf          :as xnf]))

(deftest-simple-index implication-out-test
  (let [p ["subclass_of" "cat" "mammal"]
        q ["subclass_of" "dog" "mammal"]]

    (is (= (xnf/implication-out (f/implies p q))
           (f/or (f/not p) q)))
    (is (= (xnf/implication-out (f/and p (f/implies p q)))
           (f/and p (f/or (f/not  p) q))))))

(deftest-simple-index negation-in-test
  (let [p ["subclass_of" "cat" "mammal"]
        q ["subclass_of" "dog" "mammal"]
        r ["subclass_of" "human" "mammal"]]
    (is (= (xnf/negation-in (f/not (f/not p))) p))))

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

;;(f/fmake :and
;;  (f/fmake "sublcass_of" "cat" "mammal")
;;  (f/fmake "subclass_of" "dog" "mammal"))
;;
;;
;;(f/fmake "sublcass_of" "cat" "mammal")

;;(deftest-simple-index cnf-test
;;  (let [p ["subclass_of" "cat" "mammal"]
;;        q ["subclass_of" "dog" "mammal"]
;;        r ["subclass_of" "human" "mammal"]]
;;  (are [formula expectation]
;;     (= expectation (xnf/cnf formula))
;;    (f/not (f/and (f/implies p q) (f/not r)))
;;    (f/and (f/or (f/not p) r) (f/or q r)))))


;;    ¬((¬p→¬q)∧¬r)  (¬p∨r)∧(q∨r)
;;    (p→q)→(¬r∧q)
;;    (p∨¬r)∧(p∨q)∧(¬q∨¬r)
;;
;;    (p -> (q /\ r))
;;    (~ p \/ q) /\ (~ p \/ r)
;;
;;
;;    A → (  ∧  )

;;(def p (f/make "subclass_of" "cat" "mammal"))
;;(def q (f/make "subclass_of" "dog" "mammal"))
;;
;;
;;(f/make :or (f/make :not p) q)
;;
;;  (xnf/implication-out (f/make :if p q))
