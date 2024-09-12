(ns scotus.syntax.xnf-test
  (:require [clojure.test :refer [are deftest is]]
            [scotus.syntax.formula :as f]
            [scotus.syntax.xnf :as xnf]))


(deftest t-conjunction-left
  (let [p ["P" "p"]
        q ["P" "q"]
        r ["P" "r"]]
    (is (= #{{:left #{p q} :right #{}}}
           (xnf/conjunction-left
            {:left #{[:and p q]} :right #{}})))

    (is (= #{{:left #{p q r} :right #{}}}
           (xnf/conjunction-left
            {:left #{[:and p q r]} :right #{}})))

    (is (= #{{:left #{p [:and q r]} :right #{p}}}
           (xnf/conjunction-left
            {:left #{[:and p [:and q r]]} :right #{p}})))))


(deftest t-conjunction-right
  (let [p ["P" "p"]
        q ["P" "q"]
        r ["P" "r"]
        s ["P" "s"]]
    (is (= #{{:left #{r} :right #{p}} {:left #{r} :right #{q}}}
           (xnf/conjunction-right
            {:left #{r} :right #{[:and p q]}})))

    (is (= #{{:left #{} :right #{p}}
             {:left #{} :right #{q}}
             {:left #{} :right #{r}}}
           (xnf/conjunction-right
            {:left #{} :right #{[:and p q r]}})))

    (is (= #{{:left #{} :right #{p r}}
             {:left #{} :right #{q r}}
             {:left #{} :right #{p s}}
             {:left #{} :right #{q s}}}
           (xnf/conjunction-right
            {:left #{} :right #{[:and p q] [:and r s]}})))

    (is (= #{{:left #{} :right #{}}}
           (xnf/conjunction-right {:left #{} :right #{}})))

    (is (= #{{:left #{p} :right #{q}}}
           (xnf/conjunction-right {:left #{p} :right #{q}})))))

(deftest t-disjunction-left
  (let [p ["P" "p"]
        q ["P" "q"]
        r ["P" "r"]
        s ["P" "s"]]
    (is (= #{{:left #{p} :right #{[:and p q]}}
              {:left #{q} :right #{[:and p q]}}}
           (xnf/disjunction-left
            {:left #{[:or p q]} :right #{[:and p q]}})))

    (is (= #{{:left #{[:and p q]} :right #{[:and p q]}}}
           (xnf/disjunction-left
            {:left #{[:and p q]} :right #{[:and p q]}})))

    (is (= #{{:left #{p r} :right #{}}
             {:left #{q r} :right #{}}
             {:left #{p s} :right #{}}
             {:left #{q s} :right #{}}}
           (xnf/disjunction-left
            {:left #{[:or p q] [:or r s]} :right #{}})))

    (is (= #{{:left #{} :right #{}}}
           (xnf/disjunction-left {:left #{} :right #{}})))))

(deftest t-dnf
  (let [p ["subclass_of" "cat" "mammal"]
        q ["subclass_of" "dog" "mammal"]
        r ["subclass_of" "human" "mammal"]
        s ["subclass_of" "human" "person"]]
    (is (f/same? p (xnf/dnf [:not [:not p]])))
    (is (= :tautology (xnf/dnf [:implies p [:not [:not p]]])))
    (is (= :contradiction (xnf/dnf [:and p [:not p]])))
    (is (f/same? [:or [:and r [:not q]] [:and p [:not q]]]
           (xnf/dnf [:and [:not q] [:or r p]])))
    (is (f/same? [:or [:not p] [:not q]]
           (xnf/dnf [:or [:not p] [:not q]])))
    (is (f/same? [:or [:and p r] [:and q r] [:and s q] [:and s p]]
           (xnf/dnf [:and [:or p q] [:or r s]])))
    (is (f/same? [:or [:and s p] [:and p r] [:and q r] [:and s q]]
           (xnf/dnf
            [:or [:and p s] [:and p r] [:and q s] [:and q r]])))))

(deftest t-cnf
  (let [p ["subclass_of" "cat" "mammal"]
        q ["subclass_of" "dog" "mammal"]
        r ["subclass_of" "human" "mammal"]]
    (is (f/same? [:and [:or p r] [:or r [:not q]]]
                 (xnf/cnf [:not [:and [:implies p q] [:not r]]])))

    (is (= :tautology (xnf/cnf [:implies p [:not [:not p]]])))
    (is (= :contradiction (xnf/cnf [:and p [:not p]])))


    (is (f/same? [:and
            [:or p [:not r]]
            [:or [:not q] [:not r]]
            [:or p q]]
           (xnf/cnf
            [:implies [:implies p q] [:and [:not r] q]])))

    (is (f/same? [:and
            [:or q [:not p]]
            [:or r [:not p]]]
           (xnf/cnf
            [:implies p [:and q r]])))

    (is (f/same? p (xnf/cnf p)))

    (is (f/same? [:and q p] (xnf/cnf [:and p q])))

    (is (f/same? [:or p q] (xnf/cnf [:or p q])))

    (is (f/same? [:and q r p] (xnf/cnf [:and p [:and q r]])))

    (is (f/same? [:and [:not q] p] (xnf/cnf [:and p [:not q]])))

    (is (f/same? [:and p [:not q] [:not r]] (xnf/cnf [:and p [:not [:or q r]]])))))
