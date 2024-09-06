(ns scotus.syntax.formula-test
  (:require [clojure.test :refer [is deftest]]
            [scotus.syntax.formula :as  f]))

(deftest formula?-test
  (let [p ["subclass_of" "cat" "mammal"]
        q ["subclass_of" "dog" "mammal"]
        r ["subclass_of" "rat" "mammal"]
        t ["subclass_of" "frog" "amphibian"]]
    (is (f/formula? [:and p q]))
    (is (f/formula? [:or p q r]))
    (is (f/formula? [:implies [:and p q] [:or [:not r] t]]))))

(deftest t-same?
  (let [p ["subclass_of" "cat" "mammal"]
        q ["subclass_of" "dog" "mammal"]
        r ["subclass_of" "rat" "mammal"]
        t ["subclass_of" "frog" "amphibian"]]
    (is (f/same? p p))
    (is (f/same? [:and p q] [:and q p]))
    (is (not (f/same? [:and p q] [:or p q])))
    (is (f/same? [:and [:or p q] r t] [:and r [:or q p] t]))
    (is (f/same? [:and p] [:and p p]))
    (is (not (f/same? [:and p q] [:and p r])))))

(deftest t-substitute
  (let [p ["subclass_of" "cat" "mammal"]
        q ["subclass_of" "dog" "mammal"]
        r ["subclass_of" "rat" "mammal"]
        t '["subclass_of" ?variable "amphibian"]]

    (is (= (f/substitute t '?variable '?replaced)
           '["subclass_of" ?replaced "amphibian"]))

    (is (= (f/substitute t '?other '?replaced)
           '["subclass_of" ?variable "amphibian"]))

    (is (= (f/substitute [:and p  q] "cat" "dog")
           [:and q q]))

    (is (= (f/substitute [:and p  q] "mammal" "animal")
           [:and ["subclass_of" "cat" "animal"] ["subclass_of" "dog" "animal"]]))

    (is (= (f/substitute p p q) q))

    (is (= (f/substitute [:implies p q] q r)
           [:implies p r]))

    (is (= (f/substitute [:implies [:and [:or p q] r] [:or p q]] [:or p q] t)
           [:implies [:and t r] t]))))
