(ns scotus.query.binding-test
  (:require
   [clojure.test :refer [testing is deftest]]
   [scotus.query.binding :as binding]))

(deftest t-formula-apply
  (is (= '["subclass_of" ?x "dog"]
         (binding/formula-apply
          '{?y "dog" ?z "cat"}
          '["subclass_of" ?x ?y])))
  (is (= '["subclass_of" ?x ?y]
         (binding/formula-apply
          '{}
          '["subclass_of" ?x ?y]))))

(deftest t-formula-apply-all
  (is (= '[["subclass_of" ?x "dog"]
           ["subclass_of" ?x "cat"]]
         (binding/formula-apply-all
          '["subclass_of" ?x ?y]
          '[{?y "dog" ?z "cat"}
            {?y "cat" ?z "cat"}])))
  (is (= '[["subclass_of" ?x ?y]]
         (binding/formula-apply-all
          '["subclass_of" ?x ?y]
          '[]))))

(deftest t-expansion?
  (is (binding/expansion?
       '{?x "dog" ?y "cat"}
       '{?x "dog" ?z "elephant"}))
  (is (binding/expansion?
       '{?x "dog"}
       '{?x "dog" ?z "elephant"}))
  (is (binding/expansion?
       '{?x "dog" ?y "cat"}
       '{?x "dog"}))
  (is (not
       (binding/expansion?
        '{?x "dog" ?y "cat"}
        '{?x "dog" ?y "elephant"}))))

(deftest t-expand-all-with-all

  (is (= '#{{?x "anparisi" ?y "cat"}
            {?x "anparisi" ?y "dog"}}
         (binding/expand-all-with-all
          '[{?x "anparisi" ?y "dog"}]
          '[{?x "anparisi" ?y "cat"}])))

  (is (= '#{{?x "anparisi" ?y "cat" ?z "dog"}}
         (binding/expand-all-with-all
          '[{?x "anparisi" ?z "dog"}]
          '[{?x "anparisi" ?y "cat"}])))

  (is (= '#{{?x "anparisi" ?y "dog" :justification [1 2]}}
         (binding/expand-all-with-all
          '[{?x "anparisi" :justification [2]}]
          '[{?x "anparisi" ?y "dog" :justification [1]}])))

  (is (= '#{{?x "anparisi" ?y "cat"}
            {?x "anparisi" ?y "dog"}}
         (binding/expand-all-with-all
          '[{?x "anparisi" ?y "cat"}]
          '[{?x "anparisi" ?y "dog"}])))

  (is (= '#{{?x "anparisi" ?y "cat" ?z "dog"}}
         (binding/expand-all-with-all
          '[{?x "anparisi" ?y "cat"}]
          '[{?x "anparisi" ?z "dog"}])))

  (is (= '#{{?x "anparisi" ?y "dog" :justification [2 1]}}
         (binding/expand-all-with-all
          '[{?x "anparisi" ?y "dog" :justification [1]}]
          '[{?x "anparisi" :justification [2]}])))

  (is (= '#{{?x "anparisi"}}
         (binding/expand-all-with-all
          '[{?x "anparisi"}]
          [])))

  (is (= '#{{?x "anparisi"}}
         (binding/expand-all-with-all
          []
          '[{?x "anparisi"}]))))
