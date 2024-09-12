(ns scotus.semantic.resolution-test
(:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [scotus.database.test-utils :refer [each-fixture once-fixture]]
   [scotus.semantic.assert :as assert]
   [scotus.database.query :as dbq]
   [scotus.semantic.resolution :as resolution]
   [scotus.semantic.retract :as retract]))

(use-fixtures :each each-fixture)
(use-fixtures :once once-fixture)

(deftest ^:integration t-<-literal
  (retract/predicate! "believes")
  (assert/predicate! "believes" :args ["person" "formula"])
  (assert/!
  [:and
   ["arg_instance" "believes" "formula" "assertion"]
   ["instance" "Penny" "dog"]]
  :asserter "anparisi")
  (let [assert-id (-> "instance"
                      (dbq/lookup-rows
                       ["universal"] false [["Penny" "dog"]] :include-meta? true)
                      first
                      (get :instance/id)
                      str)]
    (is (= ["believes" "anparisi" {:formula ["instance" "Penny" "dog"]
                                   :context "universal"}]
           (resolution/<-literal ["believes" "anparisi" assert-id])))))

(deftest ^:integration t-unresolve
  (retract/predicate! "believes")
  (assert/predicate! "believes" :args ["person" "formula"])
  (assert/!
   [:and
    ["arg_instance" "believes" "formula" "assertion"]
    ["instance" "Penny" "dog"]]
   :asserter "anparisi")
  (let [assert-id (-> "instance"
                      (dbq/lookup-rows
                       ["universal"] false [["Penny" "dog"]] :include-meta? true)
                      first
                      (get :instance/id)
                      str)]
    (is (= ["believes" "anparisi" assert-id]
           (resolution/->literal
            ["believes" "anparisi" {:formula ["instance" "Penny" "dog"]
                                    :context "universal"}])))))
