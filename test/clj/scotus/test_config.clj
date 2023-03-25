(ns scotus.test-config
  (:require [clojure.test :refer [deftest]]
            [scotus.state :as state]))

(def simple-index {"subclass_of" ["subclass" "superclass"]})

(defmacro deftest-simple-index
  [test-name & body]
  `(deftest ~test-name
     (with-redefs [state/predicate-index (constantly simple-index)]
       ~@body)))
