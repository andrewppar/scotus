(ns scotus.heuristic.utils
  (:require [scotus.state :as state]))

(defn nlp-predicate? [predicate]
  (seq (state/table-nlp-args predicate)))
