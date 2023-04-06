(ns scotus.core
  (:require
   [scotus.config  :as cfg]
   [scotus.formula :as f]
   [scotus.state   :as state]))


(defn potentially-setup-scotus!
  "Check if scotus has been initialized in the current environment"
  [])

(defn initialize-scotus!
  "Initialize all configuration and state for scotus to run."
  []
  (cfg/init!)
  (state/init!)
  #_#_ someday
  (map (fn [[predicate args]]
         (f/make-predicate predicate args))
       (state/predicate-index)))

(initialize-scotus!)
