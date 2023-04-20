(ns scotus.core
  (:require
   [scotus.config  :as cfg]
   [scotus.formula :as f]
   [scotus.setup   :as setup]
   [scotus.state   :as state]))

(defn initialize-scotus!
  "Initialize all configuration and state for scotus to run."
  []
  (when-not (setup/setup?)
    (setup/setup!))
  (cfg/init!)
  (state/init!))
  #_#_ someday
  (map (fn [[predicate args]]
         (f/make-predicate predicate args))
       (state/predicate-index)))

(initialize-scotus!)
