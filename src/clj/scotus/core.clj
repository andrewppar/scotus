(ns scotus.core
  (:require
   [scotus.config :as cfg]
   [scotus.state  :as state]))


(defn potentially-setup-scotus!
  "Check if scotus has been initialized in the current environment"
  [])

(defn initialize-scotus!
  "Initialize all configuration and state for scotus to run."
  []
  (cfg/init!)
  (state/init!))

(initialize-scotus!)
