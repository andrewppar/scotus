(ns scotus.database.test-utils
  (:require
   [scotus.config :as config]
   [scotus.setup :as setup]
   [scotus.state :as state]))

(defn once-fixture [f]
  (config/init!)
  (let [old-kb (config/get-item :db :name)]
    (config/init!)
    (config/set-item [:db :name] "kb_test")
    (state/init!)
    (f)
    (config/set-item [:db :name] old-kb)
    (state/close-store)
    (state/init!)))

(defn each-fixture [f]
  (setup/setup!)
  (f)
  (setup/tear-down!))
