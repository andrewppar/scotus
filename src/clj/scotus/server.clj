(ns scotus.server
  (:require [clojure.java.io :as io]
            [compojure.core           :as compojure]
            [compojure.route          :as route]
            [org.httpkit.server       :as server]
            [ring.middleware.cors     :as cors]
            [ring.middleware.defaults :as middleware]
            [scotus.transact          :as transact])
  (:gen-class))


(defn assert!
  "Given a formula add it to the database."
  ([formula asserter]
   (transact/assert! formula asserter))
  ([formula asserter context]
   (transact/assert! formula asserter context)))
