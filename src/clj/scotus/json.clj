(ns scotus.json
  (:require [jsonista.core :as json]))

(defn encode [clojure-map]
  (json/write-value-as-string clojure-map))
