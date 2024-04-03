(ns scotus.database.assert-map)

(def empty-assert-map {:assert-count 0})

(defn merge-assert-maps-internal [map-one {:keys [assert-count]}]
  (update map-one :assert-count + assert-count))

(defn merge-assert-maps [& maps]
  (reduce merge-assert-maps-internal empty-assert-map maps))
