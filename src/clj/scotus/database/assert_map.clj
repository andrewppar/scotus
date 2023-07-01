(ns scotus.database.assert-map)

(def empty-assert-map {:assert-count 0})

(defn merge-assert-maps-internal [map-one map-two]
  (let [old-count (get map-one :assert-count)
        new-count (get map-two :assert-count)]
    {:assert-count (+ old-count new-count)}))

(defn merge-assert-maps [& maps]
  (reduce merge-assert-maps-internal empty-assert-map maps))
