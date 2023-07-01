(ns scotus.assert
  (:require
   [scotus.json       :as json]
   [scotus.identifier :as id]))

(defn id [pred args context]
  (-> {:formula `[~pred ~@args] :context context}
      json/encode
      id/encode))

(defn from-id [assert-id]
  (-> assert-id
      id/decode
      json/decode))

(defrecord Assert [pred args context negative? justification id])


(defmethod print-method Assert
  [{:keys [pred args context]} writer]
  (.write writer (format "Assert#%s:%s" `[~pred ~@args] context)))

(defn make
  [pred args context negative? justification]
  (let [assert-id (id pred args context)]
    (->Assert pred args context negative? justification assert-id)))

(defn assert?
  [object]
  (isa? (type object) scotus.assert.Assert))

(defn arg
  [{:keys [args]} argnum]
  (get args (dec argnum)))

(defn predicate
  [assert]
  (get assert :pred))
