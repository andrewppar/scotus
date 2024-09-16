(ns scotus.database.justification
  (:require
   [honey.sql.helpers :as h]
   [scotus.database.utils :as utils]
   [scotus.json :as json]))

(defn add [assertion-ids justification]
  (let [justification-id  (if (string? justification)
                            (utils/->uuid justification)
                            (utils/->uuid (set justification)))
        justification-row {:id justification-id :justification [:cast (json/encode justification) :jsonb]}
        joins (map
               (fn [assertion-id]
                 {:assertion-id assertion-id
                  :justification-id justification-id})
               assertion-ids)]
    (+
     (-> (h/insert-into :assertion-justification)
         (h/values joins)
         (h/on-conflict :assertion-id :justification-id)
         h/do-nothing
         utils/execute!
         utils/insert-count)
     (-> (h/insert-into :justification)
         (h/values [justification-row])
         (h/on-conflict :id)
         h/do-nothing
         utils/execute!
         utils/insert-count))))

(defn create-table []
  (every?
   utils/ddl-success?
   [
    (-> (h/create-table :justification)
        (h/with-columns [[:id :uuid] [:derived :bool] [:justification :jsonb]])
        utils/execute!)

    (-> (h/create-table :assertion-justification)
        (h/with-columns [[:assertion-id :uuid] [:justification-id :uuid]])
        utils/execute!)

    (-> (h/alter-table :assertion-justification)
        (h/add-index :primary-key :assertion-id :justification-id)
        utils/execute!)]))
