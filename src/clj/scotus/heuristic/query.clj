(ns scotus.heuristic.query
  (:require [scotus.database.query :as dbq]
            [scotus.natural-language.database.query :as nlp.query]
            [scotus.heuristic.utils :as utils]))

(defn lookup-assertion [assertion-id]
  (let [predicate (dbq/predicate-for-id assertion-id)]
    (if (utils/nlp-predicate? predicate)
      (nlp.query/lookup-assertion assertion-id predicate)
      (dbq/lookup-assertion assertion-id (keyword predicate)))))

(defn lookup-rows [pred contexts negated? specs]
  (if (utils/nlp-predicate? pred)
    (nlp.query/lookup pred contexts specs)
    (dbq/lookup-rows pred contexts negated? specs)))
