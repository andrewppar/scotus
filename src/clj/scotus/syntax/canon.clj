(ns scotus.syntax.canon
  (:require [scotus.syntax.formula :as f]
            [scotus.syntax.xnf :as xnf]))

(def purposes #{:query :assert})

(defn enact [formula purpose]
  (let [normal-form (case purpose
                      :assert (xnf/cnf formula)
                      :query (xnf/dnf formula))]))
