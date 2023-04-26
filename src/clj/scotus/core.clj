(ns scotus.core
  (:require
   [scotus.config   :as cfg]
   [scotus.formula  :as f]
   [scotus.query    :as query]
   [scotus.setup    :as setup]
   [scotus.state    :as state]
   [scotus.transact :as transact]))

(defn initialize-scotus!
  "Initialize all configuration and state for scotus to run."
  []
  (cfg/init!)
  (state/init!)
  (when-not (setup/setup?)
    (setup/setup!)))

(comment :someday
(map (fn [[predicate args]]
         (f/make-predicate predicate args))
     (state/predicate-index))
)


;; knowledge management

;; maybe make this a macro...
(defn required-field
  [operation field-name value]
  (when-not value
    (throw
     (ex-info (format "%s is a required field for %s"
                      field-name operation)
              {}))))

(defn assert!
  [formula & {:keys [asserter context] :or {context "universal"}}]
  (required-field "assert!" "asserter" asserter)
  (transact/assert! formula asserter context))

;; Maybe asserter should be from a login value or a config value.
(defn create-predicate!
  [predicate & {:keys [asserter args]}]
  (required-field "create-predicate!" "asserter" asserter)
  (transact/create-predicate! predicate args asserter))

(defn query
  [formula context]
  (query/query formula context))


(comment
  (initialize-scotus!)
  )
