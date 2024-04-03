(ns scotus.core
  (:require
   [scotus.config :as cfg]
   [scotus.setup :as setup]
   [scotus.state :as state]))

(defn init!
  "Initialize all configuration and state for scotus to run."
  []
  (cfg/init!)
  (state/init!)
  (when-not (setup/setup?)
    (setup/setup!)))



;; knowledge management

(defn required-field
  [operation field-name value]
  (when-not value
    (throw
     (ex-info (format "%s is a required field for %s"
                      field-name operation)
              {}))))

(defmacro defn-api
  "Create an API.

  This works the same as `defn` with the exception that a vector of
  `required-args` is required after the function arguments. The reason
  for this is to be able to specify keyword args, but throw an error
  for any that must be present for the API to work."
  [fn-name documentation-string args required-args & body]
  (when-not (string? documentation-string)
    (throw
     (ex-info "A documentation string is required for `defn-api`"
              {:caused-by (name fn-name)})))
  (let [required-fns (map
                      (fn [arg]
                        `(required-field
                          ~(name fn-name) ~(name arg) ~arg))
                      required-args)]
    `(defn ~fn-name ~documentation-string ~args
       (do
         (do ~@required-fns)
         (do ~@body)))))

#_
(defn-api assert!
  "Assert `formula`.

   - `asserter` is a required keyword argument.
   - `context` is an optional keyword argument. If it is not supplied
               the context is \"universal\"."
  [formula & {:keys [asserter context] :or {context "universal"}}]
  [asserter]
  (transact/assert! formula asserter context))

;; Maybe asserter should be from a login value or a config value.
#_
(defn-api create-predicate!
  "Create a new predicate.

  - `args` is a required keyword argument that names the arguments for
           the newly created predicate.
  - `asserter` is a required keyword argument."
  [predicate & {:keys [asserter args]}]
  [asserter args]
  (transact/create-predicate! predicate args asserter))

#_
(defn-api delete-predicate!
  "Delete `predicate` from sctous.

  - `predicate` is the predicate to be removed."
  [predicate]
  []
  (transact/delete-predicate! predicate))

#_
(defn-api query
  "Run a query."
  [formula & {:keys [context] :or {context "universal"}}]
  []
  (query/query formula context))

#_
(defn-api retract!
  "Remove an assertion.

  - `context` is an optional keyword argument. If it is not supplied
    the context is \"universal\"."
  ;; Think about whether or not we really want "universal"
  ;; to be the default or if we should also have a "lowest"
  ;; context
  [formula & {:keys [context] :or {context "universal"}}]
  []
  (let [contexts (->> "universal"
                      (query/query
                       `["subcontext_of" ~context ?context])
                      (map
                       (fn [result]
                         (get result 'scotus.core/?context))))]
  (transact/retract! formula contexts)))


(comment
  (init!)

  (state/tables)



  )
