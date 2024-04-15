(ns scotus.semantic.retract
  (:require
   [scotus.database.remove :as dbr]
   [scotus.syntax.canon :as canon]
   [scotus.syntax.formula :as f]))

(defn predicate!
  [predicate]
  (dbr/drop-table! predicate))

(defn retract-literal! [literal context]
  (let [predicate (f/literal-predicate literal)
        spec (mapv
              (fn [arg] (when-not (f/variable? arg) arg))
              (f/literal-args literal))
        negated? (f/negation? literal)]
    (dbr/delete-rows predicate context negated? spec)))


(defn !
  [formula &
   {:keys [context] :or {context "universal"}}]
  (let [dnf (canon/enact formula :query)]
    (cond (f/literal? formula)
          (retract-literal! formula context)

          (and (f/conjunction? formula)
               (every? f/literal? (f/args formula)))
          (mapv
           (fn [literal]
             (retract-literal! literal context)))

          :else
          (throw
           (ex-info "I don't know how to retract rules yet."
                    {:caused-by dnf})))))
