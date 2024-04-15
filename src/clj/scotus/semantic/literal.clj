(ns scotus.semantic.literal
  (:require
   [scotus.state :as state]
   [scotus.syntax.formula :as f]))

(defn ->map [literal]
  (let [pred (f/literal-predicate literal)
        args (f/literal-args literal)]
    (zipmap (state/table-args pred) args)))

(defn <-map [arg-name->arg literal]
  (let [pred (f/literal-predicate literal)
        arg-names (state/table-args pred)
        args (map (fn [arg-name] (get arg-name->arg arg-name)) arg-names)
        atomic (f/->atom pred args)]
    (if (f/negation? literal)
      [:not atomic]
      atomic)))

(defn arg-get
  "Get the value of `arg-name` in `literal`."
  [literal arg-name]
  (get (->map literal) arg-name))

(defn variable-arg?
  "Check if the value in `arg-name` of `literal` is a variable."
  [literal arg-name]
  (f/variable? (arg-get literal arg-name)))

(defn put
  "Assign `arg-name` to `value` in `literal`."
  [literal arg-name value]
  (-> literal
      (->map)
      (assoc arg-name value)
      (<-map literal)))

(defn unifies? [query-spec fully-bound]
  (and
   (f/fully-bound? fully-bound)
   (= (f/literal-predicate query-spec)
      (f/literal-predicate fully-bound))
   (every? identity
           (map (fn [query-arg fully-bound-arg]
                  (or (f/variable? query-arg)
                      (= query-arg fully-bound-arg)))
                (f/literal-args query-spec)
                (f/literal-args fully-bound)))))
