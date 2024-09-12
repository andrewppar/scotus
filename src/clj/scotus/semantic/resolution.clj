(ns scotus.semantic.resolution
  (:require
   [scotus.database.query :as dbq]
   [scotus.query.assert-spec :as assert-spec]
   [scotus.semantic.literal :as literal]
   [scotus.syntax.formula :as f]
   [scotus.logic.closure :as cl]))

(defn ->context
  "Resolve a `context` into all its subcontexts."
  [context]
  (cl/resolve-contexts context))

;; TODO We need a semantic lookup layer instead of dbq
(defn <-literal
  "Resolve any metalinguistic references in `literal` to their formulas."
  [literal & {:keys [context] :or {context "universal"}}]
  (let [pred (f/literal-predicate literal)
        ctxts (->context context)
        specs [[pred nil "assertion"]]
        assertion-args (map
                        (fn [spec] (get spec :arg_instance/argument))
                        (dbq/lookup-rows "arg_instance" ctxts false specs))
        arg-map (literal/->map literal)]
    (literal/<-map (reduce
            (fn [result assert-arg]
              (let [assert-spec (dbq/lookup-assertion (get arg-map assert-arg))
                    pred (namespace (first (keys assert-spec)))
                    formula (assert-spec/->formula assert-spec)
                    context (get assert-spec (keyword pred "context"))]
                (assoc result assert-arg {:formula formula :context context})))
            arg-map
            assertion-args)
           literal)))

(defn ->literal
  "Replace any assertion specification in `literal` with their assertion
  ids."
  [literal & {:keys [context] :or {context "universal"}}]
  (let [pred (f/literal-predicate literal)
        ctxts (->context context)
        specs [[pred nil "assertion"]]
        assertion-args (map
                        (fn [spec] (get spec :arg_instance/argument))
                        (dbq/lookup-rows "arg_instance" ctxts false specs))
        arg-map (literal/->map literal)]
    (literal/<-map (reduce
            (fn [result assert-arg]
              (let [{subcontext :context :keys [formula]} (get arg-map assert-arg)
                    pred (f/literal-predicate formula)
                    spec (f/literal-args formula)
                    negated? (f/negation? formula)
                    id (-> pred
                           (dbq/lookup-rows
                            [subcontext] negated? [spec] :include-meta? true)
                           first
                           (get (keyword pred "id"))
                           str)]
                (assoc result assert-arg id)))
            arg-map
            assertion-args)
           literal)))
