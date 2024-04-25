(ns scotus.query.resolution
  (:require
   [scotus.syntax.formula :as f]
   [scotus.query.binding :as binding]))

(defn r-inequality [literal bindings _ compare-fn]
  (set
   (filter
    (fn [binding]
      (let [args (f/literal-args
                  (binding/formula-apply binding literal))]
        (apply compare-fn args)))
    bindings)))

(defn r-<= [literal bindings context]
  (r-inequality literal bindings context <=))

(defn r->= [literal bindings context]
  (r-inequality literal bindings context >=))

(defn r-< [literal bindings context]
  (r-inequality literal bindings context <))

(defn r-> [literal bindings context]
  (r-inequality literal bindings context >))

(def resolution-modules
  [{:name :<=
    :test-fn (fn [literal bindings]
               (and (= (f/predicate literal) "<=")
                    (every?
                     f/fully-bound?
                     (binding/formula-apply-all literal bindings))))
    :resolve-fn r-<=}
   {:name :>=
    :test-fn (fn [literal bindings]
               (and (= (f/predicate literal) ">=")
                    (every?
                     f/fully-bound?
                     (binding/formula-apply-all literal bindings))))
    :resolve-fn r->=}
   {:name :<
    :test-fn (fn [literal bindings]
               (and (= (f/predicate literal) "<")
                    (every?
                     f/fully-bound?
                     (binding/formula-apply-all literal bindings))))
    :resolve-fn r-<}
   {:name :>
    :test-fn (fn [literal bindings]
               (and (= (f/predicate literal) ">")
                    (every?
                     f/fully-bound?
                     (binding/formula-apply-all literal bindings))))
    :resolve-fn r->}])

(defn ? [literal bindings]
  (->> resolution-modules
       (some (fn [{:keys [test-fn]}] (test-fn literal bindings)))
       boolean))

(defn get-fns [literal bindings]
  (filter
   (fn [{:keys [test-fn]}] (test-fn literal bindings))
   resolution-modules))
