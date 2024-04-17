(ns scotus.transitivity.closure
  (:require [scotus.database.query :as dbq]
            [scotus.state :as state]))

(defn ->row-specs [args from-pred-arg table-args]
  (let [replace-arg (fn [arg table-arg] (when (= table-arg from-pred-arg) arg))
        ->row-spec  (fn [arg] (map (partial replace-arg arg) table-args))]
    (map ->row-spec args)))

(defn one-step [args predicate from-pred-arg contexts]
  (->> predicate
       state/table-args
       (->row-specs args from-pred-arg)
       (dbq/lookup-rows predicate contexts false)
       (reduce (fn [acc assertion] (update acc
                                          (get assertion (keyword predicate from-pred-arg))
                                          (fnil conj #{})
                                          assertion))
               {})))

(defn ^:private closure-loop
  [predicate start-args from-pred-arg to-pred-arg contexts]
  (let [to-key (keyword predicate to-pred-arg)
        init (->> (one-step start-args predicate from-pred-arg contexts)
                  vals
                  (reduce
                   (fn [acc asserts]
                     (into acc (map (partial conj []) asserts)))
                   #{}))]
  (loop [paths init
           seen #{}]
      (let [next-nodes (reduce
                        (fn [acc path]
                          (let [node (last path)]
                            (if (contains? seen node)
                              acc
                              (conj acc node))))
                        #{}
                        paths)]
        (if (seq next-nodes)
          (let [next-step (one-step
                           (map (fn [row] (get row to-key)) next-nodes)
                           predicate from-pred-arg contexts)]
            (if (seq next-step)
              (recur
               (set (reduce
                     (fn [acc path]
                       (if-let [new-ends (get next-step (get (last path) to-key))]
                         (into acc (mapv (partial conj path) new-ends))
                         (conj acc path)))
                     #{}
                     paths))
               (into seen next-nodes))
              paths))
          paths)))))

(defn resolve-contexts [context]
  (conj
   (reduce
    (fn [result path]
      (into result
            (map
             (fn [{:subcontext_of/keys [subcontext]}] subcontext)
             path)))
    #{}
    (closure-loop
     "subcontext_of" [context] "supercontext" "subcontext" ["universal"]))
   context
   "universal"))

(defn closure
  "Build a set of paths whose roots are `start-args` that represent
  the transitive closure of `predicate` travelling from `from-pred-arg`
  to `to-pred-arg`"
  [start-args predicate from-pred-arg to-pred-arg
   & {:keys [context] :or {context "universal"}}]
  (closure-loop predicate start-args from-pred-arg to-pred-arg (resolve-contexts context)))

(defn slice-internal
  [closure start-key start-arg end-key end-arg no-start?]
  (loop [spec (first closure)
         todo (rest closure)
         started? no-start?
         finished? false
         result []]
    (cond (and started? (= (get spec end-key) end-arg))
          {:finished? true :result (conj result spec)}

          (and (nil? spec) (not (seq todo)))
          {:finished? false :result nil}

          (and (not started?) (= (get spec start-key) start-arg))
          (recur (first todo) (rest todo) true false (conj result spec))

          :else
          (recur (first todo) (rest todo) started? finished? (conj result spec)))))

(defn slice
  ([closure end-key end-arg]
   (get (slice-internal closure nil nil end-key end-arg true)
        :result))
  ([closure start-key start-arg end-key end-arg]
   (get (slice-internal closure start-key start-arg end-key end-arg false)
        :result)))


(defn map-arg [arg-name closure]
  (let [pred (-> closure first keys first namespace)
        arg-key (keyword pred arg-name)]
    (mapv (fn [spec] (get spec arg-key)) closure)))
