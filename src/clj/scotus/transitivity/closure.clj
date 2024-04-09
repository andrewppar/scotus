(ns scotus.transitivity.closure
  (:require [scotus.database.query :as dbq]
            [scotus.state :as state]))

(defn ->row-specs [args from-pred-arg table-args]
  (let [replace-arg (fn [arg table-arg] (when (= table-arg from-pred-arg) arg))
        ->row-spec  (fn [arg] (map (partial replace-arg arg) table-args))]
    (map ->row-spec args)))

(defn one-step [args predicate from-pred-arg to-pred-arg context]
  (->> predicate
       state/table-args
       (->row-specs args from-pred-arg)
       (dbq/lookup-rows predicate [context] false)
       (reduce (fn [acc assertion] (update acc
                                          (get assertion (keyword predicate from-pred-arg))
                                          (fnil conj #{})
                                          assertion))
               {})))

(defn closure
  "Build a set of paths whose roots are `start-args` that represent
  the transitive closure of `predicate` travelling from `from-pred-arg`
  to `to-pred-arg`"
  [start-args predicate from-pred-arg to-pred-arg
   & {:keys [context] :or {context "universal"}}]
  (let [to-keyword (keyword predicate to-pred-arg)]
    (loop [paths  (mapcat (fn [asserts] (map (partial conj []) asserts))
                          (vals
                           (one-step start-args predicate from-pred-arg to-pred-arg context)))
           seen #{}]
      (let [next-nodes (->> paths
                            (map last)
                            (filter (complement (partial contains? seen)))
                            set)]
        (if (seq next-nodes)
          (let [steps (one-step
                       (map (fn [row] (get row to-keyword)) next-nodes)
                       predicate from-pred-arg to-pred-arg context)]
            (if (seq steps)
              (let [new-paths (set
                               (reduce (fn [acc path]
                                         (if-let [new-ends (get steps (get (last path) to-keyword))]
                                           (into acc (mapv (partial conj path) new-ends))
                                           (conj acc path)))
                                       #{}
                                       paths))]
                (recur new-paths
                       (into seen next-nodes)))
              paths))
          paths)))))

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
