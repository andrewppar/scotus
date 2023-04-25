(ns scotus.atomic-proof
  (:require
   [clojure.set :as set]
   [scotus.database   :as db]
   [scotus.formula    :as formula]
   [scotus.transitive :as transitive]
   [scotus.state      :as state]))

(defn expand-args
  [predicate args new-values arg-name]
  (let [arg-map (zipmap (state/get-table-args predicate) args)]
    (map
     (fn [new-value]
       (-> arg-map
           (assoc arg-name new-value)
           (update-vals
            (fn [arg] (when-not (formula/variable? arg) arg)))
           vals))
     new-values)))

(defn args->row-spec [formula]
  (mapv
   (fn [arg] (when-not (formula/variable? arg) arg))
   (formula/args formula)))

(defn results->bindings [predicate formula results]
  (let [variable-map (reduce-kv
                      (fn [acc k v]
                        (if (formula/variable? k)
                          (assoc acc (keyword predicate v) k)
                          acc))
                      {}
                      (zipmap (formula/args formula)
                              (state/get-table-args predicate)))]
    (map
     (fn [result]
       (set/map-invert (set/rename-keys variable-map result)))
     results)))

(defn lookup [predicate formulas contexts]
  (let [row-specs (mapv args->row-spec formulas)]
    (db/lookup-rows predicate contexts row-specs)))

(defn variable-arg? [formula arg-name]
  (let [[pred & args] formula]
    (-> pred
        state/get-table-args
        (zipmap args)
        (get arg-name)
        formula/variable?
        boolean)))

(defmulti prove
  {:arglists '([predicate-type formulas base-results properties contexts])}
  (fn [predicate-type _ _ _ _]
    predicate-type))

(defmethod prove :default
  [_ _ base-results _ _]
  base-results)

;;; Transitivity

(defn transitive-predicate-map [predicate contexts]
  (->> [[predicate nil nil nil nil]]
       (db/lookup-rows "transitive_arg" contexts)
       (map
        (fn [result]
          (reduce-kv
           (fn [acc k v] (assoc acc (-> k name keyword) v))
           {}
           result)))))

(defn closure
  [value {:keys [transitive_predicate start_arg transitive_arg]} contexts direction]
  (case direction
    :forward  (transitive/closure
               [value] start_arg transitive_predicate transitive_arg contexts)
    :backward (transitive/closure
              [value] transitive_arg transitive_predicate start_arg contexts)))


(defn forward-transitive-lookup
  [predicate results contexts {:keys [arg] :as transitive-predicate-map}]
  (let [arg-keyword (keyword predicate arg)
        groups      (group-by arg-keyword results)]
    (reduce-kv
     (fn [acc group bindings]
       (let [new-values   (closure
                           group transitive-predicate-map contexts :forward)
             new-bindings (mapcat
                           (fn [binding]
                             (map
                              (fn [new-value]
                                (assoc binding arg-keyword new-value))
                              new-values))
                           bindings)]
         (concat acc new-bindings)))
     results
     groups)))

(defn backward-transitive-lookup
  [predicate formulas contexts {:keys [arg] :as transitive-predicate-map}]
  (->> formulas
       (group-by (partial formula/arg-by-name arg))
       (reduce-kv
        (fn [acc group forms]
          ;; TODO - do all values at once
                ;; memoize `closure` function
          (let [new-args (closure
                          group transitive-predicate-map contexts :backward)]
            (concat acc
                    (mapcat
                     (fn [[_ & args]]
                       (expand-args predicate args new-args arg))
                     formulas))))
        [])
       (db/lookup-rows predicate contexts)))

(defn transitive-lookup
  [predicate formulas results contexts {:keys [arg] :as transitive-map}]
  (if (variable-arg? (first formulas) arg)
    (forward-transitive-lookup predicate results contexts transitive-map)
    (backward-transitive-lookup predicate formulas contexts transitive-map)))

(defmethod prove "transitive-predicate"
  [_ formulas base-results properties contexts]
  (let [[pred & args]   (first formulas)]
    (reduce
     (fn [result transitivity-map]
       (transitive-lookup
        pred formulas result contexts transitivity-map))
     base-results
     (transitive-predicate-map pred contexts))))

;;; Reflexivity

(defmethod prove "reflexive-predicate"
  [_ formulas base-results _ _]
  ;; This is a query-wff thing, not a prove thing.
;;   (when-not (count (formula/args canonical-formula) 2)
;;       (throw
;;        (ex-info
;;         (format "%s is not a reflexive relation since it has more than 2 args" (formula/predicate canonical-formula)))))

  (let [canonical-formula  (first formulas)
        [pred & args]      canonical-formula
        predicate-args (map
                        (partial keyword pred)
                        (state/get-table-args pred))
        variable-positions (->> canonical-formula
                                (map-indexed
                                 (fn [idx arg]
                                   (when (formula/variable? arg)
                                     idx)))
                                (filter (comp not nil?)))]
    (if (= (count variable-positions) 1)
      (let [pos       (first variable-positions)
            other-pos (case pos 1 2 2 1)]
        (concat
         (map
          (fn [formula]
            (let [args             (vec (formula/args formula))
                  reflexive-value  (get formula other-pos)]
              (zipmap predicate-args (assoc args (dec pos) reflexive-value))))
          formulas)
         base-results))
      base-results)))


;;; Atomic Proof

(defn formula-signatures-match? [formulas]
  (loop [signature (formula/signature (first formulas))
         todo      (rest formulas)]
    (if (not (seq todo))
      true
      (let [next-signature (formula/signature (first todo))]
        (if (= signature next-signature)
          (recur next-signature (rest todo))
          false)))))

(defn get-predicate-types [predicate contexts]
  (map
   (fn [row] (get row :instance/class))
   (db/lookup-rows "instance" contexts [[predicate nil]])))

(defn atomic-proof [formulas contexts]
  (when-not (formula-signatures-match? formulas)
    (throw
     (ex-info
      "Cannot generate atomic proof for formulas with different signatures"
      {:caused-by formulas})))
  (let [canonical-formula  (first formulas)
        [predicate & args] canonical-formula
        predicate-types    (get-predicate-types predicate contexts)
        base-results       (lookup predicate formulas contexts)]
    (->> predicate-types
         (map
          (fn [predicate-type]
            (prove predicate-type formulas base-results {} contexts)))
         (apply concat)
         (results->bindings predicate canonical-formula)
         set)))
