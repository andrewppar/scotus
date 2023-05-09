(ns scotus.atomic-proof
  (:require
   [clojure.set               :as set]
   [clojure.walk              :as walk]
   [scotus.database           :as db]
   [scotus.formula.formula    :as formula]
   [scotus.transitive         :as transitive]
   [scotus.state              :as state]))

(defn expand-args
  [predicate args new-values arg-name]
  (let [arg-map (zipmap (state/table-args predicate) args)]
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

(defn lookup [predicate formulas contexts negated?]
  (let [row-specs (mapv args->row-spec formulas)]
    (db/lookup-rows predicate contexts negated? row-specs)))

(defn variable-arg? [formula arg-name]
  (let [[pred & args] formula]
    (-> pred
        state/table-args
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
       (db/lookup-rows "transitive_arg" contexts false)
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
       (db/lookup-rows predicate contexts false)))

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
                        (state/table-args pred))
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
   (db/lookup-rows "instance" contexts false [[predicate nil]])))

(defn substitute-bindings
  [formula  bindings]
  (if (seq bindings)
    (distinct
     (map
      (fn [binding]
        (walk/postwalk (fn [item] (get binding item item)) formula))
      bindings))
    [formula]))

(defmulti prove-formulas
  {:arglists '([formulas predicate contexts])}
  (fn [_ predicate _]
    predicate))

(defmethod prove-formulas :default
  [formulas predicate contexts]
  (let [predicate-types    (get-predicate-types predicate contexts)
        base-results       (lookup predicate formulas contexts false)]
    (->> predicate-types
         (map ;;pmap
          (fn [predicate-type]
            (prove predicate-type formulas base-results {} contexts)))
         (apply concat))))

(defmethod prove-formulas "asserted"
  [formulas _ contexts]
  (let [predicate (-> formulas first (formula/arg 1) formula/predicate)
        new-formulas (map (fn [formula] (formula/arg formula 1)) formulas)]
    (lookup predicate new-formulas contexts false)))

(defmethod prove-formulas "unknown"
  [formulas _ contexts]
  (let [asserted-formulas (prove-formulas
                           (map (fn [form]
                                  ["asserted" (formula/arg form 1)])
                                formulas)
                           "asserted"
                           contexts)
        ]
    ;; Create a map of named args to bindings for both formulas
    ;; and asserted formulas, take the difference, and treat those
    ;; as a result -- this implements [unknown [exists bindings PHI]]
    ;; but we treat variables as implicitly universally quantified so
    ;; I guess it's ok...
    ))


(defn results->bindings
  [results predicate formula]
  (let [variable-map (->> (state/table-args predicate)
                          (zipmap (formula/args formula))
                          (reduce-kv
                           (fn [acc k v]
                             (if (formula/variable? k)
                               (assoc acc (keyword predicate v) k)
                               acc))
                           {}))]
    (map (fn [result]
           (select-keys
            (set/rename-keys result variable-map)
            (vals variable-map)))
         results)))

(defn join-bindings
  [new-bindings old-bindings]
  (if (seq old-bindings)
    (let [common-bindings (set/intersection
                           (set (keys (first new-bindings)))
                           (set (keys (first old-bindings))))]
      (mapcat
       (fn [old-binding]
         (let [old-sub (select-keys old-binding common-bindings)]
           (->> new-bindings
                (filter
                 (fn [new-binding]
                   (let [new-sub (select-keys new-binding common-bindings)]
                     (= new-sub old-sub))))
                (map (partial merge old-binding)))))
       old-bindings))
    new-bindings))

(defn prove-atom [formula bindings contexts]
  (let [original-predicate (formula/predicate formula)
        subpredicate (case original-predicate
                       "asserted" (-> formula
                                     (formula/arg 1)
                                     formula/predicate)
                       original-predicate)
        proof-formula (case original-predicate
                        "asserted" (formula/arg formula 1)
                        formula)]
    (-> formula
       (substitute-bindings bindings)
       (prove-formulas original-predicate contexts)
       (results->bindings subpredicate proof-formula)
       (join-bindings bindings)
       set)))

(defn prove-negation [formula bindings contexts]
  (let [negatum      (formula/negatum formula)
        new-formulas (substitute-bindings negatum bindings)
        predicate (formula/predicate negatum)]
    (-> predicate
        (lookup new-formulas contexts true)
        (results->bindings predicate negatum)
        (join-bindings bindings))))

(defn proof [formula bindings contexts]
  (if (formula/negation? formula)
    (prove-negation formula bindings contexts)
    (prove-atom formula bindings contexts)))
