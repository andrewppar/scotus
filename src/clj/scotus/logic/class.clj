(ns scotus.logic.class
  (:require
   [clojure.set :as set]
   [scotus.logic.closure :as cl]
   [scotus.database.query :as dbq]))

(defn ^:private ->formula [{:subclass_of/keys [subclass superclass]}]
  ["subclass_of" subclass superclass])

(defn ^:private path->formulas [path]
  (mapv ->formula path))

(defn ^:private paths->justification [paths target]
  (reduce
   (fn [acc path]
     (let [justification (path->formulas path)]
       (reduce
        (fn [acc* n]
          (let [slice (take n justification)
                end (last slice)
                node (case target :subclass (second end) :superclass (nth end 2))]
            (update acc* node (fnil conj #{}) (set slice))))
        acc
        (range 1 (inc (count justification))))))
   {}
   paths))

(defn wrap-justification [result justification? justification-fn]
  (if justification?
    (with-meta result {:justification (justification-fn)})
    result))

;; todo merge this with subclasses
(defn superclasses
  [arg &
   {:keys [context justification?]
    :or {context "universal" justification? false}}]
  (let [reflexive-case {:subclass_of/subclass arg :subclass_of/superclass arg}
        paths (cl/closure
               [arg] "subclass_of" "subclass" "superclass" :context context)
        ;; todo make this into one reduce
        result (->> paths
                    (reduce into #{reflexive-case})
                    (mapcat
                     (fn [{:subclass_of/keys [subclass superclass]}]
                       [subclass superclass]))
                    set)]
    (wrap-justification
     result justification?
     (fn []
       (update
        (paths->justification paths :superclass)
        arg (fnil conj #{}) #{["subclass_of" arg arg]})))))

(defn subclasses
  [arg &
   {:keys [context justification?]
    :or {context "universal" justification? false}}]
  (let [reflexive-case {:subclass_of/subclass arg :subclass_of/superclass arg}
        paths (cl/closure [arg] "subclass_of" "superclass" "subclass" :context context)
        result (->> paths
                    (reduce into #{reflexive-case})
                    (mapcat
                     (fn [{:subclass_of/keys [subclass superclass]}]
                       [subclass superclass]))
                    set)]
    (wrap-justification
     result justification?
     (fn []
       (update
        (paths->justification paths :subclass)
        arg (fnil conj #{}) ["subclass_of" arg arg])))))

(defn subclass?
  "The return for this (like queries) is either #{} or #{#{}} with the
first representing false and the second true. Clojure doesn't allow adding
meta to booleans, and that's how we store justifications."
  [subclass superclass
   & {:keys [context justification?]
      :or {context "universal" justification? false}}]
  (let [fail #{}
        succeed #{fail}]
    (if (= subclass superclass)
      (wrap-justification
       succeed justification?
       (fn [] #{["subclass_of" subclass superclass]}))
      (let [path (cl/path superclass subclass "subclass_of" "superclass" "subclass" :context context)]
        (if (seq path)
          (wrap-justification
           succeed justification?
           (fn [] (set (path->formulas path))))
          (wrap-justification
           fail justification?
           (fn [] #{})))))))

(defn any-disjoint-with-any?
  [one-classes two-classes context]
  (let [specs (->> (for [one one-classes
                         two two-classes]
                     [[two one] [one two]])
                   (apply concat)
                   (into []))]
    ;; TODO: This could be more efficient with a limit 1 query
    ;; we could special case disjoint queries

    (when-let [{:disjoint/keys [class_one class_two]}
             (->> specs
                  (dbq/lookup-rows "disjoint" [context] false)
                  first)]
      ["disjoint" class_one class_two])))

(defn slice-justification [path end]
  (let [{:keys [found? result]} (reduce
                                 (fn [acc item]
                                   (if (= (last item) end)
                                     (reduced
                                      (-> acc
                                          (assoc :found? true)
                                          (update :result conj item)))
                                     (update acc :result conj item)))
                                 {:found? false :result []}
                                 path)]
    (when found? result)))

(defn disjoint?
  [arg-one arg-two &
   {:keys [context justification?]
    :or {context "universal" justification? false}}]
  (let [arg-one-classes (superclasses arg-one :context context :justification? justification?)
        arg-two-classes (superclasses arg-two :context context :justification? justification?)
        result (any-disjoint-with-any? arg-one-classes arg-two-classes context)
        disj-class-one (nth result 1)
        disj-class-two (nth result 2)]
    (if justification?
      ;; todo this seems inefficient now that we are storing the justification
      ;; as a map
      (let [all-paths (reduce
                       into
                       (reduce into #{} (vals (get (meta arg-one-classes) :justification)))
                       (vals (get (meta arg-two-classes) :justification)))
            justification-one (some (fn [path] (slice-justification path disj-class-one)) all-paths)
            justification-two (some (fn [path] (slice-justification path disj-class-two)) all-paths)]
        (with-meta result
          {:justification (set/union justification-one justification-two #{result})}))
      result)))
