(ns scotus.transitivity.class
  (:require
   [clojure.set :as set]
   [scotus.transitivity.closure :as cl]
   [scotus.database.query :as dbq]))

(defn ->formula [{:subclass_of/keys [subclass superclass]}]
  ["subclass_of" subclass superclass])

(defn path->formulas [path]
  (mapv ->formula path))

(defn paths->justification [arg paths]
  (set (map path->formulas paths))
  #_(if (seq paths)

    #{[["subclass_of" arg arg]]}))

(defn superclasses
  [arg &
   {:keys [context justification?]
    :or {context "universal" justification? false}}]
  (let [paths (cl/closure
               [arg] "subclass_of" "subclass" "superclass" :context context)
        result (->> paths
                    (apply into #{{:subclass_of/subclass arg
                                   :subclass_of/superclass arg}})
                    (mapcat
                     (fn [{:subclass_of/keys [subclass superclass]}]
                       [subclass superclass]))
                    set)]
    (if justification?
      (with-meta result {:justification (paths->justification arg paths)})
      result)))


(defn subclasses
  [arg &
   {:keys [context justification?]
    :or {context "universal" justification? false}}]
  (let [paths (cl/closure [arg] "subclass_of" "superclass" "subclass" :context context)
        result (->> paths
                    (apply into #{{:subclass_of/subclass arg
                                   :subclass_of/superclass arg}})
                    (mapcat
                     (fn [{:subclass_of/keys [subclass superclass]}]
                       [subclass superclass]))
                    set)]
    (if justification?
      (with-meta result {:justification (paths->justification arg paths)})
      result)))

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
      (let [all-paths (set (concat (get (meta arg-one-classes) :justification)
                                   (get (meta arg-two-classes) :justification)))
            justification-one (some (fn [path] (slice-justification path disj-class-one)) all-paths)
            justification-two (some (fn [path] (slice-justification path disj-class-two)) all-paths)]
        (with-meta result
          {:justification (set/union justification-one justification-two #{result})}))
      result)))
