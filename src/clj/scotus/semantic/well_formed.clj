(ns scotus.semantic.well-formed
  (:require
   [scotus.database.query :as dbq]
   [scotus.database.utils :as dbu]
   [scotus.state :as state]
   [scotus.syntax.formula :as f]
   [scotus.syntax.xnf :as xnf]))

(defn logical-predicate? [object]
  (contains? #{"equal" "subclass_of" "instance" "<=" ">=" ">" "<"} object))

(defn predicate? [object]
  (or (logical-predicate? object)
      (contains? (set (state/tables)) object)))

(defn well-typed-atom?
  [atomic-formula & {:keys [context] :or {context "universal"}}]
  (let [predicate (f/predicate atomic-formula)
        args (f/args atomic-formula)
        table-args (zipmap (map dbu/to-keyword (state/table-args predicate)) args)

        constraints (dbq/lookup-rows "arg_instance" ["universal"] false [[predicate nil nil]])
        to-lookup (reduce
                   (fn [row-specs {:arg_instance/keys [argument class]}]
                     (let [formula-arg (get table-args (keyword argument))]
                       (conj row-specs [formula-arg class])))
                   []
                   constraints)]
    (= (count (dbq/lookup-rows "instance" [context] false to-lookup))
       (count to-lookup))))

(defn well-typed-literal? [formula & {:keys [context] :or {context "universal"}}]
  (cond (f/atom? formula)
        (well-typed-atom? formula context)

        (f/negation? formula)
        (well-typed-atom? (f/negatum formula) context)

        :else
        nil))

(defn typing-error [type-context literal formula]
  {:error :non-predicate
   :context type-context
   :caused-by {:predicate (f/literal-predicate literal)
               :literal literal
               :formula formula}})

(defn assert-well-typed?
  [formula & {:keys [context] :or {context "universal"}}]
  (if (or (xnf/cnf? formula)
          (xnf/dnf? formula))
    (let [literals (mapcat (fn [subformula] (f/args subformula)) (f/args formula))]
      (every? (fn [literal] (well-typed-literal? literal context)) literals))
    (assert-well-typed? (xnf/cnf formula))))

(defn query-type-errors [formula & collect-all?]
  (if (or (xnf/cnf? formula)
          (xnf/dnf? formula))
    (reduce
     (fn [acc literal]
       (let [pred (f/literal-predicate literal)]
         (if (or (predicate? pred) (f/variable? pred))
           acc
           (let [error (typing-error :query literal formula)]
             (if collect-all?
               (conj acc error)
               (reduced error))))))
     []
     (xnf/literals formula))
    (query-type-errors (xnf/dnf formula))))


(defn mistyped? [formula context])

(comment
  (require '[scotus.database.add :as dba])
  (require '[scotus.database.remove :as dbr])


  (dba/add-rows-by-table "instance" "universal" "anparisi" false [["dog" "class"]])
  (dbr/delete-rows "instance" "universal" false ["dog" "class"]))
