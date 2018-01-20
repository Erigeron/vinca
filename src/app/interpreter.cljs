
(ns app.interpreter (:require [cuerdas.core :as str] [verbosely.core :refer [log!]]))

(def special-forms {})

(defn interpret [scope expr *io]
  (log! expr)
  (let [base-value {:type :value, :value nil, :scope scope, :expr expr, :io @*io}]
    (merge
     base-value
     (if (= :expr (:type expr))
       (let [method (interpret scope (first (:data expr)) *io)]
         (case (:type method)
           :special-form
             (apply (:data method) (map (fn [x] (interpret scope *io)) (rest expr)))
           :function (apply (:data method) (map (fn [x] (interpret scope *io)) (rest expr)))
           (throw (js/Error. (str "Unknown method: " str)))))
       (let [leaf expr, x (:data leaf)]
         (cond
           (str/numeric? x) {:type :number, :str/parse-number x}
           (contains? special-forms x) {:type :special-form, :data (get special-forms x)}
           (contains? scope x) {:type :function, (:data (get scope x)) nil}
           :else (throw (js/Error. (str "Unknown value: " x)))))))))

(defn markup-expr [tree coord]
  (if (vector? tree)
    {:type :expr,
     :coord coord,
     :data (->> tree (map-indexed (fn [idx child] (markup-expr child (conj coord idx)))))}
    {:type :leaf, :coord coord, :data tree}))

(defn load-program [tree]
  (let [initial-ast (map (fn [line] (markup-expr tree [])) tree)
        initial-scope {}
        *io (atom [])]
    (log! initial-ast)
    (loop [scope initial-scope, ast initial-ast]
      (if (empty? ast) (println @*io) (recur (interpret scope (first ast) *io) (rest ast))))))
