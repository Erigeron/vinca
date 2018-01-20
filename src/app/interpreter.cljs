
(ns app.interpreter (:require [cuerdas.core :as str] [verbosely.core :refer [log!]]))

(defn form-add [x y]
  (assert (= :number (:type x)) (str "x should be a number" x))
  (assert (= :number (:type y)) (str "y should be a number: " y))
  {:type :number, :data (+ (:data x) (:data y))})

(def special-forms #{"+" "echo"})

(defn interpret [scope expr *io]
  (assert (or (= :expr (:type expr)) (= :leaf (:type expr))) "only accept code")
  (log! expr)
  (let [base-value {:type :value, :data nil, :scope scope, :io @*io}]
    (merge
     base-value
     (if (= :expr (:type expr))
       (let [method (interpret scope (first (:data expr)) *io)]
         (case (:type method)
           :special-form
             (let [params (map (fn [x] (interpret scope x *io)) (rest (:data expr)))]
               (merge
                {:evaluation {:type :special-form, :data method, :params params}}
                (case (:data method)
                  "+" (apply form-add params)
                  "echo" {:type :value, :data (doseq [p params] (swap! *io conj p))}
                  (throw (js/Error. (str "Unknown special form: " (:data method)))))))
           :function
             (apply (:data method) (map (fn [x] (interpret scope x *io)) (rest (:data expr))))
           (throw (js/Error. (str "Unknown method: " method)))))
       (let [leaf expr, x (:data leaf)]
         (cond
           (str/numeric? x) {:type :number, :data (str/parse-number x)}
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
  (let [initial-ast (map (fn [line] (markup-expr line [])) tree)
        initial-scope {}
        *io (atom [])]
    (log! initial-ast)
    (loop [scope initial-scope, ast initial-ast]
      (if (empty? ast)
        (println @*io)
        (recur (:scope (interpret scope (first ast) *io)) (rest ast))))))
