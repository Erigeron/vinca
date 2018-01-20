
(ns app.interpreter
  (:require [cuerdas.core :as str] [verbosely.core :refer [log! verbosely!]]))

(defn form-add [x y]
  (assert (= :number (:type x)) (str "x should be a number" x))
  (assert (= :number (:type y)) (str "y should be a number: " y))
  {:type :number, :data (+ (:data x) (:data y))})

(def special-forms #{"+" "echo" "def" "defn"})

(defn interpret [scope expr *io]
  (assert (or (= :expr (:type expr)) (= :leaf (:type expr))) "only accept code")
  (let [base-value {:type :value, :data nil, :scope scope}]
    (merge
     base-value
     (if (= :expr (:type expr))
       (let [method (interpret scope (first (:data expr)) *io)]
         (case (:type method)
           :special-form
             (case (:data method)
               "+"
                 (let [params (map (fn [x] (interpret scope x *io)) (rest (:data expr)))]
                   (merge
                    {:evaluation {:type :special-form,
                                  :data method,
                                  :params params,
                                  :scope scope}}
                    (apply form-add params)))
               "echo"
                 (do
                  (assert (= 2 (count (:data expr))) "echo only handles 1 parameter")
                  {:type :value,
                   :data (swap! *io conj (interpret scope (last (:data expr)) *io))})
               "def"
                 {:type :value,
                  :data nil,
                  :scope (assoc
                          scope
                          (:data (nth (:data expr) 1))
                          (update
                           (interpret scope (last (:data expr)) *io)
                           :evaluation
                           (fn [x]
                             {:special-form "def",
                              :variable (nth (:data expr) 1),
                              :data expr,
                              :value x})))}
               "defn"
                 {:type :value,
                  :data nil,
                  :scope (assoc
                          scope
                          (:data (nth (:data expr) 1))
                          (do
                           (assert (= :expr (:type (nth (:data expr) 2))))
                           (log! (:data (nth (:data expr) 2)))
                           (let [arg-names (->> (:data (nth (:data expr) 2)) (map :data))
                                 f-body (nth (:data expr) 3)]
                             (assert
                              (every? string? arg-names)
                              "argument name should be string")
                             {:type :function,
                              :data (fn [& passed-in-args]
                                (let [f-scope (merge scope (zipmap arg-names passed-in-args))]
                                  (interpret f-scope f-body *io))),
                              :evaluation {:special-form "defn"}})))}
               (throw (js/Error. (str "Unknown special form: " (:data method)))))
           :function
             (apply (:data method) (map (fn [x] (interpret scope x *io)) (rest (:data expr))))
           (throw (js/Error. (str "Unknown method: " method)))))
       (let [leaf expr, x (:data leaf)]
         (cond
           (str/numeric? x) {:type :number, :data (str/parse-number x), :expr leaf}
           (contains? special-forms x) {:type :special-form, :data (get special-forms x)}
           (contains? scope x) (get scope x)
           :else (throw (js/Error. (str "Unknown value: " x)))))))))

(defn markup-expr [tree coord]
  (if (vector? tree)
    {:type :expr,
     :coord coord,
     :data (->> tree (map-indexed (fn [idx child] (markup-expr child (conj coord idx)))))}
    {:type :leaf, :coord coord, :data tree}))

(defn load-program [tree]
  (let [initial-ast (map-indexed (fn [idx line] (markup-expr line [idx])) tree)
        initial-scope {}
        *io (atom [])]
    (loop [scope initial-scope, ast initial-ast]
      (if (empty? ast)
        (println @*io)
        (recur (:scope (interpret scope (first ast) *io)) (rest ast))))))
