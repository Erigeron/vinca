
(ns client.types )

(defrecord Token [file coord data])
(defrecord Expression [file coord data])
(defrecord UniType [type value source])

(defn new-token [x] (->Token (:file x) (:coord x) (:data x)))
(defn new-expression [x] (->Expression (:file x) (:coord x) (:data x)))
(defn new-uni-type [x] (->UniType (:type x) (:value x) (:source x)))

(defn token? [x] (instance? Token x))
(defn expression? [x] (instance? Expression x))
(defn uni-type? [x] (instance? UniType x))
