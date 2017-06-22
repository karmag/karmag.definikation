(ns karmag.definikation.common
  (:require [clojure.set]
            [clojure.walk :refer [walk]]))

(defrecord Id [type id])

(defn make-id [type id]
  (Id. type id))

(defn id? [obj]
  (instance? Id obj))

(defn type? [obj type]
  (if (id? obj)
    (= type (:type obj))
    (= type (:type (:id obj)))))

(defn find-ids [data]
  "Traverses the given data and returns a set of ids found."
  (cond
    (id? data) #{data}
    (map? data) (->> (mapcat identity data)
                     (map find-ids)
                     (reduce clojure.set/union #{}))
    (coll? data) (walk find-ids
                       #(reduce clojure.set/union #{} %)
                       data)
    :else #{}))

(defn sub-context?
  "Returns true if the given context is a sub-context of the
  given base."
  [base context]
  (every? (fn [[k v]]
            (= v (get base k)))
          context))

(defn merge-rec
  "Recursively merge the given additional-data into the data. nil
  values in additional-data indicates that the corresponding value
  should be removed in data.

  data additional-data -> action
  map? map? -> merge, nil values are removed
  set? set? -> union
  set? map? -> merge, map keys are added, nil values removes
  vector? map? -> merge, keys must be ints, resize vector if needed
  _ x -> x"
  [data additional-data]
  (cond

    (map? data)
    (cond
      (map? additional-data) (reduce (fn [data [k v]]
                                       (cond
                                         (map? v) (update data k merge-rec v)
                                         (nil? v) (dissoc data k)
                                         :else    (assoc data k v)))
                                     data
                                     additional-data)
      :else additional-data)

    (set? data)
    (cond
      (set? additional-data) (clojure.set/union data additional-data)
      (map? additional-data) (reduce (fn [data [k v]]
                                       (if (nil? v)
                                         (disj data k)
                                         (conj data k)))
                                     data
                                     additional-data)
      :else additional-data)

    (vector? data)
    (cond
      (map? additional-data) (reduce (fn [data [k v]]
                                       (let [data (if (< (count data) k)
                                                    (->> (concat data (repeat nil))
                                                         (take k)
                                                         vec)
                                                    data)]
                                         (update data k merge-rec v)))
                                     data
                                     additional-data)
      :else additional-data)

    :else additional-data))
