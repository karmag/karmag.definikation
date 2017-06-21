(ns karmag.definikation.common
  (:require [clojure.set]
            [clojure.walk :refer [walk]]))

(defrecord Id [type id])

(defn make-id [type id]
  (Id. type id))

(defn id? [obj]
  (instance? Id obj))

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

(defn merge-rec
  "Recursively merge the given map of additional-data into the data
  map. nil values in additional-data indicates that the corresponding
  value should be removed in the data map."
  [data additional-data]
  (reduce (fn [data [k v]]
            (cond
              (map? v) (update-in data [k] merge-rec v)
              (nil? v) (dissoc data k)
              :else    (assoc data k v)))
          data
          additional-data))
