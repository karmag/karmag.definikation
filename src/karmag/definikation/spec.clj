(ns karmag.definikation.spec
  (:require [clojure.walk :refer [postwalk]]
            [karmag.definikation.common :as common]))

(defn find-referencing
  "Returns a sequence of items that reference the given item or
  id. Does not include the actual item."
  [spec item-or-id]
  (let [id (if (common/id? item-or-id)
             item-or-id
             (:id item-or-id))]
    (->> (vals spec)
         (remove #(= id (:id %)))
         (filter (fn [item]
                   ((common/find-ids item) id))))))

(defn apply-changes
  "Locates the changes in the spec that are part of the given context
  and applies them. Returns the spec with updated items."
  [spec context]
  (let [changes (->> (vals spec)
                     (filter #(common/type? % :karmag.definikation/change))
                     (filter #(common/sub-context? context (:context %))))]
    (reduce (fn [spec change]
              (let [change-data (:change change)]
                (reduce (fn [spec id]
                          (update-in spec [id] common/merge-rec change-data))
                        spec
                        (:for change))))
            spec
            changes)))

(defn get-at
  "Traverses the item in the same manner as get-in but when an id is
  encountered it's replaced by the corresponding item."
  [spec item-or-id path]
  (reduce (fn [data key]
            (if (common/id? data)
              (get (get spec data) key)
              (get data key)))
          (get spec (if (common/id? item-or-id)
                      item-or-id
                      (:id item-or-id)))
          path))

(defn expand-item
  "Expand any ids contained in the given item. Recursively apply the
  expansion to any ids expanded. The id of any item is not
  affected. If there are recursive structures the expansion will
  result in a stack overflow."
  [spec item-or-id]
  (let [item (if (common/id? item-or-id)
               (get spec item-or-id)
               item-or-id)
        id (:id item)]
    (assoc (postwalk (fn [obj]
                       (if-let [replacement (get spec obj)]
                         (expand-item spec replacement)
                         obj))
                     (dissoc item :id))
           :id id)))
