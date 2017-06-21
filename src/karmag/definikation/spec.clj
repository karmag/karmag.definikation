(ns karmag.definikation.spec
  (:require [karmag.definikation.common :as common]))

(defn find-referenced-by
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
