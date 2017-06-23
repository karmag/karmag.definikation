(ns karmag.definikation.validation
  (:require [karmag.definikation.common :as common]))

;; item validation

(defn check-id [spec item]
  (cond
    (not (find item :id))         ["Key :id is required"]
    (not (common/id? (:id item))) ["Key :id must be an #id"]
    :else nil))

(defn check-change [spec item]
  (when (common/type? item :karmag.definikation/change)
    (cond
      (not (set? (:for item))) ["Key :for must be a set"]
      (not (:change item))     ["Key :change is required"]
      :else nil)))

(defn check-references [spec item]
  (->> (common/find-ids (dissoc item :id))
       (map (fn [item-id]
              (when-not (get spec item-id)
                (str "Referenced item does not exist: " item-id))))
       (remove nil?)))

;; validation

(defn validate [spec item-checkers]
  (reduce (fn [errors [f item]]
            (let [err (f spec item)]
              (if (empty? err)
                errors
                (update-in errors [(:id item)] concat err))))
          nil
          (for [f item-checkers, item (vals spec)] [f item])))

(defn validate-all [spec]
  (validate spec [check-id check-change check-references]))
