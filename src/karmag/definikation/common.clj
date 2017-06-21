(ns karmag.definikation.common)

(defrecord Id [type id])

(defn make-id [type id]
  (Id. type id))

(defn id? [obj]
  (instance? Id obj))
