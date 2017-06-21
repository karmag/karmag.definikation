(ns karmag.definikation.io
  (:refer-clojure :exclude [read read-string])
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [karmag.definikation.common :as common])
  (:import java.io.StringReader))

(defn id-reader [value]
  (assert (and (vector? value) (= 2 (count value)))
          (str "Id must be a vector of two values; type and id. " value))
  (common/make-id (first value) (second value)))

(def default-config {:readers {'id id-reader}})

(defn read
  ([source]
   (load default-config source))
  ([config source]
   (mapcat (fn [source]
             (with-open [reader (io/reader source :encoding "UTF-8")
                         pbr (java.io.PushbackReader. reader)]
               (let [opts (merge config {:eof ::eof})]
                 (->> #(edn/read opts pbr)
                      repeatedly
                      (take-while #(not= ::eof %))
                      doall))))
           (if (coll? source)
             source
             [source]))))

(defn read-string
  ([string]
   (read-string default-config string))
  ([config string]
   (read config
         (if (string? string)
           (StringReader. string)
           (map #(StringReader. %) string)))))

(defn- validate-id [item]
  (cond
    (not (find item :id)) "Key :id is required"
    (not (common/id? (:id item))) "Key :id must be an #id"
    :else nil))

(defn validate
  "Returns a map of {item [error]}."
  [items]
  (let [errors (reduce (fn [errors item]
                         (if-let [error (validate-id item)]
                           (update-in errors [item] conj error)
                           errors))
                       nil
                       items)
        id-overlap (->> items
                        (reduce (fn [m item]
                                  (update-in m [(:id item)] conj item))
                                nil)
                        (remove (comp #(= % 1) count val)))]
    (->> id-overlap
         (reduce (fn [errors [id items]]
                   (reduce (fn [errors item]
                             (update-in errors [item]
                                        conj "Id overlaps with another item"))
                           errors
                           items))
                 errors)
         (reduce (fn [errors [k v]]
                   (assoc errors k (set v)))
                 nil))))

(defn collect [items]
  (reduce #(assoc %1 (:id %2) %2) nil items))

(defn quick-read
  ([source]
   (quick-read default-config source))
  ([config source]
   (let [items (read config source)
         errors (validate items)]
     (if (empty? errors)
       (collect items)
       (throw (ex-info (str "Errors in spec: " errors)
                       {:errors errors}))))))

(defn quick-read-string
  ([string]
   (quick-read-string default-config string))
  ([config string]
   (read-string config string)))
