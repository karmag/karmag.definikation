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
  "Returns a sequence of items. Source is interpreted as by
  clojure.java.io/reader."
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
  "Same as 'read' but treats the string as the data to be read."
  ([string]
   (read-string default-config string))
  ([config string]
   (read config
         (if (string? string)
           (StringReader. string)
           (map #(StringReader. %) string)))))

(defn collect
  "Build a spec from the given items. If any item ids overlap an
  exception will be raised."
  [items]
  (reduce (fn [spec item]
            (if-let [old (get spec (:id item))]
              (throw (ex-info (str "Multiple items with the id " (:id item))
                              {:id (:id item)
                               :one old
                               :two item}))
              (assoc spec (:id item) item)))
          nil
          items))
