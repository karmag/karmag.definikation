(ns karmag.definikation.core
  (:refer-clojure :exclude [read read-string])
  (:require [karmag.definikation.io :as io])
  (:import java.io.StringReader))

(def default-read-config io/default-config)

(defn string-reader [str]
  (StringReader. str))

(defn read
  "Read the source and returns [spec errors]. If source is a
  collection it's treated as a sequence of sources."
  ([source]
   (read default-read-config source))
  ([config source]
   (let [items (io/read config source)
         errors (io/validate items)]
     [(io/collect items) errors])))

(defn read-string
  "Like 'read' but expects sources to be strings."
  ([string]
   (read-string default-read-config string))
  ([config string]
   (read config
         (if (string? string)
           (string-reader string)
           (map string-reader string)))))
