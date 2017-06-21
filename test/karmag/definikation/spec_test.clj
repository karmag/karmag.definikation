(ns karmag.definikation.spec-test
  (:require [clojure.test :refer :all]
            [karmag.definikation.core :as core]
            [karmag.definikation.common :refer [make-id]]
            [karmag.definikation.spec :as spec]))

(def local-spec
  (let [[spec errors]
        (core/read-string
         "{:id #id [:prop :x]}
          {:id #id [:pointer 1], :point #id [:prop :x]}")]
    (assert (empty? errors))
    spec))

(deftest find-referenced-by-test
  (let [found (spec/find-referenced-by local-spec (make-id :prop :x))]
    (is (= 1 (count found)))
    (is (= (make-id :pointer 1) (-> found first :id))))
  (is (empty? (spec/find-referenced-by local-spec (make-id :pointer 1)))))
