(ns karmag.definikation.core-test
  (:refer-clojure :exclude [read read-string])
  (:require [clojure.test :refer :all]
            [karmag.definikation.core :refer :all]
            [karmag.definikation.common :as common]))

(deftest read-string-test
  (let [[spec errors] (read-string "{:id #id [:type :id]}")]
    (is (= spec (let [id (common/make-id :type :id)]
                  {id {:id id}})))
    (is (empty? errors))))

(deftest multiple-of-same-id-test
  (is (thrown? clojure.lang.ExceptionInfo
               (read-string "{:id #id [:a :b]} {:id #id [:a :b]}"))))
