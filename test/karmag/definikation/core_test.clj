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

(deftest read-string-validation-test
  (testing "missing id"
    (let [[spec errors] (read-string "{:not-id :a}")
          item {:not-id :a}]
      (is (= 1 (count errors)))
      (is (= 1 (count (get errors item))))
      (is (= "Key :id is required" (first (get errors item))))))
  (testing "incorrect id"
    (let [[spec errors] (read-string "{:id :failure}")
          item {:id :failure}]
      (is (= 1 (count errors)))
      (is (= 1 (count (get errors item))))
      (is (= "Key :id must be an #id" (first (get errors item))))))
  (testing "overlapping ids"
    (let [[spec errors] (read-string "{:id #id [:type :id] :key 1} {:id #id [:type :id]}")
          item (let [id (common/make-id :type :id)]
                 {:id id})]
      (is (= 2 (count errors)))
      (is (= 1 (count (get errors item))))
      (is (= "Id overlaps with another item" (first (get errors item))))
      (is (= 1 (->> errors (mapcat val) set count))))))
