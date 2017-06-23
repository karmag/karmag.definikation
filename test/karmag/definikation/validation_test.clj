(ns karmag.definikation.validation-test
  (:require [clojure.test :refer :all]
            [karmag.definikation.core :as df]
            [karmag.definikation.common :as common]
            [karmag.definikation.validation :refer [validate-all]]))

(defn- get-errors [x & xs]
  (let [[spec errors] (df/read-string (cons x xs))]
    (assert (not-empty errors)
            (str "Expected errors to be non empty -- " spec))
    errors))

(deftest general-item-validation-test
  (testing "missing id"
    (let [errors (get-errors "{:not-id :a}")]
      (is (= 1 (count errors)))
      (is (= 1 (count (get errors nil))))
      (is (= "Key :id is required" (first (get errors nil))))))
  (testing "incorrect id"
    (let [errors (get-errors "{:id :failure}")]
      (is (= 1 (count errors)))
      (is (= 1 (count (get errors :failure))))
      (is (= "Key :id must be an #id" (first (get errors :failure))))))
  (testing "reference non-existing item"
    (let [errors (get-errors "{:id #id [:a :b], :pointer #id [:x :y]}")
          id (first (keys errors))]
      (is (= 1 (count errors)))
      (is (= 1 (count (get errors id))))
      (is (= "Referenced item does not exist: [:x :y]"
             (first (get errors id)))))))

(deftest change-item-validation-test
  (testing "missing :for"
    (let [errors (get-errors "{:id #id [:karmag.definikation/change 1]
                               :change {}}")
          id (first (keys errors))]
      (is (= 1 (count errors)))
      (is (= 1 (count (get errors id))))
      (is (= "Key :for must be a set" (first (get errors id))))))
  (testing "missing :change"
    (let [errors (get-errors "{:id #id [:karmag.definikation/change 1]
                               :for #{}}")
          id (first (keys errors))]
      (is (= 1 (count errors)))
      (is (= 1 (count (get errors id))))
      (is (= "Key :change is required" (first (get errors id)))))))
