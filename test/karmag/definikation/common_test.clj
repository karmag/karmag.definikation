(ns karmag.definikation.common-test
  (:require [clojure.test :refer :all]
            [karmag.definikation.common :refer
             [make-id find-ids merge-rec]]))

(deftest find-ids-test
  (is (= #{}
         (find-ids 1)))
  (is (= #{(make-id :attribute :a)}
         (find-ids (make-id :attribute :a))))
  (is (= #{(make-id :attribute :a) (make-id :attribute :b)}
         (find-ids {:key (make-id :attribute :a)
                    (make-id :attribute :b) :something})))
  (is (= #{(make-id :attribute :a) (make-id :attribute :b)}
         (find-ids [[[(make-id :attribute :a)] [:not-attr :x]]
                    (make-id :attribute :b)]))))

(deftest merge-rec-test
  (are [a b result] (= result (merge-rec a b))
    {:a 1}                {:b 2}        {:a 1 :b 2}
    {:a {:b 100}}         {:a {:b 200}} {:a {:b 200}}
    {:a 1}                {:a nil}      {}
    {:a {:b 100, :c 200}} {:a {:b nil}} {:a {:c 200}}))
