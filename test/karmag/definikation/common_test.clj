(ns karmag.definikation.common-test
  (:require [clojure.test :refer :all]
            [karmag.definikation.common :refer
             [make-id find-ids sub-context? merge-rec]]))

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

(deftest sub-context?-test
  (is (sub-context? {:a 1} {}))
  (is (sub-context? {:a 1} {:a 1}))
  (is (sub-context? {:a 1, :b 2} {:a 1}))
  (is (not (sub-context? {} {:a 1})))
  (is (not (sub-context? {:b 2} {:a 1})))
  (is (not (sub-context? {:a 1} {:a 2}))))

(deftest merge-rec-test
  (are [a b result] (= result (merge-rec a b))
    ;; map data
    {:a 1}                {:b 2}        {:a 1 :b 2}
    {:a {:b 100}}         {:a {:b 200}} {:a {:b 200}}
    {:a 1}                {:a nil}      {}
    {:a {:b 100, :c 200}} {:a {:b nil}} {:a {:c 200}}
    {:a 1}                :value        :value
    ;; set data
    #{:a :b} #{:c}            #{:a :b :c}
    #{:a :b} {:b nil}         #{:a}
    #{:a :b} {:b false}       #{:a :b}
    #{:a}    {:b true :c :go} #{:a :b :c}
    #{:a}    {:a nil}         #{}
    #{:a}    :value           :value
    ;; vector data
    [:a :b] {0 :c}   [:c :b]
    [:a]    {3 :c}   [:a nil nil :c]
    [:a :b] [:c]     [:c]
    [:a :b] :value   :value
    ;; combined
    {:key #{:a}}       {:key {:a nil :b :add}}         {:key #{:b}}
    [{:key [1]}]       {0 {:key {0 101}}}              [{:key [101]}]
    [{:key [1 #{:x}]}] {0 {:key {1 {:x nil :a true}}}} [{:key [1 #{:a}]}]
    ))
