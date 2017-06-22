(ns karmag.definikation.spec-test
  (:require [clojure.test :refer :all]
            [karmag.definikation.core :as core]
            [karmag.definikation.common :refer [make-id]]
            [karmag.definikation.spec :as spec]))

(def local-spec
  (let [[spec errors]
        (core/read-string
         "{:id #id [:prop :x]}
          {:id #id [:pointer 1], :point #id [:prop :x]}

          {:id #id [:target :alpha]
           :data {:key :value}}
          {:id #id [:karmag.definikation/change 1]
           :context {:ctx :engage, :part true}
           :for #{#id [:target :alpha]}
           :change {:data {:key :new-value}}}")]
    (assert (empty? errors)
            (str "Errors: " errors))
    spec))

(deftest find-referencing-test
  (let [found (spec/find-referencing local-spec (make-id :prop :x))]
    (is (= 1 (count found)))
    (is (= (make-id :pointer 1) (-> found first :id))))
  (is (empty? (spec/find-referencing local-spec (make-id :pointer 1)))))

(deftest apply-changes-test
  (let [id (make-id :target :alpha)
        original-item {:id id, :data {:key :value}}]
    (testing "no application"
      (is (= (get local-spec id) original-item)))
    (testing "context match"
      (let [spec (spec/apply-changes local-spec {:ctx :engage, :part true})]
        (is (= (get spec id)
               {:id id, :data {:key :new-value}}))))
    (testing "smaller context"
      (let [spec (spec/apply-changes local-spec {:ctx :engage})]
        (is (= (get spec id) original-item))))
    (testing "larger context"
      (let [spec (spec/apply-changes local-spec {:ctx :engage
                                                 :part true
                                                 :more :stuff})]
        (is (= (get spec id)
               {:id id, :data {:key :new-value}}))))))
