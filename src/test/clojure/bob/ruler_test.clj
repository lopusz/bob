(ns bob.ruler-test
  (:require [clojure.test :refer :all]
            [bob.data-test :refer [rules]]
            [bob.ruler :refer :all]))

(deftest calc-substrate-res-set-test
  (is (=
    (calc-substrate-res-set [])
    #{ }))
  (is (=
    (calc-substrate-res-set [ { :inp #{ "f7" "f8"} :out #{ "f9" }} ])
    #{ "f7" "f8"}))
  (is (=
    (calc-substrate-res-set rules)
    #{ "f1" "f2"})))

(deftest calc-product-res-set-test
  (is (=
    (calc-product-res-set [])
    #{}))
  (is (=
    (calc-product-res-set [ { :inp #{ "f7" "f8"} :cmd "C" :out #{ "f9" }} ])
    #{ "f9"}))
  (is (=
    (calc-product-res-set rules)
    #{ "f3" "f4" "f5" "f6" "f7" "f8"})))

(deftest calc-tag-set-test [rules]
  (is (=
    (calc-tag-set [])
    #{}))
  (is (=
    (calc-tag-set rules)
    #{ "F3" "F4" "F5-6" "F7" "F8" "level1" "level2" "level3"}))
  (let [
         rules* (map #(dissoc % :tag) rules)
        ]
    (is (=
         (calc-tag-set rules*)
         #{}))))
