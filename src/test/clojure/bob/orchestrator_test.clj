(ns bob.orchestrator-test
  (:require [clojure.test :refer :all]
            [me.raynes.fs :refer [touch delete]]
            [bob.data-test :refer [rules]]
            [bob.ruler :refer [calc-substrate-res-set]]
            [bob.executor :refer [run-rule-dummy]]
            [bob.orchestrator :refer :all]))

(deftest find-inp-for-given-single-out-res-test
  (let [
         find-inp-for-given-single-out-res
           @#'bob.orchestrator/find-inp-for-given-single-out-res
        ]
    (is (=
          (find-inp-for-given-single-out-res rules "f1")
          #{ "f1" }))
    (is (=
          (find-inp-for-given-single-out-res rules "dummy")
          #{ "dummy" }))
    (is (=
          (find-inp-for-given-single-out-res rules "f3")
          #{"f1" "f2"}))
    (is (=
          (find-inp-for-given-single-out-res rules "f5")
          #{"f3"}))
    (is (=
          (find-inp-for-given-single-out-res [] "f1")
          #{"f1"}))))

(deftest find-inp-for-given-out-res-test
  (let [
         find-inp-for-given-out-res
           @#'bob.orchestrator/find-inp-for-given-out-res
        ]
    (is (=
          (find-inp-for-given-out-res rules #{ })
          #{ }))
    (is (=
          (find-inp-for-given-out-res rules #{ "f3" })
          #{ "f1" "f2"}))
    (is (=
          (find-inp-for-given-out-res rules #{ "f3" "f8"})
          #{ "f1" "f2" "f4" "f6"}))))

(deftest split-substrate-res-test
  (let [
         split-substrate-res
           @#'bob.orchestrator/split-substrate-res
         all-substrate-res (calc-substrate-res-set rules)
        ]
    (is (=
          (split-substrate-res #{ "f1" "f3"} all-substrate-res)
          [ #{ "f1" } #{ "f3" } ]))
    (is (=
          (split-substrate-res #{ "f1" "f3" "f2" "f8" } all-substrate-res)
          [ #{ "f1" "f2" } #{ "f3" "f8"} ]))))

(deftest filter-only-necessary-rules-test
  (is (=
        (filter-only-necessary-rules rules #{ "f8" })
        [ #{ { :inp #{ "f1" "f2"} :out #{"f3"} :cmd "C1" :cpu 1 :tag #{"F3" "level1"}}
             { :inp #{"f2"} :out #{"f4"} :cmd "C2" :cpu 1 :tag #{ "F4" "level2"}}
             { :inp #{"f3"} :out #{ "f5" "f6" } :cmd "C3" :cpu 1  :tag #{ "F5-6" "level2"}}
             { :inp #{"f6" "f4"} :out #{"f8"} :cmd "C4" :cpu 1 :tag #{ "F8" "level3"} }}
          #{ "f1" "f2" } ])))

(deftest build-test
  (let [
         _ (touch "ala.txt")
         res
           (build
             [ { :inp #{"ala.txt"}, :out #{"tmp.txt"},
                 :cmd "ls -1 > tmp.txt", :cpu 1 }]
             #{"tmp.txt"} { :max-cpu 4 :run-rule-f run-rule-dummy })
         _ (delete "ala.txt")
        ]
  (is (=
    res
    0))))
