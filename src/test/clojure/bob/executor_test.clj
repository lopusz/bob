(ns bob.executor-test
  (:require [clojure.test :refer :all]
            [me.raynes.fs :refer [touch delete]]
            [bob.data-test :refer [rules]]
            [bob.executor :refer :all]))

(deftest sort-rules-by-cpu-desc-test
  (let [
          sort-rules-by-cpu-desc
            @#'bob.executor/sort-rules-by-cpu-desc
        ]
    (is (=
         (sort-rules-by-cpu-desc rules)
         [ { :inp #{"f5"} :out #{"f7"} :cmd "C5" :cpu 2 :tag #{"F7" "level3"}}
           { :inp #{ "f1" "f2"} :out #{"f3"} :cmd "C1" :cpu 1 :tag #{"F3" "level1"} }
           { :inp #{"f2"} :out #{"f4"} :cmd "C2" :cpu 1 :tag #{"F4" "level2"}}
           { :inp #{"f3"} :out #{ "f5" "f6" } :cmd "C3" :cpu 1  :tag #{ "F5-6" "level2"}}
           { :inp #{"f4" "f6"} :out #{"f8"} :cmd "C4" :cpu 1 :tag #{ "F8" "level3"} }]))))

(deftest select-rule-with-closest-cpu-test
  (let [
          select-rule-with-closest-cpu
            @#'bob.executor/select-rule-with-closest-cpu
          rules*
              (map #(merge % { :cpu 3 }) rules)
        ]
  (is (=
        (select-rule-with-closest-cpu rules 2)
          { :inp #{"f5"} :out #{"f7"} :cmd "C5" :cpu 2 :tag #{"F7" "level3"}}))
  (is (=
        (select-rule-with-closest-cpu rules 1)
        { :inp #{ "f1" "f2"} :out #{"f3"} :cmd "C1" :cpu 1 :tag #{"F3" "level1"}}))
  (is (=
        (select-rule-with-closest-cpu rules* 1)
        nil))))

(deftest split-rules-to-run-test
  (is (=
        (split-rules-to-run rules 4)
        [ [ { :inp #{"f5"} :out #{"f7"} :cmd "C5" :cpu 2 :tag #{"F7" "level3"}}
            { :inp #{ "f1" "f2"} :out #{"f3"} :cmd "C1" :cpu 1 :tag #{"F3" "level1"}}
            { :inp #{"f2"} :out #{"f4"} :cmd "C2" :cpu 1 :tag #{"F4" "level2"} } ]
          [ { :inp #{"f3"} :out #{ "f5" "f6" } :cmd "C3" :cpu 1 :tag #{ "F5-6" "level2"}}
            { :inp #{"f6" "f4"} :out #{"f8"} :cmd "C4" :cpu 1 :tag #{ "F8" "level3"}} ]
          0 ]))
   (is (=
         (split-rules-to-run rules 3)
         [ [ { :inp #{"f5"} :out #{"f7"} :cmd "C5" :cpu 2 :tag #{"F7" "level3"} }
             { :inp #{ "f1" "f2"} :out #{"f3"} :cmd "C1" :cpu 1 :tag #{"F3" "level1"}} ]
           [ { :inp #{"f2"} :out #{"f4"} :cmd "C2" :cpu 1 :tag #{"F4" "level2"}}
             { :inp #{"f3"} :out #{ "f5" "f6" } :cmd "C3" :cpu 1 :tag #{ "F5-6" "level2" } }
             { :inp #{"f6" "f4"} :out #{"f8"} :cmd "C4" :cpu 1 :tag #{ "F8" "level3"}}]
           0 ]))
   (is (=
         (split-rules-to-run rules 2)
         [ [ { :inp #{"f5"} :out #{"f7"} :cmd "C5" :cpu 2 :tag #{"F7" "level3"} } ]
           [ { :inp #{ "f1" "f2"} :out #{"f3"} :cmd "C1" :cpu 1 :tag #{"F3" "level1"}}
             { :inp #{"f2"} :out #{"f4"} :cmd "C2" :cpu 1 :tag #{"F4" "level2"}}
             { :inp #{"f3"} :out #{ "f5" "f6" } :cmd "C3" :cpu 1 :tag #{ "F5-6" "level2" }}
             { :inp #{"f6" "f4"} :out #{"f8"} :cmd "C4" :cpu 1 :tag #{ "F8" "level3"}} ]
           0 ]))
   (is (=
         (split-rules-to-run rules 1)
         [ [ { :inp #{ "f1" "f2"} :out #{"f3"} :cmd "C1" :cpu 1 :tag #{"F3" "level1"}}]
           [ { :inp #{"f2"} :out #{"f4"} :cmd "C2" :cpu 1 :tag #{"F4" "level2"}}
             { :inp #{"f3"} :out #{ "f5" "f6" } :cmd "C3" :cpu 1 :tag #{ "F5-6" "level2" }}
             { :inp #{"f6" "f4"} :out #{"f8"} :cmd "C4" :cpu 1 :tag #{ "F8" "level3"}}
             { :inp #{"f5"} :out #{"f7"} :cmd "C5" :cpu 2 :tag #{"F7" "level3"}} ]
           0 ])))

