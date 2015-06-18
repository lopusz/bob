(ns bob.grapher-test
  (:require [ clojure.test :refer :all]
            [ bob.data-test :refer [rules]]
            [ bob.grapher :refer :all ]))

(deftest calc-graph-test
  (is (=
    (into #{}
       (calc-tag-graph-edges
          rules
          (conv-tags-to-maps #{ "level1" "level2" "level3"})))
    #{ [ "level1" "level2"] [ "level2" "level3" ]} )))
