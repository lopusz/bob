(ns bob.main-test
  (:require
      [clojure.test :refer :all]
      [bob.main :refer :all] ))

(def ^:private rules
  [ { :inp #{ "f1.txt" "f2.txt" } :out #{ "f3.txt" } :tag #{ "t1" } }
    { :inp #{ "f3.txt" "f4.txt" } :out #{ "f5.txt" } :tag #{ "t1" "t2"} }
    { :inp #{ "f5.txt" } :out #{ "f6.txt" } :tag #{ "t2" } }
    { :inp #{ "f5.txt" } :out #{ "f7.txt" } :tag #{ } } ])

(deftest  extract-out-with-any-tag-test
  (is (=
    (extract-out-with-any-tag #{ "t1" } [] )
    #{}))
  (is (=
    (extract-out-with-any-tag #{} rules)
    #{}))
  (is (=
    (extract-out-with-any-tag #{ "t1" } rules )
     #{ "f3.txt" "f5.txt" }))
  (is (=
    (extract-out-with-any-tag #{ "t2" } rules )
    #{ "f5.txt" "f6.txt" }))
  (is (=
    (extract-out-with-any-tag #{ "t1" "t2" } rules )
    #{ "f3.txt" "f5.txt" "f6.txt" })))

(deftest  extract-out-with-all-tag-test
  (is (=
    (extract-out-with-all-tag #{ "t1" } [] )
    #{}))
  (is (=
    (extract-out-with-all-tag #{} rules)
    #{}))
  (is (=
    (extract-out-with-all-tag #{ "t1" } rules )
     #{ "f3.txt" "f5.txt" }))
  (is (=
    (extract-out-with-all-tag #{ "t2" } rules )
    #{ "f5.txt" "f6.txt" }))
  (is (=
    (extract-out-with-all-tag #{ "t1" "t2" } rules )
    #{ "f5.txt" })))
