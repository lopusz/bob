(ns bob.generator-test
  (:require [clojure.test :refer :all]
            [bob.generator :refer :all]
            [bob.helper :refer [pwd]]))

(deftest make-rule-from-seq-test
  (set-max-cpu! 10)
  (is (=
    (@#'bob.generator/make-rule-from-seq
      [ (inp* "file1.txt") (inp* "file2.txt") (out* "file.lst") ])
    { :inp #{ "file1.txt" "file2.txt" }
      :out #{ "file.lst" }
      :cpu 1 }))

  (is (=
    (@#'bob.generator/make-rule-from-seq
      [ "ls -l" (inp "file1.txt") (inp "file2.txt") ">" (out "file.lst") ])
    { :inp #{ "file1.txt" "file2.txt" }
      :out #{ "file.lst" }
      :cmd "ls -l file1.txt file2.txt > file.lst"
      :cpu 1 }))
  (is (=
    (@#'bob.generator/make-rule-from-seq
      [ "ls -l" (inp "file1.txt" "file2.txt") ">" (out "file.lst") ])
     { :inp #{ "file1.txt" "file2.txt" }
       :out #{ "file.lst" }
       :cmd "ls -l file1.txt file2.txt > file.lst"
       :cpu 1 }))
  (is (=
    (@#'bob.generator/make-rule-from-seq
      [ "ls -l" (inp "file1.txt")
       (inp [ "file2.txt" "file3.txt" ]) ">" (out "file.lst") ])
    { :inp #{ "file1.txt" "file2.txt" "file3.txt" }
      :out #{ "file.lst" }
      :cmd "ls -l file1.txt file2.txt file3.txt > file.lst"
      :cpu 1 }))
  (is (=
    (@#'bob.generator/make-rule-from-seq
      [ "ls -l" (inp* "file1.txt")
        (inp [ "file2.txt" "file3.txt" ]) ">" (out "file.lst") ])
    { :inp #{ "file1.txt" "file2.txt" "file3.txt" }
      :out #{ "file.lst" }
      :cmd "ls -l file2.txt file3.txt > file.lst"
      :cpu 1 }))
  (is (=
    (@#'bob.generator/make-rule-from-seq
      [ "ls -l" (inp* "file1.txt")
        (inp [ "file2.txt" "file3.txt" ]) "-o" (out "file1.lst")
        (out* "file2.lst") (out* "file3.lst") ])
    { :inp #{ "file1.txt" "file2.txt" "file3.txt" }
      :out #{ "file1.lst" "file2.lst" "file3.lst" }
      :cmd "ls -l file2.txt file3.txt -o file1.lst"
      :cpu 1 }))
  (is (=
    (@#'bob.generator/make-rule-from-seq
      [ (cpu* 2) "ls -l"
        (inp [ "file1.txt" "file2.txt" ]) ">"
        (out "file1.lst") (out* "file2.lst") (out* "file3.lst")
        (cpu* 5) ])
    { :inp #{ "file1.txt" "file2.txt" }
      :out #{ "file1.lst" "file2.lst" "file3.lst" }
      :cmd "ls -l file1.txt file2.txt > file1.lst"
      :cpu 2 }))
  (is (=
    (@#'bob.generator/make-rule-from-seq
      [ "mycmd -n" (cpu 4)
        (inp [ "file1.txt" "file2.txt" ]) ">"
        (out "file1.lst") (out* "file2.lst") (out* "file3.lst")
        (cpu* 5) ])
    { :inp #{ "file1.txt" "file2.txt" }
      :out #{ "file1.lst" "file2.lst" "file3.lst" }
      :cmd "mycmd -n 4 file1.txt file2.txt > file1.lst"
      :cpu 4 }))
  (is (=
    (@#'bob.generator/make-rule-from-seq
      [ (file "mycmd")
        (inp [ "file1.txt" "file2.txt" ]) ">" (out "file1.lst")
        (out* "file2.lst") (out* "file3.lst")
        (cpu* 4) ])
    { :inp #{ "file1.txt" "file2.txt" }
      :out #{ "file1.lst" "file2.lst" "file3.lst" }
      :cmd "mycmd file1.txt file2.txt > file1.lst"
      :cpu 4 }))
  (is (=
    (@#'bob.generator/make-rule-from-seq
      [ (tag "myrule")
        (file "mycmd")
        (inp [ "file1.txt" "file2.txt" ]) ">" (out "file1.lst")
        (out* "file2.lst") (out* "file3.lst")
        (cpu* 4) ])
    { :inp #{ "file1.txt" "file2.txt" }
      :out #{ "file1.lst" "file2.lst" "file3.lst" }
      :cmd "mycmd file1.txt file2.txt > file1.lst"
      :cpu 4
      :tag #{"myrule"}}))
  (is (=
    (@#'bob.generator/make-rule-from-seq
      [ (tag "myrule1")
        (file "mycmd")
        (inp [ "file1.txt" "file2.txt" ]) ">" (out "file1.lst")
        (out* "file2.lst") (out* "file3.lst")
        (cpu* 4)
        (tag "myrule2") ])
    { :inp #{ "file1.txt" "file2.txt" }
      :out #{ "file1.lst" "file2.lst" "file3.lst" }
      :cmd "mycmd file1.txt file2.txt > file1.lst"
      :cpu 4
      :tag #{ "myrule1" "myrule2" }}))
  (is (=
    (@#'bob.generator/make-rule-from-seq
      [ (file "mycmd")
        (inp [ "file1.txt" "file2.txt" ]) ">" (out "file1.lst")
        (out* "file2.lst") (out* "file3.lst")
        (cpu* 4)
        (tag [ "myrule1" "myrule2"]) ])
    { :inp #{ "file1.txt" "file2.txt" }
      :out #{ "file1.lst" "file2.lst" "file3.lst" }
      :cmd "mycmd file1.txt file2.txt > file1.lst"
      :cpu 4
      :tag #{ "myrule1" "myrule2" }}))
  (is (=
    (@#'bob.generator/make-rule-from-seq
      [ (file "mycmd")
        (inp [ "file1.txt" "file2.txt" ]) ">" (out "file1.lst")
        (out* "file2.lst") (out* "file3.lst")
        (cpu* 4)
        (tag "myrule1" "myrule2") ])
    { :inp #{ "file1.txt" "file2.txt" }
      :out #{ "file1.lst" "file2.lst" "file3.lst" }
      :cmd "mycmd file1.txt file2.txt > file1.lst"
      :cpu 4
      :tag #{ "myrule1" "myrule2" }}))
  (is (=
    (@#'bob.generator/make-rule-from-seq
          [ "ls -l" (inp "file1(.txt") (inp "file2.txt") ">" (out "file.lst") ])
      { :inp #{ "file1(.txt" "file2.txt" }
        :out #{ "file.lst" }
        :cmd "ls -l file1\\(.txt file2.txt > file.lst"
        :cpu 1 })))

(deftest add-tag-test
  (let [
        r  [ (inp "./test.sh") (inp "input") (out "output") ]
        rules (into [] (take 10 (repeat r)))
        rules* (add-tag "out1" rules)
        rules**  (add-tag "out2" rules*)
        rules***  (add-tag (ftag) rules)
       ]
    (is
      (every? #(= (:tag %) #{ "out1"})
              (make-rules-from-seq2 rules*)))
    (is
      (every? #(= (:tag %) #{ "out1" "out2" })
              (make-rules-from-seq2 rules**)))
    ; This seems wrong. For reasons unknown to me file in ftag returns the
    ; file path bob/generator_test.clj, not
    ; src/test/clojure/bob/generator_test.clj :(
    (is
      (every? #(= (:tag %) #{ "bob" })
              (make-rules-from-seq2 rules***)))))
