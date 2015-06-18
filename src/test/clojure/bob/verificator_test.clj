(ns bob.verificator-test
  (:require [ clojure.test :refer :all]
            [ me.raynes.fs :refer [touch delete]]
            [ clojure.java.shell :refer [sh]]
            [ bob.verificator :refer :all]))

(deftest check-rule-files-mod-time-test1
  (let [
          rule { :inp  #{ "ala.txt" "ola.txt"}
                 :out  #{ "jas.txt" "rys.txt" } }
          _  (touch "ala.txt")
          _  (touch "ola.txt")
          _  (Thread/sleep 1000)
          _  (touch "jas.txt")
          _  (touch "rys.txt")
          res (check-rule-files-mod-time rule)
          _  (doall (map delete [ "ala.txt" "ola.txt" "jas.txt" "rys.txt"]))
       ]
    (is (= res false))))

(deftest check-rule-files-mod-time-test2
  (let [
          rule { :inp  #{ "ala.txt" "ola.txt"}
                 :out  #{ "jas.txt" "rys.txt"} }
          _  (touch "ola.txt")
          _  (touch "rys.txt")
          _  (Thread/sleep 1000)
          _  (touch "ala.txt")
          _  (touch "jas.txt")
          res (check-rule-files-mod-time rule)
          _  (doall (map delete [ "ala.txt" "ola.txt" "jas.txt" "rys.txt"]))
       ]
    (is (= res true))))

(deftest check-missing-out-test1
  (let [
          rule { :inp  #{ "ala.txt" "ola.txt"}
                 :out  #{ "jas.txt" "rys.txt"} }
          _  (touch "rys.txt")
          _  (touch "jas.txt")
          res (@#'bob.verificator/check-missing-out rule)
          _ (doall (map delete [ "jas.txt" "rys.txt"]))
        ]
    (is (= res false))))

(deftest check-for-missing-out-test2
  (let [
          rule { :inp  #{ "ala.txt" "ola.txt"}
                 :out  #{ "jas.txt" "rys.txt"} }
          _  (touch "jas.txt")
          res (@#'bob.verificator/check-missing-out rule)
          _ (delete "jas.txt")
        ]
    (is (= res true))))

(deftest check-for-missing-out-test3
  (let [
          rule { :inp  #{ "ala.txt" "ola.txt"}
                 :out  #{ "jas.txt" "rys.txt"} }
          _  (touch "rys.txt")
          res (@#'bob.verificator/check-missing-out rule)
          _ (delete "rys.txt")
        ]
    (is (= res true))))

(deftest check-missing-out-test4
  (let [
          rule { :inp  #{ "ala.txt" "ola.txt"}
                 :out  #{ "jas.txt" "rys.txt"} }
          res (@#'bob.verificator/check-missing-out rule)
        ]
    (is (= res true))))

(deftest check-missing-out-test5
  (let [
          rule { :inp  #{ "ala.txt" "ola.txt"}
                 :out  #{ } }
          res (@#'bob.verificator/check-missing-out rule)
        ]
    (is (= res false))))

(deftest check-inp-in-outdated-res-test
  (let [
          outdated-res-set #{ "ala.txt" }
          rule { :inp  #{ "ala.txt" "ola.txt"}
                 :out  #{ "jas.txt" "rys.txt"} }
          res
            (@#'bob.verificator/check-inp-in-outdated-res
              rule outdated-res-set)
        ]
    (is (= res true))))
