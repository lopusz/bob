(ns bob.helper-test
  (:require [ clojure.test :refer :all]
            [ me.raynes.fs :as fs]
            [ bob.helper :refer :all ]))

(deftest escape-path-test
  (is (= (escape-path "") ""))
  (is (= (escape-path "ala(") "ala\\("))
  (is (= (escape-path "ala\\(") "ala\\("))
  (is (= (escape-path "ala\\)") "ala\\)"))
  (is (= (escape-path "ala(test") "ala\\(test"))
  (is (= (escape-path "ala\\(test") "ala\\(test"))
  (is (= (escape-path "ala\\)test") "ala\\)test"))
  (is (= (escape-path "ala|") "ala\\|"))
  (is (= (escape-path "ala&") "ala\\&"))
  (is (= (escape-path "ala;") "ala\\;"))
  (is (= (escape-path "ala<") "ala\\<"))
  (is (= (escape-path "ala>") "ala\\>"))
  (is (= (escape-path "ala$") "ala\\$"))
  (is (= (escape-path "ala'") "ala\\'"))
  (is (= (escape-path "ala\"") "ala\\\""))
  (is (= (escape-path "ala`") "ala\\`"))
  (is (= (escape-path "ala ") "ala\\ "))
  (is (= (escape-path "ala\n") "ala\\\n"))
  (is (= (escape-path "ala\t") "ala\\\t"))
  (is (= (escape-path "ala*") "ala\\*"))
  (is (= (escape-path "ala?") "ala\\?"))
  (is (= (escape-path "ala[") "ala\\["))
  (is (= (escape-path "ala]") "ala\\]"))
  (is (= (escape-path "ala#") "ala\\#"))
  (is (= (escape-path "ala~") "ala\\~"))
  (is (= (escape-path "ala=") "ala\\="))
  (is (= (escape-path "ala%") "ala\\%"))
  (is (= (escape-path "ala\\") "ala\\\\"))
  (is (= (escape-path "ala\\\\") "ala\\\\"))
  (is (= (escape-path "ala\\ola") "ala\\\\ola"))
  (is (= (escape-path "ala\\\\ola") "ala\\\\ola")))

(deftest has-ending-slash-test
  (is (= (has-ending-slash? "") false))
  (is (= (has-ending-slash? "dir") false))
  (is (= (has-ending-slash? "dir") false))
  (is (= (has-ending-slash? "dir/dir") false))
  (is (= (has-ending-slash? "dir/") true))
  (is (= (has-ending-slash? "/") true)))

(deftest ensure-ending-slash-test
  (is (= (ensure-ending-slash "") "/"))
  (is (= (ensure-ending-slash "dir") "dir/"))
  (is (= (ensure-ending-slash "dir/") "dir/"))
  (is (= (ensure-ending-slash "dir/dir") "dir/dir/"))
  (is (= (ensure-ending-slash "dir/dir/") "dir/dir/")))

(deftest remove-ending-slash-test
  (is (= (remove-ending-slash "/") ""))
  (is (= (remove-ending-slash "dir/") "dir"))
  (is (= (remove-ending-slash "dir") "dir"))
  (is (= (remove-ending-slash "dir/dir") "dir/dir"))
  (is (= (remove-ending-slash "dir/dir/") "dir/dir")))

(deftest normalize-path-test
  (is (= (normalize-path "./dir//") "dir/"))
  (is (= (normalize-path "/dir//") "/dir/"))
  (is (= (normalize-path "/dir/") "/dir/"))
  (is (= (normalize-path "/dir") "/dir"))
  (is (= (normalize-path "/dir/./") "/dir/"))
  (is (= (normalize-path "/dir1/../dir2/") "/dir2/"))
  (is (= (normalize-path "/dir1/../dir2/../dir3") "/dir3"))
  (is (= (normalize-path "//dir1//./dir2")  "/dir1/dir2"))
  (is (= (normalize-path "/dir1/dir2/../") "/dir1/"))
  (is (= (normalize-path "/dir1/dir2/../") "/dir1/"))
  (is (= (normalize-path "dir1/../../dir2") "../dir2"))
  (is (= (normalize-path "dir1/../../dir2") "../dir2"))
  (is (= (normalize-path "/../dir2") nil)))

(deftest extract-dir-set-test
  (is (= (extract-dir-set []) #{}))
  (is (= (extract-dir-set ["ala.txt" "psa.txt"]) #{}))
  (is (= (extract-dir-set
           [ "ala/ma/kota.txt" "kot/ma/ale.txt"])
         #{ "ala/ma" "kot/ma"}))
  (is (= (extract-dir-set
           [ "ala/ma/kota.txt" "kot/ma/ale.txt" "ala/ma/psa.txt"])
         #{ "ala/ma" "kot/ma"})))

(deftest is-empty-dir-test
  (is (= (is-empty-dir? ".") false))
  (is (= (is-empty-dir? "src") false))
  (is (= (is-empty-dir? "project.clj") false))
  (is (= (is-empty-dir? "probably-not-exisiting-directory") false))
  (let [
        dir (fs/temp-dir "bob")
        res (is-empty-dir? (.toString dir))
        _ (fs/delete dir)
       ]
    (is res)))

(deftest prune-dir-test
  (let [
         dir-name1 (fs/temp-name "bob")
         dir-name2 (fs/temp-name "bob")
         dir-full (str dir-name1 "/" dir-name2)
         _ (fs/mkdirs dir-full)
         _ (prune-dir dir-name1)
         res1 (fs/exists? dir-name1)
         _ (prune-dir dir-full)
         res2 (fs/exists? dir-name1)
        ]
    (is res1)
    (is (not res2))))

(deftest remove-prefix-test
  (is (= (remove-prefix nil  nil) nil))
  (is (= (remove-prefix nil  "ala") nil))
  (is (= (remove-prefix ""  "") ""))
  (is (= (remove-prefix "ala ma kota"  nil) "ala ma kota"))
  (is (= (remove-prefix "ala ma kota"  "") "ala ma kota"))
  (is (= (remove-prefix "ala ma kota"  "") "ala ma kota"))
  (is (= (remove-prefix " ala ma kota"  "ala") " ala ma kota")))
