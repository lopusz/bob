(ns bob.data-test)

;    f1   f2
;      \ /  \
;      f3    \       level1
;      / \    \
;    f5   f6   f4    level2
;    /     \  /
;   f7      f8       level3
;

(def rules [
  { :inp #{ "f1" "f2"} :out #{"f3"} :cmd "C1" :cpu 1 :tag #{"F3" "level1"} }
  { :inp #{"f2"} :out #{"f4"} :cmd "C2" :cpu 1 :tag #{"F4" "level2"}}
  { :inp #{"f3"} :out #{ "f5" "f6" } :cmd "C3" :cpu 1  :tag #{ "F5-6" "level2"}}
  { :inp #{"f4" "f6"} :out #{"f8"} :cmd "C4" :cpu 1 :tag #{ "F8" "level3"} }
  { :inp #{"f5"} :out #{"f7"} :cmd "C5" :cpu 2 :tag #{"F7" "level3"} } ] )


