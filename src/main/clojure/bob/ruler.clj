(ns bob.ruler
  (:refer-clojure :exclude [assert])
  (:require
    [ pjstadig.assertions :refer [assert]]
    [ clojure.set :refer [union difference]]))

(defn remove-tagging-rules [ rules ]
  (filter #(contains? % :cmd) rules))

(defn calc-product-res-set [ rules ]
  (if (empty? rules)
    #{}
    (->> rules
      (remove-tagging-rules )
      (map :out)
      (reduce union))))

(defn calc-substrate-res-set [ rules ]
  (if (empty? rules)
    #{}
    (let [
            inp
              (reduce
                union
                (map :inp rules))
            out
              (calc-product-res-set rules)
         ]
      (difference inp out))))

(defn calc-tag-set [ rules ]
  (reduce
    union
    (map #(get % :tag #{}) rules)))

; Functions converting rules to look-up maps containing tags

(defn- merge-maps [ m1 m2 ]
  (merge-with union m1 m2))

(defn- conv-rule-to-tag*inp-res [ r ]
  (let [
         keys (:tag r)
         values (repeat (:inp r))
       ]
    (zipmap keys values)))

(defn conv-rules-to-tag*inp-res [ rules ]
  (reduce
    merge-maps
    (map conv-rule-to-tag*inp-res rules)))

(defn- conv-rule-to-out-res*tag [ r ]
  (let [
         keys (:out r)
         values (repeat (:tag r))
       ]
    (zipmap keys values)))

(defn conv-rules-to-out-res*tag [ rules ]
  (reduce
    merge-maps
    (map conv-rule-to-out-res*tag rules)))
