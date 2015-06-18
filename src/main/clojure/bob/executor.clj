(ns bob.executor
  (:refer-clojure :exclude [assert])
  (:require
    [ pjstadig.assertions :refer [assert]]
    [ clojure.java.shell :refer [sh]]
    [ clojure.core.async :refer [ >!! thread]]
    [ bob.timer :refer [ get-time conv-time-interval-to-str ]]))

(defn- sort-rules-by-cpu-desc [ rules ]
  (let [
          comp-cpus-f #(> (:cpu %1) (:cpu %2))
        ]
  (sort comp-cpus-f rules)))

(defn- select-rule-with-closest-cpu [ rules free-cpu ]
  (->> rules
       (filter #(<= (:cpu %) free-cpu))
       (sort-rules-by-cpu-desc)
       ;;; To get empty rule instead of nil replace the `first` by
       ;;; (#(if (empty? %) {} (first %)))))
       first))

(defn split-rules-to-run [ rules free-cpu-0 ]
  (loop [
          runnable-rules []
          rest-rules rules
          free-cpu free-cpu-0
         ]
    (let [
          r
            (select-rule-with-closest-cpu rest-rules free-cpu)
          rest-rules*
            (remove #{r} rest-rules)
          runnable-rules*
            (if-not (nil? r)
              (cons r runnable-rules)
              runnable-rules)
          free-cpu*
            (if-not (nil? r)
              (- free-cpu (:cpu r))
              free-cpu)
          ]
      (if (nil? r)
        ;adding new rules is in front, to keep the "big jobs" first add reverse
        [ (reverse runnable-rules*) rest-rules* free-cpu*]
        (recur runnable-rules* rest-rules*  free-cpu*)))))

(defn run-rule [ server-ch rule ]
  (thread
    (let [
           t-start (get-time)
           cmd (:cmd rule)
           res (sh "bash" "-" :in cmd )
           t-stop (get-time)
           res* (assoc res
                   :start-time t-start
                   :stop-time t-stop)
         ]
   (>!! server-ch [ rule res* ]))))

(defn run-rule-dummy [ server-ch rule ]
  (thread
    (>!! server-ch
      [ rule { :out "" :err "" :exit 0 :start-time 0 :stop-time 0} ])))
