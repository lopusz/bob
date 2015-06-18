(ns bob.orchestrator
  (:refer-clojure :exclude [assert])
  (:require
    [ pjstadig.assertions :refer [assert]]
    [ clojure.java.shell :refer [sh]]
    [ clojure.set :refer [union difference intersection]]
    [ clojure.core.async :refer [<!! >!! chan thread]]
    [ me.raynes.fs :refer [exists? file? delete]]
    [ bob.info :refer [get-info]]
    [ bob.helper :refer
      [conv-coll-to-str fail!! extract-dir-set ensure-dirs prune-dirs]]
    [ bob.logger :refer [log-f]]
    [ bob.ruler :refer [ calc-product-res-set calc-substrate-res-set]]
    [ bob.executor :refer [ run-rule split-rules-to-run ]]
    [ bob.verificator :refer [ check-rule-for-rebuild ]]
    [ bob.timer :refer [ get-time conv-time-interval-to-str ]]))

(def ^:private ^:const STOP-FNAME  "STOP")

(defn exists-stopfile? []
  (file? STOP-FNAME))

(defn remove-stopfile []
  (delete STOP-FNAME))

(defn fail-on-stopfile!! []
  (when (exists-stopfile?)
    (fail!! "File " STOP-FNAME " exists. Exiting.")))

(defn- find-inp-for-given-single-out-res
  "Returns set of inptus producing given output `out-res` according to `rules`.
   If there is no `out-res` in given rules, the value of `out-res` is returned."
  [ rules out-res ]
  (assert (coll? rules))
  (let [
         return-rule-if-it-has-given-out-f
           #(when (contains? (:out %) out-res) %)
         matching-rule
           (some
             return-rule-if-it-has-given-out-f
             rules)
        ]
  (if matching-rule
      (:inp matching-rule)
      #{out-res})))

(defn- find-inp-for-given-out-res [ rules out-res-set ]
  (assert (coll? rules))
  (assert (set? out-res-set))
  ; Function assumes that are no substrate res in `out-res-set`.
  ; the assertion below can check it.
  (comment
  (assert (empty?
             (filter #(not (set? %))
               (map
                 #(find-inp-for-given-single-out-res rules %)
                 out-res-set)))))
  (let [
         inp-res-seq
           (map #(find-inp-for-given-single-out-res rules %) out-res-set)
        ]
        (if (empty? inp-res-seq)
          #{}
          (reduce #(union %1 %2) inp-res-seq))))

(defn- split-substrate-res [ res-set substrate-res-set ]
  (assert (set? res-set))
  (assert (set? substrate-res-set))
  [ (intersection res-set substrate-res-set)
    (difference res-set substrate-res-set) ])

(defn- is-runnable? [ r avail-res-set ]
  (let [
        inp-res-set (:inp r)
        ]
    (=
      (count inp-res-set)
      (count
        (intersection avail-res-set inp-res-set)))))

(defn- split-runnable-rules [ rules avail-res-set ]
  (let [
         { runnable-rules true waiting-rules false}
           (merge  { true [] false [] }
             (group-by #(is-runnable? % avail-res-set) rules))
        ]
    [runnable-rules waiting-rules]))

(defn- calc-out-rule-map [ rules ]
  (let [
          gen-out-rule-pair
           (fn [r] (map #(vector % r) (:out r)))
       ]
      (into {} (mapcat gen-out-rule-pair rules))))

(defn- find-rules-for-given-res* [needed-res-set out*rule]
  (into #{}
     (map out*rule needed-res-set)))

(defn filter-only-necessary-rules [ rules-0 needed-res-set-0 ]
  (assert (coll? rules-0))
  (assert (set? needed-res-set-0))
  (loop [
          all-substrate-res-set (calc-substrate-res-set rules-0)
          out*rule (calc-out-rule-map rules-0)
          rest-rules-set (into #{} rules-0)
          necessary-rules-set #{}
          substrate-res-set #{}
          needed-res-set needed-res-set-0
         ]
    (let [
          [new-substrate-res-set needed-res-set*]
            (split-substrate-res needed-res-set all-substrate-res-set)
          new-necessary-rules-set
            (find-rules-for-given-res* needed-res-set* out*rule)
          rest-rules-set*
            (difference rest-rules-set new-necessary-rules-set)
          necessary-rules-set*
            (union new-necessary-rules-set necessary-rules-set)
          needed-res-set**
            (reduce union (map :inp new-necessary-rules-set))
          substrate-res-set*
            (union new-substrate-res-set substrate-res-set)
          ]
      (if (empty? needed-res-set*)
        [ necessary-rules-set* substrate-res-set*]
        (recur all-substrate-res-set
               out*rule
               rest-rules-set*
               necessary-rules-set*
               substrate-res-set*
               needed-res-set**)))))

(defn split-outdated-rules [ rules outdated-res-set ]
  (let [
          { outdated-rules true, up-to-date-rules false }
            (merge  { true [] false [] }
              (group-by #(check-rule-for-rebuild % outdated-res-set) rules))
       ]
    [ outdated-rules up-to-date-rules ]))

(defn filter-only-outdated-rules [rules-0 avail-res-set-0 ]
  (assert (coll? rules-0))
  (assert (not (empty? rules-0)))
  (assert (set? avail-res-set-0))
  (loop [
           outdated-rules []
           outdated-res-set #{}
           avail-res-set avail-res-set-0
           rules rules-0
         ]
    (let [
          [current-rules rules*]
            (split-runnable-rules rules avail-res-set)
          new-avail-res-set
            (reduce
               #(union %1 %2)
               (map :out current-rules))
          [ new-outdated-rules _ ]
            (split-outdated-rules current-rules outdated-res-set)
          new-outdated-res-set
            (if (empty? new-outdated-rules)
                #{}
                (reduce
                  #(union %1 %2)
                  (map :out new-outdated-rules)))
          outdated-res-set*
             (union new-outdated-res-set outdated-res-set)
          avail-res-set*
             (union new-avail-res-set avail-res-set)
          outdated-rules*
             (concat new-outdated-rules outdated-rules)
         ]
      (if (empty? rules*)
          [ outdated-rules* (difference avail-res-set* outdated-res-set*)]
          (recur
             outdated-rules*
             outdated-res-set*
             avail-res-set*
             rules*)))))

(defn- run-rules [ server-ch rules run-rule-f ]
  (doseq [ r rules ]
    (log-f :info "Running " (:cmd r))
    (run-rule-f server-ch r)))

(defn- init-build-state [ rules avail-res-set needed-res-set env ]
  (let [
        server-ch (chan)
        { :keys [ max-cpu split-rules-to-run-f run-rule-f ] }
           env
        [ runnable-rules waiting-rules ]
          (split-runnable-rules rules avail-res-set)
         [ running-rules runnable-rules* free-cpu]
           (split-rules-to-run-f runnable-rules max-cpu)
         _
           (run-rules server-ch running-rules run-rule-f)
         ]
    { :server-ch server-ch
      :waiting-rules waiting-rules
      :runnable-rules runnable-rules*
      :running-rules running-rules
      :avail-res-set avail-res-set
      :free-cpu  free-cpu }))

(defn- advance-build [ r build-state env ]
  (let [
         { :keys [ run-rule-f split-rules-to-run-f] }
           env
         { :keys [ server-ch waiting-rules runnable-rules running-rules
                   avail-res-set free-cpu ] }
           build-state
         free-cpu*
           (+ free-cpu (:cpu r))
         avail-res-set*
           (union (:out r) avail-res-set)
         [ new-runnable-rules waiting-rules*]
           (split-runnable-rules waiting-rules avail-res-set*)
         runnable-rules*
           (union (into #{} new-runnable-rules) runnable-rules)
         [ new-running-rules runnable-rules** free-cpu**]
           (split-rules-to-run-f runnable-rules* free-cpu*)
         _
           (run-rules server-ch new-running-rules run-rule-f)
         running-rules*
           (difference
             (union
               (into #{} new-running-rules)
               (into #{} running-rules))
               #{r})
        ]
      { :server-ch (:server-ch build-state)
        :waiting-rules waiting-rules*
        :runnable-rules  runnable-rules**
        :running-rules running-rules*
        :avail-res-set avail-res-set*
        :free-cpu free-cpu** }))

(defn- are-all-res-built? [ needed-res-set build-state ]
  (= (intersection  (:avail-res-set build-state) needed-res-set)
     needed-res-set))

(defn- report-rule-finished [ r res ]
  (let [
         cmd
           (:cmd r)
         { :keys [ out err exit start-time stop-time] }
           res
         time-str
           (conv-time-interval-to-str start-time stop-time)
       ]
    (if (= exit 0)
      (log-f :info time-str " wtime, exit OK, "  cmd)
      (log-f :error  time-str (format " wtime, exit %2d " exit) ", " cmd))
    (when-not (empty? out) (log-f :stout out))
    (when-not (empty? err) (log-f :sterr err))))

(defn- wait-for-running-rules [ build-state ]
  (when-not (empty? (:running-rules build-state))
    (loop [
            left-rules (:running-rules build-state)
          ]
      (let [
             [r res]
               (<!! (:server-ch build-state))
             _
               (report-rule-finished r res)
             left-rules*
               (remove #{r} left-rules)
            ]
        (when-not (empty? left-rules*)
          (recur left-rules*))))))

(defn- run-build [ rules avail-res-set needed-res-set env ]
  (loop [
         build-state
           (init-build-state rules avail-res-set needed-res-set env)
        ]
      (let [
             [r res]
               (<!! (:server-ch build-state))
             _ (report-rule-finished r res)
             failure (not= (:exit res) 0)
             stop (exists-stopfile?)
             build-state*
               (if (or failure stop)
                  (update-in
                    build-state
                    [:running-rules]
                    #(remove #{r} %))
                  (advance-build r build-state env))
             done (are-all-res-built? needed-res-set build-state*)
           ]
        (cond
          failure
            (do
              ; Error code was already reported by report rule finished
              (log-f :info "Waiting for the running rules to finish...")
              (wait-for-running-rules build-state*)
              [r (:exit res)])
          stop
            (do
              (log-f :info "File " STOP-FNAME " found. Canceling build.")
              (log-f :info "Waiting for the running rules to finish...")
              (remove-stopfile)
              (wait-for-running-rules build-state*)
              [{} nil])
          done
              [{} 0]
          :else
            (recur build-state*)))))

(defn- validate-rules!! [ rules ]
  (when (empty? rules)
    (do
      (log-f :info "Everything up to date.")
      (System/exit 0))))

(defn- validate-inp-res!! [ rules ]
  (let [
         substrate-res
           (calc-substrate-res-set rules)
         nonexistent-substrate-res
           (filter (complement exists?) substrate-res)
         err
           (not (empty? nonexistent-substrate-res))
         nonexistent-substrate-str
           (when err (conv-coll-to-str nonexistent-substrate-res))
        ]
    (when err
      (do
        (log-f :error
          "Input file(s) " nonexistent-substrate-str " not found." )
        (System/exit 1)))))

(defn- conv-status-to-str [ status ]
  (case status
    nil "terminated by STOP file"
    0 "OK"
    (str "error code " status)))

(defn build [ rules needed-res-set env ]
  (assert (coll? rules))
  (assert (set? needed-res-set))
  (assert (map? env))
  (assert (contains? env :max-cpu))
  (assert (=  needed-res-set
              (intersection needed-res-set (calc-product-res-set rules))))
  (let [
        t-start (get-time)
         _ (log-f :info "Welcome to bob, version " (get-info) ".")
         _ (log-f :info "Started building using " (:max-cpu env) " CPU(s).")
         env-default
           { :split-rules-to-run-f split-rules-to-run
             :run-rule-f run-rule }
         env*
           (merge env-default env)
         needed-res-str
           (conv-coll-to-str needed-res-set)
         [rules* avail-res-set]
           (filter-only-necessary-rules rules needed-res-set)
         _ (validate-inp-res!! rules*)
         [rules** avail-res-set*]
           (filter-only-outdated-rules rules* avail-res-set)
         _ (validate-rules!! rules**)
         _ (log-f :info
                  "Read " (count rules) " rules, "
                          (count rules*) " relevant, "
                          (count rules**) " outdated.")
         needed-dir-set
           (extract-dir-set
              (apply
                union
                (map :out rules**)))
         _
           (do
             (log-f :info "Creating output directories, if necessary.")
             (ensure-dirs needed-dir-set))
         _ (log-f :info "Building: "  needed-res-str)
         [ r status ]
           (run-build rules** avail-res-set* needed-res-set env*)
         _ (prune-dirs needed-dir-set)
         t-stop (get-time)
         t-str (conv-time-interval-to-str t-start t-stop)
         _ (log-f :info "Total building time: " t-str ".")
         _ (log-f :info "Build status - " (conv-status-to-str status))
        ]
    status))
