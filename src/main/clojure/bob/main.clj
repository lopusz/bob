(ns bob.main
  (:require
     [clojure.edn :as e]
     [clojure.set :refer [union difference intersection]]
     [clojure.string :refer [upper-case]]
     [clojure.tools.cli :refer [parse-opts]]
     [clojure.pprint :refer [pprint]]
     [me.raynes.fs :refer [exists? file? base-name delete]]
     [bob.info :refer [get-info]]
     [bob.generator :refer
       [gen-default-bob-file-for-dir import-module-file make-rules-from-seq2]]
     [bob.ruler :refer
       [calc-product-res-set calc-tag-set remove-tagging-rules]]
     [bob.executor :refer [run-rule run-rule-dummy]]
     [bob.orchestrator :refer
       [build filter-only-necessary-rules fail-on-stopfile!!]]
     [bob.helper :refer
       [canonicalize-path pwd conv-coll-to-str fail!!
        extract-dir-set prune-dirs]]
     [bob.grapher :refer
       [conv-tags-to-maps
        calc-tag-graph-edges filter-out-self-edges
        conv-tag-graph-to-dot-str conv-tag-graph-to-graphml-str]])
  (:gen-class))

(defn print-usage []
  (println "Usage: bob CMD [OPTION]")
  (println (str "Available CMD are build (bu), clean (cl), "
                "graph (gr), tags (ta), outputs (ou), "
                "version (ve)."))
  (println "Run \"bob CMD -h\" to se available options for given CMD."))

(defn validate-needed-res!! [ needed-res-set rules ]
  (let [
         unknown-res-set
           (difference
              needed-res-set
              (calc-product-res-set rules))
         err (not (empty? unknown-res-set))
         unknown-res-str
           (when err (conv-coll-to-str unknown-res-set))
        ]
    (when err
      (fail!! (str "Unknown resource(s) " unknown-res-str ".")))))

(defn conv-errors-to-str [ cmd errors ]
  (apply str
    (interpose "\n"
      (concat
         errors
         [(str "Type bob " cmd " --help  for list of options.")]))))

(defn parse-cmd-opts!! [ argv cmd-data ]
  (assert (map? cmd-data))
  (assert (contains? cmd-data :usage))
  (assert (contains? cmd-data :opts))
  (let [
         { :keys [ options arguments summary errors ] }
           (parse-opts argv (:opts cmd-data))
         _ (when-not (empty? errors)
             (fail!! (conv-errors-to-str (:cmd cmd-data) errors)))
         _ (when (:help options)
             (do (println (:usage cmd-data))
                 (printf "The following options are available for %s:\n"
                         (:cmd cmd-data))
                 (println summary)
                 (System/exit 0)))
        ]
    {:options options :arguments arguments}))

(defn import-rules!!
  [^String s]
  (if (exists? s)
    (if (file? s)
      (make-rules-from-seq2 (import-module-file s))
      (let [ s* (gen-default-bob-file-for-dir s) ]
        (if (exists? s*)
          (make-rules-from-seq2 (import-module-file s*))
          (fail!! "Could not find " s*))))
    (fail!! "Could not find " s)))

(defn reduce-cpu [ max-cpu cpu ]
  (cond
    (> cpu max-cpu) max-cpu
    (= cpu -1) max-cpu
    :else cpu))

(defn correct-cpu-in-rules [ max-cpu rules ]
  (let [
         reduce-cpu-f #(reduce-cpu max-cpu %)
        ]
    (map #(update-in % [ :cpu ] reduce-cpu-f) rules)))

(defn extract-out-with-any-tag [tag-set rules]
  (let [
         rules-with-any-tag
           (filter
             #(not (empty? (intersection tag-set (:tag %))))
            rules)
         out-with-any-tag
           (map :out rules-with-any-tag)
       ]
    (reduce union out-with-any-tag)))

(defn extract-out-with-all-tag [tag-set rules]
  (if (empty? tag-set)
    #{}
    (let [
           n (count tag-set)
           rules-with-all-tag
             (filter
                #(= n (count (intersection tag-set (:tag %))))
                rules)
           out-with-all-tag
             (map :out rules-with-all-tag)
         ]
    (reduce union out-with-all-tag))))

(defn gen-needed-res-set [ mode arguments rules ]
  (let [
          argument-set (into #{} arguments)
        ]
    (cond
      (empty? argument-set)
        (calc-product-res-set rules)
      (= mode :res)
        argument-set
      (= mode :tags-or)
        (extract-out-with-any-tag argument-set rules)
      (= mode :tags-and)
        (extract-out-with-all-tag argument-set rules))))

(defn gen-rules-and-needed-res!! [ options arguments ]
  (let [
         { :keys [ rules-path max-cpu mode ] }
           options
         rules
           (import-rules!! rules-path)
         needed-res-set
           (gen-needed-res-set mode arguments rules)
         _ (validate-needed-res!! needed-res-set rules)
      ]
    {:rules rules :needed-res-set needed-res-set}))

(defn parse-mode [s]
  (case (upper-case s)
    "A" :tags-and
    "O" :tags-or
    :res))

(def common-opts
  [ [ "-h" "--help" ]
    [ "-r" "--rules-path PATH" "file or directory with rules"
           :default "." ]
    [ "-m" "--mode MODE"
        (str "mode forarguments interpretation "
             "(a = tags and, o = tags or, otherwise resources)")
       :default :tags-or :parse-fn parse-mode ] ])

;; BUILD COMMAND

(def build-cmd-data
  { :cmd "build"
    :usage (str "Usage: bob build [OPTIONS] [ARG1 ARG2 ... ]\n\n"
                "Builds all resources and their dependencies.\n"
                "If no resources are given everything is built.\n")
    :opts
      (concat common-opts
        [ [ "-c" "--max-cpu CPU" "maximum CPUs to use, -1 means ALL"
          :default -1
          :parse-fn #(Integer/parseInt %) ]
         [ "-d" "--dry-run" "show what will be build, but do nothing" ]])})

(defn main-build [ argv ]
  (let [
         _ (fail-on-stopfile!!)
         { :keys [ options arguments ] }
           (parse-cmd-opts!! argv build-cmd-data)
         { :keys [ rules needed-res-set ] }
           (gen-rules-and-needed-res!! options arguments)
         { :keys [max-cpu dry-run] }
           options
         max-cpu*
           (if (= max-cpu -1)
             (.availableProcessors (Runtime/getRuntime))
             max-cpu)
         rules*
           (->> rules
             remove-tagging-rules
             (correct-cpu-in-rules max-cpu*))
         run-rule-f
           (if dry-run
             run-rule-dummy
             run-rule)
        ]
    (build rules* needed-res-set
      { :max-cpu max-cpu* :run-rule-f run-rule-f })))

;; CLEAN COMMAND

(def clean-cmd-data
  {  :cmd "clean"
     :usage (str "Usage: bob clean [OPTIONS] [ARG1 ARG2 ... ]\n\n"
                 "Cleans all resources given and their depndencies.\n"
                 "If no resources are given everything is cleaned.\n")
      :opts
        (concat
           common-opts
           [ [ "-f" "--force" "force cleaning without confirmation"
               :default false ]
             [ "-k" "--keep-dirs" "do not clean empty directories"
               :default false ] ]) })

(defn confirm-clean [ out ]
  (println "The following files will be deleted:")
  (dorun (map #(do (print "  ") (println %)) out))
  (printf "Are you sure you want to delete these %d files?\n" (count out))
  (flush)
  (contains? #{"Y" "YES" } (upper-case (read-line))))

(defn main-clean [ argv ]
  (let [
         { :keys [ options arguments ] }
          (parse-cmd-opts!!  argv clean-cmd-data)
         { :keys [ rules needed-res-set ] }
          (gen-rules-and-needed-res!! options arguments)
         [rules* _]
           (-> rules
             remove-tagging-rules
             (filter-only-necessary-rules needed-res-set))
         out (sort (calc-product-res-set rules*))
         out-dir-set (extract-dir-set out)
          ]
    (if (or (:force options) (confirm-clean out))
      (do
        (dorun (map delete out))
        (when-not (:keep-dirs options) (prune-dirs out-dir-set))
        (println "Cleaning done."))
      (println "Cleaning cancelled."))))

;; OUTPUTS COMMAND

(def outputs-cmd-data
  {  :cmd "outputs"
     :usage (str "Usage: bob outputs [OPTIONS] [RES]\n\n"
                "Shows all output files needed to build specified resources.\n"
                "If no resources are given all output files are displayed.\n")
     :opts
       common-opts })

(defn main-outputs [ argv ]
  (let [
         { :keys [ options arguments ] }
          (parse-cmd-opts!! argv outputs-cmd-data)
         { :keys [ rules needed-res-set ] }
          (gen-rules-and-needed-res!! options arguments)
         [rules* _]
           (-> rules
             remove-tagging-rules
             (filter-only-necessary-rules needed-res-set))
         out (sort (calc-product-res-set rules*))
        ]
    (dorun (map println out))))

;; RULES COMMAND

(def rules-cmd-data
  {
   :cmd "rules"
   :usage (str "Usage: bob rules [OPTIONS] [RES]\n\n"
               "Explicitly shows all the rules needed to build all resources.\n"
               "If no resources are given all rules are displayed.\n")
   :opts
     (concat
       common-opts
       [ [ "-c" "--max-cpu CPU" "maximum CPUs to use, -1 means ALL"
           :default -1
           :parse-fn #(Integer/parseInt %) ] ]) })

(defn print-rule [ r ]
  (printf "%s %s\n" "out:" (conv-coll-to-str (:out r)))
  (printf "%s %s\n" "inp:" (conv-coll-to-str (:inp r)))
  (printf "%s %d\n" "cpu:" (:cpu r))
  (println "tag:" (:tag r))
  (printf "%s %s\n\n" "cmd:" (:cmd r)))

(defn main-rules [ argv ]
  (let [
         { :keys [ options arguments ] }
           (parse-cmd-opts!! argv rules-cmd-data)
         { :keys [ rules needed-res-set ] }
           (gen-rules-and-needed-res!! options arguments)
         { :keys [ max-cpu ] }
           options
         max-cpu*
           (if (= max-cpu -1)
             (.availableProcessors (Runtime/getRuntime))
             max-cpu)
         rules*
           (->> rules
             remove-tagging-rules
             (correct-cpu-in-rules max-cpu*))
         [rules** _]
           (filter-only-necessary-rules rules* needed-res-set)
        ]
    (dorun (map print-rule rules**))
    (flush)))

;; VERSION COMMAND

(defn main-version [ ]
  (println "bob version" (get-info)))

;; GRAPH COMMAND

(def graph-cmd-data
  { :cmd "rules"
    :usage (str "Usage: bob garph [OPTIONS] [TAGS_EDN_FILE]\n\n"
                "Print tags dependency graph generated on theb basis of "
                " rules.\n"
                "Necessary tags are read from TAGS_EDN_FILE.
                 If the file is not given all tags are used.\n")
    :opts
      (concat
         (filter #(not= (first %) "-m") common-opts)
         [ [ "-f" "--format FORMAT" "output graph format (d)ot/(g)raphml"
             :default "dot" ]
           [ "-s" "--self-deps" :default false ]])})

(defn gen-rules!! [ options ]
  (import-rules!!
    (:rules-path options)))

(defn validate-tag-maps!! [ tag-set tag-maps ]
  (if-not
    (and
        (every? #(contains? tag-set (:tag %)) tag-maps)
        (every? #(contains? % :color) tag-maps)
        (every? #(contains? % :shape) tag-maps))
    (fail!! "Errors in TAGS_EDN_FILE.")
    tag-maps))

(defn gen-tags!! [ arguments rules ]
  (let [
         all-tag-set (calc-tag-set rules)
        ]
    (cond
      (empty? arguments)
        (conv-tags-to-maps all-tag-set)
      (> (count arguments) 1)
        (fail!! "To many TAG_EDN_FILEs given. Provide one name only.")
      (not (exists? (first arguments)))
        (fail!! "No " (first arguments) " file found.")
      :else
         (validate-tag-maps!! all-tag-set
           (e/read
             (java.io.PushbackReader.
               (clojure.java.io/reader (first arguments))))))))

(defn main-graph [ argv ]
  (let [
         { :keys [ options arguments ] }
          (parse-cmd-opts!! argv graph-cmd-data)
         rules
          (gen-rules!! options)
         { :keys [format self-deps] }
           options
         needed-tag-maps
          (gen-tags!! arguments rules)
         tag-graph-edges
          (calc-tag-graph-edges rules needed-tag-maps)
         tag-graph-edges*
           (if self-deps
             tag-graph-edges
             (filter-out-self-edges tag-graph-edges))
         ]
   (cond
     (contains? #{ "dot" "d"} format)
       (println (conv-tag-graph-to-dot-str
                   needed-tag-maps
                   tag-graph-edges*))
     (contains? #{ "graphml" "g"} format)
       (println (conv-tag-graph-to-graphml-str
                   needed-tag-maps
                   tag-graph-edges*))
     :else (fail!! "Unknown format" format "."))))

;; TAGS COMMAND

(def tags-cmd-data
  {  :cmd "tags"
     :usage (str "Usage: bob tags [OPTIONS]\n\n"
                 "Displays all tags.\n")
     :opts
       (concat
         (filter #(not= (first %) "-m") common-opts)
         [ [ "-f" "--format FORMAT" "output graph format (t)ext/(e)dn)"
             :default "text" ]])})

(defn print-tag-edn [ tag-map starter stopper ]
  (let [
         tag-map-str (pr-str tag-map)
        ]
  (println
    (str starter " " tag-map-str " " stopper))))

(defn print-tags-edn [ tags ]
  (let [
         n-1 (dec (count tags))
       ]
  (dorun
    (map print-tag-edn
         (conv-tags-to-maps tags)
         (concat ["[" ] (repeat n-1 " "))
         (concat (repeat n-1 "") [ "]" ])))))

(defn main-tags [ argv ]
  (let [
         { :keys [ options arguments ] }
          (parse-cmd-opts!! argv tags-cmd-data)
         rules
          (gen-rules!! options)
         { :keys [ format ] }
           options
         tags
           (sort (calc-tag-set rules))
       ]
   (cond
     (contains? #{"text" "t"} format)
       (dorun (map println tags))
     (contains? #{"edn" "e"} format)
       (print-tags-edn tags)
     :else (fail!! "Unknown format " format "."))))

;; MAIN BOB FUNCTION

(defn -main [ & argv ]
  (if (empty? argv)
    (print-usage)
    (case (first argv)
      ("bu" "build") (System/exit (main-build (rest argv)))
      ("cl" "clean") (main-clean (rest argv))
      ("ou" "outputs") (main-outputs (rest argv))
      ("ru" "rules") (main-rules (rest argv))
      ("gr" "graph") (main-graph (rest argv))
      ("ta" "tags") (main-tags (rest argv))
      ("ve" "version" "--version" "-v") (main-version)
      (print-usage))))
