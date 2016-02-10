(ns bob.generator
  (:refer-clojure :exclude [assert])
  (:require
    [ pjstadig.assertions :refer [assert]]
    [ clojure.string :as s]
    [ clojure.set :as set]
    [ me.raynes.fs :as fs]
    [ bob.helper :refer
      [ensure-ending-slash has-ending-slash? dir-name parent-dir-name
       pwd normalize-path canonicalize-path conv-coll-to-str escape-path
       remove-prefix]]))

; Functionality realted to curent directory (*cur-dir*)

;; *cur-dir* is either empty or has a path with '/' at the end

(def ^:dynamic ^:private ^String *cur-dir* "")

(defn get-cur-dir []
  (System/getProperty("user.dir")))

(defn- conv-fname-to-ns [fname]
  (fs/path-ns (str "bobmod." (parent-dir-name fname))))

(defmacro bob-module [ & requires ]
  `(ns ~(conv-fname-to-ns *file*)
      [:require
        [ bob.generator :refer :all ]
        ~@requires]))

(defn get-cur-dir []
  *cur-dir*)

(defn append-to-cur-dir [ dir ]
  (if-not (empty? dir)
    (ensure-ending-slash
      (str *cur-dir* dir))
    dir))

(defn ls-cur-dir
  ([]
    (fs/list-dir *cur-dir*))

  ([ ^String path ]
    (fs/list-dir (append-to-cur-dir path)))

  ([ ^String path ^java.util.regex.Pattern re ]
     (filter
        #(re-find re %)
        (ls-cur-dir path))))

(defn ls-cur-dir-with-path
  ([ ^String path ]
    (map #(str path "/" %)  (ls-cur-dir path)))
  ([ ^String path ^java.util.regex.Pattern re ]
    (map #(str path "/" %) (ls-cur-dir path re))))

; Loader function for definition files

(defn load-clj-file [ fname ]
  (let [
         fname* (normalize-path fname)
         base-name (fs/base-name fname*)
         dir-name (dir-name fname*)
         dir-name-cur (append-to-cur-dir dir-name)
        ]
      (load-file (str dir-name-cur base-name))))

; Importer for clj modules

(defn gen-default-bob-file-for-dir [ dir ]
  (str (ensure-ending-slash dir) "bob.clj"))

(defn- import-one-module-file-internals [ base-name ]
  (let [
         full-fname (str *cur-dir* base-name)
           ns-name  (conv-fname-to-ns full-fname)
       ]
       (load-file full-fname)
       ((resolve
         (symbol (str ns-name "/-gen-rules"))))))

(defn- import-one-module-file [ fname ]
  (let [
         fname* (normalize-path fname)
         base-name (fs/base-name fname*)
         dir-name (dir-name fname*)
         rules
           (binding [
                      *ns* (the-ns 'bob.generator)
                      *cur-dir* (append-to-cur-dir dir-name)
                  ]
             (import-one-module-file-internals base-name))
       ]
    rules))

(defn import-module-file [ & fnames ]
  (apply
    concat
    (map import-one-module-file fnames)))

(defn- import-one-module-dir [ dname ]
  (import-module-file
    (gen-default-bob-file-for-dir dname)))

(defn import-module-dir [ & dnames ]
  (apply
    concat
    (map import-one-module-dir dnames)))

(defn- prefix-fname [ fname ]
  (if (or
         (empty? *cur-dir*)
         (fs/absolute? fname))
    fname
    (normalize-path (str *cur-dir* fname))))

; Helper functions for generating rules

(defn- make-seq-and-prefix [ item ]
  (if (sequential? item)
    (map prefix-fname item)
    [ (prefix-fname item) ]))

(defn inp [ & fnames ]
  { :inp
      (doall
        (mapcat make-seq-and-prefix fnames)) })

(defn out [ & fnames ]
  { :out
      (doall
        (mapcat make-seq-and-prefix fnames)) })

(defn inp* [ & fnames ]
  { :inp*
      (doall
        (mapcat make-seq-and-prefix fnames)) } )

(defn out* [ & fnames ]
  { :out*
      (doall
        (mapcat make-seq-and-prefix fnames)) } )

(defn file [ & fnames ]
  { :file
      (doall
        (mapcat make-seq-and-prefix fnames))} )

(defn cpu [ num-cpu ]
  { :cpu num-cpu } )

(defn cpu* [ num-cpu ]
   { :cpu* num-cpu })

(defn- make-seq [ item ]
  (if (sequential? item)
    item
    [ item ]))

(defn tag [ & tnames ]
  { :tag
     (doall
       (mapcat make-seq tnames)) } )

(defn- remove-pwd-from-dir [ dname ]
  (let [
        dname* (remove-prefix dname (str (pwd) "/"))
        ]
    (if (= dname dname*)
      (remove-prefix dname (pwd))
      dname*)))

(defn conv-fname-to-tag [fname]
  (-> fname
    (canonicalize-path)
    (dir-name)
    (remove-pwd-from-dir)))

(defmacro ftag []
  `(conv-fname-to-tag ~*file*))

(defn- add-tag-to-seq [ new-tag seq ]
  (cons (tag new-tag) seq))

(defn add-tag [ new-tag seq  ]
  (cond
    (empty? new-tag)
      seq
    (sequential? (first seq))
      (map #(add-tag-to-seq new-tag %) seq)
    :else
      (add-tag-to-seq new-tag seq)))

(defn- contains-any? [ coll keys ]
  (some
    #(contains? coll %) keys))

(defn- gen-cumulative-kwd-seq [ kwds seq ]
  (let [
         pred-f
           #(and (map? %) (contains-any? % kwds))
        ]
    (->> seq
         (filter pred-f)
         (mapcat vals))))

(defn- conv-map-seq-item-to-str [ seq-item ]
  (cond
    (contains? seq-item :inp)
      (conv-coll-to-str
        (map escape-path (:inp seq-item))
        " ")
    (contains? seq-item :out)
      (conv-coll-to-str
        (map escape-path (:out seq-item))
        " ")
    (contains? seq-item :file)
      (conv-coll-to-str
        (map escape-path (:file seq-item))
        " ")
    (contains? seq-item :cpu)
      (str (:cpu seq-item))
    :else ""))

(defn- conv-seq-item-to-str [ seq-item ]
  (cond
    (string? seq-item)
      seq-item
    (float? seq-item)
      (str seq-item)
    (integer? seq-item)
      (str seq-item)
    (map? seq-item)
      (conv-map-seq-item-to-str seq-item)
    (map? seq-item)
      nil
    :else ""))

(defn- gen-cmd [ seq ]
  (->> seq
    (map conv-seq-item-to-str)
    (filter (complement empty?))
    (interpose " ")
    (apply str)))

(defn- assoc-if-not-empty [ m k v ]
  (if-not (empty? v)
    (assoc m k v)
    m))

(defn- make-rule-from-seq  [ seq ]
  (let [
        inp
          (into #{}
           (mapcat identity
            (gen-cumulative-kwd-seq [ :inp :inp* ] seq)))
        out
          (into #{}
            (mapcat identity
              (gen-cumulative-kwd-seq [ :out :out* ] seq)))
        cmd
          (gen-cmd seq)
        cpu
          (if-let
            [a-cpu (first (gen-cumulative-kwd-seq [ :cpu :cpu* ] seq)) ]
            a-cpu
            1)
         tag
           (into #{}
             (mapcat identity
               (gen-cumulative-kwd-seq [ :tag ] seq)))
       ]
    (-> { :inp inp :out out :cpu cpu }
         (assoc-if-not-empty :tag tag)
         (assoc-if-not-empty :cmd cmd))))

(defn make-rules-from-seq2 [ seq2 ]
  (map make-rule-from-seq seq2))

(defn- make-realized-seq2 [ seq ]
  (if (sequential? (first seq))
    (doall seq)
    (vector (doall seq))))

(defmacro defrule [ & body ]
  `(defn ~'-gen-rules []
     (add-tag (ftag)
       (@#'bob.generator/make-realized-seq2
         ~@body))))

(defmacro defrule* [ & body ]
  `(defn ~'-gen-rules []
     (@#'bob.generator/make-realized-seq2
       ~@body)))
