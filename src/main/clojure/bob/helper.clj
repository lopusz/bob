(ns bob.helper
  (:require
   [ clojure.string :as str]
   [ clojure.java.io :as io]
   [ me.raynes.fs :as fs])
  (:import
    [ java.io File]
    [ java.nio.file Files Paths]
    [ bob Tools ]))

;; Collection utilities

(defn conv-coll-to-str
  ([ coll ]
    (apply str (interpose ", " coll)))
  ([ coll sep-str ]
     (apply str (interpose sep-str coll))))

;; System utilites

(defn fail!! [ & msg ]
  (println (apply str msg))
  (System/exit 1))

;; File utilities

(defn escape-path [ ^String s ]
  (Tools/escapePathString s))

(defn pwd []
  (.toString (System/getProperty "user.dir")))

(defn dir-name
  "Returns directory part of a given path string.
   Coutner-partner of fs/base-name.
   When there is no directory part nil is returned."
  [ ^String path ]
  (if-let [ dname
              (-> path
                  File.
                 .getParent)
           ]
    dname
    ""))

(defn parent-dir-name [ ^String path ]
  "Returns the directory name in which the file exits."
  (-> path
      File.
      .getCanonicalFile
      .getParentFile
      .getName))

(defn canonicalize-path [ path ]
  (.toString (.getCanonicalFile (io/file path))))

(defn- get-last-char [ ^String s]
  (let [
        length-minus-one (max 0 (dec (.length s)))
       ]
    (. s substring length-minus-one)))

(defn has-ending-slash? [ ^String s]
  (= (get-last-char s) "/"))

(defn ensure-ending-slash [ ^String s ]
  (if (has-ending-slash? s)
    s
    (str s "/")))

(defn remove-ending-slash [ ^String s ]
  (if (has-ending-slash? s)
    (str/replace s #"/$" "")
    s))

(defn- normalize-path* [ ^String path ]
  (loop [
         tokens-to-process (filter #(not= "." %) (str/split path #"\/+"))
         result []
       ]
    (let [
          token (first tokens-to-process)
          tokens-to-process* (rest tokens-to-process)
          result*
              (if (and (= token "..")
                       (not= (count result) 0)
                       (not= (peek result) "")
                       (not= (peek result) ".."))
                 (pop result)
                 (conj result token))
          ]
      (if (< (count tokens-to-process*) 1)
        (apply str (interpose "/" result*))
        (recur tokens-to-process*  result*)))))

(defn normalize-path
  "Normalizes `path` given as as String.
   This means removing any non-initial '..' and initial '.'.

   There is `org.apache.commons.io.FilenameUtil.normalize` that does
   similar thing but on 'ala/../../ma' returns nil instead of '../ma'."

  [ ^String path ]
  (let [
         res (normalize-path* path)
        ]
    (if (re-find #"^\/\.\." res) ; Return nil if you want to go up from root
      nil
      (if (has-ending-slash? path)
        (str res "/")
        res))))

(defn is-empty-dir? [ ^String path ]
  (if-not (and path (fs/directory? path))
    false
    (with-open [ d (Files/newDirectoryStream
                     (Paths/get path (into-array [ "" ]))) ]
      (not (. (. d iterator) hasNext)))))

(defn extract-dir-set [ files ]
  (->> files
    (map  (comp dir-name normalize-path))
    (filter (complement empty?))
    (into #{})))

(defn ensure-dir [ dir ]
  (when-not (fs/exists? dir)
    (fs/mkdirs dir)))

(defn ensure-dirs [ dirs ]
  (dorun (map ensure-dir dirs)))

(defn prune-dir [ dir ]
  (when (is-empty-dir? dir)
     (fs/delete dir)
     (recur (dir-name (remove-ending-slash dir)))))

(defn prune-dirs [ dirs ]
  (dorun (map prune-dir dirs)))

;; String utilities

(defn remove-prefix
  "Removes initial `prefix` from string `s`.
   If `s` does not begin with `prefix` original `s` value is returned."
  [ ^String s ^String prefix ]
  (if (and s prefix (= (.indexOf s prefix) 0))
    (.substring s (.length prefix))
    s))
