(ns bob.verificator
  (:require [ clojure.set :refer [intersection]]
            [ clojure.pprint :refer [pprint]]
            [ clojure.java.io :refer [input-stream]]
            [ me.raynes.fs :refer [mod-time size exists?]]
            [ bob.logger :refer [log-f]])
  (:import [org.joda.time DateTime DateTimeZone]
           [org.apache.commons.codec.digest DigestUtils]))

(defn get-file-checksum-md2 [fname]
  (DigestUtils/md2Hex (input-stream fname)))

(defn get-file-checksum-md5 [fname]
  (DigestUtils/md5Hex (input-stream fname)))

(defn get-file-checksum-sha1 [fname]
  (DigestUtils/sha1Hex (input-stream fname)))

(defn get-file-checksum-sha256 [fname]
  (DigestUtils/sha256Hex (input-stream fname)))

(defn get-file-checksum-sha384 [fname]
  (DigestUtils/sha384Hex (input-stream fname)))

(defn get-file-checksum-sha512 [fname]
  (DigestUtils/sha512Hex (input-stream fname)))

(defn get-file-checksum-time-size [fname]
  (str (mod-time fname) "A" (size fname)))

(defn conv-timestamp-to-datetime [timestamp]
  (DateTime. timestamp ^DateTimeZone (DateTimeZone/getDefault)))

(defn report-rebuilding [ r reason ]
  (let [
          out
            (:out r)
          out-str
            (apply str (interpose ", " out))
        ]
    (log-f :info "Scheduling " out-str " (" reason ").")))

(defn check-rule-files-mod-time [r]
  (let [
          inp (:inp r)
          out (:out r)
          out-timestamp-min
            (apply min (map mod-time out))
          newer-than-out-f
            #(when (< out-timestamp-min (mod-time %)) %)
          inp-newer
            (some newer-than-out-f inp)
          _ (when-not (nil? inp-newer)
              (report-rebuilding r (str inp-newer " is newer than output")))
        ]
      (not (nil? inp-newer))))


(defn check-rule-files-checksum [ r checksum-f cheksum-dict ]
  ; TODO This should work as a replacement of  `check-rule-files-mod-time`
  false)

(defn- check-missing-out [ r ]
  (let [
         out (:out r)
         out-not-exists
           (some #(when-not (exists? %) %) out)
         _ (when-not (nil? out-not-exists)
            (report-rebuilding r (str out-not-exists " does not exist")))
        ]
  (not (nil? out-not-exists))))

(defn- check-inp-in-outdated-res [r outdated-res-set]
   (assert (set? outdated-res-set))
  (let [
         inp
           (:inp r)
         inp-in-rebuilding
           (some
              #(when (contains? outdated-res-set %) %)
              inp)
          _
            (when-not (nil? inp-in-rebuilding)
              (report-rebuilding
                r
                (str inp-in-rebuilding " is rebuilt")))
        ]
    (not (nil? inp-in-rebuilding))))

(defn check-rule-for-rebuild [ r outdated-res-set ]
  (assert (set? outdated-res-set))
  (if (check-inp-in-outdated-res r outdated-res-set)
    true
    (if (check-missing-out r)
      true
      (check-rule-files-mod-time r))))
