(ns bob.info)

;; {{msg}}

(defn get-version []
  "{{version}}")

(defn get-commit []
  "{{commit}}")

(defn get-build-date []
  "{{build-date}}")

(defn get-info []
  (str (get-version) " (" (get-commit) ") " (get-build-date)))
