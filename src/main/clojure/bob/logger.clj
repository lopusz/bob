(ns bob.logger
  (:require
      [ clojure.string :refer [upper-case split-lines]]
      [ clansi :refer [style]]))

;; TODO
(defn log-to-file [ fname msg-type msg ]
  nil)

(def ^:private msg-type*color
  { :info  :green
    :warn  :magenta
    :error :red
    :stout :blue
    :sterr :cyan })

(defn log-to-stdout-ansi [ msg-type & msgs ]
  (let [
         msg-label
           (style
             (format "%-5s" (upper-case (name msg-type)))
             (get msg-type*color msg-type :black) :bright)
         msg
           (apply str msgs)
         msg-lines
           (split-lines msg)
        ]
    (dorun (map #(println (str "[" msg-label "] " %)) msg-lines))))

(def log-f log-to-stdout-ansi)
