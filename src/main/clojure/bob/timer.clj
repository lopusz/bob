(ns bob.timer)

(defn ^:private conv-nanosecs-to-str
  "Converts time in nanoseconds (`nanosecs`) to string of the form hh:mm:dd."
  [ nanosecs ]
  (let [
        c10_9 1000000000
        secs (quot nanosecs c10_9)
        hours (quot secs 3600)
        hours_rest (rem secs 3600)
        minutes (quot hours_rest 60)
        seconds
        (double
          (/
            (-
              nanosecs
              (* c10_9
                (+
                  (* 3600 hours)
                  (* 60 minutes))))
            c10_9))


        ]
    (format "%d:%02d:%05.2f" hours minutes seconds)))

(defn get-time []
  (System/nanoTime))

(defn conv-time-interval-to-str [ t-start t-stop ]
  (conv-nanosecs-to-str (- t-stop t-start)))