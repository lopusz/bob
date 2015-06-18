(defproject bob "0.1.0-SNAPSHOT"

  ; GENERAL OPTIONS

  :description "description"
  :url "url"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :aot :all
  :omit-source true

  :jvm-opts ["-ea"]

  :jar-name "bob.jar"
  :uberjar-name "bob-standalone.jar"

  :main bob.main

  ; DEPENDENCIES

  :dependencies [
    [org.clojure/clojure "1.6.0"]

    ;; Channels
    [org.clojure/core.async "0.1.303.0-886421-alpha"]
    ;; Command line options
    [org.clojure/tools.cli "0.3.1" ]
    ;; Runtime assertions
    [pjstadig/assertions "0.1.0"]
    ;; File system utilities
    [me.raynes/fs "1.4.5"]
    ;; Date & time utilities
    [clj-time "0.7.0"]
    ;; ANSI colors in terminal
    [clansi "1.0.0"]
    ;; File checksums
    [commons-codec/commons-codec "1.9"]
   ]

  ; SOURCE DIRECTORY RECONFIGURATION

  :source-paths ["src/main/clojure"]
  :java-source-paths ["src/main/java"]
  :test-paths ["src/test/clojure"]

  ; PLUGINS + CONFIGURATION

  :plugins [
             [codox "0.8.10"]
             [lein-resource "0.3.7"]
             [lein-ancient "0.5.5"]
           ]

  ;; codox configuration

  :codox {
          :output-dir "target/apidoc"
          :sources [ "src/main/clojure"]
          :defaults {:doc/format :markdown}
          ;; Uncomment this to get github links in sources
          ;; :src-dir-uri "githubrepo/blob/master/"
          ;; :src-linenum-anchor-prefix "L"
          }

  ;; resource plugin configuration
  :hooks [ leiningen.resource ]

  :resource {
             :resource-paths ["src-res"]
             :target-path "src/main/clojure/bob"
             :extra-values
               { :msg
                   ~(str "This file was generated automatically"
                         " by the lein resource plugin.")
                 :version
                    ~(clojure.string/trim
                       (:out (clojure.java.shell/sh "sbin/info" "version")))
                 :build-date
                    ~(clojure.string/trim
                       (:out (clojure.java.shell/sh "sbin/info" "buil-date")))
                 :commit
                    ~(clojure.string/trim
                       (:out (clojure.java.shell/sh "sbin/info" "commit"))) }
               }
 )
