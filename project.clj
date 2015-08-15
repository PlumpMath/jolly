(defproject org.clj-grenada/jolly "0.1.0-SNAPSHOT"
  :description "Small Clojure library for interop between Grenada and Grimoire"
  :url "https://github.com/clj-grenada/jolly"
  :license {:name "MIT license"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [dire "0.5.3"]
                 [me.arrdem/guten-tag "0.1.4"]
                 [org.clj-grenada/lib-grenada "0.3.3-SNAPSHOT"]
                 [org.clj-grenada/poomoo "0.1.0-SNAPSHOT"]
                 ;; Remove the (remove … ".git") when bumping.
                 [org.clojure-grimoire/lib-grimoire "0.10.2"]
                 [prismatic/plumbing "0.4.0"]
                 [prismatic/schema "0.4.3"]])
