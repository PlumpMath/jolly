(defproject org.clj-grenada/jolly "1.0.0-rc.3"
  :description "Small Clojure library for interop between Grenada and Grimoire"
  :url "https://github.com/clj-grenada/jolly"
  :license {:name "MIT license"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [dire "0.5.3"]
                 [org.clojars.rmoehn/guten-tag "0.1.5"]
                 [org.clj-grenada/lib-grenada "1.0.0-rc.4"]
                 [org.clj-grenada/poomoo "1.0.0-rc.3"]
                 [org.clojars.rmoehn/lib-grimoire "0.10.3"]
                 [prismatic/plumbing "0.4.0"]
                 [prismatic/schema "0.4.3"]]

  :codox {:sources ["src"]
          :output-dir "api-docs"
          :src-dir-uri "https://github.com/clj-grenada/jolly/blob/master/"
          :homepage-uri "https://github.com/clj-grenada/jolly/tree/master/"
          :src-linenum-anchor-prefix "L"})
