(defproject clj-petitparser "0.1.0-SNAPSHOT"
  :description "Clojure port of Lukas Renggli's PetitParser"
  :url "http://example.com/FIXME"
  :license {:name "MIT License"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.10.1"]]
  :repl-options {:init-ns clj-petitparser.core}
  :profiles {:dev {:dependencies [[proto-repl "0.3.1"]
                                  [org.clojure/tools.cli "1.0.194"]
                                  [org.clojure/tools.namespace "0.3.1"]
                                  [org.clojars.beppu/clj-audio "0.3.0"]]
                   :plugins [[com.jakemccrary/lein-test-refresh "0.24.1"]]}})
