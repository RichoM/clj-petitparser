(defproject clj-petitparser "0.1.2-SNAPSHOT"
  :description "Clojure port of Lukas Renggli's PetitParser"
  :url "https://github.com/RichoM/clj-petitparser"
  :license {:name "MIT License"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.10.1" :scope "provided"]
                 [org.clojure/clojurescript "1.10.773" :scope "provided"]]

  :plugins [[lein-cljsbuild "1.1.8"]]
  :cljsbuild {:builds {:dev {:source-paths ["src"]
                             :compiler {:output-to "resources/private/js/main.js"
                                        :optimizations :whitespace
                                        :pretty-print true}}
                       :test {:source-paths ["src" "test"]
                              :compiler {:output-to "resources/private/js/test.js"
                                         :optimizations :whitespace
                                         :pretty-print true}}}}

  :profiles {:dev {:dependencies [[proto-repl "0.3.1"]
                                  [org.clojure/tools.cli "1.0.194"]
                                  [org.clojure/tools.namespace "0.3.1"]
                                  [org.clojure/core.async "0.6.532"]
                                  [org.clojars.beppu/clj-audio "0.3.0"]]
                   :plugins [[com.jakemccrary/lein-test-refresh "0.24.1"]]
                   :global-vars {*unchecked-math* :warn-on-boxed
                                 *warn-on-reflection* true}
                   :source-paths ["env/dev/clj"]
                   :repl-options {:init-ns user}}
             :test {:resource-paths ["env/test/sounds"]}})
