(defproject frontend "0.1.0-SNAPSHOT"
  :description "UI-facing GraphQL server interface to data services."
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [com.lucasbradstreet/cljs-uuid-utils "1.0.2"]
                 [com.lucasbradstreet/instaparse-cljs "1.4.1.0"]
                 [org.clojure/clojurescript "0.0-3269"]]
  :node-dependencies [[source-map-support "0.2.8"]
                      ["zmq" "^2.11.1"]
                      [q "1.4.1"]
                      [express "4.13.3"]
                      [body-parser "1.13.2"]
                      [morgan "1.6.1"]
                      [passport "0.2.2"]
                      [passport-http-bearer "1.0.1"]
                      [sha1 "1.1.1"]
                      [graphql "0.4.2"]]
  :plugins [[lein-npm "0.4.0"]
            [lein-cljsbuild "1.0.4"]]
  :source-paths ["src" "target/classes"]
  :clean-targets ["out"]
  :cljsbuild {
    :builds [{:id "frontend"
              :source-paths ["src/frontend"]
              :compiler {
                :main frontend.core
                :output-to "out/frontend/frontend.js"
                :output-dir "out/frontend"
                :optimizations :simple
                :target :nodejs
                :cache-analysis true
                :source-map "out/frontend/frontend.js.map"}}]})
