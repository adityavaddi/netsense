(defproject event-generator "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.zeromq/cljzmq "0.1.4"]
                 [org.clojure/core.typed "0.3.3"]
                 [org.clojure/data.json "0.2.6"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [org.clojure/test.check "0.7.0"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [clojure-msgpack "1.1.0"]]
  :jvm-opts ["-Djava.library.path=/usr/local/lib"]
  :main ^:skip-aot event-generator.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}}
  :plugins [[lein-typed "0.3.5"]]
  :core.typed {:check []})
