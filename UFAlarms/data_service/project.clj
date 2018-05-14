(defproject data-service "1.0.0-SNAPSHOT"
  :description "FIXME: write description"
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.zeromq/cljzmq "0.1.4"]
                 [clojurewerkz/cassaforte "2.0.0"]
                 [org.clojure/data.json "0.2.6"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [clojurewerkz/elastisch "2.2.0-beta4"]
                 [clj-http "1.1.2"]
                 [clojurewerkz/neocons "3.1.0-rc1"]
                 [clojure-msgpack "1.1.0"]]
  :jvm-opts ["-Djava.library.path=/usr/local/lib"]
  :profiles {:main-proxy {:main service.proxy}
            :main-worker {:main service.worker}}
  :plugins [[lein-typed "0.3.5"]]
  :core.typed {:check [adapters.core adapters.cassandra adapters.elasticsearch]})
