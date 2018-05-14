(defproject dealer "1.0.0-SNAPSHOT"
  :description "FIXME: write description"
  :repositories [["eclipse" "https://repo.eclipse.org/content/repositories/paho-releases/"]
                 ]
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.zeromq/cljzmq "0.1.4"]
                 [org.zeromq/jzmq "3.1.0"]
                 [clojure-msgpack "1.1.0"]
                 [org.clojure/data.json "0.2.6"]
                 [danlentz/clj-uuid "0.1.7"]
                 [org.json/json "20141113"]
                 [de.ubercode.clostache/clostache "1.4.0"]
                 ;; [org.slf4j/slf4j-log4j12 "1.7.12"]
                 [ch.qos.logback/logback-classic "1.1.3"]
                 [ch.qos.logback/logback-core "1.1.3"]
                 [org.slf4j/slf4j-api "1.7.12"]
                 [org.clojure/tools.logging "0.3.1"]
                 [org.codehaus.groovy/groovy-all "2.4.4"]
                 [cc.qbits/hayt "4.0.0"]
                 [com.datastax.cassandra/cassandra-driver-core "3.4.0" :exclusions [io.netty/netty-handler]]
                 [net.jpountz.lz4/lz4 "1.3.0"]
                 [org.xerial.snappy/snappy-java "1.1.2"]
                 [org.neo4j.driver/neo4j-java-driver "1.5.0"]
                 [org.neo4j/neo4j-cypher "3.2.9"]
                 [org.neo4j/neo4j-enterprise "3.2.9"]
                 [org.eclipse.paho/org.eclipse.paho.client.mqttv3 "1.0.2"]
                 [org.clojure/core.async "0.2.374"] ;; be careful :)
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [clj-time "0.11.0"]
                 [clj-http "3.6.1"]
                 [cheshire "5.7.1"]
                 [clj-message-digest "1.0.0"]
                 [environ "1.0.1"]
                 [org.mapdb/mapdb "1.0.8"]
                 [biscuit "1.0.0"]
                 [net.logstash.logback/logstash-logback-encoder "4.4"]
                 ;; [org.scala-lang/scala-library "2.11.4"] ;; scala 2.11 causes severe issues with neo4j "no engine installed" exception
                 ;; [clj-logging-config/clj-logging-config "1.9.12"]

                 [org.clojure/tools.nrepl "0.2.12"]
                 [com.stuartsierra/component "0.3.1"]
                 [metrics-clojure "2.7.0"]
                 [metrics-clojure-graphite "2.7.0"]
                 [metrics-clojure-jvm "2.7.0"]
                 [me.raynes/fs "1.4.6"]
                 [com.novemberain/langohr "3.6.1"]
                 ;[org.apache.kafka/kafka-clients "1.0.0"]
                 [org.apache.kafka/kafka-streams "0.10.1.0"]
                 [org.apache.kafka/kafka_2.11 "0.10.1.0" :exclusions [org.slf4j/slf4j-log4j12
                                                                      log4j/log4j
                                                                      javax.jms/jms
                                                                      com.sun.jdmk/jmxtools
                                                                      com.sun.jmx/jmxri]]
                 [clojure-future-spec "1.9.0-alpha17"]
                 [com.gfredericks/test.chuck "0.2.7"]
                 [reduce-fsm "0.1.4"]]
  :plugins [[lein-environ "1.0.3"]]
  :source-paths ["src"]
  :java-source-paths ["src-java"]
  :resource-paths ["resources"]
  ;;:javac-options ["-target" "1.8" "-source" "1.8" "-Xlint:unchecked" "-g"]
  :main dealer.core ;; Provide option "all" to run all the 0mq services at once
  :filespecs [{:type :fn
               :fn (fn [p]
                     {:type :bytes
                      :path "git-describe"
                      :bytes (:out (clojure.java.shell/sh
                                    "git" "describe" "--tags" "--dirty=-DIRTY"))})}]
  :jvm-opts ["-Djava.library.path=/usr/local/lib"]
  ;:repl-options {:init-ns dealer.devsvcworker} ;
  :profiles {:main-dealer {:main dealer.dealer}
             :main-worker {:main dealer.worker}
             :main-cape {:main utils.cape}
             :main-nwrap {:main neowrap.neowrapper}
             :main-cassandra {:main utils.cassandra_init}
             :main-neo4j {:main utils.nodeconfig}
             :main-acl {:main dealer.aclservice}
             :main-video-analytics {:main dealer.videoAnalyticsService}
             :dev {:env {:name :development}
                   :dependencies [[org.clojure/test.check "0.9.0"]]}
             :test {:env {:name :test}
                    :dependencies [[reduce-fsm "0.1.4" :exclusions [dorothy]]]
                    :resource-paths ["src-mock"
                                     "patches"]}
             :testjs {:source-paths ["test"]
                      :env {:name :test
                            :neo4j-fixture :jstest}
                      :dependencies [[reduce-fsm "0.1.4" :exclusions [dorothy]]]
                      :resource-paths ["src-mock"
                                       "patches"]
                      :main utils.fixtures}
             :prodjs {:source-paths ["test"]
                      :env {:name :test
                            :neo4j-fixture :production}
                      :main utils.fixtures}
             :jstest {:source-paths ["test" "dev-src"]
                      :main build-utils.js-test-runner}
             :production {:env {:name :production}
                          :dependencies [[reduce-fsm "0.1.4" :exclusions [dorothy]]]
                          :resource-paths ["src-mock"
                                           "patches"]}}

  :aliases {"jstest" ["with-profile" "dev,testjs,jstest" "run"]}

  :aot [:all])
