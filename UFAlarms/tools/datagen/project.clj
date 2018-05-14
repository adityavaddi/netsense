(defproject datagen "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [net.sf.supercsv/super-csv-dozer "2.3.1"]
                 [crypto-random "1.2.0"]
                 [org.clojure/data.json "0.2.6"]
                 [cheshire "5.5.0"]
                 [org.apache.commons/commons-io "1.3.2"]
                 [org.zeromq/cljzmq "0.1.4"]
                 [clojure-msgpack "1.1.0"]
                 [org.neo4j/neo4j-enterprise "2.2.5"]
                 [org.clojure/data.json "0.2.6"]
                 [clojurewerkz/neocons "3.1.0-rc1"]
                 [org.json/json "20141113"]
                 [de.ubercode.clostache/clostache "1.4.0"]
                 ;; [org.slf4j/slf4j-log4j12 "1.7.12"]
                 [ch.qos.logback/logback-classic "1.1.3"]
                 [ch.qos.logback/logback-core "1.1.3"]
                 [org.slf4j/slf4j-api "1.7.12"]
                 [org.clojure/tools.logging "0.3.1"]
                 [org.codehaus.groovy/groovy-all "2.4.4"]
                 [clojurewerkz/cassaforte "2.0.0"]
                 [net.jpountz.lz4/lz4 "1.3.0"]
                 [org.xerial.snappy/snappy-java "1.1.2"]
                 [org.neo4j/neo4j-shell "2.2.5"]
                 [org.neo4j.app/neo4j-server "2.2.5"]]
  :source-paths ["src"]
  :java-source-paths ["src/java"]
  :main ^:skip-aot datagen.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}
             :main-createnodes {:main datagen.nodecreation}})
