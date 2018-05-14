(ns logging.test.cassandra
  (:use [clojure.test] )
  (:require [logging.activitylogger :as logger]
            [utils.cassandra_fixture :as cass]
            [clj-time.core :as ctime]))

(use-fixtures :once cass/cassandra-fixture)

(deftest test-cassandra
  (do
    (logger/log "user" {:activity "createNode" :targettype "Node" :targetid "N013341e5" :message "props" :userid "testuser"})
    (logger/log "site" {:activity "createNode" :targettype "Node" :targetid "N013341e5" :message "props" :siteid "testsite"})
    (logger/log_connection_status "N013341e5" true)
    (logger/log_connection_status "N013341e5" false)
    (logger/log_sensor_sample {:nodeid "N013341e5" :sensor "rf" :value 18446744073702866887. :time 1450142892644317})
    (logger/log_traffic_current_status {
                                         :nodeid "xubernode",
                                         :time 1490829334423382,
                                         :active true,
                                         :channel 0,
                                         :count 4927,
                                         :data "{\"orgid\":\"uberorg\",\"nodeid\":\"xubernode\",\"detected_objects\":[],\"trafficdetectioneventid\":\"zlx(?UXb>bMNUVOfSLK}\",\"name\":\"LineCrossingEvent\",\"siteid\":\"ubersite\",\"time\":\"2017-03-29 23:15:34.42\",\"channel\":0,\"active\":true,\"count\":4927,\"count_per_class\":{\"car\":4927}}",
                                         :name "LineCrossingEvent",
                                         :orgid "uberorg",
                                         :siteid "ubersite",
                                         :trafficdetectioneventid "zlx(?UXb>bMNUVOfSLK}",
                                         :updated "052272b0-14d6-11e7-b662-dd40506e6e80"})
    (logger/log_traffic_current_status {
                                         :nodeid "xubernode",
                                         :time 1490829334423383,
                                         :active true,
                                         :channel 0,
                                         :count 4928,
                                         :data "{\"orgid\":\"uberorg\",\"nodeid\":\"xubernode\",\"detected_objects\":[],\"trafficdetectioneventid\":\"zlx(?UXb>bMNUVOfSLK}\",\"name\":\"LineCrossingEvent\",\"siteid\":\"ubersite\",\"time\":\"2017-03-29 23:15:34.43\",\"channel\":0,\"active\":true,\"count\":4928,\"count_per_class\":{\"car\":4928}}",
                                         :name "LineCrossingEvent",
                                         :orgid "uberorg",
                                         :siteid "ubersite",
                                         :trafficdetectioneventid "zlx(?UXb>bMNUVOfSLK}",
                                         :updated "052272b1-14d6-11e7-b662-dd40506e6e80"})

    (Thread/sleep 500)

    (is (= [{:value 18446744073702866887. :time "2015-12-15T01:28:12.644Z"}]
           (logger/get_latest_sensor_samples "N013341e5"
                                             "rf"
                                             (-> 5
                                                 ctime/minutes
                                                 ctime/ago
                                                 .toDate)
                                             1
                                             "UTC")))

    (logger/log_device_alarms {:nodeid "N013341e5" :alarmtype "HWFail_NIGHTHAWK" :severitycode "Major" :message "Not responding"})

    (let [startdt (.toDate (ctime/plus (ctime/now) (ctime/minutes 30)))
          expected [
                    {
                     "detected_objects" []
                     "nodeid" "xubernode"
                     "count" 4928
                     "name" "LineCrossingEvent"
                     "trafficdetectioneventid" "zlx(?UXb>bMNUVOfSLK}"
                     "time" "2017-03-29 23:15:34.43"
                     "siteid" "ubersite"
                     "channel" 0
                     "active" true
                     :aggregated_count 5
                     "orgid" "uberorg"
                     "count_per_class" {"car" 4928}
                     }
                    ]]
      (logger/log_light_mode {:nodeid ["N013341e5"] :isscheduled false :harvest_trigger true :policy "override" :driver 50 :startdt startdt})
      (logger/log_light_mode {:nodeid ["N013341e5"] :isscheduled true :harvest_trigger false :policy "override" :driver 100 :startdt startdt})

      (is (= (logger/get_latest_light_mode "N013341e5") {:nodeid "N013341e5" :isscheduled true :harvest_trigger false :policy "override" :driver 100 :priority nil :startdt startdt}))

      (logger/log_node_status {:nodeid "N013341e5" :siteid "siteid" :orgid "orgid" :net_stat 1 :lig_stat "off" :sen_stat "0"})

      (is (= (logger/get_latest_site_nodes_statuses "siteid") [{:nodeid "N013341e5", :lig_stat "off", :net_stat true :sen_stat "0" :siteid "siteid", :orgid "orgid"}]))
      (is (= (logger/get_latest_traffic_info "ubersite" "xubernode" nil ) expected)))


    ))
