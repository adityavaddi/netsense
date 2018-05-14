(ns appl.et-daylight-harvesting.crud-test
  (:require [appl.et-daylight-harvesting :as sut]
            [appl.et-daylight-harvesting.spec :as etdh-spec]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.test :refer :all]
            [clojure.tools.logging :refer :all]
            [neowrap.neowrapper :as neo4j]
            [clojure.walk :refer [keywordize-keys]]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]]
            [dealer.devsvcctrl-test :as dctrl-test]
            [mqtt.fixture :as mqtt]
            [kafka.mock :as kafka]
            [utils
             [cape-test :refer [vectors-to-sets
                                *user*
                                run
                                create-test-org
                                default-test-site
                                create-test-site
                                create-test-etdhprofile
                                default-test-node
                                create-test-node
                                create-test-lighting-group]]
             [cassandra_fixture :as cass]
             [neo4j-fixture :as neo4j-fixture]]))

;;; See: https://xeranet.atlassian.net/wiki/pages/viewpage.action?pageId=122966762

(use-fixtures :once (join-fixtures [cass/cassandra-fixture
                                    neo4j-fixture/neo4j-fixture
                                    mqtt/mqtt-fixture
                                    kafka/kafka-fixture
                                    dctrl-test/device-service-control-fixture
                                    dctrl-test/lfs-timeout-fixture
                                    utils.cape-test/scoped-thread-pool-fixture
                                    utils.cape-test/mock-google-api]))

(def test-etdhprofile
  {:etdhprofileid "07035604-0af1-450c-ab5e-c7c08102a403"
   :name "Example DHProfile"
   :high-lux 100
   :high-driver 0
   :low-lux 50
   :low-driver 20
   :min-lux 10
   :min-driver 80
   :fast-poll 30
   :slow-poll 600
   :scheduled [{:beginTime "sunrise-2"
                :endTime "sunset+2"}
               {:beginTime "20:00:00"
                :endTime "23:00:00"}]})

(defn get-etdhprofile
  [orgid siteid etdhprofileid]
  (run {:type "getETDHProfile"
        :user *user*
        :orgprops {:orgid orgid}
        :siteprops {:siteid siteid}
        :etdhprofileprops {:etdhprofileid etdhprofileid}}))

(defn get-group
  [orgid siteid groupid]
  (run {:type "getGroup"
        :user *user*
        :orgprops {:orgid orgid}
        :siteprops {:siteid siteid}
        :groupprops {:groupid groupid}}))

(defn get-node
  [orgid siteid nodeid]
  (run {:type "getNode"
        :user *user*
        :orgprops {:orgid orgid}
        :siteprops {:siteid siteid}
        :nodeprops {:nodeid nodeid}}))

(deftest crud-tests
  (testing "Able to Create a ETDHProfile"
    (let [orgid (create-test-org)
          siteid (create-test-site orgid)
          {:keys [etdhprofileid]
           :as etdhprofile} (gen/generate (s/gen :etdh/etdhprofile))]
      (testing "with default profile"
        (create-test-etdhprofile orgid siteid))
      (testing "with provided profile"
        (is (= etdhprofileid
               (create-test-etdhprofile orgid siteid etdhprofile))))))
  (testing "Able to Read a ETDHProfile"
    (let [orgid (create-test-org)
          siteid (create-test-site orgid)
          etdhprofile (gen/generate (s/gen :etdh/etdhprofile))
          etdhprofileid (create-test-etdhprofile orgid siteid etdhprofile)]
      (is (= (-> {:success true
                  :etdhprofile (-> etdhprofile
                                   sut/with-etdhprofile-defaults
                                   (assoc :etdhprofileid etdhprofileid
                                          :sites []
                                          :groups []
                                          :nodes []))}
                 vectors-to-sets)
             (-> (get-etdhprofile orgid siteid etdhprofileid)
                 vectors-to-sets)))))
  (testing "Able to Update a ETDHProfile"
    (let [orgid (create-test-org)
          site-latlon (select-keys default-test-site [:latitude
                                                      :longitude])
          siteid (create-test-site orgid site-latlon)
          [etdhprofile etdhprofile-update] (->> (repeatedly #(gen/generate (s/gen :etdh/etdhprofile)))
                                                (map #(dissoc % :etdhprofileid))
                                                distinct)
          etdhprofileid (create-test-etdhprofile orgid siteid etdhprofile)
          etdhprofile-update (assoc etdhprofile-update
                                    :etdhprofileid etdhprofileid)
          update-output (-> {:type "updateETDHProfile"
                             :user *user*
                             :orgprops {:orgid orgid}
                             :siteprops {:siteid siteid}
                             :etdhprofileprops etdhprofile-update}
                            run
                            vectors-to-sets)]
      (is (not= etdhprofile (dissoc etdhprofile-update
                                    :etdhprofileid)))
      (is (not= (-> {:success true
                     :etdhprofile (-> etdhprofile
                                      sut/with-etdhprofile-defaults
                                      (assoc :etdhprofileid etdhprofileid
                                             :sites []
                                             :groups []
                                             :nodes []))}
                    vectors-to-sets)
                update-output))
      (is (= (-> {:success true
                  :site site-latlon
                  :etdhprofile (-> etdhprofile-update
                                   sut/with-etdhprofile-defaults
                                   (assoc :etdhprofileid etdhprofileid
                                          :sites []
                                          :groups []
                                          :nodes []))}
                 vectors-to-sets)
             update-output))))
  (testing "Able to Delete a ETDHProfile"
    (let [orgid (create-test-org)
          siteid (create-test-site orgid)
          etdhprofile (gen/generate (s/gen :etdh/etdhprofile))
          etdhprofileid (create-test-etdhprofile orgid siteid etdhprofile)]
      (is (= (-> {:success true
                  :etdhprofile (-> etdhprofile
                                   sut/with-etdhprofile-defaults
                                   (assoc :etdhprofileid etdhprofileid
                                          :sites []
                                          :groups []
                                          :nodes []))}
                 vectors-to-sets)
             (-> (get-etdhprofile orgid siteid etdhprofileid)
                 vectors-to-sets)))
      (is (= {:success true}
             (run {:type "deleteETDHProfile"
                   :user *user*
                   :orgprops {:orgid orgid}
                   :siteprops {:siteid siteid}
                   :etdhprofileprops {:etdhprofileid etdhprofileid}})))
      (is (= {:success false
              :etdhprofile {}
              :error (format "ETDHProfile '%s' not found"
                             etdhprofileid)
              :status 404}
             (get-etdhprofile orgid siteid etdhprofileid)))))
  (testing "Able to Read all ETDHProfiles for a Site"
    (let [orgid (create-test-org)
          siteid (create-test-site orgid)
          etdhprofiles (->> (repeatedly #(gen/generate (s/gen :etdh/etdhprofile)))
                            distinct
                            (take 1))]
      (doseq [etdhprofile etdhprofiles]
        (create-test-etdhprofile orgid siteid etdhprofile))
      (is (= (-> {:success true
                  :items (mapv (comp #(assoc %
                                             :sites []
                                             :groups []
                                             :nodes [])
                                     sut/with-etdhprofile-defaults)
                               etdhprofiles)}
                 vectors-to-sets)
             (-> {:type "getAllETDHProfiles"
                  :user *user*
                  :orgprops {:orgid orgid}
                  :siteprops {:siteid siteid}}
                 run
                 vectors-to-sets))))))

(defn apply-etdh-site
  [orgid siteid etdhprofileid]
  (run {:type "applyETDHtoSite"
        :user *user*
        :orgprops {:orgid orgid}
        :siteprops {:siteid siteid}
        :etdhprofileprops {:etdhprofileid etdhprofileid}}))

(defn apply-etdh-groups
  [orgid siteid etdhprofileid groupids]
  (run {:type "applyETDHtoGroup"
        :user *user*
        :orgprops {:orgid orgid}
        :siteprops {:siteid siteid}
        :etdhprofileprops {:etdhprofileid etdhprofileid}
        :groupprops {:groupids groupids}}))

(defn org-site-slg
  []
  (let [orgid (create-test-org)
        siteid (create-test-site orgid)
        site-lighting-group (->> {:type "getAllGroups"
                                  :user *user*
                                  :orgprops {:orgid orgid}
                                  :siteprops {:siteid siteid}}
                                 run
                                 :items
                                 (map :groupid)
                                 first)]
    [orgid siteid site-lighting-group]))

(defn confirm-slg-assignment
  [orgid siteid
   {{site-lighting-group :groupid} :group
    :as cape-group}
   {:keys [etdhprofileid]
    etdhprofile-name :name
    :as etdhprofile}
   nodeids nodes]
  (is (= (-> {:success true
              :etdhprofile (-> etdhprofile
                               sut/with-etdhprofile-defaults
                               (assoc :etdhprofileid etdhprofileid
                                      :sites [{:name "Site Lighting Group"
                                               :groupid site-lighting-group}]
                                      :groups []
                                      :nodes (mapv #(do {:nodeid %})
                                                   nodeids)))}
             vectors-to-sets)
         (-> (get-etdhprofile orgid siteid etdhprofileid)
             vectors-to-sets)))
  (is (= (assoc-in cape-group
                   [:group
                    :etdhprofiles] [{:etdhprofileid etdhprofileid
                                     :name etdhprofile-name}])
         (get-group orgid siteid site-lighting-group)))
  (doseq [[node nodeid] (map vector nodes nodeids)]
    (is (= (update node
                   :node
                   merge {:etdhprofileid etdhprofileid
                          :etdhprofilename etdhprofile-name})
           (update (get-node orgid siteid nodeid)
                   :node dissoc
                   :configid
                   :configname)))))

(defn confirm-single-lg-assignment
  [orgid siteid
   {:keys [etdhprofileid]
    etdhprofile-name :name
    :as etdhprofile}
   {{:keys [groupid]} :group
    :as cape-group}
   [_
    _
    nodeid3
    nodeid4
    :as nodeids] nodes]
  (is (= (-> {:success true
              :etdhprofile (-> etdhprofile
                               sut/with-etdhprofile-defaults
                               (assoc :etdhprofileid etdhprofileid
                                      :sites []
                                      :groups [{:name "Test Lighting Group"
                                                :groupid groupid}]
                                      :nodes (mapv #(do {:nodeid %})
                                                   [nodeid3
                                                    nodeid4])))}
             vectors-to-sets)
         (-> (get-etdhprofile orgid siteid etdhprofileid)
             vectors-to-sets)))
  (is (= (assoc-in cape-group
                   [:group
                    :etdhprofiles] [{:etdhprofileid etdhprofileid
                                     :name etdhprofile-name}])
         (get-group orgid siteid groupid)))
  (doseq [[node nodeid] (->> (map vector nodes nodeids)
                             (take 2))]
    (is (= node
           (get-node orgid siteid nodeid))))
  (doseq [[node nodeid] (->> (map vector nodes nodeids)
                             (drop 2))]
    ;; TODO: Fix NSN-6353, NSN-6354
    #_(is (= (update node
                     :node
                     merge {:etdhprofileid etdhprofileid
                            :etdhprofilename etdhprofile-name})
             (get-node orgid siteid nodeid)))))

(defn confirm-multiple-lg-assignment
  [orgid siteid
   {:keys [etdhprofileid]
    etdhprofile-name :name
    :as etdhprofile}
   groupids cape-groups
   [_
    nodeid2
    nodeid3
    nodeid4
    :as nodeids] nodes]
  (is (= (-> {:success true
              :etdhprofile (-> etdhprofile
                               sut/with-etdhprofile-defaults
                               (assoc :etdhprofileid etdhprofileid
                                      :sites []
                                      :groups (mapv #(do {:name "Test Lighting Group"
                                                          :groupid %})
                                                    groupids)
                                      :nodes (mapv #(do {:nodeid %})
                                                   [nodeid2
                                                    nodeid3
                                                    nodeid4])))}
             vectors-to-sets)
         (-> (get-etdhprofile orgid siteid etdhprofileid)
             vectors-to-sets)))
  (doseq [[group groupid] (->> (map vector
                                    cape-groups
                                    groupids))]
    (is (= (assoc-in group
                     [:group
                      :etdhprofiles] [{:etdhprofileid etdhprofileid
                                       :name etdhprofile-name}])
           (get-group orgid siteid groupid))))
  (doseq [[node nodeid] (->> (map vector nodes nodeids)
                             (take 1))]
    (is (= node
           (get-node orgid siteid nodeid))))
  (doseq [[node nodeid] (->> (map vector nodes nodeids)
                             (drop 1))]
    (is (= (update node
                   :node
                   merge {:etdhprofileid etdhprofileid
                          :etdhprofilename etdhprofile-name})
           (update (get-node orgid siteid nodeid)
                   :node dissoc
                   :configid
                   :configname)))))

(deftest lighting-group-tests
  (testing "Assignment of a ETDH Profile to LG(s)"
    (testing "on"
      (testing "SiteLightingGroup"
        (testing "via"
          (testing "applyETDHtoSite"
            (let [[orgid siteid site-lighting-group] (org-site-slg)
                  [nodeid1
                   nodeid2
                   nodeid3
                   nodeid4
                   :as nodeids] (repeatedly 4
                                            #(create-test-node orgid
                                                               siteid))
                  groupid1 (create-test-lighting-group orgid
                                                       siteid
                                                       {:nodeList [nodeid2]})
                  groupid2 (create-test-lighting-group orgid
                                                       siteid
                                                       {:nodeList [nodeid3
                                                                   nodeid4]})
                  etdhprofile (gen/generate (s/gen :etdh/etdhprofile))
                  etdhprofileid (create-test-etdhprofile orgid siteid etdhprofile)]
              (is (= (vectors-to-sets
                      {:success true
                       :result (-> etdhprofile
                                   sut/with-etdhprofile-defaults
                                   (assoc :sites [{:name "Site Lighting Group"
                                                   :groupid site-lighting-group}]
                                          :groups []
                                          :nodes (mapv #(do {:nodeid %})
                                                       nodeids)))})
                     (-> (apply-etdh-site orgid siteid etdhprofileid)
                         vectors-to-sets)))))
          (testing "updateGroup"
            (let [[orgid siteid site-lighting-group] (org-site-slg)
                  {etdhprofile-name :name
                   :as etdhprofile} (gen/generate (s/gen :etdh/etdhprofile))
                  etdhprofileid (create-test-etdhprofile orgid siteid etdhprofile)
                  cape-group (get-group orgid siteid site-lighting-group)]
              (is (= (assoc-in cape-group
                               [:group
                                :etdhprofiles] [{:etdhprofileid etdhprofileid
                                                 :name etdhprofile-name}])
                     (run {:type "updateGroup"
                           :user *user*
                           :orgprops {:orgid orgid}
                           :siteprops {:siteid siteid}
                           :groupprops (-> cape-group
                                           :group
                                           (assoc :etdhprofiles [{:etdhprofileid etdhprofileid}]))})))))))
      (testing "a LightingGroup"
        (testing "via"
          (testing "applyETDHtoGroup"
            (let [[orgid siteid site-lighting-group] (org-site-slg)
                  [nodeid1
                   nodeid2
                   nodeid3
                   nodeid4] (repeatedly #(create-test-node orgid
                                                           siteid))
                  groupid1 (create-test-lighting-group orgid
                                                       siteid
                                                       {:nodeList [nodeid2]})
                  groupid2 (create-test-lighting-group orgid
                                                       siteid
                                                       {:nodeList [nodeid3
                                                                   nodeid4]})
                  etdhprofile (gen/generate (s/gen :etdh/etdhprofile))
                  etdhprofileid (create-test-etdhprofile orgid siteid etdhprofile)]
              (is (= (vectors-to-sets
                      {:success true
                       :result (-> etdhprofile
                                   sut/with-etdhprofile-defaults
                                   (assoc :sites []
                                          :groups [{:name "Test Lighting Group"
                                                    :groupid groupid2}]
                                          :nodes (mapv #(do {:nodeid %})
                                                       [nodeid3
                                                        nodeid4])))})
                     (-> (apply-etdh-groups orgid
                                            siteid
                                            etdhprofileid
                                            [groupid2])
                         vectors-to-sets)))))
          (testing "updateGroup"
            (let [[orgid siteid site-lighting-group] (org-site-slg)
                  [nodeid1
                   nodeid2
                   nodeid3
                   nodeid4] (repeatedly #(create-test-node orgid
                                                           siteid))
                  groupid1 (create-test-lighting-group orgid
                                                       siteid
                                                       {:nodeList [nodeid2]})
                  groupid2 (create-test-lighting-group orgid
                                                       siteid
                                                       {:nodeList [nodeid3
                                                                   nodeid4]})
                  {etdhprofile-name :name
                   :as etdhprofile} (gen/generate (s/gen :etdh/etdhprofile))
                  etdhprofileid (create-test-etdhprofile orgid siteid etdhprofile)
                  cape-group (get-group orgid siteid groupid2)]
              (is (= (assoc-in cape-group
                               [:group
                                :etdhprofiles] [{:etdhprofileid etdhprofileid
                                                 :name etdhprofile-name}])
                     (run {:type "updateGroup"
                           :user *user*
                           :orgprops {:orgid orgid}
                           :siteprops {:siteid siteid}
                           :groupprops (-> cape-group
                                           :group
                                           (assoc :etdhprofiles [{:etdhprofileid etdhprofileid}]))})))))))
      (testing "multiple LightingGroups"
        (testing "via"
          (testing "applyETDHtoGroup"
            (let [[orgid siteid site-lighting-group] (org-site-slg)
                  [nodeid1
                   nodeid2
                   nodeid3
                   nodeid4] (repeatedly #(create-test-node orgid
                                                           siteid))
                  groupids (mapv #(create-test-lighting-group orgid
                                                              siteid
                                                              {:nodeList %})
                                 [[nodeid2]
                                  [nodeid3
                                   nodeid4]])
                  etdhprofile (gen/generate (s/gen :etdh/etdhprofile))
                  etdhprofileid (create-test-etdhprofile orgid siteid etdhprofile)]
              (is (= (vectors-to-sets
                      {:success true
                       :result (-> etdhprofile
                                   sut/with-etdhprofile-defaults
                                   (assoc :sites []
                                          :groups (mapv #(do {:name "Test Lighting Group"
                                                              :groupid %})
                                                        groupids)
                                          :nodes (mapv #(do {:nodeid %})
                                                       [nodeid2
                                                        nodeid3
                                                        nodeid4])))})
                     (-> (apply-etdh-groups orgid
                                            siteid
                                            etdhprofileid
                                            groupids)
                         vectors-to-sets)))))))))
  (testing "Confirming assignment of a ETDH Profile to LG(s)"
    (testing "on"
      (testing "SiteLightingGroup"
        (testing "via"
          (testing "applyETDHtoSite"
            (let [[orgid siteid site-lighting-group] (org-site-slg)
                  [nodeid1
                   nodeid2
                   nodeid3
                   nodeid4
                   :as nodeids] (repeatedly 4
                                            #(create-test-node orgid
                                                               siteid))
                  groupid1 (create-test-lighting-group orgid
                                                       siteid
                                                       {:nodeList [nodeid2]})
                  groupid2 (create-test-lighting-group orgid
                                                       siteid
                                                       {:nodeList [nodeid3
                                                                   nodeid4]})
                  nodes (->> nodeids
                             (map #(get-node orgid siteid %))
                             doall)
                  {etdhprofile-name :name
                   :as etdhprofile} (gen/generate (s/gen :etdh/etdhprofile))
                  etdhprofileid (create-test-etdhprofile orgid siteid etdhprofile)
                  cape-group (get-group orgid siteid site-lighting-group)]
              (is (= (vectors-to-sets
                      {:success true
                       :result (-> etdhprofile
                                   sut/with-etdhprofile-defaults
                                   (assoc :sites [{:name "Site Lighting Group"
                                                   :groupid site-lighting-group}]
                                          :groups []
                                          :nodes (mapv #(do {:nodeid %})
                                                       nodeids)))})
                     (-> (apply-etdh-site orgid siteid etdhprofileid)
                         vectors-to-sets)))
              (confirm-slg-assignment orgid siteid
                                      cape-group
                                      etdhprofile
                                      nodeids nodes)))
          (testing "updateGroup"
            (let [[orgid siteid site-lighting-group] (org-site-slg)
                  [nodeid1
                   nodeid2
                   nodeid3
                   nodeid4
                   :as nodeids] (repeatedly 4
                                            #(create-test-node orgid
                                                               siteid))
                  groupid1 (create-test-lighting-group orgid
                                                       siteid
                                                       {:nodeList [nodeid2]})
                  groupid2 (create-test-lighting-group orgid
                                                       siteid
                                                       {:nodeList [nodeid3
                                                                   nodeid4]})
                  nodes (->> nodeids
                             (map #(get-node orgid siteid %))
                             doall)
                  {etdhprofile-name :name
                   :as etdhprofile} (gen/generate (s/gen :etdh/etdhprofile))
                  etdhprofileid (create-test-etdhprofile orgid siteid etdhprofile)
                  cape-group (get-group orgid siteid site-lighting-group)]
              (is (= (assoc-in cape-group
                               [:group
                                :etdhprofiles] [{:etdhprofileid etdhprofileid
                                                 :name etdhprofile-name}])
                     (run {:type "updateGroup"
                           :user *user*
                           :orgprops {:orgid orgid}
                           :siteprops {:siteid siteid}
                           :groupprops (-> cape-group
                                           :group
                                           (assoc :etdhprofiles [{:etdhprofileid etdhprofileid}]))})))
              (confirm-slg-assignment orgid siteid
                                      cape-group
                                      etdhprofile
                                      nodeids nodes)))))
      (testing "a LightingGroup"
        (testing "via"
          (testing "applyETDHtoGroup"
            (let [[orgid
                   siteid
                   site-lighting-group] (org-site-slg)
                  [nodeid1
                   nodeid2
                   nodeid3
                   nodeid4
                   :as nodeids] (repeatedly 4
                                            #(create-test-node orgid
                                                               siteid))
                  groupid1 (create-test-lighting-group orgid
                                                       siteid
                                                       {:nodeList [nodeid2]})
                  groupid2 (create-test-lighting-group orgid
                                                       siteid
                                                       {:nodeList [nodeid3
                                                                   nodeid4]})
                  cape-group (get-group orgid siteid groupid2)
                  nodes (->> nodeids
                             (map #(get-node orgid siteid %))
                             doall)
                  {etdhprofile-name :name
                   :as etdhprofile} (gen/generate (s/gen :etdh/etdhprofile))
                  etdhprofileid (create-test-etdhprofile orgid siteid etdhprofile)]
              (is (= (vectors-to-sets
                      {:success true
                       :result (-> etdhprofile
                                   sut/with-etdhprofile-defaults
                                   (assoc :sites []
                                          :groups [{:name "Test Lighting Group"
                                                    :groupid groupid2}]
                                          :nodes (mapv #(do {:nodeid %})
                                                       [nodeid3
                                                        nodeid4])))})
                     (-> (apply-etdh-groups orgid
                                            siteid
                                            etdhprofileid
                                            [groupid2])
                         vectors-to-sets)))
              (confirm-single-lg-assignment orgid siteid
                                            etdhprofile
                                            cape-group
                                            nodeids nodes)))
          (testing "updateGroup"
            (let [[orgid
                   siteid
                   site-lighting-group] (org-site-slg)
                  [nodeid1
                   nodeid2
                   nodeid3
                   nodeid4
                   :as nodeids] (repeatedly 4
                                            #(create-test-node orgid
                                                               siteid))
                  groupid1 (create-test-lighting-group orgid
                                                       siteid
                                                       {:nodeList [nodeid2]})
                  groupid2 (create-test-lighting-group orgid
                                                       siteid
                                                       {:nodeList [nodeid3
                                                                   nodeid4]})
                  cape-group (get-group orgid siteid groupid2)
                  nodes (->> nodeids
                             (map #(get-node orgid siteid %))
                             doall)
                  {etdhprofile-name :name
                   :as etdhprofile} (gen/generate (s/gen :etdh/etdhprofile))
                  etdhprofileid (create-test-etdhprofile orgid siteid etdhprofile)]
              (is (= (assoc-in cape-group
                               [:group
                                :etdhprofiles] [{:etdhprofileid etdhprofileid
                                                 :name etdhprofile-name}])
                     (run {:type "updateGroup"
                           :user *user*
                           :orgprops {:orgid orgid}
                           :siteprops {:siteid siteid}
                           :groupprops (-> cape-group
                                           :group
                                           (assoc :etdhprofiles [{:etdhprofileid etdhprofileid}]))})))
              (confirm-single-lg-assignment orgid siteid
                                            etdhprofile
                                            cape-group
                                            nodeids nodes)))))
      (testing "multiple LightingGroups"
        (testing "via"
          (testing "applyETDHtoGroup"
            (let [[orgid
                   siteid
                   site-lighting-group] (org-site-slg)
                  [nodeid1
                   nodeid2
                   nodeid3
                   nodeid4
                   :as nodeids] (repeatedly 4
                                            #(create-test-node orgid
                                                               siteid))
                  groupids (mapv #(create-test-lighting-group orgid
                                                              siteid
                                                              {:nodeList %})
                                 [[nodeid2]
                                  [nodeid3
                                   nodeid4]])
                  cape-groups (mapv #(get-group orgid siteid %)
                                    groupids)
                  nodes (->> nodeids
                             (map #(get-node orgid siteid %))
                             doall)
                  {etdhprofile-name :name
                   :as etdhprofile} (gen/generate (s/gen :etdh/etdhprofile))
                  etdhprofileid (create-test-etdhprofile orgid siteid etdhprofile)]
              (is (= (vectors-to-sets
                      {:success true
                       :result (-> etdhprofile
                                   sut/with-etdhprofile-defaults
                                   (assoc :sites []
                                          :groups (mapv #(do {:name "Test Lighting Group"
                                                              :groupid %})
                                                        groupids)
                                          :nodes (mapv #(do {:nodeid %})
                                                       [nodeid2
                                                        nodeid3
                                                        nodeid4])))})
                     (-> (apply-etdh-groups orgid
                                            siteid
                                            etdhprofileid
                                            groupids)
                         vectors-to-sets)))
              (confirm-multiple-lg-assignment orgid siteid
                                              etdhprofile
                                              groupids cape-groups
                                              nodeids nodes)))))))
  (testing "Updating assignment of a ETDH Profile to LG(s)"
    (testing "on"
      (testing "SiteLightingGroup"
        (testing "via"
          (testing "applyETDHtoSite"
            (let [[orgid
                   siteid
                   site-lighting-group] (org-site-slg)
                  [nodeid1
                   nodeid2
                   nodeid3
                   nodeid4
                   :as nodeids] (repeatedly 4
                                            #(create-test-node orgid
                                                               siteid))
                  groupid1 (create-test-lighting-group orgid
                                                       siteid
                                                       {:nodeList [nodeid2]})
                  groupid2 (create-test-lighting-group orgid
                                                       siteid
                                                       {:nodeList [nodeid3
                                                                   nodeid4]})
                  nodes (->> nodeids
                             (map #(get-node orgid siteid %))
                             doall)
                  [{etdhprofileid1 :etdhprofileid
                    :as etdhprofile1}
                   {etdhprofileid2 :etdhprofileid
                    :as etdhprofile2}] (->> (repeatedly #(gen/generate (s/gen :etdh/etdhprofile)))
                                            (map #(dissoc % :etdhprofileid))
                                            distinct
                                            (map #(assoc %
                                                         :etdhprofileid (create-test-etdhprofile orgid siteid %))))
                  cape-group (get-group orgid siteid site-lighting-group)]
              (is (= (vectors-to-sets
                      {:success true
                       :result (-> etdhprofile1
                                   sut/with-etdhprofile-defaults
                                   (assoc :etdhprofileid etdhprofileid1
                                          :sites [{:name "Site Lighting Group"
                                                   :groupid site-lighting-group}]
                                          :groups []
                                          :nodes (mapv #(do {:nodeid %})
                                                       nodeids)))})
                     (-> (apply-etdh-site orgid siteid etdhprofileid1)
                         vectors-to-sets)))
              (confirm-slg-assignment orgid siteid
                                      cape-group
                                      etdhprofile1
                                      nodeids nodes)
              (is (= (vectors-to-sets
                      {:success true
                       :result (-> etdhprofile2
                                   sut/with-etdhprofile-defaults
                                   (assoc :etdhprofileid etdhprofileid2
                                          :sites [{:name "Site Lighting Group"
                                                   :groupid site-lighting-group}]
                                          :groups []
                                          :nodes (mapv #(do {:nodeid %})
                                                       nodeids)))})
                     (-> (apply-etdh-site orgid siteid etdhprofileid2)
                         vectors-to-sets)))
              (confirm-slg-assignment orgid siteid
                                      cape-group
                                      etdhprofile2
                                      nodeids nodes)))
          (testing "updateGroup"
            (let [[orgid
                   siteid
                   site-lighting-group] (org-site-slg)
                  [nodeid1
                   nodeid2
                   nodeid3
                   nodeid4
                   :as nodeids] (repeatedly 4
                                            #(create-test-node orgid
                                                               siteid))
                  groupid1 (create-test-lighting-group orgid
                                                       siteid
                                                       {:nodeList [nodeid2]})
                  groupid2 (create-test-lighting-group orgid
                                                       siteid
                                                       {:nodeList [nodeid3
                                                                   nodeid4]})
                  nodes (->> nodeids
                             (map #(get-node orgid siteid %))
                             doall)
                  [{etdhprofileid1 :etdhprofileid
                    etdhprofile-name1 :name
                    :as etdhprofile1}
                   {etdhprofileid2 :etdhprofileid
                    etdhprofile-name2 :name
                    :as etdhprofile2}] (->> (repeatedly #(gen/generate (s/gen :etdh/etdhprofile)))
                                            (map #(dissoc % :etdhprofileid))
                                            distinct
                                            (map #(assoc %
                                                         :etdhprofileid (create-test-etdhprofile orgid siteid %))))
                  cape-group (get-group orgid siteid site-lighting-group)]
              (is (= (assoc-in cape-group
                               [:group
                                :etdhprofiles] [{:etdhprofileid etdhprofileid1
                                                 :name etdhprofile-name1}])
                     (run {:type "updateGroup"
                           :user *user*
                           :orgprops {:orgid orgid}
                           :siteprops {:siteid siteid}
                           :groupprops (-> cape-group
                                           :group
                                           (assoc :etdhprofiles [{:etdhprofileid etdhprofileid1}]))})))
              (confirm-slg-assignment orgid siteid
                                      cape-group
                                      etdhprofile1
                                      nodeids nodes)
              (is (= (assoc-in cape-group
                               [:group
                                :etdhprofiles] [{:etdhprofileid etdhprofileid2
                                                 :name etdhprofile-name2}])
                     (run {:type "updateGroup"
                           :user *user*
                           :orgprops {:orgid orgid}
                           :siteprops {:siteid siteid}
                           :groupprops (-> cape-group
                                           :group
                                           (assoc :etdhprofiles [{:etdhprofileid etdhprofileid2}]))})))
              (confirm-slg-assignment orgid siteid
                                      cape-group
                                      etdhprofile2
                                      nodeids nodes)))))
      (testing "a LightingGroup"
        (testing "via"
          (testing "applyETDHtoGroup"
            (let [[orgid
                   siteid
                   site-lighting-group] (org-site-slg)
                  [nodeid1
                   nodeid2
                   nodeid3
                   nodeid4
                   :as nodeids] (repeatedly 4
                                            #(create-test-node orgid
                                                               siteid))
                  groupid1 (create-test-lighting-group orgid
                                                       siteid
                                                       {:nodeList [nodeid2]})
                  groupid2 (create-test-lighting-group orgid
                                                       siteid
                                                       {:nodeList [nodeid3
                                                                   nodeid4]})
                  cape-group (get-group orgid siteid groupid2)
                  nodes (->> nodeids
                             (map #(get-node orgid siteid %))
                             doall)
                  [{etdhprofileid1 :etdhprofileid
                    :as etdhprofile1}
                   {etdhprofileid2 :etdhprofileid
                    :as etdhprofile2}] (->> (repeatedly #(gen/generate (s/gen :etdh/etdhprofile)))
                                            (map #(dissoc % :etdhprofileid))
                                            distinct
                                            (map #(assoc %
                                                         :etdhprofileid (create-test-etdhprofile orgid siteid %))))]
              (is (= (vectors-to-sets
                      {:success true
                       :result (-> etdhprofile1
                                   sut/with-etdhprofile-defaults
                                   (assoc :etdhprofileid etdhprofileid1
                                          :sites []
                                          :groups [{:name "Test Lighting Group"
                                                    :groupid groupid2}]
                                          :nodes (mapv #(do {:nodeid %})
                                                       [nodeid3
                                                        nodeid4])))})
                     (-> (apply-etdh-groups orgid
                                            siteid
                                            etdhprofileid1
                                            [groupid2])
                         vectors-to-sets)))
              (confirm-single-lg-assignment orgid siteid
                                            etdhprofile1
                                            cape-group
                                            nodeids nodes)

              (is (= (vectors-to-sets
                      {:success true
                       :result (-> etdhprofile2
                                   sut/with-etdhprofile-defaults
                                   (assoc :etdhprofileid etdhprofileid2
                                          :sites []
                                          :groups [{:name "Test Lighting Group"
                                                    :groupid groupid2}]
                                          :nodes (mapv #(do {:nodeid %})
                                                       [nodeid3
                                                        nodeid4])))})
                     (-> (apply-etdh-groups orgid
                                            siteid
                                            etdhprofileid2
                                            [groupid2])
                         vectors-to-sets)))
              (confirm-single-lg-assignment orgid siteid
                                            etdhprofile2
                                            cape-group
                                            nodeids nodes)))
          (testing "updateGroup"
            (let [[orgid
                   siteid
                   site-lighting-group] (org-site-slg)
                  [nodeid1
                   nodeid2
                   nodeid3
                   nodeid4
                   :as nodeids] (repeatedly 4
                                            #(create-test-node orgid
                                                               siteid))
                  groupid1 (create-test-lighting-group orgid
                                                       siteid
                                                       {:nodeList [nodeid2]})
                  groupid2 (create-test-lighting-group orgid
                                                       siteid
                                                       {:nodeList [nodeid3
                                                                   nodeid4]})
                  cape-group (get-group orgid siteid groupid2)
                  nodes (->> nodeids
                             (map #(get-node orgid siteid %))
                             doall)
                  [{etdhprofileid1 :etdhprofileid
                    etdhprofile-name1 :name
                    :as etdhprofile1}
                   {etdhprofileid2 :etdhprofileid
                    etdhprofile-name2 :name
                    :as etdhprofile2}] (->> (repeatedly #(gen/generate (s/gen :etdh/etdhprofile)))
                                            (map #(dissoc % :etdhprofileid))
                                            distinct
                                            (map #(assoc %
                                                         :etdhprofileid (create-test-etdhprofile orgid siteid %))))]
              (is (= (assoc-in cape-group
                               [:group
                                :etdhprofiles] [{:etdhprofileid etdhprofileid1
                                                 :name etdhprofile-name1}])
                     (run {:type "updateGroup"
                           :user *user*
                           :orgprops {:orgid orgid}
                           :siteprops {:siteid siteid}
                           :groupprops (-> cape-group
                                           :group
                                           (assoc :etdhprofiles [{:etdhprofileid etdhprofileid1}]))})))
              (confirm-single-lg-assignment orgid siteid
                                            etdhprofile1
                                            cape-group
                                            nodeids nodes)

              (is (= (assoc-in cape-group
                               [:group
                                :etdhprofiles] [{:etdhprofileid etdhprofileid2
                                                 :name etdhprofile-name2}])
                     (run {:type "updateGroup"
                           :user *user*
                           :orgprops {:orgid orgid}
                           :siteprops {:siteid siteid}
                           :groupprops (-> cape-group
                                           :group
                                           (assoc :etdhprofiles [{:etdhprofileid etdhprofileid2}]))})))
              (confirm-single-lg-assignment orgid siteid
                                            etdhprofile2
                                            cape-group
                                            nodeids nodes)))))
      (testing "multiple LightingGroups"
        (testing "via"
          (testing "applyETDHtoGroup"
            (let [[orgid
                   siteid
                   site-lighting-group] (org-site-slg)
                  [nodeid1
                   nodeid2
                   nodeid3
                   nodeid4
                   :as nodeids] (repeatedly 4
                                            #(create-test-node orgid
                                                               siteid))
                  groupids (mapv #(create-test-lighting-group orgid
                                                              siteid
                                                              {:nodeList %})
                                 [[nodeid2]
                                  [nodeid3
                                   nodeid4]])
                  cape-groups (mapv #(get-group orgid siteid %)
                                    groupids)
                  nodes (->> nodeids
                             (map #(get-node orgid siteid %))
                             doall)
                  [{etdhprofileid1 :etdhprofileid
                    :as etdhprofile1}
                   {etdhprofileid2 :etdhprofileid
                    :as etdhprofile2}] (->> (repeatedly #(gen/generate (s/gen :etdh/etdhprofile)))
                                            (map #(dissoc % :etdhprofileid))
                                            distinct
                                            (map #(assoc %
                                                         :etdhprofileid (create-test-etdhprofile orgid siteid %))))]
              (is (= (vectors-to-sets
                      {:success true
                       :result (-> etdhprofile1
                                   sut/with-etdhprofile-defaults
                                   (assoc :etdhprofileid etdhprofileid1
                                          :sites []
                                          :groups (mapv #(do {:name "Test Lighting Group"
                                                              :groupid %})
                                                        groupids)
                                          :nodes (mapv #(do {:nodeid %})
                                                       [nodeid2
                                                        nodeid3
                                                        nodeid4])))})
                     (-> (apply-etdh-groups orgid
                                            siteid
                                            etdhprofileid1
                                            groupids)
                         vectors-to-sets)))
              (confirm-multiple-lg-assignment orgid siteid
                                              etdhprofile1
                                              groupids cape-groups
                                              nodeids nodes)

              (is (= (vectors-to-sets
                      {:success true
                       :result (-> etdhprofile2
                                   sut/with-etdhprofile-defaults
                                   (assoc :etdhprofileid etdhprofileid2
                                          :sites []
                                          :groups (mapv #(do {:name "Test Lighting Group"
                                                              :groupid %})
                                                        groupids)
                                          :nodes (mapv #(do {:nodeid %})
                                                       [nodeid2
                                                        nodeid3
                                                        nodeid4])))})
                     (-> (apply-etdh-groups orgid
                                            siteid
                                            etdhprofileid2
                                            groupids)
                         vectors-to-sets)))
              (confirm-multiple-lg-assignment orgid siteid
                                              etdhprofile2
                                              groupids cape-groups
                                              nodeids nodes)))))))
  (testing "Deleting assignment of a ETDH Profile to LG(s)"
    (testing "on"
      (testing "SiteLightingGroup"
        (testing "updateGroup"
          (let [[orgid siteid site-lighting-group] (org-site-slg)
                [nodeid1
                 nodeid2
                 nodeid3
                 nodeid4
                 :as nodeids] (repeatedly 4
                                          #(create-test-node orgid
                                                             siteid))
                groupid1 (create-test-lighting-group orgid
                                                     siteid
                                                     {:nodeList [nodeid2]})
                groupid2 (create-test-lighting-group orgid
                                                     siteid
                                                     {:nodeList [nodeid3
                                                                 nodeid4]})
                nodes (->> nodeids
                           (map #(get-node orgid siteid %))
                           doall)
                {etdhprofile-name :name
                 :as etdhprofile} (gen/generate (s/gen :etdh/etdhprofile))
                etdhprofileid (create-test-etdhprofile orgid siteid etdhprofile)
                cape-group (get-group orgid siteid site-lighting-group)]
            (is (= (assoc-in cape-group
                             [:group
                              :etdhprofiles] [{:etdhprofileid etdhprofileid
                                               :name etdhprofile-name}])
                   (run {:type "updateGroup"
                         :user *user*
                         :orgprops {:orgid orgid}
                         :siteprops {:siteid siteid}
                         :groupprops (-> cape-group
                                         :group
                                         (assoc :etdhprofiles [{:etdhprofileid etdhprofileid}]))})))
            (confirm-slg-assignment orgid siteid
                                    cape-group
                                    etdhprofile
                                    nodeids nodes)

            (is (= cape-group
                   (run {:type "updateGroup"
                         :user *user*
                         :orgprops {:orgid orgid}
                         :siteprops {:siteid siteid}
                         :groupprops (:group cape-group)})))
            ;; TODO: Fix NSN-6353, NSN-6354
            #_(is (= {:success true
                      :etdhprofile (-> etdhprofile
                                       sut/with-etdhprofile-defaults
                                       (assoc :sites []
                                              :groups []
                                              :nodes [])
                                       vectors-to-sets)}
                     (vectors-to-sets (get-etdhprofile orgid siteid etdhprofileid))))
            (is (= cape-group
                   (get-group orgid siteid site-lighting-group)))
            ;; TODO: Fix NSN-6353, NSN-6354
            #_(doseq [{:as node
                       {:keys [nodeid]} :node} nodes]
                (is (= node
                       (get-node orgid siteid nodeid)))))))
      (testing "a LightingGroup"
        (testing "updateGroup"
          (let [[orgid
                 siteid
                 site-lighting-group] (org-site-slg)
                [nodeid1
                 nodeid2
                 nodeid3
                 nodeid4
                 :as nodeids] (repeatedly 4
                                          #(create-test-node orgid
                                                             siteid))
                groupid1 (create-test-lighting-group orgid
                                                     siteid
                                                     {:nodeList [nodeid2]})
                groupid2 (create-test-lighting-group orgid
                                                     siteid
                                                     {:nodeList [nodeid3
                                                                 nodeid4]})
                cape-group (get-group orgid siteid groupid2)
                nodes (->> nodeids
                           (map #(get-node orgid siteid %))
                           doall)
                {etdhprofile-name :name
                 :as etdhprofile} (gen/generate (s/gen :etdh/etdhprofile))
                etdhprofileid (create-test-etdhprofile orgid siteid etdhprofile)]
            (is (= (assoc-in cape-group
                             [:group
                              :etdhprofiles] [{:etdhprofileid etdhprofileid
                                               :name etdhprofile-name}])
                   (run {:type "updateGroup"
                         :user *user*
                         :orgprops {:orgid orgid}
                         :siteprops {:siteid siteid}
                         :groupprops (-> cape-group
                                         :group
                                         (assoc :etdhprofiles [{:etdhprofileid etdhprofileid}]))})))
            (confirm-single-lg-assignment orgid siteid
                                          etdhprofile
                                          cape-group
                                          nodeids nodes)

            (is (= cape-group
                   (run {:type "updateGroup"
                         :user *user*
                         :orgprops {:orgid orgid}
                         :siteprops {:siteid siteid}
                         :groupprops (:group cape-group)})))
            (is (= {:success true
                    :etdhprofile (-> etdhprofile
                                     sut/with-etdhprofile-defaults
                                     (assoc :sites []
                                            :groups []
                                            :nodes [])
                                     vectors-to-sets)}
                   (vectors-to-sets (get-etdhprofile orgid siteid etdhprofileid))))
            (is (= cape-group
                   (get-group orgid siteid groupid2)))
            (doseq [{:as node
                     {:keys [nodeid]} :node} nodes]
              ;; TODO: Fix NSN-6353, NSN-6354
              #_(is (= node
                       (get-node orgid siteid nodeid))))))))))

(defn get-etdh-trigger-nodes
  [orgid
   siteid
   etdhprofileid]
  (run {:type "getAllETDHProfileTriggers"
        :user *user*
        :orgprops {:orgid orgid}
        :siteprops {:siteid siteid}
        :etdhprofileprops {:etdhprofileid etdhprofileid}}))

(defn remove-etdh-trigger-nodes
  [orgid
   siteid
   etdhprofileid
   nodeids]
  (run {:type "removeETDHProfileTriggers"
        :user *user*
        :orgprops {:orgid orgid}
        :siteprops {:siteid siteid}
        :etdhprofileprops {:etdhprofileid etdhprofileid}
        :nodeprops {:nodeids nodeids}}))

(defn add-etdh-trigger-nodes
  [orgid
   siteid
   etdhprofileid
   nodeids]
  (run {:type "addETDHProfileTriggers"
        :user *user*
        :orgprops {:orgid orgid}
        :siteprops {:siteid siteid}
        :etdhprofileprops {:etdhprofileid etdhprofileid}
        :nodeprops {:nodeids nodeids}}))

;; #3
(deftest set-trigger-nodes-test
  (let [orgid (create-test-org)
        siteid (create-test-site orgid)
        nodeids (repeatedly 7 #(create-test-node orgid
                                                 siteid))
        extra-nodeid (create-test-node orgid
                                       siteid)
        lg (create-test-lighting-group orgid
                                       siteid
                                       {:nodeList nodeids})
        etdhprofileid (create-test-etdhprofile orgid
                                               siteid)]
    (testing "Setup LG with ETDH"
      (let [{:keys [success]} (apply-etdh-groups orgid
                                                 siteid
                                                 etdhprofileid
                                                 [lg])]
        (is (= true
               success)))
      (let [{:keys [success
                    items]} (get-etdh-trigger-nodes orgid
                                                    siteid
                                                    etdhprofileid)]
        (is (= true
               success))
        (is (= (->> nodeids
                    (map #(-> default-test-node
                              (select-keys [:latitude
                                            :longitude])
                              (assoc :nodeid %)))
                    set)
               (set items)))))
    (testing "Adding empty set ETDH Triggers"
      (let [{:keys [success]} (add-etdh-trigger-nodes orgid
                                                      siteid
                                                      etdhprofileid
                                                      [])]
        (is (= true
               success)))
      (let [{:keys [success
                    items]} (get-etdh-trigger-nodes orgid
                                                    siteid
                                                    etdhprofileid)]
        (is (= true
               success))
        (is (= (->> nodeids
                    (map #(-> default-test-node
                              (select-keys [:latitude
                                            :longitude])
                              (assoc :nodeid %)))
                    set)
               (set items)))))
    (testing "Removing empty set ETDH Triggers"
      (let [{:keys [success]} (remove-etdh-trigger-nodes orgid
                                                         siteid
                                                         etdhprofileid
                                                         [])]
        (is (= true
               success)))
      (let [{:keys [success
                    items]} (get-etdh-trigger-nodes orgid
                                                    siteid
                                                    etdhprofileid)]
        (is (= true
               success))
        (is (= (->> nodeids
                    (map #(-> default-test-node
                              (select-keys [:latitude
                                            :longitude])
                              (assoc :nodeid %)))
                    set)
               (set items)))))
    (testing "Add a Node to LG"
      (let [{:keys [success]} (run {:type "addNodeToGroup"
                                    :user *user*
                                    :orgprops  {:orgid orgid}
                                    :siteprops {:siteid siteid}
                                    :groupprops {:groupid lg}
                                    :nodeprops {:nodeid extra-nodeid}})]
        (is (= true
               success)))
      (let [{:keys [success
                    items]} (get-etdh-trigger-nodes orgid
                                                    siteid
                                                    etdhprofileid)]
        (is (= true
               success))
        (is (= (->> (conj nodeids extra-nodeid)
                    (map #(-> default-test-node
                              (select-keys [:latitude
                                            :longitude])
                              (assoc :nodeid %)))
                    set)
               (set items)))))
    (testing "Remove a Node from LG"
      (let [{:keys [success]} (run {:type "removeNodeFromGroup"
                                    :user *user*
                                    :orgprops  {:orgid orgid}
                                    :siteprops {:siteid siteid}
                                    :groupprops {:groupid lg}
                                    :nodeprops {:nodeid extra-nodeid}})]
        (is (= true
               success)))
      (let [{:keys [success
                    items]} (get-etdh-trigger-nodes orgid
                                                    siteid
                                                    etdhprofileid)]
        (is (= true
               success))
        (is (= (->> nodeids
                    (map #(-> default-test-node
                              (select-keys [:latitude
                                            :longitude])
                              (assoc :nodeid %)))
                    set)
               (set items)))))
    (testing "Remove ETDH Triggers"
      (let [{:keys [success]} (remove-etdh-trigger-nodes orgid
                                                         siteid
                                                         etdhprofileid
                                                         (take 5 nodeids))]
        (is (= true
               success)))
      (let [{:keys [success
                    items]} (get-etdh-trigger-nodes orgid
                                                    siteid
                                                    etdhprofileid)]
        (is (= true
               success))
        (is (= (->> nodeids
                    (drop 5)
                    (map #(-> default-test-node
                              (select-keys [:latitude
                                            :longitude])
                              (assoc :nodeid %)))
                    set)
               (set items)))))
    (testing "Add ETDH Triggers"
      (let [{:keys [success]} (add-etdh-trigger-nodes orgid
                                                      siteid
                                                      etdhprofileid
                                                      (take 3 nodeids))]
        (is (= true
               success)))
      (let [{:keys [success
                    items]} (get-etdh-trigger-nodes orgid
                                                    siteid
                                                    etdhprofileid)]
        (is (= true
               success))
        (is (= (->> (concat (take 3 nodeids)
                            (take-last 2 nodeids))
                    (map #(-> default-test-node
                              (select-keys [:latitude
                                            :longitude])
                              (assoc :nodeid %)))
                    set)
               (set items)))))))

;; #5

(deftest assign-nodes-to-edth-is-idempotent
  (let [orgid (create-test-org)
        siteid (create-test-site orgid)
        etdhprofileid (create-test-etdhprofile orgid siteid)
        nodeid (create-test-node orgid siteid)
        lg (create-test-lighting-group orgid siteid)]
    (let [{:keys [success]
           :as ret} (run {:type "applyETDHtoGroup"
                          :user *user*
                          :orgprops {:orgid orgid}
                          :siteprops {:siteid siteid}
                          :etdhprofileprops {:etdhprofileid etdhprofileid}
                          :groupprops {:groupids [lg]}})]
      (is (= true
             success)))
    (let [{:keys [success
                  group]} (get-group orgid siteid lg)]
      (is (= true
             success))
      (let [{:keys [success]
             :as ret} (run {:type "updateGroup"
                            :user *user*
                            :orgprops {:orgid orgid}
                            :siteprops {:siteid siteid}
                            :groupprops (update group
                                                :nodeList
                                                conj nodeid)})]
        (is (= true
               success))))
    (let [{:keys [success
                  group]} (get-group orgid siteid lg)]
      (is (= true
             success))
      (let [{:keys [success]
             :as ret} (run {:type "updateGroup"
                            :user *user*
                            :orgprops {:orgid orgid}
                            :siteprops {:siteid siteid}
                            :groupprops group})]
        (is (= true
               success))))))

(deftest deduplicate-profile-scheduled-test
  (let [orgid (create-test-org)
        siteid (create-test-site orgid)
        schedule-window {:beginTime "00:00:00"
                         :endTime "00:00:00"}
        etdhprofileid (create-test-etdhprofile orgid
                                               siteid
                                               {:scheduled (repeat (inc etdh-spec/max-schedule-windows)
                                                                   schedule-window)})]
    (let [{:keys [success]
           {:keys [scheduled]} :etdhprofile
           :as foo} (get-etdhprofile orgid siteid etdhprofileid)]
      (is (= true
             success))
      (is (= [schedule-window]
             scheduled)))))
