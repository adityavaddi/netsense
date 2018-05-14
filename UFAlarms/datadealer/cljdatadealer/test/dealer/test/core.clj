(ns dealer.test.core
  (:require [clojure
             [string :as string]
             [test :refer :all]]
            [clojure.data.json :as json]
            [clojure.tools.logging :refer :all]
            [mqtt.fixture :as mqtt]
            [utils
             [cape :as cape]
             [cassandra_fixture :as cass]
             [neo4j-fixture :as neo4j]]))

(use-fixtures :once (join-fixtures [cass/cassandra-fixture
                                    neo4j/neo4j-fixture]))

(with-test
  (defn get_all_nodes4site[]
    (let [x "{'type': 'getAllNodesForSite', 'user': 'uberuser', 'siteprops': {'siteid':'ubersite'}}"]
      (try (cape/executer-main (json/read-str (string/replace x #"'" "\""))) ;;jsonify :)
        (catch Exception e
          (warn (str "Exception in main: " (.getMessage e)))
          ;; (.printStackTrace e)
          ))))
  (is (not (nil? (get_all_nodes4site))))
  (debug (str "Get All Nodes for Site: " (json/write-str(get_all_nodes4site)))))

(with-test
  (defn get_all_lost_found_nodes[]
    (let [x "{'type': 'getAllLostAndFoundNodes', 'user': 'uberuser'}"]
      (try (cape/executer-main (json/read-str (string/replace x #"'" "\""))) ;;jsonify :)
        (catch Exception e
          (warn (str "Exception in main: " (.getMessage e)))
          ;; (.printStackTrace e)
          ))))
  (is (not (nil? (get_all_lost_found_nodes))))
  (debug (str "Get All lost and found Nodes: " (json/write-str(get_all_lost_found_nodes)))))

(with-test
  (defn get_all_orgs []
    (let [x "{'type': 'getAllOrgs', 'user': 'uberuser'}"]
      (try (cape/executer-main (json/read-str (string/replace x #"'" "\"")))
           (catch Exception e
             (warn (str "Exception in main: " (.getMessage e)))
             ))))
  (is (not (nil? (get_all_orgs))))
  (debug (str "Get All Orgs: " (json/write-str (get_all_orgs)))))

(with-test
  (defn get_all_sites4org []
    (let [x "{'type': 'getAllSitesForOrg', 'user': 'uberuser', 'orgprops': {'orgid': 'uberorg'}}"]
      (try (cape/executer-main (json/read-str (string/replace x #"'" "\"")))
           (catch Exception e
             (warn (str "Exception in main: " (.getMessage e)))
             ))))
  (is (not (nil? (get_all_sites4org))))
  (debug (str "Get All Sites for Org: " (json/write-str (get_all_sites4org)))))


;(with-test
;  (defn create_org []
;    (let [x "{'type': 'createOrg', 'user': 'uberuser', 'orgprops': {'orgid': 'uberorg', 'name': 'Uber'}}"]
;      (try (cape/executer-main (json/read-str (string/replace x #"'" "\"")))
;           (catch Exception e
;             (warn (str "Exception in main: " (.getMessage e)))
;             ))))
;  (is (not (nil? (create_org))))
;  (println (str "Create Org: " (json/write-str (create_org)))))

(with-test
  (defn get_org []
    (let [x "{'type': 'getOrg', 'user': 'uberuser', 'orgprops': {'orgid': 'uberorg'}}"]
      (try (cape/executer-main (json/read-str (string/replace x #"'" "\"")))
           (catch Exception e
             (warn (str "Exception in main: " (.getMessage e)))
             ))))
  (is (not (nil? (get_org))))
  (debug (str "Get Org: " (json/write-str (get_org)))))

(with-test
  (defn get_site []
    (let [x "{'type': 'getSite', 'user': 'uberuser', 'siteprops': {'siteid': 'ubersite'}}"]
      (try (cape/executer-main (json/read-str (string/replace x #"'" "\"")))
           (catch Exception e
             (warn (str "Exception in main: " (.getMessage e)))
             ))))
  (is (not (nil? (get_site))))
  (debug (str "Get Site: " (json/write-str (get_site)))))

(with-test
  (defn get_node []
    (let [x "{'type': 'getNode', 'user': 'uberuser', 'nodeprops': {'nodeid': 'ubernode'}}"]
      (try (cape/executer-main (json/read-str (string/replace x #"'" "\"")))
           (catch Exception e
             (warn (str "Exception in main: " (.getMessage e)))
             ))))
  (is (not (nil? (get_node))))
  (debug (str "Get Node: " (json/write-str (get_node)))))

(with-test
  (defn create_fixture []
    (let [x "{'type': 'createFixture', 'user': 'uberuser', 'fixtureprops': {'fixtureid': 'uberfixture', 'name': 'Lamp'}, 'siteprops': {'siteid':'ubersite'}}"]
      (try (cape/executer-main (json/read-str (string/replace x #"'" "\"")))
           (catch Exception e
             (warn (str "Exception in main: " (.getMessage e)))
             ))))
  (is (not (nil? (create_fixture))))
  (debug (str "Create Fixture: " (json/write-str (create_fixture)))))

(with-test
  (defn create_empty_node []
    (let [x "{'type': 'createEmptyNode', 'user': 'uberuser', 'nodeprops': {'nodeid': 'dragansnode', 'model': 'unode-v2'}}"]
      (try (cape/executer-main (json/read-str (string/replace x #"'" "\"")))
           (catch Exception e
             (warn (str "Exception in main: " (.getMessage e)))
             ))))
  (is (not (nil? (create_empty_node))))
  (debug (str "Create Empty Node: " (json/write-str (create_empty_node)))))

(with-test
  (defn get_fixture []
    (let [x "{'type': 'getFixture', 'user': 'uberuser', 'fixtureprops': {'fixtureid': 'uberfixture'}}"]
      (try (cape/executer-main (json/read-str (string/replace x #"'" "\"")))
           (catch Exception e
             (warn (str "Exception in main: " (.getMessage e)))
             ))))
  (is (not (nil? (get_fixture))))
  (debug (str "Get Fixture: " (json/write-str (get_fixture)))))

(with-test
  (defn get_all_fixtures4site[]
    (let [x "{'type': 'getAllFixtures', 'user': 'uberuser', 'siteprops': {'siteid':'ubersite'}}"]
      (try (cape/executer-main (json/read-str (string/replace x #"'" "\""))) ;;jsonify :)
        (catch Exception e
          (warn (str "Exception in main: " (.getMessage e)))
          ;; (.printStackTrace e)
          ))))
  (is (not (nil? (get_all_fixtures4site))))
  (debug (str "Get All Fixtures for Site: " (json/write-str(get_all_fixtures4site)))))

(with-test
  (defn create_group []
    (let [x "{'type': 'createGroup', 'user': 'uberuser', 'groupprops': {'groupid': 'ubergroup', 'name': 'Uber Group', 'nodeList': '1,2,3', 'type': 'organizational'}, 'siteprops': {'siteid':'ubersite'}, 'orgprops': {'orgid':'uberorg'}}"]
      (try (cape/executer-main (json/read-str (string/replace x #"'" "\"")))
           (catch Exception e
             (warn (str "Exception in main: " (.getMessage e)))
             ))))
  (is (not (nil? (create_group))))
  (debug (str "Create Group: " (json/write-str (create_group)))))

(with-test
  (defn get_group []
    (let [x "{'type': 'getGroup', 'user': 'uberuser', 'groupprops': {'groupid': 'ubergroup'}}"]
      (try (cape/executer-main (json/read-str (string/replace x #"'" "\"")))
           (catch Exception e
             (warn (str "Exception in main: " (.getMessage e)))
             ))))
  (is (not (nil? (get_group))))
  (debug (str "Get Group: " (json/write-str (get_group)))))

(with-test
  (defn addNodeToGroup []
    (let [x "{'type': 'addNodeToGroup', 'user': 'uberuser', 'groupprops': {'groupid':'ubergroup'}, 'orgprops': {'orgid': 'uberorg'}, 'siteprops': {'siteid':'ubersite'}, 'nodeprops': {'nodeid': 'ubernode'}}"]
      (try (cape/executer-main (json/read-str (string/replace x #"'" "\"")))
           (catch Exception e
             (warn (str "Exception in main: " (.getMessage e)))
             ))))
  ;; (is (not (nil? (addNodeToGroup))))
  (debug (str "addNodeToGroup: " (json/write-str (addNodeToGroup)))))

(with-test
  (defn removeNodeFromGroup []
    (let [x "{'type': 'removeNodeFromGroup', 'user': 'uberuser', 'groupprops': {'groupid':'ubergroup'}, 'orgprops': {'orgid': 'uberorg'}, 'siteprops': {'siteid':'ubersite'}, 'nodeprops': {'nodeid': 'ubernode'}}"]
      (try (cape/executer-main (json/read-str (string/replace x #"'" "\"")))
           (catch Exception e
             (warn (str "Exception in main: " (.getMessage e)))
             ))))
  ;; (is (not (nil? (removeNodeFromGroup))))
  (debug (str "removeNodeFromGroup: " (json/write-str (removeNodeFromGroup)))))

(with-test
  (defn get_all_groups4site[]
    (let [x "{'type': 'getAllGroups', 'user': 'uberuser', 'siteprops': {'siteid':'ubersite'}}"]
      (try (cape/executer-main (json/read-str (string/replace x #"'" "\""))) ;;jsonify :)
        (catch Exception e
          (warn (str "Exception in main: " (.getMessage e)))
          ;; (.printStackTrace e)
          ))))
  (is (not (nil? (get_all_groups4site))))
  (debug (str "Get All Groups for Site: " (json/write-str(get_all_groups4site)))))

(with-test
  (defn create_overlay []
    (let [x "{'type': 'createOverlay', 'user': 'uberuser', 'overlayprops': {'overlayid': 'uberoverlay', 'buildingLevel': '1', 'imageBounds': '30x30', 'imageData': 'foo'}, 'siteprops': {'siteid':'ubersite'}}"]
      (try (cape/executer-main (json/read-str (string/replace x #"'" "\"")))
           (catch Exception e
             (warn (str "Exception in main: " (.getMessage e)))
             ))))
  (is (not (nil? (create_overlay))))
  (debug (str "Create Overlay: " (json/write-str (create_overlay)))))

(with-test
  (defn get_overlay []
    (let [x "{'type': 'getOverlay', 'user': 'uberuser', 'overlayprops': {'overlayid': 'uberoverlay'}}"]
      (try (cape/executer-main (json/read-str (string/replace x #"'" "\"")))
           (catch Exception e
             (warn (str "Exception in main: " (.getMessage e)))
             ))))
  (is (not (nil? (get_overlay))))
  (debug (str "Get Overlay: " (json/write-str (get_overlay)))))

(with-test
  (defn get_all_overlays4site[]
    (let [x "{'type': 'getAllOverlays', 'user': 'uberuser', 'siteprops': {'siteid':'ubersite'}}"]
      (try (cape/executer-main (json/read-str (string/replace x #"'" "\""))) ;;jsonify :)
        (catch Exception e
          (warn (str "Exception in main: " (.getMessage e)))
          ;; (.printStackTrace e)
          ))))
  (is (not (nil? (get_all_overlays4site))))
  (debug (str "Get All Overlays for Site: " (json/write-str(get_all_overlays4site)))))

;(with-test
;  (defn autoComplete []
;    (let [x "{'type': 'autoComplete', 'user': 'uberuser', 'autoprops': {'query': 'Uber', 'categories': 'Site'}}"]
;      (try (cape/executer-main (json/read-str (string/replace x #"'" "\"")))
;           (catch Exception e
;             (warn (str "Exception in main: " (.getMessage e)))
;             ))))
;  (is (not (nil? (autoComplete))))
;  (debug (str "Autocomplete: " (json/write-str (autoComplete)))))

(with-test
  (defn getUserPermissions []
    (let [x {:type "getUserPermissions"
             :email "uberuser@sensity.com"
             :user "uberuser@sensity.com"}]
      (try (cape/executer-main (clojure.walk/stringify-keys x))
           (catch Exception e
             (warn (str "Exception in main: " (.getMessage e)))
             ))))
  (is (true? (get (json/read-str (getUserPermissions))
                  "success")))
  ;; (debug (str "getUserPermissions: " (json/write-str (getUserPermissions))))
  )

(with-test
  (defn lighting-control []
    (let [x "{'type': 'lighting-control', 'user':'uberuser', 'nodeprops': {type: 'LightingForceState', 'level':'50', 'nodeid': 'ubernode'}, 'siteprops': {'siteid':'ubersite'}, 'orgprops': {'orgid': 'uberorg'}}"]
      (try (cape/executer-main (json/read-str (string/replace x #"'" "\"")))
           (catch Exception e
             (warn (str "Exception in main: " (.getMessage e)))
             ))))
  ;; (is (not (nil? (lighting-control))))
  (debug (str "lighting-control: " (json/write-str (lighting-control)))))

(with-test
  (defn lighting-control-site []
    (let [x "{'type': 'lighting-control-site', 'user':'uberuser', 'nodeprops': {type: 'LightingForceState', 'level':'50', 'nodeid': 'ubernode'}, 'siteprops': {'siteid':'ubersite'}, 'orgprops': {'orgid': 'uberorg'}}"]
      (try (cape/executer-main (json/read-str (string/replace x #"'" "\"")))
           (catch Exception e
             (warn (str "Exception in main: " (.getMessage e)))
             ))))
  ;; (is (not (nil? (lighting-control-site))))
  (debug (str "lighting-control-site: " (json/write-str (lighting-control-site)))))

(with-test
  (defn lighting-control-group []
    (let [x "{'type': 'lighting-control-group', 'user':'uberuser', 'nodeprops': {type: 'LightingForceState', 'level':'50', 'nodeid': 'ubernode'}, 'siteprops': {'siteid':'ubersite'}, 'orgprops': {'orgid': 'uberorg'},'groupprops':{'groupid':'ubergroup'}}"]
      (try (cape/executer-main (json/read-str (string/replace x #"'" "\"")))
           (catch Exception e
             (warn (str "Exception in main: " (.getMessage e)))
             ))))
  ;; (is (not (nil? (lighting-control-group))))
  (debug (str "lighting-control-group: " (json/write-str (lighting-control-group)))))
