(ns utils.neo4j-fixture
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.tools.logging :refer :all]
            [dealer.core :as core]
            [environ.core :as environ]
            [neowrap.neowrapper :as neo4j]
            [utils.config :as config])
  (:import [com.sensity.netsense.neo CypherEmbeddedImpl]))

(def fixture-map
  {:test "test/testdata.cypher"
   :jstest "test/jstestdata.cypher"})

(defn safe-delete [path]
  (if (.exists (io/file path))
    (try
      (io/delete-file path)
      (catch Exception e (str "exception: " (.getMessage e))))
    false))

(defn delete-directory [path]
  (let [contents (file-seq (io/file path))
        files-to-delete (filter #(.isFile %) contents)]
    (doseq [file files-to-delete]
      (safe-delete (.getPath file)))
    (safe-delete path)))

(defn delete-recursively [path]
  (let [func (fn [func f]
               (when (.isDirectory f)
                 (doseq [f2 (.listFiles f)]
                   (func func f2)))
               (io/delete-file f))]
    (func func (io/file path))))

(def delete-all-query "MATCH (n) DETACH DELETE n;")

(def delete-all-constraints
 ["DROP CONSTRAINT ON (org:Org) ASSERT org.orgid IS UNIQUE;"
  "DROP CONSTRAINT ON (site:Site) ASSERT site.siteid IS UNIQUE;"
  "DROP CONSTRAINT ON (node:Node) ASSERT node.nodeid IS UNIQUE;"
  "DROP CONSTRAINT ON (f:Fixture) ASSERT f.fixtureid IS UNIQUE;"
  "DROP CONSTRAINT ON (c:Config) ASSERT c.configid IS UNIQUE;"
  "DROP CONSTRAINT ON (g:Group) ASSERT g.groupid is UNIQUE;"
  "DROP CONSTRAINT ON (fw:Firmware) ASSERT fw.firmwareid is UNIQUE;"
  "DROP CONSTRAINT ON (sch:Schedule) ASSERT sch.scheduleid is UNIQUE;"
  "DROP CONSTRAINT ON (u:User) ASSERT u.userid IS UNIQUE;"
  "DROP CONSTRAINT ON (u:User) ASSERT u.email IS UNIQUE;"
  "DROP CONSTRAINT ON (r:Role) ASSERT r.rolename IS UNIQUE;"
  "DROP CONSTRAINT ON (pd:PDProfile) ASSERT pd.pdprofileid IS UNIQUE;"
  "DROP CONSTRAINT ON (dh:DHProfile) ASSERT dh.dhprofileid IS UNIQUE;"
  "DROP CONSTRAINT ON (etdh:ETDHProfile) ASSERT etdh.etdhprofileid IS UNIQUE;"
  "DROP CONSTRAINT ON (a:Alert) ASSERT a.alertid IS UNIQUE;"
  "DROP CONSTRAINT ON (sm:SiteModel) ASSERT sm.smid IS UNIQUE;"
  "DROP CONSTRAINT ON (nm:NodeModel) ASSERT nm.nmid IS UNIQUE;"
  "DROP CONSTRAINT ON (cm:ConfigModel) ASSERT cm.cmid IS UNIQUE;"
  "DROP CONSTRAINT ON (um:UserModel) ASSERT um.umid IS UNIQUE;"
  "DROP CONSTRAINT ON (om:OrgModel) ASSERT om.omid IS UNIQUE;"
  "DROP CONSTRAINT ON (olm:OverlayModel) ASSERT olm.olmid IS UNIQUE;"
  "DROP CONSTRAINT ON (fwm:FirmwareModel) ASSERT fwm.fwmid IS UNIQUE;"
  "DROP CONSTRAINT ON (am:AuditModel) ASSERT am.amid IS UNIQUE;"
  "DROP CONSTRAINT ON (fm:FixtureModel) ASSERT fm.fmid IS UNIQUE;"
  "DROP CONSTRAINT ON (nfm:NotificationModel) ASSERT nfm.nfmid IS UNIQUE;"
  "DROP CONSTRAINT ON (pz:ParkingZoneModel) ASSERT pz.parkingzoneid IS UNIQUE;"
  "DROP CONSTRAINT ON (tom:TrafficObjectModel) ASSERT tom.tomid is UNIQUE;"
  "DROP CONSTRAINT ON (alm:AlertModel) ASSERT alm.almid IS UNIQUE;"
  "DROP CONSTRAINT ON (gm:GroupModel) ASSERT gm.gmid IS UNIQUE;"
  "DROP CONSTRAINT ON (scm:ScheduleModel) ASSERT scm.scmid is UNIQUE;"
  "DROP CONSTRAINT ON (pm:PartnerModel) ASSERT pm.pmid is UNIQUE;"
  "DROP CONSTRAINT ON (rm:ReportModel) ASSERT rm.rmid is UNIQUE;"
  "DROP CONSTRAINT ON (lcm:LicenseModel) ASSERT lcm.lcmid is UNIQUE;"
  "DROP CONSTRAINT ON (ppm:ParkingPolicyModel) ASSERT ppm.ppmid is UNIQUE;"
  "DROP CONSTRAINT ON (pcm:PolicyCategoryModel) ASSERT pcm.pcmid is UNIQUE;"
  "DROP CONSTRAINT ON (pgm:ParkingGroupModel) ASSERT pgm.pgmid is UNIQUE;"
  "DROP CONSTRAINT ON (srm:SummaryReportModel) ASSERT srm.srmid is UNIQUE;"])

(def db-baseline-suffix "-baseline")
(defn- create-baseline-path
  "Takes a path (e.g., path/to/db/) and appends the `db-baseline-suffix` (e.g., path/to/db-baseline/)"
  [s]
  (let [slash java.io.File/separator]
    (if (.endsWith s slash)
      (str (apply str (butlast s)) db-baseline-suffix)
      (str s db-baseline-suffix))))

(def db-path (:dbpath (config/neo4jservice)))
(def db-baseline-path (create-baseline-path db-path))

(def cypherBaseline (delay (CypherEmbeddedImpl. db-baseline-path)))
(def db-baseline-created? (atom false))

(defn delete-neo4j-fixture []
  (neo4j/executeQuery delete-all-query nil cypherBaseline)
  (doall (map (fn [query] (neo4j/executeQuery query nil cypherBaseline)) delete-all-constraints)))


(defn create-db-baseline []
  (when (not @db-baseline-created?)
    (let [env (:neo4j-fixture environ/env
                                 :test)
          filename (or (get fixture-map env)
                       (:test fixture-map))
          cypher (slurp filename)
          lines (string/split cypher #"\n")
          filtered (filter #(not (or (.startsWith % "//") (string/blank? %))) lines)
          joined (string/join " " filtered)
          queries (string/split joined #";")]
      (delete-neo4j-fixture)
      (doall
        (map (fn [query]
               (neo4j/executeQuery query nil cypherBaseline))
             queries))
      (reset! db-baseline-created? true))))

(defn neo4j-fixture
  ([f]
   (do
     (spyf :info "Cleaning DATABASE resource %s" neo4j/db-path)
     (create-db-baseline)
     (neo4j/stop)
     (spyf :info "Using DATABASE resource %s" neo4j/db-path)
     (neo4j/start db-baseline-path db-path)
     (f))))
