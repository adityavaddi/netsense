(ns ^{:doc "The Query Builder."
      :author "Chiradip Mandal"}
    utils.propschecker (:gen-class)
    (:require [clojure.data.json :as json]
              [clojure.tools.logging :refer :all]
              [neowrap.neowrapper :as neo4j]
              [appl.et-daylight-harvesting :as etdh]
              [utils.schedules :as sch])
    (:use clj-time.format))

(defn unode_checkprops [nodeprops]
  ;; (println "unode_checkprops ...")
  ;; (println nodeprops)
  (let [err (str
              (if (nil? (:nodeid nodeprops)) "nodeProps.nodeid is required, also check the spelling - all lowercase for nodid")
              (if (nil? (:name nodeprops)) "nodeProps.name is required")
              (if (nil? (:model nodeprops)) "nodeprops.model is required"))]
    (if (= (count err) 0) true
      (do
        ;;(println err)
        (throw (ex-info (str "exception unode_checkprops: " err) {:cause :bad-request :status 400}))))))

;; why the above throw is not working?? working now :)
(defn site_checkprops [siteprops orgprops]
  (let [err (str
             (if (nil? (:siteid siteprops)) "siteprops.siteid is required, also check the spelling - all lowercase for siteid")
             (->> [:name
                   :latitude
                   :longitude]
                  (mapv #(when-not (% siteprops)
                           (str "siteprops." (name %) " is required")))
                  (apply str))
             (if (nil? (:orgid orgprops)) "orgprops.orgid is required"))]
    (when (pos? (count err))
      (throw (ex-info (str "exception site_checkprops: " err) {:cause :bad-request :status 400})))))

(defn site_checkprops_delete [siteprops orgprops]
  (let [err (str
              (if (nil? (:siteid siteprops)) "siteprops.siteid is required, also check the spelling - all lowercase for siteid")
              (if (nil? (:orgid orgprops)) "orgprops.orgid is required"))]
    (if (= (count err) 0) true
      (do
        ;;(println err)
        (throw (ex-info (str "exception site_checkprops: " err) {:cause :bad-request :status 400}))))))

(defn site_checkprops_suspend [siteprops orgprops]
  (let [err (str
              (if (nil? (:siteid siteprops)) "siteprops.siteid is required, also check the spelling - all lowercase for siteid")
              (if (nil? (:orgid orgprops)) "orgprops.orgid is required"))]
    (if (= (count err) 0) true
      (do
        ;;(println err)
        (throw (ex-info (str "exception site_checkprops: " err) {:cause :bad-request :status 400}))))))

(defn site_checkprops_activate [siteprops orgprops]
  (let [err (str
              (if (nil? (:siteid siteprops)) "siteprops.siteid is required, also check the spelling - all lowercase for siteid")
              (if (nil? (:orgid orgprops)) "orgprops.orgid is required"))]
    (if (= (count err) 0) true
      (do
        ;;(println err)
        (throw (ex-info (str "exception site_checkprops: " err) {:cause :bad-request :status 400}))))))

(defn org_checkprops [orgprops]
  (let [err (str
              (if (nil? (:orgid orgprops)) "orgProps.orgid is required, also check the spelling - all lowercase for orgid")
              (if (nil? (:name orgprops)) "orgProps.name is required"))]
    (if (= (count err) 0) true
      (do
        ;;(println err)
        (throw (ex-info (str "exception org_checkprops: " err) {:cause :bad-request :status 400}))))))

(defn add_user_checkprops [userprops orgprops]
  (let [cypher (format "MATCH(o:Role {rolename:\"%s\"}) RETURN o.rolename as name" (userprops :roles))
                results (->>
                          cypher
                          neo4j/executeQuery
                          json/read-str)
        err (str
              (if (nil? (:userid userprops)) "userprops.userid is required")
              (if (nil? (:email userprops)) "userprops.email is required")
              (if (nil? (:roles userprops)) "userprops.roles is required")
              (if (empty? results) "userprops.roles must contain valid role")
              (if (nil? (:orgid orgprops)) "orgprops.orgid is required"))]
    (if (= (count err) 0) true
      (do
        ;;(println err)
        ;(println (format "roles: %s" (userprops :roles)))
        (throw (ex-info (str "exception user_checkprops: " err) {:cause :bad-request :status 400}))))))

(defn user_checkprops [userprops orgprops]
  (let [err (str
              (if (nil? (:userid userprops)) "userprops.userid is required")
              (if (nil? (:orgid orgprops)) "orgprops.orgid is required"))]
    (if (= (count err) 0) true
      (do
        ;;(println err)
        (throw (ex-info (str "exception user_checkprops: " err) {:cause :bad-request :status 400}))))))

(defn user_email_checkprops [useremailprops]
  (let [err (str
              (if (nil? (:userid useremailprops)) "useremailprops.userid is required"))]
    (if (= (count err) 0) true
      (do
        ;;(println err)
        (throw (ex-info (str "exception user_email_checkprops: " err) {:cause :bad-request :status 400}))))))

(defn lighting_ctl_checkprops [nodeprops siteprops orgprops]
  (let [cypher (format "MATCH(o:Node {nodeid:\"%s\"}) RETURN o.nodeid as nodeid" (nodeprops :nodeid))
        results (->> cypher
                     neo4j/executeQuery
                     json/read-str)
        err (str
             (if (nil? (:type nodeprops)) "nodeprops.type is required")
             (if (nil? (:level nodeprops)) "nodeprops.level is required")
             (if (nil? (:nodeid nodeprops)) "nodeprops.nodeid is required")
             (if (empty? results) "nodeprops.nodeid must contain valid node id")
             (if (nil? (:orgid orgprops)) "orgprops.orgid is required")
             (if (nil? (:siteid siteprops)) "siteprops.siteid is required"))]
    (if (= (count err) 0) true
        (do
          ;;(println err)
          (throw (ex-info (str "exception lighting_ctl_checkprops: " err) {:cause :bad-request :status 400}))))))

(defn lighting_site_ctl_checkprops [nodeprops siteprops orgprops]
  (let [err (str
              (if (nil? (:type nodeprops)) "nodeprops.type is required")
              (if (nil? (:level nodeprops)) "nodeprops.level is required")
              (if (nil? (:orgid orgprops)) "orgprops.orgid is required")
              (if (nil? (:siteid siteprops)) "siteprops.siteid is required")
              )]
    (if (= (count err) 0) true
      (do
        ;;(println err)
        (throw (ex-info (str "exception lighting_site_ctl_checkprops: " err) {:cause :bad-request :status 400}))))))

(defn lighting_group_ctl_checkprops [nodeprops siteprops orgprops groupprops]
  (let [err (str
              (if (nil? (:type nodeprops)) "nodeprops.type is required")
              (if (nil? (:level nodeprops)) "nodeprops.level is required")
              (if (nil? (:orgid orgprops)) "orgprops.orgid is required")
              (if (nil? (:siteid siteprops)) "siteprops.siteid is required")
              (if (nil? (:groupid groupprops)) "groupprops.groupid is required")
              )]
    (if (= (count err) 0) true
      (do
        ;;(println err)
        (throw (ex-info (str "exception lighting_group_ctl_checkprops: " err) {:cause :bad-request :status 400}))))))

(defn node_checkprops [nodeprops siteprops orgprops]
  (let [err (str
              (if (clojure.string/blank? (:nodeid nodeprops)) "nodeprops.nodeid is required")
              (if (nil? (:model nodeprops)) "nodeprops.model is required")
              (if (nil? (:latitude nodeprops)) "nodeprops.latitude is required")
              (if (nil? (:longitude nodeprops)) "nodeprops.longitude is required")
              (if (nil? (:siteid siteprops)) "siteprops.siteid is required")
              (if (nil? (:orgid orgprops)) "orgprops.orgid is required"))]
    (if (= (count err) 0) true
      (do
        ;;(println err)
        (throw (ex-info (str "exception node_checkprops: " err) {:cause :bad-request :status 400}))))))

(defn update_node_checkprops [nodeprops siteprops orgprops]
  (let [err (str
             (if (clojure.string/blank? (:nodeid nodeprops)) "nodeprops.nodeid is required")
             (if (nil? (:latitude nodeprops)) "nodeprops.latitude is required")
             (if (nil? (:longitude nodeprops)) "nodeprops.longitude is required")
             (if (nil? (:siteid siteprops)) "siteprops.siteid is required")
             (if (nil? (:orgid orgprops)) "orgprops.orgid is required"))]
    (if (= (count err) 0) true
      (do
        ;;(println err)
        (throw (ex-info (str "exception update_node_checkprops: " err) {:cause :bad-request :status 400}))))))

(defn vnode_checkprops [nodeprops]
      (let [err (str
                  (if (clojure.string/blank? (:nodeid nodeprops)) "nodeprops.nodeid is required"))]
           (if (= (count err) 0) true
                                 (do
                                   ;;(println err)
                                   (throw (ex-info (str "exception node_checkprops: " err) {:cause :bad-request :status 400}))))))

(defn assign_node_checkprops [nodeprops siteprops orgprops]
  (let [err (str
              (if (nil? (:nodeid nodeprops)) "nodeprops.nodeid is required")
              (if (nil? (:siteid siteprops)) "siteprops.siteid is required")
              (if (nil? (:orgid orgprops)) "orgprops.orgid is required"))]
    (if (= (count err) 0) true
      (do
        ;;(println err)
        (throw (ex-info (str "exception assign_node_checkprops: " err) {:cause :bad-request :status 400}))))))

(defn empty_node_checkprops [nodeprops]
  (let [err (str
              (if (nil? (:nodeid nodeprops)) "nodeprops.nodeid is required")
              (if (nil? (:model nodeprops)) "nodeprops.model is required"))]
    (if (= (count err) 0) true
      (do
        ;;(println err)
        (throw (ex-info (str "exception empty_node_checkprops: " err) {:cause :bad-request :status 400}))))))

(defn overlay_checkprops [overlayprops siteprops]
  ;; (println "overlay_checkprops ...")
  ;; (println overlayprops)
  (let [err (str
              (if (nil? (:overlayid overlayprops)) "overlayProps.overlayid is required, also check the spelling - all lowercase for overlayid")
              (if (nil? (:buildingLevel overlayprops)) "overlayProps.buildingLevel is required")
              (if (nil? (:imageBounds overlayprops)) "overlayProps.imageBounds is required")
              (if (nil? (:imageData overlayprops)) "overlayProps.imageData is required")
              (if (nil? (:siteid siteprops)) "siteprops.siteid is required"))]
    (if (= (count err) 0) true
      (do
        ;;(println err)
        (throw (ex-info (str "exception overlay_checkprops: " err) {:cause :bad-request :status 400}))))))

(defn group_checkprops [groupprops siteprops orgprops]
  ;; (println "group_checkprops ...")
  ;; (println overlayprops)
  (let [err (str
              (if (nil? (:groupid groupprops)) "groupProps.groupid is required, also check the spelling - all lowercase for groupid")
              (if (nil? (:name groupprops)) "groupProps.name is required")
              (if (nil? (:nodeList groupprops)) "groupProps.nodeList is required")
              (if (nil? (:type groupprops)) "groupProps.type is required")
              (if-not (#{"organizational"
                         "lighting"
                         "site-lighting"}
                       (:type groupprops)) "groupProps.type must be a valid type")
              (if (nil? (:siteid siteprops)) "siteprops.siteid is required")
              (if (nil? (:orgid orgprops)) "orgprops.orgid is required"))]
    (if (= (count err) 0) true
      (do
        ;;(println err)
        (throw (ex-info (str "exception group_checkprops: " err) {:cause :bad-request :status 400}))))))

(defn fixture_checkprops [fixtureprops siteprops]
  ;; (println "fixture_checkprops ...")
  ;; (println overlayprops)
  (let [err (str
              (if (nil? (:fixtureid fixtureprops)) "fixtureProps.fixtureid is required, also check the spelling - all lowercase for fixtureid")
              (if (nil? (:name fixtureprops)) "fixtureProps.name is required")
              (if (nil? (:siteid siteprops)) "siteprops.siteid is required"))]
    (if (= (count err) 0) true
      (do
        ;;(println err)
        (throw (ex-info (str "exception fixture_checkprops: " err) {:cause :bad-request :status 400}))))))

(defn overlay_checkprops_delete [overlayprops]
  (let [err (str
              (if (nil? (:overlayid overlayprops)) "overlayprops.overlayid is required, also check the spelling - all lowercase for overlayid"))]
    (if (= (count err) 0) true
      (do
        ;;(println err)
        (throw (ex-info (str "exception overlay_checkprops: " err) {:cause :bad-request :status 400}))))))

(defn notification_checkprops [notificationprops siteprops]
  (let [err (str
              (if (nil? (:notificationid notificationprops)) "notificationProps.notificationid is required, also check the spelling - all lowercase for notificationid")
              (if (nil? (:siteid siteprops)) "siteprops.siteid is required"))]
    (if (= (count err) 0) true
      (do
        ;;(println err)
        (throw (ex-info (str "exception notification_checkprops: " err) {:cause :bad-request :status 400}))))))

(defn notification_delete_checkprops [notificationprops siteprops]
  (let [err (str
             (if (nil? (:notificationid notificationprops)) "notificationProps.notificationid is required, also check the spelling - all lowercase for notificationid")
             (if (nil? (:siteid siteprops)) "siteprops.siteid is required"))]
    (if (= (count err) 0) true
      (do
        ;;(println err)
        (throw (ex-info (str "exception notification_checkprops: " err) {:cause :bad-request :status 400}))))))

(defn notification_checkprops_by_name [notificationprops siteprops]
  (let [err (str
              (if (nil? (notificationprops :name)) "notificationProps.name is required")
              (if (nil? (siteprops :siteid)) "siteprops.siteid is required"))]
    (if (= (count err) 0) true
      (do
        ;;(println err)
        (throw (ex-info (str "exception notification_checkprops_by_name: " err) {:cause :bad-request :status 400}))))))

(defn alert_checkprops [alertprops siteprops]
  (let [err (str
              (if (nil? (alertprops :alertid)) "alertProps.alertid is required, also check the spelling - all lowercase for alertid")
              (if (nil? (siteprops :siteid)) "siteprops.siteid is required"))]
    (if (= (count err) 0) true
      (do
        ;;(println err)
        (throw (ex-info (str "exception alert_checkprops: " err) {:cause :bad-request :status 400}))))))

(defn parkingzone_checkprops [parkingzoneprops siteprops]
  (let [err (str
              (if (nil? (parkingzoneprops :parkingzoneid)) "parkingzoneProps.parkingzoneid is required, also check the spelling - all lowercase for parkingzoneid")
              (if (nil? (siteprops :siteid)) "siteprops.siteid is required"))]
    (if (= (count err) 0) true
      (do
        ;;(println err)
        (throw (Exception. (str "exception parkingzone_checkprops: " err)))))))

(defn parkingspot_checkprops [parkingspotprops siteprops]
  (let [err (str
              (if (nil? (parkingspotprops :parkingspotid)) "parkingspotProps.parkingspotid is required, also check the spelling - all lowercase for parkingspotid")
              (if (nil? (siteprops :siteid)) "siteprops.siteid is required"))]
    (if (= (count err) 0) true
      (do
        ;;(println err)
        (throw (Exception. (str "exception parkingspot_checkprops: " err)))))))

(defn parkinggroup_checkprops [parkinggroupprops siteprops]
  (let [err (str
              (if (nil? (parkinggroupprops :parkinggroupid)) "parkinggroupProps.parkinggroupid is required, also check the spelling - all lowercase for parkinggroupid")
              (if (nil? (siteprops :siteid)) "siteprops.siteid is required"))]
    (if (= (count err) 0) true
      (do
        ;;(println err)
        (throw (Exception. (str "exception parkinggroup_checkprops: " err)))))))

(defn activity_logs_checkprops [extprops siteprops orgprops]
  (let [err (str
              (if (nil? (:datemin extprops)) "extprops.datemin is required in the following format YYYY-mm-ddTHH:MM:SS+/-0000")
              (if (nil? (:datemax extprops)) "extprops.datemax is required in the following format YYYY-mm-ddTHH:MM:SS+/-0000")
              (if (nil? (parse (:datemin extprops))) "cannot parse extprops.datemin, format should be YYYY-mm-ddTHH:MM:SS+/-0000")
              (if (nil? (parse (:datemax extprops))) "cannot parse extprops.datemax, format should be YYYY-mm-ddTHH:MM:SS+/-0000")
              (if (nil? (:siteid siteprops)) "siteprops.siteid is required")
              (if (nil? (:orgid orgprops)) "orgprops.orgid is required"))]
    (if (= (count err) 0) true
      (do
        ;;(println err)
        (throw (ex-info (str "exception notification_checkprops: " err) {:cause :bad-request :status 400}))))))

(defn ota_status_checkprops [siteprops orgprops]
  (let [err (str (if (nil? (:siteid siteprops)) "siteprops.siteid is required")
                 (if (nil? (:orgid orgprops)) "orgprops.orgid is required"))]
    (if (= (count err) 0) true
        (do
        ;;(println err)
          (throw (ex-info (str "exception ota_status_checkprops: " err) {:cause :bad-request :status 400}))))))

(defn ota_job_checkprops [siteprops orgprops otaprops]
  (let [err (str (if (nil? (:siteid siteprops)) "siteprops.siteid is required")
                 (if (nil? (:orgid orgprops)) "orgprops.orgid is required")
                 (if (nil? (:jobid otaprops)) "otaprops.jobid is required"))]
    (if (= (count err) 0) true
        (do
        ;;(println err)
          (throw (ex-info (str "exception ota_job_checkprops: " err) {:cause :bad-request :status 400}))))))

(defn notification_checkprops_delete [notificationprops]
  (let [err (str
              (if (nil? (:notificationid notificationprops)) "notificationprops.notificationid is required, also check the spelling - all lowercase for notificationid"))]
    (if (= (count err) 0) true
      (do
        ;;(println err)
        (throw (ex-info (str "exception notification_checkprops: " err) {:cause :bad-request :status 400}))))))

(defn alert_checkprops_delete [alertprops]
  (let [err (str
              (if (nil? (alertprops :alertid)) "alertprops.alertid is required, also check the spelling - all lowercase for alertid"))]
    (if (= (count err) 0) true
      (do
        ;;(println err)
        (throw (ex-info (str "exception alert_checkprops: " err) {:cause :bad-request :status 400}))))))

(defn node_checkprops_delete [nodeprops siteprops]
  (let [err (str
              (if (nil? (:siteid siteprops)) "siteprops.siteid is required")
              (if (nil? (:nodeid nodeprops)) "nodeprops.nodeid is required"))]
    (if (= (count err) 0) true
      (do
        ;;(println err)
        (throw (ex-info (str "exception site_checkprops: " err) {:cause :bad-request :status 400}))))))

(defn pdprofile_checkprops_create [pdprofileprops siteprops orgprops]
  (let [err (str
              (if (nil? (:orgid orgprops)) "orgprops.orgid is required")
              (if (nil? (:siteid siteprops)) "siteprops.siteid is required")
              (->> [:pdprofileid
                    :minLevel
                    :maxLevel
                    :beginTime
                    :endTime
                    :detection_duration]
                   (map #(when-not (% pdprofileprops)
                           (str "pdprofileprops." (name %) " is required")))
                   (apply str)))]
    (if (= (count err) 0) true
      (do
        ;;(println err)
        (throw (ex-info (str "exception pdprofile_checkprops: " err) {:cause :bad-request :status 400}))))))

(defn etdh_checkprops_create [etdhprofileprops siteprops orgprops]
  (let [err (str
             (if (nil? (:orgid orgprops)) "orgprops.orgid is required\n")
             (if (nil? (:siteid siteprops)) "siteprops.siteid is required\n")
             (etdh/explain-etdhprofile etdhprofileprops))]
    (or (zero? (count err))
        (throw (ex-info (str "exception etdh_checkprops: " err)
                        {:cause :bad-request
                         :status 422})))))

(defn dh_checkprops_create [dhprofileprops siteprops orgprops]
  (let [err (str
              (if (nil? (:orgid orgprops)) "orgprops.orgid is required")
              (if (nil? (:siteid siteprops)) "siteprops.siteid is required")
              (->> [:dhprofileid
                    :gain
                    :resetTime
                    :minDrive
                    :maxDrive
                    :slewRate]
                   (mapv #(when-not (% dhprofileprops)
                            (str "dhprofileprops." (name %) " is required")))
                   (apply str)))]
    (if (= (count err) 0) true
      (do
        ;;(println err)
        (throw (ex-info (str "exception dh_checkprops: " err) {:cause :bad-request :status 400}))))))

(defn schedule_checkprops_create [scheduleprops siteprops orgprops]
  (let [ err (str
              (if (nil? (:orgid orgprops)) "orgprops.orgid is required")
              (if (nil? (:siteid siteprops)) "siteprops.siteid is required")
              (if (nil? (:scheduleid scheduleprops)) "scheduleprops.scheduleid is required")
              (if (nil? (:name scheduleprops)) "scheduleprops.name is required")
              (if (nil? (:events scheduleprops)) "scheduleprops.events is required")
              (if (false? (sch/validate-daily-schedule scheduleprops siteprops)) "Can't create a daily schedule with more than 6 dimming points"))]
    (if (= (count err) 0) true
      (do
        ;;(println err)
        (throw (ex-info (str "exception schedule_checkprops: " err) {:cause :bad-request :status 400}))))))

(defn pdprofile_checkprops_get [pdprofileprops siteprops orgprops]
  (let [err (str
              (if (nil? (:orgid orgprops)) "orgprops.orgid is required")
              (if (nil? (:siteid siteprops)) "siteprops.siteid is required")
              (if (nil? (:pdprofileid pdprofileprops)) "pdprofileprops.pdprofileid is required"))]
    (if (= (count err) 0) true
      (do
        ;;(println err)
        (throw (ex-info (str "exception pdprofile_checkprops: " err) {:cause :bad-request :status 400}))))))

(defn dh_checkprops_get [dhprofileprops siteprops orgprops]
  (let [err (str
              (if (nil? (:orgid orgprops)) "orgprops.orgid is required")
              (if (nil? (:siteid siteprops)) "siteprops.siteid is required")
              (if (nil? (:dhprofileid dhprofileprops)) "dhprofileprops.dhprofileid is required"))]
    (if (= (count err) 0) true
      (do
        ;;(println err)
        (throw (ex-info (str "exception dh_checkprops: " err) {:cause :bad-request :status 400}))))))

(defn etdh_checkprops_get [etdhprofileprops siteprops orgprops]
  (let [err (str
             (if (nil? (:orgid orgprops)) "orgprops.orgid is required")
             (if (nil? (:siteid siteprops)) "siteprops.siteid is required")
             (if (nil? (:etdhprofileid etdhprofileprops)) "etdhprofileprops.etdhprofileid is required"))]
    (or (zero? (count err))
        (throw (ex-info (str "exception etdh_checkprops: " err)
                        {:cause :bad-request
                         :status 422})))))

(defn schedule_checkprops_get [scheduleprops siteprops orgprops]
  (let [err (str
              (if (nil? (:orgid orgprops)) "orgprops.orgid is required")
              (if (nil? (:siteid siteprops)) "siteprops.siteid is required")
              (if (nil? (:scheduleid scheduleprops)) "scheduleprops.scheduleid is required"))]
    (if (= (count err) 0) true
      (do
        ;;(println err)
        (throw (ex-info (str "exception notification_checkprops: " err) {:cause :bad-request :status 400}))))))

(defn etdh_checkprops_apply_site [siteprops orgprops etdhprofileprops]
  (let [err (str
              (if (nil? (:orgid orgprops)) "orgprops.orgid is required")
              (if (nil? (:siteid siteprops)) "siteprops.siteid is required")
              (if (nil? (:etdhprofileid etdhprofileprops)) "etdhprofileprops.etdhprofileid is required"))]
    (when-not (zero? (count err))
      (throw (ex-info (str "exception notification_checkprops: " err)
                      {:cause :bad-request
                       :status 422})))))

(defn dh_checkprops_apply_site [siteprops orgprops dhprofileprops]
  (let [err (str
              (if (nil? (:orgid orgprops)) "orgprops.orgid is required")
              (if (nil? (:siteid siteprops)) "siteprops.siteid is required")
              (if (nil? (:dhprofileid dhprofileprops)) "dhprofileprops.dhprofileid is required"))]
    (if (= (count err) 0) true
      (do
        ;;(println err)
        (throw (ex-info (str "exception notification_checkprops: " err) {:cause :bad-request :status 400}))))))

(defn pd_checkprops_apply_site [siteprops orgprops pdprofileprops]
  (let [err (str
              (if (nil? (:orgid orgprops)) "orgprops.orgid is required")
              (if (nil? (:siteid siteprops)) "siteprops.siteid is required")
              (when-not (:pdprofileid pdprofileprops) "pdprofileprops.pdprofileid is required"))]
    (if (= (count err) 0) true
      (do
        ;;(println err)
        (throw (ex-info (str "exception notification_checkprops: " err) {:cause :bad-request :status 400}))))))

(defn dh_checkprops_apply_group [siteprops orgprops dhprofileprops groupprops]
  (let [err (str
              (if (nil? (:orgid orgprops)) "orgprops.orgid is required")
              (if (nil? (:siteid siteprops)) "siteprops.siteid is required")
              (if (nil? (:groupids groupprops)) "groupprops.groupids is required")
              (if (nil? (:dhprofileid dhprofileprops)) "dhprofileprops.dhprofileid is required"))]
    (if (= (count err) 0) true
                          (do
                            ;;(println err)
                            (throw (ex-info (str "exception notification_checkprops: " err) {:cause :bad-request :status 400}))))))

(defn etdh_checkprops_apply_group [siteprops orgprops etdhprofileprops groupprops]
  (let [err (str
              (if (nil? (:orgid orgprops)) "orgprops.orgid is required")
              (if (nil? (:siteid siteprops)) "siteprops.siteid is required")
              (if (nil? (:groupids groupprops)) "groupprops.groupids is required")
              (if (nil? (:etdhprofileid etdhprofileprops)) "etdhprofileprops.etdhprofileid is required"))]
    (when-not (zero? (count err))
      (throw (ex-info (str "exception notification_checkprops: " err)
                      {:cause :bad-request
                       :status 422})))))

(defn etdh_checkprops_apply_nodes [siteprops orgprops etdhprofileprops nodeprops]
  (let [err (str
              (if (nil? (:orgid orgprops)) "orgprops.orgid is required")
              (if (nil? (:siteid siteprops)) "siteprops.siteid is required")
              (if-not (or (:nodeid nodeprops)
                          (:nodeids nodeprops)) "nodeprops.nodeid or nodeprops.nodeids is required")
              (if (nil? (:etdhprofileid etdhprofileprops)) "etdhprofileprops.etdhprofileid is required"))]
    (if (= (count err) 0) true
                          (do
                            ;;(println err)
                            (throw (ex-info (str "exception notification_checkprops: " err) {:cause :bad-request :status 400}))))))

(defn dh_checkprops_apply_nodes [siteprops orgprops dhprofileprops nodeprops]
  (let [err (str
              (if (nil? (:orgid orgprops)) "orgprops.orgid is required")
              (if (nil? (:siteid siteprops)) "siteprops.siteid is required")
              (if-not (or (:nodeid nodeprops)
                          (:nodeids nodeprops)) "nodeprops.nodeid or nodeprops.nodeids is required")
              (if (nil? (:dhprofileid dhprofileprops)) "dhprofileprops.dhprofileid is required"))]
    (if (= (count err) 0) true
                          (do
                            ;;(println err)
                            (throw (ex-info (str "exception notification_checkprops: " err) {:cause :bad-request :status 400}))))))

(defn pd_checkprops_apply_group [siteprops orgprops pdprofileprops groupprops]
  (let [err (str
              (if (nil? (:orgid orgprops)) "orgprops.orgid is required")
              (if (nil? (:siteid siteprops)) "siteprops.siteid is required")
              (when-not (:pdprofileid pdprofileprops) "pdprofileprops.pdprofileid is required")
              (if (nil? (:groupids groupprops)) "groupprops.groupids is required"))]
    (if (= (count err) 0) true
                          (do
                            ;;(println err)
                            (throw (ex-info (str "exception notification_checkprops: " err) {:cause :bad-request :status 400}))))))

(defn pd_checkprops_apply_nodes [siteprops orgprops pdprofileprops nodeprops]
  (let [err (str
              (if (nil? (:orgid orgprops)) "orgprops.orgid is required")
              (if (nil? (:siteid siteprops)) "siteprops.siteid is required")
              (when-not (:pdprofileid pdprofileprops) "pdprofileprops.pdprofileid is required")
              (if (nil? (:nodeids nodeprops)) "nodeprops.nodeids is required"))]
    (if (= (count err) 0) true
                          (do
                            ;;(println err)
                            (throw (ex-info (str "exception notification_checkprops: " err) {:cause :bad-request :status 400}))))))

(defn schedule_checkprops_apply_group [scheduleprops siteprops orgprops groupprops]
  (let [err (str
              (if (nil? (:orgid orgprops)) "orgprops.orgid is required")
              (if (nil? (:siteid siteprops)) "siteprops.siteid is required")
              (if (nil? (:scheduleid scheduleprops)) "scheduleprops.scheduleid is required")
              (if (nil? (:groupids groupprops)) "groupprops.groupids is required"))]
    (if (= (count err) 0) true
                          (do
                            ;;(println err)
                            (throw (ex-info (str "exception notification_checkprops: " err) {:cause :bad-request :status 400}))))))

(defn schedule_checkprops_apply_node [scheduleprops siteprops orgprops nodeprops]
  (let [err (str
              (if (nil? (:orgid orgprops)) "orgprops.orgid is required")
              (if (nil? (:siteid siteprops)) "siteprops.siteid is required")
              (if (nil? (:scheduleid scheduleprops)) "scheduleprops.scheduleid is required")
              (if (nil? (:nodeid nodeprops)) "nodeprops.nodeid is required"))]
    (if (= (count err) 0) true
                          (do
                            ;;(println err)
                            (throw (ex-info (str "exception notification_checkprops: " err) {:cause :bad-request :status 400}))))))

(defn schedule_checkprops_apply_nodes [scheduleprops siteprops orgprops nodeprops]
  (let [err (str
              (if (nil? (:orgid orgprops)) "orgprops.orgid is required")
              (if (nil? (:siteid siteprops)) "siteprops.siteid is required")
              (if (nil? (:scheduleid scheduleprops)) "scheduleprops.scheduleid is required")
              (if (nil? (:nodeids nodeprops)) "nodeprops.nodeids is required"))]
    (if (zero? (count err))
      true
      (throw (ex-info (str "exception notification_checkprops: " err)
                      {:cause :bad-request
                       :status 400
                       :errors err})))))
