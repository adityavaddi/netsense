(ns ^{:doc "The CASEL Processing Engine."}
    utils.cape
  (:gen-class)
  (:require [acl.core :as acl]
            [appl.lighting-control.utils :as lc-utils]
            [appl
             [daylight_harvest :as dh]
             [et-daylight-harvesting :as etdh]
             [proximity-dimming :as pd]]
            [appl.lighting-control.core :refer [LightingPolicy
                                                OptionalLightingPolicy
                                                Overrides]
             :as lc]
            [biscuit.core :as digest]
            [clj-http.client :as client]
            [clj-time.format :as cformat :refer :all]
            [clj-time.local :as local]
            [clj-time.core :as time-core]
            [clojure.core.async :as async]
            [clojure.core.async.impl.concurrent :as conc]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.tools.logging :refer :all]
            [clojure.walk :refer :all]
            [clojure.set :as set]
            [clojure.string :refer :all
             :exclude [replace reverse]]
            [clostache.parser :refer :all]
            [com.stuartsierra.component :as component]
            [dealer.metrics]
            [dealer.devsvcctrl :as dctrl]
            [logging.activitylogger :as al]
            [metrics
             [gauges :as gauges]
             [timers :as timers]]
            [neowrap.neowrapper :as neo4j]
            [kafka.producer :as kafka]
            [utils
             [config :as cfg]
             [nodeconfig :as nodeconfig]
             [propschecker :as pcheck]
             [schedules :as sch]
             [solartime :as solartime]])
  (:import java.util.concurrent.Executors
           org.slf4j.MDC
           java.net.InetAddress
           org.joda.time.DateTime))

(defonce metric-factory
  (dealer.metrics/metric-factory-stub "utils.cape"))

;; These forward declarations are made, as the side effects of various
;; APIs are, themselves, other API calls.
(declare template-selector)
(declare db-selector)
(declare db-selector-legacy)
(declare executer-main)
(declare checkprops)

(defonce cape-thread-pool-size (get (cfg/ddconfig) :cape-thread-pool-size 10))
(def cape-thread-pool (Executors/newFixedThreadPool cape-thread-pool-size
                                                    (conc/counted-thread-factory "cape-%d" true)))

(defn cape-future
  [func & args]
  (.execute cape-thread-pool (fn [] (apply func args))))

;Convert Date to string
(defn as-time-string [date]
  (cformat/unparse (cformat/formatter "yyyy-MM-dd HH:mm:ss.SS") date))
;Used for json/write-str
(defn time-aware-value-writer [key value]
  (if (= key :startdt)  (as-time-string (DateTime. value)) value))

(def cape-api-hierarchy
  (make-hierarchy))

(defn derive!
  [t p]
  (alter-var-root #'utils.cape/cape-api-hierarchy
                  (fn [h]
                    (try
                      (derive h t p)
                      (catch Exception e
                        h)))))

(def no-cypher
  "This is a list of CASEL types which have no Neo4j cypher component
  to be executed. See `template-selector` for details."
  #{"lighting-control"
    "getSensorHistory"
    "getSensorHistoryFromTo"
    "getNodeConnectionStatus"
    "getSiteNodesStatuses"
    "getLostAndFoundNodesStatuses"
    "getNodeLightStatus"
    "getParkingInfoSite"
    "getParkingZones"
    "getOneParkingZone"
    "getCurrentTrafficInfo"
    "getTrafficHistory"
    "getTrafficConfig"
    "logActivity"
    "commandNode"
    "getDefaultConfigs"
    "createBulkNode"
    "calibrateDHProfile"
    "otaStatusForSite"
    "otaStatusForJob"
    "otaStatusJobUpdate"
    "createMetadataForParkingSpot"
    "deleteMetadataForParkingSpot"
    "getAllMetadataForParkingSpot"
    "getMetadataForParkingSpot"
    "updateMetadataForParkingSpot"
    "createAppUserData"
    "deleteAppUserData"
    "getAllAppUserData"
    "getAppUserData"
    "updateAppUserData"
    "createParkingPolicy"
    "deleteParkingPolicy"
    "getAllParkingPolicy"
    "getParkingPolicy"
    "updateParkingPolicy"
    "getParkingPolicyVersion"
    "getAllVersionsOfParkingPolicy"
    "getAllActiveParkingPolicyForPeriod"
    "getActiveParkingPolicy"
    "searchParkingPolicy"
    "createPolicyCategory"
    "deletePolicyCategory"
    "getAllPolicyCategory"
    "getPolicyCategory"
    "updatePolicyCategory"
    "policyAssociation"
    "policyDisassociation"
    "associatedParkingGroups"
    "policyTagsAssociation"
    "policyTagsDisassociation"
    "loginReq"
    })

(def has-cypher
  "This is a mapping for CASEL types which use a prepared Neo4j cypher
  query. See `template-selector` for details."
  {"getOrg" "cyphers/get_org.cypher"
   "deleteNode" "cyphers/delete_node.cypher"
   "getSite" "cyphers/get_site.cypher"
   "getDefaultConfigsForSite" "cyphers/get_config_for_site.cypher"
   "getNode" "cyphers/get_node.cypher"
   "getUser" "cyphers/get_user.cypher"
   "getUserEmail" "cyphers/get_user_email.cypher"
   "getAllLostAndFoundNodes" "cyphers/get_all_lost_found_nodes.cypher"
   "getAllPermissions" "cyphers/get_all_permissions.cypher"
   "getUserPermissions" "cyphers/get_all_usermodel_permissions_for_user.cypher"
   "createNode" "cyphers/create_node.cypher"
   "createEmptyNode" "cyphers/create_empty_node.cypher"
   "assignNode" "cyphers/assign_node.cypher"
   "getAllNodesForSite" "cyphers/get_all_nodes_for_site.cypher"
   "getAllNodeIdsForModelSite" "cyphers/get_all_nodeids_for_model_site.cypher"
   "getAllNodeIdsForModelGroup" "cyphers/get_all_nodeids_for_model_group.cypher"
   "getGroup" "cyphers/get_group.cypher"
   "addNodeToGroup" "cyphers/add_node_to_group.cypher"
   "addNodesToGroup" "cyphers/add_node_to_group.cypher"
   "resendScheduleToLG" "cyphers/get_group.cypher"
   "resendScheduleToNode" "cyphers/get_node.cypher"
   "removeNodeFromGroup" "cyphers/remove_node_from_group.cypher"
   "createETDHProfile" "cyphers/create_etdhprofile.cypher"
   "updateETDHProfile" "cyphers/update_etdhprofile.cypher"
   "applyETDHtoNodes" "cyphers/apply_etdhprofile_to_nodes.cypher"
   "applyDHtoNodes" "cyphers/apply_dhprofile_to_nodes.cypher"
   "applyETDHtoGroup" "cyphers/apply_etdhprofile_to_lighting_group.cypher"
   "applyDHtoGroup" "cyphers/apply_dhprofile_to_lighting_group.cypher"
   "applyETDHtoSite" "cyphers/apply_etdhprofile_to_site.cypher"
   "applyDHtoSite" "cyphers/apply_dhprofile_to_site.cypher"
   "getAllETDHProfiles" "cyphers/get_all_etdhprofiles_for_site.cypher"
   "getETDHProfile" "cyphers/get_etdhprofile.cypher"
   "getDHProfile" "cyphers/get_dhprofile.cypher"
   "deleteETDHProfile" "cyphers/delete_etdhprofile.cypher"
   "deleteDHProfile" "cyphers/delete_dhprofile.cypher"
   "applyPDtoNodes" "cyphers/apply_pdprofile_to_nodes.cypher"
   "applyPDtoGroup" "cyphers/apply_pdprofile_to_lighting_group.cypher"
   "applyPDtoSite" "cyphers/apply_pdprofile_to_site.cypher"
   "getPDProfile" "cyphers/get_pdprofile.cypher"
   "deletePDProfile" "cyphers/delete_pdprofile.cypher"
   "applyScheduleToSite" "cyphers/apply_schedule_to_site.cypher"
   "applyScheduleToGroup" "cyphers/apply_schedule_to_lighting_group.cypher"
   "applyScheduleToNodes" "cyphers/apply_schedule_to_nodes.cypher"
   "applyScheduleToNode" "cyphers/apply_schedule_to_node.cypher"
   "deleteSchedule" "cyphers/delete_schedule.cypher"
   "dismissAlert" "cyphers/dismiss_alert.cypher"
   "getAllOrgAlerts" "cyphers/get_all_alerts_for_org.cypher"
   "getAllAlerts" "cyphers/get_all_alerts_for_site.cypher"
   "getAlertByNodeNameType" "cyphers/get_alert_for_node_name_type.cypher"
   "getAlertSys" "cyphers/get_active_alert.cypher"
   "createAlert" "cyphers/create_alert.cypher"
   "updateAlert" "cyphers/update_alert.cypher"
   "deleteAlert" "cyphers/delete_alert.cypher"
   "getAlert" "cyphers/get_alert.cypher"
   "getAlertsForNode" "cyphers/get_all_alerts_for_node.cypher"
   "searchAlerts" "cyphers/get_all_alerts_for_node.cypher"
   "getConfig" "cyphers/get_config.cypher"
   "getSchedule" "cyphers/get_schedule.cypher"
   "updateConfig" "cyphers/update_config.cypher"
   "deleteConfig" "cyphers/delete_config.cypher"
   "getUserEmails" "cyphers/get_user_emails.cypher"
   "bulkAssignUsersToParkingGroup" "cyphers/assign_users_to_parkinggroup.cypher"
   "bulkUnassignUsersFromParkingGroup" "cyphers/unassign_users_from_parkinggroup.cypher"
   "removeETDHProfileTriggers" "cyphers/remove_etdhprofile_trigger_node.cypher"
   "addETDHProfileTriggers" "cyphers/add_etdhprofile_trigger_node.cypher"
   })



(def has-template
  "This is a mapping for CASEL types which require a custom templated
  Neo4j cypher query. If possible, avoid this and prefer use of
  `has-cypher` above. See `template-selector` for details."
  {"lighting-control-site" {:template "templates/get_all_nodeids_for_site.cypher.tmpl"
                            :template-args (fn [jsmap]
                                             {:siteid (get-in jsmap [:siteprops :siteid])})}
   "lighting-control-group" {:template "templates/get_all_nodeids_in_group.cypher.tmpl"
                             :template-args (fn [jsmap]
                                              {:groupid (get-in jsmap [:groupprops :groupid])})}
   "getAllOrgs" {:template "templates/get_all_orgs.cypher.tmpl"
                 :template-args (fn [jsmap]
                                  {:userid (jsmap :user)})}
   "getAllSuspendedOrgs" {:template "templates/get_all_suspended_orgs.cypher.tmpl"
                          :template-args (fn [jsmap]
                                           {:userid (jsmap :user)})}
   "getAllSitesForOrg" {:template "templates/get_all_sites_for_org.cypher.tmpl"
                        :template-args (fn [jsmap]
                                         {:orgid (get-in jsmap [:orgprops
                                                                :orgid])})}
   "getAllSuspendedSitesForOrg" {:template "templates/get_all_suspended_sites_for_org.cypher.tmpl"
                                 :template-args (fn [jsmap]
                                                  {:orgid (get-in jsmap [:orgprops
                                                                         :orgid])})}
   "getAllMinNodesForSite" {:template "templates/get_all_min_nodes_for_site.cypher.tmpl"
                            :template-args (fn [jsmap]
                                             {:siteid (get-in jsmap [:siteprops
                                                                     :siteid])})}
   "getAllETDHProfileTriggers" {:template "templates/get_etdhprofile_trigger_nodes.cypher.tmpl"
                              :template-args (fn [jsmap]
                                               (:etdhprofileprops jsmap))}
   "getAllDHProfileTriggers" {:template "templates/get_dhprofile_trigger_nodes.cypher.tmpl"
                              :template-args (fn [jsmap]
                                               (:dhprofileprops jsmap))}
   "removeDHProfileTrigger" {:template "templates/remove_dhprofile_trigger_node.cypher.tmpl"
                             :template-args (fn [jsmap]
                                              (merge (:dhprofileprops jsmap)
                                                     (:nodeprops jsmap)))}
   "addDHProfileTrigger" {:template "templates/add_dhprofile_trigger_node.cypher.tmpl"
                          :template-args (fn [jsmap]
                                           (merge (:dhprofileprops jsmap)
                                                  (:nodeprops jsmap)))}
   "getAllOverlays" {:template "templates/get_all_overlays_for_site.cypher.tmpl"
                     :template-args (fn [jsmap]
                                      {:siteid (get-in jsmap [:siteprops
                                                              :siteid])})}
   "getAllGroups" {:template "templates/get_all_groups_for_site.cypher.tmpl"
                   :template-args (fn [jsmap]
                                    {:siteid (get-in jsmap [:siteprops
                                                            :siteid])})}
   "getAllFixtures" {:template "templates/get_all_fixtures_for_site.cypher.tmpl"
                     :template-args (fn [jsmap]
                                      {:siteid (get-in jsmap [:siteprops
                                                              :siteid])})}
   "getAllNotificationsForSite" {:template "templates/get_all_notifications_for_site.cypher.tmpl"
                                 :template-args (fn [jsmap]
                                                  {:siteid (get-in jsmap [:siteprops
                                                                          :siteid])})}
   "getAllNotificationsForUser" {:template "templates/get_all_notifications_for_user.cypher.tmpl"
                                 :template-args (fn [jsmap]
                                                  {:userid (get-in jsmap [:userprops
                                                                          :userid])})}
   "getAllNotificationsForOrg" {:template "templates/get_all_notifications_for_org.cypher.tmpl"
                                 :template-args (fn [jsmap]
                                                  {:userid (get-in jsmap [:userprops
                                                                           :userid])
                                                   :orgid (get-in jsmap [:orgprops
                                                                          :orgid])})}
   "getAllNotificationsByName" {:template "templates/get_all_notifications_for_site_by_name.cypher.tmpl"
                                :template-args (fn [jsmap]
                                                 {:siteid (get-in jsmap [:siteprops
                                                                         :siteid])
                                                  :name (get-in jsmap [:notificationprops
                                                                       :name])})}
   "searchNotifications" {:template "templates/get_all_notifications_for_node.cypher.tmpl"
                          :template-args
                          (fn [jsmap]
                            {:nodeid (get-in jsmap [:notificationprops
                                                    :nodeid])
                             :name (get-in jsmap [:notificationprops
                                                  :name])})}
   "getAllParkingZones" {:template "templates/get_all_parkingzones_for_site.cypher.tmpl"
                         :template-args (fn [jsmap]
                                          {:siteid (get-in jsmap [:siteprops
                                                                  :siteid])})}
   "getAllParkingSpots" {:template "templates/get_all_parkingspots_for_site.cypher.tmpl"
                         :template-args (fn [jsmap]
                                          {:siteid (get-in jsmap [:siteprops
                                                                  :siteid])})}
   "getAllParkingGroups" {:template "templates/get_all_parkinggroups_for_site.cypher.tmpl"
                          :template-args (fn [jsmap]
                                           {:siteid (get-in jsmap [:siteprops
                                                                   :siteid])})}
   "getOverlay" {:template "templates/get_overlay.cypher.tmpl"
                 :template-args (fn [jsmap]
                                  {:overlayid (get-in jsmap [:overlayprops
                                                             :overlayid])})}
   "getFixture" {:template "templates/get_fixture.cypher.tmpl"
                 :template-args (fn [jsmap]
                                  {:fixtureid (get-in jsmap [:fixtureprops :fixtureid])})}
   "autoComplete" {:template "templates/auto_complete.cypher.tmpl"
                   :template-args (fn [jsmap]
                                    (merge {:userid (:user jsmap)}
                                           (:autoprops jsmap)))}
   "getNotification" {:template "templates/get_notification.cypher.tmpl"
                      :template-args (fn [jsmap]
                                       {:notificationid (get-in jsmap [:notificationprops
                                                                       :notificationid])
                                        :siteid (get-in jsmap [:siteprops
                                                               :siteid])
                                        })}
   "getNotificationSys" {:template "templates/get_notification.cypher.tmpl"
                         :template-args (fn [jsmap]
                                          {:notificationid (get-in jsmap [:notificationprops
                                                                          :notificationid])

                                           :siteid (get-in jsmap [:siteprops
                                                                  :siteid])
                                           })}
   "getParkingSpot" {:template "templates/get_parkingspot.cypher.tmpl"
                     :template-args (fn [jsmap]
                                      {:alertid (get-in jsmap [:parkingspotprops
                                                               :parkingspotid])})}
   "getParkingGroup" {:template "templates/get_parkinggroup.cypher.tmpl"
                      :template-args (fn [jsmap]
                                       (merge (:siteprops jsmap)
                                              (:parkinggroupprops jsmap)))}
   "createUser" {:template "templates/create_user.cypher.tmpl"
                 :template-args (fn [jsmap]
                                  (merge (:orgprops jsmap)
                                         (:userprops jsmap)))}
   "updateUser" {:template "templates/update_user.cypher.tmpl"
                 :template-args (fn [jsmap]
                                  (merge (:orgprops jsmap)
                                         (:userprops jsmap)))}
   "deleteUser" {:template "templates/delete_user.cypher.tmpl"
                 :template-args (fn [jsmap]
                                  (merge (:orgprops jsmap)
                                         (:userprops jsmap)))}
   "suspendUser" {:template "templates/suspend_user.cypher.tmpl"
                  :template-args (fn [jsmap]
                                   (merge (:orgprops jsmap)
                                          (:userprops jsmap)))}
   "activateUser" {:template "templates/activate_user.cypher.tmpl"
                   :template-args (fn [jsmap]
                                    (merge (:orgprops jsmap)
                                           (:userprops jsmap)))}
   "getAllUsersForOrg" {:template "templates/get_all_users_for_org.cypher.tmpl"
                        :template-args (fn [jsmap]
                                         {:orgid (get-in jsmap [:orgprops
                                                                :orgid])})}
   "getAllUsersForSite" {:template "templates/get_all_users_for_site.cypher.tmpl"
                         :template-args (fn [jsmap]
                                          {:siteid (get-in jsmap [:siteprops
                                                                  :siteid])})}
   "getAllUsersForPartner" {:template "templates/get_all_users_for_partner.cypher.tmpl"
                            :template-args (fn [jsmap]
                                             {:orgid (get-in jsmap [:orgprops
                                                                    :orgid])})}
   "getAllSuspendedUsersForOrg" {:template "templates/get_all_suspended_users_for_org.cypher.tmpl"
                                 :template-args (fn [jsmap]
                                                  {:orgid (get-in jsmap [:orgprops
                                                                         :orgid])})}
   "getAllSuspendedUsersForPartner" {:template "templates/get_all_suspended_users_for_partner.cypher.tmpl"
                                     :template-args (fn [jsmap]
                                                      {:orgid (get-in jsmap [:orgprops
                                                                             :orgid])})}
   "createOrg" {:template "templates/create_org.cypher.tmpl"
                :template-args (fn [jsmap]
                                 (merge (:orgprops jsmap)
                                        {:userid (:user jsmap)}))}
   "createSite" {:template "templates/create_site.cypher.tmpl"
                 :template-args (fn [jsmap]
                                  (merge (:orgprops jsmap)
                                         (:siteprops jsmap)))}
   "assignNodeToParkingGroup" {:template "templates/assign_node_to_parkinggroup.cypher.tmpl"
                               :template-args (fn [jsmap]
                                                (merge (:parkinggroupprops jsmap)
                                                       (:nodeprops jsmap)))}
   "createOverlay" {:template "templates/create_overlay.cypher.tmpl"
                    :template-args (fn [jsmap]
                                     (merge (:siteprops jsmap)
                                            (:overlayprops jsmap)))}
   "createFixture" {:template "templates/create_fixture.cypher.tmpl"
                    :template-args (fn [jsmap]
                                     (merge (:siteprops jsmap)
                                            (:fixtureprops jsmap)))}
   "createNotification" {:template "templates/create_notification.cypher.tmpl"
                         :template-args (fn [jsmap]
                                          (merge (:siteprops jsmap)
                                                 (:notificationprops jsmap)))}
   "createParkingZone" {:template "templates/create_parkingzone.cypher.tmpl"
                        :template-args (fn [jsmap]
                                         (merge (:siteprops jsmap)
                                                (:parkinggroupprops jsmap)
                                                (:parkingzoneprops jsmap)))}
   "createParkingSpot" {:template "templates/create_parkingspot.cypher.tmpl"
                        :template-args (fn [jsmap]
                                         (merge (:siteprops jsmap)
                                                (:parkingzoneprops jsmap)
                                                (:parkingspotprops jsmap)))}
   "createParkingGroup" {:template "templates/create_parkinggroup.cypher.tmpl"
                         :template-args (fn [jsmap]
                                          (merge (:siteprops jsmap)
                                                 (:parkinggroupprops jsmap)))}
   "updateOrg" {:template "templates/update_org.cypher.tmpl"
                :template-args (fn [jsmap]
                                 (:orgprops jsmap))}
   "updateOverlay" {:template "templates/update_overlay.cypher.tmpl"
                    :template-args (fn [jsmap]
                                     (:overlayprops jsmap))}
   "updateFixture" {:template "templates/update_fixture.cypher.tmpl"
                    :template-args (fn [jsmap]
                                     (:fixtureprops jsmap))}
   "updateNode" {:template "templates/update_node.cypher.tmpl"
                 :template-args (fn [jsmap]
                                  (merge (:siteprops jsmap)
                                         (:nodeprops jsmap)))}
   "activateNode" {:template "templates/activate_node.cypher.tmpl"
                   :template-args (fn [jsmap]
                                    (merge (:siteprops jsmap)
                                           (:nodeprops jsmap)))}
   "deactivateNode" {:template "templates/deactivate_node.cypher.tmpl"
                     :template-args (fn [jsmap]
                                      (merge (:siteprops jsmap)
                                             (:nodeprops jsmap)))}
   "deleteFixture" {:template "templates/delete_fixture.cypher.tmpl"
                    :template-args (fn [jsmap]
                                     (:fixtureprops jsmap))}
   "deleteGroup" {:template "templates/delete_group.cypher.tmpl"
                  :template-args (fn [jsmap]
                                   (:groupprops jsmap))}
   "deleteOrg" {:template "templates/delete_org.cypher.tmpl"
                :template-args (fn [jsmap]
                                 (merge (:orgprops jsmap)
                                        {:userid (:user jsmap)}))}
   "suspendOrg" {:template "templates/suspend_org.cypher.tmpl"
                 :template-args (fn [jsmap]
                                  (merge (:orgprops jsmap)
                                         {:userid (:user jsmap)}))}
   "activateOrg" {:template "templates/activate_org.cypher.tmpl"
                  :template-args (fn [jsmap]
                                   (merge (:orgprops jsmap)
                                          {:userid (:user jsmap)}))}
   "realDeleteNode" {:template "templates/real_delete_node.cypher.tmpl"
                     :template-args (fn [jsmap]
                                      (merge (:siteprops jsmap)
                                             (:nodeprops jsmap)))}
   "deleteOverlay" {:template "templates/delete_overlay.cypher.tmpl"
                    :template-args (fn [jsmap]
                                     (:overlayprops jsmap))}
   "updateSite" {:template "templates/update_site.cypher.tmpl"
                 :template-args (fn [jsmap]
                                  (:siteprops jsmap))}
   "updateNotification" {:template "templates/update_notification.cypher.tmpl"
                         :template-args (fn [jsmap]
                                          (merge (:siteprops jsmap)
                                                 (:notificationprops jsmap)))}
   "activateNotification" {:template "templates/activate_notification.cypher.tmpl"
                           :template-args (fn [jsmap]
                                            (:notificationprops jsmap))}
   "deactivateNotification" {:template "templates/deactivate_notification.cypher.tmpl"
                             :template-args (fn [jsmap]
                                              (:notificationprops jsmap))}
   "updateParkingZone" {:template "templates/update_parkingzone.cypher.tmpl"
                        :template-args (fn [jsmap]
                                         (merge (:siteprops jsmap)
                                                (:parkingzoneprops jsmap)))}
   "updateParkingSpot" {:template "templates/update_parkingspot.cypher.tmpl"
                        :template-args (fn [jsmap]
                                         (merge (:siteprops jsmap)
                                                (:parkingspotprops jsmap)))}
   "updateParkingGroup" {:template "templates/update_parkinggroup.cypher.tmpl"
                         :template-args (fn [jsmap]
                                          (merge (:siteprops jsmap)
                                                 (:parkinggroupprops jsmap)))}
   "deleteSite" {:template "templates/delete_site.cypher.tmpl"
                 :template-args (fn [jsmap]
                                  {:siteid (get-in jsmap [:siteprops
                                                          :siteid])})}
   "suspendSite" {:template "templates/suspend_site.cypher.tmpl"
                  :template-args (fn [jsmap]
                                   {:siteid (get-in jsmap [:siteprops
                                                           :siteid])})}
   "activateSite" {:template "templates/activate_site.cypher.tmpl"
                   :template-args (fn [jsmap]
                                    {:siteid (get-in jsmap [:siteprops
                                                            :siteid])})}
   "deleteNotification" {:template "templates/delete_notification.cypher.tmpl"
                         :template-args (fn [jsmap]
                                          (merge (:siteprops jsmap)
                                                 (:notificationprops jsmap)))}
   "deleteParkingZone" {:template "templates/delete_parkingzone.cypher.tmpl"
                        :template-args (fn [jsmap]
                                         (:parkingzoneprops jsmap))}
   "deleteParkingSpot" {:template "templates/delete_parkingspot.cypher.tmpl"
                        :template-args (fn [jsmap]
                                         (:parkingspotprops jsmap))}
   "deleteParkingGroup" {:template "templates/delete_parkinggroup.cypher.tmpl"
                         :template-args (fn [jsmap]
                                          (:parkinggroupprops jsmap))}
   "createInstallPod" {:template "templates/create_install_pod.cypher.tmpl"
                       :template-args (fn [jsmap]
                                        (:podprops jsmap))}
   "deleteInstallPod" {:template "templates/delete_install_pod.cypher.tmpl"
                       :template-args (fn [jsmap]
                                        (:podprops jsmap))}
   "getFirmware" {:template "templates/get_firmware.cypher.tmpl"
                  :template-args (fn [jsmap]
                                   (:firmwareprops jsmap))}
   "getAllFirmwares" {:template "templates/get_all_firmwares.cypher.tmpl"
                      :template-args (fn [jsmap]
                                       (:firmwareprops jsmap))}
   "createFirmware" {:template "templates/create_firmware.cypher.tmpl"
                     :template-args (fn [jsmap]
                                      (:firmwareprops jsmap))}
   "updateFirmware" {:template "templates/update_firmware.cypher.tmpl"
                     :template-args (fn [jsmap]
                                      (:firmwareprops jsmap))}
   "deleteFirmware" {:template "templates/delete_firmware.cypher.tmpl"
                     :template-args (fn [jsmap]
                                      (:firmwareprops jsmap))}
   "assignFirmwareToNode" {:template "templates/assign_firmware_to_nodes.cypher.tmpl"
                           :template-args (fn [jsmap]
                                            (:firmwareprops jsmap))}
   "assignFirmwareToSite" {:template "templates/assign_firmware_to_nodes.cypher.tmpl"
                           :template-args (fn [jsmap]
                                            (:firmwareprops jsmap))}
   "assignFirmwareToGroup" {:template "templates/assign_firmware_to_nodes.cypher.tmpl"
                            :template-args (fn [jsmap]
                                             (:firmwareprops jsmap))}
   "getAllPDProfiles" {:template "templates/get_all_pdprofiles_for_site.cypher.tmpl"
                       :template-args (fn [jsmap]
                                        (:siteprops jsmap))}
   "getAllDHProfiles" {:template "templates/get_all_dhprofiles_for_site.cypher.tmpl"
                       :template-args (fn [jsmap]
                                        (:siteprops jsmap))}
   "getAllSchedules" {:template "templates/get_all_schedules_for_site.cypher.tmpl"
                      :template-args (fn [jsmap]
                                       (:siteprops jsmap))}
   "createPDProfile" {:template "templates/create_pdprofile.cypher.tmpl"
                      :template-args (fn [jsmap]
                                       (merge (:pdprofileprops jsmap)
                                              (:siteprops jsmap)))}
   "createDHProfile" {:template "templates/create_dhprofile.cypher.tmpl"
                      :template-args (fn [jsmap]
                                       (merge (:dhprofileprops jsmap)
                                              (:siteprops jsmap)))}
   "createSchedule" {:template "templates/create_schedule.cypher.tmpl"
                     :template-args (fn [jsmap]
                                      (merge (:scheduleprops jsmap)
                                             (:siteprops jsmap)))}
   "updatePDProfile" {:template "templates/update_pdprofile.cypher.tmpl"
                      :template-args (fn [jsmap]
                                       (:pdprofileprops jsmap))}
   "updateDHProfile" {:template "templates/update_dhprofile.cypher.tmpl"
                      :template-args (fn [jsmap]
                                       (:dhprofileprops jsmap))}
   "updateSchedule" {:template "templates/update_schedule.cypher.tmpl"
                     :template-args (fn [jsmap]
                                      (merge (:siteprops jsmap)
                                             (:scheduleprops jsmap)))}
   "getAllConfigs" {:template "templates/get_all_configs.cypher.tmpl"
                    :template-args (fn [jsmap]
                                     (:siteprops jsmap))}
   "applyConfigToSite" {:template "templates/apply_config_to_site.cypher.tmpl"
                        :template-args (fn [jsmap]
                                         (merge (:configprops jsmap)
                                                (:siteprops jsmap)))}
   "applyConfigToGroup" {:template "templates/apply_config_to_group.cypher.tmpl"
                         :template-args (fn [jsmap]
                                          (merge (:configprops jsmap)
                                                 (:groupprops jsmap)
                                                 (:siteprops jsmap)))}
   "applyConfigToNodes" {:template "templates/apply_config_to_nodes.cypher.tmpl"
                         :template-args (fn [jsmap]
                                          (merge (:configprops jsmap)
                                                 (:siteprops jsmap)
                                                 (:nodeprops jsmap)))}
   "applyServerToNodes" {:template "templates/apply_server_to_nodes.cypher.tmpl"
                         :template-args (fn [jsmap]
                                          (:siteprops jsmap))}
   "createConfig" {:template "templates/create_config.cypher.tmpl"
                   :template-args (fn [jsmap]
                                    (merge (:configprops jsmap)
                                           (:siteprops jsmap)))}
   "assignFixtureToNode" {:template "templates/assign_fixture_to_node.cypher.tmpl"
                          :template-args (fn [jsmap]
                                           (:fixtureprops jsmap))}
   "assignFixtureToNodes" {:template "templates/assign_fixture_to_node.cypher.tmpl"
                          :template-args (fn [jsmap]
                                           (:fixtureprops jsmap))}
   "assignFixtureToSite" {:template "templates/assign_fixture_to_node.cypher.tmpl"
                          :template-args (fn [jsmap]
                                           (:fixtureprops jsmap))}
   "assignFixtureToGroup" {:template "templates/assign_fixture_to_node.cypher.tmpl"
                           :template-args (fn [jsmap]
                                            (:fixtureprops jsmap))}})

(defn template-selector
  "Given a CASEL type, it may not require a Neo4j query, it may
  require a general cypher query, or it may require a templated cypher
  query. Those cases are handled by looking up the type in
  `no-cypher`, `has-cypher`, and `has-template`. Any remaining cases
  are few, and are handled by the latter clause of this function."
  [{:keys [type]
    qtype :type
    :as jsmap}]
  (let [has-no-cypher (no-cypher type)
        cypher (spy :debug (get has-cypher type))
        {:keys [template
                template-args]} (get has-template type)]
    (cond has-no-cypher jsmap
          cypher (assoc jsmap :cypher (->> cypher
                                           io/resource
                                           slurp
                                           (spy :debug)))
          (and (spy :debug template)
               template-args) (assoc jsmap :cypher (->> (template-args jsmap)
                                                        (render-resource template)
                                                        (spy :debug)))

          :else (case qtype
                  ;; The below one needs no query or query template - passing Nil
                  "getActivityLogs" (assoc jsmap :cypher {:userid (jsmap :user)
                                                          :daterange [(get-in jsmap [:extprops :datemin])
                                                                      (get-in jsmap [:extprops :datemax])]})

                  "createGroup" (case (-> jsmap
                                          :groupprops
                                          :type)
                                  "organizational" (->> (merge (:siteprops jsmap)
                                                               (:groupprops jsmap))
                                                        (render-resource "templates/create_group.cypher.tmpl")
                                                        (spy :debug)
                                                        (assoc jsmap :cypher))
                                  "lighting" (->> (merge (:siteprops jsmap)
                                                         (:groupprops jsmap))
                                                  (render-resource "templates/create_lighting_group.cypher.tmpl")
                                                  (spy :debug)
                                                  (assoc jsmap :cypher))
                                  "site-lighting" jsmap)

                  "updateGroup" (case (-> jsmap
                                          :groupprops
                                          :type)
                                  "organizational" (->> (merge (:siteprops jsmap)
                                                               (:groupprops jsmap))
                                                        (render-resource "templates/update_group.cypher.tmpl")
                                                        (spy :debug)
                                                        (assoc jsmap :cypher))
                                  ("site-lighting"
                                   "lighting") (->> "cyphers/update_lighting_group.cypher"
                                                    io/resource
                                                    slurp
                                                    (spy :debug)
                                                    (assoc jsmap :cypher))
                                  (doto (ex-info "Unhandled case in `template-selector`, for type `updateGroup`"
                                                 {:jsmap jsmap})
                                    error
                                    throw))
                  (doto (ex-info "Unhandled case in `template-selector`"
                                 {:jsmap jsmap})
                    error
                    throw)))))

;; Return status code for cause
(defn status-code-for
  [cause]
  (case cause
        :not-authorized 403
        :not-found 404
        500))

;; A small utility method to get date field working
(defn json-date-writer [key value]
  (if (= key :when)
    (str (org.joda.time.DateTime. value))
    value))

(defn get-site-location
  [siteid]
  (spy :debug (format "getting lat/long for site %s" siteid))
  (let [{{:keys [latitude
                 longitude]} :site
         :as result} (-> "cyphers/get_site.cypher"
                         io/resource
                         slurp
                         (neo4j/executeQuery {"siteid" siteid})
                         (json/read-str :key-fn keyword))]
    {"longitude" longitude
     "latitude" latitude}))

(defmulti db-selector (comp keyword
                            :type)
  :hierarchy #'cape-api-hierarchy)

(defmethod db-selector :default
  [jsmap]
  (db-selector-legacy jsmap))

(defn apply-schedule
  [{:keys [type
           cypher
           scheduleprops]
    {:keys [scheduleid]} :scheduleprops
    {:keys [orgid siteid]} :siteprops
    {:keys [groupids]} :groupprops
    {:keys [nodeid
            nodeids]} :nodeprops
    userid :user
    :as jsmap}]
  {:pre [(#{"applyScheduleToGroup"
            "applyScheduleToSite"
            "applyScheduleToNode"
            "applyScheduleToNodes"} type)]}
  (spy :debug jsmap)
  (when scheduleprops
    (let [target-type (.substring type (count "applyScheduleTo"))
          {:keys [cypher-resource
                  target-id
                  cypher-data]} (get {"Site" {:target-id siteid
                                              :cypher-data {:siteid siteid
                                                            :scheduleid scheduleid}}
                                      "Group" {:target-id (clojure.string/join ", " groupids)
                                               :cypher-data {:groupids groupids
                                                             :siteid siteid
                                                             :scheduleid scheduleid}}
                                      "Node" {:target-id nodeid
                                              :cypher-data {:nodeid nodeid
                                                            :scheduleid scheduleid}}
                                      "Nodes" {:target-id (clojure.string/join ", " nodeids)
                                               :cypher-data {:siteid siteid
                                                             :nodeids nodeids
                                                             :scheduleid scheduleid}}}
                                     target-type)
          {:keys [exception]
           nodes :items
           {:keys [events name network]
            :as schedule} :schedule
          :as cypher-result} (-> cypher
                                  (neo4j/executeQuery (stringify-keys cypher-data))
                                  (json/read-str :key-fn keyword))
          nodeids (map :nodeid nodes)
          nodemodels (into {} (map #(hash-map (:nodeid %) (:model %)) nodes))
          nodeids-sch (filter #(= (get nodemodels %) "cnext") nodeids)
          nodeids-nosch (filter #(not= (get nodemodels %) "cnext") nodeids)
          query-error (cond
                        (some? exception) {:error exception}
                        (and (= target-type "Site") (empty? cypher-result)) {:error (format "Site %s not found" target-id) :status 404}
                        (and (= target-type "Group") (empty? cypher-result)) {:error (format "Group %s not found" target-id) :status 404}
                        (empty? cypher-result) {:error (format "Schedule %s does not exist" scheduleid) :status 404}
                        :else {})
          query-success (empty? query-error)
          props (stringify-keys cypher-data)
          log_map {:activity type
                   :targettype target-type
                   :targetid target-id
                   :message props}
          siteLocation (spy :debug (get-site-location siteid))
          latitude (get siteLocation "latitude")
          longitude (get siteLocation "longitude")
          error (merge (cond
                         (or (empty? latitude)
                             (empty? longitude)) {:error (format "Site's Latitude '%s' and/or longitude '%s' must be defined"
                                                                 latitude
                                                                 longitude)}
                         :else {})
                       query-error)
          success (and (empty? error)
                       query-success)]
      (spy :debug (format "Latitude '%s' Longitude '%s' Scheduleid: %s" latitude longitude scheduleid))
      (when success
        (al/log "user" (merge {:userid userid} log_map))
        (when siteid
          (al/log "site" (merge {:siteid siteid :userid userid} log_map)))
        (when (seq nodeids)
          (if schedule
            (do (when (seq nodeids-nosch)
                  (cape-future sch/send-daily-schedule events network latitude longitude nodeids-nosch))
                (when (seq nodeids-sch)
                  (sch/send-schedule-to-lss userid orgid siteid events network latitude longitude nodeids-sch scheduleid)))
            (let [jsmap (assoc jsmap
                               :type "applyScheduleToNodes"
                               :nodeprops {:nodeids nodeids})
                  {:keys [success]
                   cypher-error :error} (-> jsmap
                                            template-selector
                                            db-selector
                                            (json/read-str :key-fn keyword))]
              (when-not success
                (doto (ex-info "Failed to apply schedule to nodes"
                               {:error cypher-error
                                :nodeids nodeids
                                :jsmap jsmap})
                  error
                  throw)))))
        (when (or (= "Site" target-type)
                  (and (= "Group" target-type)
                       (-> "MATCH (slg:SiteLightingGroup)
WHERE slg.groupid IN {props}.groupids
RETURN slg"
                           (neo4j/executeQuery {"groupids" groupids})
                           (json/read-str :key-fn keyword)
                           :slg)))
          (when-let [other-nodes (-> (render-resource "templates/get_inheriting_lg_nodes.cypher.tmpl"
                                                      {:type "Schedule"})
                                     (neo4j/executeQuery {"siteid" siteid})
                                     (json/read-str :key-fn keyword)
                                     :nodeids
                                     (->> (map :nodeid))
                                     seq)]
            (let [jsmap (-> jsmap
                            (dissoc :groupprops)
                            (assoc :type "applyScheduleToNodes"
                                   :nodeprops {:nodeids other-nodes}))
                  {:keys [success]
                   cypher-error :error} (-> jsmap
                                            template-selector
                                            db-selector
                                            (json/read-str :key-fn keyword))]
              (when-not success
                (doto (ex-info "Failed to apply schedule to nodes"
                               {:error cypher-error
                                :nodeids nodeids
                                :jsmap jsmap})
                  error
                  throw))))))
      (json/write-str (assoc error
                             :success success
                             :schedule (-> (assoc jsmap :type "getSchedule")
                                           template-selector
                                           db-selector
                                           (json/read-str :key-fn keyword)
                                           :schedule))))))

(defn get-timezone
  [lat lon]
  (spy :debug (format "getting timezone with lat: %s and long: %s" lat lon))
  (if-not
      (and (empty? lon) (empty? lat))
    (str (solartime/get-time-zone lat lon))
    nil))

(defn get-site-timezone
  [siteid]
  (let [siteLocation (get-site-location siteid)
        lat (get siteLocation "latitude")
        lon (get siteLocation "longitude")]
    (get-timezone lat lon)))

(defn update-node-timezone
  [props siteid]
  (let [nodeid (get props "nodeid")
        site-timezone (get-site-timezone siteid)
        timezone (if-not (nil? site-timezone) site-timezone "UTC")]
    (debugf "updating timezone for node %s with %s" nodeid timezone)
    (assoc props "time_zone" timezone)))

(defn update-site-timezone
  [props siteid]
  (let [lon (get props "longitude")
        lat (get props "latitude")
        timezone (or (get-timezone lat lon)
                     "UTC")]
    (debugf "updating timezone for site %s with %s" siteid timezone)
    (assoc props "time_zone" timezone)))

(defn filter-country-code
  [component]
  (let [types (get component :types)]
    (some #(= "country" %) types)))

(defn update-country-code
  [props lat lon]
  (let [url (format "https://maps.googleapis.com/maps/api/geocode/json?key=AIzaSyA_GF7hf_BZsdY6LS9zZ-Jl9nXKffKc6Lw&latlng=%s,%s" lat lon)
        resp (client/get url {:as :json})
        status (get resp :status)]
    (spy :debug resp)
    (if (= status 200)
      (let [results (get-in resp [:body :results])
            data (get results 0 {:address_components [{:types ["country"] :short_name ""}]})
            components (get data :address_components)
            country (filter filter-country-code components)
            code (->> country first :short_name)]
        (debugf "updating country_code to %s" code)
        (assoc props "country_code" code))
      props)))

(defn update-node-country-code
  [props siteid]
  (let [siteLocation (get-site-location siteid)
        lat (get siteLocation "latitude")
        lon (get siteLocation "longitude")]
    (update-country-code props lat lon)))

(defn update-site-country-code
  [props]
  (let [lat (get props "latitude")
        lon (get props "longitude")]
    (update-country-code props lat lon)))

(defn update-location-data
  [props siteid type]
  (spy :debug (format " update location data: %s (site: %s)" type siteid))
  (case type
    "createNode"
    (-> props
        (update-node-timezone siteid)
        (update-node-country-code siteid))
    "updateNode"
    (-> props
        (update-node-timezone siteid)
        (update-node-country-code siteid))
    "assignNode"
    (-> props
        (update-node-timezone siteid)
        (update-node-country-code siteid))
    "createSite"
    (-> props
        (update-site-timezone siteid)
        update-site-country-code)
    "updateSite"
    (-> props
        (update-site-timezone siteid)
        update-site-country-code)
    props))

(defn get-config-by-id
  [configid]
  (let [{:keys [config]} (-> "cyphers/get_config_by_id.cypher"
                             io/resource
                             slurp
                             (neo4j/executeQuery {"configid" configid})
                             (json/read-str :key-fn keyword))]
    (or config
        {})))

(defn delete-config
  [{:keys [cypher] type :type userid :user :as jsmap}]
  (let [siteid (get-in jsmap [:siteprops :siteid])
        configid (get-in jsmap [:configprops :configid])
        results (json/read-str (neo4j/executeQuery cypher {"configid" configid "siteid" siteid}))
        nodeids (get results "nodes" [])
        exception (get results "exception" nil)
        error (if-not (nil? exception) {:error exception} {})
        success (nil? exception)
        log_map {:activity type :targettype "config" :targetid configid :message (stringify-keys jsmap)}]
    (spy :debug (format "user %s deleting config %s in site %s" userid configid siteid))
    (spy :debug (format "these nodes %s will be reseted" nodeids))
    (when success
      (al/log "user" (merge {:userid userid} log_map))
      (when siteid
        (al/log "site" (merge {:siteid siteid :userid userid} log_map)))
      (future (dctrl/query-exec-msgpk {:nodeprops {:type "DeviceActionReq" :cmd "ColdReset" :nodeid (flatten nodeids)}})))
    (json/write-str (merge {:success success :nodes nodeids} error))))

(defn maybe-delete-config
  [jsmap]
  (let [configid (get-in jsmap [:configprops :configid])
        {:keys [nodes]
         :as config} (get-config-by-id configid)]
    (if (empty? nodes)
      (delete-config jsmap)
      (json/write-str {:error "config is attached to nodes"}))))

(defn create-item
  [{:keys [cypher] type :type userid :user :as jsmap}]
  {:pre [(#{"createConfig" "createSchedule"} type)]}
  (let [siteid (get-in jsmap [:siteprops :siteid])
        target-type (.substring type (count "create"))
        type-name (.toLowerCase target-type)
        key-name (keyword type-name)
        key-props (keyword (str type-name "props"))
        ; Override the `server` config for core nodes NSN-11604 regardless of API payload value
        overrides (get {"Schedule" {"siteid" siteid}
                        "Config"   {"siteid" siteid "server" (:server (nodeconfig/default-node-config))}}
                               target-type)
        props (merge (stringify-keys (key-props jsmap)) overrides)
        id (get props (str type-name "id"))
        {creation-exception :exception} (-> cypher
                                            (neo4j/executeQuery props)
                                            (json/read-str :key-fn keyword))
        {get-exception :exception
         :as created} (-> (or (some-> "cyphers/get_%s.cypher"
                                      (format type-name)
                                      io/resource
                                      slurp)
                              (some-> "templates/get_%s.cypher.tmpl"
                                      (format type-name)
                                      (render-resource (merge (key-props jsmap) (:siteprops jsmap)))
                                      (->> (spy :debug))))
                          (neo4j/executeQuery props)
                          (json/read-str :key-fn keyword))
        error (cond
                (some? creation-exception) {:error creation-exception}
                (some? get-exception) {:error get-exception}
               :else {})
        success (and (seq created)
                     (empty? error))
        log_map {:activity type :targettype target-type :targetid id :message props}]
    (spy :debug (format "type %s userid %s siteid %s target-type %s type-name %s key-name %s key-props %s id %s props %s"
                       type userid siteid target-type type-name (str key-name) (str key-props) id (str props)))
    (when success
      (al/log "user" (merge {:userid userid} log_map))
      (when siteid
        (al/log "site" (merge {:siteid siteid :userid userid} log_map))))
    (json/write-str (merge created {:success success} error))))

(defn update-node-on-login [messagemap]
  (let [cypher (spy :debug (render-resource "cyphers/update_node_on_login.cypher" (:nodeprops messagemap)))
        result (neo4j/executeQuery cypher (stringify-keys (:nodeprops messagemap)))
        exception (get result "exception")
        success (nil? exception)]
    (when (not success)
      (errorf "Updating node %s on LoginReq failed" (:nodeid (:nodeprops messagemap))))))

(defn calculate-lg-update
  [base-casel
   new-id
   old-id
   slg-id
   incoming
   nodeList
   groupid
   slg-groupid
   oldgroup
   lighting-policy]
  {:pre [(satisfies? LightingPolicy
                     lighting-policy)]}
  (let [type (lc/neo4j-label lighting-policy)]
    (cond ;; Either we're not updating *this* policy
      (and (= new-id
              old-id)
           (not (seq incoming))) {:type type
                                  :case :not-me}
      ;; or we're just adding to the nodes
      (and (or (= new-id
                  old-id)
               (not oldgroup))
           (seq incoming)
           (or new-id
               slg-id)) {:type type
                         :case :adding-nodes
                         :casel (lc/to-nodes lighting-policy
                                             base-casel
                                             (or new-id
                                                 slg-id)
                                             incoming)}
      ;; or we're adding to the nodes, to an empty profile with no SLG profile
      (and (or (= new-id
                  old-id)
               (not oldgroup))
           (seq incoming)
           (not (or new-id
                    slg-id))) {:type type
                               :case :adding-nodes-to-non-existent
                               :cypher (when (satisfies? OptionalLightingPolicy
                                                         lighting-policy)
                                         (lc/delete-nodes-cypher lighting-policy))
                               :action (when (satisfies? Overrides
                                                         lighting-policy)
                                         (lc-utils/reset-lights incoming))}
      ;; Or we're removing the policy from the lg and there's no backup slg policy
      (and (nil? new-id)
           (some? old-id)
           (nil? slg-id)) {:type type
                           :case :removing-lg-id-with-no-fallback-slg
                           :cypher (lc/delete-group lighting-policy)
                           :action #(do
                                      (when (satisfies? Overrides
                                                        lighting-policy)
                                        (lc-utils/reset-lights (lc/get-nodes (lc/->LightingGroup groupid))))
                                      (pd/invalidate-pdprofile-cache-for-id old-id))}
      ;; Or we're removing the schedule from the lg, but we've got a slg id
      (and (nil? new-id)
           (some? old-id)) {:type type
                            :case :removing-lg-id-to-inherit-slg
                            :cypher (lc/delete-group lighting-policy)
                            :action #(do
                                       (when (satisfies? Overrides
                                                        lighting-policy)
                                         (lc-utils/reset-lights (lc/get-nodes (lc/->LightingGroup groupid))))
                                       (pd/invalidate-pdprofile-cache-for-id old-id))
                            :casel (lc/to-nodes lighting-policy
                                                base-casel
                                                slg-id
                                                ;; If we're removing a schedule, then we need to send to *all* nodes.
                                                nodeList)}
      ;; Or, we're replacing (or adding) the schedule of the SLG,
      ;; which means we need to update *all* the nodes in LGs that
      ;; don't have a declared schedule.
      (and new-id
           (not= new-id
                 old-id)
           (= slg-groupid
              groupid)) {:type type
                         :case :replacing-slg-id
                         :casels (let [group-update (lc/to-group lighting-policy
                                                                 base-casel
                                                                 new-id
                                                                 [groupid])
                                       {:keys [siteid]} (:siteprops base-casel)
                                       other-nodes (-> (render-resource "templates/get_inheriting_lg_nodes.cypher.tmpl"
                                                                        {:type type})
                                                       (neo4j/executeQuery {"siteid" siteid})
                                                       (json/read-str :key-fn keyword)
                                                       :nodeids
                                                       (->> (map :nodeid))
                                                       seq)
                                       node-update (lc/to-nodes lighting-policy
                                                                base-casel
                                                                new-id
                                                                other-nodes)]
                                   (if other-nodes
                                     [group-update
                                      node-update]
                                     [group-update]))}
      ;; Or we're replacing the schedule (or adding one)
      (and new-id
           (not= new-id
                 old-id)) {:type type
                           :case :replacing-id
                           :casel (lc/to-group lighting-policy
                                               base-casel
                                               new-id
                                               [groupid])}
      :else (throw (ex-info "Unhandled case in `calculate-lg-update`"
                            {:args [base-casel
                                    new-id
                                    old-id
                                    slg-id
                                    incoming
                                    nodeList
                                    groupid
                                    slg-groupid
                                    oldgroup
                                    type]})))))

(defn lighting-group-diff-update
  "Call after changing Lighting Group membership. This will propagate
  the specified schedule/profile to the nodes new to the group, and
  propagate the Site Lighting Group schedule/profiles to the nodes
  leaving the group."
  [{:keys [incoming
           outgoing]
    :as diffs}
   {userid :user
    :keys [type]
    {:keys [orgid]} :orgprops
    {:keys [siteid]} :siteprops
    {:keys [groupid
            nodeList]
     [{new-scheduleid :scheduleid}] :schedules
     [{new-etdhprofileid :etdhprofileid}] :etdhprofiles
     [{new-dhprofileid :dhprofileid}] :dhprofiles
     [{new-pdprofileid :pdprofileid}] :pdprofiles
     :as groupprops} :groupprops
    {[{old-scheduleid :scheduleid}] :schedules
     [{old-etdhprofileid :etdhprofileid}] :etdhprofiles
     [{old-dhprofileid :dhprofileid}] :dhprofiles
     [{old-pdprofileid :pdprofileid}] :pdprofiles
     :as oldgroup} :oldgroup
    :as jsmap}]
  {:pre [orgid siteid
         (or (nil? incoming)
             (coll? incoming))]}
  (let [{:keys [exception]
         {slg-scheduleid :scheduleid
          slg-etdhprofileid :etdhprofileid
          slg-dhprofileid :dhprofileid
          slg-pdprofileid :pdprofileid
          slg-groupid :groupid} :result} (-> "cyphers/get_sitelightinggroup_schedule_and_profiles.cypher"
                                             io/resource
                                             slurp
                                             (neo4j/executeQuery {"siteid" siteid})
                                             (json/read-str :key-fn keyword))
        base-casel (select-keys jsmap
                                [:user
                                 :orgprops
                                 :siteprops])
        schedule-action (calculate-lg-update base-casel
                                             new-scheduleid
                                             old-scheduleid
                                             slg-scheduleid
                                             incoming
                                             nodeList
                                             groupid
                                             slg-groupid
                                             oldgroup
                                             (lc/->Schedule))
        etdhprofile-action (calculate-lg-update base-casel
                                                new-etdhprofileid
                                                old-etdhprofileid
                                                slg-etdhprofileid
                                                incoming
                                                nodeList
                                                groupid
                                                slg-groupid
                                                oldgroup
                                                (lc/->ETDHProfile))
        dhprofile-action (calculate-lg-update base-casel
                                              new-dhprofileid
                                              old-dhprofileid
                                              slg-dhprofileid
                                              incoming
                                              nodeList
                                              groupid
                                              slg-groupid
                                              oldgroup
                                              (lc/->DHProfile))
        pdprofile-action (calculate-lg-update base-casel
                                              new-pdprofileid
                                              old-pdprofileid
                                              slg-pdprofileid
                                              incoming
                                              nodeList
                                              groupid
                                              slg-groupid
                                              oldgroup
                                              (lc/->PDProfile))]
    (when (and (= slg-groupid
                  groupid)
               (nil? new-scheduleid))
      (doto (ex-info "Site lighting group must have a schedule"
                     {:orgid orgid
                      :siteid siteid
                      :groupid groupid
                      :scheduleid new-scheduleid})
        error
        throw))
    ;; We should *always* have a Schedule assigned to the SLG
    (when-not slg-scheduleid
      (doto (ex-info "Site lighting group has no schedule"
                     {:orgid orgid
                      :siteid siteid
                      :groupid slg-groupid})
        error
        throw))
    (->> [schedule-action
          etdhprofile-action
          dhprofile-action
          pdprofile-action]
         (map (fn [{:keys [action
                           casel
                           casels
                           cypher
                           cyphers]}]
                ;; Perform `action` before `casel`, before
                ;; `cypher`. Unfortunately, `action`s require info
                ;; from existing Neo4j relationships.
                (when action
                  (action))
                (when casel
                  (-> casel
                      template-selector
                      db-selector))
                (when (seq casels)
                  (->> casels
                       (map #(-> %
                                 template-selector
                                 db-selector))
                       dorun))
                (when cypher
                  (-> cypher
                      (neo4j/executeQuery {"groupid" groupid
                                           "incoming" incoming})))
                (when (seq cyphers)
                  (->> cyphers
                       (map #(neo4j/executeQuery % {"groupid" groupid
                                                    "incoming" incoming}))
                       dorun))))
         dorun)
    ;; Toss the outgoing nodes back into the SLG
    (when (seq outgoing)
      (let [{:keys [success]
             cypher-error :error} (-> {:type "addNodesToGroup"
                                       :user userid
                                       :orgprops {:orgid orgid}
                                       :siteprops {:siteid siteid}
                                       :groupprops {:groupid slg-groupid}
                                       :nodeprops {:nodeids outgoing}}
                                      template-selector
                                      db-selector
                                      (json/read-str :key-fn keyword))]
        (when-not success
          (doto (ex-info "Failed to add nodes to group"
                         {:error cypher-error
                          :nodeids outgoing
                          :groupid slg-groupid})
            error
            throw))))))

(defn parse-double
  [double-string]
  (try (Double/parseDouble double-string)
    (catch Throwable t
      (warnf "Cannot parse %s as a Double"
             double-string)
      nil)))

(defn send-gps-update [userid orgid siteid nodeid latitude longitude]
  (let [request {:instanceid (.. java.net.InetAddress getLocalHost toString)
                 :timestamp (str (time-core/now))
                 :type "GPS"
                 :model "GpsModel"
                 :action "CAN_UPDATE"
                 :user userid
                 :orgprops {:orgid orgid}
                 :siteprops {:siteid siteid}
                 :gpsprops (if (and (not= "" (str latitude)) (not= "" (str longitude))) {:nodeid nodeid :name "GpsSample" :latitude (parse-double latitude) :longitude (parse-double longitude)} {:nodeid nodeid :name "GpsSample"})}
        msg {:messageid (str (java.util.UUID/randomUUID))
             :request request}]
    (when (and latitude longitude)
      ;(spy :debug (format "send-gps-update(userid: %s, orgid: %s, siteid: %s, nodeid: %s, latitude: %s, longitude: %s" userid orgid siteid nodeid latitude longitude))
      (spy :debug msg)
      (kafka/send-to-producer @kafka/gps-topic (json/write-str (stringify-keys msg))))))

(defn update-item
  [{:keys [cypher] type :type userid :user :as jsmap}]
  {:pre [(#{"updateUser" "updateOrg" "updateSite" "updateNode" "updateOverlay"
            "updateGroup" "updateFixture" "updateFirmware" "updateNotification"
            "updateParkingZone" "updateParkingSpot" "updateParkingGroup" "updateSchedule" "updateConfig" "updateETDHProfile" "updateDHProfile"
            "updatePDProfile"}
          type)
         cypher]}
  (let [orgid (get-in jsmap [:orgprops :orgid])
        siteid (get-in jsmap [:siteprops :siteid])
        target-type (.substring type (count "update"))
        type-name (.toLowerCase target-type)
        key-name (keyword type-name)
        key-props (keyword (str type-name "props"))
        ; Override the `server` config for core nodes NSN-11604 regardless of API payload value
        overrides (if (= target-type "Config")
                    {"siteid" siteid "server" (:server (nodeconfig/default-node-config))}
                    {"siteid" siteid})
        props (merge (stringify-keys (key-props jsmap)) overrides)
        id (get props (str type-name "id"))
        {:keys [diffs]
         :as results} (-> cypher
                          (neo4j/executeQuery (update-location-data props siteid type))
                          (json/read-str :key-fn keyword))
        get-updated #(-> (or (some-> "cyphers/get_%s.cypher"
                                     (format type-name)
                                     io/resource
                                     slurp)
                             (some-> "templates/get_%s.cypher.tmpl"
                                     (format type-name)
                                     (render-resource (merge (key-props jsmap) (:siteprops jsmap)))
                                     (->> (spy :debug))))
                         (neo4j/executeQuery props)
                         (json/read-str :key-fn keyword))
        updated (get-updated)
        exception (or (:exception results)
                      (:exception updated))
        error (if-not (nil? exception) {:error exception} {})
        success  (not (nil? (and (seq updated) (nil? exception))))
        log_map {:activity type :targettype target-type :targetid id :message props}]
    (spy :debug (format "type %s userid %s siteid %s target-type %s type-name %s key-name %s key-props %s id %s props %s"
                       type userid siteid target-type type-name (str key-name) (str key-props) id (str props)))
    (when success
      (al/log "user" (merge {:userid userid} log_map))
      (when siteid
        (al/log "site" (merge {:siteid siteid :userid userid} log_map)))
      (when (seq diffs)
        (lighting-group-diff-update diffs jsmap))
      (when (= "updateETDHProfile"
               type)
        (etdh/handle-cape type jsmap))
      (when (= "updateNode" type)
        (send-gps-update userid orgid siteid (get props "nodeid") (get props "latitude") (get props "longitude")))
      (when (#{"updatePDProfile"
               "updateNode"}
               type)
        (pd/handle-cape type jsmap)))
    (json/write-str (merge (if (and success
                                    (seq diffs))
                             (get-updated)
                             updated)
                           {:success success}
                           error))))

(defn update-schedule
  [jsmap]
  (let [updated-sch (-> jsmap
                        update-item)
        {:keys [schedule success]} (json/read-str updated-sch :key-fn keyword)
        {:keys [events network nodes]} schedule
        userid (spy :debug (:user jsmap))
        orgid (spy :debug (:orgid (:orgprops jsmap)))
        siteid (spy :debug (:siteid (:siteprops jsmap)))
        nodeids (spy :debug (map :nodeid nodes))
        scheduleid (spy :debug (:scheduleid (:scheduleprops jsmap)))
        nodes (spy :debug (sch/get-nodes-model nodeids))
        nodemodels (spy :debug (into {} (map #(hash-map (:nodeid %) (:model %)) (spy :debug nodes))))
        nodeids-sch (spy :debug (filter #(= (get nodemodels %) "cnext") nodeids))
        nodeids-nosch (spy :debug (filter #(not= (get nodemodels %) "cnext") nodeids))
        siteLocation (get-site-location (get-in jsmap [:siteprops :siteid]))
        latitude (get siteLocation "latitude")
        longitude (get siteLocation "longitude")]
    (when (and success (not (empty? nodeids)))
      (when (seq nodeids-nosch)
        (spy :debug "Sending schedule to DD cape...")
        (cape-future sch/send-daily-schedule events network latitude longitude (spy :debug nodeids-nosch)))
      (when (seq nodeids-sch)
        (spy :debug "Sending schedule to kafka...")
        (sch/send-schedule-to-lss userid orgid siteid events network latitude longitude nodeids-sch scheduleid)))
    updated-sch))

(defn dissoc-wifi-info
  [conf]
  (dissoc conf
          :networkXSSID
          :networkXSecurity
          :networkYSSID
          :networkYSecurity
          :networkXPasskey
          :networkYPasskey))

(defn get-default-config
  [model]
  (let [cypher (render-resource "templates/get_default_configs.cypher.tmpl" {:model model})
        results (->> cypher
                     neo4j/executeQuery
                     json/read-str)
        conf (get results "config" {})
        default-conf (stringify-keys (nodeconfig/get-full-conf-by-model model))
        full-conf (merge default-conf conf)]
    (case model
      "unode-v5" (dissoc-wifi-info full-conf)
      "unode-v6" (dissoc-wifi-info full-conf)
      full-conf)))

(defn get-config-for-node
  [nodeid model]
  (let [{:keys [exception
                config]} (-> "cyphers/get_assigned_config_to_node.cypher"
                             io/resource
                             slurp
                             (neo4j/executeQuery {"nodeid" nodeid})
                             (json/read-str :key-fn keyword))]
    (cond
      (some? exception) (do
                          (error exception)
                          {})
      (empty? config) (let [default-config (get-default-config model)
                            default-code (get default-config "network_region")
                            {code :country_code
                             :or {code default-code}} (-> "cyphers/get_region_code_for_node.cypher"
                                                          io/resource
                                                          slurp
                                                          (neo4j/executeQuery {"nodeid" nodeid})
                                                          (json/read-str :key-fn keyword))]
                        (nodeconfig/translate-config (assoc default-config "network_region" code)))
      :else (nodeconfig/translate-config config))))

(defn send-config-to-node
  [msg]
  (let [nodeid (:nodeid msg)
        model (:clientType msg)
        oldCfgToken (:configToken msg)
        config (get-config-for-node nodeid model)
        {cfgToken :token kvpairs :kvpairs} (cfg/calculate-token config)
        props {:nodeprops {:configtype "configupdate" :type "ConfigResp" :nodeid [nodeid] :kvpairs (merge {:token cfgToken} kvpairs)}}
        error (cond
                (empty? config) {:error "Config not found"}
                (= oldCfgToken cfgToken) {:error (format "Config tokens match %s %s" oldCfgToken cfgToken)}
                :else {})]
    (if (empty? error)
      (dctrl/query-exec-msgpk props)
      (:error error))))

(defn duplicate-config
  [{new-config :configprops
    :as jsmap}
   {old-name :name
    old-configid :configid
    :as old-config}]
  (let [new-configid (str (java.util.UUID/randomUUID))
        new-name (get-in new-config [:name] (str old-name "-" (cformat/unparse (cformat/formatter "MM/dd/yyyy HH:mm:ss z")
                                              (local/local-now))))
        configprops (merge old-config new-config
                           {:configid new-configid
                            :name new-name})]
    (debugf "duplicating config %s %s to %s %s"
           old-name old-configid
           new-name new-configid)

    (-> (merge jsmap
               {:type "createConfig"
                :configprops configprops})
        template-selector
        create-item)))

(defn maybe-update-config
  [{{:keys [configid]} :configprops
    :as jsmap}]
  (let [{:keys [nodes]
         :as config} (get-config-by-id configid)]
    (if (empty? nodes)
      (update-item jsmap)
      (duplicate-config jsmap (dissoc config :nodes)))))

(defn update-group
  [{:keys [type]
    :as jsmap}]
  {:pre [(#{"updateGroup"} type)]}
  (let [{:keys [groupid]
         proposed-type :type
         proposed-nodelist :nodeList
         proposed-schedule :schedules
         proposed-dhprofile :dhprofiles
         proposed-pdprofile :pdprofiles} (:groupprops jsmap)
        {current-type :type
         current-nodelist :nodeList
         current-schedule :schedules
         current-dhprofile :dhprofiles
         current-pdprofile :pdprofiles
         :as oldgroup} (-> {:type "getGroup"
                            :groupprops {:groupid groupid}}
                           template-selector
                           db-selector
                           (json/read-str :key-fn keyword)
                           :group)
        ;; Cache the old group for `lighting-group-diff-update`.
        jsmap (assoc jsmap :oldgroup oldgroup)]
    (cond
      (and (= "site-lighting"
              current-type)
           (not= "site-lighting"
                 proposed-type)) (json/write-str {:error "Site lighting group type cannot be changed."})
      (and (= "site-lighting"
              current-type)
           (= []
              proposed-schedule)) (json/write-str {:error "Site lighting group must be assigned schedule."})
      (and (= "site-lighting"
              current-type)
           (not= (set proposed-nodelist)
                 (set current-nodelist))) (let [added (set/difference (set proposed-nodelist) (set current-nodelist))
                                                removed (set/difference (set current-nodelist) (set proposed-nodelist))
                                                msg_added (if (> (count added) 0) "Use 'Add Node to Group' API to add nodes to Site Lighting Group." "")
                                                msg_removed (if (> (count removed) 0) "Node can not be removed from Site Lighting group." "")
                                                msg (if (and (> (count added) 0) (> (count removed) 0)) "Node can not be added to or removed from Site Lighting group." (trim (str msg_added " " msg_removed)))]
                                            (json/write-str {:error msg}))
      (not= current-type
            proposed-type) (json/write-str {:error "Updating a group with a different type is not supported."})
      :else (let [pdprofileids (delay (->> (set/difference (set proposed-nodelist)
                                                           (set current-nodelist))
                                           (map pd/get-profile-from-node)
                                           (remove nil?)
                                           set))
                  ret (delay (update-item jsmap))]
              (when (#{"lighting"
                       "site-lighting"}
                     current-type)
                (deref pdprofileids))
              (deref ret)
              (when (realized? pdprofileids)
                (doseq [pdprofileid @pdprofileids]
                  (pd/invalidate-pdprofile-cache-for-id pdprofileid)))
              @ret))))

(defn get-node
  [{:keys [type cypher] userid :user :as jsmap}]
  (let [{:keys [nodeid]} (:nodeprops jsmap)
        results (-> cypher
                    (neo4j/executeQuery {"nodeid" nodeid})
                    json/read-str)
        success (not (empty? results))
        node (get results "node" {})
        model (get node "model")
        config (get-config-for-node nodeid model)
        {cfgToken :token} (cfg/calculate-token config)
        error (if-not (true? success) {:error (format "node '%s' not found" nodeid) :status 404} {})
        confStatus (if (= cfgToken (get node "configToken"))
                     "ok"
                     "pending")
        resp (merge {"node" (assoc node "configStatus" confStatus) :success success} error)]
    (json/write-str resp)))

(defn get-config
  [{:keys [type cypher] userid :user :as jsmap}]
  (let [configid (get-in jsmap
                         [:configprops :configid])
        siteid (get-in jsmap [:siteprops :siteid])

        results (-> cypher
                    (neo4j/executeQuery {"configid" configid "siteid" siteid})
                    json/read-str)
        success (not (empty? results))
        config (get results "config" {})
        nodes (get config "nodes")

        {cfgToken :token} (cfg/calculate-token (nodeconfig/translate-config config))
        error (if-not (true? success) {:error (format "config '%s' not found" configid) :status 404} {})
        updated-nodes (do
                        (map
                          (fn [node]
                            (let [confStatus (if (= cfgToken (get node "configToken"))
                                               "ok"
                                               "pending")]
                              (assoc node "configStatus" confStatus)))
                          nodes))
        resp (merge {"config" (assoc config "nodes" updated-nodes) :success success} error)]
    (json/write-str resp)))

(defn prepare-etdhprofile-for-read
  [js-resp]
  (let [{:keys [success]
         :as resp} (json/read-str js-resp
                                  :key-fn keyword)]
    (if success
      (json/write-str (update-in resp
                                 [:etdhprofile
                                  :scheduled] json/read-str))
      js-resp)))

(defn get-item
  [{:keys [type
           cypher]
    :as jsmap}]
  {:pre [(#{"getUser" "getUserEmail" "getOrg" "getSite" "getOverlay" "getGroup"
            "getFixture" "getFirmware" "getNotification" "getAlert" "getParkingGroup" "getParkingSpot"
            "getSchedule" "getETDHProfile" "getDHProfile" "getPDProfile"}
          type)
         cypher]}
  (let [target-type (.substring type (count "get"))
        type-name (.toLowerCase target-type)
        key-props (keyword (str type-name "props"))
        props (merge {"siteid" (get-in jsmap [:siteprops :siteid])}
                                 (stringify-keys (key-props jsmap)))

        results (-> cypher
                    (neo4j/executeQuery props)
                    json/read-str)
        key-name (keyword type-name)
        id (get-in jsmap [(keyword (str type-name "props"))
                          (keyword (str type-name "id"))])
        error (cond
                (empty? results) {:error (format "%s '%s' not found" target-type id)
                                  :status 404}
                (get results "exception") {:error (format "Neo4j exception: %s"
                                                          (get results "exception"))}
                :else {})
        success (empty? error)
        resp (merge {key-name (get results type-name {})
                     :success success}
                    error)]
    (json/write-str resp)))


(defn group-node-config-by-model
  [nodes config old-token]

  (let [group-node-config
        (fn [[k v]]
          (let [config-pairs (merge (nodeconfig/default-sensor-config k) config)
                config-pairs-sorted (sort config-pairs)
                config-token (format "%x" (digest/crc32 (json/write-str config-pairs-sorted)))
                config-pairs-with-token (merge {:token config-token} config-pairs)
                resolved (if (= old-token config-token) [] [{:model k
                                                             :nodeids (map :nodeid v)
                                                             :config config-pairs-with-token}])]
            resolved))]

    (mapcat group-node-config (group-by :model nodes))))

(defn send-redirect-to-node
  [nodeid model server]
  (let[config (get-config-for-node nodeid model)
       config-with-server (assoc config :server server)
       {cfgToken :token kvpairs :kvpairs} (cfg/calculate-token config-with-server)
       props {:nodeprops {:configtype "serverupdate" :type "ConfigResp" :nodeid [nodeid] :kvpairs (merge {:token cfgToken} kvpairs)}}
       error (cond
               (empty? config) {:error (format "Empty config found for nodeid %s" nodeid)}
               (nil? server) {:error  (format "Unknown platform server found!")}
               :else {})]
    (if (empty? error)
      (do
        ;(errorf "Sending config %s" props)
       (dctrl/query-exec-msgpk props)
        {:nodeid nodeid :server server})
      error)))

(defn redirect-nodes-to-server
  [nodeList server]
  (let [node-model-pairs (-> "cyphers/get_nodes_model.cypher"
                             io/resource
                             slurp
                             (neo4j/executeQuery {"nodeids" nodeList})
                             (json/read-str :key-fn keyword)
                             (get :nodes))]
    (doall (map (fn [{:keys [nodeid model]}]
           (do
           ;(errorf "I get here nodeid %s model %s server %s" nodeid model server)
           (send-redirect-to-node nodeid model server))) node-model-pairs))))

(defn apply-server
  [{:keys [type nodeprops cypher] userid :user :as jsmap}]
  {:pre [(#{"applyServerToNodes"} type)]}
  (when nodeprops
    (let [siteid (get-in jsmap [:siteprops :siteid])
          nodeList (get-in jsmap [:nodeprops :nodeList])
          server (get-in jsmap [:nodeprops :server])
          props {"nodeList" nodeList "server" server}
          status-list (redirect-nodes-to-server nodeList server)
          error-list (filter #(:error %) status-list)
          error (cond
                  (not-empty error-list) (first status-list)
                  :else {})
          success (empty? error)
          log_map {:activity type
                   :targettype type
                   :targetid "node"
                   :message (stringify-keys props)}]
      (when success
        (al/log "user" (merge {:userid userid} log_map))
        (when siteid
          (al/log "site" (merge {:siteid siteid :userid userid} log_map))))
      (json/write-str (merge {:config status-list :success success} error)))))

(defn apply-config
  [{:keys [type
           cypher]
    {:keys [configid]
     oldCfgToken :configToken} :configprops
    {:keys [siteid]} :siteprops
    {:keys [groupid]} :groupprops
    {:keys [nodeids]} :nodeprops
    userid :user
    :as jsmap}]
  {:pre [(#{"applyConfigToSite" "applyConfigToGroup" "applyConfigToNodes"} type)
         cypher]}
  (when configid
    (let [target-type (.substring type (count "applyConfigTo"))
          {:keys [target-id]} (get {"Site" {:target-id siteid}
                                    "Group" {:target-id groupid}
                                    "Nodes" {:target-id (clojure.string/join "/" nodeids)}}
                                   target-type)
          {:keys [nodes
                  exception]} (-> cypher
                                  (neo4j/executeQuery {"nodeids" nodeids})
                                  (json/read-str :key-fn keyword))
          config (nodeconfig/translate-config (get-config-by-id configid))
          nodeids2 (sort (mapv :nodeid nodes))
          config-model (get config :model)
          updated-config (case (:model config)
                           "unode-v5" (dissoc-wifi-info config)
                           "unode-v6" (dissoc-wifi-info config)
                           config)
          {cfgToken :token
           kvpairs :kvpairs} (cfg/calculate-token updated-config)
          error (cond
                  (some? exception) {:error exception}
                  (empty? config) {:error (format "Config '%s' not found" configid)}
                  (empty? nodes) {:error (format "No nodes found for %s '%s'" target-type target-id)}
                  (= oldCfgToken cfgToken) {:error (format "Config tokens match %s %s" oldCfgToken cfgToken)}
                  (= config-model "falcon-q") {:error (format "Config model %s not supported" config-model)}
                  :else {})
          success (empty? error)
          props {:nodeprops {:configtype "configupdate" :type "ConfigResp" :nodeid nodeids2 :kvpairs (merge {:token cfgToken} kvpairs)}}
          log_map {:activity type
                   :targettype target-type
                   :targetid target-id
                   :message (stringify-keys kvpairs)}
          resp (merge {:config (nodeconfig/encode-config updated-config) :nodes nodes :success success} error)]
      (spy :debug (format "nodes %s config %s" nodes updated-config))
      (when success
        (al/log "user" (merge {:userid userid} log_map))
        (when siteid
          (al/log "site" (merge {:siteid siteid :userid userid} log_map)))
        (cape-future dctrl/query-exec-msgpk props))
      (json/write-str resp))))

(defn maybe-apply-config
  [{:keys [type configprops siteprops orgprops] userid :user :as jsmap}]
  {:pre [(#{"applyConfigToSite" "applyConfigToGroup" "applyConfigToNodes"} type)]}
  (let [configid (get-in jsmap [:configprops :configid])
        siteid (get-in jsmap [:siteprops :siteid])
        groupid (get-in jsmap [:groupprops :groupid])
        nodeids (get-in jsmap [:nodeprops :nodeids])
        target-type (.substring type (count "applyConfigTo"))
        {:keys [cypher-file cypher-props]} (get {"Site" {:cypher-file "cyphers/get_nodes_model_in_site.cypher"
                                                         :cypher-props {"siteid" siteid}}
                                                 "Group" {:cypher-file "cyphers/get_nodes_model_in_group.cypher"
                                                          :cypher-props {"groupid" groupid}}
                                                 "Nodes" {:cypher-file "cyphers/get_nodes_model.cypher"
                                                          :cypher-props {"nodeids" nodeids}}}
                                                target-type)
        {:keys [exception nodes]} (-> cypher-file
                            io/resource
                            slurp
                            (neo4j/executeQuery cypher-props)
                            (json/read-str :key-fn keyword))
        config (nodeconfig/translate-config (get-config-by-id configid))
        config-model (get config :model)
        right-modelnodes (map #(% :nodeid) (filter (comp #{config-model} :model) nodes) )
        ]

    (apply-config (merge jsmap {:nodeprops {:nodeids right-modelnodes}}))))

(derive! :applyETDHtoSite :apply-etdaylight-harvesting)
(derive! :applyETDHtoGroup :apply-etdaylight-harvesting)
(derive! :applyETDHtoNodes :apply-etdaylight-harvesting)

(defmethod db-selector :createGroup
  [{cypher :cypher
    userid :user
    :as jsmap}]
  (when (jsmap :groupprops)
    (if (= "site-lighting"
           (get-in jsmap [:groupprops :type]))
      (json/write-str {:error "Site lighting groups cannot be manually created."})
      (let [{:keys [groupid
                    schedules
                    pdprofiles
                    nodeList]
             grouptype :type
             :as groupprops} (:groupprops jsmap)
            siteid (get-in jsmap [:siteprops :siteid])
            pdprofileid (:pdprofileid (first pdprofiles))
            scheduleprops (first (map #(select-keys % [:scheduleid]) schedules))
            props (stringify-keys groupprops)
            result (json/read-str (neo4j/executeQuery cypher props))
            exception (get result "exception" nil)
            error (if-not (nil? exception) {:error exception} {})
            success (nil? exception)
            log_map {:activity "createGroup" :targettype "Group" :targetid groupid :message props}]
        (when success
          (al/log "user" (merge {:userid userid} log_map))
          (al/log "site" (merge {:siteid siteid :userid userid} log_map))
          (when (= "lighting" grouptype)
            (when (seq nodeList)
              (let [jsmap (-> jsmap
                              (assoc :type "addNodesToGroup")
                              (assoc-in [:groupprops :groupid] groupid)
                              (assoc-in [:nodeprops :nodeids] nodeList))
                    {:keys [success]
                     cypher-error :error} (-> jsmap
                                              template-selector
                                              db-selector
                                              (json/read-str :key-fn keyword))]
                (when-not success
                  (doto (ex-info "Failed to add nodes to group"
                                 {:error cypher-error
                                  :nodeids nodeList
                                  :groupid groupid
                                  :jsmap jsmap})
                    error
                    throw))))
            (when scheduleprops
              (let [jsmap (-> jsmap
                              (assoc :type "applyScheduleToGroup")
                              (assoc :scheduleprops scheduleprops)
                              (assoc-in [:groupprops :groupids] groupid)
                              (assoc-in [:nodeprops :nodeids] nodeList))
                    {:keys [success]
                     cypher-error :error} (-> jsmap
                                              template-selector
                                              db-selector
                                              (json/read-str :key-fn keyword))]
                (when-not success
                  (doto (ex-info "Failed to add schedule to group"
                                 {:error cypher-error
                                  :scheduleprops scheduleprops
                                  :groupid groupid
                                  :jsmap jsmap})
                    error
                    throw))))
            (when pdprofileid
              (let [jsmap (-> jsmap
                              (assoc :type "applyPDtoGroup")
                              (assoc :pdprofileprops {:pdprofileid pdprofileid})
                              (assoc-in [:groupprops :groupids] groupid)
                              (assoc-in [:nodeprops :nodeids] nodeList))
                    {:keys [success]
                     cypher-error :error} (-> jsmap
                                              template-selector
                                              db-selector
                                              (json/read-str :key-fn keyword))]
                (when-not success
                  (doto (ex-info "Failed to add pd profile to group"
                                 {:error cypher-error
                                  :groupid groupid
                                  :pdprofileid pdprofileid
                                  :jsmap jsmap})
                    error
                    throw))))
            ))
        (json/write-str (merge {:group props :success success} error))))))

(defmethod db-selector :apply-etdaylight-harvesting
  [{:keys [type
           cypher
           etdhprofileprops]
    {:keys [etdhprofileid]} :etdhprofileprops
    {:keys [siteid]} :siteprops
    {:keys [groupids]} :groupprops
    {:keys [nodeids]} :nodeprops
    userid :user
    :as jsmap}]
  {:pre [(#{"applyETDHtoSite" "applyETDHtoGroup" "applyETDHtoNodes"} type)
         cypher]}
  (when etdhprofileprops
    (let [target-type (.substring type (count "applyETDHto"))
          {:keys [target-id
                  cypher-data]} (get {"Site" {:target-id siteid
                                              :cypher-data {:siteid siteid
                                                            :etdhprofileid etdhprofileid}}
                                      "Group" {:target-id (clojure.string/join ", " groupids)
                                               :cypher-data {:siteid siteid
                                                             :groupids groupids
                                                             :etdhprofileid etdhprofileid}}
                                      "Nodes" {:target-id (clojure.string/join ", " nodeids)
                                               :cypher-data {:nodeids nodeids
                                                             :etdhprofileid etdhprofileid}}}
                                     target-type)
          {:keys [result
                  exception]
           {:keys [nodes
                   cypher-etdhprofileid]} :result
           {n :nodes} :nodes
           :as cypher-result} (-> cypher
                                  (neo4j/executeQuery (stringify-keys cypher-data))
                                  (json/read-str :key-fn keyword))
          nodeids (map :nodeid nodes)
          assignedResult (merge {:etdhprofileid etdhprofileid} cypher-data)
          log_map {:activity type
                   :targettype target-type
                   :targetid target-id
                   :message assignedResult}
          error (cond
                  (some? exception) {:error exception}
                  (empty? cypher-result) {:error (format "No ETDHProfile found for '%s'" etdhprofileid)}
                  :else {})
          success (empty? error)]
      ;; On successful execution of `cypher`:
      (when success
        ;; Log success to Cassandra
        (al/log "user" (merge {:userid userid} log_map))
        (al/log "site" (merge {:siteid siteid} log_map))
        ;; We have `n` when we are `applyETDHtoNodes`.
        (when (seq n)
          ;; For each node in `n`, group by the config it uses.
          (doseq [[nodeids
                   {:keys [config-key
                           config]}] (-> "cyphers/get_nodes_model.cypher"
                                         io/resource
                                         slurp
                                         (neo4j/executeQuery {"nodeids" (mapv :nodeid n)})
                                         (json/read-str :key-fn keyword)
                                         :nodes
                                         (->> (map (fn [{:keys [nodeid model]
                                                         :as m}]
                                                     (assoc m
                                                            :config (merge {:model model}
                                                                           (get-config-for-node nodeid model)))))
                                              ;; Group by the config
                                              ;; name and id, as the
                                              ;; id "default" is
                                              ;; shared across
                                              ;; different models.
                                              (group-by (comp #(select-keys %
                                                                            [:configid
                                                                             :name])
                                                              :config))
                                              ;; Invert the return
                                              ;; from `group-by` to
                                              ;; have the `nodeid`s as
                                              ;; the index, and the
                                              ;; config-key and config
                                              ;; object as the value.
                                              (map (fn [[config-key [{:keys [config]} & _ :as vs]]]
                                                     [(mapv :nodeid vs) {:config config
                                                                         :config-key config-key}]))
                                              (into {})))]
            ;; Ask ETDH, for this profile, given the `config-key`:
            ;; - a no-op
            ;; - We have an existing config to assign to the `nodeids`
            ;; - We need to create a new config, assign to the
            ;;   `nodeids`, and record this new configid with ETDH
            (when-let [{:keys [etdh-configid
                               new-config]} (etdh/etdh-config etdhprofileid config-key)]
              (when (or etdh-configid
                        new-config)
                (let [new-configid (or etdh-configid
                                       (-> (duplicate-config (assoc jsmap
                                                                    :configprops new-config)
                                                             config)
                                           (json/read-str :key-fn keyword)
                                           :config
                                           :configid))]
                  (-> jsmap
                      (assoc :type "applyConfigToNodes"
                             :configprops {:configid new-configid}
                             :nodeprops {:nodeids nodeids})
                      template-selector
                      db-selector)
                  (when-not etdh-configid
                    (etdh/set-etdh-config! etdhprofileid config-key new-configid)))))))
        ;; We resolve `nodeids` when we are `applyETDHtoSite` or
        ;; `applyETDHtoGroup`.
        (when (seq nodeids)
          (let [jsmap (assoc jsmap
                             :type "applyETDHtoNodes"
                             :nodeprops {:nodeids nodeids})
                {:keys [success]
                 cypher-error :error} (-> jsmap
                                          template-selector
                                          db-selector
                                          (json/read-str :key-fn keyword))]
            (when-not success
              (doto (ex-info "Failed to apply ETDH to nodes"
                             {:error cypher-error
                              :nodeids nodeids
                              :jsmap jsmap})
                error
                throw))))
        (when (or (= "Site" target-type)
                  (and (= "Group" target-type)
                       (-> "MATCH (slg:SiteLightingGroup)
WHERE slg.groupid IN {props}.groupids
RETURN slg"
                           (neo4j/executeQuery {"groupids" groupids})
                           (json/read-str :key-fn keyword)
                           :slg)))
          (when-let [other-nodes (-> (render-resource "templates/get_inheriting_lg_nodes.cypher.tmpl"
                                                      {:type "ETDHProfile"})
                                     (neo4j/executeQuery {"siteid" siteid})
                                     (json/read-str :key-fn keyword)
                                     :nodeids
                                     (->> (map :nodeid))
                                     seq)]
            (let [jsmap (-> jsmap
                            (dissoc :groupprops)
                            (assoc :type "applyETDHtoNodes"
                                   :nodeprops {:nodeids other-nodes}))
                  {:keys [success]
                   cypher-error :error} (-> jsmap
                                            template-selector
                                            db-selector
                                            (json/read-str :key-fn keyword))]
              (when-not success
                (doto (ex-info "Failed to apply schedule to nodes"
                               {:error cypher-error
                                :nodeids nodeids
                                :jsmap jsmap})
                  error
                  throw)))))

        ;; Uncomment once DCC can handle Daylight Harvesting messages.
        #_(send-via cape-thread-pool cape-agent dctrl/query-exec-msgpk-agent (assoc-in jsmap
                                                                                       [:nodeprops :nodeid]
                                                                                       nodeids)))
      (json/write-str (assoc error
                             :result (-> (assoc jsmap :type "getETDHProfile")
                                         template-selector
                                         db-selector
                                         (json/read-str :key-fn keyword)
                                         :etdhprofile)
                             :success success)))))

(defn apply-daylight-harvesting
  [{:keys [type
           cypher
           dhprofileprops]
    {:keys [dhprofileid]} :dhprofileprops
    {:keys [siteid]} :siteprops
    {:keys [groupids]} :groupprops
    {:keys [nodeids]} :nodeprops
    userid :user
    :as jsmap}]
  {:pre [(#{"applyDHtoSite" "applyDHtoGroup" "applyDHtoNodes"} type)
         cypher]}
  (when dhprofileprops
    (let [target-type (.substring type (count "applyDHto"))
          {:keys [target-id
                  cypher-data]} (get {"Site" {:target-id siteid
                                              :cypher-data {:siteid siteid
                                                            :dhprofileid dhprofileid}}
                                      "Group" {:target-id (clojure.string/join ", " groupids)
                                               :cypher-data {:siteid siteid
                                                             :groupids groupids
                                                             :dhprofileid dhprofileid}}
                                      "Nodes" {:target-id (clojure.string/join ", " nodeids)
                                               :cypher-data {:nodeids nodeids
                                                             :dhprofileid dhprofileid}}}
                                     target-type)
          {:keys [result
                  exception]
           {:keys [nodes
                   cypher-dhprofileid]} :result
           :as cypher-result} (-> cypher
                                  (neo4j/executeQuery (stringify-keys cypher-data))
                                  (json/read-str :key-fn keyword))
          nodeids (map :nodeid nodes)
          assignedResult (merge {:dhprofileid dhprofileid} cypher-data)
          log_map {:activity type
                   :targettype target-type
                   :targetid target-id
                   :message assignedResult}

                                        ;nodeids (get-in cypher-result ["result" "nodeids"] [])
          error (cond
                  (some? exception) {:error exception}
                  (empty? cypher-result) {:error (format "No DHProfile found for '%s'" dhprofileid)}
                                        ;(empty? nodeids) {:error (format "No Nodes found for %s '%s'" target-type target-id)}
                  :else {})
          success (empty? error)]
      (when success
        (al/log "user" (merge {:userid userid} log_map))
        (al/log "site" (merge {:siteid siteid :userid userid} log_map))
        (when (seq nodeids)
          (let [jsmap (assoc jsmap
                             :type "applyDHtoNodes"
                             :nodeprops {:nodeids nodeids})
                {:keys [success]
                 cypher-error :error} (-> jsmap
                                          template-selector
                                          db-selector
                                          (json/read-str :key-fn keyword))]
            (when-not success
              (doto (ex-info "Failed to apply DH to nodes"
                             {:error cypher-error
                              :nodeids nodeids
                              :jsmap jsmap})
                error
                throw))))
        (when (or (= "Site" target-type)
                  (and (= "Group" target-type)
                       (-> "MATCH (slg:SiteLightingGroup)
WHERE slg.groupid IN {props}.groupids
RETURN slg"
                           (neo4j/executeQuery {"groupids" groupids})
                           (json/read-str :key-fn keyword)
                           :slg)))
          (when-let [other-nodes (-> (render-resource "templates/get_inheriting_lg_nodes.cypher.tmpl"
                                                      {:type "DHProfile"})
                                     (neo4j/executeQuery {"siteid" siteid})
                                     (json/read-str :key-fn keyword)
                                     :nodeids
                                     (->> (map :nodeid))
                                     seq)]
            (let [jsmap (-> jsmap
                            (dissoc :groupprops)
                            (assoc :type "applyDHtoNodes"
                                   :nodeprops {:nodeids other-nodes}))
                  {:keys [success]
                   cypher-error :error} (-> jsmap
                                            template-selector
                                            db-selector
                                            (json/read-str :key-fn keyword))]
              (when-not success
                (doto (ex-info "Failed to apply schedule to nodes"
                               {:error cypher-error
                                :nodeids nodeids
                                :jsmap jsmap})
                  error
                  throw)))))

        ;; Uncomment once DCC can handle Daylight Harvesting messages.
        #_(send-via cape-thread-pool cape-agent dctrl/query-exec-msgpk-agent (assoc-in jsmap
                                                                                       [:nodeprops :nodeid]
                                                                                       nodeids)))
      (json/write-str (assoc error
                             :result (-> (assoc jsmap :type "getDHProfile")
                                         template-selector
                                         db-selector
                                         (json/read-str :key-fn keyword)
                                         :dhprofile)
                             :success success)))))

(defn apply-proximity-dimming
  [{:keys [type
           cypher
           pdprofileprops]
    {:keys [pdprofileid]} :pdprofileprops
    {:keys [siteid]} :siteprops
    {:keys [groupids]} :groupprops
    {:keys [nodeids]} :nodeprops
    userid :user
    :as jsmap}]
  {:pre [(#{"applyPDtoGroup" "applyPDtoNodes" "applyPDtoSite"} type)
         cypher]}
  (when pdprofileprops
    (let [target-type (.substring type (count "applyPDto"))
          is-site-group (or (= "Site" target-type)
                            (and (= "Group" target-type)
                                 (-> "MATCH (slg:SiteLightingGroup)
                                              WHERE slg.groupid IN {props}.groupids
                                              RETURN slg"
                                     (neo4j/executeQuery {"groupids" groupids})
                                     (json/read-str :key-fn keyword)
                                     :slg)))
          {:keys [target-id
                  cypher-data]} (get {"Site" {:target-id siteid
                                              :cypher-data {:siteid siteid
                                                            :pdprofileid pdprofileid}}
                                      "Group" {:target-id (clojure.string/join ", " groupids)
                                               :cypher-data {:siteid siteid
                                                             :groupids groupids
                                                             :pdprofileid pdprofileid}}
                                      "Nodes" {:target-id (clojure.string/join ", " nodeids)
                                               :cypher-data {:nodeids nodeids
                                                             :pdprofileid pdprofileid}}}
                                     target-type)
          {:keys [exception]
           {:keys [pdprofiles
                   nodes]} :result
           :as cypher-result} (if is-site-group nil (-> cypher
                                                        (neo4j/executeQuery (stringify-keys cypher-data))
                                                        (json/read-str :key-fn keyword)))
          nodeids (map :nodeid nodes)
          nodemodels (into {} (map #(hash-map (:nodeid %) (:model %)) nodes))
          nodeids-sch (filter #(= (get nodemodels %) "cnext") nodeids)
          assignedResult (merge {:pdprofileid pdprofileid} cypher-data)
          log_map {:activity type
                   :targettype target-type
                   :targetid target-id
                   :message assignedResult}

                                        ;nodeids (get-in cypher-result ["result" "nodeids"] [])
          error (cond
                  is-site-group
                  {:error "Updating a site lighting group with a proximity dimming profile is not supported."}
                  (some? exception) {:error exception}
                  (empty? cypher-result) {:error (format "No PDProfile %s found for %s '%s'" pdprofileid target-type target-id)}
                  (seq nodeids-sch)
                  {:error "Applying a proximity dimming profile to nodes containing a NGCN node is not currently supported."}
                                        ;(empty? nodeids) {:error (format "No Nodes found for %s '%s'" target-type target-id)}
                  :else {})
          success (empty? error)]
      (when success
        (al/log "user" (merge {:userid userid} log_map))
        (al/log "site" (merge {:siteid siteid :userid userid} log_map))
        (when (and (seq nodeids)
                   (not= "Nodes"
                         target-type))
          (let [jsmap (assoc jsmap
                             :type "applyPDtoNodes"
                             :nodeprops {:nodeids nodeids})
                {:keys [success]
                 cypher-error :error} (-> jsmap
                                          template-selector
                                          db-selector
                                          (json/read-str :key-fn keyword))]
            (when-not success
              (doto (ex-info "Failed to apply PD to nodes"
                              {:error cypher-error
                               :jsmap jsmap})
                error
                throw)))
          (->> pdprofiles
               (map :pdprofileid)
               (remove nil?)
               (map pd/invalidate-pdprofile-cache-for-id)
               dorun))
        (when is-site-group
          (when-let [other-nodes (-> (render-resource "templates/get_inheriting_lg_nodes.cypher.tmpl"
                                                      {:type "PDProfile"})
                                     (neo4j/executeQuery {"siteid" siteid})
                                     (json/read-str :key-fn keyword)
                                     :nodeids
                                     (->> (map :nodeid))
                                     seq)]
            (let [jsmap (-> jsmap
                            (dissoc :groupprops)
                            (assoc :type "applyPDtoNodes"
                                   :nodeprops {:nodeids other-nodes}))
                  {:keys [success]
                   cypher-error :error} (-> jsmap
                                            template-selector
                                            db-selector
                                            (json/read-str :key-fn keyword))]
              (when-not success
                (doto (ex-info "Failed to apply schedule to nodes"
                               {:error cypher-error
                                :nodeids nodeids
                                :jsmap jsmap})
                  error
                  throw)))))

        ;; Uncomment once DCC can handle Proximity Dimming messages.
        #_(send-via cape-thread-pool cape-agent dctrl/query-exec-msgpk-agent (assoc-in jsmap [:nodeprops :nodeid] nodeids))
        (when (= "applyPDtoNodes"
                 type)
          (pd/invalidate-cache-profile-from-nodes nodeids)
          (pd/invalidate-cache-pd-nodes pdprofileid)
          (pd/handle-cape type jsmap)))
      (json/write-str (assoc error
                             :result (-> (assoc jsmap :type "getPDProfile")
                                         template-selector
                                         db-selector
                                         (json/read-str :key-fn keyword)
                                         :pdprofile)
                             :success success)))))

(defn assign-fixture
  [{:keys [type
           cypher
           fixtureprops]
    {:keys [fixtureid]} :fixtureprops
    {:keys [siteid]} :siteprops
    {:keys [groupids]} :groupprops
    {:keys [nodeid]} :nodeprops
    {:keys [nodeids]} :nodeprops
    userid :user
    :as jsmap}]
  {:pre [(#{"assignFixtureToGroup" "assignFixtureToSite" "assignFixtureToNode" "assignFixtureToNodes"} type)]}
  (when fixtureprops
    (let [target-type (.substring type (count "assignFixtureTo"))
          {:keys [cypher-resource
                  target-id
                  cypher-data
                  name-type]} (get {"Site" {:cypher-resource "templates/get_all_nodeids_for_site.cypher.tmpl"
                                            :target-id siteid
                                            :cypher-data {:siteid siteid}
                                            :name-type "items"}
                                    "Group" {:cypher-resource "cyphers/get_all_nodes_in_group.cypher"
                                             :target-id (clojure.string/join ", " groupids)
                                             :cypher-data {:groupids groupids}
                                             :name-type "items"}
                                    "Node" {:cypher-resource "cyphers/get_node.cypher"
                                            :target-id nodeid
                                            :cypher-data {:nodeid nodeid}
                                            :name-type "node"}
                                    "Nodes" {:cypher-resource "cyphers/get_nodes.cypher"
                                            :target-id (clojure.string/join ", " nodeids)
                                            :cypher-data {:nodeids nodeids}
                                            :name-type "items"}}
                                   target-type)
          cypher-result (-> cypher-resource
                            (render-resource cypher-data)
                            (neo4j/executeQuery (stringify-keys cypher-data))
                            json/read-str)
          assignedResult (merge {:fixtureid fixtureid} cypher-data)
          log_map {:activity type
                   :targettype target-type
                   :targetid target-id
                   :message assignedResult}
          items (get cypher-result name-type [])
          nodes (if (map? items) [items] items)
          nodeids (map #(get % "nodeid") nodes)
          props {"nodeids" nodeids}
          result (json/read-str (neo4j/executeQuery cypher props))
          exception (get cypher-result "exception" (get result "exception" nil))
          error (cond
                  (some? exception) {:error exception}
                  (empty? cypher-result) {:error (format "No Fixture found for '%s'" fixtureid)}
                  (empty? result) {:error (format "Fixture assignment failed for Fixture '%s'" fixtureid)}
                  (empty? nodeids) {:error (format "No Nodes found for %s '%s'" target-type target-id)}
                  :else {})
          success (empty? error)]
      (when success
        (al/log "user" (merge {:userid userid} log_map))
        (when siteid
          (al/log "site" (merge {:siteid siteid :userid userid} log_map))))
      (json/write-str (merge {:success success :result assignedResult} error)))))

(defn assign-firmware
  [{:keys [type
           cypher
           firmwareprops]
    {:keys [firmwareid description]} :firmwareprops
    {:keys [siteid]} :siteprops
    {:keys [groupids]} :groupprops
    {:keys [nodeid]} :nodeprops
    userid :user
    :as jsmap}]
  {:pre [(#{"assignFirmwareToGroup" "assignFirmwareToSite" "assignFirmwareToNode"} type)]}
  (when firmwareprops
    (let [target-type (.substring type (count "assignFirmwareTo"))
          {:keys [cypher-resource
                  target-id
                  cypher-data
                  name-type]} (get {"Site" {:cypher-resource "cyphers/get_all_nodes_for_site.cypher"
                                            :target-id siteid
                                            :cypher-data {:siteid siteid}
                                            :name-type :items}
                                    "Group" {:cypher-resource "cyphers/get_all_nodes_in_group.cypher"
                                             :target-id (clojure.string/join ", " groupids)
                                             :cypher-data {:groupids groupids}
                                             :name-type :items}
                                    "Node" {:cypher-resource "cyphers/get_node.cypher"
                                            :target-id nodeid
                                            :cypher-data {:nodeid nodeid}
                                            :name-type :node}}
                                   target-type)
          cypher-result (-> cypher-resource
                            (render-resource cypher-data)
                            (neo4j/executeQuery (stringify-keys cypher-data))
                            (json/read-str :key-fn keyword))
          assignedResult (merge {:firmwareid firmwareid} cypher-data)
          log_map {:activity type
                   :targettype target-type
                   :targetid target-id
                   :message assignedResult}
          items (get cypher-result name-type [])
          filtered-nodes (filter (fn [n] (= (get n :softwareVersion) firmwareid)) (if (map? items) [items] items))
          nodes (filter (fn [n] (not= (get n :softwareVersion) firmwareid)) (if (map? items) [items] items))
          filtered-nodeids (map #(get % :nodeid) filtered-nodes)
          nodeids (map #(get % :nodeid) nodes)
          props {"nodeids" nodeids}
          firmware-cypher-result (json/read-str (neo4j/executeQuery cypher props))
          exception (get cypher-result :exception (get firmware-cypher-result "exception" nil))
          error (cond
                  (some? exception) {:error exception}
                  (empty? items) {:error (format "No Nodes found for %s '%s'" target-type target-id)}
                  (empty? nodes) {:error (format "No upgradable Nodes found for %s '%s'" target-type target-id)}
                  (empty? firmware-cypher-result) {:error (format "Firmware '%s' not found" firmwareid)}
                  :else {})
          success (empty? error)]
      (when success
        (al/log "user" (merge {:userid userid} log_map))
        (when siteid
          (al/log "site" (merge {:siteid siteid :userid userid} log_map)))
        (doseq  [[model nodes] (group-by :model nodes)]
          (cape-future dctrl/query-exec-msgpk
                       (spy :debug (assoc jsmap
                                          :nodeprops {:target_type (lower-case target-type)
                                                      :target_id target-id
                                                      :description description
                                                      :nodeid (map :nodeid nodes)
                                                      :model model
                                                      :type "AssignFirmware"})))))
      (json/write-str (merge {:success success
                              :skipped filtered-nodeids} error)))))

(defn get-all-items
  [{:keys [type cypher] userid :user :as jsmap}]
  {:pre [(#{"getAllConfigs" "getAllFixtures" "getAllFirmwares" "getAllOverlays"
            "getAllGroups" "getAllNotificationsForUser" "getAllNotificationsForOrg" "getAllNotificationsForSite" "getAllNotificationsByName"
            "getAllParkingZones" "getAllParkingSpots" "getAllParkingGroups" "getAllOrgs" "getAllSuspendedOrgs" "getAllSitesForOrg"
            "getAllSuspendedSitesForOrg"
            "getAllMinNodesForSite" "getAllLostAndFoundNodes"
            "getAllUsersForSite" "getAllUsersForOrg" "getAllUsersForPartner"
            "getAllSuspendedUsersForOrg" "getAllSuspendedUsersForPartner"
            "getAllSchedules" "getAllDHProfileTriggers" "getAllETDHProfileTriggers"}
          type)]}
  (let [results (->> cypher
                     neo4j/executeQuery
                     json/read-str)
        exception (get results "exception" nil)
        error (cond
                (some? exception) {:error exception}
                :else {})
        success (empty? error)
        items (get results "items" [])
        item-list (if (map? items) [items] items)]
    (json/write-str (merge {:items item-list :success success} error))))

(defn get-all-items-no-template
  [{:keys [type cypher] userid :user :as jsmap}]
  {:pre [(#{"getAllNodesForSite" "getAllNodeIdsForModelSite" "getAllNodeIdsForModelGroup" }
          type)]}
  (let [props { "siteid" (get-in jsmap [:siteprops :siteid] "")
                "model" (get-in jsmap [:nodeprops :model] "")
                "groupid" (get-in jsmap [:nodeprops :groupid] "")}
         results (-> cypher
                     (neo4j/executeQuery props)
                     json/read-str)
        exception (get results "exception" nil)
        error (cond
                (some? exception) {:error exception}
                :else {})
        success (empty? error)
        items (get results "items" [])
        item-list (if (map? items) [items] items)]
    (json/write-str (merge {:items item-list :success success} error))))

(defn get-all-items-no-template-with-userprops
  [{:keys [type cypher userprops] userid :user :as jsmap}]
  {:pre [(#{"getUserEmails"}
          type)]}
  (let [ userprops (stringify-keys userprops)
         results (-> cypher
                     (neo4j/executeQuery userprops)
                     json/read-str)
        exception (get results "exception" nil)
        error (cond
                (some? exception) {:error exception}
                :else {})
        success (empty? error)
        items (get results "items" [])
        item-list (if (vector? items) items [items])]
    (json/write-str (merge {:items item-list :success success} error))))

(defn calibrate-daylight-harvesting
  [{:keys [type]
    :as jsmap}]
  {:pre [(#{"calibrateDHProfile"} type)]}
  (let [dhprofileid (get-in jsmap [:dhprofileprops :dhprofileid])
        dhprofile (dh/calibrate dhprofileid)
        error (instance? Throwable dhprofile)]
    (json/write-str (cond
                      error {:success false
                             :error (json/write-str {:message (.getMessage dhprofile)
                                                     :data (ex-data dhprofile)})}
                      dhprofile {:success true
                                 :dhprofile (-> jsmap
                                                (assoc :type "getDHProfile")
                                                template-selector
                                                db-selector
                                                (json/read-str :key-fn keyword)
                                                :dhprofile)}
                      :else {:success false}))))


(defn get-AlertSys-AlertByNodeNameType [{cypher :cypher
                                         userid :user
                                         :as jsmap}]
      (if-not (nil? (jsmap :alertprops))
              (let [props (stringify-keys (jsmap :alertprops))
                    result (json/read-str (neo4j/executeQuery cypher props))
                    alertid (get-in jsmap [:alertprops :alertid])
                    alert (get result "alert" nil)
                    error (cond
                            (empty? result) {:error (format "Alert '%s' not found" alertid)
                                             :status 404}
                            (get result "exception") {:error (format "Neo4j exception: %s"
                                                                     (get result "exception"))}
                            :else {})
                    success (empty? error)]
                   (json/write-str (merge {:alert alert :success success} error)))))


(defn delete-schedule
  [{:keys [cypher]
    userid :user
    :as jsmap}]
  (when (jsmap :scheduleprops)
    (let [{:keys [success
                  error
                  schedule]
           {:keys [groups]} :schedule
           :as cape} (-> jsmap
                         (assoc :type "getSchedule")
                         template-selector
                         db-selector
                         (json/read-str :key-fn keyword))
          groupids (map :groupid groups)]
      (if (or error
              (not success))
        (json/write-str cape)
        (let [{:keys [sites groups]} schedule]
          (cond
            (seq groups) (json/write-str {:error "Cannot delete schedule in use by a Lighting Group."})
            (seq sites) (json/write-str {:error "Cannot delete schedule in use by Site Lighting Group."})
            :else (let [scheduleid (get-in jsmap [:scheduleprops :scheduleid])
                        siteid (get-in jsmap [:siteprops :siteid])
                        props {"scheduleid" scheduleid}
                        {:keys [exception]
                         :as result} (-> (neo4j/executeQuery cypher props)
                                         (json/read-str :key-fn keyword))
                        error (if-not (nil? exception) {:error exception} {})
                        success (nil? exception)
                        scheduleprops (stringify-keys (jsmap :scheduleprops))
                        log_map {:activity "deleteSchedule"
                                 :targettype "Schedule"
                                 :targetid scheduleid
                                 :message scheduleprops}]
                    (when success
                      (al/log "user" (merge {:userid userid} log_map))
                      (al/log "site" (merge {:siteid siteid :userid userid} log_map)))
                    (json/write-str (merge {:success success} error)))))))))


;; Get catery for alert type
(defn alert-category [type]
   (case type
        ("CommFail"
          "DegradedNetwork"
          "PMACFail"
          "X509ClientFail"
          "X509ServerFail") "Network"
        ("SimFail"
          "SensorFail"
          "NotTested"
          "DownrevSoftware"
          "BadSensorData"
          "ConfigFail"
          "SoftwareUpdateFail"
          "PreRuninFail"
          "PostRuninFail"
          "USPFail"
          "FarmUSPFail"
          "StrangeReboot"
          "Assert"
          "HWFail_generic"
          "HWFail_HIH6131"
          "HWFail_ISL29023"
          "HWFail_SE95"
          "HWFail_ZMotion"
          "HWFail_MMA8451"
          "HWFail_TSC3414"
          "HWFail_UrbanUSP"
          "HWFail_RTC"
          "HWFail_EEPROM"
          "HWFail_NIGHTHAWK"
          "SWUpdateFail_SENSORPOD"
          "HWFail_PCT2075"
          "HWFAIL_SIHAWK"
          "HWFAIL_GPS"
          "HWFail_PodBus"
          "Epic_Fail"
          "Disconnect") "Sensor"
        ("ScheduleFail"
          "DriverFail"
          "UnderPower"
          "OverPower"
          "HardFault"
          "HWFail_STUCK_RELAY") "Light"))

(defn resend-schedule-to-lg
  [{:keys [type
           cypher]
    userid :user
    {:keys [groupid]} :groupprops
    {:keys [siteid]} :siteprops
    :as jsmap}]
  {:pre [jsmap
         type
         (= "resendScheduleToLG"
            type)
         cypher
         siteid
         groupid]}
  (let [props {"groupid" groupid}
        {:keys [exception]
         {:keys [type]
          nodeids :nodeList
          [{:keys [scheduleid]}] :schedules
          [{:keys [pdprofileid]}] :pdprofiles
          [{:keys [dhprofileid]}] :dhprofiles} :group
         :as cypher-return} (-> cypher
                                (neo4j/executeQuery props)
                                (json/read-str :key-fn keyword))
        update-diff {:schedules [{:scheduleid scheduleid}]
                     :dhprofiles [{:dhprofileid dhprofileid}]
                     :pdprofiles [{:pdprofileid pdprofileid}]}
        error (cond
                (some? exception) {:error exception}
                (empty? cypher-return) {:error "Could not find Lighting Group."}
                (nil? (#{"lighting"
                         "site-lighting"} type)) {:error "Only Lighting Groups or Site Lighting Groups can resend Schedules to Nodes"}
                (empty? nodeids) {:error "Group has no nodes to send to"}
                :else {})
        success (empty? error)
        message {"groupid" groupid
                 "nodeids" nodeids
                 "siteid" siteid}
        log_map {:activity type
                 :targettype "Group"
                 :targetid groupid
                 :message message}]
    (when success
      (al/log "user" (assoc log_map
                            :userid userid))
      (when siteid
        (al/log "site" (assoc log_map
                              :siteid siteid :userid userid)))
      (lighting-group-diff-update {:incoming nodeids}
                                  (assoc jsmap :groupprops update-diff)))
    (json/write-str (merge {:success success
                            :result message}
                           error))))

(defn resend-schedule-to-node
  [{:keys [type
           cypher]
    userid :user
    {:keys [nodeid]} :nodeprops
    {:keys [siteid]} :siteprops
    :as jsmap}]
  {:pre [jsmap
         type
         (= "resendScheduleToNode"
            type)
         cypher
         siteid
         nodeid]}
  (let [props {"nodeid" nodeid}
        {:keys [exception]
         {:keys [scheduleid
                 pdprofileid
                 dhprofileid]} :node
         :as cypher-return} (-> cypher
                                (neo4j/executeQuery props)
                                (json/read-str :key-fn keyword))
        update-diff {:schedules [{:scheduleid scheduleid}]
                     :dhprofiles [{:dhprofileid dhprofileid}]
                     :pdprofiles [{:pdprofileid pdprofileid}]}
        error (cond
                (empty? cypher-return) {:error "Could not find Node."}
                (some? exception) {:error exception}
                :else {})
        success (empty? error)
        message {"nodeid" nodeid
                 "siteid" siteid}
        log_map {:activity type
                 :targettype "Node"
                 :targetid nodeid
                 :message message}]
    (when success
      (al/log "user" (assoc log_map
                            :userid userid))
      (when siteid
        (al/log "site" (assoc log_map
                              :siteid siteid :userid userid)))
      (lighting-group-diff-update {:incoming [nodeid]}
                                  (assoc jsmap :groupprops update-diff)))
    (json/write-str (merge {:success success
                            :result message}
                           error))))

(defn get-target-name
  [target-type target-id]
  (if-not (nil? target-id)
    (:name (-> (format "cyphers/get_target_%s_name.cypher" target-type)
               io/resource
               slurp
               (neo4j/executeQuery (stringify-keys {:id target-id}))
               (json/read-str :key-fn keyword)))
    nil))

(defn get-ota-status-for-site
  [jsmap]
  (let [orgid (get-in jsmap [:orgprops :orgid])
        siteid (get-in jsmap [:siteprops :siteid])
        results (al/get_ota_log_for_site orgid siteid)
        success (if-not (nil? results) true false)
        items (if success
                (map (fn [[id value]]
                       (let [v (first value)
                             target_type (:target_type v)
                             target_id (:target_id v)
                             target_name (get-target-name target_type target_id)]
                         {:jobid id
                          :target_id target_id
                          :target_type target_type
                          :target_name target_name
                          :node_count (:node_count v)
                          :description (:description v)
                          :firmwareid (:firmwareid v)
                          :orgid (:orgid v)
                          :siteid (:siteid v)
                          :model (:model v)
                          :when (:when v)}))
                     (group-by :jobid results))
                [])]
    (json/write-str {:items items :success success})))

(defn get-ota-status-for-job
  [jsmap]
  (let [orgid (get-in jsmap [:orgprops :orgid])
        siteid (get-in jsmap [:siteprops :siteid])
        jobid (get-in jsmap [:otaprops :jobid])
        results (al/get_ota_log_for_job orgid siteid jobid)
        success (if-not (nil? results) true false)
        items (if success results [])
        target_name (let [{:keys [target_type target_id model]} (first items)]
                      (get-target-name target_type target_id))
        updated-items (map (fn [{:keys [target_type target_id] :as job}]
                             (assoc job :target_name target_name))
                           items)]
    (json/write-str {:items updated-items :success success})))

(defn update-ota-status-for-job
  [{userid :user :as jsmap}]
  (let [orgid (get-in jsmap [:orgprops :orgid])
        siteid (get-in jsmap [:siteprops :siteid])
        jobid (get-in jsmap [:otaprops :jobid])
        action (get-in jsmap [:otaprops :action])
        type (case action
               "stop" "OTAStop"
               "faster" "OTAFaster"
               "slower" "OTASlower"
               "OTAFaster")
        updatedmap (-> jsmap
                       (assoc-in [:nodeprops :type] type)
                       (assoc-in [:nodeprops :nodeid] ["fake"]))
        results (al/get_ota_log_for_job orgid siteid jobid)
        error (cond
                (empty? results) {:error "Job not found"}
                :else {})
        success (empty? error)
        log_map {:activity type
                 :targettype "Job"
                 :targetid jobid
                 :message action}]
    (when success
      (al/log "user" (assoc log_map :userid userid))
      (cape-future dctrl/query-exec-msgpk updatedmap))
    (json/write-str (merge {:success success} error))))

;; Runs one of traffic CASEls
(defn getTrafficData
  [{qtype :type
    cypher :cypher
    userid :user
    :as jsmap}]
  (case qtype

    "getCurrentTrafficInfo"
        (do
          (let [siteid (get-in jsmap [:siteprops :siteid])
                nodeid (get-in jsmap [:extprops :nodeid] nil)
                type (get-in jsmap [:extprops :type] nil)
                from (get-in jsmap [:extprops :from])
                to (get-in jsmap [:extprops :to] nil)
                filter (get-in jsmap [:extprops])
                results (spy :debug (al/get_latest_traffic_info siteid nodeid type))
                exception (get results "exception" nil)
                error (if-not (nil? exception) {:error exception} {})
                success (nil? exception)]
            (json/write-str (merge {:data results :success success} error))))

    "getTrafficHistory"
        (do
          (let [siteid (get-in jsmap [:siteprops :siteid])
                from (get-in jsmap [:extprops :from])
                to (get-in jsmap [:extprops :to])
                filter (get-in jsmap [:extprops])
                results (al/get_traffic_history siteid filter)
                exception (get results "exception" nil)
                error (if-not (nil? exception) {:error (.getMessage exception) :status (status-code-for (:cause exception))} {})
                success (nil? exception)
                data (if-not (nil? exception) nil results)]
            (json/write-str (merge {:data data :success success} error))))

    "getTrafficConfig"
        (do
          (let [siteid (get-in jsmap [:siteprops :siteid])
                nodeid (get-in jsmap [:nodeprops :nodeid])
                filter (get-in jsmap [:extprops])
                results (spy :debug (al/get_traffic_config siteid nodeid filter))
                exception (get results "exception" nil)
                error (if-not (nil? exception) {:error (.getMessage exception) :status (status-code-for (:cause exception))} {})
                success (nil? exception)
                data (if-not (nil? exception) nil results)]
            (json/write-str (merge {:data data :success success} error))))


  ))

;; Selects the DB based on deduced type
(defn handle-alerts
  [{qtype :type
    cypher :cypher
    userid :user
    :as jsmap}]
  (case qtype

    ("createAlert" "updateAlert")
      (do
        (if-not (nil? (jsmap :alertprops))
                (let [alertid (get-in jsmap [:alertprops :alertid])
                      siteid (get-in jsmap [:siteprops :siteid])
                      alertprops (:alertprops jsmap)
                      merged (merge (:siteprops jsmap)
                                    (:orgprops jsmap)
                                    alertprops
                                    {:category (alert-category (:type alertprops))})
                      props (stringify-keys merged)
                      result (json/read-str (neo4j/executeQuery cypher props))
                      alert (get result "alert" nil)
                      exception (get result "exception" nil)
                      error (if-not (nil? exception) {:error exception} {})
                      success (nil? exception)
                      log_map {:activity qtype :targettype "alert" :targetid alertid :message props}]
                  (if success
                      (do
                        (al/log "user" (merge {:userid userid} log_map))
                        (al/log "site" (merge {:siteid siteid :userid userid} log_map))))
                  (json/write-str (merge {:alert alert    :success success} error)))))

    ("getAllAlerts" "getAllOrgAlerts" "getAlertsForNode" "searchAlerts")
      (do
        (let [merged  (merge (:siteprops jsmap)
                             (:orgprops jsmap)
                             (:nodeprops jsmap))
              props (stringify-keys merged)
              result (json/read-str (neo4j/executeQuery cypher props))
              items (get result "items" [])
              item-list (if (map? items) [items] items)
              exception (get result "exception" nil)
              error (if-not (nil? exception) {:error exception} {})
              success (nil? exception)]
          (json/write-str (merge {:items item-list :success success} error))))

    "deleteAlert"
      (do
        (if-not (nil? (jsmap :alertprops))
                (let [alertid (get-in jsmap [:alertprops :alertid])
                      siteid (get-in jsmap [:siteprops :siteid])
                      merged (merge (:siteprops jsmap)
                                    (:alertprops jsmap))
                      props (stringify-keys merged)
                      result (json/read-str (neo4j/executeQuery cypher props))
                      exception (get result "exception" nil)

                      error (cond
                              (some? exception) {:error exception}
                              (empty? result) {:error (format "Alert with id: '%s' not found" alertid) :status 404}
                              :else {})
                      success (empty? error)
                      alertprops (stringify-keys (jsmap :alertprops))
                      log_map {:activity "deletealert" :targettype "alert" :targetid alertid :message alertprops}]
                  (if success
                      (do
                        (al/log "user" (merge {:userid userid} log_map))
                        (al/log "site" (merge {:siteid siteid :userid userid} log_map))))
                  (json/write-str (merge {:success success} error)))))

    "dismissAlert"
      (do
        (if-not (nil? (jsmap :alertprops))
                (let [alertid (get-in jsmap [:alertprops :alertid])
                      siteid (get-in jsmap [:siteprops :siteid])
                      merged (merge (:siteprops jsmap)
                                    (:alertprops jsmap))
                      props (stringify-keys merged)
                      result (json/read-str (neo4j/executeQuery cypher props))
                      exception (get result "exception" nil)

                      error (cond
                              (some? exception) {:error exception}
                              (empty? result) {:error (format "Alert with id: '%s' not found" alertid) :status 404}
                              :else {})
                      success (empty? error)

                      alertprops (stringify-keys (jsmap :alertprops))
                      log_map {:activity "dismissalert" :targettype "alert" :targetid alertid :message alertprops}]
                  (if success
                      (do
                        (al/log "user" (merge {:userid userid} log_map))
                        (al/log "site" (merge {:siteid siteid :userid userid} log_map))))
                  (json/write-str (merge {:success success} error)))))

    "getSiteNodesStatuses"
      (do
        (let [siteid (get-in jsmap [:siteprops :siteid])
              node_status (spy :debug (al/get_latest_site_nodes_statuses siteid))
              exception (get node_status "exception" nil)
              error (if-not (nil? exception) {:error exception} {})
              success (nil? exception)
              items (-> "cyphers/site_nodes_alarms.cypher"
                        io/resource
                        slurp
                        (neo4j/executeQuery {"siteid" siteid})
                        json/read-str)
              node_alerts (keywordize-keys (get items "items"))
              exception-na (get node_alerts "exception" nil)
              error-na (if-not (nil? exception-na) {:error exception-na} {})
              success-na (nil? exception-na)
              merged (set/join  node_status  node_alerts ) ]
          (json/write-str (merge {:status merged :success success} error))))

  ))

(defn add-node-to-group
  [{:keys [type
           cypher
           groupprops]
    userid :user
    :as jsmap}]
  {:pre [(#{"addNodeToGroup"
            "addNodesToGroup"}
          type)
         cypher
         groupprops]}
  (let [orgid (get-in jsmap [:orgprops :orgid])
        siteid (get-in jsmap [:siteprops :siteid])
        groupid (get-in jsmap [:groupprops :groupid])
        nodeid (get-in jsmap [:nodeprops :nodeid])
        nodeids (or (get-in jsmap [:nodeprops :nodeids])
                    [nodeid])
        prior-pdprofileids (->> nodeids
                                (map pd/get-profile-from-node)
                                (remove nil?)
                                set)
        props {"orgid" orgid
               "siteid" siteid
               "groupid" groupid
               "nodeids" nodeids}
        {:keys [exception]
         {:keys [lighting
                 scheduleid
                 etdhprofileid
                 dhprofileid
                 pdprofileid]
          :as result} :result
         :as cypher-return} (-> (neo4j/executeQuery cypher props)
                                (json/read-str :key-fn keyword))
        update-diff {:schedules [{:scheduleid scheduleid}]
                     :etdhprofiles [{:etdhprofileid etdhprofileid}]
                     :dhprofiles [{:dhprofileid dhprofileid}]
                     :pdprofiles [{:pdprofileid pdprofileid}]}
        error (cond
                (empty? cypher-return) {:error "Could not find Node(s) and Group belonging to Site and Org."}
                (some? exception) {:error exception}
                :else {})
        success (empty? error)
        message {"groupid" groupid
                 "nodeids" nodeids
                 "siteid" siteid}
        log_map {:activity type
                 :targettype "Group"
                 :targetid groupid
                 :message message}]
    (when success
      (doseq [pdprofileid prior-pdprofileids]
        (pd/invalidate-pdprofile-cache-for-id pdprofileid))
      (al/log "user" (merge {:userid userid} log_map))
      (when siteid
        (al/log "site" (merge {:siteid siteid :userid userid} log_map)))
      (when lighting
        (lighting-group-diff-update (assoc result :incoming nodeids)
                                    (assoc jsmap :groupprops update-diff))))
    (json/write-str (merge {:success success}
                           (if success
                             {:result message}
                             error)))))

(defn prepare-etdhprofile-for-write
  [etdhprofile]
  (-> etdhprofile
      (update :scheduled json/write-str)))

(defn manipulate-etdh-triggers
  [{:as jsmap
    :keys [cypher
           type]
    userid :user
    {:keys [orgid]} :orgprops
    {:keys [siteid]} :siteprops
    {:keys [etdhprofileid]} :etdhprofileprops
    {:keys [nodeid
            nodeids]} :nodeprops
    :or {nodeids [nodeid]}}]
  (when jsmap
    (let [{:keys [exception
                  alien
                  etdhprofile]} (-> cypher
                                    (neo4j/executeQuery {"etdhprofileid" etdhprofileid
                                                         "nodeids" nodeids})
                                    (->> (spy :debug))
                                    (json/read-str :key-fn keyword))
          error (cond
                  exception {:error exception}
                  (nil? etdhprofile) {:error "Could not find etdhprofile with trigger node."}
                  (some? alien) {:error "One or more nodes do not belong to the specified etdhprofile."}
                  :else nil)
          success (nil? error)
          message {"siteid" siteid
                   "etdhprofileid" etdhprofileid
                   "nodeids" nodeids}
          log_map {:activity type
                   :targettype "Node"
                   :targetid nodeid
                   :message message}
          json (merge {:success success}
                      (when error
                        error))]
      (when success
        (al/log "user" (merge {:userid userid} log_map))
        (al/log "site" (merge {:siteid siteid :userid userid} log_map)))
      (json/write-str json))))

;; Every parking acl requires this check:
;; Basically it checks if the user has admin Role or not
;; If it does, user has privilage abd no need to check if user has a link to parkingGroups (as per Udit)
(defn is-admin-role [jsmap]
  (trace "execute-parking-query ...")
  (let [userid (jsmap :user)
        parkinggroupid (get-in jsmap [:parkingGroupprops :parkingGroupId])
        siteid (get-in jsmap [:siteprops :siteid])]
    (-> "cyphers/check_if_role_is_admin.cypher"
        io/resource
        slurp
        (neo4j/executeQuery {"userid" userid "parkinggroupid" parkinggroupid})
        json/read-str)))


;; Check If given user is linked to given parkingGroup? If not all bets are off
;; If it does, user has the privilage to perform microservice call  (as per Udit)
(defn is-user-linked-to-parking-group [jsmap]
  (trace "is-user-linked-to-parking-group ...")
  (let [userid (jsmap :user)
        parkinggroupid (get-in jsmap [:parkingGroupprops :parkingGroupId])
        siteid (get-in jsmap [:siteprops :siteid])
        cypher-result (-> "cyphers/check_if_user_is_linked_to_parkinggroup.cypher"
                           io/resource
                           slurp
                           (neo4j/executeQuery {"userid" userid "parkinggroupid" parkinggroupid})
                           json/read-str)
        link-s (spy :debug (get cypher-result "type(s)"))
        link-r (spy :debug (get cypher-result "type(r)"))
        exception (get cypher-result "exception" nil)]

    (json/write-str {:userid userid
                     :success (and
                               (nil? exception)
                               (= link-s "USERS_TO_PARKING_GROUP")
                               (= link-r "PARKING_GROUP_TO_USERS"))}) ))

(defn check-user-privilage [jsmap]
  (let[userid (jsmap :user)
       parkinggroupid (get-in jsmap [:parkingGroupprops :parkingGroupId])
       siteid (get-in jsmap [:siteprops :siteid])
       cypher-result (is-admin-role jsmap)
       exception (get cypher-result "exception" nil)
       role-name (spy :debug (get cypher-result "role.rolename" {}))]
    (if (and (nil? exception)
             (not (empty? role-name)))

      (json/write-str {:userid userid :success true})
      (is-user-linked-to-parking-group jsmap))))

(defn resolve-node [nodeid]
      (with-open [_ (timers/start (metric-factory :timer
                                                  ["resolve-node"]))]
                 (let [cypher (-> "cyphers/get_site_and_org_for_node.cypher"
                                  io/resource
                                  slurp)
                       results (keywordize-keys (get (json/read-str (neo4j/executeQuery cypher {"nodeid" nodeid})) "data"))]
                      results)))


(defn create-empty-node
  [{userid :user
    :keys [cypher]
    :as jsmap}]
  (if-not (nil? (jsmap :nodeprops))
    (let [nodeid (get-in jsmap [:nodeprops :nodeid])
          result (json/read-str (neo4j/executeQuery cypher (stringify-keys (:nodeprops jsmap))))
          exception (get result "exception" nil)
          error (if-not (nil? exception) {:error exception} {})
          success (nil? exception)
          props (stringify-keys (jsmap :nodeprops))
          log_map {:activity "createEmptyNode" :targettype "Node" :targetid nodeid :message props}]
      (if success
        (al/log "user" (merge {:userid userid} log_map)))
      (json/write-str (merge {:node props :success success} error)))))

(defn get-node-model [nodeid]
  (-> "cyphers/get_node_model.cypher"
      io/resource
      slurp
      (neo4j/executeQuery {"nodeid" nodeid})
      (json/read-str :key-fn keyword)
      (get :model)))

(defmethod db-selector :deleteNode
  [{userid :user
    :keys [cypher
           nodeprops]
    {:keys [nodeid]} :nodeprops
    {:keys [siteid]} :siteprops
    :as jsmap}]
  (when (some? nodeprops)
    (let [pdprofileid (pd/get-profile-from-node nodeid)
          props (stringify-keys nodeprops)
          {:keys [exception]
           :as result} (-> cypher
                           (neo4j/executeQuery props)
                           (json/read-str :key-fn keyword))
          error (if (some? exception)
                  {:error exception}
                  {})
          success (empty? error)
          log_map {:activity "deleteNode" :targettype "Node" :targetid nodeid :message props}]
      (when success
        (al/log "user" (merge {:userid userid} log_map))
        (al/log "site" (merge {:siteid siteid :userid userid} log_map))
        (al/log_node_status {:nodeid nodeid :siteid "Unknown" :orgid "Unknown"})
        (al/log_node_hierarchy {:nodeid nodeid :siteid "Unknown" :sitename "Unknown" :orgid "Unknown" :orgname "Unknown"})
        (when pdprofileid
          (pd/invalidate-pdprofile-cache-for-id pdprofileid))
        (sch/send-loginreq-schedule "Unknown" "default" nodeid (= "cnext" (get-node-model nodeid)) userid)
        (dctrl/query-exec-msgpk {:nodeprops {:type "LightingSetAuto"
                                             :nodeid nodeid}}))
      (json/write-str (merge {:success success} error)))))

(defn db-selector-legacy
  "DO NOT EXTEND.

  Add an new `defmethod` to `db-selector` instead."
  ;;([cypher qtype userid nodeprops siteprops orgprops userprops extprops & args]
  [{qtype :type
    cypher :cypher
    userid :user
    :as jsmap}]
  (case qtype

    ("createAlert" "updateAlert" "getAllAlerts" "getAllOrgAlerts" "getAlertsForNode" "searchAlerts" "deleteAlert" "dismissAlert" "getSiteNodesStatuses")
      (handle-alerts jsmap)

    "commandNode"
    (do
      (let [nodeid (get-in jsmap [:nodeprops :nodeid])
            siteid (get-in jsmap [:siteprops :siteid])
            type (get-in jsmap [:nodeprops :type])
            updatedmap (assoc-in jsmap [:nodeprops :nodeid] (flatten [nodeid]))
            log_map {:activity type :targettype "node"
                     :targetid "nodeList" :message updatedmap}]
        (do
          (al/log "user" (merge {:userid userid} log_map))
          (al/log "site" (merge {:siteid siteid :userid userid} log_map)))
        (cape-future dctrl/query-exec-msgpk updatedmap)
        (json/write-str {:response "OK" :success true})))

    "lighting-control"
    (let [nodeid (get-in jsmap [:nodeprops :nodeid])
          siteid (get-in jsmap [:siteprops :siteid])
          type (get-in jsmap [:nodeprops :type])
          updatedmap (assoc-in jsmap [:nodeprops :nodeid] (flatten [nodeid]))
          log_map {:activity type :targettype "node"
                   :targetid nodeid :message updatedmap}]
      (do
        (al/log "user" (merge {:userid userid} log_map))
        (al/log "site" (merge {:siteid siteid :userid userid} log_map)))
      (cape-future dctrl/query-exec-msgpk updatedmap)
      (json/write-str {:response "OK" :success true})) ;; Pass through

    "lighting-control-site"
    (do
      (let [siteid (get-in jsmap [:siteprops :siteid])
            type (get-in jsmap [:nodeprops :type])
            results (json/read-str (neo4j/executeQuery cypher))
            items (get results "items" [])
            noderesults (if (map? items) [items] items)
            exception (get results "exception" nil)
            error (if-not (nil? exception) {:error exception} {})
            success (nil? exception)
            nodes (map (fn [node] (get node "nodeid")) noderesults)
            updatedmap (assoc-in jsmap [:nodeprops :nodeid] nodes)
            log_map {:activity type :targettype "site"
                     :targetid siteid :message updatedmap}]
        (if success
          (do
            (al/log "user" (merge {:userid userid} log_map))
            (al/log "site" (merge {:siteid siteid :userid userid} log_map))))
                                        ; Send nodeid as an array of node ids.
        (cape-future dctrl/query-exec-msgpk updatedmap)
        (json/write-str (merge {:response "OK" :success success} error))))

    "lighting-control-group"
    (do
      (let [siteid (get-in jsmap [:siteprops :siteid])
            groupid (get-in jsmap [:groupprops :groupid])
            type (get-in jsmap [:nodeprops :type])
            results (json/read-str (neo4j/executeQuery cypher))
            items (get results "items" [])
            noderesults (if (map? items) [items] items)
            exception (get results "exception" nil)
            error (if-not (nil? exception) {:error exception} {})
            success (nil? exception)
            nodes (map (fn [node] (get node "nodeid")) noderesults)
            updatedmap (assoc-in jsmap [:nodeprops :nodeid] nodes)
            log_map {:activity type :targettype "group"
                     :targetid groupid :message updatedmap}]
        (if success
          (do
            (al/log "user" (merge {:userid userid} log_map))
            (al/log "site" (merge {:siteid siteid :userid userid} log_map))))
                                        ; Send nodeid as an array of node ids.
                                        ;(spy (format "lighting-control-group result %s nodes %s updatedmap %s" results nodes updatedmap))
        (cape-future dctrl/query-exec-msgpk updatedmap)
        (json/write-str (merge {:response "OK" :success success} error))))

    "getDefaultConfigs"
    (do
      (let [node-model (get-in jsmap [:nodeprops :model])
            conf (get-default-config node-model)
            success (not (empty? conf))
            resp (merge {:config conf} {:success success})]
        (json/write-str resp)))

    "getDefaultConfigsForSite"
    (do
      (let [node-model (get-in jsmap [:nodeprops :model])
            conf (get-default-config node-model)
            param (stringify-keys {:siteid (get-in jsmap [:siteprops :siteid])})
            cc (keywordize-keys (json/read-str (neo4j/executeQuery cypher param)))
            success (not (empty? conf))
            resp (merge  {:config (merge conf (:item cc))}  {:success success})]
        (json/write-str resp)))

    "getNode" (get-node jsmap)

    "getConfig" (get-config jsmap)

    "getETDHProfile" ((comp prepare-etdhprofile-for-read
                            get-item) jsmap)

    ("getUser"
     "getUserEmail"
     "getOrg"
     "getSite"
     "getOverlay"
     "getGroup"
     "getFixture"
     "getFirmware"
     "getNotification"
     "getAlert"
     "getParkingSpot"
     "getParkingGroup"
     "getSchedule"
     "getDHProfile"
     "getPDProfile")
    (get-item jsmap)

    "updateDHProfile"
    (-> jsmap
        (update :dhprofileprops
                dissoc :setPoint)
        update-item)

    "updateSchedule" (update-schedule (spy :debug jsmap))
    "updateETDHProfile" (-> jsmap
                            (update :etdhprofileprops etdh/cleanup-etdhprofile)
                            (update :etdhprofileprops prepare-etdhprofile-for-write)
                            update-item
                            prepare-etdhprofile-for-read)


    ("updateUser"
     "updateOrg"
     "updateSite"
     "updateNode"
     "updateOverlay"
     "updateFixture"
     "updateFirmware"
     "updateNotification"
     "updateParkingZone"
     "updateParkingSpot"
     "updateParkingGroup"
     "updatePDProfile")
    (update-item jsmap)

    "updateConfig" (maybe-update-config jsmap)
    "updateGroup" (update-group jsmap)

    ( "getAllConfigs"
     "getAllFixtures"
     "getAllFirmwares"
     "getAllOverlays"
     "getAllGroups"
     "getAllNotificationsForSite"
     "getAllNotificationsForUser"
     "getAllNotificationsForOrg"
     "getAllNotificationsByName"
     "getAllParkingZones"
     "getAllParkingSpots"
     "getAllParkingGroups"
     "getAllOrgs"
     "getAllSuspendedOrgs"
     "getAllSitesForOrg"
     "getAllSuspendedSitesForOrg"
     "getAllMinNodesForSite"
     "getAllLostAndFoundNodes"
     "getAllUsersForSite"
     "getAllUsersForOrg"
     "getAllUsersForPartner"
     "getAllSuspendedUsersForOrg"
     "getAllSuspendedUsersForPartner"
     "getAllSchedules"
     "getAllDHProfileTriggers"
     "getAllETDHProfileTriggers")
    (get-all-items jsmap)

    ("getAllNodeIdsForModelSite"
    "getAllNodesForSite"
    "getAllNodeIdsForModelGroup")  (get-all-items-no-template jsmap)

    ("getUserEmails")  (get-all-items-no-template-with-userprops jsmap)

    ( "createConfig"
     "createSchedule") (create-item jsmap)
    ( "deleteConfig") (maybe-delete-config jsmap)

    "otaStatusForSite"
    (do (get-ota-status-for-site jsmap))

    "otaStatusForJob"
    (do (get-ota-status-for-job jsmap))

    "otaStatusJobUpdate"
    (do (update-ota-status-for-job jsmap))

    "getActivityLogs"
    (do
      (let [results (al/getactivitylogs userid (get-in jsmap [:orgprops :orgid]) (get-in jsmap [:siteprops :siteid]) (:extprops jsmap))
            success (if-not (nil? results) true false)
            logs (if success results [])]
        (json/write-str {:logs logs :success success} :value-fn json-date-writer :key-fn name)))

    "logActivity"
    (do
      (let [logprops (jsmap :logprops)
            type (jsmap :logtype)]
        (al/log type logprops))
      (json/write-str {:success true :response "OK" }))

    "autoComplete"
    (do
      (let [results (json/read-str (neo4j/executeQuery cypher))
            success (if-not (nil? results) true false)
            match (if-not (nil? results) (get results "matches") {})
            match (get results "matches" {})
            success (if-not (empty? match) true false)]
        ;; TODO: Check what may be required from cassandra
        ;; Post Processing
        (json/write-str {:matches match, :success success})))

    "deleteUser"
    (do
      (if-not (nil? (jsmap :userprops))
        (let [userid (get-in jsmap [:userprops :userid])
              result (json/read-str (neo4j/executeQuery cypher))
              exception (get result "exception" nil)
              error (if-not (nil? exception) {:error exception} {})
              success (nil? exception)
              userprops (stringify-keys (jsmap :userprops))
              log_map {:activity "deleteUser" :targettype "User" :targetid userid :message userprops}]
          (if success
            (al/log "user" (merge {:userid userid} log_map)))
          (json/write-str (merge {:success success :user {}} error)))))

    "suspendUser"
    (do
      (if-not (nil? (jsmap :userprops))
        (let [userid (get-in jsmap [:userprops :userid])
              result (json/read-str (neo4j/executeQuery cypher))
              exception (get result "exception" nil)
              error (if-not (nil? exception) {:error exception} {})
              success (nil? exception)
              userprops (stringify-keys (jsmap :userprops))
              log_map {:activity "suspendUser" :targettype "User" :targetid userid :message userprops}]
          (if success
            (al/log "user" (merge {:userid userid} log_map)))
          (json/write-str (merge {:success success :user {}} error)))))

    "activateUser"
    (do
      (if-not (nil? (jsmap :userprops))
        (let [userid (get-in jsmap [:userprops :userid])
              result (json/read-str (neo4j/executeQuery cypher))
              exception (get result "exception" nil)
              error (if-not (nil? exception) {:error exception} {})
              success (nil? exception)
              userprops (stringify-keys (jsmap :userprops))
              log_map {:activity "activateUser" :targettype "User" :targetid userid :message userprops}]
          (if success
            (al/log "user" (merge {:userid userid} log_map)))
          (json/write-str (merge {:success success :user {}} error)))))

    "createUser"
    (do
      (if-not (nil? (jsmap :userprops))
        (let [newuserid (get-in jsmap [:userprops :userid])
              result (json/read-str (neo4j/executeQuery cypher))
              exception (get result "exception" nil)
              error (if-not (nil? exception) {:error exception} {})
              success (nil? exception)
              props (stringify-keys (jsmap :userprops))
              log_map {:activity "createUser" :targettype "User" :targetid newuserid :message props}]
          (if success
            (al/log "user" (merge {:userid userid} log_map)))
          (json/write-str (merge {:user props :success success} error)))))

    "createOrg"
    (do
      (if-not (nil? (jsmap :orgprops))
        (let [orgprops (jsmap :orgprops)
              orgid (orgprops :orgid)
              props (stringify-keys jsmap)
              result (json/read-str (neo4j/executeQuery cypher props))
              exception (get result "exception" nil)
              error (if-not (nil? exception) {:error exception} {})
              success (nil? exception)
              log_map {:activity "createOrg" :targettype "Org" :targetid orgid :message props}]
          (if success
            (al/log "user" (merge {:userid userid} log_map)))
          (json/write-str (merge {:org orgprops :success success} error)))))

    "createSite"
    (when (some? (jsmap :siteprops))
      (let [orgid (get-in jsmap [:orgprops :orgid])
            siteid (get-in jsmap [:siteprops :siteid])
            props (-> (stringify-keys (jsmap :siteprops))
                      (update-location-data siteid "createSite"))
            result (-> cypher
                       (neo4j/executeQuery props)
                       json/read-str)
            exception (get result "exception" nil)
            error (if (some? exception)
                    {:error exception}
                    {})
            success (nil? exception)
            log_map {:activity "createSite"
                     :targettype "Site"
                     :targetid siteid
                     :message props}
            scheduleid (.. java.util.UUID
                           randomUUID
                           toString)
            groupid (.. java.util.UUID
                        randomUUID
                        toString)]
        (when success
          (al/log "user" (merge {:userid userid} log_map))
          (al/log "site" (merge {:siteid siteid :userid userid} log_map))
          (-> {:type "createSchedule"
               :user userid
               :orgprops {:orgid orgid}
               :siteprops {:siteid siteid}
               :scheduleprops {:scheduleid scheduleid
                               :name "Default schedule"
                               :description "Default site schedule"
                               :events [{:days ["mon" "tue" "wed" "thu" "fri" "sat" "sun"]
                                         :photocell_enabled true
                                         :photocell_highLevel 100
                                         :photocell_lowLevel 0
                                         :actions [{:time "00:00:00" :level 100}]}]
                               :network {:photocell_enabled true
                                         :photocell_highLevel 100
                                         :photocell_lowLevel 0
                                         :highTime "00:00:00"
                                         :highLevel 100}}}
              template-selector
              db-selector)
          (-> {:type "createGroup"
               :user userid
               :orgprops {:orgid orgid}
               :siteprops {:siteid siteid}
               :groupprops {:groupid groupid
                            :name "Site Lighting Group"
                            :description "Default site lighting group"
                            :type "lighting"
                            :nodeList []}}
              template-selector
              db-selector)
          (neo4j/executeQuery "MATCH (lg:Group:LightingGroup)
WHERE lg.groupid={props}.groupid
SET lg:SiteLightingGroup"
                              {"groupid" groupid})
          (-> {:type "applyScheduleToSite"
               :user userid
               :orgprops {:orgid orgid}
               :siteprops {:siteid siteid}
               :scheduleprops {:scheduleid scheduleid}}
              template-selector
              db-selector))
        (json/write-str (merge {:site props
                                :success success}
                               error))))

    "createNode"
    (when (jsmap :nodeprops)
      (let [nodeid (get-in jsmap [:nodeprops :nodeid])
            siteid (get-in jsmap [:siteprops :siteid])
            latitude (get-in jsmap [:nodeprops :latitude])
            longitude (get-in jsmap [:nodeprops :longitude])
            merged  (merge (:siteprops jsmap)
                          (:nodeprops jsmap))
            props (-> (stringify-keys merged)
                      (update-location-data siteid "createNode"))
            {:keys [exception]
             {:keys [orgid
                     orgname
                     siteid
                     sitename
                     model
                     groupid]
              :as node} :node
             :as result} (-> (neo4j/executeQuery cypher props)
                             (json/read-str :key-fn keyword))
            error (if-not (nil? exception) {:error exception} {})
            success (nil? exception)
            log_map {:activity "createNode"
                     :targettype "Node"
                     :targetid siteid
                     :message props}]
        (when success
          (al/log "user" (merge {:userid userid} log_map))
          (al/log "site" (merge {:siteid siteid :userid userid} log_map))
          (al/log_node_status {:nodeid nodeid :siteid siteid :orgid orgid})
          (al/log_node_hierarchy {:nodeid nodeid :nodehw model :siteid siteid :sitename sitename :orgid orgid :orgname orgname})
          (send-gps-update userid orgid siteid nodeid latitude longitude)
          (-> jsmap
              (assoc :type "addNodeToGroup")
              (assoc-in [:orgprops :orgid] orgid)
              (assoc-in [:groupprops :groupid] groupid)
              template-selector
              db-selector))
        (json/write-str (merge {:node (dissoc node
                                              :orgid
                                              :orgname
                                              :siteid
                                              :sitename
                                              :groupid)
                                :success success}
                               error))))

    "assignNode"
    (when (jsmap :nodeprops)
      (let [nodeid (get-in jsmap [:nodeprops :nodeid])
            siteid (get-in jsmap [:siteprops :siteid])
            props  (stringify-keys (merge (:siteprops jsmap)
                                           (:nodeprops jsmap)))
            {:keys [exception]
             {:keys [orgid
                     orgname
                     siteid
                     sitename
                     model
                     groupid]} :node
             :as result} (-> (neo4j/executeQuery cypher (update-location-data props siteid "assignNode"))
                             (json/read-str :key-fn keyword))
            error (if-not (nil? exception) {:error exception} {})
            success (nil? exception)
            props (stringify-keys (jsmap :nodeprops))
            log_map {:activity "assignNode"
                     :targettype "Node"
                     :targetid siteid
                     :message props}]
        (when success
          (al/log "user" (merge {:userid userid} log_map))
          (al/log "site" (merge {:siteid siteid :userid userid} log_map))
          (al/log_node_status {:nodeid nodeid :siteid siteid :orgid orgid})
          (al/log_node_hierarchy {:nodeid nodeid :nodehw model :siteid siteid :sitename sitename :orgid orgid :orgname orgname})
          (-> jsmap
              (assoc :type "addNodeToGroup")
              (assoc-in [:orgprops :orgid] orgid)
              (assoc-in [:groupprops :groupid] groupid)
              template-selector
              db-selector)
          (future (dctrl/query-exec-msgpk {:nodeprops {:type "DeviceActionReq" :cmd "ColdReset" :nodeid [nodeid]}})))
        (json/write-str (merge {:node props :success success} error))))

    "assignNodeToParkingGroup"
    (do
      (if-not (nil? (jsmap :nodeprops))
        (let [nodeid (get-in jsmap [:nodeprops :nodeid])
              siteid (get-in jsmap [:siteprops :siteid])
              parkinggroupid (get-in jsmap [:parkinggroupprops :parkinggroupid])
              result (json/read-str (neo4j/executeQuery cypher))
              exception (get result "exception" nil)
              error (if-not (nil? exception) {:error exception} {})
              success (nil? exception)
              props (stringify-keys (jsmap :nodeprops))
              log_map {:activity "assignNodeToParkingGroup" :targettype "ParkingGroup" :targetid parkinggroupid :message props}]
          (if success
            (do
              (al/log "user" (merge {:userid userid} log_map))
              (al/log "site" (merge {:siteid siteid :userid userid} log_map))))
          (json/write-str (merge {:node props :success success} error)))))

    "bulkAssignUsersToParkingGroup"
    (do
      (let [userid (jsmap :user)
            users (get-in jsmap [:userIdToLinkprops :userIdsToLink])
            parkinggroupid (get-in jsmap [:parkingGroupprops :parkingGroupId])
            siteid (get-in jsmap [:siteprops :siteid])
            result (json/read-str (neo4j/executeQuery cypher (stringify-keys {:userIdsToLink users :parkinggroupid parkinggroupid})))
            exception (get result "exception" nil)
            error (if-not (nil? exception) {:error exception} {})
            success (nil? exception)
            log_map {:activity "assigUsersToParkingGroup" :targettype "ParkingGroup" :targetid parkinggroupid}]
        (if success
          (do
            (al/log "user" (merge {:userid userid} log_map))
            (al/log "site" (merge {:siteid siteid :userid userid} log_map))))
        (json/write-str (merge {:userid userid :success success} error))))

    "bulkUnassignUsersFromParkingGroup"
    (do
      (let [userid (jsmap :user)
            users (get-in jsmap [:userIdToLinkprops :userIdsToLink])
            parkinggroupid (get-in jsmap [:parkingGroupprops :parkingGroupId])
            siteid (get-in jsmap [:siteprops :siteid])
            result (json/read-str (neo4j/executeQuery cypher (stringify-keys {:userIdsToLink users :parkinggroupid parkinggroupid})))
            exception (get result "exception" nil)
            error (if-not (nil? exception) {:error exception} {})
            success (nil? exception)
            log_map {:activity "unassigUsersFromParkingGroup" :targettype "ParkingGroup" :targetid parkinggroupid}]
        (if success
          (do
            (al/log "user" (merge {:userid userid} log_map))
            (al/log "site" (merge {:siteid siteid :userid userid} log_map))))
        (json/write-str (merge {:userid userid :success success} error))))

    ("createMetadataForParkingSpot"
     "deleteMetadataForParkingSpot"
     "getAllMetadataForParkingSpot"
     "getMetadataForParkingSpot"
     "updateMetadataForParkingSpot"
     "createAppUserData"
     "deleteAppUserData"
     "getAllAppUserData"
     "getAppUserData"
     "updateAppUserData"
     "createParkingPolicy"
     "deleteParkingPolicy"
     "getAllParkingPolicy"
     "getParkingPolicy"
     "updateParkingPolicy"
     "getParkingPolicyVersion"
     "getAllVersionsOfParkingPolicy"
     "getAllActiveParkingPolicyForPeriod"
     "getActiveParkingPolicy"
     "searchParkingPolicy"
     "createPolicyCategory"
     "deletePolicyCategory"
     "getAllPolicyCategory"
     "getPolicyCategory"
     "updatePolicyCategory"
     "associatedParkingGroups"
     "policyTagsAssociation"
     "policyTagsDisassociation") (json/write-str {:userid userid :success true})

    ("policyAssociation"
     "policyDisassociation"
     ) (check-user-privilage jsmap)

    "createEmptyNode" (create-empty-node jsmap)

    "loginReq"
    (do
      (if-not (nil? (jsmap :nodeprops))
              (let [msgnodeid (get-in jsmap [:nodeprops :nodeid])
                    devicemodel (get-in jsmap [:nodeprops :model])
                    resolvednode (resolve-node msgnodeid)
                    {:keys [nodeid scheduleid configid model]
                     :or {scheduleid "default"
                          configid "default"}} resolvednode
                    msgmap {:user "device"
                            :type "createEmptyNode"
                            :nodeprops (jsmap :nodeprops)}]
                   (when nodeid
                         ; Refresh the db with latest node values from loginreq
                         (update-node-on-login jsmap)
                         )

                   ; Node does not exist in platform
                   (when-not nodeid
                             ; Create it
                             (spy :debug (db-selector (template-selector (checkprops msgmap))))
                             )
                   )))


        "activateNode"
    (do
      (if-not (nil? (jsmap :nodeprops))
        (let [nodeid (get-in jsmap [:nodeprops :nodeid])
              siteid (get-in jsmap [:siteprops :siteid])
              result (json/read-str (neo4j/executeQuery cypher (stringify-keys (:nodeprops jsmap))))
              exception (get result "exception" nil)
              error (if-not (nil? exception) {:error exception} {})
              success (nil? exception)
              updated (stringify-keys (jsmap :nodeprops))
              log_map {:activity "activateNode" :targettype "Node" :targetid nodeid :message updated}]
          (if success
            (do
              (al/log "user" (merge {:userid userid} log_map))
              (al/log "site" (merge {:siteid siteid :userid userid} log_map))))
          (json/write-str (merge {:node updated :success success} error)))))

    "deactivateNode"
    (do
      (if-not (nil? (jsmap :nodeprops))
        (let [nodeid (get-in jsmap [:nodeprops :nodeid])
              siteid (get-in jsmap [:siteprops :siteid])
              result (json/read-str (neo4j/executeQuery cypher (stringify-keys (:nodeprops jsmap))))
              exception (get result "exception" nil)
              error (if-not (nil? exception) {:error exception} {})
              success (nil? exception)
              updated (stringify-keys (jsmap :nodeprops))
              log_map {:activity "deactivateNode" :targettype "Node" :targetid nodeid :message updated}]
          (if success
            (do
              (al/log "user" (merge {:userid userid} log_map))
              (al/log "site" (merge {:siteid siteid :userid userid} log_map))))
          (json/write-str (merge {:node updated :success success} error)))))

    "activateNotification"
    (do
      (if-not (nil? (jsmap :notificationprops))
        (let [notificationid (get-in jsmap [:notificationprops :notificationid])
              result (json/read-str (neo4j/executeQuery cypher (stringify-keys (:notificationprops jsmap))))
              exception (get result "exception" nil)
              error (if-not (nil? exception) {:error exception} {})
              success (nil? exception)
              updated (stringify-keys (jsmap :notificationprops))
              log_map {:activity "activateNotification" :targettype "Notification" :targetid notificationid :message updated}]
          (if success
            (do
              (al/log "user" (merge {:userid userid} log_map))))
          (json/write-str (merge {:notification updated :success success} error)))))

    "deactivateNotification"
    (do
      (if-not (nil? (jsmap :notificationprops))
        (let [notificationid (get-in jsmap [:notificationprops :notificationid])
              result (json/read-str (neo4j/executeQuery cypher (stringify-keys (:notificationprops jsmap))))
              exception (get result "exception" nil)
              error (if-not (nil? exception) {:error exception} {})
              success (nil? exception)
              updated (stringify-keys (jsmap :notificationprops))
              log_map {:activity "deactivateNotification" :targettype "Notification" :targetid notificationid :message updated}]
          (if success
            (do
              (al/log "user" (merge {:userid userid} log_map))))
          (json/write-str (merge {:notification updated :success success} error)))))

    "createOverlay"
    (do
      (if-not (nil? (jsmap :overlayprops))
        (let [overlayid (get-in jsmap [:overlayprops :overlayid])
              siteid (get-in jsmap [:siteprops :siteid])
              result (json/read-str (neo4j/executeQuery cypher))
              exception (get result "exception" nil)
              error (if-not (nil? exception) {:error exception} {})
              success (nil? exception)
              props (stringify-keys (jsmap :overlayprops))
              log_map {:activity "createOverlay" :targettype "Overlay" :targetid overlayid :message props}]
          (if success
            (do
              (al/log "user" (merge {:userid userid} log_map))
              (al/log "site" (merge {:siteid siteid :userid userid} log_map))))
          (json/write-str (merge {:overlay props :success success} error)))))

    "createFixture"
    (do
      (if-not (nil? (jsmap :fixtureprops))
        (let [fixtureid (get-in jsmap [:fixtureprops :fixtureid])
              siteid (get-in jsmap [:siteprops :siteid])
              result (json/read-str (neo4j/executeQuery cypher))
              exception (get result "exception" nil)
              error (if-not (nil? exception) {:error exception} {})
              success (nil? exception)
              props (stringify-keys (jsmap :fixtureprops))
              log_map {:activity "createFixture" :targettype "Fixture" :targetid fixtureid :message props}]
          (if success
            (do
              (al/log "user" (merge {:userid userid} log_map))
              (al/log "site" (merge {:siteid siteid :userid userid} log_map))))
          (json/write-str (merge {:fixture props :success success} error)))))

    "createNotification"
    (do
      (if-not (nil? (jsmap :notificationprops))
        (let [notificationid (get-in jsmap [:notificationprops :notificationid])
              siteid (get-in jsmap [:siteprops :siteid])
              props (stringify-keys (jsmap :notificationprops))
              result (json/read-str (neo4j/executeQuery cypher props))
              exception (get result "exception" nil)
              error (if-not (nil? exception) {:error exception} {})
              success (nil? exception)
              log_map {:activity "createNotification" :targettype "notification" :targetid notificationid :message props}]
          (if success
            (do
              (al/log "user" (merge {:userid userid} log_map))
              (al/log "site" (merge {:siteid siteid :userid userid} log_map))))
          (json/write-str (merge {:notification props :success success} error)))))

    "searchNotifications"
    (do
      (if-not (nil? (jsmap :notificationprops))
        (let [result (json/read-str (neo4j/executeQuery cypher))
              items (get result "items" [])
              exception (get result "exception" nil)
              error (if-not (nil? exception) {:error exception} {})
              success (nil? exception)]
          (json/write-str (merge {:items items :success success} error)))))

    "createParkingZone"
    (do
      (if-not (nil? (jsmap :parkingzoneprops))
        (let [parkingzoneid (get-in jsmap [:parkingzoneprops :parkingzoneid])
              siteid (get-in jsmap [:siteprops :siteid])
              result (json/read-str (neo4j/executeQuery cypher))
              exception (get result "exception" nil)
              error (if-not (nil? exception) {:error exception} {})
              success (nil? exception)
              props (stringify-keys (jsmap :parkingzoneprops))
              log_map {:activity "createParkingZone" :targettype "parkingzone" :targetid parkingzoneid :message props}]
          (if success
            (do
              (al/log "user" (merge {:userid userid} log_map))
              (al/log "site" (merge {:siteid siteid :userid userid} log_map))))
          (json/write-str (merge {:parkingzone props :success success} error)))))

    "createParkingSpot"
    (do
      (if-not (nil? (jsmap :parkingspotprops))
        (let [parkingspotid (get-in jsmap [:parkingspotprops :parkingspotid])
              siteid (get-in jsmap [:siteprops :siteid])
              result (json/read-str (neo4j/executeQuery cypher))
              exception (get result "exception" nil)
              error (if-not (nil? exception) {:error exception} {})
              success (nil? exception)
              props (stringify-keys (jsmap :parkingspotprops))
              log_map {:activity "createParkingSpot" :targettype "parkingspot" :targetid parkingspotid :message props}]
          (if success
            (do
              (al/log "user" (merge {:userid userid} log_map))
              (al/log "site" (merge {:siteid siteid :userid userid} log_map))))
          (json/write-str (merge {:parkingspot props :success success} error)))))

    "createParkingGroup"
    (do
      (if-not (nil? (jsmap :parkinggroupprops))
        (let [parkinggroupid (get-in jsmap [:parkinggroupprops :parkinggroupid])
              siteid (get-in jsmap [:siteprops :siteid])
              result (json/read-str (neo4j/executeQuery cypher))
              exception (get result "exception" nil)
              error (if-not (nil? exception) {:error exception} {})
              success (nil? exception)
              props (stringify-keys (jsmap :parkinggroupprops))
              log_map {:activity "createParkingGroup" :targettype "parkinggroup" :targetid parkinggroupid :message props}]
          (if success
            (do
              (al/log "user" (merge {:userid userid} log_map))
              (al/log "site" (merge {:siteid siteid :userid userid} log_map))))
          (json/write-str (merge {:parkinggroup props :success success} error)))))

    "getUserPermissions"
    (do
      (let [auth-cypher "cyphers/get_all_usermodel_permissions_for_user.cypher"
            orgs-cypher "cyphers/get_all_orgs_and_sites.cypher"
            email (:email jsmap)
            props {"email" email}
            auth-result (-> auth-cypher
                            (render-resource)
                             (neo4j/executeQuery props)
                             (json/read-str))
            orgs-result (-> orgs-cypher
                            (render-resource)
                            (neo4j/executeQuery  props)
                            (json/read-str))
            exception (and (includes? auth-result "exception")
                           (includes? orgs-result "exception"))
            error (cond
                    exception {:error "Could not access permissions."}
                    (nil? email) {:error "Invalid email."}
                    :else nil)
            authorization (get auth-result "authorization")
            userid (get-in orgs-result ["user" "userid"])
            name (get-in orgs-result ["user" "name"])
            user (get orgs-result "user")
            access_stat  (if-not (nil? email)(al/get_access_stat email) {})
            result-map {
                :success (nil? error)
                :params userid
                :data {
                    :id userid
                    :name name
                    :user user
                    :authorization authorization
                    :login {
                        :userid userid
                        :email email
                        :last_seen (:last_seen access_stat)
                        :login_attempts (:login_attempts access_stat)
                     }
                 }
            }]
        (json/write-str (merge result-map error))))

    "getAllPermissions"
    (do
      (let [resultstr (neo4j/executeQuery cypher)
            resultjson (json/read-str resultstr)
            authorization (get resultjson "authorization" {})
            success (not (empty? authorization))]
        (debugf "Cypher %s" cypher)
        (debugf "Result %s" resultstr)
        (json/write-str {:authorization authorization :success success})))

    "deleteOrg"
    (do
      (if-not (nil? (jsmap :orgprops))
        (let [orgid (get-in jsmap [:orgprops :orgid])
              result (json/read-str (neo4j/executeQuery cypher))
              exception (get result "exception" nil)
              error (if-not (nil? exception) {:error exception} {})
              success (nil? exception)
              orgprops (stringify-keys (jsmap :orgprops))
              log_map {:activity "deleteOrg" :targettype "Org" :targetid orgid :message orgprops}]
          (if success
            (al/log "user" (merge {:userid userid} log_map)))
          (json/write-str (merge {:success success} error)))))

    "suspendOrg"
    (do
      (if-not (nil? (jsmap :orgprops))
        (let [orgid (get-in jsmap [:orgprops :orgid])
              result (json/read-str (neo4j/executeQuery cypher))
              exception (get result "exception" nil)
              error (if-not (nil? exception) {:error exception} {})
              success (nil? exception)
              orgprops (stringify-keys (jsmap :orgprops))
              log_map {:activity "suspendOrg" :targettype "Org" :targetid orgid :message orgprops}]
          (if success
            (al/log "user" (merge {:userid userid} log_map)))
          (json/write-str (merge {:success success} error)))))

    "activateOrg"
    (do
      (if-not (nil? (jsmap :orgprops))
        (let [orgid (get-in jsmap [:orgprops :orgid])
              result (json/read-str (neo4j/executeQuery cypher))
              exception (get result "exception" nil)
              error (if-not (nil? exception) {:error exception} {})
              success (nil? exception)
              orgprops (stringify-keys (jsmap :orgprops))
              log_map {:activity "activateOrg" :targettype "Org" :targetid orgid :message orgprops}]
          (if success
            (al/log "user" (merge {:userid userid} log_map)))
          (json/write-str (merge {:success success} error)))))

    "deleteFixture"
    (do
      (if-not (nil? (jsmap :fixtureprops))
        (let [fixtureid (get-in jsmap [:fixtureprops :fixtureid])
              siteid (get-in jsmap [:siteprops :siteid])
              result (json/read-str (neo4j/executeQuery cypher))
              exception (get result "exception" nil)
              error (if-not (nil? exception) {:error exception} {})
              success (nil? exception)
              fixtureprops (stringify-keys (jsmap :fixtureprops))
              log_map {:activity "deleteFixture" :targettype "Fixture" :targetid fixtureid :message fixtureprops}]
          (if success
            (do
              (al/log "user" (merge {:userid userid} log_map))
              (al/log "site" (merge {:siteid siteid :userid userid} log_map))))
          (json/write-str (merge {:success success} error)))))

    "realDeleteNode"
    (do
      (if-not (nil? (jsmap :nodeprops))
        (let [nodeid (get-in jsmap [:nodeprops :nodeid])
              siteid (get-in jsmap [:siteprops :siteid])
              result (json/read-str (neo4j/executeQuery cypher))
              exception (get result "exception" nil)
              error (if-not (nil? exception) {:error exception} {})
              success (nil? exception)
              nodeprops (stringify-keys (jsmap :nodeprops))
              log_map {:activity "realDeleteNode" :targettype "Node" :targetid nodeid :message nodeprops}]
          (when success
            (al/log "user" (merge {:userid userid} log_map))
            (al/log "site" (merge {:siteid siteid :userid userid} log_map))
            (al/log_node_status {:nodeid nodeid :siteid "Unknown" :orgid "Unknown"})
            (al/log_node_hierarchy {:nodeid nodeid :siteid "Unknown" :sitename "Unknown" :orgid "Unknown" :orgname "Unknown"}))
          (json/write-str (merge {:success success} error)))))

    ("addNodeToGroup"
     "addNodesToGroup")
    (add-node-to-group jsmap)

    "resendScheduleToLG"
(resend-schedule-to-lg jsmap)

    "resendScheduleToNode"
(resend-schedule-to-node jsmap)

    "removeNodeFromGroup"
    (when (jsmap :groupprops)
      (let [groupid (get-in jsmap [:groupprops :groupid])
            orgid (get-in jsmap [:orgprops :orgid])
            siteid (get-in jsmap [:siteprops :siteid])
            nodeid (get-in jsmap [:nodeprops :nodeid])
            nodeids (or (get-in jsmap [:nodeprops :nodeids])
                        [nodeid])
            prior-pdprofileids (->> nodeids
                                    (map pd/get-profile-from-node)
                                    (remove nil?)
                                    set)
            props {"groupid" groupid
                   "nodeids" nodeids}
            {:keys [exception]
             {:keys [lighting]} :result
             :as result} (-> cypher
                             (neo4j/executeQuery props)
                             (json/read-str :key-fn keyword))
            error (cond
                    exception {:error exception}
                    (empty? result) {:error "Node can not be removed from Site Lighting group"}
                    :else {})
            success (empty? error)
            message {"groupid" groupid
                     "nodeid" nodeid
                     "siteid" siteid}
            log_map {:activity "removeNodeFromGroup"
                     :targettype "Group"
                     :targetid groupid
                     :message message}]
        (when success
          (doseq [pdprofileid prior-pdprofileids]
            (pd/invalidate-pdprofile-cache-for-id pdprofileid))
          (al/log "user" (merge {:userid userid} log_map))
          (when siteid
            (al/log "site" (merge {:siteid siteid :userid userid} log_map)))

          (when lighting
            (lighting-group-diff-update {:outgoing nodeids}
                                        jsmap)))
        (json/write-str (-> error
                            (assoc :success success)
                            (merge (when success {:result message}))))))

    "deleteOverlay"
    (do
      (if-not (nil? (jsmap :overlayprops))
        (let [overlayid (get-in jsmap [:overlayprops :overlayid])
              siteid (get-in jsmap [:siteprops :siteid])
              result (json/read-str (neo4j/executeQuery cypher))
              exception (get result "exception" nil)
              error (if-not (nil? exception) {:error exception} {})
              success (nil? exception)
              overlayprops (stringify-keys (jsmap :overlayprops))
              log_map {:activity "deleteOverlay" :targettype "Overlay" :targetid overlayid :message overlayprops}]
          (if success
            (do
              (al/log "user" (merge {:userid userid} log_map))
              (al/log "site" (merge {:siteid siteid :userid userid} log_map))))
          (json/write-str (merge {:success success} error)))))

    "deleteGroup"
    (when (:groupprops jsmap)
      (let [groupid (get-in jsmap [:groupprops :groupid])
            group (-> {:type "getGroup"
                       :groupprops {:groupid groupid}}
                      template-selector
                      db-selector
                      (json/read-str :key-fn keyword)
                      :group)
            {:keys [nodeList type]} group]
        (cond
          (= "site-lighting"
             type) (json/write-str {:error "A site lighting group cannot be deleted."})
          (and (seq nodeList)
               (= "lighting"
                  type)) (json/write-str {:error "Lighting Group can't be deleted when it has nodes. Please move the nodes to a different group before deleting this lighting group."})
          :else (let [siteid (get-in jsmap [:siteprops :siteid])
                      result (json/read-str (neo4j/executeQuery cypher))
                      exception (get result "exception" nil)
                      error (if-not (nil? exception) {:error exception} {})
                      success (nil? exception)
                      groupprops (stringify-keys (jsmap :groupprops))
                      log_map {:activity "deleteGroup"
                               :targettype "Group"
                               :targetid groupid
                               :message groupprops}]
                  (when success
                    (al/log "user" (merge {:userid userid} log_map))
                    (al/log "site" (merge {:siteid siteid :userid userid} log_map)))
                  (json/write-str (merge {:success success} error))))))

    "deleteNotification"
    (do
      (if-not (nil? (jsmap :notificationprops))
        (let [notificationid (get-in jsmap [:notificationprops :notificationid])
              siteid (:siteid (jsmap :siteprops))
              result (json/read-str (neo4j/executeQuery cypher))
              exception (get result "exception" nil)
              error (cond
                      (some? exception) {:error exception}
                      (empty? result) {:error (format "Notification with id: '%s' not found" notificationid) :status 404}
                      :else {})
              success (empty? error)

              notificationprops (stringify-keys (jsmap :notificationprops))
              log_map {:activity "deleteNotification" :targettype "Notification" :targetid notificationid :message notificationprops}]
          (if success
            (do
              (al/log "site" (merge {:siteid siteid :userid userid} log_map))
              (al/log "user" (merge {:userid userid} log_map))))
          (json/write-str (merge {:success success} error)))))

    "deleteParkingZone"
    (do
      (if-not (nil? (jsmap :parkingzoneprops))
        (let [parkingzoneid (get-in jsmap [:parkingzoneprops :parkingzoneid])
              siteid (get-in jsmap [:siteprops :siteid])
              result (json/read-str (neo4j/executeQuery cypher))
              exception (get result "exception" nil)
              error (if-not (nil? exception) {:error exception} {})
              success (nil? exception)
              props (stringify-keys (jsmap :parkingzoneprops))
              log_map {:activity "deleteParkingZone" :targettype "parkingzone" :targetid parkingzoneid :message props}]
          (if success
            (do
              (al/log "user" (merge {:userid userid} log_map))
              (al/log "site" (merge {:siteid siteid :userid userid} log_map))))
          (json/write-str (merge {:success success} error)))))

    "deleteParkingSpot"
    (do
      (if-not (nil? (jsmap :parkingspotprops))
        (let [parkingspotid (get-in jsmap [:parkingspotprops :parkingspotid])
              siteid (get-in jsmap [:siteprops :siteid])
              result (json/read-str (neo4j/executeQuery cypher))
              exception (get result "exception" nil)
              error (if-not (nil? exception) {:error exception} {})
              success (nil? exception)
              props (stringify-keys (jsmap :parkingspotprops))
              log_map {:activity "deleteParkingSpot" :targettype "parkingspot" :targetid parkingspotid :message props}]
          (if success
            (do
              (al/log "user" (merge {:userid userid} log_map))
              (al/log "site" (merge {:siteid siteid :userid userid} log_map))))
          (json/write-str (merge {:success success} error)))))

    "deleteParkingGroup"
    (do
      (if-not (nil? (jsmap :parkinggroupprops))
        (let [parkinggroupid (get-in jsmap [:parkinggroupprops :parkinggroupid])
              siteid (get-in jsmap [:siteprops :siteid])
              result (json/read-str (neo4j/executeQuery cypher))
              exception (get result "exception" nil)
              error (if-not (nil? exception) {:error exception} {})
              success (nil? exception)
              props (stringify-keys (jsmap :parkinggroupprops))
              log_map {:activity "deleteParkingGroup" :targettype "parkinggroup" :targetid parkinggroupid :message props}]
          (if success
            (do
              (al/log "user" (merge {:userid userid} log_map))
              (al/log "site" (merge {:siteid siteid :userid userid} log_map))))
          (json/write-str (merge {:success success} error)))))

    "deleteSite"
    (do
      (if-not (nil? (jsmap :siteprops))
        (let [siteid (get-in jsmap [:siteprops :siteid])
              result (json/read-str (neo4j/executeQuery cypher))
              exception (get result "exception" nil)
              error (if-not (nil? exception) {:error exception} {})
              success (nil? exception)
              deleted (stringify-keys (jsmap :siteprops))
              log_map {:activity "deleteSite" :targettype "Site" :targetid siteid :message deleted}]
          (if success
            (do
              (al/log "user" (merge {:userid userid} log_map))
              (al/log "site" (merge {:siteid siteid :userid userid} log_map))))
          (json/write-str (merge {:success success} error)))))

    "suspendSite"
    (do
      (if-not (nil? (jsmap :siteprops))
        (let [siteid (get-in jsmap [:siteprops :siteid])
              result (json/read-str (neo4j/executeQuery cypher))
              exception (get result "exception" nil)
              error (if-not (nil? exception) {:error exception} {})
              success (nil? exception)
              suspended (stringify-keys (jsmap :siteprops))
              log_map {:activity "suspendSite" :targettype "Site" :targetid siteid :message suspended}]
          (if success
            (do
              (al/log "user" (merge {:userid userid} log_map))
              (al/log "site" (merge {:siteid siteid :userid userid} log_map))))
          (json/write-str (merge {:success success} error)))))

    "activateSite"
    (do
      (if-not (nil? (jsmap :siteprops))
        (let [siteid (get-in jsmap [:siteprops :siteid])
              result (json/read-str (neo4j/executeQuery cypher))
              exception (get result "exception" nil)
              error (if-not (nil? exception) {:error exception} {})
              success (nil? exception)
              activated (stringify-keys (jsmap :siteprops))
              log_map {:activity "activateSite" :targettype "Site" :targetid siteid :message activated}]
          (if success
            (do
              (al/log "user" (merge {:userid userid} log_map))
              (al/log "site" (merge {:siteid siteid :userid userid} log_map))))
          (json/write-str (merge {:success success} error)))))

    "createInstallPod"
    (do
      (if-not (nil? (jsmap :podprops))
        (let [podid (get-in jsmap [:podprops :podid])
              siteid (get-in jsmap [:siteprops :siteid])
              result (json/read-str (neo4j/executeQuery cypher))
              exception (get result "exception" nil)
              error (if-not (nil? exception) {:error exception} {})
              success (nil? exception)
              props (stringify-keys (jsmap :podprops))
              log_map {:activity "createInstallPod" :targettype "Pod" :targetid podid :message props}]
          (if success
            (do
              (al/log "user" (merge {:userid userid} log_map))
              (al/log "site" (merge {:siteid siteid :userid userid} log_map))))
          (json/write-str (merge {:pod props :success success} error)))))

    "deleteInstallPod"
    (do
      (if-not (nil? (jsmap :podprops))
        (let [podid (get-in jsmap [:podprops :podid])
              siteid (get-in jsmap [:siteprops :siteid])
              result (json/read-str (neo4j/executeQuery cypher))
              exception (get result "exception" nil)
              error (if-not (nil? exception) {:error exception} {})
              success (nil? exception)
              podprops (stringify-keys (jsmap :podprops))
              log_map {:activity "deleteInstallPod" :targettype "Pod" :targetid podid :message podprops}]
          (if success
            (do
              (al/log "user" (merge {:userid userid} log_map))
              (al/log "site" (merge {:siteid siteid :userid userid} log_map))))
          (json/write-str (merge {:pod (stringify-keys (jsmap :podprops)) :success success} error)))))

    "createFirmware"
    (do
      (if-not (nil? (jsmap :firmwareprops))
        (let [firmwareid (get-in jsmap [:firmwareprops :firmwareid])
              result (json/read-str (neo4j/executeQuery cypher))
              exception (get result "exception" nil)
              error (if-not (nil? exception) {:error exception} {})
              success (nil? exception)
              props (stringify-keys (jsmap :firmwareprops))
              log_map {:activity "createFirmware" :targettype "Firmware" :targetid firmwareid :message props}]
          (if success
            (al/log "user" (merge {:userid userid} log_map)))
          (json/write-str (merge {:firmware props :success success} error)))))

    "deleteFirmware"
    (do
      (if-not (nil? (jsmap :firmwareprops))
        (let [firmwareid (get-in jsmap [:firmwareprops :firmwareid])
              result (json/read-str (neo4j/executeQuery cypher))
              exception (get result "exception" nil)
              error (if-not (nil? exception) {:error exception} {})
              success (nil? exception)
              firmwareprops (stringify-keys (jsmap :firmwareprops))
              log_map {:activity "deleteFirmware" :targettype "Firmware" :targetid firmwareid :message firmwareprops}]
          (if success
            (al/log "user" (merge {:userid userid} log_map)))
          (json/write-str (merge {:success success} error)))))

    ( "assignFirmwareToNode"
     "assignFirmwareToGroup"
     "assignFirmwareToSite")
    (assign-firmware jsmap)

    ("getAllPDProfiles"
     "getAllETDHProfiles"
     "getAllDHProfiles"
     #_"getAllSchedules")
    (do
      (let [siteid (get-in jsmap [:siteprops :siteid])
            results (json/read-str (neo4j/executeQuery cypher {"siteid" siteid}))
            exception (get results "exception" nil)
            items (get results "items" [])
            items (if (map? items) [items] items)
            error (if-not (nil? exception) {:error exception} {})
            success (nil? exception)]
        (json/write-str (merge {:items (cond
                                         (= "getAllETDHProfiles"
                                            qtype) (map #(update %
                                                                 "scheduled" json/read-str)
                                                        items)
                                         :else items)
                                :success success}
                               error))))

    "createETDHProfile"
    (let [{{:keys [siteid]} :siteprops
           {:keys [etdhprofileid]
            :as etdhprofile-orig} :etdhprofileprops
           :as jsmap} (update jsmap
                              :etdhprofileprops etdh/cleanup-etdhprofile)
          etdhprofile (prepare-etdhprofile-for-write etdhprofile-orig)
          props (stringify-keys {:siteid siteid
                                 :etdhprofile etdhprofile})
          {:keys [exception]} (-> cypher
                                  (neo4j/executeQuery props)
                                  (json/read-str :key-fn keyword))
          error (if-not (nil? exception)
                  {:error exception}
                  {})
          success (empty? error)
          log_map {:activity "createETDHprofile"
                   :targettype "ETDHProfile"
                   :targetid etdhprofileid
                   :message props}]
      (when success
        (al/log "user" (merge {:userid userid} log_map))
        (al/log "site" (merge {:siteid siteid} log_map))
        (etdh/handle-cape qtype jsmap))
      (json/write-str (assoc error
                             :etdhprofile etdhprofile-orig
                             :success success)))

    "createDHProfile"
    (when (:dhprofileprops jsmap)
      (let [{:keys [dhprofileid]
             :as dhprofile} (:dhprofileprops jsmap)
            dhprofile (-> dhprofile
                          (assoc :autocalibrate true))
            siteid (get-in jsmap [:siteprops :siteid])
            props (stringify-keys dhprofile)
            result (neo4j/executeQuery cypher props)
            exception (get result "exception" nil)
            error (if-not (nil? exception) {:error exception} {})
            success (nil? exception)
            log_map {:activity "createDHprofile"
                     :targettype "DHProfile"
                     :targetid dhprofileid
                     :message props}]
        (when success
          (al/log "user" (merge {:userid userid} log_map))
          (al/log "site" (merge {:siteid siteid :userid userid} log_map))
          (dh/schedule-new-profile dhprofileid))
        (json/write-str (merge {:dhprofile props :success success} error))))

    "createPDProfile"
    (do
      (if-not (nil? (jsmap :pdprofileprops))
        (let [pdprofileid (get-in jsmap [:pdprofileprops :pdprofileid])
              siteid (get-in jsmap [:siteprops :siteid])
              events (stringify-keys (:pdprofileprops jsmap))
              result (json/read-str (neo4j/executeQuery cypher events))
              exception (get result "exception" nil)
              error (if-not (nil? exception) {:error exception} {})
              success (nil? exception)
              props (stringify-keys (jsmap :pdprofileprops))
              log_map {:activity "createPDProfile" :targettype "PDProfile" :targetid pdprofileid :message props}]
          (when success
            (al/log "user" (merge {:userid userid} log_map))
            (al/log "site" (merge {:siteid siteid :userid userid} log_map))
            (pd/handle-cape qtype jsmap))
          (json/write-str (merge {:pdprofile props :success success} error)))))

    "deletePDProfile"
    (when (jsmap :pdprofileprops)
      (let [pdprofileid (get-in jsmap [:pdprofileprops :pdprofileid])
            siteid (get-in jsmap [:siteprops :siteid])
            orgid (get-in jsmap [:orgprops :orgid])
            props {"orgid" orgid
                   "siteid" siteid
                   "pdprofileid" pdprofileid}
            {:keys [exception
                    result]} (-> cypher
                                 (neo4j/executeQuery props)
                                 (json/read-str :key-fn keyword))
            error (cond
                    (some? exception) {:error exception}
                    (nil? result) {:error "Could not find the specified pdprofile"}
                    :else {})
            success (= {}
                       error)
            log_map {:activity "deletePDProfile"
                     :targettype "PDProfile"
                     :targetid pdprofileid
                     :message props}]
        (when success
          (al/log "user" (assoc log_map
                                :userid userid))
          (al/log "site" (assoc log_map
                                :siteid siteid :userid userid))
          (pd/handle-cape qtype jsmap))
        (json/write-str (assoc error
                               :success success))))

    "deleteETDHProfile"
    (when (jsmap :etdhprofileprops)
      (let [etdhprofileid (get-in jsmap [:etdhprofileprops :etdhprofileid])
            siteid (get-in jsmap [:siteprops :siteid])
            orgid (get-in jsmap [:orgprops :orgid])
            props {"orgid" orgid
                   "siteid" siteid
                   "etdhprofileid" etdhprofileid}
            {:keys [exception
                    result]} (-> cypher
                         (neo4j/executeQuery props)
                         (json/read-str :key-fn keyword))
            error (cond
                    (some? exception) {:error exception}
                    (nil? result) {:error "Could not find the specified etdhprofile"}
                    :else {})
            success (= {}
                       error)
            log_map {:activity "deleteETDHProfile"
                     :targettype "ETDHProfile"
                     :targetid etdhprofileid
                     :message props}]
        (when success
          (al/log "user" (assoc log_map
                                :userid userid))
          (al/log "site" (assoc log_map
                                :siteid siteid))
          (etdh/handle-cape qtype jsmap))
        (json/write-str (assoc error
                               :success success))))

    "deleteDHProfile"
    (when (jsmap :dhprofileprops)
      (let [dhprofileid (get-in jsmap [:dhprofileprops :dhprofileid])
            siteid (get-in jsmap [:siteprops :siteid])
            orgid (get-in jsmap [:orgprops :orgid])
            props {"orgid" orgid
                   "siteid" siteid
                   "dhprofileid" dhprofileid}
            {:keys [exception
                    result]} (-> cypher
                         (neo4j/executeQuery props)
                         (json/read-str :key-fn keyword))
            error (cond
                    (some? exception) {:error exception}
                    (nil? result) {:error "Could not find the specified dhprofile"}
                    :else {})
            success (= {}
                       error)
            log_map {:activity "deleteDHProfile"
                     :targettype "DHProfile"
                     :targetid dhprofileid
                     :message props}]
        (when success
          (al/log "user" (assoc log_map
                                :userid userid))
          (al/log "site" (assoc log_map
                                :siteid siteid :userid userid)))
        (json/write-str (assoc error
                               :success success))))

    "deleteSchedule" (delete-schedule jsmap)

    "getSensorHistory"
    (do
      (if-not (nil? (jsmap :extprops))
        (let [siteid (get-in jsmap [:siteprops :siteid])
              nodeid (get-in jsmap [:nodeprops :nodeid])
              sensorid (get-in jsmap [:extprops :sensorid])
              date (.toDate (cformat/parse (get-in jsmap [:extprops :date])))
              limit (get-in jsmap [:extprops :limit])
              site (-> "cyphers/get_site.cypher"
                       io/resource
                       slurp
                       (neo4j/executeQuery {"siteid" siteid})
                       json/read-str)
              tz (get-in site ["site" "time_zone"] "UTC")
              results (spy :debug (al/get_latest_sensor_samples nodeid sensorid date limit tz))
              exception (get results "exception" nil)
              error (if-not (nil? exception) {:error exception} {})
              success (nil? exception)
              ctrlmap {:nodeprops {:nodeid [nodeid] :type "SensorSampleReq"}
                       :orgprops {:orgid (get-in jsmap [:orgprops :orgid])}
                       :siteprops {:siteid siteid}
                       :extprops {:sensor (nodeconfig/translate-sensor-id sensorid)}}]
          (cape-future dctrl/query-exec-msgpk ctrlmap)
          (json/write-str (merge {:items results :success success} error)))))

    "getSensorHistoryFromTo"
    (do
      (if-not (nil? (jsmap :extprops))
        (let [siteid (get-in jsmap [:siteprops :siteid])
              nodeid (get-in jsmap [:nodeprops :nodeid])
              sensorid (get-in jsmap [:extprops :sensorid])
              date1 (.toDate (cformat/parse (get-in jsmap [:extprops :date1])))
              date2 (.toDate (cformat/parse (get-in jsmap [:extprops :date2])))
              limit (get-in jsmap [:extprops :limit])
              period (get-in jsmap [:extprops :period])
              site (-> "cyphers/get_site.cypher"
                       io/resource
                       slurp
                       (neo4j/executeQuery {"siteid" siteid})
                       json/read-str)
              tz (get-in site ["site" "time_zone"] "UTC")
              results (spy :debug (al/get_latest_sensor_samples_ft nodeid sensorid date1 date2 limit period tz siteid))
              exception (get results "exception" nil)
              error (if-not (nil? exception) {:error exception} {})
              success (nil? exception)
              props (stringify-keys (jsmap :extprops))
              ctrlmap {:nodeprops {:nodeid [nodeid] :type "SensorSampleReq"}
                       :orgprops {:orgid (get-in jsmap [:orgprops :orgid])}
                       :siteprops {:siteid siteid}
                       :extprops {:sensor (nodeconfig/translate-sensor-id sensorid)}}]
          (cape-future dctrl/query-exec-msgpk ctrlmap)
          (json/write-str (merge {:items results :success success} error)))))
    "getNodeConnectionStatus"
    (do
      (let [nodeid (get-in jsmap [:nodeprops :nodeid])
            results (spy  :debug (al/get_latest_connection_status nodeid))
            exception (get results "exception" nil)
            error (if-not (nil? exception) {:error exception} {})
            success (nil? exception)]
        (json/write-str (merge {:status results :success success} error))))

    "getParkingInfoSite"
    (do
      (let [siteid (get-in jsmap [:siteprops :siteid])
            from (get-in jsmap [:extprops :from])
            to (get-in jsmap [:extprops :to])
            filter (get-in jsmap [:extprops])
            results (spy :debug (if (nil? from) (al/get_latest_parking_info siteid) (al/get_parking_history siteid filter)))
            exception (get results "exception" nil)
            error (if-not (nil? exception) {:error exception} {})
            success (nil? exception)]
        (json/write-str (merge {:status results :success success} error))))

    ("getCurrentTrafficInfo" "getTrafficHistory" "getTrafficConfig")
      (getTrafficData jsmap)

    "getLostAndFoundNodesStatuses"
    (do
      (let [results (spy :debug (al/get_latest_lf_nodes_statuses))
            exception (get results "exception" nil)
            error (if-not (nil? exception) {:error exception} {})
            success (nil? exception)]
        (json/write-str (merge {:status results :success success} error))))

    "getParkingZones"
    (do
      (let [siteid (get-in jsmap [:siteprops :siteid])
            results (spy :debug (al/get_parkingzone siteid))
            exception (get results "exception" nil)
            error (if-not (nil? exception) {:error exception} {})
            success (nil? exception)]
        (json/write-str (merge {:status results :success success} error))))

    "getOneParkingZone"
    (do
      (let [siteid (get-in jsmap [:siteprops :siteid])
            parkingzoneid (get-in jsmap [:parkingzoneprops :parkingzoneid])
            results (spy :debug (al/get_one_parkingzone siteid parkingzoneid))
            exception (get results "exception" nil)
            error (if-not (nil? exception) {:error exception} {})
            success (nil? exception)]
           (json/write-str (merge {:status results :success success} error))))

    "getNodeLightStatus"
    (do
      (let [nodeid (get-in jsmap [:nodeprops :nodeid])
            results (spy :debug (al/get_latest_light_mode nodeid))
            exception (get results "exception" nil)
            error (if-not (nil? exception) {:error exception} {})
            success (nil? exception)
            res (merge {:nodeid nodeid :isscheduled false :driver 0 :harvest_trigger false :policy "unknown"} results)]
        (json/write-str (merge {:status res :success success} error) :value-fn time-aware-value-writer)))

    ("applyDHtoSite"
     "applyDHtoGroup"
     "applyDHtoNodes") (apply-daylight-harvesting jsmap)

    ("applyPDtoSite"
     "applyPDtoGroup"
     "applyPDtoNodes") (apply-proximity-dimming jsmap)

    ( "applyScheduleToGroup"
     "applyScheduleToSite"
     "applyScheduleToNode"
     "applyScheduleToNodes")
    (apply-schedule jsmap)

    ("assignFixtureToNode"
     "assignFixtureToNodes"
     "assignFixtureToSite"
     "assignFixtureToGroup")
    (assign-fixture jsmap)

    ( "applyConfigToSite"
     "applyConfigToGroup"
     "applyConfigToNodes")
    (maybe-apply-config jsmap)

    "applyServerToNodes" (apply-server jsmap)
    ("removeETDHProfileTriggers"
     "addETDHProfileTriggers")
    (manipulate-etdh-triggers jsmap)
    ("removeDHProfileTrigger"
     "addDHProfileTrigger")
    (when jsmap
      (let [siteid (get-in jsmap [:siteprops :siteid])
            nodeid (get-in jsmap [:nodeprops :nodeid])
            dhprofileid (get-in jsmap [:dhprofileprops :dhprofileid])
            result (json/read-str (spy :debug (neo4j/executeQuery cypher)))
            exception (get result "exception" nil)
            error (cond
                    exception {:error exception}
                    (nil? (result "dhprofile")) {:error "Could not find dhprofile with trigger node."}
                    :else nil)
            success (nil? error)
            message {"dhprofileid" dhprofileid
                     "nodeid" nodeid
                     "siteid" siteid}
            log_map {:activity qtype
                     :targettype "Node"
                     :targetid nodeid
                     :message message}
            json (merge {:success success}
                        (when error
                          error))]
        (when success
          (al/log "user" (merge {:userid userid} log_map))
          (al/log "site" (merge {:siteid siteid :userid userid} log_map)))
        (json/write-str json)))

    "calibrateDHProfile" (calibrate-daylight-harvesting jsmap)

    ( "getAlertByNodeNameType" "getAlertSys" ) (get-AlertSys-AlertByNodeNameType jsmap)

    ( "getNotificationSys" )
    (do
      (if-not (nil? (jsmap :notificationprops))
        (let [result (json/read-str (neo4j/executeQuery cypher))
              notification (get result "notification" nil)
              exception (get result "exception" nil)
              error (if-not (nil? exception) {:error exception} {})
              success (nil? exception)]
          (json/write-str (merge {:notification notification :success success} error)))))

    "createBulkNode"
    (do
      (if-not (nil? (jsmap :nodeprops))
        (let [nodes (get-in jsmap [:nodeprops :nodes])
              emptynode (get-in jsmap [:extprops :emptynode] false)
              cypherQuery (if emptynode
                            (render-resource "cyphers/load_empty_nodes.cypher")
                            (render-resource "cyphers/load_bulk_nodes.cypher"))
              result (spy :debug (keywordize-keys (json/read-str (neo4j/executeQuery cypherQuery {"nodes" (stringify-keys nodes)}))))
              rnodes (get result :nodes)
              exception (or (get result "exception" nil) (if-not (seq rnodes) "Input had no valid nodes."))
              error (if-not (nil? exception) {:error exception} {})
              success (nil? exception)]
          (when success
            (doseq [n rnodes]
                (let [nodeid (get n :nodeid)
                      model (get n :model "Unknown")
                      siteid (get n :siteid "Unknown")
                      sitename (get n :sitename "Unknown")
                      orgid (get n :orgid "Unknown")
                      orgname (get n :orgname "Unknown")
                      latitude (get n :latitude)
                      longitude (get n :longitude)]
                  (when (and (not-empty siteid) (not-empty orgid)))
                    (al/log_node_status {:nodeid nodeid :siteid siteid :orgid orgid})
                    (al/log_node_hierarchy {:nodeid nodeid :nodehw model :siteid siteid :sitename sitename :orgid orgid :orgname orgname})
                    (send-gps-update userid orgid siteid nodeid  latitude longitude))))
          (json/write-str (merge {:success success :nodeids (map #(get % :nodeid) rnodes)} error)))))))

;; returns the same output as the input if validation check passes
(defn checkprops [{:keys [user]
                   :as jsmap}]
  (trace "checkprops ...")
  (try
    (if (nil? user) (throw (ex-info "user prop is absent - security violation" {:cause :bad-request :status 400})))
    (MDC/put "USER" user)

    (case (jsmap :type)
      "getAllOrgs" jsmap
      "getAllSuspendedOrgs" jsmap

      "getAllConfigs" (if (get-in jsmap [:siteprops :siteid])
                        jsmap
                        (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))
      "getDefaultConfigs" (if (get-in jsmap [:nodeprops :model])
                            jsmap
                            (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "getDefaultConfigsForSite" (if (get-in jsmap [:nodeprops :model])
                            jsmap
                            (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))


      "createConfig" (if (and (get-in jsmap [:configprops :configid])
                              (get-in jsmap [:siteprops :siteid]))  ; siteid is needed here for logging to cassandra
                       jsmap
                       (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      ("getConfig"
       "deleteConfig"
       "updateConfig") (if (and (get-in jsmap [:configprops :configid])
                                (get-in jsmap [:siteprops :siteid]))
                         jsmap
                         (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "getOrg" (if (get-in jsmap [:orgprops :orgid])
                 jsmap
                 (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "getSite" (if (get-in jsmap [:siteprops :siteid])
                  jsmap
                  (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      ("getNode"
       "resendScheduleToNode") (if (get-in jsmap [:nodeprops :nodeid])
                                 jsmap
                                 (throw (ex-info "checkprops: property check failed"
                                                 {:cause :bad-request
                                                  :status 400})))

      "getSensorHistory" (if (get-in jsmap [:nodeprops :nodeid])
                           jsmap
                           (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "getSensorHistoryFromTo" (if (get-in jsmap [:nodeprops :nodeid])
                                 jsmap
                                 (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "getNodeConnectionStatus" (if (get-in jsmap [:nodeprops :nodeid])
                                  jsmap
                                  (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))
      "getSiteNodesStatuses" (if (get-in jsmap [:siteprops :siteid])
                               jsmap
                               (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))
      "getLostAndFoundNodesStatuses" jsmap

      "getNodeLightStatus" (if (get-in jsmap [:nodeprops :nodeid])
                             jsmap
                             (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "getParkingInfoSite" (if (get-in jsmap [:siteprops :siteid])
                             jsmap
                             (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "getCurrentTrafficInfo" (if (get-in jsmap [:siteprops :siteid])
                             jsmap
                             (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "getTrafficHistory" (if (get-in jsmap [:siteprops :siteid])
                               jsmap
                               (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))
      "getTrafficConfig" (if (get-in jsmap [:siteprops :siteid])
                               jsmap
                               (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "getOverlay" (if (get-in jsmap [:overlayprops :overlayid])
                     jsmap
                     (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "getGroup" (if (get-in jsmap [:groupprops :groupid])
                   jsmap
                   (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "getFixture" (if (get-in jsmap [:fixtureprops :fixtureid])
                     jsmap
                     (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "getAllFixtures" (if (get-in jsmap [:siteprops :siteid])
                         jsmap
                         (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "getNotification" (if (and (get-in jsmap [:notificationprops :notificationid])
                                 (get-in jsmap [:siteprops :siteid]))
                          jsmap
                          (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "getNotificationSys" (if (and (get-in jsmap [:notificationprops :notificationid])
                                    (get-in jsmap [:siteprops :siteid]))
                             jsmap
                             (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "getAlert" (if (get-in jsmap [:alertprops :alertid])
                   (if (get-in jsmap [:siteprops :siteid])
                     jsmap
                     (throw (ex-info "checkprops: property check failed: siteid is missing from args of getAlert" {:cause :bad-request :status 400})))
                   (throw (ex-info "checkprops: property check failed: alertid is missing from args of getAlert" {:cause :bad-request :status 400})))

      "getParkingZones" (if (get-in jsmap [:siteprops :siteid])
                         jsmap
                         (throw (Exception. "checkprops: property check failed")))

      "getOneParkingZone" (if (get-in jsmap [:siteprops :siteid])
                            (if (get-in jsmap [:parkingzoneprops :parkingzoneid])
                            jsmap
                            (throw (ex-info "checkprops: property check failed - parking zone id missing" {:cause :bad-request :status 400})))
                          (throw (Exception. "checkprops: property check failed - site id missing")))

      "getParkingSpot" (if (get-in jsmap [:parkingspotprops :parkingspotid])
                         jsmap
                         (throw (Exception. "checkprops: property check failed")))

      "getParkingGroup" (if (get-in jsmap [:parkinggroupprops :parkinggroupid])
                          jsmap
                          (throw (Exception. "checkprops: property check failed")))

      "getAlertByNodeNameType" (if (get-in jsmap [:alertprops :nodeid])
                                 (if (get-in jsmap [:alertprops :name])
                                   (if (get-in jsmap [:alertprops :type])
                                     jsmap
                                     (throw (ex-info "checkprops: property check failed - alert type missing" {:cause :bad-request :status 400})))
                                   (throw (ex-info "checkprops: property check failed - alert name missing" {:cause :bad-request :status 400})))
                                 (throw (ex-info "checkprops: property check failed - alert nodeid missing" {:cause :bad-request :status 400})))

      "getAlertSys" (if (get-in jsmap [:alertprops :alertid])
                      jsmap
                      (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "getAllSitesForOrg" (if (get-in jsmap [:orgprops :orgid])
                            jsmap
                            (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "getAllSuspendedSitesForOrg" (if (get-in jsmap [:orgprops :orgid])
                                     jsmap
                                     (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "autoComplete" (if (get-in jsmap [:autoprops :categories])
                       jsmap (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      ;; The unode_chekprops passes it return the JS as it is for further processing
      "createUNode" (if (pcheck/unode_checkprops (jsmap :nodeprops)) jsmap)
      ;; Reference: {'type': 'createNodeToSite', 'user':'uberuser', 'nodeprops':  {'nodeid': 'someid',
      ;; 'name': 'somename'}, 'siteprops': {'siteid': 'siteid001'}}
      "createUNodeToSite" (if (and (pcheck/unode_checkprops (jsmap :nodeprops)) (some? (get-in jsmap [:siteprops :siteid])))
                            jsmap
                            (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))
      "getAllNodesForSite" (if (get-in jsmap [:siteprops :siteid])
                             jsmap
                             (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "getAllNodeIdsForModelSite" (if (get-in jsmap [:siteprops :siteid])
                                      (if (get-in jsmap [:nodeprops :model])
                                          jsmap
                                          (throw (ex-info "checkprops: property check failed - model missing" {:cause :bad-request :status 400})))
                                      (throw (ex-info "checkprops: property check failed - siteid missing" {:cause :bad-request :status 400})))

      "getAllNodeIdsForModelGroup" (if (get-in jsmap [:siteprops :siteid])
                                      (if (get-in jsmap [:nodeprops :groupid])
                                        (if (get-in jsmap [:nodeprops :model])
                                            jsmap
                                            (throw (ex-info "checkprops: property check failed - model missing" {:cause :bad-request :status 400})))
                                          (throw (ex-info "checkprops: property check failed - groupid missing" {:cause :bad-request :status 400})))
                                      (throw (ex-info "checkprops: property check failed - siteid missing" {:cause :bad-request :status 400})))

      "getAllMinNodesForSite" (if (get-in jsmap [:siteprops :siteid])
                                jsmap
                                (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "getAllLostAndFoundNodes"  jsmap

      "getAllOverlays" (if (get-in jsmap [:siteprops :siteid])
                         jsmap
                         (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))
      "getAllGroups" (if (get-in jsmap [:siteprops :siteid])
                       jsmap
                       (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))
      "getAllNotificationsForSite" (if (get-in jsmap [:siteprops :siteid])
                                     jsmap
                                     (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "getAllNotificationsForOrg" (if (get-in jsmap [:orgprops :orgid])
                                      (if (get-in jsmap [:userprops :userid])
                                          jsmap
                                          (throw (ex-info "checkprops: property check failed - userid missing" {:cause :bad-request :status 400})))
                                      (throw (ex-info "checkprops: property check failed - orgid missing" {:cause :bad-request :status 400})))

      "getAllNotificationsForUser" jsmap
                                        ;"getAllNotificationsForUser" (if (get-in jsmap [:siteprops :siteid])
                                        ;                       jsmap
                                        ;                       (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "getAllNotificationsByName" (if (pcheck/notification_checkprops_by_name (jsmap :notificationprops) (jsmap :siteprops))
                                    jsmap
                                    (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "searchNotifications" jsmap

      "getAllAlerts" (if (get-in jsmap [:siteprops :siteid])
                       jsmap
                       (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "getAlertsForNode" (if (get-in jsmap [:nodeprops :nodeid])
                           (if (get-in jsmap [:siteprops :siteid])
                             jsmap
                             (throw (ex-info "checkprops: property check failed - siteid missing" {:cause :bad-request :status 400})))
                           (throw (ex-info "checkprops: property check failed - nodeid missing" {:cause :bad-request :status 400})))

      "getAllOrgAlerts" (if (get-in jsmap [:orgprops :orgid])
                             jsmap
                             (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "searchAlerts" jsmap

      "getAllParkingZones" (if (get-in jsmap [:siteprops :siteid])
                             jsmap
                             (throw (Exception. "checkprops: property check failed")))

      "getAllParkingSpots" (if (get-in jsmap [:siteprops :siteid])
                             jsmap
                             (throw (Exception. "checkprops: property check failed")))

      "getAllParkingGroups" (if (get-in jsmap [:siteprops :siteid])
                              jsmap
                              (throw (Exception. "checkprops: property check failed")))

      "getActivityLogs" (if (pcheck/activity_logs_checkprops (jsmap :extprops) (jsmap :siteprops) (jsmap :orgprops))
                          jsmap
                          (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "logActivity" jsmap

      "otaStatusForSite" (if (pcheck/ota_status_checkprops (jsmap :siteprops) (jsmap :orgprops))
                          jsmap
                          (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "otaStatusForJob" (if (pcheck/ota_job_checkprops (jsmap :siteprops) (jsmap :orgprops) (jsmap :otaprops))
                          jsmap
                          (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "otaStatusJobUpdate" (if (pcheck/ota_job_checkprops (jsmap :siteprops) (jsmap :orgprops) (jsmap :otaprops))
                             jsmap
                             (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "getUser" (if (pcheck/user_checkprops (jsmap :userprops) (jsmap :orgprops))
                  jsmap
                  (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "getUserEmail" (if (pcheck/user_email_checkprops (jsmap :useremailprops) )
                       jsmap
                       (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "createUser" (if (pcheck/add_user_checkprops (jsmap :userprops) (jsmap :orgprops))
                     jsmap
                     (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "updateUser" (if (pcheck/add_user_checkprops (jsmap :userprops) (jsmap :orgprops))
                     jsmap
                     (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "deleteUser" (if (pcheck/user_checkprops (jsmap :userprops) (jsmap :orgprops))
                     jsmap
                     (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))
      "suspendUser" (if (pcheck/user_checkprops (jsmap :userprops) (jsmap :orgprops))
                      jsmap
                      (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))
      "activateUser" (if (pcheck/user_checkprops (jsmap :userprops) (jsmap :orgprops))
                       jsmap
                       (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "getAllUsersForOrg" (if (get-in jsmap [:orgprops :orgid])
                            jsmap
                            (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "getAllUsersForSite" (if (get-in jsmap [:siteprops :siteid])
                             jsmap
                             (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "getAllUsersForPartner" (if (get-in jsmap [:orgprops :orgid])
                                jsmap
                                (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "getAllSuspendedUsersForOrg" (if (get-in jsmap [:orgprops :orgid])
                                     jsmap
                                     (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "getAllSuspendedUsersForPartner" (if (get-in jsmap [:orgprops :orgid])
                                         jsmap
                                         (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "createOrg" (if (pcheck/org_checkprops (jsmap :orgprops))
                    jsmap (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "createSite" (pcheck/site_checkprops (jsmap :siteprops) (jsmap :orgprops))

      "createNode" (if (pcheck/node_checkprops (jsmap :nodeprops) (jsmap :siteprops) (jsmap :orgprops))
                     jsmap (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "assignNode" (if (pcheck/assign_node_checkprops (jsmap :nodeprops) (jsmap :siteprops) (jsmap :orgprops))
                     jsmap (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "assignNodeToParkingGroup" jsmap

      "loginReq" (if (pcheck/empty_node_checkprops (jsmap :nodeprops))
                   jsmap (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "createEmptyNode" (if (pcheck/empty_node_checkprops (jsmap :nodeprops))
                          jsmap (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "updateNode" (if (pcheck/update_node_checkprops (jsmap :nodeprops) (jsmap :siteprops) (jsmap :orgprops))
                     jsmap (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "activateNode" (if (get-in jsmap [:nodeprops :nodeid])
                       jsmap (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "deactivateNode" (if (get-in jsmap [:nodeprops :nodeid])
                         jsmap (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "commandNode" (if (get-in jsmap [:nodeprops :nodeid])
                      jsmap (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "createOverlay" (if (pcheck/overlay_checkprops (jsmap :overlayprops) (jsmap :siteprops))
                        jsmap (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "createGroup" (if (pcheck/group_checkprops (jsmap :groupprops) (jsmap :siteprops) (jsmap :orgprops))
                      jsmap (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "createFixture" (if (pcheck/fixture_checkprops (jsmap :fixtureprops) (jsmap :siteprops))
                        jsmap (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "createNotification" (if (pcheck/notification_checkprops (jsmap :notificationprops) (jsmap :siteprops))
                             jsmap (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "createAlert" (if (pcheck/alert_checkprops (jsmap :alertprops) (jsmap :siteprops))
                      jsmap (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "createParkingZone" (if (pcheck/parkingzone_checkprops (jsmap :parkingzoneprops) (jsmap :siteprops))
                            jsmap (throw (Exception. "checkprops: property check failed")))

      "createParkingSpot" (if (pcheck/parkingspot_checkprops (jsmap :parkingspotprops) (jsmap :siteprops))
                            jsmap (throw (Exception. "checkprops: property check failed")))

      "createParkingGroup" (if (pcheck/parkinggroup_checkprops (jsmap :parkinggroupprops) (jsmap :siteprops))
                             jsmap (throw (Exception. "checkprops: property check failed")))

      "lighting-control" (if (pcheck/lighting_ctl_checkprops (jsmap :nodeprops) (jsmap :siteprops) (jsmap :orgprops))
                           jsmap (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "lighting-control-site" (if (pcheck/lighting_site_ctl_checkprops (jsmap :nodeprops) (jsmap :siteprops) (jsmap :orgprops))
                                jsmap (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "lighting-control-group" (if (pcheck/lighting_group_ctl_checkprops (jsmap :nodeprops) (jsmap :siteprops) (jsmap :orgprops) (jsmap :groupprops))
                                 jsmap (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "getUserPermissions" jsmap

      "getAllPermissions" jsmap

      "updateOrg" (if (pcheck/org_checkprops (jsmap :orgprops))
                    jsmap (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "updateOverlay" (if (pcheck/overlay_checkprops (jsmap :overlayprops) (jsmap :siteprops))
                        jsmap (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "updateGroup" (if (pcheck/group_checkprops (jsmap :groupprops) (jsmap :siteprops) (jsmap :orgprops))
                      jsmap (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "updateFixture" (if (pcheck/fixture_checkprops (jsmap :fixtureprops) (jsmap :siteprops))
                        jsmap (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "deleteFixture" (if (and (get-in jsmap [:fixtureprops :fixtureid])
                               (get-in jsmap [:siteprops :siteid]))  ; siteid is needed here for logging to cassandra
                        jsmap
                        (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "deleteOrg" (if (get-in jsmap [:orgprops :orgid])
                    jsmap
                    (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "suspendOrg" (if (get-in jsmap [:orgprops :orgid])
                     jsmap
                     (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "activateOrg" (if (get-in jsmap [:orgprops :orgid])
                      jsmap
                      (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "deleteNode" (if (pcheck/node_checkprops_delete (jsmap :nodeprops) (jsmap :siteprops))
                     jsmap (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "realDeleteNode" (if (pcheck/node_checkprops_delete (jsmap :nodeprops) (jsmap :siteprops))
                         jsmap (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      ("addNodeToGroup"
       "addNodesToGroup"
       "resendScheduleToLG") (if (and (get-in jsmap [:groupprops :groupid])
                                      (get-in jsmap [:siteprops :siteid]))
                               jsmap
                               (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "removeNodeFromGroup" (if (and (get-in jsmap [:groupprops :groupid])
                                     (get-in jsmap [:siteprops :siteid]))
                              jsmap
                              (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "deleteOverlay" (if (and (get-in jsmap [:overlayprops :overlayid])
                               (get-in jsmap [:siteprops :siteid])) ; siteid is needed here for logging to cassandra
                        jsmap
                        (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "deleteGroup" (if (and (get-in jsmap [:groupprops :groupid])
                             (get-in jsmap [:siteprops :siteid])) ; siteid is needed here for logging to cassandra
                      jsmap
                      (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "updateNotification" (if (pcheck/notification_checkprops (jsmap :notificationprops) (jsmap :siteprops))
                             jsmap (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "activateNotification" (if (get-in jsmap [:notificationprops :notificationid])
                               jsmap (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "deactivateNotification" (if (get-in jsmap [:notificationprops :notificationid])
                                 jsmap (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "deleteNotification" (if (pcheck/notification_delete_checkprops (jsmap :notificationprops) (jsmap :siteprops))
                             jsmap (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "updateAlert" (if (pcheck/alert_checkprops (jsmap :alertprops) (jsmap :siteprops))
                      jsmap (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "updateParkingZone" (if (pcheck/parkingzone_checkprops (jsmap :parkingzoneprops) (jsmap :siteprops))
                            jsmap (throw (Exception. "checkprops: property check failed")))

      "updateParkingSpot" (if (pcheck/parkingspot_checkprops (jsmap :parkingspotprops) (jsmap :siteprops))
                            jsmap (throw (Exception. "checkprops: property check failed")))

      "updateParkingGroup" (if (pcheck/parkinggroup_checkprops (jsmap :parkinggroupprops) (jsmap :siteprops))
                             jsmap (throw (Exception. "checkprops: property check failed")))

      "deleteAlert" (if (pcheck/alert_checkprops (jsmap :alertprops) (jsmap :siteprops))
                      jsmap (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "dismissAlert" (if (pcheck/alert_checkprops (jsmap :alertprops) (jsmap :siteprops))
                      jsmap (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "deleteParkingZone" (if (pcheck/parkingzone_checkprops (jsmap :parkingzoneprops) (jsmap :siteprops))
                            jsmap (throw (Exception. "checkprops: property check failed")))

      "deleteParkingSpot" (if (pcheck/parkingspot_checkprops (jsmap :parkingspotprops) (jsmap :siteprops))
                            jsmap (throw (Exception. "checkprops: property check failed")))

      "deleteParkingGroup" (if (pcheck/parkinggroup_checkprops (jsmap :parkinggroupprops) (jsmap :siteprops))
                             jsmap (throw (Exception. "checkprops: property check failed")))

      "updateSite" (pcheck/site_checkprops (jsmap :siteprops) (jsmap :orgprops))

      "deleteSite" (if (pcheck/site_checkprops_delete (jsmap :siteprops) (jsmap :orgprops))
                     jsmap (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "suspendSite" (if (pcheck/site_checkprops_suspend (jsmap :siteprops) (jsmap :orgprops))
                      jsmap (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "activateSite" (if (pcheck/site_checkprops_activate (jsmap :siteprops) (jsmap :orgprops))
                       jsmap (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      ;; Install pod is generalization of pole for outdoor and install place for indoor
      "createInstallPod" (if (get-in jsmap [:podprops :siteid]) ; FIXME require siteid here
                           jsmap
                           (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))
      "deleteInstallPod" (if (get-in jsmap [:podprops :podid])  ; FIXME require siteid here
                           jsmap
                           (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "getAllFirmwares" jsmap

      "getFirmware" (if (get-in jsmap [:firmwareprops :firmwareid])
                      jsmap
                      (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "createFirmware" (if (get-in jsmap [:firmwareprops :firmwareid])
                         jsmap
                         (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "deleteFirmware" (if (get-in jsmap [:firmwareprops :firmwareid])
                         jsmap
                         (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "updateFirmware" (if (get-in jsmap [:firmwareprops :firmwareid])
                         jsmap
                         (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "assignFirmwareToNode" (if (get-in jsmap [:firmwareprops :firmwareid])
                               jsmap
                               (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "assignFirmwareToSite" (if (get-in jsmap [:firmwareprops :firmwareid])
                               jsmap
                               (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "assignFirmwareToGroup" (if (get-in jsmap [:firmwareprops :firmwareid])
                                jsmap
                                (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))


      "deletePDProfile" (if (pcheck/pdprofile_checkprops_get (jsmap :pdprofileprops) (jsmap :siteprops) (jsmap :orgprops))
                          jsmap (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "deleteETDHProfile" (if (pcheck/etdh_checkprops_get (jsmap :etdhprofileprops) (jsmap :siteprops) (jsmap :orgprops))
                          jsmap (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "deleteDHProfile" (if (pcheck/dh_checkprops_get (jsmap :dhprofileprops) (jsmap :siteprops) (jsmap :orgprops))
                          jsmap (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "deleteSchedule" (if (pcheck/schedule_checkprops_get (jsmap :scheduleprops) (jsmap :siteprops) (jsmap :orgprops))
                         jsmap (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "getPDProfile" (if (pcheck/pdprofile_checkprops_get (jsmap :pdprofileprops) (jsmap :siteprops) (jsmap :orgprops))
                       jsmap (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "getETDHProfile" (if (pcheck/etdh_checkprops_get (jsmap :etdhprofileprops) (jsmap :siteprops) (jsmap :orgprops))
                       jsmap (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "getDHProfile" (if (pcheck/dh_checkprops_get (jsmap :dhprofileprops) (jsmap :siteprops) (jsmap :orgprops))
                       jsmap (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "getSchedule" (if (pcheck/schedule_checkprops_get (jsmap :scheduleprops) (jsmap :siteprops) (jsmap :orgprops))
                      jsmap (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "createPDProfile" (if (pcheck/pdprofile_checkprops_create (jsmap :pdprofileprops) (jsmap :siteprops) (jsmap :orgprops))
                          jsmap (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "createETDHProfile" (if (pcheck/etdh_checkprops_create (jsmap :etdhprofileprops) (jsmap :siteprops) (jsmap :orgprops))
                          jsmap (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "createDHProfile" (if (pcheck/dh_checkprops_create (jsmap :dhprofileprops) (jsmap :siteprops) (jsmap :orgprops))
                          jsmap (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "createSchedule" (if (pcheck/schedule_checkprops_create (jsmap :scheduleprops) (jsmap :siteprops) (jsmap :orgprops))
                         jsmap (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "updatePDProfile" (if (pcheck/pdprofile_checkprops_create (jsmap :pdprofileprops) (jsmap :siteprops) (jsmap :orgprops))
                          jsmap (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "updateETDHProfile" (if (pcheck/etdh_checkprops_create (jsmap :etdhprofileprops) (jsmap :siteprops) (jsmap :orgprops))
                          jsmap (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "updateDHProfile" (if (pcheck/dh_checkprops_create (jsmap :dhprofileprops) (jsmap :siteprops) (jsmap :orgprops))
                          jsmap (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "updateSchedule" (if (pcheck/schedule_checkprops_create (jsmap :scheduleprops) (jsmap :siteprops) (jsmap :orgprops))
                         jsmap (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "getAllPDProfiles" jsmap

      "getAllETDHProfiles" jsmap

      "getAllDHProfiles" jsmap

      "getAllSchedules" jsmap

      "applyConfigToGroup" jsmap

      "applyConfigToSite" jsmap

      "applyConfigToNodes" (when-not (and (get-in jsmap [:orgprops :orgid])
                                          (get-in jsmap [:siteprops :siteid])
                                          (get-in jsmap [:configprops :configid])
                                          (get-in jsmap [:nodeprops :nodeids]))
                             (throw (ex-info "checkprops: property check failed"
                                             {:cause :bad-request
                                              :status 400})))

      "applyServerToNodes" jsmap

      "applyPDtoGroup" (if (pcheck/pd_checkprops_apply_group (jsmap :siteprops) (jsmap :orgprops) (jsmap :pdprofileprops) (jsmap :groupprops))
                         jsmap (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "applyPDtoSite" (if (pcheck/pd_checkprops_apply_site (jsmap :siteprops) (jsmap :orgprops) (jsmap :pdprofileprops))
                        jsmap (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "applyPDtoNodes" (if (pcheck/pd_checkprops_apply_nodes (jsmap :siteprops) (jsmap :orgprops) (jsmap :pdprofileprops) (jsmap :nodeprops))
                         jsmap (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "applyETDHtoGroup" (pcheck/etdh_checkprops_apply_group (jsmap :siteprops) (jsmap :orgprops) (jsmap :etdhprofileprops) (jsmap :groupprops))

      "applyDHtoGroup" (if (pcheck/dh_checkprops_apply_group (jsmap :siteprops) (jsmap :orgprops) (jsmap :dhprofileprops) (jsmap :groupprops))
                         jsmap (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      ("applyETDHtoSite"
       "getAllETDHProfileTriggers") (pcheck/etdh_checkprops_apply_site (jsmap :siteprops)
                                                                       (jsmap :orgprops)
                                                                       (jsmap :etdhprofileprops))

      ("applyDHtoSite"
       "getAllDHProfileTriggers"
       "calibrateDHProfile") (if (pcheck/dh_checkprops_apply_site (jsmap :siteprops) (jsmap :orgprops) (jsmap :dhprofileprops))
                               jsmap (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      ("applyETDHtoNodes"
       "removeETDHProfileTriggers"
       "addETDHProfileTriggers") (or (pcheck/etdh_checkprops_apply_nodes (jsmap :siteprops)
                                                                         (jsmap :orgprops)
                                                                         (jsmap :etdhprofileprops)
                                                                         (jsmap :nodeprops))
                                     (throw (ex-info "checkprops: property check failed"
                                                     {:cause :bad-request
                                                      :status 400})))

      ("applyDHtoNodes"
       "removeDHProfileTrigger"
       "addDHProfileTrigger") (if (pcheck/dh_checkprops_apply_nodes (jsmap :siteprops) (jsmap :orgprops) (jsmap :dhprofileprops) (jsmap :nodeprops))
                                jsmap (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "applyScheduleToGroup" (if (pcheck/schedule_checkprops_apply_group (jsmap :scheduleprops) (jsmap :siteprops) (jsmap :orgprops) (jsmap :groupprops))
                               jsmap (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "applyScheduleToSite" (if (pcheck/schedule_checkprops_get (jsmap :scheduleprops) (jsmap :siteprops) (jsmap :orgprops))
                              jsmap (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "applyScheduleToNode" (if (pcheck/schedule_checkprops_apply_node (jsmap :scheduleprops) (jsmap :siteprops) (jsmap :orgprops) (jsmap :nodeprops))
                              jsmap (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "applyScheduleToNodes" (if (pcheck/schedule_checkprops_apply_nodes (jsmap :scheduleprops) (jsmap :siteprops) (jsmap :orgprops) (jsmap :nodeprops))
                               jsmap (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "assignFixtureToNode" (if (and (get-in jsmap [:fixtureprops :fixtureid])
                                     (get-in jsmap [:nodeprops :nodeid]))
                              jsmap
                              (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))
      "assignFixtureToNodes" (if (and (get-in jsmap [:fixtureprops :fixtureid])
                                     (get-in jsmap [:nodeprops :nodeids])
                                      (get-in jsmap [:siteprops :siteid]))
                              jsmap
                              (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "assignFixtureToSite" (if (and (get-in jsmap [:fixtureprops :fixtureid])
                                     (get-in jsmap [:siteprops :siteid]))
                              jsmap
                              (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "assignFixtureToGroup" (if (and (get-in jsmap [:fixtureprops :fixtureid])
                                      (get-in jsmap [:groupprops :groupids]))
                               jsmap
                               (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "createBulkNode" (when-not (and (get-in jsmap [:nodeprops :nodes])
                                      ;; we need to assert that all
                                      ;; nodes have models.
                                      (every? (fn [{:keys [model]
                                                     :as node}]
                                                 model)
                                               (get-in jsmap [:nodeprops :nodes])))
                         (spy :info jsmap)
                         (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "getUserEmails" (if (get-in jsmap [:userprops :userids])
                           jsmap
                           (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))

      "bulkAssignUsersToParkingGroup" (if (and (get-in jsmap [:userIdToLinkprops :userIdsToLink])
                             (get-in jsmap [:parkingGroupprops :parkingGroupId])
                             (get-in jsmap [:orgprops :orgid])
                             (get-in jsmap [:siteprops :siteid]))
                      jsmap
                      (throw (ex-info "checkprops: property check failed for operation bulkAssignUsersToParkingGroup" {:cause :bad-request :status 400})))

      "bulkUnassignUsersFromParkingGroup" (if (and (get-in jsmap [:userIdToLinkprops :userIdsToLink])
                                               (get-in jsmap [:parkingGroupprops :parkingGroupId])
                                               (get-in jsmap [:orgprops :orgid])
                                               (get-in jsmap [:siteprops :siteid]))
                                        jsmap
                                        (throw (ex-info "checkprops: property check failed for operation bulkUnassignUsersFromParkingGroup" {:cause :bad-request :status 400})))

      ("policyAssociation"
       "policyDisassociation"
       )

      (if (and (get-in jsmap [:parkingGroupprops :parkingGroupId])
                                              (get-in jsmap [:orgprops :orgid])
                                              (get-in jsmap [:siteprops :siteid]))
                                       jsmap
                                       (throw (ex-info "checkprops: property check failed for parking operations" {:cause :bad-request :status 400})))


      ("createAppUserData"
      "deleteAppUserData"
      "getAllAppUserData"
      "getAppUserData"
      "updateAppUserData")

      (if (get-in jsmap [:userprops :userids])
        jsmap
        (throw (ex-info "checkprops: property check failed" {:cause :bad-request :status 400})))


      ("createMetadataForParkingSpot"
       "deleteMetadataForParkingSpot"
       "getAllMetadataForParkingSpot"
       "getMetadataForParkingSpot"
       "updateMetadataForParkingSpot"
       "createParkingPolicy"
       "deleteParkingPolicy"
       "getAllParkingPolicy"
       "getParkingPolicy"
       "updateParkingPolicy"
       "getParkingPolicyVersion"
       "getAllVersionsOfParkingPolicy"
       "getAllActiveParkingPolicyForPeriod"
       "getActiveParkingPolicy"
       "searchParkingPolicy"
       "createPolicyCategory"
       "deletePolicyCategory"
       "getAllPolicyCategory"
       "getPolicyCategory"
       "updatePolicyCategory"
       "associatedParkingGroups"
       "policyTagsAssociation"
       "policyTagsDisassociation"
       )

      (if (and (get-in jsmap [:orgprops :orgid])
               (get-in jsmap [:siteprops :siteid]))
                                       jsmap
                                       (throw (ex-info "checkprops: property check failed for parking operations" {:cause :bad-request :status 400})))

      (throw (ex-info (str "Action " (jsmap :type) " is not supported. Please check correctness of the 'type' parameter in your query in Interface Service" )
                      {:check "correctness of the 'type' parameter in your query"
                       :module-to-check "Interface Service"})))
    (catch clojure.lang.ExceptionInfo e
      (doto e
        error
        throw))
    (catch Exception e
      (doto e
        error
        throw)))
  jsmap)

;; The entry point to this program. This method provides all the glue code and workflow required to
;; divide, construct, route and execute queries and aggregate the result for the caller
(defn executer-main [jsmap]
  (trace "executer_main ...")
  ;; read the query content
  ;; Sample JSON Query:
  ;; {type: "createUNode", nodeProps: {nodeid: <nodeid>, name: <name>}, extProps: {}...}
  (let [{:keys [type]
         :as kwjsmap} (keywordize-keys jsmap)]
    (with-open [_ (timers/start (metric-factory :timer
                                                ["casel-timer"]))
                _ (timers/start (metric-factory :timer
                                                ["casel-timer" type]))]
      (try
        (trace (str "jsonmap: " jsmap))
        (debugf "TYPE: %s" type)
        (->> kwjsmap
             checkprops
             acl/actrl
             template-selector
             db-selector
             #_(spy :debug))
        (catch Throwable e
          (doto e
            warn
            throw))))))

;; The entry point to this program from device side. This is different from standard entry point
;; because we trust devices to send correct parameters and are always authorized (as opposed to users)
(defn executer-device [jsmap]
  (trace "executer_device ...")
  ;; read the query content
  ;; Sample JSON Query:
  ;; {type: "createUNode", nodeProps: {nodeid: <nodeid>, name: <name>}, extProps: {}...}
  (let [{:keys [type]
         :as kwjsmap} (keywordize-keys jsmap)]
    (with-open [_ (timers/start (metric-factory :timer
                                                ["casel-timer"]))
                _ (timers/start (metric-factory :timer
                                                ["casel-timer" type]))]
      (try
        (trace (str "jsonmap: " jsmap))
        (debugf "TYPE: %s" type)
        (->> kwjsmap
             checkprops
             template-selector
             db-selector
             #_(spy :debug))
        (catch Throwable e
          (doto e
            error
            throw))))))

(defn etdh-requests-consumer
  []
  (async/pipeline-blocking 1
                  etdh/casel-responses
                  (map #(try
                          (executer-main %)
                          (catch Throwable t
                            t)))
                  etdh/casel-requests))

(defrecord Cape [metrics-registry]
  component/Lifecycle
  (start [component]
    (etdh-requests-consumer)
    (let [{:keys [registry
                  prefix]} metrics-registry]
      (gauges/gauge-fn registry
                       (conj prefix "cape-thread-pool" "utilization")
                       #(let [length (.size (.getQueue cape-thread-pool))]
                          (debugf "Pool Length %s" length)
                          length))

      (alter-var-root #'metric-factory
                      (fn [& _]
                        (dealer.metrics/metric-factory-factory "utils.cape"
                                                               registry
                                                               prefix)))))
  (stop [component]
    (async/close! etdh/casel-requests)
    component))

(defn new-cape
  []
  (map->Cape {}))
