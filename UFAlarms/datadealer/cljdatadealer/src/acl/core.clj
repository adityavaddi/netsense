(ns ^{:author "Chiradip Mandal"
      :doc "The ACL Core."}
    acl.core
  (:gen-class)
  (:require [clojure.data.json :as json]
            [clojure.tools.logging :refer :all]
            [clojure.core :refer :all]
            [clostache.parser :refer :all]
            [neowrap.neowrapper :as neo4j]))

(def not-nil? (complement nil?))

(declare acl_create_unode)

(def assign_firmware_acl_cypher "MATCH (u:User {userid:\"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_ASSIGN_TO_SITES]->(:NodeModel) return r") ;; TODO: Do a proper access control

(def get_firmware_acl_cypher "MATCH (u:User {userid:\"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_READ]->(:FirmwareModel) return r")


(def get_org_user_acl_cypher "MATCH (o:Org {orgid: \"{{orgid}}\"})-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid:\"{{userid}}\"})-
  [:IS]->(role:Role)-[r:CAN_READ_ORG_USER]->(:UserModel) return r")

(def create_org_user_acl_cypher "MATCH (o:Org {orgid: \"{{orgid}}\"})-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-
  [:IS]->(role:Role)-[r:CAN_CREATE_ORG_USER]->(:UserModel) return r")

(def get_user_rolename_cypher "MATCH (u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r]->(:UserModel) return distinct role.rolename as rolename")

(def get_user_createlist_cypher "MATCH (role:Role {rolename:\"{{role}}\"})-[r:CAN_CREATE]->(role:Role {rolename: \"{{target_role}}\"}) return r")

; This is a bit of a hack, but needed to avoid fetching a list of orgids due to orphaned orgs when we delete them that now look like the root sensity org.
; This works as ID is auto-incrementing, and will always be higher integer than Sensity Systems ID value.  See NSN-13180 for result of this problem.
(def sensity_org_id "MATCH (o:Org) WHERE NOT (o)-[:IS_CUSTOMER_OF]->(:Org) RETURN o.orgid AS orgid ORDER BY ID(o) LIMIT 1")

(def get_user_deletelist_cypher "MATCH (role:Role {rolename:\"{{role}}\"})-[r:CAN_DELETE]->(role:Role {rolename: \"{{target_role}}\"}) return r")

(def get_user_updatelist_cypher "MATCH (role:Role {rolename:\"{{role}}\"})-[r:CAN_CHANGE]->(role:Role {rolename: \"{{target_role}}\"}) return r")

(def delete_org_user_acl_cypher "MATCH (o:Org {orgid: \"{{orgid}}\"})-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid:\"{{userid}}\"})-
  [:IS]->(role:Role)-[r:CAN_SUSPEND_ORG_USER]->(:UserModel) return r")

(def update_org_user_acl_cypher "MATCH (o:Org {orgid: \"{{orgid}}\"})-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-
  [:IS]->(role:Role)-[r:CAN_CHANGE_ORG_USER]->(:UserModel) return r")


(def get_partner_user_acl_cypher "MATCH (o:Org {orgid: \"{{orgid}}\"})-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid:\"{{userid}}\"})-
  [:IS]->(role:Role)-[r:CAN_READ_ORG_USER]->(:UserModel) return r")

(def create_partner_user_acl_cypher "MATCH (o:Org {orgid: \"{{orgid}}\"})-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-
  [:IS]->(role:Role)-[r:CAN_CREATE_ORG_USER]->(:UserModel) return r")

(def delete_partner_user_acl_cypher "MATCH (o:Org {orgid: \"{{orgid}}\"})-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid:\"{{userid}}\"})-
  [:IS]->(role:Role)-[r:CAN_SUSPEND_ORG_USER]->(:UserModel) return r")

(def update_partner_user_acl_cypher "MATCH (o:Org {orgid: \"{{orgid}}\"})-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-
  [:IS]->(role:Role)-[r:CAN_CHANGE_ORG_USER]->(:UserModel) return r")

(def get_users4org_acl_cypher "MATCH p = (um:UserModel)<-[:CAN_READ_ORG_USER]-(:Role)<-[:IS]-(u:User {userid: \"{{userid}}\"})
  -[:IS_USER_OF]->(:Org)-[:HAS_CUSTOMER*0..4]->(:Org {orgid: \"{{orgid}}\"}) return distinct um")

(def get_users4site_acl_cypher "MATCH p = (um:UserModel)<-[:CAN_READ_SITE_USER]-(:Role)<-[:IS]-(u:User {userid: \"{{userid}}\"})
  -[:IS_USER_OF]->(:Org)-[:HAS_CUSTOMER*0..4]->(:Org)<-[:BELONGS_TO]-(:Site {siteid: \"{{siteid}}\"}) return distinct um")

(def get_users4partner_acl_cypher "MATCH (o:Org {orgid: \"{{orgid}}\"})-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid:\"{{userid}}\"})-
  [:IS]->(role:Role)-[r:CAN_READ_ORG_USER]->(:UserModel) return r")


(def get_suspended_users4org_acl_cypher "MATCH p = (um:UserModel)<-[:CAN_SUSPEND_ORG_USER]-(:Role)<-[:IS]-(u:User {userid: \"{{userid}}\"})
  -[:IS_USER_OF]->(:Org)-[:HAS_CUSTOMER*0..4]->(:Org {orgid: \"{{orgid}}\"}) return distinct um")

(def get_suspended_users4partner_acl_cypher "MATCH (o:Org {orgid: \"{{orgid}}\"})-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid:\"{{userid}}\"})-
  [:IS]->(role:Role)-[r:CAN_SUSPEND_PARTNER_USER]->(:UserModel) return r")

(def get_activity_acl_cypher "MATCH (o:Org {orgid: \"{{orgid}}\"})-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid:\"{{userid}}\"})-
  [:IS]->(role:Role)-[r:CAN_READ]->(:AuditModel) return r")

(def get_ota_status_acl_cypher "MATCH (o:Org {orgid: \"{{orgid}}\"})-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid:\"{{userid}}\"})-
  [:IS]->(role:Role)-[r:CAN_READ]->(:FirmwareModel) return r")

(def get_org_acl_cypher "MATCH(o:Org {orgid: \"{{orgid}}\"})-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid:\"{{userid}}\"})-
  [:IS]->(role:Role)-[r:CAN_READ]->(:OrgModel) return r")

(def create_org_acl_cypher "MATCH(o:Org {orgid: \"{{orgid}}\"})-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid:\"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_CREATE]->(:OrgModel),(lo:Org {orgid:\"{{orgid}}\"}) WHERE NOT (lo)-[:IS_CUSTOMER_OF*2]->(:Org) return r")

(def update_org_acl_cypher "MATCH(o:Org {orgid: \"{{orgid}}\"})-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid:\"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_CHANGE]->(:OrgModel) return r")

(def delete_org_acl_cypher "MATCH(o:Org {orgid: \"{{orgid}}\"})-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid:\"{{userid}}\"})-
  [:IS]->(role:Role)-[r:CAN_DELETE]->(:OrgModel) return r")

(def suspend_org_acl_cypher "MATCH(o:Org {orgid: \"{{orgid}}\"})-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid:\"{{userid}}\"})-
  [:IS]->(role:Role)-[r:CAN_SUSPEND]->(:OrgModel) return r")

(def activate_org_acl_cypher "MATCH(o:Org {orgid: \"{{orgid}}\"})-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid:\"{{userid}}\"})-
  [:IS]->(role:Role)-[r:CAN_SUSPEND]->(:OrgModel) return r")

(def get_partner_acl_cypher "MATCH(o:Org {orgid: \"{{orgid}}\"})-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid:\"{{userid}}\"})-
  [:IS]->(role:Role)-[r:CAN_READ]->(:PartnerModel) return r")

(def create_partner_acl_cypher "MATCH (u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_CREATE]->(:PartnerModel) return r")

(def update_partner_acl_cypher "MATCH (u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_UPDATE]->(:PartnerModel) return r")

(def delete_partner_acl_cypher "MATCH(o:Org {orgid: \"{{orgid}}\"})-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid:\"{{userid}}\"})-
  [:IS]->(role:Role)-[r:CAN_DELETE]->(:PartnerModel) return r")

(def delete_org_partner_acl_cypher "MATCH(o:Org {orgid: \"{{orgid}}\"})-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid:\"{{userid}}\"})-
  [:IS]->(role:Role)-[r:CAN_DELETE]->(m) WHERE (m:OrgModel AND o.type IS NULL) OR (m:OrgModel AND o.type<>\"partner\") OR (m:PartnerModel AND o.type=\"partner\") return r")

(def suspend_org_partner_acl_cypher "MATCH(o:Org {orgid: \"{{orgid}}\"})-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid:\"{{userid}}\"})-
  [:IS]->(role:Role)-[r:CAN_SUSPEND]->(m) WHERE (m:OrgModel AND o.type IS NULL) OR (m:OrgModel AND o.type<>\"partner\") OR (m:PartnerModel AND o.type=\"partner\") return r")

(def suspend_partner_acl_cypher "MATCH(o:Org {orgid: \"{{orgid}}\"})-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid:\"{{userid}}\"})-
  [:IS]->(role:Role)-[r:CAN_SUSPEND]->(:PartnerModel) return r")

(def activate_partner_acl_cypher "MATCH(o:Org {orgid: \"{{orgid}}\"})-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid:\"{{userid}}\"})-
  [:IS]->(role:Role)-[r:CAN_SUSPEND]->(:PartnerModel) return r")

(def get_site_acl_cypher "MATCH(s:Site {siteid: \"{{siteid}}\"})-[:BELONGS_TO]->(o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->
  (u:User {userid:\"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_READ]->(:SiteModel) return r")

(def get_node_acl_cypher "MATCH(n:Node {nodeid: \"{{nodeid}}\"})-[:BELONGS_TO]->(s:Site)-[:BELONGS_TO]->(o:Org)-[:IS_CUSTOMER_OF*0..4]->(:Org)-
  [:HAS_USER]->(u:User {userid:\"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_READ]->(:NodeModel) return r")

(def get_sensor_acl_cypher "MATCH(n:Node {nodeid: \"{{nodeid}}\"})-[:BELONGS_TO]->(s:Site {siteid: \"{{siteid}}\"})-[:BELONGS_TO]->(o:Org {orgid: \"{{orgid}}\"})-[:IS_CUSTOMER_OF*0..4]->(:Org)-
  [:HAS_USER]->(u:User {userid:\"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_READ]->(:NodeModel) return r")

(def get_sensor_site_acl_cypher "MATCH(s:Site {siteid: \"{{siteid}}\"})-[:BELONGS_TO]->(o:Org {orgid: \"{{orgid}}\"})-[:IS_CUSTOMER_OF*0..4]->(:Org)-
  [:HAS_USER]->(u:User {userid:\"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_READ]->(:NodeModel) return r")

(def get_overlay_acl_cypher "MATCH (ol:Overlay {overlayid: \"{{overlayid}}\"})-[:BELONGS_TO]->(s:Site)-[:BELONGS_TO]->
  (o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid:\"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_READ]->(:OverlayModel) return r")

(def get_group_acl_cypher "MATCH (g:Group {groupid: \"{{groupid}}\"})-[:BELONGS_TO]->(s:Site)-[:BELONGS_TO]->
  (o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid:\"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_READ]->(:GroupModel) return r")

(def get_fixture_acl_cypher "MATCH (f:Fixture {fixtureid: \"{{fixtureid}}\"})-[:BELONGS_TO]->(s:Site)-[:BELONGS_TO]->
  (o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid:\"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_READ]->(:FixtureModel) return r")

(def delete_notification_acl_cypher "MATCH (s:Site {siteid: \"{{siteid}}\"})-[:BELONGS_TO]->(o:Org {orgid: \"{{orgid}}\"})-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid:\"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_DELETE]->(:NotificationModel) return r")

(def get_notification_acl_cypher "MATCH (s:Site {siteid: \"{{siteid}}\"})-[:BELONGS_TO]->(o:Org {orgid: \"{{orgid}}\"})-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid:\"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_READ]->(:NotificationModel) return r")

(def delete_alert_acl_cypher "MATCH (s:Site {siteid: \"{{siteid}}\"})-[:BELONGS_TO]->(o:Org {orgid: \"{{orgid}}\"})-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid:\"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_DELETE]->(:AlertModel) return r")

(def delete_parkingzone_acl_cypher "MATCH (n:ParkingZone {parkingzoneid: \"{{parkingzoneid}}\"})-[:BELONGS_TO]->(g:ParkingGroup)-[:BELONGS_TO]->(s:Site)-[:BELONGS_TO]->
  (o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid:\"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_DELETE]->(:ParkingZoneModel) return r")

(def delete_parkinggroup_acl_cypher "MATCH (n:ParkingGroup {parkinggroupid: \"{{parkinggroupid}}\"})-[:BELONGS_TO]->(s:Site)-[:BELONGS_TO]->
  (o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid:\"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_DELETE]->(:ParkingZoneModel) return r")

(def get_alert_acl_cypher "MATCH (s:Site {siteid: \"{{siteid}}\"})-[:BELONGS_TO]->(o:Org {orgid: \"{{orgid}}\"})-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid:\"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_READ]->(:AlertModel) return r")

(def get_parkingzone_acl_cypher "MATCH (s:Site {siteid: \"{{siteid}}\"})-[:BELONGS_TO]->
  (o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid:\"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_READ]->(:ParkingZoneModel) return r")

(def get_parkinggroup_acl_cypher "MATCH (n:ParkingGroup {parkinggroupid: \"{{parkinggroupid}}\"})-[:BELONGS_TO]->(s:Site)-[:BELONGS_TO]->
  (o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid:\"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_READ]->(:ParkingZoneModel) return r")

(def get_parkinginfo_acl_cypher "MATCH (s:Site {siteid: \"{{siteid}}\"})-[:BELONGS_TO]->
  (o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid:\"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_READ]->(:ParkingZoneModel) return r")

(def get_trafficinfo_acl_cypher "MATCH (s:Site {siteid: \"{{siteid}}\"})-[:BELONGS_TO]->
  (o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid:\"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_READ]->(:TrafficObjectModel) return r")

(def delete_overlay_acl_cypher "MATCH (ol:Overlay {overlayid: \"{{overlayid}}\"})-[:BELONGS_TO]->(s:Site)-[:BELONGS_TO]->
  (o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_DELETE]->(:OverlayModel) return r")

(def get_all_overlays_acl_cypher "MATCH (s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->(:Org)-[:IS_CUSTOMER_OF*0..4]->(:Org)-[:HAS_USER]->
  (u:User {userid:\"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_READ]->(:OverlayModel) return r")

(def get_all_groups_acl_cypher "MATCH (s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->(:Org)-[:IS_CUSTOMER_OF*0..4]->(:Org)-[:HAS_USER]->
  (u:User {userid:\"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_READ]->(:GroupModel) return r")

(def get_all_schedules_acl_cypher "MATCH (s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->(:Org)-[:IS_CUSTOMER_OF*0..4]->(:Org)-[:HAS_USER]->
  (u:User {userid:\"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_READ]->(:ScheduleModel) return r")

(def get_all_fixtures_acl_cypher "MATCH (s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->(:Org)-[:IS_CUSTOMER_OF*0..4]->(:Org)-[:HAS_USER]->
  (u:User {userid:\"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_READ]->(:FixtureModel) return r")

(def get_all_notifications_for_site_acl_cypher "MATCH (s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->(:Org {orgid: \"{{orgid}}\"})-[:IS_CUSTOMER_OF*0..4]->(:Org)-[:HAS_USER]->
  (u:User {userid:\"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_READ]->(:NotificationModel) return r")

(def get_all_notifications_for_user_acl_cypher "MATCH (u:User {userid:\"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_READ]->(:NotificationModel) return r")

(def get_all_notifications_for_org_acl_cypher "MATCH (:Org {orgid:\"{{orgid}}\"})-[:IS_CUSTOMER_OF*0..4]->(:Org)-[:HAS_USER]->(u:User {userid:\"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_READ]->(:NotificationModel) return r")

(def get_all_alerts_acl_cypher "MATCH (s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->(:Org {orgid: \"{{orgid}}\"})-[:IS_CUSTOMER_OF*0..4]->(:Org)-[:HAS_USER]->
  (u:User {userid:\"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_READ]->(:AlertModel) return r")

(def get_all_alerts_for_node_acl_cypher "MATCH (n:Node {nodeid: \"{{nodeid}}\"})-[:BELONGS_TO]->(s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->(:Org {orgid: \"{{orgid}}\"})-[:IS_CUSTOMER_OF*0..4]->(:Org)-[:HAS_USER]->
  (u:User {userid:\"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_READ]->(:AlertModel) return r")

(def get_all_org_alerts_acl_cypher "MATCH (s:Org {orgid:\"{{orgid}}\"})-[:IS_CUSTOMER_OF*0..4]->(:Org)-[:HAS_USER]->
  (u:User {userid:\"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_READ]->(:AlertModel) return r")

(def get_all_parkingzones_acl_cypher "MATCH (s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->(:Org)-[:IS_CUSTOMER_OF*0..4]->(:Org)-[:HAS_USER]->
  (u:User {userid:\"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_READ]->(:ParkingZoneModel) return r")

(def get_all_parkinggroups_acl_cypher "MATCH (s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->(:Org)-[:IS_CUSTOMER_OF*0..4]->(:Org)-[:HAS_USER]->
  (u:User {userid:\"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_READ]->(:ParkingZoneModel) return r")

(def update_node_acl_cypher "MATCH (n:Node {nodeid: \"{{nodeid}}\"})-[:BELONGS_TO]->(s:Site)-[:BELONGS_TO]->
  (o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_CHANGE]->(:NodeModel) return r")

(def activate_node_acl_cypher "MATCH (n:Node {nodeid: \"{{nodeid}}\"})-[:BELONGS_TO]->(s:Site)-[:BELONGS_TO]->
  (o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_DEACTIVATE]->(:NodeModel) return r")

(def deactivate_node_acl_cypher "MATCH (n:Node {nodeid: \"{{nodeid}}\"})-[:BELONGS_TO]->(s:Site)-[:BELONGS_TO]->
  (o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_DEACTIVATE]->(:NodeModel) return r")

(def create_node_acl_cypher "MATCH (s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->
  (o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_CREATE]->(:NodeModel) return r")

(def assign_node_acl_cypher "MATCH (s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->
  (o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org {orgid:\"{{soid}}\"})-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_CREATE]->(:NodeModel) return r")

(def create_empty_node_acl_cypher "MATCH (u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_CREATE]->(:NodeModel) return r")

(def create_overlay_acl_cypher "MATCH (s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->
  (o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_CREATE]->(:OverlayModel) return r")

(def create_group_acl_cypher "MATCH (s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->
  (o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_CREATE]->(:GroupModel) return r")

(def create_fixture_acl_cypher "MATCH (s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->
  (o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_CREATE]->(:FixtureModel) return r")

(def create_notification_acl_cypher "MATCH (s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->
  (o:Org {orgid: \"{{orgid}}\"})-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_CREATE]->(:NotificationModel) return r")

(def create_alert_acl_cypher "MATCH (s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->
  (o:Org {orgid: \"{{orgid}}\"})-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_CREATE]->(:AlertModel) return r")

(def create_parkingzone_acl_cypher "MATCH (s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->
  (o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_CREATE]->(:ParkingZoneModel) return r")

(def assign_parkinggroup_acl_cypher "MATCH (pg:ParkingGroup {parkinggroupid:\"{{parkinggroupid}}\"})-[:BELONGS_TO]->(s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->
  (o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_CREATE]->(:ParkingZoneModel) return r")

(def create_site_acl_cypher "MATCH (o:Org {orgid: \"{{orgid}}\"})-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_CREATE]->(:SiteModel) return r")

(def update_overlay_acl_cypher "MATCH (ol:Overlay {overlayid: \"{{overlayid}}\"})-[:BELONGS_TO]->(s:Site)-[:BELONGS_TO]->
  (o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_UPDATE]->(:OverlayModel) return r")

(def update_group_acl_cypher "MATCH (g:Group {groupid: \"{{groupid}}\"})-[:BELONGS_TO]->(s:Site)-[:BELONGS_TO]->
  (o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_UPDATE]->(:GroupModel) return r")

(def update_fixture_acl_cypher "MATCH (f:Fixture {fixtureid: \"{{fixtureid}}\"})-[:BELONGS_TO]->(s:Site)-[:BELONGS_TO]->
  (o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_UPDATE]->(:FixtureModel) return r")

(def update_notification_acl_cypher "MATCH (s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->(o:Org {orgid: \"{{orgid}}\"})-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_UPDATE]->(:NotificationModel) return r")

(def activate_notification_acl_cypher "MATCH (s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->(o:Org {orgid: \"{{orgid}}\"})-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_ACTIVATE]->(:NotificationModel) return r")

(def deactivate_notification_acl_cypher "MATCH (s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->(o:Org {orgid: \"{{orgid}}\"})-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_DEACTIVATE]->(:NotificationModel) return r")

(def update_alert_acl_cypher "MATCH (s:Site {siteid: \"{{siteid}}\"})-[:BELONGS_TO]->(o:Org {orgid: \"{{orgid}}\"})-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_UPDATE]->(:AlertModel) return r")

(def update_parkingzone_acl_cypher "MATCH (n:ParkingZone {parkingzoneid: \"{{parkingzoneid}}\"})-[:BELONGS_TO]->(g:ParkingGroup)-[:BELONGS_TO]->(s:Site)-[:BELONGS_TO]->
  (o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_UPDATE]->(:ParkingZoneModel) return r")

(def update_parkinggroup_acl_cypher "MATCH (n:ParkingGroup {parkinggroupid: \"{{parkinggroupid}}\"})-[:BELONGS_TO]->(s:Site)-[:BELONGS_TO]->
  (o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_UPDATE]->(:ParkingZoneModel) return r")

(def update_site_acl_cypher "MATCH(s:Site {siteid: \"{{siteid}}\"})-[:BELONGS_TO]->
  (o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_CHANGE]->(:SiteModel) return r")

(def get_sites4org_acl_cypher "MATCH p = (sm:SiteModel)<-[:CAN_READ]-(:Role)<-[:IS]-(u:User {userid: \"{{userid}}\"})
  -[:IS_USER_OF]->(:Org)-[:HAS_CUSTOMER*0..4]->(:Org {orgid: \"{{orgid}}\"}) return distinct sm")

(def get_suspendedsites4org_acl_cypher "MATCH p = (sm:SiteModel)<-[:CAN_READ]-(:Role)<-[:IS]-(u:User {userid: \"{{userid}}\"})
  -[:IS_USER_OF]->(:Org)-[:HAS_CUSTOMER*0..4]->(:Org {orgid: \"{{orgid}}\"}) return distinct sm")

(def get_nodes4site_acl_cypher "MATCH p = (nm:NodeModel)<-[:CAN_READ]-(:Role)<-[:IS]-(u:User {userid: \"{{userid}}\"})
  -[:IS_USER_OF]->(:Org)-[:HAS_CUSTOMER*0..4]->(:Org)<-[:BELONGS_TO]-(:Site {siteid: \"{{siteid}}\"}) return distinct nm")

(def get_lost_found_nodes_acl_cypher "MATCH p = (nm:NodeModel)<-[:CAN_READ]-(:Role)<-[:IS]-(u:User {userid: \"{{userid}}\"}) return distinct nm")

(def delete_group_acl_cypher "MATCH (g:Group {groupid: \"{{groupid}}\"})-[:BELONGS_TO]->(s:Site)-[:BELONGS_TO]->
  (o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_DELETE]->(:GroupModel) return r")

(def delete_fixture_acl_cypher "MATCH (f:Fixture {fixtureid: \"{{fixtureid}}\"})-[:BELONGS_TO]->(s:Site)-[:BELONGS_TO]->
  (o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_DELETE]->(:FixtureModel) return r")

(def delete_node_acl_cypher "MATCH(n:Node {nodeid: \"{{nodeid}}\"})-[:BELONGS_TO]->(s:Site {siteid: \"{{siteid}}\"})-[:BELONGS_TO]->
  (o:Org {orgid: \"{{orgid}}\"})-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_DEACTIVATE]->(:NodeModel) return r")

(def delete_node_really_acl_cypher "MATCH(n:Node {nodeid: \"{{nodeid}}\"})-[:BELONGS_TO]->(s:Site {siteid: \"{{siteid}}\"})-[:BELONGS_TO]->
  (o:Org {orgid: \"{{orgid}}\"})-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_DELETE]->(:NodeModel) return r")

(def get_usermodel_permissions_for_user_acl_cypher "MATCH p = (u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)
  -[:CAN_READ_USER]->(:UserModel) RETURN p")

(def create_install_pod_acl_cypher "MATCH p = (s:Site {siteid: \"{{siteid}}\"})-[:BELONGS_TO]->(o:Org)<-[r:IS_USER_OF]-
  (u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)
  -[:CAN_INSTALL_POD]->(:PodModel)
  return p")

(def delete_install_pod_acl_cypher "MATCH p = (p:InstallPod {podid: \"{{podid}}\"})-[:BELONGS_TO]->(s:Site)-[:BELONGS_TO]->(o:Org)<-[r:IS_USER_OF]-
  (u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)
  -[:CAN_DELETE_POD]->(:PodModel)
  return p")

(def delete_site_acl_cypher "MATCH(s:Site {siteid: \"{{siteid}}\"})-[:BELONGS_TO]->
  (o:Org {orgid: \"{{orgid}}\"})-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid:\"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_DELETE]->(:SiteModel) return r")

(def suspend_site_acl_cypher "MATCH(s:Site {siteid: \"{{siteid}}\"})-[:BELONGS_TO]->
  (o:Org {orgid: \"{{orgid}}\"})-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid:\"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_SUSPEND]->(:SiteModel) return r")

(def delete_firmware_acl_cypher "MATCH (u:User {userid:\"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_DELETE]->(:FirmwareModel) return r")

(def create_firmware_acl_cypher "MATCH (u:User {userid:\"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_CREATE]->(:FirmwareModel) return r")

(def update_firmware_acl_cypher "MATCH (u:User {userid:\"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_UPDATE]->(:FirmwareModel) return r")

(def get_firmware_acl_cypher "MATCH (u:User {userid:\"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_READ]->(:FirmwareModel) return r")

(def get_edth_trigger_acl_cypher "MATCH (profile:ETDHProfile {etdhprofileid: \"{{etdhprofileid}}\"})-[:BELONGS_TO]->(s:Site {siteid: \"{{siteid}}\"})-[:BELONGS_TO]->
  (o:Org {orgid: \"{{orgid}}\"})-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_READ]->(:ScheduleModel) return r")

(def delete_edth_trigger_acl_cypher "MATCH (profile:ETDHProfile {etdhprofileid: \"{{etdhprofileid}}\"})-[:BELONGS_TO]->(s:Site {siteid: \"{{siteid}}\"})-[:BELONGS_TO]->
  (o:Org {orgid: \"{{orgid}}\"})-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_DELETE]->(:ScheduleModel) return r")

(def update_edth_trigger_acl_cypher "MATCH (profile:ETDHProfile {etdhprofileid: \"{{etdhprofileid}}\"})-[:BELONGS_TO]->(s:Site {siteid: \"{{siteid}}\"})-[:BELONGS_TO]->
  (o:Org {orgid: \"{{orgid}}\"})-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_UPDATE]->(:ScheduleModel) return r")

(def update_schedule_acl_cypher "MATCH (sch:Schedule {scheduleid: \"{{scheduleid}}\"})-[:BELONGS_TO]->(s:Site)-[:BELONGS_TO]->
  (o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_UPDATE]->(:ScheduleModel) return r")

(def get_schedule_acl_cypher "MATCH (sch:Schedule {scheduleid: \"{{scheduleid}}\"})-[:BELONGS_TO]->(s:Site {siteid: \"{{siteid}}\"})-[:BELONGS_TO]->
  (o:Org {orgid: \"{{orgid}}\"})-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_READ]->(:ScheduleModel) return r")

(def delete_schedule_acl_cypher "MATCH (sch:Schedule {scheduleid: \"{{scheduleid}}\"})-[:BELONGS_TO]->(s:Site)-[:BELONGS_TO]->
  (o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_DELETE]->(:ScheduleModel) return r")

(def create_schedule_acl_cypher "MATCH (s:Site {siteid: \"{{siteid}}\"})-[:BELONGS_TO]->
  (o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_CREATE]->(:ScheduleModel) return r")

(def apply_schedule_acl_cypher "MATCH (sc:Schedule {scheduleid: \"{{scheduleid}}\"})-[:BELONGS_TO]->(s:Site {siteid: \"{{siteid}}\"})-[:BELONGS_TO]->
  (o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_APPLY]->(:ScheduleModel) return r")

(def apply_etdh_acl_cypher "MATCH (etdh:ETDHProfile {etdhprofileid: {props}.etdhprofileid})-[:BELONGS_TO]->(s:Site {siteid: {props}.siteid})-[:BELONGS_TO]->
  (o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: {props}.userid})-[:IS]->(role:Role)-[r:CAN_APPLY]->(:ScheduleModel) return r")

(def apply_dh_acl_cypher "MATCH (dh:DHProfile {dhprofileid: {props}.dhprofileid})-[:BELONGS_TO]->(s:Site {siteid: {props}.siteid})-[:BELONGS_TO]->
  (o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: {props}.userid})-[:IS]->(role:Role)-[r:CAN_APPLY]->(:ScheduleModel) return r")

(def apply_etdh_group_acl_cypher "MATCH (etdh:ETDHProfile {etdhprofileid: {props}.etdhprofileid})-[:BELONGS_TO]->(s:Site {siteid: {props}.siteid})-[:BELONGS_TO]->
  (o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: {props}.userid})-[:IS]->(role:Role)-[r:CAN_APPLY]->(:ScheduleModel),(g)-[:BELONGS_TO]->(s) WHERE g.groupid IN {props}.groupids return r")

(def apply_dh_group_acl_cypher "MATCH (dh:DHProfile {dhprofileid: {props}.dhprofileid})-[:BELONGS_TO]->(s:Site {siteid: {props}.siteid})-[:BELONGS_TO]->
  (o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: {props}.userid})-[:IS]->(role:Role)-[r:CAN_APPLY]->(:ScheduleModel),(g)-[:BELONGS_TO]->(s) WHERE g.groupid IN {props}.groupids return r")

(def apply_pdprofile_acl_cypher "MATCH (p {pdprofileid: \"{{pdprofileid}}\"})-[:BELONGS_TO]->(s:Site {siteid: \"{{siteid}}\"})-[:BELONGS_TO]->
  (o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_APPLY]->(:ScheduleModel) return r")

(def assign_fixture_acl_cypher "MATCH (s:Site {siteid: \"{{siteid}}\"})-[:BELONGS_TO]->
  (o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_ASSIGN_FIXTURE]->(:NodeModel) return r")

(def can_manage_light_control
  "MATCH (s:Site {siteid: \"{{siteid}}\"})-[:BELONGS_TO]->
  (o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_MANAGE_LIGHTS]->(:NodeModel) return r")

(def update_config_acl_cypher "MATCH (s:Site {siteid: \"{{siteid}}\"})-[:BELONGS_TO]->
  (o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_UPDATE]->(:ConfigModel) return r")

(def get_config_acl_cypher "MATCH (s:Site {siteid: \"{{siteid}}\"})-[:BELONGS_TO]->
  (o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_READ]->(:ConfigModel) return r")

(def get_default_config_acl_cypher "MATCH (s:Site {siteid: \"{{siteid}}\"})-[:BELONGS_TO]->
  (o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_READ]->(:ConfigModel) return r")

(def delete_config_acl_cypher "MATCH (s:Site {siteid: \"{{siteid}}\"})-[:BELONGS_TO]->
  (o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_DEACTIVATE]->(:ConfigModel) return r")

(def create_config_acl_cypher "MATCH (s:Site {siteid: \"{{siteid}}\"})-[:BELONGS_TO]->
  (o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_CREATE]->(:ConfigModel) return r")

(def apply_config_acl_cypher "MATCH (s:Site {siteid: \"{{siteid}}\"})-[:BELONGS_TO]->
  (o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_APPLY]->(:ConfigModel) return r")

(def assign_users_to_parkinggroup_acl_cypher "MATCH (pg:ParkingGroup {parkinggroupid:\"{{parkinggroupid}}\"})-[:BELONGS_TO]->(s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->
  (o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_CREATE]->(:ParkingGroupModel) return r")

(def unassign_users_from_parkinggroup_acl_cypher "MATCH (pg:ParkingGroup {parkinggroupid:\"{{parkinggroupid}}\"})-[:BELONGS_TO]->(s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->
  (o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_UPDATE]->(:ParkingGroupModel) return r")

(def create_metadata_for_parkingspot_acl_cypher "MATCH (s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->(o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_CREATE]->(:ParkingSpotModel) return r")

(def delete_metadata_for_parkingspot_acl_cypher "MATCH (s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->(o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_DELETE]->(:ParkingSpotModel) return r")

(def get_all_metadata_for_parkingspot_acl_cypher "MATCH (s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->(o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_READ]->(:ParkingSpotModel) return r")

(def get_metadata_for_parkingspot_acl_cypher "MATCH (s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->(o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_READ]->(:ParkingSpotModel) return r")

(def update_metadata_for_parkingspot_acl_cypher "MATCH (s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->(o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_UPDATE]->(:ParkingSpotModel) return r")

(def create_parking_policy_acl_cypher "MATCH (s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->(o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_CREATE]->(:ParkingPolicyModel) return r")

(def create_app_user_data_acl_cypher "MATCH (u:User {userid: \"{{userid}}\"})  return u")

(def delete_app_user_data_acl_cypher "MATCH (u:User {userid: \"{{userid}}\"})  return u")

(def get_all_app_user_data_acl_cypher "MATCH (u:User {userid: \"{{userid}}\"})  return u")

(def get_app_user_data_acl_cypher "MATCH (u:User {userid: \"{{userid}}\"})  return u")

(def update_app_user_data_acl_cypher "MATCH (u:User {userid: \"{{userid}}\"})  return u")

(def delete_parking_policy_acl_cypher "MATCH (s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->(o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_DELETE]->(:ParkingPolicyModel) return r")

(def get_all_parking_policy_acl_cypher "MATCH (s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->(o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_READ]->(:ParkingPolicyModel) return r")

(def get_parking_policy_acl_cypher "MATCH (s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->(o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_READ]->(:ParkingPolicyModel) return r")

(def update_parking_policy_acl_cypher "MATCH (s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->(o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_UPDATE]->(:ParkingPolicyModel) return r")

(def get_parking_policy_version_acl_cypher "MATCH (s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->(o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_READ]->(:ParkingPolicyModel) return r")

(def get_all_versions_of_parking_policy_acl_cypher "MATCH (s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->(o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_READ]->(:ParkingPolicyModel) return r")

(def get_all_active_parking_policy_for_period_acl_cypher "MATCH (s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->(o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_READ]->(:ParkingPolicyModel) return r")

(def get_active_parking_policy_acl_cypher "MATCH (s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->(o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_READ]->(:ParkingPolicyModel) return r")

(def search_parking_policy_acl_cypher "MATCH (s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->(o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_READ]->(:ParkingPolicyModel) return r")

(def create_policy_category_acl_cypher "MATCH (s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->(o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_CREATE]->(:PolicyCategoryModel) return r")

(def delete_policy_category_acl_cypher "MATCH (s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->(o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_DELETE]->(:PolicyCategoryModel) return r")

(def get_all_policy_category_acl_cypher "MATCH (s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->(o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_READ]->(:PolicyCategoryModel) return r")

(def get_policy_category_acl_cypher "MATCH (s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->(o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_READ]->(:PolicyCategoryModel) return r")

(def update_policy_category_acl_cypher "MATCH (s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->(o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_UPDATE]->(:PolicyCategoryModel) return r")

(def policy_association_acl_cypher "MATCH (pg:ParkingGroup {parkinggroupid:\"{{parkinggroupid}}\"})-[:BELONGS_TO]->(s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->
  (o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_UPDATE]->(:ParkingPolicyModel) return r")

(def policy_disassociation_acl_cypher "MATCH (pg:ParkingGroup {parkinggroupid:\"{{parkinggroupid}}\"})-[:BELONGS_TO]->(s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->
  (o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_UPDATE]->(:ParkingPolicyModel) return r")

(def associated_parking_groups_acl_cypher "MATCH (s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->(o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_READ]->(:ParkingGroupModel) return r")

(def policy_tag_association_acl_cypher "MATCH (s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->(o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_UPDATE]->(:ParkingPolicyModel) return r")

(def policy_tag_disassociation_acl_cypher "MATCH (s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->(o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_DELETE]->(:ParkingPolicyModel) return r")

(def get_business_alert_acl_cypher "MATCH (s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->(o:Org {orgid: \"{{orgid}}\"})-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_READ]->(:BusinessAlertModel) return r")

(def create_business_alert_acl_cypher "MATCH (s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->(o:Org {orgid: \"{{orgid}}\"})-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_CREATE]->(:BusinessAlertModel) return r")

(def update_business_alert_acl_cypher "MATCH (s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->(o:Org {orgid: \"{{orgid}}\"})-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_UPDATE]->(:BusinessAlertModel) return r")

(def delete_business_alert_acl_cypher "MATCH (s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->(o:Org {orgid: \"{{orgid}}\"})-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_DELETE]->(:BusinessAlertModel) return r")

(def get_trigger_acl_cypher "MATCH (s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->(o:Org {orgid: \"{{orgid}}\"})-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_READ]->(:TriggerModel) return r")

(def create_trigger_acl_cypher "MATCH (s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->(o:Org {orgid: \"{{orgid}}\"})-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_CREATE]->(:TriggerModel) return r")

(def update_trigger_acl_cypher "MATCH (s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->(o:Org {orgid: \"{{orgid}}\"})-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_UPDATE]->(:TriggerModel) return r")

(def delete_trigger_acl_cypher "MATCH (s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->(o:Org {orgid: \"{{orgid}}\"})-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_DELETE]->(:TriggerModel) return r")

(def get_gps_acl_cypher "MATCH (s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->(o:Org {orgid: \"{{orgid}}\"})-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_READ]->(:GpsModel) return r")

(def create_whatif_job_acl_cypher "MATCH (s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->(o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_CREATE]->(:WhatIfAnalysisModel) return r")

(def delete_whatif_job_acl_cypher "MATCH (s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->(o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_DELETE]->(:WhatIfAnalysisModel) return r")

(def get_whatif_job_acl_cypher "MATCH (s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->(o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_READ]->(:WhatIfAnalysisModel) return r")

(def update_whatif_job_acl_cypher "MATCH (s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->(o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_UPDATE]->(:WhatIfAnalysisModel) return r")

(def create_whatif_policy_acl_cypher "MATCH (s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->(o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_CREATE]->(:WhatIfPolicyModel) return r")

(def delete_whatif_policy_acl_cypher "MATCH (s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->(o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_DELETE]->(:WhatIfPolicyModel) return r")

(def get_whatif_job_policy_cypher "MATCH (s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->(o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_READ]->(:WhatIfPolicyModel) return r")

(def update_whatif_policy_acl_cypher "MATCH (s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->(o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_UPDATE]->(:WhatIfPolicyModel) return r")

(def create_whatif_tag_acl_cypher "MATCH (s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->(o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_CREATE]->(:WhatIfTagModel) return r")

(def delete_whatif_tag_acl_cypher "MATCH (s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->(o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_DELETE]->(:WhatIfTagModel) return r")

(def get_whatif_job_tag_cypher "MATCH (s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->(o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_READ]->(:WhatIfTagModel) return r")

(def update_whatif_tag_acl_cypher "MATCH (s:Site {siteid:\"{{siteid}}\"})-[:BELONGS_TO]->(o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_UPDATE]->(:WhatIfTagModel) return r")

(def get_uf_alarms_acl_cypher "MATCH (po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_READ]->(:UFAlarmModel) return r")

(def create_uf_alarms_acl_cypher "MATCH (po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_CREATE]->(:UFAlarmModel) return r")

(def update_uf_alarms_acl_cypher "MATCH (po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_UPDATE]->(:UFAlarmModel) return r")

(def delete_uf_alarms_acl_cypher "MATCH (po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r:CAN_DELETE]->(:UFAlarmModel) return r")

(def get_notification_types_acl_cypher "MATCH (po:Org)-[:HAS_USER]->(u:User {userid: \"{{userid}}\"})-[:IS]->(role:Role)-[r]->(:NotificationModel) return r")


;; Return status code for cause
(defn status-code-for
  [cause]
  (case cause
    :not-authorized 403
    :not-found 404
    400))

;; Return object name for map {:objectid "someid"}
(defn describe-item [jsmap]
  (let [key (first (keys jsmap ))
        key-name (name key)
        id (get jsmap key)
        type (clojure.string/capitalize (subs key-name 0 (- (count key-name) 2 )))
        fixed-type (case type
                         "Pdprofile" "PDProfile"
                         "Dhprofile" "DHProfile"
                         type)
        props {"type" type "keyname" key-name "id" id}
        cypher (if (vector? id)
                 (spy :debug (format "MATCH(o:%s) WHERE %s IN {props}.id RETURN COLLECT(o.name) as name" type key-name))
                 (spy :debug (format "MATCH(o:%s {%s:{props}.id}) RETURN o.name as name" type key-name)))
        results (-> cypher
                   (neo4j/executeQuery props)
                   json/read-str)
        success (not (empty? results))
        error (empty? results)
        type-name (.toLowerCase type)
        name_s (first (vals results ) )
        returned_all  (if (vector? id) (and success (= (count name_s) (count id))) success )
        object-name (if (vector? id) (clojure.string/join "', '" name_s) name_s )
        ;; key-name (keyword type-name)
        ;; id (get-in jsmap [key-name (keyword (str type-name "id"))])
        name (if-not (true? success) (format "unknown-%s ('%s')" type-name id) object-name)
       ]
        (   if-not (true? returned_all)
               (throw (ex-info (format (if (vector? id) " some of %s not found" "%s with id=%s not found") fixed-type id) {:cause :not-found}) )
               name
               )
       ;(   if(true? error)
       ;         (throw (ex-info (format "%s with id=%s not found" fixed-type id) {:cause :not-found}) )
       ;         name
       ;)
  )
)

;; Prepare exception
(defn prepare-exception [e]
  (let [message (.getMessage e)
        cause (:cause (ex-data e))
        ;; cause (get e :cause )
        status-code (status-code-for cause)
       ]
       (ex-info message {:cause cause :status status-code})
  )
)

(def get-sensity-org-id
  (delay (get (json/read-str (neo4j/executeQuery sensity_org_id)) "orgid"))
)

(defn appropriate-role [orgid target_role]
  (if (= orgid @get-sensity-org-id) (or (= target_role "sensity_user") (= target_role "sensity_admin") (= target_role "sensity_read_only") (= target_role "customer_admin")) true)
)

(defn acl_get_activity_logs [jsmap]
  (let [userid (jsmap :user)
        orgid (get-in jsmap [:orgprops :orgid])
        query
        (spy :debug (render get_activity_acl_cypher {:userid userid :orgid orgid}))]
    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (format "User: %s is not authorized to get audit for org %s" (describe-item {:userid userid}) (describe-item {:orgid orgid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_get_ota_status [jsmap]
  (let [userid (jsmap :user)
        orgid (get-in jsmap [:orgprops :orgid])
        query
        (spy :debug (render get_ota_status_acl_cypher {:userid userid :orgid orgid}))]
    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (format "User: %s is not authorized to get ota status for org %s" (describe-item {:userid userid}) (describe-item {:orgid orgid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_create_site [jsmap]
  (let [userid (jsmap :user)
        orgid (get-in jsmap [:orgprops :orgid])
        query
        (spy :debug (render create_site_acl_cypher {:userid userid :orgid orgid}))]
    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to create a site") {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))



(defn acl_update_overlay [jsmap]
  (let [userid (jsmap :user)
        overlayid (get-in jsmap [:overlayprops :overlayid])
        query
        (spy :debug (render update_overlay_acl_cypher {:userid userid :overlayid overlayid}))]
    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to update an overlay") {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_update_group [jsmap]
  (let [userid (jsmap :user)
        groupid (get-in jsmap [:groupprops :groupid])
        query
        (spy :debug (render update_group_acl_cypher {:userid userid :groupid groupid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to update a group") {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_update_fixture [jsmap]
  (let [userid (jsmap :user)
        fixtureid (get-in jsmap [:fixtureprops :fixtureid])
        query
        (spy :debug (render update_fixture_acl_cypher {:userid userid :fixtureid fixtureid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to update a fixture") {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_update_notification [jsmap]
  (let [userid (jsmap :user)
        notificationid (get-in jsmap [:notificationprops :notificationid])
        siteid (get-in jsmap [:siteprops :siteid])
        orgid (get-in jsmap [:orgprops :orgid])
        query
        (spy :debug (render update_notification_acl_cypher {:userid userid :notificationid notificationid :siteid siteid :orgid orgid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to update an notification " (describe-item {:notificationid notificationid}) " for site " (describe-item {:siteid siteid}) " and org " (describe-item {:orgid orgid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_update_alert [jsmap]
  (let [userid (jsmap :user)
        alertid (get-in jsmap [:alertprops :alertid])
        siteid (get-in jsmap [:siteprops :siteid])
        orgid (get-in jsmap [:orgprops :orgid])
        query
        (spy :debug (render update_alert_acl_cypher {:userid userid :alertid alertid :siteid siteid :orgid orgid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to update an alert " (describe-item {:alertid alertid}) " for site " (describe-item {:siteid siteid}) " and org " (describe-item {:orgid orgid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_update_parkingzone [jsmap]
  (let [userid (jsmap :user)
        parkingzoneid (get-in jsmap [:parkingzoneprops :parkingzoneid])
        query
        (spy :debug (render update_parkingzone_acl_cypher {:userid userid :parkingzoneid parkingzoneid}))]
    (debug query)
    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to update a parking zone " (describe-item {:parkingzoneid parkingzoneid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_update_parkingspot [jsmap]
  (let [userid (jsmap :user)
        parkingzoneid (get-in jsmap [:parkingzoneprops :parkingzoneid])
        query
        (spy :debug (render update_parkingzone_acl_cypher {:userid userid :parkingzoneid parkingzoneid}))]
    (debug query)
    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to update a parking spot" (describe-item {:parkingzoneid parkingzoneid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_update_parkinggroup [jsmap]
  (let [userid (jsmap :user)
        parkinggroupid (get-in jsmap [:parkinggroupprops :parkinggroupid])
        query
        (spy :debug (render update_parkinggroup_acl_cypher {:userid userid :parkinggroupid parkinggroupid}))]
    (debug query)
    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to update a parking group" (describe-item {:parkinggroupid parkinggroupid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_update_site [jsmap]
  (let [userid (jsmap :user)
        siteid (get-in jsmap [:siteprops :siteid])
        query
        (spy :debug (render update_site_acl_cypher {:userid userid :siteid siteid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to update a site " (describe-item {:siteid siteid}) ) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_assign_node [jsmap]
  (let [userid (jsmap :user)
        siteid (get-in jsmap [:siteprops :siteid])
        soid (get-in jsmap [:soid])
        query
        (spy :debug (render assign_node_acl_cypher {:userid userid :siteid siteid :soid soid}))]

       (try
         (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
              (if (= mycount 0)
                (throw (ex-info (format "User: %s is not authorized to assign a node for site %s" userid siteid) {:cause :not-authorized}))))
         (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
       jsmap))

(defn acl_create_node [jsmap]
  (let [userid (jsmap :user)
        siteid (get-in jsmap [:siteprops :siteid])
        query
        (spy :debug (render create_node_acl_cypher {:userid userid :siteid siteid}))]

       (try
         (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
              (if (= mycount 0)
                (throw (ex-info (format "User: %s is not authorized to create a node for site %s" userid siteid) {:cause :not-authorized}))))
         (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
       jsmap))

(defn acl_update_node [jsmap]
  (let [userid (jsmap :user)
        nodeid (get-in jsmap [:nodeprops :nodeid])
        siteid (get-in jsmap [:siteprops :siteid])
        query
        (spy :debug (render update_node_acl_cypher {:userid userid :siteid siteid :nodeid nodeid}))]

       (try
         (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
              (if (= mycount 0)
                (throw (ex-info (format "User: %s is not authorized to update a node %s for site %s" (describe-item {:userid userid}) (describe-item {:nodeid nodeid}) (describe-item {:siteid siteid})) {:cause :not-authorized}))))
         (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
       jsmap))

(defn acl_activate_node [jsmap]
  (let [userid (jsmap :user)
        nodeid (get-in jsmap [:nodeprops :nodeid])
        siteid (get-in jsmap [:siteprops :siteid])
        query
        (spy :debug (render activate_node_acl_cypher {:userid userid :siteid siteid :nodeid nodeid}))]

       (try
         (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
              (if (= mycount 0)
                (throw (ex-info (format "User: %s is not authorized to activate a node %s for site %s" (describe-item {:userid userid}) (describe-item {:nodeid nodeid}) jsmap) {:cause :not-authorized}))))
         (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
       jsmap))

(defn acl_deactivate_node [jsmap]
  (let [userid (jsmap :user)
        nodeid (get-in jsmap [:nodeprops :nodeid])
        siteid (get-in jsmap [:siteprops :siteid])
        query
        (spy :debug (render deactivate_node_acl_cypher {:userid userid :siteid siteid :nodeid nodeid}))]

       (try
         (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
              (if (= mycount 0)
                (throw (ex-info (format "User: %s is not authorized to deactivate a node %s for site %s" (describe-item {:userid userid}) (describe-item {:nodeid nodeid}) (describe-item {:siteid siteid})) {:cause :not-authorized}))))
         (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
       jsmap))

(defn acl_create_empty_node [jsmap]
  (let [userid (jsmap :user)
        query
        (spy :debug (render create_empty_node_acl_cypher {:userid userid}))]

       (try
         (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
              (if (= mycount 0)
                (throw (ex-info (format "User: %s is not authorized to create a node" (describe-item {:userid userid})) {:cause :not-authorized}))))
         (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
       jsmap))

(defn acl_create_overlay [jsmap]
  (let [userid (jsmap :user)
        siteid (get-in jsmap [:siteprops :siteid])
        query
        (spy :debug (render create_overlay_acl_cypher {:userid userid :siteid siteid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to create an overlay") {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_create_fixture [jsmap]
  (let [userid (jsmap :user)
        siteid (get-in jsmap [:siteprops :siteid])
        query
        (spy :debug (render create_fixture_acl_cypher {:userid userid :siteid siteid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to create a fixture") {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_create_notification [jsmap]
  (let [userid (jsmap :user)
        siteid (get-in jsmap [:siteprops :siteid])
        orgid (get-in jsmap [:orgprops :orgid])
        query
        (spy :debug (render create_notification_acl_cypher {:userid userid :siteid siteid :orgid orgid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to create an notification at site " (describe-item {:siteid siteid}) " and org " (describe-item {:orgid orgid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_activate_notification [jsmap]
  (let [userid (jsmap :user)
        notificationid (get-in jsmap [:notificationprops :notificationid])
        siteid (get-in jsmap [:siteprops :siteid])
        orgid (get-in jsmap [:orgprops :orgid])
        query
        (spy :debug (render activate_notification_acl_cypher {:userid userid :notificationid notificationid :siteid siteid :orgid orgid}))]

       (try
         (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
              (if (= mycount 0)
                (throw (ex-info (format "User: %s is not authorized to activate a notification %s for site %s and org %s" (describe-item {:userid userid}) (describe-item {:notificationid notificationid}) (describe-item {:siteid siteid}) (describe-item {:orgid orgid})) {:cause :not-authorized}))))
         (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
       jsmap))

(defn acl_deactivate_notification [jsmap]
  (let [userid (jsmap :user)
        notificationid (get-in jsmap [:notificationprops :notificationid])
        siteid (get-in jsmap [:siteprops :siteid])
        orgid (get-in jsmap [:orgprops :orgid])
        query
        (spy :debug (render deactivate_notification_acl_cypher {:userid userid :notificationid notificationid :siteid siteid :orgid orgid}))]

       (try
         (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
              (if (= mycount 0)
                (throw (ex-info (format "User: %s is not authorized to deactivate a notification %s for site %s and org %s" (describe-item {:userid userid}) (describe-item {:notificationid notificationid}) (describe-item {:siteid siteid}) (describe-item {:orgid orgid})) {:cause :not-authorized}))))
         (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
       jsmap))

(defn acl_create_alert [jsmap]
  (let [userid (jsmap :user)
        siteid (get-in jsmap [:siteprops :siteid])
        orgid (get-in jsmap [:orgprops :orgid])
        query
        (spy :debug (render create_alert_acl_cypher {:userid userid :siteid siteid :orgid orgid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to create an alert at site " (describe-item {:siteid siteid}) " and org " (describe-item {:orgid orgid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_create_parkingzone [jsmap]
  (let [userid (jsmap :user)
        siteid (get-in jsmap [:siteprops :siteid])
        query
        (spy :debug (render create_parkingzone_acl_cypher {:userid userid :siteid siteid}))]
    (debug query)
    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (Exception. (str "User: " (describe-item {:userid userid}) " is not authorized to create a parking zone at site " (describe-item {:siteid siteid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_create_parkingspot [jsmap]
  (let [userid (jsmap :user)
        siteid (get-in jsmap [:siteprops :siteid])
        query
        (spy :debug (render create_parkingzone_acl_cypher {:userid userid :siteid siteid}))]
    (debug query)
    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (Exception. (str "User: " (describe-item {:userid userid}) " is not authorized to create a parking spot at site " (describe-item {:siteid siteid}) ) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_create_parkinggroup [jsmap]
  (let [userid (jsmap :user)
        siteid (get-in jsmap [:siteprops :siteid])
        query
        (spy :debug (render create_parkingzone_acl_cypher {:userid userid :siteid siteid}))]
    (debug query)
    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (Exception. (str "User: " (describe-item {:userid userid}) " is not authorized to create a parking groupat site " (describe-item {:siteid siteid}) ) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_assign_parkinggroup [jsmap]
  (let [userid (jsmap :user)
        nid (jsmap :nodeid)
        pgid (jsmap :parkinggroupid)
        siteid (get-in jsmap [:siteprops :siteid])
        query
        (spy :debug (render assign_parkinggroup_acl_cypher {:userid userid :siteid siteid :parkinggroupid pgid :nodeid nid}))]
    (debug query)
    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to assing a node " (describe-item {:nodeid nid})" to parking group " (describe-item {:parkinggroupid pgid}) " at site " (describe-item {:siteid siteid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_get_org [jsmap]
  (let [userid (jsmap :user)
        orgid (get-in jsmap [:orgprops :orgid])
        query
        (spy :debug (render get_org_acl_cypher {:userid userid :orgid orgid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to read organization " (describe-item {:orgid orgid}) ) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_create_org [jsmap]
  (let [userid (jsmap :user)
        orgid (get-in jsmap [:orgprops :po])
        query
        (spy :debug (render create_org_acl_cypher {:userid userid :orgid orgid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to create an org " (describe-item {:orgid orgid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_update_org [jsmap]
  (let [userid (jsmap :user)
        orgid (get-in jsmap [:orgprops :orgid])
        query
        (spy :debug (render update_org_acl_cypher {:userid userid :orgid orgid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to update an org " (describe-item {:orgid orgid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_delete_org [jsmap]
  (let [userid (jsmap :user)
        orgid (get-in jsmap [:orgprops :orgid])
        query
        (spy :debug (render delete_org_acl_cypher {:userid userid :orgid orgid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (format "User: %s is not authorized to delete organization %s" (describe-item {:userid userid}) (describe-item {:orgid orgid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_suspend_org [jsmap]
  (let [userid (jsmap :user)
        orgid (get-in jsmap [:orgprops :orgid])
        query
        (spy :debug (render suspend_org_acl_cypher {:userid userid :orgid orgid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (format "User: %s is not authorized to suspend organization %s" (describe-item {:userid userid}) (describe-item {:orgid orgid})) {:cause :not-authorized}))))
     (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_activate_org [jsmap]
  (let [userid (jsmap :user)
        orgid (get-in jsmap [:orgprops :orgid])
        query
        (spy :debug (render activate_org_acl_cypher {:userid userid :orgid orgid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (format "User: %s is not authorized to activate organization" (describe-item {:userid userid}) (describe-item {:orgid orgid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_get_partner [jsmap]
  (let [userid (jsmap :user)
        orgid (get-in jsmap [:orgprops :orgid])
        query
        (spy :debug (render get_partner_acl_cypher {:userid userid :orgid orgid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to read organization" (describe-item {:orgid orgid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_create_partner [jsmap]
  (let [userid (jsmap :user)
        query
        (spy :debug (render create_partner_acl_cypher {:userid userid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to create an org") {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_update_partner [jsmap]
  (let [userid (jsmap :user)
        orgid (get-in jsmap [:orgprops :orgid])
        query
        (spy :debug (render update_partner_acl_cypher {:userid userid :orgid orgid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to update an org") {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_delete_partner [jsmap]
  (let [userid (jsmap :user)
        orgid (get-in jsmap [:orgprops :orgid])
        query
        (spy :debug (render delete_partner_acl_cypher {:userid userid :orgid orgid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (format "User: %s is not authorized to delete %s" (describe-item {:userid userid}) (describe-item {:orgid orgid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_delete_org_partner [jsmap]
  (let [userid (jsmap :user)
        orgid (get-in jsmap [:orgprops :orgid])
        query
        (spy :debug (render delete_org_partner_acl_cypher {:userid userid :orgid orgid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (format "User: %s is not authorized to delete %s" (describe-item {:userid userid}) (describe-item {:orgid orgid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_suspend_partner [jsmap]
  (let [userid (jsmap :user)
        orgid (get-in jsmap [:orgprops :orgid])
        query
        (spy :debug (render suspend_partner_acl_cypher {:userid userid :orgid orgid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (format "User: %s is not authorized to suspend %s" (describe-item {:userid userid}) (describe-item {:orgid orgid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_suspend_org_partner [jsmap]
  (let [userid (jsmap :user)
        orgid (get-in jsmap [:orgprops :orgid])
        query
        (spy :debug (render suspend_org_partner_acl_cypher {:userid userid :orgid orgid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (format "User: %s is not authorized to suspend %s" (describe-item {:userid userid}) (describe-item {:orgid orgid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_activate_partner [jsmap]
  (let [userid (jsmap :user)
        orgid (get-in jsmap [:orgprops :orgid])
        query
        (spy :debug (render activate_partner_acl_cypher {:userid userid :orgid orgid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (format "User: %s is not authorized to activate any organizations" (describe-item {:userid userid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_get_site [jsmap]
  (let [userid (jsmap :user)
        siteid (get-in jsmap [:siteprops :siteid])
        query
        (spy :debug (render get_site_acl_cypher {:userid userid :siteid siteid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to read site" (describe-item {:siteid siteid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_get_node [jsmap]
  (let [userid (jsmap :user)
        nodeid (get-in jsmap [:nodeprops :nodeid])
        query
        (spy :debug (render get_node_acl_cypher {:userid userid :nodeid nodeid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to read node " (describe-item {:nodeid nodeid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_get_sensors [jsmap]
  (let [userid (jsmap :user)
        nodeid (get-in jsmap [:nodeprops :nodeid])
        siteid (get-in jsmap [:siteprops :siteid])
        querystring (if (= nodeid "all") get_sensor_site_acl_cypher get_sensor_acl_cypher)
        orgid (get-in jsmap [:orgprops :orgid])
        query
        (spy :debug (render querystring {:userid userid :nodeid nodeid :siteid siteid :orgid orgid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to read sensor from node " (describe-item {:nodeid nodeid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_get_overlay [jsmap]
  (let [userid (jsmap :user)
        overlayid (get-in jsmap [:overlayprops :overlayid])
        query
        (spy :debug (render get_overlay_acl_cypher {:userid userid :overlayid overlayid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to read overlay" (describe-item {:overlayid overlayid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_get_group [jsmap]
  (let [userid (jsmap :user)
        groupid (get-in jsmap [:groupprops :groupid])
        query
        (spy :debug (render get_group_acl_cypher {:userid userid :groupid groupid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to read groups" (describe-item {:groupid groupid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_get_fixture [jsmap]
  (let [userid (jsmap :user)
        fixtureid (get-in jsmap [:fixtureprops :fixtureid])
        query
        (spy :debug (render get_fixture_acl_cypher {:userid userid :fixtureid fixtureid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to read fixture" (describe-item {:fixtureid fixtureid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))


(defn acl_get_all_overlays [jsmap]
  (let [userid (jsmap :user)
        siteid (get-in jsmap [:siteprops :siteid])
        query
        (spy :debug (render get_all_overlays_acl_cypher {:userid userid :siteid siteid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to read any overlays for site " (describe-item {:siteid siteid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_get_all_groups [jsmap]
  (let [userid (jsmap :user)
        siteid (get-in jsmap [:siteprops :siteid])
        query
        (spy :debug (render get_all_groups_acl_cypher {:userid userid :siteid siteid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to read any groups for site " (describe-item {:siteid siteid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_get_all_fixtures [jsmap]
  (let [userid (jsmap :user)
        siteid (get-in jsmap [:siteprops :siteid])
        query
        (spy :debug (render get_all_fixtures_acl_cypher {:userid userid :siteid siteid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to read any fixtures for site " (describe-item {:siteid siteid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_get_notification [jsmap]
  (let [userid (jsmap :user)
        notificationid (get-in jsmap [:notificationprops :notificationid])
        siteid (get-in jsmap [:siteprops :siteid])
        orgid (get-in jsmap [:orgprops :orgid])
        query
        (spy :debug (render get_notification_acl_cypher {:userid userid :notificationid notificationid :siteid siteid :orgid orgid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to read notification " (describe-item {:notificationid notificationid}) " from site " (describe-item {:siteid siteid}) " and org " (describe-item {:orgid orgid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_get_all_notifications_for_site [jsmap]
  (let [userid (jsmap :user)
        siteid (get-in jsmap [:siteprops :siteid])
        orgid (get-in jsmap [:orgprops :orgid])
        query
        (spy :debug (render get_all_notifications_for_site_acl_cypher {:userid userid :siteid siteid :orgid orgid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to read any notifications for site " (describe-item {:siteid siteid}) " and org " (describe-item {:orgid orgid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_get_all_notifications_for_user [jsmap]
  (let [userid (jsmap :user)
        query
        (spy :debug (render get_all_notifications_for_user_acl_cypher {:userid userid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to read any notifications") {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_get_all_notifications_for_org [jsmap]
  (let [userid (jsmap :user)
        orgid (get-in jsmap [:orgprops :orgid])
        query
        (spy :debug (render get_all_notifications_for_org_acl_cypher {:userid userid :orgid orgid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to read any notifications for org " (describe-item {:orgid orgid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_get_alert [jsmap]
  (let [userid (jsmap :user)
        alertid (get-in jsmap [:alertprops :alertid])
        siteid (get-in jsmap [:siteprops :siteid])
        orgid (get-in jsmap [:orgprops :orgid])
        query
        (spy :debug (render get_alert_acl_cypher {:userid userid :alertid alertid :siteid siteid :orgid orgid}))]
    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to read alert" (describe-item {:alertid alertid}) " for site " (describe-item {:siteid siteid}) " and org " (describe-item {:orgid orgid}) ) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_get_parkingzone [jsmap]
  (let [userid (jsmap :user)
        parkingzoneid (get-in jsmap [:parkingzoneprops :parkingzoneid])
        siteid (get-in jsmap [:siteprops :siteid])
        query
        (spy :debug (render get_parkingzone_acl_cypher {:userid userid :siteid siteid}))]
    (debug query)
    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to read parking zone " (describe-item {:parkingzoneid parkingzoneid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_get_parkingspot [jsmap]
  (let [userid (jsmap :user)
        parkingzoneid (get-in jsmap [:parkingzoneprops :parkingzoneid])
        query
        (spy :debug (render get_parkingzone_acl_cypher {:userid userid :parkingzoneid parkingzoneid}))]
    (debug query)
    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to read parking spot "(describe-item {:parkingzoneid parkingzoneid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_get_parkinginfo [jsmap]
  (let [userid (jsmap :user)
        siteid (get-in jsmap [:siteprops :siteid])
        query
        (spy :debug (render get_parkinginfo_acl_cypher {:userid userid :siteid siteid}))]
    (debug query)
    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to read any parking info for site " (describe-item {:siteid siteid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_get_trafficinfo [jsmap]
  (let [userid (jsmap :user)
        siteid (get-in jsmap [:siteprops :siteid])
        query
        (spy :debug (render get_trafficinfo_acl_cypher {:userid userid :siteid siteid}))]
    (debug query)
    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to read any traffic info for site " (describe-item {:siteid siteid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_get_parkinggroup [jsmap]
  (let [userid (jsmap :user)
        parkinggroupid (get-in jsmap [:parkinggroupprops :parkinggroupid])
        query
        (spy :debug (render get_parkinggroup_acl_cypher {:userid userid :parkinggroupid parkinggroupid}))]
    (debug query)
    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to read parking group " (describe-item {:parkinggroupid parkinggroupid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_get_all_alerts [jsmap]
  (let [userid (jsmap :user)
        siteid (get-in jsmap [:siteprops :siteid])
        orgid (get-in jsmap [:orgprops :orgid])
        query
        (spy :debug (render get_all_alerts_acl_cypher {:userid userid :siteid siteid :orgid orgid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to read any alerts for site " (describe-item {:siteid siteid}) " and org " (describe-item {:orgid orgid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_get_all_org_alerts [jsmap]
  (let [userid (jsmap :user)
        orgid (get-in jsmap [:orgprops :orgid])
        query
        (spy :debug (render get_all_org_alerts_acl_cypher {:userid userid :orgid orgid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to read any alerts for org " (describe-item {:orgid orgid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_get_all_parkingzones [jsmap]
  (let [userid (jsmap :user)
        siteid (get-in jsmap [:siteprops :siteid])
        query
        (spy :debug (render get_all_parkingzones_acl_cypher {:userid userid :siteid siteid}))]
    (debug query)
    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to read any parking zones for site " (describe-item {:siteid siteid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_get_all_parkingspots [jsmap]
  (let [userid (jsmap :user)
        siteid (get-in jsmap [:siteprops :siteid])
        query
        (spy :debug (render get_all_parkingzones_acl_cypher {:userid userid :siteid siteid}))]
    (debug query)
    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to read any parking spots for site " (describe-item {:siteid siteid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_get_all_parkinggroups [jsmap]
  (let [userid (jsmap :user)
        siteid (get-in jsmap [:siteprops :siteid])
        query
        (spy :debug (render get_all_parkinggroups_acl_cypher {:userid userid :siteid siteid}))]
    (debug query)
    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to read any parking groups for site " (describe-item {:siteid siteid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_get_all_alerts_for_node [jsmap]
  (let [userid (jsmap :user)
        nodeid (get-in jsmap [:nodeprops :nodeid])
        siteid (get-in jsmap [:siteprops :siteid])
        orgid (get-in jsmap [:orgprops :orgid])
        query
        (spy :debug (render get_all_alerts_for_node_acl_cypher {:userid userid :nodeid nodeid :siteid siteid :orgid orgid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to read any alerts for node " (describe-item {:nodeid nodeid}) " linked to site " (describe-item {:siteid siteid}) " and org " (describe-item {:orgid orgid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_get_nodes4site [jsmap]
  (let [userid (jsmap :user)
        siteid (get-in jsmap [:siteprops :siteid])
        query
        (spy :debug (render get_nodes4site_acl_cypher {:userid userid :siteid siteid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to get nodes of a site " (describe-item {:siteid siteid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_get_lost_found_nodes [jsmap]
  (let [userid (jsmap :user)
        query
        (spy :debug (render get_lost_found_nodes_acl_cypher {:userid userid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to get lost and found nodes") {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_get_sites4org [jsmap]
  (let [userid (jsmap :user)
        orgid (get-in jsmap [:orgprops :orgid])
        query
        (spy :debug (render get_sites4org_acl_cypher {:userid userid :orgid orgid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to get sites of a org") {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_get_suspendedsites4org [jsmap]
  (let [userid (jsmap :user)
        orgid (get-in jsmap [:orgprops :orgid])
        query
        (spy :debug (render get_suspendedsites4org_acl_cypher {:userid userid :orgid orgid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to get suspended sites of a org") {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_create_group [jsmap]
  (let [userid (jsmap :user)
        siteid (get-in jsmap [:siteprops :siteid])
        query
        (spy :debug (render create_group_acl_cypher {:userid userid :siteid siteid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to create any groups") {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_get_org_user [jsmap]
  (let [userid (jsmap :user)
        orgid (get-in jsmap [:orgprops :orgid])
        query
        (spy :debug (render get_org_user_acl_cypher {:userid userid :orgid orgid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (format "User: %s is not authorized to read user" (describe-item {:userid userid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_create_org_user [jsmap]
  (let [userid (jsmap :user)
        orgid (get-in jsmap [:orgprops :orgid])
        target_role (get-in jsmap [:userprops :roles] )
        role (spy :debug (json/read-str (neo4j/executeQuery (render get_user_rolename_cypher {:userid userid :orgid orgid}))))
        role_name (if (empty? role) "none" (get role "rolename"))
        can_create (spy :debug (json/read-str (neo4j/executeQuery (render get_user_createlist_cypher {:target_role target_role :role role_name}))))
        query (spy :debug (render create_org_user_acl_cypher {:userid userid :orgid orgid}))]
    (try
      (do
        (if (or (and (not= role_name "none") (not can_create)) (not (appropriate-role orgid target_role)))
          (throw (ex-info (format "User with role %s can not update user with role %s" role_name target_role) {:cause :not-authorized})))
          ;; (debug role)
        (debug query)
        (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
          (if (= mycount 0)
            (throw (ex-info (format "User: %s is not authorized to create user" (describe-item {:userid userid}) " for org " (describe-item {:orgid orgid})) {:cause :not-authorized})))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))



(defn acl_delete_org_user [jsmap]
  (let [userid (jsmap :user)
        orgid (get-in jsmap [:orgprops :orgid])
        del_userid (get-in jsmap [:userprops :userid])
        target_role (spy :debug (json/read-str (neo4j/executeQuery (render get_user_rolename_cypher {:userid del_userid :orgid orgid}))))
        target_role_name (if (empty? target_role) "none" (get target_role "rolename"))
        role (spy :debug (json/read-str (neo4j/executeQuery (render get_user_rolename_cypher {:userid userid :orgid orgid}))))
        role_name (if (empty? role) "none" (get role "rolename"))
        can_create (spy :debug (json/read-str (neo4j/executeQuery (render get_user_deletelist_cypher {:target_role target_role_name :role role_name}))))
        query (spy :debug (render delete_org_user_acl_cypher {:userid userid :orgid orgid}))]
    (try
      (do
        (if (and (not= role_name "none") (not can_create))
          (throw (ex-info (format "User with role %s can not delete user with role %s" role_name target_role) {:cause :not-authorized})))
          ;; (debug role)
        (debug query)
          (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
            (if (= mycount 0)
              (throw (ex-info (format "User: %s is not authorized to delete user" (describe-item {:userid userid})) {:cause :not-authorized})))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_update_org_user [jsmap]
  (let [userid (jsmap :user)
        orgid (get-in jsmap [:orgprops :orgid])
        upd_userid (get-in jsmap [:userprops :userid])
        target_role (spy :debug (json/read-str (neo4j/executeQuery (render get_user_rolename_cypher {:userid upd_userid :orgid orgid}))))
        target_role_name (if (empty? target_role) "none" (get target_role "rolename"))
        role (spy :debug (json/read-str (neo4j/executeQuery (render get_user_rolename_cypher {:userid userid :orgid orgid}))))
        role_name (if (empty? role) "none" (get role "rolename"))
        can_create (spy :debug (json/read-str (neo4j/executeQuery (render get_user_updatelist_cypher {:target_role target_role_name :role role_name}))))
        query (spy :debug (render update_org_user_acl_cypher {:userid userid :orgid orgid}))]
    (try
      (do
        (if (and (not= role_name "none") (not can_create) (not (appropriate-role orgid target_role)))
          (throw (ex-info (format "User with role %s can not create user with role %s" role_name target_role) {:cause :not-authorized})))
          ;; (debug role)
        (debug query)
        (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
          (if (= mycount 0)
            (throw (ex-info (format "User: %s is not authorized to update user" (describe-item {:userid userid})) {:cause :not-authorized})))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_get_partner_user [jsmap]
  (let [userid (jsmap :user)
        orgid (get-in jsmap [:orgprops :orgid])
        query
        (spy :debug (render get_partner_user_acl_cypher {:userid userid :orgid orgid}))]
    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (format "User: %s is not authorized to read user" (describe-item {:userid userid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_create_partner_user [jsmap]
  (let [userid (jsmap :user)
        orgid (get-in jsmap [:orgprops :orgid])
        target_role (get-in jsmap [:userprops :roles] )
        role (spy :debug (json/read-str (neo4j/executeQuery (render get_user_rolename_cypher {:userid userid :orgid orgid}))))
        role_name (if (empty? role) "none" (get role "rolename"))
        can_create (spy :debug (json/read-str (neo4j/executeQuery (render get_user_createlist_cypher {:target_role target_role :role role_name}))))
        query (spy :debug (render create_partner_user_acl_cypher {:userid userid :orgid orgid}))]
    (try
      (if (or (= orgid @get-sensity-org-id) (not can_create)) (throw (ex-info (format "User: %s is not authorized to create user" (describe-item {:userid userid})) {:cause :not-authorized})))
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (format "User: %s is not authorized to create user" (describe-item {:userid userid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_delete_partner_user [jsmap]
  (let [userid (jsmap :user)
        orgid (get-in jsmap [:orgprops :orgid])
        del_userid (get-in jsmap [:userprops :userid])
        target_role (spy :debug (json/read-str (neo4j/executeQuery (render get_user_rolename_cypher {:userid del_userid :orgid orgid}))))
        target_role_name (if (empty? target_role) "none" (get target_role "rolename"))
        role (spy :debug (json/read-str (neo4j/executeQuery (render get_user_rolename_cypher {:userid userid :orgid orgid}))))
        role_name (if (empty? role) "none" (get role "rolename"))
        can_create (spy :debug (json/read-str (neo4j/executeQuery (render get_user_deletelist_cypher {:target_role target_role_name :role role_name}))))
        query (spy :debug (render delete_partner_user_acl_cypher {:userid userid :orgid orgid}))]
    (try
      (if (not can_create) (throw (ex-info (format "User: %s is not authorized to delete user" (describe-item {:userid userid})) {:cause :not-authorized})))
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (format "User: %s is not authorized to delete user" (describe-item {:userid userid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_update_partner_user [jsmap]
  (let [userid (jsmap :user)
        orgid (get-in jsmap [:orgprops :orgid])
        upd_userid (get-in jsmap [:userprops :userid])
        target_role (spy :debug (json/read-str (neo4j/executeQuery (render get_user_rolename_cypher {:userid upd_userid :orgid orgid}))))
        target_role_name (if (empty? target_role) "none" (get target_role "rolename"))
        role (spy :debug (json/read-str (neo4j/executeQuery (render get_user_rolename_cypher {:userid userid :orgid orgid}))))
        role_name (if (empty? role) "none" (get role "rolename"))
        can_create (spy :debug (json/read-str (neo4j/executeQuery (render get_user_updatelist_cypher {:target_role target_role_name :role role_name}))))
        query (spy :debug (render update_partner_user_acl_cypher {:userid userid :orgid orgid}))]
    (try
      (if (not can_create) (throw (ex-info (format "User: %s is not authorized to update user" (describe-item {:userid userid})) {:cause :not-authorized})))
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (format "User: %s is not authorized to update user" (describe-item {:userid userid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_get_users4org [jsmap]
  (let [userid (jsmap :user)
        orgid (get-in jsmap [:orgprops :orgid])
        query
        (spy :debug (render get_users4org_acl_cypher {:userid userid :orgid orgid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to get users of a org") {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_get_users4site [jsmap]
  (let [userid (jsmap :user)
        siteid (get-in jsmap [:siteprops :siteid])
        query
        (spy :debug (render get_users4site_acl_cypher {:userid userid :siteid siteid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to get users of a site") {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_get_users4partner [jsmap]
  (let [userid (jsmap :user)
        orgid (get-in jsmap [:orgprops :orgid])
        query
        (spy :debug (render get_users4partner_acl_cypher {:userid userid :orgid orgid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to get users of a org") {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_get_suspended_users4org [jsmap]
  (let [userid (jsmap :user)
        orgid (get-in jsmap [:orgprops :orgid])
        query
        (spy :debug (render get_suspended_users4org_acl_cypher {:userid userid :orgid orgid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to get users of a org") {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_get_suspended_users4partner [jsmap]
  (let [userid (jsmap :user)
        orgid (get-in jsmap [:orgprops :orgid])
        query
        (spy :debug (render get_suspended_users4partner_acl_cypher {:userid userid :orgid orgid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to get users of a partner") {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_get_user_permissions [jsmap]
  (let [userid (jsmap :user)
        orgid (get-in jsmap [:orgprops :orgid])
        query
        (spy :debug (render get_usermodel_permissions_for_user_acl_cypher {:userid userid :orgid orgid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to create(?) user") {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_delete_group [jsmap]
  (let [userid (jsmap :user)
        groupid (get-in jsmap [:groupprops :groupid])
        query
        (spy :debug (render delete_group_acl_cypher {:userid userid :groupid groupid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to delete any groups") {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_delete_fixture [jsmap]
  (let [userid (jsmap :user)
        fixtureid (get-in jsmap [:fixtureprops :fixtureid])
        query
        (spy :debug (render delete_fixture_acl_cypher {:userid userid :fixtureid fixtureid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to delete any fixtures") {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_delete_notification [jsmap]
  (let [userid (jsmap :user)
        notificationid (get-in jsmap [:notificationprops :notificationid])
        siteid (get-in jsmap [:siteprops :siteid])
        orgid (get-in jsmap [:orgprops :orgid])
        query
        (spy :debug (render delete_notification_acl_cypher {:userid userid :notificationid notificationid :siteid siteid :orgid orgid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to delete notification " (describe-item {:notificationid notificationid}) " for site " (describe-item {:siteid siteid}) " and org " (describe-item {:orgid orgid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_delete_alert [jsmap]
  (let [userid (jsmap :user)
        alertid (get-in jsmap [:alertprops :alertid])
        siteid (get-in jsmap [:siteprops :siteid])
        orgid (get-in jsmap [:orgprops :orgid])
        query
        (spy :debug (render delete_alert_acl_cypher {:userid userid :alertid alertid :siteid siteid :orgid orgid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to delete alert " (describe-item {:alertid alertid}) " for site " (describe-item {:siteid siteid}) " and org " (describe-item {:orgid orgid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_delete_parkingzone [jsmap]
  (let [userid (jsmap :user)
        parkingzoneid (get-in jsmap [:parkingzoneprops :parkingzoneid])
        query
        (spy :debug (render delete_parkingzone_acl_cypher {:userid userid :parkingzoneid parkingzoneid}))]
    (debug query)
    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to delete parking zones " (describe-item {:parkingzoneid parkingzoneid}) ) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_delete_parkingspot [jsmap]
  (let [userid (jsmap :user)
        parkingzoneid (get-in jsmap [:parkingzoneprops :parkingzoneid])
        query
        (spy :debug (render delete_parkingzone_acl_cypher {:userid userid :parkingzoneid parkingzoneid}))]
    (debug query)
    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to delete  parking spots " (describe-item {:parkingzoneid parkingzoneid}) )
                          {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_delete_parkinggroup [jsmap]
  (let [userid (jsmap :user)
        parkinggroupid (get-in jsmap [:parkinggroupprops :parkinggroupid])
        query
        (spy :debug (render delete_parkinggroup_acl_cypher {:userid userid :parkinggroupid parkinggroupid}))]
    (debug query)
    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to delete any parking group " (describe-item {:parkinggroupid parkinggroupid }) )
                          {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_delete_node [jsmap]
  (let [userid (jsmap :user)
        nodeid (get-in jsmap [:nodeprops :nodeid])
        siteid (get-in jsmap [:siteprops :siteid])
        orgid (get-in jsmap [:orgprops :orgid])
        query
        (spy :debug (render delete_node_acl_cypher {:userid userid :nodeid nodeid :siteid siteid :orgid orgid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (format "User %s is not authorized to delete node %s for site %s and org %s" (describe-item {:userid userid}) (describe-item {:nodeid nodeid}) (describe-item {:siteid siteid}) (describe-item {:orgid orgid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_real_delete_node [jsmap]
  (let [userid (jsmap :user)
        nodeid (get-in jsmap [:nodeprops :nodeid])
        siteid (get-in jsmap [:siteprops :siteid])
        orgid (get-in jsmap [:orgprops :orgid])
        query
        (spy :debug (render delete_node_really_acl_cypher {:userid userid :nodeid nodeid :siteid siteid :orgid orgid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (format "User %s is not authorized to really delete node %s for site %s and org %s" (describe-item {:userid userid}) (describe-item {:nodeid nodeid}) (describe-item {:siteid siteid}) (describe-item {:orgid orgid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_delete_overlay [jsmap]
  (let [userid (jsmap :user)
        overlayid (get-in jsmap [:overlayprops :overlayid])
        query
        (spy :debug (render delete_overlay_acl_cypher {:userid userid :overlayid overlayid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to delete any overlays") {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_create_install_pod [jsmap]
  (let [userid (jsmap :user)
        siteid (jsmap :siteid)
        query
        (spy :debug (render create_install_pod_acl_cypher {:userid userid :siteid siteid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to create any install pod") {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_delete_install_pod [jsmap]
  (let [userid (jsmap :user)
        query
        (spy :debug (render delete_install_pod_acl_cypher {:userid userid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to delete install pod") {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))



(defn acl_delete_site [jsmap]
  (let [userid (jsmap :user)
        orgid (get-in jsmap [:orgprops :orgid])
        siteid (get-in jsmap [:siteprops :siteid])
        query
        (spy :debug (render delete_site_acl_cypher {:userid userid :orgid orgid :siteid siteid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (format "User: %s is not authorized to delete any sites" (describe-item {:userid userid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_suspend_site [jsmap]
  (let [userid (jsmap :user)
        orgid (get-in jsmap [:orgprops :orgid])
        siteid (get-in jsmap [:siteprops :siteid])
        query
        (spy :debug (render suspend_site_acl_cypher {:userid userid :orgid orgid :siteid siteid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (format "User: %s is not authorized to suspend any sites" (describe-item {:userid userid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_activate_site [jsmap]
  (let [userid (jsmap :user)
        orgid (get-in jsmap [:orgprops :orgid])
        siteid (get-in jsmap [:siteprops :siteid])
        query
        (spy :debug (render suspend_site_acl_cypher {:userid userid :orgid orgid :siteid siteid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (format "User: %s is not authorized to activate any sites" (describe-item {:userid userid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_delete_firmware [jsmap]
  (let [userid (jsmap :user)
        firmwareid (get-in jsmap [:firmwareprops :firmwareid])
        query
        (spy :debug (render delete_firmware_acl_cypher {:userid userid :firmwareid firmwareid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (format "User: %s is not authorized to delete any firmware" (describe-item {:userid userid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_get_firmware [jsmap]
  (let [userid (jsmap :user)
        firmwareid (get-in jsmap [:firmwareprops :firmwareid])
        query
        (spy :debug (render get_firmware_acl_cypher {:userid userid :firmwareid firmwareid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (format "User: %s is not authorized to access any firmware" (describe-item {:userid userid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_create_firmware [jsmap]
  (let [userid (jsmap :user)
        siteid (get-in jsmap [:siteprops :siteid])
        query
        (spy :debug (render create_firmware_acl_cypher {:userid userid :siteid siteid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (format "User: %s is not authorized to create any firmware" (describe-item {:userid userid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_assign_firmware [jsmap]
  (let [userid (jsmap :user)
        siteid (get-in jsmap [:siteprops :siteid])
        query
        (spy :debug (render assign_firmware_acl_cypher {:userid userid :siteid siteid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (format "User: %s is not authorized to assign any firmware" (describe-item {:userid userid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_update_firmware [jsmap]
  (let [userid (jsmap :user)
        firmwareid (get-in jsmap [:firmwareprops :firmwareid])
        query
        (spy :debug (render update_firmware_acl_cypher {:userid userid  :firmwareid firmwareid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (format "User: %s is not authorized to update any firmware" (describe-item {:userid userid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))


(defn acl_get_etdh_trigger [jsmap]
  (let [userid (jsmap :user)
        etdhprofileid (get-in jsmap [:etdhprofileprops :etdhprofileid])
        siteid (get-in jsmap [:siteprops :siteid])
        orgid (get-in jsmap [:orgprops :orgid])
        query
        (spy :debug (render get_edth_trigger_acl_cypher {:userid userid :etdhprofileid etdhprofileid :siteid siteid :orgid orgid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (format "User: %s is not authorized to access EDTH trigger %s" (describe-item {:userid userid})  (describe-item {:etdhprofileid etdhprofileid}) ) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))


(defn acl_delete_etdh_trigger [jsmap]
  (let [userid (jsmap :user)
        etdhprofileid (get-in jsmap [:etdhprofileprops :etdhprofileid])
        siteid (get-in jsmap [:siteprops :siteid])
        orgid (get-in jsmap [:orgprops :orgid])
        query
        (spy :debug (render delete_edth_trigger_acl_cypher {:userid userid :etdhprofileid etdhprofileid :siteid siteid :orgid orgid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (format "User: %s is not authorized to delete EDTH trigger %s" (describe-item {:userid userid})  (describe-item {:etdhprofileid etdhprofileid}) ) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_update_etdh_trigger [jsmap]
  (let [userid (jsmap :user)
        etdhprofileid (get-in jsmap [:etdhprofileprops :etdhprofileid])
        siteid (get-in jsmap [:siteprops :siteid])
        orgid (get-in jsmap [:orgprops :orgid])
        query
        (spy :debug (render update_edth_trigger_acl_cypher {:userid userid :etdhprofileid etdhprofileid :siteid siteid :orgid orgid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (format "User: %s is not authorized to update EDTH trigger %s" (describe-item {:userid userid})  (describe-item {:etdhprofileid etdhprofileid}) ) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))



(defn acl_delete_schedule [jsmap]
  (let [userid (jsmap :user)
        scheduleid (get-in jsmap [:scheduleprops :scheduleid])
        query
        (spy :debug (render delete_schedule_acl_cypher {:userid userid :scheduleid scheduleid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (format "User: %s is not authorized to delete any schedule" (describe-item {:userid userid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_get_schedule [jsmap]
  (let [userid (jsmap :user)
        scheduleid (get-in jsmap [:scheduleprops :scheduleid])
        siteid (get-in jsmap [:siteprops :siteid])
        orgid (get-in jsmap [:orgprops :orgid])
        query
        (spy :debug (render get_schedule_acl_cypher {:userid userid :scheduleid scheduleid :siteid siteid :orgid orgid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (format "User: %s is not authorized to access schedule %s" (describe-item {:userid userid})  (describe-item {:scheduleid scheduleid}) ) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_create_schedule [jsmap]
  (let [userid (jsmap :user)
        siteid (get-in jsmap [:siteprops :siteid])
        query
        (spy :debug (render create_schedule_acl_cypher {:userid userid :siteid siteid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (format "User: %s is not authorized to create any schedule" (describe-item {:userid userid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_update_schedule [jsmap]
  (let [userid (jsmap :user)
        scheduleid (get-in jsmap [:scheduleprops :scheduleid])
        query
        (spy :debug (render update_schedule_acl_cypher {:userid userid  :scheduleid scheduleid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (format "User: %s is not authorized to update any schedule" (describe-item {:userid userid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_apply_schedule [jsmap]
  (let [userid (jsmap :user)
        siteid (get-in jsmap [:siteprops :siteid])
        scheduleid (get-in jsmap [:scheduleprops :scheduleid])
        groupid (get-in jsmap [:groupprops :groupid])
        ;nodeid (get-in jsmap [:nodeprops :nodeid])
        cypher (str apply_schedule_acl_cypher)
        query
        (spy :debug (render apply_schedule_acl_cypher {:userid userid  :siteid siteid :scheduleid scheduleid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (format "User: %s is not authorized to apply %s schedule" (describe-item {:userid userid}) (describe-item {:scheduleid scheduleid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_apply_etdh [jsmap]
  (let [userid (jsmap :user)
        siteid (get-in jsmap [:siteprops :siteid])
        etdhprofileid (get-in jsmap [:etdhprofileprops :etdhprofileid])
        groupids (spy :debug (get-in jsmap [:groupprops :groupids]))
        nodeid (get-in jsmap [:nodeprops :nodeid])
        cypher (spy :debug (if (nil? groupids) apply_etdh_acl_cypher apply_etdh_group_acl_cypher))
        props {"userid" userid "siteid" siteid "etdhprofileid" etdhprofileid "groupids" groupids "nodeid" nodeid }]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery cypher props))))]
        (if (= mycount 0)
            (cond
              (not-nil? groupids) (throw (ex-info (format "User: %s is not authorized to apply ETDH profile %s for site %s and given groups"
                                                          (describe-item {:userid userid})
                                                          (describe-item {:etdhprofileid etdhprofileid})
                                                          (describe-item {:siteid siteid}))
                                                  {:cause :not-authorized}))
              :else (throw (ex-info (format "User: %s is not authorized to apply ETDH profile %s for site %s"
                                            (describe-item {:userid userid})
                                            (describe-item {:etdhprofileid etdhprofileid})
                                            (describe-item {:siteid siteid}))
                                    {:cause :not-authorized})))))
      (catch clojure.lang.ExceptionInfo e
        (throw (prepare-exception e))))))

(defn acl_apply_dh [jsmap]
  (let [userid (jsmap :user)
        siteid (get-in jsmap [:siteprops :siteid])
        dhprofileid (get-in jsmap [:dhprofileprops :dhprofileid])
        groupids (spy :debug (get-in jsmap [:groupprops :groupids]))
        nodeid (get-in jsmap [:nodeprops :nodeid])
        cypher (spy :debug (if (nil? groupids) apply_dh_acl_cypher apply_dh_group_acl_cypher))
        props {"userid" userid "siteid" siteid "dhprofileid" dhprofileid "groupids" groupids "nodeid" nodeid }]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery cypher props))))]
        (if (= mycount 0)
            (cond
              (not-nil? groupids) (throw (ex-info (format "User: %s is not authorized to apply DH profile %s for site %s and given groups" (describe-item {:userid userid}) (describe-item {:dhprofileid dhprofileid}) (describe-item {:siteid siteid}) ) {:cause :not-authorized}))
              :else (throw (ex-info (format "User: %s is not authorized to apply DH profile %s for site %s" (describe-item {:userid userid}) (describe-item {:dhprofileid dhprofileid}) (describe-item {:siteid siteid})) {:cause :not-authorized})))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e))))))

(defn acl_apply_pdprofile [jsmap]
  (let [userid (jsmap :user)
        siteid (get-in jsmap [:siteprops :siteid])
        pdprofileid (get-in jsmap [:pdprofileprops :pdprofileid])
        query
        (spy :debug (render apply_pdprofile_acl_cypher {:userid userid  :siteid siteid :pdprofileid pdprofileid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (format "User: %s is not authorized to apply PD profile %s for site %s" (describe-item {:userid userid}) (describe-item {:pdprofileid pdprofileid}) (describe-item {:siteid siteid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_assign_fixture [jsmap]
  (let [userid (jsmap :user)
        siteid (get-in jsmap [:siteprops :siteid])
        query
        (spy :debug (render assign_fixture_acl_cypher {:userid userid :siteid siteid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (format "User: %s is not authorized to assign a fixture" (describe-item {:userid userid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_can_manage_lights [jsmap]
  (let [userid (jsmap :user)
        siteid (get-in jsmap [:siteprops :siteid])
        query
        (spy :debug (render can_manage_light_control {:userid userid :siteid siteid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (format "User: %s is not authorized to manage lights for site %s" userid siteid) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_get_all_schedules [jsmap]
  (let [userid (jsmap :user)
        siteid (get-in jsmap [:siteprops :siteid])
        query
        (spy :debug (render get_all_schedules_acl_cypher {:userid userid :siteid siteid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to read any schedules for site " (describe-item {:siteid siteid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_delete_config [jsmap]
  (let [userid (jsmap :user)
        configid (get-in jsmap [:configprops :configid])
        siteid (get-in jsmap [:siteprops :siteid])
        query
        (spy :debug (render delete_config_acl_cypher {:userid userid :configid configid :siteid siteid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (format "User: %s is not authorized to delete config %s" (describe-item {:userid userid}) (describe-item {:configid configid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_get_default_config [jsmap]
  (let [userid (jsmap :user)
        siteid (get-in jsmap [:siteprops :siteid])
        query
        (spy :debug (render get_default_config_acl_cypher {:userid userid :siteid siteid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
            (throw (ex-info (format "User: %s is not authorized to access default config for site %s" (describe-item {:userid userid}) (describe-item {:siteid siteid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_get_config [jsmap]
  (let [userid (jsmap :user)
        siteid (get-in jsmap [:siteprops :siteid])
        query
        (spy :debug (render get_config_acl_cypher {:userid userid :siteid siteid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (format "User: %s is not authorized to access config" (describe-item {:userid userid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_create_config [jsmap]
  (let [userid (jsmap :user)
        siteid (get-in jsmap [:siteprops :siteid])
        query
        (spy :debug (render create_config_acl_cypher {:userid userid :siteid siteid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (format "User: %s is not authorized to create config %s for site %s" (describe-item {:userid userid}) (describe-item {:siteid siteid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_update_config [jsmap]
  (let [userid (jsmap :user)
        configid (get-in jsmap [:configprops :configid])
        siteid (get-in jsmap [:siteprops :siteid])
        query
        (spy :debug (render update_config_acl_cypher {:userid userid :configid configid :siteid siteid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (format "User: %s is not authorized to update config %s" (describe-item {:userid userid}) (describe-item {:configid configid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_apply_config [jsmap]
  (let [userid (jsmap :user)
        siteid (get-in jsmap [:siteprops :siteid])
        query
        (spy :debug (render apply_config_acl_cypher {:userid userid  :siteid siteid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (format "User: %s is not authorized to apply any config to site %s" (describe-item {:userid userid}) (describe-item {:siteid siteid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))


(defn acl_assign_users_to_parkinggroup [jsmap]
  (let [userid (jsmap :user)
        users (get-in jsmap [:userIdToLinkprops :userIdsToLink])
        pgid (get-in jsmap [:parkingGroupprops :parkingGroupId])
        siteid (get-in jsmap [:siteprops :siteid])
        query
        (render assign_users_to_parkinggroup_acl_cypher {:userid userid :siteid siteid :parkinggroupid pgid})]

    (debug query)
    (try
      (let [mycount (count (json/read-str (neo4j/executeQuery query)))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to assing a node " (describe-item {:userIdsToLink users})" to parking group " (describe-item {:parkinggroupid pgid}) " at site " (describe-item {:siteid siteid})) {:cause :not-authorized} ))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))



(defn acl_unassign_users_from_parkinggroup [jsmap]
  (let [userid (jsmap :user)
        users (get-in jsmap [:userIdToLinkprops :userIdsToLink])
        pgid (get-in jsmap [:parkingGroupprops :parkingGroupId])
        siteid (get-in jsmap [:siteprops :siteid])
        query
        (render unassign_users_from_parkinggroup_acl_cypher {:userid userid :siteid siteid :parkinggroupid pgid})]

    (debug query)
    (try
      (let [mycount (count (json/read-str (neo4j/executeQuery query)))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to assing a node " (describe-item {:userIdsToLink users})" to parking group " (describe-item {:parkinggroupid pgid}) " at site " (describe-item {:siteid siteid})) {:cause :not-authorized} ))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))




(defn acl_create_metadata_for_parkingspot [jsmap]
  (let [userid (jsmap :user)
        siteid (get-in jsmap [:siteprops :siteid])
        query
        (render create_metadata_for_parkingspot_acl_cypher {:userid userid :siteid siteid})]

    (debug query)
    (try
      (let [mycount (count (json/read-str (neo4j/executeQuery query)))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to create Parking Spot Metadata at site " (describe-item {:siteid siteid})) {:cause :not-authorized} ))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))


(defn acl_delete_metadata_for_parkingspot [jsmap]
  (let [userid (jsmap :user)
        siteid (get-in jsmap [:siteprops :siteid])
        query
        (render delete_metadata_for_parkingspot_acl_cypher {:userid userid :siteid siteid})]

    (debug query)
    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to delete Parking Spot Metadata at site " (describe-item {:siteid siteid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_get_all_metadata_for_parkingspot [jsmap]
  (let [userid (jsmap :user)
        siteid (get-in jsmap [:siteprops :siteid])
        query
        (render get_all_metadata_for_parkingspot_acl_cypher {:userid userid :siteid siteid})]

    (debug query)
    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to get all metadata Parking Spot Metadata at site " (describe-item {:siteid siteid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))


(defn acl_get_metadata_for_parkingspot [jsmap]
  (let [userid (jsmap :user)
        siteid (get-in jsmap [:siteprops :siteid])
        query
        (render get_metadata_for_parkingspot_acl_cypher {:userid userid :siteid siteid})]

    (debug query)
    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to get a metadata Parking Spot Metadata at site " (describe-item {:siteid siteid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))


(defn acl_update_metadata_for_parkingspot [jsmap]
  (let [userid (jsmap :user)
        siteid (get-in jsmap [:siteprops :siteid])
        query
        (render update_metadata_for_parkingspot_acl_cypher {:userid userid :siteid siteid})]

    (debug query)
    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to update metadata Parking Spot Metadata at site " (describe-item {:siteid siteid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))


(defn acl_create_app_user_data [jsmap]
  (let [userid (jsmap :user)
        query
        (render create_app_user_data_acl_cypher {:userid userid})]

    (debug query)
    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to Create APP User Data " ) {:cause :not-authorized} ))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_delete_app_user_data [jsmap]
  (let [userid (jsmap :user)
        query
        (render delete_app_user_data_acl_cypher {:userid userid})]

    (debug query)
    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to Delete APP User Data " ) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))


(defn acl_get_all_app_user_data [jsmap]
  (let [userid (jsmap :user)
        query
        (render get_all_app_user_data_acl_cypher {:userid userid})]

    (debug query)
    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to Get All APP User Data " ) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_get_app_user_data [jsmap]
  (let [userid (jsmap :user)
        query
        (render get_app_user_data_acl_cypher {:userid userid})]

    (debug query)
    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to Get APP User Data ") {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_update_app_user_data [jsmap]
  (let [userid (jsmap :user)
        query
        (render update_app_user_data_acl_cypher {:userid userid})]

    (debug query)
    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to update APP User Data ") {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_create_parking_policy [jsmap]
  (let [userid (jsmap :user)
        siteid (get-in jsmap [:siteprops :siteid])
        query
        (render create_parking_policy_acl_cypher {:userid userid :siteid siteid})]

    (debug query)
    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to Create Parking policy at site " (describe-item {:siteid siteid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))


(defn acl_delete_parking_policy [jsmap]
  (let [userid (jsmap :user)
        siteid (get-in jsmap [:siteprops :siteid])
        query
        (render delete_parking_policy_acl_cypher {:userid userid :siteid siteid})]

    (debug query)
    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to Delete Parking policy at site " (describe-item {:siteid siteid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))


(defn acl_get_all_parking_policy [jsmap]
  (let [userid (jsmap :user)
        siteid (get-in jsmap [:siteprops :siteid])
        query
        (render get_all_parking_policy_acl_cypher {:userid userid :siteid siteid})]

    (debug query)
    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to Read all  Parking policies at site " (describe-item {:siteid siteid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))


(defn acl_get_parking_policy [jsmap]
  (let [userid (jsmap :user)
        siteid (get-in jsmap [:siteprops :siteid])
        query
        (render get_parking_policy_acl_cypher {:userid userid :siteid siteid})]

    (debug query)
    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to Read Parking policy at site " (describe-item {:siteid siteid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_update_parking_policy [jsmap]
  (let [userid (jsmap :user)
        siteid (get-in jsmap [:siteprops :siteid])
        query
        (render update_parking_policy_acl_cypher {:userid userid :siteid siteid})]

    (debug query)
    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to update Parking policy at site " (describe-item {:siteid siteid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_get_parking_policy_version [jsmap]
  (let [userid (jsmap :user)
        siteid (get-in jsmap [:siteprops :siteid])
        query
        (render get_parking_policy_version_acl_cypher {:userid userid :siteid siteid})]

    (debug query)
    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to Read Parking policy of specefic version at site " (describe-item {:siteid siteid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_get_all_versions_of_parking_policy [jsmap]
  (let [userid (jsmap :user)
        siteid (get-in jsmap [:siteprops :siteid])
        query
        (render get_all_versions_of_parking_policy_acl_cypher {:userid userid :siteid siteid})]

    (debug query)
    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to Read all versions of Parking policies at site " (describe-item {:siteid siteid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_get_all_active_parking_policy_for_period [jsmap]
  (let [userid (jsmap :user)
        siteid (get-in jsmap [:siteprops :siteid])
        query
        (render get_all_active_parking_policy_for_period_acl_cypher {:userid userid :siteid siteid})]

    (debug query)
    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to Read all active Parking policies for a given period at site " (describe-item {:siteid siteid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_get_active_parking_policy [jsmap]
  (let [userid (jsmap :user)
        siteid (get-in jsmap [:siteprops :siteid])
        query
        (render get_active_parking_policy_acl_cypher {:userid userid :siteid siteid})]

    (debug query)
    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to Read active Parking policies at site " (describe-item {:siteid siteid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))


(defn acl_search_parking_policy [jsmap]
  (let [userid (jsmap :user)
        siteid (get-in jsmap [:siteprops :siteid])
        query
        (render search_parking_policy_acl_cypher {:userid userid :siteid siteid})]

    (debug query)
    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to search forany Parking policy at site " (describe-item {:siteid siteid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))



(defn acl_create_policy_category [jsmap]
  (let [userid (jsmap :user)
        siteid (get-in jsmap [:siteprops :siteid])
        query
        (render create_policy_category_acl_cypher {:userid userid :siteid siteid})]

    (debug query)
    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to create policy category at site " (describe-item {:siteid siteid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))


(defn acl_delete_policy_category [jsmap]
  (let [userid (jsmap :user)
        siteid (get-in jsmap [:siteprops :siteid])
        query
        (render delete_policy_category_acl_cypher {:userid userid :siteid siteid})]

    (debug query)
    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to delete policy category at site " (describe-item {:siteid siteid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_get_all_policy_category [jsmap]
  (let [userid (jsmap :user)
        siteid (get-in jsmap [:siteprops :siteid])
        query
        (render get_all_policy_category_acl_cypher {:userid userid :siteid siteid})]

    (debug query)
    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to get all policy categories at site " (describe-item {:siteid siteid})) {:cause :not-authorized} ))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))


(defn acl_get_policy_category [jsmap]
  (let [userid (jsmap :user)
        siteid (get-in jsmap [:siteprops :siteid])
        query
        (render get_policy_category_acl_cypher {:userid userid :siteid siteid})]

    (debug query)
    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to get policy category at site " (describe-item {:siteid siteid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_update_policy_category [jsmap]
  (let [userid (jsmap :user)
        siteid (get-in jsmap [:siteprops :siteid])
        query
        (render update_policy_category_acl_cypher {:userid userid :siteid siteid})]

    (debug query)
    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to update policy category at site " (describe-item {:siteid siteid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))


(defn acl_policy_association [jsmap]
  (let [userid (jsmap :user)
        pgid (get-in jsmap [:configprops :ParkingGroupPolicyLink :parkinggroupid])
        siteid (get-in jsmap [:siteprops :siteid])
        query
        (render policy_association_acl_cypher {:userid userid :siteid siteid :parkinggroupid pgid})]

    (debug query)
    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " who might be linked to " (describe-item {:parkinggroupid pgid}) " is not authorized to associate a policy to a parking group at site " (describe-item {:siteid siteid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_policy_disassociation [jsmap]
  (let [userid (jsmap :user)
        pgid (get-in jsmap [:configprops :ParkingGroupPolicyLink :parkinggroupid])
        siteid (get-in jsmap [:siteprops :siteid])
        query
        (render policy_disassociation_acl_cypher {:userid userid :siteid siteid :parkinggroupid pgid})]

    (debug query)
    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " who might be linked to " (describe-item {:parkinggroupid pgid}) " is not authorized to disassociate a policy to a parking group at site " (describe-item {:siteid siteid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))


(defn acl_associated_parking_groups [jsmap]
      (let [userid (jsmap :user)
            siteid (get-in jsmap [:siteprops :siteid])
            query
            (render associated_parking_groups_acl_cypher {:userid userid :siteid siteid})]

           (debug query)
           (try
             (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
                  (if (= mycount 0)
                    (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to Get all associated parking groups for given policy at site " (describe-item {:siteid siteid})) {:cause :not-authorized}))))
             (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
           jsmap))


(defn acl_policy_tag_association [jsmap]
      (let [userid (jsmap :user)
            siteid (get-in jsmap [:siteprops :siteid])
            query
            (render policy_tag_association_acl_cypher {:userid userid :siteid siteid})]

           (debug query)
           (try
             (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
                  (if (= mycount 0)
                    (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to associate tag to a policy at site " (describe-item {:siteid siteid})) {:cause :not-authorized}))))
             (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
           jsmap))


(defn acl_policy_tag_disassociation [jsmap]
      (let [userid (jsmap :user)
            siteid (get-in jsmap [:siteprops :siteid])
            query
            (render policy_tag_disassociation_acl_cypher {:userid userid :siteid siteid})]

           (debug query)
           (try
             (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
                  (if (= mycount 0)
                    (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to disassociate tag to a policy at site " (describe-item {:siteid siteid})) {:cause :not-authorized}))))
             (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
           jsmap))

(defn acl_get_business_alert [jsmap]
      (let [userid (jsmap :user)
            siteid (get-in jsmap [:siteprops :siteid])
            orgid (get-in jsmap [:orgprops :orgid])
            query
            (spy :debug (render get_business_alert_acl_cypher {:userid userid :siteid siteid :orgid orgid}))]

           (try
             (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
                  (if (= mycount 0)
                    (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to get business alert for site " (describe-item {:siteid siteid}) " and org " (describe-item {:orgid orgid})) {:cause :not-authorized}))))
             (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
           jsmap))


(defn acl_create_business_alert [jsmap]
      (let [userid (jsmap :user)
            siteid (get-in jsmap [:siteprops :siteid])
            orgid (get-in jsmap [:orgprops :orgid])
            query
            (spy :debug (render create_business_alert_acl_cypher {:userid userid :siteid siteid :orgid orgid}))]

           (try
             (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
                  (if (= mycount 0)
                    (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to create business alert for site " (describe-item {:siteid siteid}) " and org " (describe-item {:orgid orgid})) {:cause :not-authorized}))))
             (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
           jsmap))


(defn acl_update_business_alert [jsmap]
      (let [userid (jsmap :user)
            siteid (get-in jsmap [:siteprops :siteid])
            orgid (get-in jsmap [:orgprops :orgid])
            query
            (spy :debug (render update_business_alert_acl_cypher {:userid userid :siteid siteid :orgid orgid}))]

           (try
             (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
                  (if (= mycount 0)
                    (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to update business alert for site " (describe-item {:siteid siteid}) " and org " (describe-item {:orgid orgid})) {:cause :not-authorized}))))
             (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
           jsmap))


(defn acl_delete_business_alert [jsmap]
      (let [userid (jsmap :user)
            siteid (get-in jsmap [:siteprops :siteid])
            orgid (get-in jsmap [:orgprops :orgid])
            query
            (spy :debug (render delete_business_alert_acl_cypher {:userid userid :siteid siteid :orgid orgid}))]

           (try
             (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
                  (if (= mycount 0)
                    (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to delete business alert for site " (describe-item {:siteid siteid}) " and org " (describe-item {:orgid orgid})) {:cause :not-authorized}))))
             (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
           jsmap))


(defn acl_get_trigger [jsmap]
      (let [userid (jsmap :user)
            siteid (get-in jsmap [:siteprops :siteid])
            orgid (get-in jsmap [:orgprops :orgid])
            query
            (spy :debug (render get_trigger_acl_cypher {:userid userid :siteid siteid :orgid orgid}))]

           (try
             (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
                  (if (= mycount 0)
                    (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to get trigger for site " (describe-item {:siteid siteid}) " and org " (describe-item {:orgid orgid})) {:cause :not-authorized}))))
             (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
           jsmap))


(defn acl_create_trigger [jsmap]
      (let [userid (jsmap :user)
            siteid (get-in jsmap [:siteprops :siteid])
            orgid (get-in jsmap [:orgprops :orgid])
            query
            (spy :debug (render create_trigger_acl_cypher {:userid userid :siteid siteid :orgid orgid}))]

           (try
             (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
                  (if (= mycount 0)
                    (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to create trigger for site " (describe-item {:siteid siteid}) " and org " (describe-item {:orgid orgid})) {:cause :not-authorized}))))
             (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
           jsmap))


(defn acl_update_trigger [jsmap]
      (let [userid (jsmap :user)
            siteid (get-in jsmap [:siteprops :siteid])
            orgid (get-in jsmap [:orgprops :orgid])
            query
            (spy :debug (render update_trigger_acl_cypher {:userid userid :siteid siteid :orgid orgid}))]

           (try
             (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
                  (if (= mycount 0)
                    (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to update trigger for site " (describe-item {:siteid siteid}) " and org " (describe-item {:orgid orgid})) {:cause :not-authorized}))))
             (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
           jsmap))


(defn acl_delete_trigger [jsmap]
      (let [userid (jsmap :user)
            siteid (get-in jsmap [:siteprops :siteid])
            orgid (get-in jsmap [:orgprops :orgid])
            query
            (spy :debug (render delete_trigger_acl_cypher {:userid userid :siteid siteid :orgid orgid}))]

           (try
             (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
                  (if (= mycount 0)
                    (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to delete trigger for site " (describe-item {:siteid siteid}) " and org " (describe-item {:orgid orgid})) {:cause :not-authorized}))))
             (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
           jsmap))

(defn acl_create_whatif_job [jsmap]
      (let [userid (jsmap :user)
            siteid (get-in jsmap [:siteprops :siteid])
            orgid (get-in jsmap [:orgprops :orgid])
            query
            (spy :debug (render create_whatif_job_acl_cypher {:userid userid :siteid siteid :orgid orgid}))]

           (try
             (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
                  (if (= mycount 0)
                    (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to create what if project for site " (describe-item {:siteid siteid}) " and org " (describe-item {:orgid orgid})) {:cause :not-authorized}))))
             (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
           jsmap))


(defn acl_update_whatif_job [jsmap]
      (let [userid (jsmap :user)
            siteid (get-in jsmap [:siteprops :siteid])
            orgid (get-in jsmap [:orgprops :orgid])
            query
            (spy :debug (render update_whatif_job_acl_cypher {:userid userid :siteid siteid :orgid orgid}))]

           (try
             (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
                  (if (= mycount 0)
                    (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to update/abort what if project for site " (describe-item {:siteid siteid}) " and org " (describe-item {:orgid orgid})) {:cause :not-authorized}))))
             (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
           jsmap))

(defn acl_get_whatif_job [jsmap]
      (let [userid (jsmap :user)
            siteid (get-in jsmap [:siteprops :siteid])
            orgid (get-in jsmap [:orgprops :orgid])
            query
            (spy :debug (render get_whatif_job_acl_cypher {:userid userid :siteid siteid :orgid orgid}))]

           (try
             (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
                  (if (= mycount 0)
                    (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to get whatif project for site " (describe-item {:siteid siteid}) " and org " (describe-item {:orgid orgid})) {:cause :not-authorized}))))
             (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
           jsmap))

(defn acl_delete_whatif_job [jsmap]
      (let [userid (jsmap :user)
            siteid (get-in jsmap [:siteprops :siteid])
            orgid (get-in jsmap [:orgprops :orgid])
            query
            (spy :debug (render delete_whatif_job_acl_cypher {:userid userid :siteid siteid :orgid orgid}))]

           (try
             (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
                  (if (= mycount 0)
                    (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to delete whatif project for site " (describe-item {:siteid siteid}) " and org " (describe-item {:orgid orgid})) {:cause :not-authorized}))))
             (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
           jsmap))

(defn acl_get_whatif_policy [jsmap]
      (let [userid (jsmap :user)
            siteid (get-in jsmap [:siteprops :siteid])
            orgid (get-in jsmap [:orgprops :orgid])
            query
            (spy :debug (render get_whatif_job_policy_cypher {:userid userid :siteid siteid :orgid orgid}))]

           (try
             (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
                  (if (= mycount 0)
                    (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to get whatif policy for site " (describe-item {:siteid siteid}) " and org " (describe-item {:orgid orgid})) {:cause :not-authorized}))))
             (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
           jsmap))


(defn acl_create_whatif_policy [jsmap]
      (let [userid (jsmap :user)
            siteid (get-in jsmap [:siteprops :siteid])
            orgid (get-in jsmap [:orgprops :orgid])
            query
            (spy :debug (render create_whatif_policy_acl_cypher {:userid userid :siteid siteid :orgid orgid}))]

           (try
             (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
                  (if (= mycount 0)
                    (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to create whatif policy for site " (describe-item {:siteid siteid}) " and org " (describe-item {:orgid orgid})) {:cause :not-authorized}))))
             (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
           jsmap))


(defn acl_update_whatif_policy [jsmap]
      (let [userid (jsmap :user)
            siteid (get-in jsmap [:siteprops :siteid])
            orgid (get-in jsmap [:orgprops :orgid])
            query
            (spy :debug (render update_whatif_policy_acl_cypher {:userid userid :siteid siteid :orgid orgid}))]

           (try
             (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
                  (if (= mycount 0)
                    (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to update whatif policy for site " (describe-item {:siteid siteid}) " and org " (describe-item {:orgid orgid})) {:cause :not-authorized}))))
             (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
           jsmap))


(defn acl_delete_whatif_policy [jsmap]
      (let [userid (jsmap :user)
            siteid (get-in jsmap [:siteprops :siteid])
            orgid (get-in jsmap [:orgprops :orgid])
            query
            (spy :debug (render delete_whatif_policy_acl_cypher {:userid userid :siteid siteid :orgid orgid}))]

           (try
             (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
                  (if (= mycount 0)
                    (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to delete whatif policy for site " (describe-item {:siteid siteid}) " and org " (describe-item {:orgid orgid})) {:cause :not-authorized}))))
             (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
           jsmap))

(defn acl_get_whatif_tag [jsmap]
      (let [userid (jsmap :user)
            siteid (get-in jsmap [:siteprops :siteid])
            orgid (get-in jsmap [:orgprops :orgid])
            query
            (spy :debug (render get_whatif_job_tag_cypher {:userid userid :siteid siteid :orgid orgid}))]

           (try
             (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
                  (if (= mycount 0)
                    (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to get whatif tag for site " (describe-item {:siteid siteid}) " and org " (describe-item {:orgid orgid})) {:cause :not-authorized}))))
             (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
           jsmap))


(defn acl_create_whatif_tag [jsmap]
      (let [userid (jsmap :user)
            siteid (get-in jsmap [:siteprops :siteid])
            orgid (get-in jsmap [:orgprops :orgid])
            query
            (spy :debug (render create_whatif_tag_acl_cypher {:userid userid :siteid siteid :orgid orgid}))]

           (try
             (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
                  (if (= mycount 0)
                    (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to create tag for site " (describe-item {:siteid siteid}) " and org " (describe-item {:orgid orgid})) {:cause :not-authorized}))))
             (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
           jsmap))


(defn acl_update_whatif_tag [jsmap]
      (let [userid (jsmap :user)
            siteid (get-in jsmap [:siteprops :siteid])
            orgid (get-in jsmap [:orgprops :orgid])
            query
            (spy :debug (render update_whatif_tag_acl_cypher {:userid userid :siteid siteid :orgid orgid}))]

           (try
             (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
                  (if (= mycount 0)
                    (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to update tag for site " (describe-item {:siteid siteid}) " and org " (describe-item {:orgid orgid})) {:cause :not-authorized}))))
             (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
           jsmap))


(defn acl_delete_whatif_tag [jsmap]
      (let [userid (jsmap :user)
            siteid (get-in jsmap [:siteprops :siteid])
            orgid (get-in jsmap [:orgprops :orgid])
            query
            (spy :debug (render delete_whatif_tag_acl_cypher {:userid userid :siteid siteid :orgid orgid}))]

           (try
             (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
                  (if (= mycount 0)
                    (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to delete tag for site " (describe-item {:siteid siteid}) " and org " (describe-item {:orgid orgid})) {:cause :not-authorized}))))
             (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
           jsmap))

(defn acl_get_gps [jsmap]
  (let [userid (jsmap :user)
        siteid (get-in jsmap [:siteprops :siteid])
        orgid (get-in jsmap [:orgprops :orgid])
        query
        (spy :debug (render get_gps_acl_cypher {:userid userid :siteid siteid :orgid orgid}))]

    (try
      (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
        (if (= mycount 0)
          (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to get gps for site " (describe-item {:siteid siteid}) " and org " (describe-item {:orgid orgid})) {:cause :not-authorized}))))
      (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
    jsmap))

(defn acl_get_uf_alarm [jsmap]
      (let [userid (jsmap :user)
            query
            (spy :debug (render get_uf_alarms_acl_cypher {:userid userid}))]

           (try
             (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
                  (if (= mycount 0)
                    (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to read any Alarm mappings") {:cause :not-authorized}))))
             (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
           jsmap))


(defn acl_create_uf_alarm [jsmap]
      (let [userid (jsmap :user)
            query
            (spy :debug (render create_uf_alarms_acl_cypher {:userid userid}))]

           (try
             (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
                  (if (= mycount 0)
                    (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to create any User Friendly Alarms") {:cause :not-authorized}))))
             (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
           jsmap))


(defn acl_update_uf_alarm [jsmap]
      (let [userid (jsmap :user)
            query
            (spy :debug (render update_uf_alarms_acl_cypher {:userid userid}))]

           (try
             (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
                  (if (= mycount 0)
                    (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to update any User Friendly Alarms") {:cause :not-authorized}))))
             (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
           jsmap))


(defn acl_delete_uf_alarm [jsmap]
      (let [userid (jsmap :user)
            query
            (spy :debug (render delete_uf_alarms_acl_cypher {:userid userid}))]

           (try
             (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
                  (if (= mycount 0)
                    (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to delete any User Friendly Alarms") {:cause :not-authorized}))))
             (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
           jsmap))


(defn acl_get_notification_types [jsmap]
      (let [userid (jsmap :user)
            orgid (get-in jsmap [:orgprops :orgid])
            query
            (spy :debug (render get_notification_types_acl_cypher {:userid userid :orgid orgid}))]

           (try
             (let [mycount (count (spy :debug (json/read-str (neo4j/executeQuery query))))]
                  (if (= mycount 0)
                    (throw (ex-info (str "User: " (describe-item {:userid userid}) " is not authorized to fetch Notification types for org " (describe-item {:orgid orgid})) {:cause :not-authorized}))))
             (catch clojure.lang.ExceptionInfo e ( throw (prepare-exception e) )))
           jsmap))

(defn actrl [{action :type
              :as jsmap}]
  (case action
    ("assignFirmwareToSite"
     "assignFirmwareToGroup"
     "assignFirmwareToNode") (do (acl_assign_firmware jsmap) jsmap)
    ("lighting-control"
     "lighting-control-site"
     "lighting-control-group") (do (acl_can_manage_lights jsmap) jsmap)
    "autoComplete" jsmap ;; TODO: Do a proper access control

    "getSite" (do (acl_get_site jsmap) jsmap)
    "getNode" (do (acl_get_node jsmap) jsmap)
    "getSensorHistory" (do(acl_get_sensors jsmap) jsmap)
    "getSensorHistoryFromTo" (do(acl_get_sensors jsmap) jsmap)
    "getNodeConnectionStatus" (do(acl_get_node jsmap) jsmap)
    "getSiteNodesStatuses" (do(acl_get_nodes4site jsmap) jsmap)
    "getLostAndFoundNodesStatuses" (do (acl_get_lost_found_nodes jsmap) jsmap)
    "getNodeLightStatus" (do(acl_get_node jsmap) jsmap)
    "getParkingInfoSite" (do(acl_get_parkinginfo jsmap) jsmap)
    "getCurrentTrafficInfo" (do(acl_get_trafficinfo jsmap) jsmap)
    "getTrafficHistory" (do(acl_get_trafficinfo jsmap) jsmap)
    "getTrafficConfig" (do(acl_get_trafficinfo jsmap) jsmap)
    "getOverlay"(do (acl_get_overlay jsmap) jsmap)
    "getGroup"(do (acl_get_group jsmap) jsmap)
    "getFixture"(do (acl_get_fixture jsmap) jsmap)
    "getNotification"(do (acl_get_notification jsmap) jsmap)
    "getAlert"(do (acl_get_alert jsmap) jsmap)
    "getParkingZones"(do (acl_get_parkingzone jsmap) jsmap)
    "getOneParkingZone"(do (acl_get_parkingzone jsmap) jsmap)
    "getParkingSpot"(do (acl_get_parkingspot jsmap) jsmap)
    "getParkingGroup"(do (acl_get_parkinggroup jsmap) jsmap)
    "getAlertByNodeNameType" jsmap
    "getAlertSys" jsmap
    "getNotificationSys" jsmap
    "getAllOrgs" jsmap
    "getAllSuspendedOrgs" jsmap
    "getAllSitesForOrg" (do (acl_get_sites4org jsmap))
    "getAllSuspendedSitesForOrg" (do (acl_get_suspendedsites4org jsmap))
    "getAllNodesForSite" (do (acl_get_nodes4site jsmap) jsmap)
    "getAllNodeIdsForModelGroup" (do (acl_get_nodes4site jsmap) jsmap)
    "getAllNodeIdsForModelSite" (do (acl_get_nodes4site jsmap) jsmap)
    "getAllMinNodesForSite" (do (acl_get_nodes4site jsmap) jsmap)
    "getAllLostAndFoundNodes" (do (acl_get_lost_found_nodes jsmap) jsmap)

    "getUser" (case (get-in jsmap [:userprops :type] "default")
                "partner" (do (acl_get_partner_user jsmap) jsmap)
                "default" (do (acl_get_org_user jsmap) jsmap)
                (do (acl_get_org_user jsmap) jsmap))
    "createUser" (case (get-in jsmap [:userprops :type] "default")
                   "partner" (do (acl_create_partner_user jsmap) jsmap)
                   "default" (do (acl_create_org_user jsmap) jsmap)
                   (do (acl_create_org_user jsmap) jsmap))
    "updateUser" (case (get-in jsmap [:userprops :type] "default")
                   "partner" (do (acl_update_partner_user jsmap) jsmap)
                   "default" (do (acl_update_org_user jsmap) jsmap)
                   (do (acl_update_org_user jsmap) jsmap))
    "deleteUser" (case (get-in jsmap [:userprops :type] "default")
                   "partner" (do (acl_delete_partner_user jsmap) jsmap)
                   "default" (do (acl_delete_org_user jsmap) jsmap)
                   (do (acl_delete_org_user jsmap) jsmap))
    "suspendUser" (case (get-in jsmap [:userprops :type] "default")
                    "partner" (do (acl_delete_partner_user jsmap) jsmap)
                    "default" (do (acl_delete_org_user jsmap) jsmap)
                    (do (acl_delete_org_user jsmap) jsmap))
    "activateUser" (case (get-in jsmap [:userprops :type] "default")
                     "partner" (do (acl_delete_partner_user jsmap) jsmap)
                     "default" (do (acl_delete_org_user jsmap) jsmap)
                     (do (acl_delete_org_user jsmap) jsmap))
    ;;"createUser" (do (acl_create_user jsmap) jsmap)
    ;;"updateUser" (do (acl_update_user jsmap) jsmap)
    ;;"deleteUser" (do (acl_delete_user jsmap) jsmap)
    "getAllUsersForOrg" (do (acl_get_users4org jsmap))
    "getAllUsersForSite" (do (acl_get_users4site jsmap))
    "getAllUsersForPartner" (do (acl_get_users4partner jsmap))
    "getAllSuspendedUsersForOrg" (do (acl_get_suspended_users4org jsmap))
    "getAllSuspendedUsersForPartner" (do (acl_get_suspended_users4partner jsmap))
    "getUserEmail" jsmap ;; Used for notifications

    "getOrg" (case (get-in jsmap [:orgprops :type] "default")
               "partner" (do (acl_get_partner jsmap) jsmap)
               "default" (do (acl_get_org jsmap) jsmap)
               (do (acl_get_org jsmap) jsmap))
    "createOrg" (case (get-in jsmap [:orgprops :type] "default")
                  "partner" (do (acl_create_partner jsmap) jsmap)
                  "default" (do (acl_create_org jsmap) jsmap)
                  (do (acl_create_org jsmap) jsmap))
    "updateOrg" (case (get-in jsmap [:orgprops :type] "default")
                  "partner" (do (acl_update_partner jsmap) jsmap)
                  "default" (do (acl_update_org jsmap) jsmap)
                  (do (acl_update_org jsmap) jsmap))
    "deleteOrg" (case (get-in jsmap [:orgprops :type] "")
                  "partner" (do (acl_delete_partner jsmap) jsmap)
                  "default" (do (acl_delete_org jsmap) jsmap)
                  (do (acl_delete_org_partner jsmap) jsmap))
    "suspendOrg" (case (get-in jsmap [:orgprops :type] "")
                   "partner" (do (acl_suspend_partner jsmap) jsmap)
                   "default" (do (acl_suspend_org jsmap) jsmap)
                   (do (acl_suspend_org_partner jsmap) jsmap))
    "activateOrg" (case (get-in jsmap [:orgprops :type] "default")
                    "partner" (do (acl_activate_partner jsmap) jsmap)
                    "default" (do (acl_activate_org jsmap) jsmap)
                    (do (acl_activate_org jsmap) jsmap))
    "createSite" (do (acl_create_site jsmap) jsmap)
    "createNode" (do (acl_create_node jsmap) jsmap)
    "assignNode" (do (acl_assign_node (merge jsmap {:soid @get-sensity-org-id})) jsmap)
    "assignNodeToParkingGroup" (do (acl_assign_parkinggroup jsmap) jsmap)
    "updateNode" (do (acl_update_node jsmap) jsmap)
    "activateNode" (do (acl_activate_node jsmap) jsmap)
    "deactivateNode" (do (acl_deactivate_node jsmap) jsmap)
    "commandNode" (do (acl_assign_firmware jsmap) jsmap)
    ("createEmptyNode"
     "createBulkNode") (do (acl_create_empty_node jsmap) jsmap)
    "createOverlay" (do (acl_create_overlay jsmap) jsmap)
    "createGroup" (do (acl_create_group jsmap) jsmap)
    "createFixture" (do (acl_create_fixture jsmap) jsmap)
    "getAllPermissions" jsmap
    "createNotification" (do (acl_create_notification jsmap) jsmap)
    "createAlert" (do (acl_create_alert jsmap) jsmap)
    "createParkingZone" (do (acl_create_parkingzone jsmap) jsmap)
    "createParkingSpot" (do (acl_create_parkingspot jsmap) jsmap)
    "createParkingGroup" (do (acl_create_parkinggroup jsmap) jsmap)
    "getUserPermissions" jsmap
    "getAllOverlays" (do (acl_get_all_overlays jsmap) jsmap)
    "getAllGroups" (do (acl_get_all_groups jsmap) jsmap)
    "getAllFixtures" (do (acl_get_all_fixtures jsmap) jsmap)
    "getAllNotificationsForSite" (do (acl_get_all_notifications_for_site jsmap) jsmap)
    "getAllNotificationsForUser" (do (acl_get_all_notifications_for_user jsmap) jsmap)
    "getAllNotificationsForOrg" (do (acl_get_all_notifications_for_org jsmap) jsmap)
    "getAllNotificationsByName" jsmap
    "getAllAlerts" (do (acl_get_all_alerts jsmap) jsmap)
    "getAllOrgAlerts" (do (acl_get_all_org_alerts jsmap) jsmap)
    "getAllParkingZones" (do (acl_get_all_parkingzones jsmap) jsmap)
    "getAllParkingSpots" (do (acl_get_all_parkingspots jsmap) jsmap)
    "getAllParkingGroups" (do (acl_get_all_parkinggroups jsmap) jsmap)
    "getAlertsForNode" (do (acl_get_all_alerts_for_node jsmap) jsmap)
    "getActivityLogs" (do (acl_get_activity_logs jsmap) jsmap)
    "logActivity" jsmap
    "otaStatusForSite" (do (acl_get_ota_status jsmap) jsmap)
    "otaStatusForJob" (do (acl_get_ota_status jsmap) jsmap)
    "otaStatusJobUpdate" (do (acl_get_ota_status jsmap) jsmap)
    "updateOverlay" (do (acl_update_overlay jsmap) jsmap)
    "updateGroup" (do (acl_update_group jsmap) jsmap)
    "updateFixture" (do (acl_update_fixture jsmap) jsmap)
    ("addNodeToGroup"
     "addNodesToGroup") (do (acl_update_group jsmap) jsmap)
    ("resendScheduleToLG"
     "resendScheduleToNode") jsmap
    "removeNodeFromGroup" (do (acl_update_group jsmap) jsmap)
    "updateNotification" (do (acl_update_notification jsmap) jsmap)
    "activateNotification" (do (acl_activate_notification jsmap) jsmap)
    "deactivateNotification" (do (acl_deactivate_notification jsmap) jsmap)
    "updateAlert" (do (acl_update_alert jsmap) jsmap)
    "updateParkingZone" (do (acl_update_parkingzone jsmap) jsmap)
    "updateParkingSpot" (do (acl_update_parkingspot jsmap) jsmap)
    "updateParkingGroup" (do (acl_update_parkinggroup jsmap) jsmap)
    "updateSite" (do (acl_update_site jsmap) jsmap)
    "deleteGroup" (do (acl_delete_group jsmap) jsmap)
    "deleteFixture" (do (acl_delete_fixture jsmap) jsmap)
    "deleteNode" (do (acl_delete_node jsmap) jsmap)
    "realDeleteNode" (do (acl_real_delete_node jsmap) jsmap)
    "deleteNotification" (do (acl_delete_notification jsmap) jsmap)
    "deleteAlert" (do (acl_delete_alert jsmap) jsmap)
    "dismissAlert" (do (acl_delete_alert jsmap) jsmap)
    "deleteParkingZone" (do (acl_delete_parkingzone jsmap) jsmap)
    "deleteParkingGroup" (do (acl_delete_parkinggroup jsmap) jsmap)
    "deleteParkingSpot" (do (acl_delete_parkingspot jsmap) jsmap)
    "deleteOverlay" (do (acl_delete_overlay jsmap) jsmap)
    "createInstallPod" (do (acl_create_install_pod jsmap) jsmap)
    "deleteInstallPod" (do (acl_delete_install_pod jsmap) jsmap)
    "deleteSite" (do (acl_delete_site jsmap) jsmap)
    "suspendSite" (do (acl_suspend_site jsmap) jsmap)
    "activateSite" (do (acl_activate_site jsmap) jsmap)
    "getFirmware" (do (acl_get_firmware jsmap) jsmap)
    "createFirmware" (do (acl_create_firmware jsmap) jsmap)
    "updateFirmware" (do (acl_update_firmware jsmap) jsmap)
    "deleteFirmware" (do (acl_delete_firmware jsmap) jsmap)
    "getAllFirmwares" jsmap
    "getPDProfile" jsmap
    "getETDHProfile" jsmap
    "getDHProfile" jsmap
    "getSchedule" (do (acl_get_schedule jsmap) jsmap)
    "createPDProfile" jsmap
    "createETDHProfile" jsmap
    "createDHProfile" jsmap
    "createSchedule" (do (acl_create_schedule jsmap) jsmap)
    "updatePDProfile" jsmap
    "updateETDHProfile" jsmap
    "updateDHProfile" jsmap
    "updateSchedule" (do (acl_update_schedule jsmap) jsmap)
    "deletePDProfile" jsmap
    "deleteETDHProfile" jsmap
    "deleteDHProfile" jsmap
    "deleteSchedule" (do (acl_delete_schedule jsmap) jsmap)
    ("getAllPDProfiles"
     "getAllETDHProfiles"
     "getAllDHProfiles"
     "getAllSchedules") (do (acl_get_all_schedules jsmap) jsmap)
    "calibrateDHProfile" jsmap
    ("applyConfigToGroup"
     "applyConfigToSite"
     "applyConfigToNodes"
     "applyConfigToNode"
     "applyServerToNodes") (do (acl_apply_config jsmap) jsmap)

    ("applyPDtoGroup"
     "applyPDtoSite"
     "applyPDtoNodes") (do (acl_apply_pdprofile jsmap) jsmap)
    ("applyScheduleToGroup"
     "applyScheduleToSite"
     "applyScheduleToNode"
     "applyScheduleToNodes") (do (acl_apply_schedule jsmap) jsmap)
    ("applyETDHtoGroup"
     "applyETDHtoSite"
     "applyETDHtoNodes") (doto jsmap
                           acl_apply_etdh)
    ("applyDHtoGroup"
     "applyDHtoSite"
     "applyDHtoNodes") (do (acl_apply_dh jsmap) jsmap)
    "getAllETDHProfileTriggers" (do (acl_get_etdh_trigger jsmap) jsmap)
    "removeETDHProfileTriggers" (do (acl_delete_etdh_trigger jsmap) jsmap)
    "addETDHProfileTriggers" (do (acl_update_etdh_trigger jsmap) jsmap)
    ("getAllDHProfileTriggers"
     "removeDHProfileTrigger"
     "addDHProfileTrigger") jsmap
    ("assignFixtureToNode"
     "assignFixtureToNodes"
     "assignFixtureToSite"
     "assignFixtureToGroup") (do (acl_assign_fixture jsmap) jsmap)
    "getAllConfigs" jsmap
    "getDefaultConfigs" jsmap
    "getDefaultConfigsForSite" (do (acl_get_default_config jsmap) jsmap)
    "getConfigFromNode" (do (acl_get_config jsmap) jsmap)
    "getConfig" (do (acl_get_config jsmap) jsmap)
    "createConfig" (do (acl_create_config jsmap) jsmap)
    "updateConfig" (do (acl_update_config jsmap) jsmap)
    "deleteConfig" (do (acl_delete_config jsmap) jsmap)
    "updateNodeWifi" (do (acl_apply_config jsmap) jsmap)
    "getUserEmails" jsmap
    "bulkAssignUsersToParkingGroup" (do (acl_assign_users_to_parkinggroup jsmap) jsmap)
    "bulkUnassignUsersFromParkingGroup" (do (acl_unassign_users_from_parkinggroup jsmap) jsmap)
    "createMetadataForParkingSpot"  (do (acl_create_metadata_for_parkingspot jsmap) jsmap)
    "deleteMetadataForParkingSpot"  (do (acl_delete_metadata_for_parkingspot jsmap) jsmap)
    "getAllMetadataForParkingSpot"  (do (acl_get_all_metadata_for_parkingspot jsmap) jsmap)
    "getMetadataForParkingSpot"  (do (acl_get_metadata_for_parkingspot jsmap) jsmap)
    "updateMetadataForParkingSpot"  (do (acl_update_metadata_for_parkingspot jsmap) jsmap)
    "createAppUserData" (do (acl_create_app_user_data jsmap) jsmap)
    "deleteAppUserData" (do (acl_delete_app_user_data jsmap) jsmap)
    "getAllAppUserData" (do (acl_get_all_app_user_data jsmap) jsmap)
    "getAppUserData" (do (acl_get_app_user_data jsmap) jsmap)
    "updateAppUserData" (do (acl_update_app_user_data jsmap) jsmap)
    "createParkingPolicy"  (do (acl_create_parking_policy jsmap) jsmap)
    "deleteParkingPolicy"  (do (acl_delete_parking_policy jsmap) jsmap)
    "getAllParkingPolicy"  (do (acl_get_all_parking_policy jsmap) jsmap)
    "getParkingPolicy"  (do (acl_get_parking_policy jsmap) jsmap)
    "updateParkingPolicy"  (do (acl_update_parking_policy jsmap) jsmap)
    "getParkingPolicyVersion"  (do (acl_get_parking_policy_version jsmap) jsmap)
    "getAllVersionsOfParkingPolicy"  (do (acl_get_all_versions_of_parking_policy jsmap) jsmap)
    "getAllActiveParkingPolicyForPeriod"  (do (acl_get_all_active_parking_policy_for_period jsmap) jsmap)
    "getActiveParkingPolicy"  (do (acl_get_active_parking_policy jsmap) jsmap)
    "searchParkingPolicy"  (do (acl_search_parking_policy jsmap) jsmap)
    "createPolicyCategory" (do (acl_create_policy_category jsmap) jsmap)
    "deletePolicyCategory" (do (acl_delete_policy_category jsmap) jsmap)
    "getAllPolicyCategory" (do (acl_get_all_policy_category jsmap) jsmap)
    "getPolicyCategory" (do (acl_get_policy_category jsmap) jsmap)
    "updatePolicyCategory" (do (acl_update_policy_category jsmap) jsmap)
    "policyAssociation" (do (acl_policy_association jsmap) jsmap)
    "policyDisassociation" (do (acl_policy_disassociation jsmap) jsmap)
    "associatedParkingGroups" (do (acl_associated_parking_groups jsmap) jsmap)
    "policyTagsAssociation" (do (acl_policy_tag_association jsmap) jsmap)
    "policyTagsDisassociation" (do (acl_policy_tag_disassociation jsmap) jsmap)
    "createBusinessAlert" (do (acl_create_business_alert jsmap) jsmap)
    "updateBusinessAlert" (do (acl_update_business_alert jsmap) jsmap)
    ("getBusinessAlert"
    "getAllBusinessAlerts"
    "filterBusinessAlert") (do (acl_get_business_alert jsmap) jsmap)
    ("dismissBusinessAlert"
    "deleteBusinessAlert") (do (acl_delete_business_alert jsmap) jsmap)
    "createTrigger" (do (acl_create_trigger jsmap) jsmap)
    "updateTrigger" (do (acl_update_trigger jsmap) jsmap)
    ("getTrigger"
    "getAllTriggers"
    "filterTrigger") (do (acl_get_trigger jsmap) jsmap)
    "deleteTrigger" (do (acl_delete_trigger jsmap) jsmap)
    "createWhatIfJob" (do (acl_create_whatif_job jsmap) jsmap)
    "updateWhatIfJob" (do (acl_update_whatif_job jsmap) jsmap)
    ("searchWhatIfJob"
    "getAllWhatIfJobs"
    "getWhatIfJob") (do (acl_get_whatif_job jsmap) jsmap)
    ("deleteWhatIfJob"
     "abortWhatIfJob") (do (acl_delete_whatif_job jsmap) jsmap)
    "createWhatIfPolicy" (do (acl_create_whatif_policy jsmap) jsmap)
    ("updateWhatIfPolicy"
    "whatIfPolicyTagsAssociation") (do (acl_update_whatif_policy jsmap) jsmap)
    ("getAllWhatIfPolicies"
    "getWhatIfPolicy") (do (acl_get_whatif_policy jsmap) jsmap)
    ("deleteWhatIfPolicy"
    "whatIfPolicyTagsDisassociation") (do (acl_delete_whatif_policy jsmap) jsmap)
    "createWhatIfTag" (do (acl_create_whatif_tag jsmap) jsmap)
    "updateWhatIfTag" (do (acl_update_whatif_tag jsmap) jsmap)
    ("getAllWhatIfTags"
    "getWhatIfTag") (do (acl_get_whatif_tag jsmap) jsmap)
    "deleteWhatIfTag" (do (acl_delete_whatif_tag jsmap) jsmap)
    ("updateVPNInfo"
    "connectToVPN"
    "disconnectFromVPN") (do (acl_assign_firmware jsmap) jsmap)
    ("getGpsByNodeId"
      "getGpsForOrgIdAndSiteId") (do (acl_get_gps jsmap)jsmap)
    ("getAllUFAlarms"
    "getUFAlarm") (do (acl_get_uf_alarm jsmap) jsmap)
    ("createUFAlarm"
    "createBulkUFAlarms") (do (acl_create_uf_alarm jsmap) jsmap)
    ("updateUFAlarm"
    "resetUFAlarm") (do (acl_update_uf_alarm jsmap) jsmap)
    "deleteUFAlarm" (do (acl_delete_uf_alarm jsmap) jsmap)
    "getNotificationTypes" (do (acl_get_notification_types jsmap) jsmap)

    (throw (ex-info (format "Action %s is not supported [acl/core]" action)
                    {:check "correctness of the 'type' parameter in your query"
                     :action action
                     :module-to-check "Interface Service"
                     :cause :bad-request
                     :status 400}))))
