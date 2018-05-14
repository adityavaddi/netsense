

//------------- THIS IS GENERATED FILE - DO NOT EDIT  --------------------//
// Generate this file from Farallones/tools/matrixgen/privileges.js
//
// $ cd Farallones/tools/matrixgen/
//
// edit privileges.js with ACL changes
//
// $ node privileges.js
//
// This will produce the file
// Farallones/dbinit/patches/AllPrivileges-dbpatch.cypher
//----------------------------------------------------------------------- //

CREATE CONSTRAINT ON (org:Org) ASSERT org.orgid IS UNIQUE;
CREATE CONSTRAINT ON (site:Site) ASSERT site.siteid IS UNIQUE;
CREATE CONSTRAINT ON (node:Node) ASSERT node.nodeid IS UNIQUE;
CREATE CONSTRAINT ON (f:Fixture) ASSERT f.fixtureid IS UNIQUE;
CREATE CONSTRAINT ON (c:Config) ASSERT c.configid IS UNIQUE;
CREATE CONSTRAINT ON (g:Group) ASSERT g.groupid is UNIQUE;
CREATE CONSTRAINT ON (fw:Firmware) ASSERT fw.firmwareid is UNIQUE;
CREATE CONSTRAINT ON (sch:Schedule) ASSERT sch.scheduleid is UNIQUE;
CREATE CONSTRAINT ON (u:User) ASSERT u.userid IS UNIQUE;
CREATE CONSTRAINT ON (u:User) ASSERT u.email IS UNIQUE;
CREATE CONSTRAINT ON (r:Role) ASSERT r.rolename IS UNIQUE;
CREATE CONSTRAINT ON (pd:PDProfile) ASSERT pd.pdprofileid IS UNIQUE;
CREATE CONSTRAINT ON (dh:DHProfile) ASSERT dh.dhprofileid IS UNIQUE;
CREATE CONSTRAINT ON (etdh:ETDHProfile) ASSERT etdh.etdhprofileid IS UNIQUE;
CREATE INDEX ON :Alert(nodeid);
CREATE INDEX ON :Alert(name);
CREATE INDEX ON :Alert(type);
CREATE CONSTRAINT ON (a:Alert) ASSERT a.alertid IS UNIQUE;
CREATE CONSTRAINT ON (sm:SiteModel) ASSERT sm.smid IS UNIQUE;
CREATE CONSTRAINT ON (nm:NodeModel) ASSERT nm.nmid IS UNIQUE;
CREATE CONSTRAINT ON (cm:ConfigModel) ASSERT cm.cmid IS UNIQUE;
CREATE CONSTRAINT ON (um:UserModel) ASSERT um.umid IS UNIQUE;
CREATE CONSTRAINT ON (om:OrgModel) ASSERT om.omid IS UNIQUE;
CREATE CONSTRAINT ON (olm:OverlayModel) ASSERT olm.olmid IS UNIQUE;
CREATE CONSTRAINT ON (fwm:FirmwareModel) ASSERT fwm.fwmid IS UNIQUE;
CREATE CONSTRAINT ON (am:AuditModel) ASSERT am.amid IS UNIQUE;
CREATE CONSTRAINT ON (fm:FixtureModel) ASSERT fm.fmid IS UNIQUE;
CREATE CONSTRAINT ON (n:Notification) ASSERT n.notificationid IS UNIQUE;
CREATE CONSTRAINT ON (nfm:NotificationModel) ASSERT nfm.nfmid IS UNIQUE;
CREATE CONSTRAINT ON (pz:ParkingZoneModel) ASSERT pz.parkingzoneid IS UNIQUE;
CREATE CONSTRAINT ON (tom:TrafficObjectModel) ASSERT tom.tomid is UNIQUE;
CREATE CONSTRAINT ON (alm:AlertModel) ASSERT alm.almid IS UNIQUE;
CREATE CONSTRAINT ON (gm:GroupModel) ASSERT gm.gmid IS UNIQUE;
CREATE CONSTRAINT ON (scm:ScheduleModel) ASSERT scm.scmid is UNIQUE;
CREATE CONSTRAINT ON (pm:PartnerModel) ASSERT pm.pmid is UNIQUE;
CREATE CONSTRAINT ON (rm:ReportModel) ASSERT rm.rmid is UNIQUE;
CREATE CONSTRAINT ON (lcm:LicenseModel) ASSERT lcm.lcmid is UNIQUE;
CREATE CONSTRAINT ON (psm:ParkingSpotModel) ASSERT psm.psmid IS UNIQUE;
CREATE CONSTRAINT ON (ppm:ParkingPolicyModel) ASSERT ppm.ppmid IS UNIQUE;
CREATE CONSTRAINT ON (pcm:PolicyCategoryModel) ASSERT pcm.pcmid IS UNIQUE;
CREATE CONSTRAINT ON (pgm:ParkingGroupModel) ASSERT pgm.pgmid IS UNIQUE;
CREATE CONSTRAINT ON (srm:SummaryReportModel) ASSERT srm.srmid IS UNIQUE;
CREATE CONSTRAINT ON (bam:BusinessAlertModel) ASSERT bam.bamid IS UNIQUE;
CREATE CONSTRAINT ON (tm:TriggerModel) ASSERT tm.tmid IS UNIQUE;
CREATE CONSTRAINT ON (gps:GpsModel) ASSERT gps.gpsid IS UNIQUE;
CREATE CONSTRAINT ON (wam:WhatIfAnalysisModel) ASSERT wam.wamid IS UNIQUE;
CREATE CONSTRAINT ON (wpm:WhatIfPolicyModel) ASSERT wpm.wpmid IS UNIQUE;
CREATE CONSTRAINT ON (wtm:WhatIfTagModel) ASSERT wtm.wtmid IS UNIQUE;


CREATE CONSTRAINT ON (ufam:UFAlarmModel) ASSERT ufam.ufamid is UNIQUE;



MERGE (om:OrgModel:Model {omid: "omid", name: "OrgModel"})
  ON CREATE SET om.created = timestamp();
MERGE (sm:SiteModel:Model {smid: "smid", name: "SiteModel"})
  ON CREATE SET sm.created = timestamp();
MERGE (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
  ON CREATE SET nm.created = timestamp();
MERGE (cm:ConfigModel:Model {cmid: "cmid", name: "ConfigModel"})
  ON CREATE SET cm.created = timestamp();
MERGE (um:UserModel:Model {umid: "umid", name: "UserModel"})
  ON CREATE SET um.created = timestamp();
MERGE (olm:OverlayModel:Model {olmid: "olmid", name: "OverlayModel"})
  ON CREATE SET olm.created = timestamp();
MERGE (fwm:FirmwareModel:Model {fwmid: "fwmid", name: "FirmwareModel"})
  ON CREATE SET fwm.created = timestamp();
MERGE (am:AuditModel:Model {amid: "amid", name: "AuditModel"})
  ON CREATE SET am.created = timestamp();
MERGE (fm:FixtureModel:Model {fmid: "fmid", name: "FixtureModel"})
  ON CREATE SET fm.created = timestamp();
MERGE (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
  ON CREATE SET nfm.created = timestamp();
MERGE (pz:ParkingZoneModel:Model {parkingzoneid: "parkingzoneid", name: "ParkingZoneModel"})
  ON CREATE SET pz.created = timestamp();
MERGE (alm:AlertModel:Model {almid: "almid", name: "AlertModel"})
  ON CREATE SET alm.created = timestamp();
MERGE (gm:GroupModel:Model {gmid: "gmid", name: "GroupModel"})
  ON CREATE SET gm.created = timestamp();
MERGE (scm:ScheduleModel:Model {scmid: "scmid", name: "ScheduleModel"})
  ON CREATE SET scm.created = timestamp();
MERGE (pm:PartnerModel:Model {pmid: "pmid", name: "PartnerModel"})
  ON CREATE SET pm.created = timestamp();
MERGE (rm:ReportModel:Model {rmid: "rmid", name: "ReportModel"})
  ON CREATE SET rm.created = timestamp();
MERGE (lcm:LicenseModel:Model {lcmid: "lcmid", name: "LicenseModel"})
  ON CREATE SET lcm.created = timestamp();
MERGE (tom:TrafficObjectModel:Model {tomid: "tomid", name: "TrafficObjectModel"})
  ON CREATE SET tom.created = timestamp();
MERGE (psm:ParkingSpotModel:Model {psmid: "psmid", name: "ParkingSpotModel"})
  ON CREATE SET psm.created = timestamp();
MERGE (ppm:ParkingPolicyModel:Model {ppmid: "ppmid", name: "ParkingPolicyModel"})
  ON CREATE SET ppm.created = timestamp();
MERGE (pcm:PolicyCategoryModel:Model {pcmid: "pcmid", name: "PolicyCategoryModel"})
  ON CREATE SET pcm.created = timestamp();
MERGE (pgm:ParkingGroupModel:Model {pgmid: "pgmid", name: "ParkingGroupModel"})
  ON CREATE SET pgm.created = timestamp();
MERGE (srm:SummaryReportModel:Model {srmid: "srmid", name: "SummaryReportModel"})
  ON CREATE SET srm.created = timestamp();
MERGE (bam:BusinessAlertModel:Model {bamid: "bamid", name: "BusinessAlertModel"})
  ON CREATE SET bam.created = timestamp();
MERGE (tm:TriggerModel:Model {tmid: "tmid", name: "TriggerModel"})
  ON CREATE SET tm.created = timestamp();
MERGE(gps:GpsModel:Model {gpsid: "gpsid",name: "GpsModel"})
  ON CREATE SET gps.created = timestamp();
  MERGE (wam:WhatIfAnalysisModel:Model {wamid: "wamid", name: "WhatIfAnalysisModel"})
    ON CREATE SET wam.created = timestamp();
  MERGE (wpm:WhatIfPolicyModel:Model {wpmid: "wpmid", name: "WhatIfPolicyModel"})
    ON CREATE SET wpm.created = timestamp();
  MERGE (wtm:WhatIfTagModel:Model {wtmid: "wtmid", name: "WhatIfTagModel"})
    ON CREATE SET wtm.created = timestamp();
MERGE (ufam:UFAlarmModel:Model {ufamid: "ufamid", name: "UFAlarmModel"})
  ON CREATE SET ufam.created = timestamp();




MERGE (sensity_admin:Role:Admin {rolename: "sensity_admin"})
  ON CREATE SET sensity_admin.created = timestamp();
MERGE (sensity_read_only:Role:Admin {rolename: "sensity_read_only"})
  ON CREATE SET sensity_read_only.created = timestamp();
MERGE (sensity_user:Role:Admin {rolename: "sensity_user"})
  ON CREATE SET sensity_user.created = timestamp();

MERGE (partner_admin:Role {rolename: "partner_admin"})
  ON CREATE SET partner_admin.created = timestamp();
MERGE (partner_deployment_user:Role {rolename: "partner_deployment_user"})
  ON CREATE SET partner_deployment_user.created = timestamp();
MERGE (partner_lighting_user:Role {rolename: "partner_lighting_user"})
  ON CREATE SET partner_lighting_user.created = timestamp();
MERGE (partner_sensor_user:Role {rolename: "partner_sensor_user"})
  ON CREATE SET partner_sensor_user.created = timestamp();
MERGE (partner_networking_user:Role {rolename: "partner_networking_user"})
  ON CREATE SET partner_networking_user.created = timestamp();
MERGE (partner_read_only:Role {rolename: "partner_read_only"})
  ON CREATE SET partner_read_only.created = timestamp();
MERGE (partner_api:Role {rolename: "partner_api"})
  ON CREATE SET partner_api.created = timestamp();

MERGE (end_user_admin:Role {rolename: "end_user_admin"})
  ON CREATE SET end_user_admin.created = timestamp();
MERGE (end_user_lighting_user:Role {rolename: "end_user_lighting_user"})
  ON CREATE SET end_user_lighting_user.created = timestamp();
MERGE (end_user_sensor_user:Role {rolename: "end_user_sensor_user"})
  ON CREATE SET end_user_sensor_user.created = timestamp();
MERGE (end_user_networking_user:Role {rolename: "end_user_networking_user"})
  ON CREATE SET end_user_networking_user.created = timestamp();
MERGE (end_user_read_only:Role {rolename: "end_user_read_only"})
  ON CREATE SET end_user_read_only.created = timestamp();
MERGE (end_user_api:Role {rolename: "end_user_api"})
  ON CREATE SET end_user_api.created = timestamp();

MERGE (installer:Role {rolename: "installer"})
  ON CREATE SET installer.created = timestamp();

MERGE (parking_owner_admin:Role {rolename: "parking_owner_admin"})
  ON CREATE SET parking_owner_admin.created = timestamp();
MERGE (parking_manager:Role {rolename: "parking_manager"})
  ON CREATE SET parking_manager.created = timestamp();
MERGE (policy_authority:Role {rolename: "policy_authority"})
  ON CREATE SET policy_authority.created = timestamp();

MATCH (sensity_admin_user:User:Active {userid: "114ad560-a046-11e5-a57f-ef24ae600576"}),
      (sensity_admin:Role:Admin {rolename: "sensity_admin"}),
      (sensity:Org:Active {orgid: "efe5bdb3-baac-5d8e-6cae57771c13"})
MERGE (sensity_admin_user)-[:IS]->(sensity_admin)
MERGE (sensity_admin)-[:HAS]->(sensity_admin_user)
MERGE (sensity_admin_user)-[:IS_USER_OF]->(sensity)
MERGE (sensity)-[:HAS_USER]->(sensity_admin_user);

MATCH (sensity_user_user:User:Active {userid: "507ef3cc-cd2d-46d8-ae6d-7ccf430c1110"}),
      (sensity_user:Role:Admin {rolename: "sensity_user"}),
      (sensity:Org:Active {orgid: "efe5bdb3-baac-5d8e-6cae57771c13"})
MERGE (sensity_user_user)-[:IS]->(sensity_user)
MERGE (sensity_user)-[:HAS]->(sensity_user_user)
MERGE (sensity_user_user)-[:IS_USER_OF]->(sensity)
MERGE (sensity)-[:HAS_USER]->(sensity_user_user);

MATCH (sensity_read_only_user:User:Active {userid: "47e3e2f2-b93b-4557-b668-271d43d028ac"}),
      (sensity_read_only:Role:Admin {rolename: "sensity_read_only"}),
      (sensity:Org:Active {orgid: "efe5bdb3-baac-5d8e-6cae57771c13"})
MERGE (sensity_read_only_user)-[:IS]->(sensity_read_only)
MERGE (sensity_read_only)-[:HAS]->(sensity_read_only_user)
MERGE (sensity_read_only_user)-[:IS_USER_OF]->(sensity)
MERGE (sensity)-[:HAS_USER]->(sensity_read_only_user);

MATCH (:Role)-[r]->(:Model) DELETE r;
MATCH (sensity_admin:Role:Admin {rolename: "sensity_admin"}), (pm:PartnerModel:Model {pmid: "pmid", name: "PartnerModel"})
MERGE (sensity_admin)-[:CAN_CREATE]->(pm);
MATCH (sensity_admin:Role:Admin {rolename: "sensity_admin"}), (pm:PartnerModel:Model {pmid: "pmid", name: "PartnerModel"})
MERGE (sensity_admin)-[:CAN_DELETE]->(pm);
MATCH (sensity_admin:Role:Admin {rolename: "sensity_admin"}), (pm:PartnerModel:Model {pmid: "pmid", name: "PartnerModel"})
MERGE (sensity_admin)-[:CAN_UPDATE]->(pm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (pm:PartnerModel:Model {pmid: "pmid", name: "PartnerModel"})
MERGE (sensity_user)-[:CAN_UPDATE]->(pm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (pm:PartnerModel:Model {pmid: "pmid", name: "PartnerModel"})
MERGE (partner_admin)-[:CAN_UPDATE]->(pm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (pm:PartnerModel:Model {pmid: "pmid", name: "PartnerModel"})
MERGE (sensity_user)-[:CAN_SUSPEND]->(pm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (pm:PartnerModel:Model {pmid: "pmid", name: "PartnerModel"})
MERGE (partner_admin)-[:CAN_SUSPEND]->(pm);
MATCH (sensity_admin:Role:Admin {rolename: "sensity_admin"}), (pm:PartnerModel:Model {pmid: "pmid", name: "PartnerModel"})
MERGE (sensity_admin)-[:CAN_READ]->(pm);
MATCH (sensity_read_only:Role:Admin {rolename: "sensity_read_only"}), (pm:PartnerModel:Model {pmid: "pmid", name: "PartnerModel"})
MERGE (sensity_read_only)-[:CAN_READ]->(pm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (pm:PartnerModel:Model {pmid: "pmid", name: "PartnerModel"})
MERGE (sensity_user)-[:CAN_READ]->(pm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (pm:PartnerModel:Model {pmid: "pmid", name: "PartnerModel"})
MERGE (partner_admin)-[:CAN_READ]->(pm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (pm:PartnerModel:Model {pmid: "pmid", name: "PartnerModel"})
MERGE (partner_deployment_user)-[:CAN_READ]->(pm);
MATCH (partner_lighting_user:Role {rolename: "partner_lighting_user"}), (pm:PartnerModel:Model {pmid: "pmid", name: "PartnerModel"})
MERGE (partner_lighting_user)-[:CAN_READ]->(pm);
MATCH (partner_sensor_user:Role {rolename: "partner_sensor_user"}), (pm:PartnerModel:Model {pmid: "pmid", name: "PartnerModel"})
MERGE (partner_sensor_user)-[:CAN_READ]->(pm);
MATCH (partner_networking_user:Role {rolename: "partner_networking_user"}), (pm:PartnerModel:Model {pmid: "pmid", name: "PartnerModel"})
MERGE (partner_networking_user)-[:CAN_READ]->(pm);
MATCH (partner_read_only:Role {rolename: "partner_read_only"}), (pm:PartnerModel:Model {pmid: "pmid", name: "PartnerModel"})
MERGE (partner_read_only)-[:CAN_READ]->(pm);
MATCH (partner_api:Role {rolename: "partner_api"}), (pm:PartnerModel:Model {pmid: "pmid", name: "PartnerModel"})
MERGE (partner_api)-[:CAN_READ]->(pm);
MATCH (sensity_admin:Role:Admin {rolename: "sensity_admin"}), (om:OrgModel:Model {omid: "omid", name: "OrgModel"})
MERGE (sensity_admin)-[:CAN_CREATE]->(om);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (om:OrgModel:Model {omid: "omid", name: "OrgModel"})
MERGE (sensity_user)-[:CAN_CREATE]->(om);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (om:OrgModel:Model {omid: "omid", name: "OrgModel"})
MERGE (partner_admin)-[:CAN_CREATE]->(om);
MATCH (partner_api:Role {rolename: "partner_api"}), (om:OrgModel:Model {omid: "omid", name: "OrgModel"})
MERGE (partner_api)-[:CAN_CREATE]->(om);
MATCH (sensity_admin:Role:Admin {rolename: "sensity_admin"}), (om:OrgModel:Model {omid: "omid", name: "OrgModel"})
MERGE (sensity_admin)-[:CAN_CHANGE]->(om);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (om:OrgModel:Model {omid: "omid", name: "OrgModel"})
MERGE (sensity_user)-[:CAN_CHANGE]->(om);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (om:OrgModel:Model {omid: "omid", name: "OrgModel"})
MERGE (partner_admin)-[:CAN_CHANGE]->(om);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (om:OrgModel:Model {omid: "omid", name: "OrgModel"})
MERGE (partner_deployment_user)-[:CAN_CHANGE]->(om);
MATCH (partner_lighting_user:Role {rolename: "partner_lighting_user"}), (om:OrgModel:Model {omid: "omid", name: "OrgModel"})
MERGE (partner_lighting_user)-[:CAN_CHANGE]->(om);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (om:OrgModel:Model {omid: "omid", name: "OrgModel"})
MERGE (end_user_admin)-[:CAN_CHANGE]->(om);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (om:OrgModel:Model {omid: "omid", name: "OrgModel"})
MERGE (parking_owner_admin)-[:CAN_CHANGE]->(om);
MATCH (sensity_admin:Role:Admin {rolename: "sensity_admin"}), (om:OrgModel:Model {omid: "omid", name: "OrgModel"})
MERGE (sensity_admin)-[:CAN_SUSPEND]->(om);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (om:OrgModel:Model {omid: "omid", name: "OrgModel"})
MERGE (sensity_user)-[:CAN_SUSPEND]->(om);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (om:OrgModel:Model {omid: "omid", name: "OrgModel"})
MERGE (partner_admin)-[:CAN_SUSPEND]->(om);
MATCH (partner_api:Role {rolename: "partner_api"}), (om:OrgModel:Model {omid: "omid", name: "OrgModel"})
MERGE (partner_api)-[:CAN_SUSPEND]->(om);
MATCH (sensity_admin:Role:Admin {rolename: "sensity_admin"}), (om:OrgModel:Model {omid: "omid", name: "OrgModel"})
MERGE (sensity_admin)-[:CAN_DELETE]->(om);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (om:OrgModel:Model {omid: "omid", name: "OrgModel"})
MERGE (sensity_user)-[:CAN_DELETE]->(om);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (om:OrgModel:Model {omid: "omid", name: "OrgModel"})
MERGE (partner_admin)-[:CAN_DELETE]->(om);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (om:OrgModel:Model {omid: "omid", name: "OrgModel"})
MERGE (partner_deployment_user)-[:CAN_DELETE]->(om);
MATCH (partner_api:Role {rolename: "partner_api"}), (om:OrgModel:Model {omid: "omid", name: "OrgModel"})
MERGE (partner_api)-[:CAN_DELETE]->(om);
MATCH (sensity_admin:Role:Admin {rolename: "sensity_admin"}), (om:OrgModel:Model {omid: "omid", name: "OrgModel"})
MERGE (sensity_admin)-[:CAN_READ]->(om);
MATCH (sensity_read_only:Role:Admin {rolename: "sensity_read_only"}), (om:OrgModel:Model {omid: "omid", name: "OrgModel"})
MERGE (sensity_read_only)-[:CAN_READ]->(om);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (om:OrgModel:Model {omid: "omid", name: "OrgModel"})
MERGE (sensity_user)-[:CAN_READ]->(om);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (om:OrgModel:Model {omid: "omid", name: "OrgModel"})
MERGE (partner_admin)-[:CAN_READ]->(om);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (om:OrgModel:Model {omid: "omid", name: "OrgModel"})
MERGE (partner_deployment_user)-[:CAN_READ]->(om);
MATCH (partner_lighting_user:Role {rolename: "partner_lighting_user"}), (om:OrgModel:Model {omid: "omid", name: "OrgModel"})
MERGE (partner_lighting_user)-[:CAN_READ]->(om);
MATCH (partner_sensor_user:Role {rolename: "partner_sensor_user"}), (om:OrgModel:Model {omid: "omid", name: "OrgModel"})
MERGE (partner_sensor_user)-[:CAN_READ]->(om);
MATCH (partner_networking_user:Role {rolename: "partner_networking_user"}), (om:OrgModel:Model {omid: "omid", name: "OrgModel"})
MERGE (partner_networking_user)-[:CAN_READ]->(om);
MATCH (partner_read_only:Role {rolename: "partner_read_only"}), (om:OrgModel:Model {omid: "omid", name: "OrgModel"})
MERGE (partner_read_only)-[:CAN_READ]->(om);
MATCH (partner_api:Role {rolename: "partner_api"}), (om:OrgModel:Model {omid: "omid", name: "OrgModel"})
MERGE (partner_api)-[:CAN_READ]->(om);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (om:OrgModel:Model {omid: "omid", name: "OrgModel"})
MERGE (end_user_admin)-[:CAN_READ]->(om);
MATCH (end_user_read_only:Role {rolename: "end_user_read_only"}), (om:OrgModel:Model {omid: "omid", name: "OrgModel"})
MERGE (end_user_read_only)-[:CAN_READ]->(om);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (om:OrgModel:Model {omid: "omid", name: "OrgModel"})
MERGE (end_user_api)-[:CAN_READ]->(om);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (om:OrgModel:Model {omid: "omid", name: "OrgModel"})
MERGE (parking_owner_admin)-[:CAN_READ]->(om);
MATCH (policy_authority:Role {rolename: "policy_authority"}), (om:OrgModel:Model {omid: "omid", name: "OrgModel"})
MERGE (policy_authority)-[:CAN_READ]->(om);
MATCH (parking_manager:Role {rolename: "parking_manager"}), (om:OrgModel:Model {omid: "omid", name: "OrgModel"})
MERGE (parking_manager)-[:CAN_READ]->(om);
MATCH (end_user_lighting_user:Role {rolename: "end_user_lighting_user"}), (om:OrgModel:Model {omid: "omid", name: "OrgModel"})
MERGE (end_user_lighting_user)-[:CAN_READ]->(om);
MATCH (end_user_sensor_user:Role {rolename: "end_user_sensor_user"}), (om:OrgModel:Model {omid: "omid", name: "OrgModel"})
MERGE (end_user_sensor_user)-[:CAN_READ]->(om);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (sm:SiteModel:Model {smid: "smid", name: "SiteModel"})
MERGE (sensity_user)-[:CAN_CREATE]->(sm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (sm:SiteModel:Model {smid: "smid", name: "SiteModel"})
MERGE (partner_admin)-[:CAN_CREATE]->(sm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (sm:SiteModel:Model {smid: "smid", name: "SiteModel"})
MERGE (partner_deployment_user)-[:CAN_CREATE]->(sm);
MATCH (partner_lighting_user:Role {rolename: "partner_lighting_user"}), (sm:SiteModel:Model {smid: "smid", name: "SiteModel"})
MERGE (partner_lighting_user)-[:CAN_CREATE]->(sm);
MATCH (partner_sensor_user:Role {rolename: "partner_sensor_user"}), (sm:SiteModel:Model {smid: "smid", name: "SiteModel"})
MERGE (partner_sensor_user)-[:CAN_CREATE]->(sm);
MATCH (partner_api:Role {rolename: "partner_api"}), (sm:SiteModel:Model {smid: "smid", name: "SiteModel"})
MERGE (partner_api)-[:CAN_CREATE]->(sm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (sm:SiteModel:Model {smid: "smid", name: "SiteModel"})
MERGE (sensity_user)-[:CAN_CHANGE]->(sm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (sm:SiteModel:Model {smid: "smid", name: "SiteModel"})
MERGE (partner_admin)-[:CAN_CHANGE]->(sm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (sm:SiteModel:Model {smid: "smid", name: "SiteModel"})
MERGE (partner_deployment_user)-[:CAN_CHANGE]->(sm);
MATCH (partner_api:Role {rolename: "partner_api"}), (sm:SiteModel:Model {smid: "smid", name: "SiteModel"})
MERGE (partner_api)-[:CAN_CHANGE]->(sm);
MATCH (end_user_lighting_user:Role {rolename: "end_user_lighting_user"}), (sm:SiteModel:Model {smid: "smid", name: "SiteModel"})
MERGE (end_user_lighting_user)-[:CAN_CHANGE]->(sm);
MATCH (end_user_sensor_user:Role {rolename: "end_user_sensor_user"}), (sm:SiteModel:Model {smid: "smid", name: "SiteModel"})
MERGE (end_user_sensor_user)-[:CAN_CHANGE]->(sm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (sm:SiteModel:Model {smid: "smid", name: "SiteModel"})
MERGE (end_user_api)-[:CAN_CHANGE]->(sm);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (sm:SiteModel:Model {smid: "smid", name: "SiteModel"})
MERGE (parking_owner_admin)-[:CAN_CHANGE]->(sm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (sm:SiteModel:Model {smid: "smid", name: "SiteModel"})
MERGE (sensity_user)-[:CAN_SUSPEND]->(sm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (sm:SiteModel:Model {smid: "smid", name: "SiteModel"})
MERGE (partner_admin)-[:CAN_SUSPEND]->(sm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (sm:SiteModel:Model {smid: "smid", name: "SiteModel"})
MERGE (partner_deployment_user)-[:CAN_SUSPEND]->(sm);
MATCH (partner_api:Role {rolename: "partner_api"}), (sm:SiteModel:Model {smid: "smid", name: "SiteModel"})
MERGE (partner_api)-[:CAN_SUSPEND]->(sm);
MATCH (sensity_admin:Role:Admin {rolename: "sensity_admin"}), (sm:SiteModel:Model {smid: "smid", name: "SiteModel"})
MERGE (sensity_admin)-[:CAN_READ]->(sm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (sm:SiteModel:Model {smid: "smid", name: "SiteModel"})
MERGE (sensity_user)-[:CAN_READ]->(sm);
MATCH (sensity_read_only:Role:Admin {rolename: "sensity_read_only"}), (sm:SiteModel:Model {smid: "smid", name: "SiteModel"})
MERGE (sensity_read_only)-[:CAN_READ]->(sm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (sm:SiteModel:Model {smid: "smid", name: "SiteModel"})
MERGE (partner_admin)-[:CAN_READ]->(sm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (sm:SiteModel:Model {smid: "smid", name: "SiteModel"})
MERGE (partner_deployment_user)-[:CAN_READ]->(sm);
MATCH (partner_lighting_user:Role {rolename: "partner_lighting_user"}), (sm:SiteModel:Model {smid: "smid", name: "SiteModel"})
MERGE (partner_lighting_user)-[:CAN_READ]->(sm);
MATCH (partner_sensor_user:Role {rolename: "partner_sensor_user"}), (sm:SiteModel:Model {smid: "smid", name: "SiteModel"})
MERGE (partner_sensor_user)-[:CAN_READ]->(sm);
MATCH (partner_networking_user:Role {rolename: "partner_networking_user"}), (sm:SiteModel:Model {smid: "smid", name: "SiteModel"})
MERGE (partner_networking_user)-[:CAN_READ]->(sm);
MATCH (partner_read_only:Role {rolename: "partner_read_only"}), (sm:SiteModel:Model {smid: "smid", name: "SiteModel"})
MERGE (partner_read_only)-[:CAN_READ]->(sm);
MATCH (partner_api:Role {rolename: "partner_api"}), (sm:SiteModel:Model {smid: "smid", name: "SiteModel"})
MERGE (partner_api)-[:CAN_READ]->(sm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (sm:SiteModel:Model {smid: "smid", name: "SiteModel"})
MERGE (end_user_admin)-[:CAN_READ]->(sm);
MATCH (end_user_lighting_user:Role {rolename: "end_user_lighting_user"}), (sm:SiteModel:Model {smid: "smid", name: "SiteModel"})
MERGE (end_user_lighting_user)-[:CAN_READ]->(sm);
MATCH (end_user_sensor_user:Role {rolename: "end_user_sensor_user"}), (sm:SiteModel:Model {smid: "smid", name: "SiteModel"})
MERGE (end_user_sensor_user)-[:CAN_READ]->(sm);
MATCH (end_user_networking_user:Role {rolename: "end_user_networking_user"}), (sm:SiteModel:Model {smid: "smid", name: "SiteModel"})
MERGE (end_user_networking_user)-[:CAN_READ]->(sm);
MATCH (end_user_read_only:Role {rolename: "end_user_read_only"}), (sm:SiteModel:Model {smid: "smid", name: "SiteModel"})
MERGE (end_user_read_only)-[:CAN_READ]->(sm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (sm:SiteModel:Model {smid: "smid", name: "SiteModel"})
MERGE (end_user_api)-[:CAN_READ]->(sm);
MATCH (installer:Role {rolename: "installer"}), (sm:SiteModel:Model {smid: "smid", name: "SiteModel"})
MERGE (installer)-[:CAN_READ]->(sm);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (sm:SiteModel:Model {smid: "smid", name: "SiteModel"})
MERGE (parking_owner_admin)-[:CAN_READ]->(sm);
MATCH (policy_authority:Role {rolename: "policy_authority"}), (sm:SiteModel:Model {smid: "smid", name: "SiteModel"})
MERGE (policy_authority)-[:CAN_READ]->(sm);
MATCH (parking_manager:Role {rolename: "parking_manager"}), (sm:SiteModel:Model {smid: "smid", name: "SiteModel"})
MERGE (parking_manager)-[:CAN_READ]->(sm);
MATCH (sensity_admin:Role:Admin {rolename: "sensity_admin"}), (um:UserModel:Model {umid: "umid", name: "UserModel"})
MERGE (sensity_admin)-[:CAN_CREATE_ORG_USER]->(um);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (um:UserModel:Model {umid: "umid", name: "UserModel"})
MERGE (sensity_user)-[:CAN_CREATE_ORG_USER]->(um);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (um:UserModel:Model {umid: "umid", name: "UserModel"})
MERGE (partner_admin)-[:CAN_CREATE_ORG_USER]->(um);
MATCH (partner_api:Role {rolename: "partner_api"}), (um:UserModel:Model {umid: "umid", name: "UserModel"})
MERGE (partner_api)-[:CAN_CREATE_ORG_USER]->(um);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (um:UserModel:Model {umid: "umid", name: "UserModel"})
MERGE (end_user_admin)-[:CAN_CREATE_ORG_USER]->(um);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (um:UserModel:Model {umid: "umid", name: "UserModel"})
MERGE (parking_owner_admin)-[:CAN_CREATE_ORG_USER]->(um);
MATCH (sensity_admin:Role:Admin {rolename: "sensity_admin"}), (um:UserModel:Model {umid: "umid", name: "UserModel"})
MERGE (sensity_admin)-[:CAN_CHANGE_ORG_USER]->(um);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (um:UserModel:Model {umid: "umid", name: "UserModel"})
MERGE (sensity_user)-[:CAN_CHANGE_ORG_USER]->(um);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (um:UserModel:Model {umid: "umid", name: "UserModel"})
MERGE (partner_admin)-[:CAN_CHANGE_ORG_USER]->(um);
MATCH (partner_api:Role {rolename: "partner_api"}), (um:UserModel:Model {umid: "umid", name: "UserModel"})
MERGE (partner_api)-[:CAN_CHANGE_ORG_USER]->(um);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (um:UserModel:Model {umid: "umid", name: "UserModel"})
MERGE (end_user_admin)-[:CAN_CHANGE_ORG_USER]->(um);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (um:UserModel:Model {umid: "umid", name: "UserModel"})
MERGE (parking_owner_admin)-[:CAN_CHANGE_ORG_USER]->(um);
MATCH (sensity_admin:Role:Admin {rolename: "sensity_admin"}), (um:UserModel:Model {umid: "umid", name: "UserModel"})
MERGE (sensity_admin)-[:CAN_SUSPEND_ORG_USER]->(um);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (um:UserModel:Model {umid: "umid", name: "UserModel"})
MERGE (sensity_user)-[:CAN_SUSPEND_ORG_USER]->(um);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (um:UserModel:Model {umid: "umid", name: "UserModel"})
MERGE (partner_admin)-[:CAN_SUSPEND_ORG_USER]->(um);
MATCH (partner_api:Role {rolename: "partner_api"}), (um:UserModel:Model {umid: "umid", name: "UserModel"})
MERGE (partner_api)-[:CAN_SUSPEND_ORG_USER]->(um);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (um:UserModel:Model {umid: "umid", name: "UserModel"})
MERGE (end_user_admin)-[:CAN_SUSPEND_ORG_USER]->(um);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (um:UserModel:Model {umid: "umid", name: "UserModel"})
MERGE (parking_owner_admin)-[:CAN_SUSPEND_ORG_USER]->(um);
MATCH (sensity_admin:Role:Admin {rolename: "sensity_admin"}), (um:UserModel:Model {umid: "umid", name: "UserModel"})
MERGE (sensity_admin)-[:CAN_READ_ORG_USER]->(um);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (um:UserModel:Model {umid: "umid", name: "UserModel"})
MERGE (sensity_user)-[:CAN_READ_ORG_USER]->(um);
MATCH (sensity_read_only:Role:Admin {rolename: "sensity_read_only"}), (um:UserModel:Model {umid: "umid", name: "UserModel"})
MERGE (sensity_read_only)-[:CAN_READ_ORG_USER]->(um);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (um:UserModel:Model {umid: "umid", name: "UserModel"})
MERGE (partner_admin)-[:CAN_READ_ORG_USER]->(um);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (um:UserModel:Model {umid: "umid", name: "UserModel"})
MERGE (partner_deployment_user)-[:CAN_READ_ORG_USER]->(um);
MATCH (partner_lighting_user:Role {rolename: "partner_lighting_user"}), (um:UserModel:Model {umid: "umid", name: "UserModel"})
MERGE (partner_lighting_user)-[:CAN_READ_ORG_USER]->(um);
MATCH (partner_sensor_user:Role {rolename: "partner_sensor_user"}), (um:UserModel:Model {umid: "umid", name: "UserModel"})
MERGE (partner_sensor_user)-[:CAN_READ_ORG_USER]->(um);
MATCH (partner_networking_user:Role {rolename: "partner_networking_user"}), (um:UserModel:Model {umid: "umid", name: "UserModel"})
MERGE (partner_networking_user)-[:CAN_READ_ORG_USER]->(um);
MATCH (partner_read_only:Role {rolename: "partner_read_only"}), (um:UserModel:Model {umid: "umid", name: "UserModel"})
MERGE (partner_read_only)-[:CAN_READ_ORG_USER]->(um);
MATCH (partner_api:Role {rolename: "partner_api"}), (um:UserModel:Model {umid: "umid", name: "UserModel"})
MERGE (partner_api)-[:CAN_READ_ORG_USER]->(um);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (um:UserModel:Model {umid: "umid", name: "UserModel"})
MERGE (end_user_admin)-[:CAN_READ_ORG_USER]->(um);
MATCH (end_user_lighting_user:Role {rolename: "end_user_lighting_user"}), (um:UserModel:Model {umid: "umid", name: "UserModel"})
MERGE (end_user_lighting_user)-[:CAN_READ_ORG_USER]->(um);
MATCH (end_user_sensor_user:Role {rolename: "end_user_sensor_user"}), (um:UserModel:Model {umid: "umid", name: "UserModel"})
MERGE (end_user_sensor_user)-[:CAN_READ_ORG_USER]->(um);
MATCH (end_user_networking_user:Role {rolename: "end_user_networking_user"}), (um:UserModel:Model {umid: "umid", name: "UserModel"})
MERGE (end_user_networking_user)-[:CAN_READ_ORG_USER]->(um);
MATCH (end_user_read_only:Role {rolename: "end_user_read_only"}), (um:UserModel:Model {umid: "umid", name: "UserModel"})
MERGE (end_user_read_only)-[:CAN_READ_ORG_USER]->(um);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (um:UserModel:Model {umid: "umid", name: "UserModel"})
MERGE (end_user_api)-[:CAN_READ_ORG_USER]->(um);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (um:UserModel:Model {umid: "umid", name: "UserModel"})
MERGE (parking_owner_admin)-[:CAN_READ_ORG_USER]->(um);
MATCH (policy_authority:Role {rolename: "policy_authority"}), (um:UserModel:Model {umid: "umid", name: "UserModel"})
MERGE (policy_authority)-[:CAN_READ_ORG_USER]->(um);
MATCH (parking_manager:Role {rolename: "parking_manager"}), (um:UserModel:Model {umid: "umid", name: "UserModel"})
MERGE (parking_manager)-[:CAN_READ_ORG_USER]->(um);
MATCH (installer:Role {rolename: "installer"}), (um:UserModel:Model {umid: "umid", name: "UserModel"})
MERGE (installer)-[:CAN_READ_ORG_USER]->(um);
MATCH (sensity_admin:Role:Admin {rolename: "sensity_admin"}), (um:UserModel:Model {umid: "umid", name: "UserModel"})
MERGE (sensity_admin)-[:CAN_GENERATE_API_KEY]->(um);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (um:UserModel:Model {umid: "umid", name: "UserModel"})
MERGE (sensity_user)-[:CAN_GENERATE_API_KEY]->(um);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (um:UserModel:Model {umid: "umid", name: "UserModel"})
MERGE (partner_admin)-[:CAN_GENERATE_API_KEY]->(um);
MATCH (installer:Role {rolename: "installer"}), (um:UserModel:Model {umid: "umid", name: "UserModel"})
MERGE (installer)-[:CAN_GENERATE_API_KEY]->(um);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (gm:GroupModel:Model {gmid: "gmid", name: "GroupModel"})
MERGE (sensity_user)-[:CAN_CREATE]->(gm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (gm:GroupModel:Model {gmid: "gmid", name: "GroupModel"})
MERGE (partner_admin)-[:CAN_CREATE]->(gm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (gm:GroupModel:Model {gmid: "gmid", name: "GroupModel"})
MERGE (partner_deployment_user)-[:CAN_CREATE]->(gm);
MATCH (partner_lighting_user:Role {rolename: "partner_lighting_user"}), (gm:GroupModel:Model {gmid: "gmid", name: "GroupModel"})
MERGE (partner_lighting_user)-[:CAN_CREATE]->(gm);
MATCH (partner_sensor_user:Role {rolename: "partner_sensor_user"}), (gm:GroupModel:Model {gmid: "gmid", name: "GroupModel"})
MERGE (partner_sensor_user)-[:CAN_CREATE]->(gm);
MATCH (partner_api:Role {rolename: "partner_api"}), (gm:GroupModel:Model {gmid: "gmid", name: "GroupModel"})
MERGE (partner_api)-[:CAN_CREATE]->(gm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (gm:GroupModel:Model {gmid: "gmid", name: "GroupModel"})
MERGE (end_user_admin)-[:CAN_CREATE]->(gm);
MATCH (end_user_lighting_user:Role {rolename: "end_user_lighting_user"}), (gm:GroupModel:Model {gmid: "gmid", name: "GroupModel"})
MERGE (end_user_lighting_user)-[:CAN_CREATE]->(gm);
MATCH (end_user_sensor_user:Role {rolename: "end_user_sensor_user"}), (gm:GroupModel:Model {gmid: "gmid", name: "GroupModel"})
MERGE (end_user_sensor_user)-[:CAN_CREATE]->(gm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (gm:GroupModel:Model {gmid: "gmid", name: "GroupModel"})
MERGE (end_user_api)-[:CAN_CREATE]->(gm);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (gm:GroupModel:Model {gmid: "gmid", name: "GroupModel"})
MERGE (parking_owner_admin)-[:CAN_CREATE]->(gm);
MATCH (end_user_networking_user:Role {rolename: "end_user_networking_user"}), (gm:GroupModel:Model {gmid: "gmid", name: "GroupModel"})
MERGE (end_user_networking_user)-[:CAN_CREATE]->(gm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (gm:GroupModel:Model {gmid: "gmid", name: "GroupModel"})
MERGE (sensity_user)-[:CAN_UPDATE]->(gm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (gm:GroupModel:Model {gmid: "gmid", name: "GroupModel"})
MERGE (partner_admin)-[:CAN_UPDATE]->(gm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (gm:GroupModel:Model {gmid: "gmid", name: "GroupModel"})
MERGE (partner_deployment_user)-[:CAN_UPDATE]->(gm);
MATCH (partner_lighting_user:Role {rolename: "partner_lighting_user"}), (gm:GroupModel:Model {gmid: "gmid", name: "GroupModel"})
MERGE (partner_lighting_user)-[:CAN_UPDATE]->(gm);
MATCH (partner_sensor_user:Role {rolename: "partner_sensor_user"}), (gm:GroupModel:Model {gmid: "gmid", name: "GroupModel"})
MERGE (partner_sensor_user)-[:CAN_UPDATE]->(gm);
MATCH (partner_api:Role {rolename: "partner_api"}), (gm:GroupModel:Model {gmid: "gmid", name: "GroupModel"})
MERGE (partner_api)-[:CAN_UPDATE]->(gm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (gm:GroupModel:Model {gmid: "gmid", name: "GroupModel"})
MERGE (end_user_admin)-[:CAN_UPDATE]->(gm);
MATCH (end_user_lighting_user:Role {rolename: "end_user_lighting_user"}), (gm:GroupModel:Model {gmid: "gmid", name: "GroupModel"})
MERGE (end_user_lighting_user)-[:CAN_UPDATE]->(gm);
MATCH (end_user_sensor_user:Role {rolename: "end_user_sensor_user"}), (gm:GroupModel:Model {gmid: "gmid", name: "GroupModel"})
MERGE (end_user_sensor_user)-[:CAN_UPDATE]->(gm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (gm:GroupModel:Model {gmid: "gmid", name: "GroupModel"})
MERGE (end_user_api)-[:CAN_UPDATE]->(gm);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (gm:GroupModel:Model {gmid: "gmid", name: "GroupModel"})
MERGE (parking_owner_admin)-[:CAN_UPDATE]->(gm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (gm:GroupModel:Model {gmid: "gmid", name: "GroupModel"})
MERGE (partner_deployment_user)-[:CAN_UPDATE]->(gm);
MATCH (end_user_networking_user:Role {rolename: "end_user_networking_user"}), (gm:GroupModel:Model {gmid: "gmid", name: "GroupModel"})
MERGE (end_user_networking_user)-[:CAN_UPDATE]->(gm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (gm:GroupModel:Model {gmid: "gmid", name: "GroupModel"})
MERGE (sensity_user)-[:CAN_DELETE]->(gm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (gm:GroupModel:Model {gmid: "gmid", name: "GroupModel"})
MERGE (partner_admin)-[:CAN_DELETE]->(gm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (gm:GroupModel:Model {gmid: "gmid", name: "GroupModel"})
MERGE (partner_deployment_user)-[:CAN_DELETE]->(gm);
MATCH (partner_lighting_user:Role {rolename: "partner_lighting_user"}), (gm:GroupModel:Model {gmid: "gmid", name: "GroupModel"})
MERGE (partner_lighting_user)-[:CAN_DELETE]->(gm);
MATCH (partner_sensor_user:Role {rolename: "partner_sensor_user"}), (gm:GroupModel:Model {gmid: "gmid", name: "GroupModel"})
MERGE (partner_sensor_user)-[:CAN_DELETE]->(gm);
MATCH (partner_api:Role {rolename: "partner_api"}), (gm:GroupModel:Model {gmid: "gmid", name: "GroupModel"})
MERGE (partner_api)-[:CAN_DELETE]->(gm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (gm:GroupModel:Model {gmid: "gmid", name: "GroupModel"})
MERGE (end_user_admin)-[:CAN_DELETE]->(gm);
MATCH (end_user_lighting_user:Role {rolename: "end_user_lighting_user"}), (gm:GroupModel:Model {gmid: "gmid", name: "GroupModel"})
MERGE (end_user_lighting_user)-[:CAN_DELETE]->(gm);
MATCH (end_user_sensor_user:Role {rolename: "end_user_sensor_user"}), (gm:GroupModel:Model {gmid: "gmid", name: "GroupModel"})
MERGE (end_user_sensor_user)-[:CAN_DELETE]->(gm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (gm:GroupModel:Model {gmid: "gmid", name: "GroupModel"})
MERGE (end_user_api)-[:CAN_DELETE]->(gm);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (gm:GroupModel:Model {gmid: "gmid", name: "GroupModel"})
MERGE (parking_owner_admin)-[:CAN_DELETE]->(gm);
MATCH (end_user_networking_user:Role {rolename: "end_user_networking_user"}), (gm:GroupModel:Model {gmid: "gmid", name: "GroupModel"})
MERGE (end_user_networking_user)-[:CAN_DELETE]->(gm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (gm:GroupModel:Model {gmid: "gmid", name: "GroupModel"})
MERGE (sensity_user)-[:CAN_READ]->(gm);
MATCH (sensity_read_only:Role:Admin {rolename: "sensity_read_only"}), (gm:GroupModel:Model {gmid: "gmid", name: "GroupModel"})
MERGE (sensity_read_only)-[:CAN_READ]->(gm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (gm:GroupModel:Model {gmid: "gmid", name: "GroupModel"})
MERGE (partner_admin)-[:CAN_READ]->(gm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (gm:GroupModel:Model {gmid: "gmid", name: "GroupModel"})
MERGE (partner_deployment_user)-[:CAN_READ]->(gm);
MATCH (partner_lighting_user:Role {rolename: "partner_lighting_user"}), (gm:GroupModel:Model {gmid: "gmid", name: "GroupModel"})
MERGE (partner_lighting_user)-[:CAN_READ]->(gm);
MATCH (partner_sensor_user:Role {rolename: "partner_sensor_user"}), (gm:GroupModel:Model {gmid: "gmid", name: "GroupModel"})
MERGE (partner_sensor_user)-[:CAN_READ]->(gm);
MATCH (partner_networking_user:Role {rolename: "partner_networking_user"}), (gm:GroupModel:Model {gmid: "gmid", name: "GroupModel"})
MERGE (partner_networking_user)-[:CAN_READ]->(gm);
MATCH (partner_read_only:Role {rolename: "partner_read_only"}), (gm:GroupModel:Model {gmid: "gmid", name: "GroupModel"})
MERGE (partner_read_only)-[:CAN_READ]->(gm);
MATCH (partner_api:Role {rolename: "partner_api"}), (gm:GroupModel:Model {gmid: "gmid", name: "GroupModel"})
MERGE (partner_api)-[:CAN_READ]->(gm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (gm:GroupModel:Model {gmid: "gmid", name: "GroupModel"})
MERGE (end_user_admin)-[:CAN_READ]->(gm);
MATCH (end_user_lighting_user:Role {rolename: "end_user_lighting_user"}), (gm:GroupModel:Model {gmid: "gmid", name: "GroupModel"})
MERGE (end_user_lighting_user)-[:CAN_READ]->(gm);
MATCH (end_user_sensor_user:Role {rolename: "end_user_sensor_user"}), (gm:GroupModel:Model {gmid: "gmid", name: "GroupModel"})
MERGE (end_user_sensor_user)-[:CAN_READ]->(gm);
MATCH (end_user_networking_user:Role {rolename: "end_user_networking_user"}), (gm:GroupModel:Model {gmid: "gmid", name: "GroupModel"})
MERGE (end_user_networking_user)-[:CAN_READ]->(gm);
MATCH (end_user_read_only:Role {rolename: "end_user_read_only"}), (gm:GroupModel:Model {gmid: "gmid", name: "GroupModel"})
MERGE (end_user_read_only)-[:CAN_READ]->(gm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (gm:GroupModel:Model {gmid: "gmid", name: "GroupModel"})
MERGE (end_user_api)-[:CAN_READ]->(gm);
MATCH (installer:Role {rolename: "installer"}), (gm:GroupModel:Model {gmid: "gmid", name: "GroupModel"})
MERGE (installer)-[:CAN_READ]->(gm);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (gm:GroupModel:Model {gmid: "gmid", name: "GroupModel"})
MERGE (parking_owner_admin)-[:CAN_READ]->(gm);
MATCH (parking_manager:Role {rolename: "parking_manager"}), (gm:GroupModel:Model {gmid: "gmid", name: "GroupModel"})
MERGE (parking_manager)-[:CAN_READ]->(gm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (gm:GroupModel:Model {gmid: "gmid", name: "GroupModel"})
MERGE (partner_deployment_user)-[:CAN_READ]->(gm);
MATCH (policy_authority:Role {rolename: "policy_authority"}), (gm:GroupModel:Model {gmid: "gmid", name: "GroupModel"})
MERGE (policy_authority)-[:CAN_READ]->(gm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (lcm:LicenseModel:Model {lcmid: "lcmid", name: "LicenseModel"})
MERGE (sensity_user)-[:CAN_CREATE]->(lcm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (lcm:LicenseModel:Model {lcmid: "lcmid", name: "LicenseModel"})
MERGE (partner_admin)-[:CAN_CREATE]->(lcm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (lcm:LicenseModel:Model {lcmid: "lcmid", name: "LicenseModel"})
MERGE (end_user_admin)-[:CAN_CREATE]->(lcm);
MATCH (sensity_read_only:Role:Admin {rolename: "sensity_read_only"}), (lcm:LicenseModel:Model {lcmid: "lcmid", name: "LicenseModel"})
MERGE (sensity_read_only)-[:CAN_READ]->(lcm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (lcm:LicenseModel:Model {lcmid: "lcmid", name: "LicenseModel"})
MERGE (sensity_user)-[:CAN_READ]->(lcm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (lcm:LicenseModel:Model {lcmid: "lcmid", name: "LicenseModel"})
MERGE (partner_admin)-[:CAN_READ]->(lcm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (lcm:LicenseModel:Model {lcmid: "lcmid", name: "LicenseModel"})
MERGE (end_user_admin)-[:CAN_READ]->(lcm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (lcm:LicenseModel:Model {lcmid: "lcmid", name: "LicenseModel"})
MERGE (sensity_user)-[:CAN_READ_USAGE]->(lcm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (lcm:LicenseModel:Model {lcmid: "lcmid", name: "LicenseModel"})
MERGE (partner_admin)-[:CAN_READ_USAGE]->(lcm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (lcm:LicenseModel:Model {lcmid: "lcmid", name: "LicenseModel"})
MERGE (end_user_admin)-[:CAN_READ_USAGE]->(lcm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (lcm:LicenseModel:Model {lcmid: "lcmid", name: "LicenseModel"})
MERGE (sensity_user)-[:CAN_DELETE]->(lcm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (lcm:LicenseModel:Model {lcmid: "lcmid", name: "LicenseModel"})
MERGE (partner_admin)-[:CAN_DELETE]->(lcm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (lcm:LicenseModel:Model {lcmid: "lcmid", name: "LicenseModel"})
MERGE (end_user_admin)-[:CAN_DELETE]->(lcm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (scm:ScheduleModel:Model {scmid: "scmid", name: "ScheduleModel"})
MERGE (sensity_user)-[:CAN_CREATE]->(scm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (scm:ScheduleModel:Model {scmid: "scmid", name: "ScheduleModel"})
MERGE (partner_admin)-[:CAN_CREATE]->(scm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (scm:ScheduleModel:Model {scmid: "scmid", name: "ScheduleModel"})
MERGE (partner_deployment_user)-[:CAN_CREATE]->(scm);
MATCH (partner_lighting_user:Role {rolename: "partner_lighting_user"}), (scm:ScheduleModel:Model {scmid: "scmid", name: "ScheduleModel"})
MERGE (partner_lighting_user)-[:CAN_CREATE]->(scm);
MATCH (partner_api:Role {rolename: "partner_api"}), (scm:ScheduleModel:Model {scmid: "scmid", name: "ScheduleModel"})
MERGE (partner_api)-[:CAN_CREATE]->(scm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (scm:ScheduleModel:Model {scmid: "scmid", name: "ScheduleModel"})
MERGE (end_user_admin)-[:CAN_CREATE]->(scm);
MATCH (end_user_lighting_user:Role {rolename: "end_user_lighting_user"}), (scm:ScheduleModel:Model {scmid: "scmid", name: "ScheduleModel"})
MERGE (end_user_lighting_user)-[:CAN_CREATE]->(scm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (scm:ScheduleModel:Model {scmid: "scmid", name: "ScheduleModel"})
MERGE (end_user_api)-[:CAN_CREATE]->(scm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (scm:ScheduleModel:Model {scmid: "scmid", name: "ScheduleModel"})
MERGE (sensity_user)-[:CAN_UPDATE]->(scm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (scm:ScheduleModel:Model {scmid: "scmid", name: "ScheduleModel"})
MERGE (partner_admin)-[:CAN_UPDATE]->(scm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (scm:ScheduleModel:Model {scmid: "scmid", name: "ScheduleModel"})
MERGE (partner_deployment_user)-[:CAN_UPDATE]->(scm);
MATCH (partner_lighting_user:Role {rolename: "partner_lighting_user"}), (scm:ScheduleModel:Model {scmid: "scmid", name: "ScheduleModel"})
MERGE (partner_lighting_user)-[:CAN_UPDATE]->(scm);
MATCH (partner_api:Role {rolename: "partner_api"}), (scm:ScheduleModel:Model {scmid: "scmid", name: "ScheduleModel"})
MERGE (partner_api)-[:CAN_UPDATE]->(scm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (scm:ScheduleModel:Model {scmid: "scmid", name: "ScheduleModel"})
MERGE (end_user_admin)-[:CAN_UPDATE]->(scm);
MATCH (end_user_lighting_user:Role {rolename: "end_user_lighting_user"}), (scm:ScheduleModel:Model {scmid: "scmid", name: "ScheduleModel"})
MERGE (end_user_lighting_user)-[:CAN_UPDATE]->(scm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (scm:ScheduleModel:Model {scmid: "scmid", name: "ScheduleModel"})
MERGE (end_user_api)-[:CAN_UPDATE]->(scm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (scm:ScheduleModel:Model {scmid: "scmid", name: "ScheduleModel"})
MERGE (sensity_user)-[:CAN_DELETE]->(scm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (scm:ScheduleModel:Model {scmid: "scmid", name: "ScheduleModel"})
MERGE (partner_admin)-[:CAN_DELETE]->(scm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (scm:ScheduleModel:Model {scmid: "scmid", name: "ScheduleModel"})
MERGE (partner_deployment_user)-[:CAN_DELETE]->(scm);
MATCH (partner_lighting_user:Role {rolename: "partner_lighting_user"}), (scm:ScheduleModel:Model {scmid: "scmid", name: "ScheduleModel"})
MERGE (partner_lighting_user)-[:CAN_DELETE]->(scm);
MATCH (partner_api:Role {rolename: "partner_api"}), (scm:ScheduleModel:Model {scmid: "scmid", name: "ScheduleModel"})
MERGE (partner_api)-[:CAN_DELETE]->(scm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (scm:ScheduleModel:Model {scmid: "scmid", name: "ScheduleModel"})
MERGE (end_user_admin)-[:CAN_DELETE]->(scm);
MATCH (end_user_lighting_user:Role {rolename: "end_user_lighting_user"}), (scm:ScheduleModel:Model {scmid: "scmid", name: "ScheduleModel"})
MERGE (end_user_lighting_user)-[:CAN_DELETE]->(scm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (scm:ScheduleModel:Model {scmid: "scmid", name: "ScheduleModel"})
MERGE (end_user_api)-[:CAN_DELETE]->(scm);
MATCH (sensity_read_only:Role:Admin {rolename: "sensity_read_only"}), (scm:ScheduleModel:Model {scmid: "scmid", name: "ScheduleModel"})
MERGE (sensity_read_only)-[:CAN_READ]->(scm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (scm:ScheduleModel:Model {scmid: "scmid", name: "ScheduleModel"})
MERGE (sensity_user)-[:CAN_READ]->(scm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (scm:ScheduleModel:Model {scmid: "scmid", name: "ScheduleModel"})
MERGE (partner_admin)-[:CAN_READ]->(scm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (scm:ScheduleModel:Model {scmid: "scmid", name: "ScheduleModel"})
MERGE (partner_deployment_user)-[:CAN_READ]->(scm);
MATCH (partner_lighting_user:Role {rolename: "partner_lighting_user"}), (scm:ScheduleModel:Model {scmid: "scmid", name: "ScheduleModel"})
MERGE (partner_lighting_user)-[:CAN_READ]->(scm);
MATCH (partner_sensor_user:Role {rolename: "partner_sensor_user"}), (scm:ScheduleModel:Model {scmid: "scmid", name: "ScheduleModel"})
MERGE (partner_sensor_user)-[:CAN_READ]->(scm);
MATCH (partner_read_only:Role {rolename: "partner_read_only"}), (scm:ScheduleModel:Model {scmid: "scmid", name: "ScheduleModel"})
MERGE (partner_read_only)-[:CAN_READ]->(scm);
MATCH (partner_api:Role {rolename: "partner_api"}), (scm:ScheduleModel:Model {scmid: "scmid", name: "ScheduleModel"})
MERGE (partner_api)-[:CAN_READ]->(scm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (scm:ScheduleModel:Model {scmid: "scmid", name: "ScheduleModel"})
MERGE (end_user_admin)-[:CAN_READ]->(scm);
MATCH (end_user_lighting_user:Role {rolename: "end_user_lighting_user"}), (scm:ScheduleModel:Model {scmid: "scmid", name: "ScheduleModel"})
MERGE (end_user_lighting_user)-[:CAN_READ]->(scm);
MATCH (end_user_sensor_user:Role {rolename: "end_user_sensor_user"}), (scm:ScheduleModel:Model {scmid: "scmid", name: "ScheduleModel"})
MERGE (end_user_sensor_user)-[:CAN_READ]->(scm);
MATCH (end_user_networking_user:Role {rolename: "end_user_networking_user"}), (scm:ScheduleModel:Model {scmid: "scmid", name: "ScheduleModel"})
MERGE (end_user_networking_user)-[:CAN_READ]->(scm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (scm:ScheduleModel:Model {scmid: "scmid", name: "ScheduleModel"})
MERGE (end_user_api)-[:CAN_READ]->(scm);
MATCH (installer:Role {rolename: "installer"}), (scm:ScheduleModel:Model {scmid: "scmid", name: "ScheduleModel"})
MERGE (installer)-[:CAN_READ]->(scm);
MATCH (end_user_read_only:Role {rolename: "end_user_read_only"}), (scm:ScheduleModel:Model {scmid: "scmid", name: "ScheduleModel"})
MERGE (end_user_read_only)-[:CAN_READ]->(scm);
MATCH (partner_networking_user:Role {rolename: "partner_networking_user"}), (scm:ScheduleModel:Model {scmid: "scmid", name: "ScheduleModel"})
MERGE (partner_networking_user)-[:CAN_READ]->(scm);
MATCH (parking_manager:Role {rolename: "parking_manager"}), (scm:ScheduleModel:Model {scmid: "scmid", name: "ScheduleModel"})
MERGE (parking_manager)-[:CAN_READ]->(scm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (scm:ScheduleModel:Model {scmid: "scmid", name: "ScheduleModel"})
MERGE (sensity_user)-[:CAN_APPLY]->(scm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (scm:ScheduleModel:Model {scmid: "scmid", name: "ScheduleModel"})
MERGE (partner_admin)-[:CAN_APPLY]->(scm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (scm:ScheduleModel:Model {scmid: "scmid", name: "ScheduleModel"})
MERGE (partner_deployment_user)-[:CAN_APPLY]->(scm);
MATCH (partner_lighting_user:Role {rolename: "partner_lighting_user"}), (scm:ScheduleModel:Model {scmid: "scmid", name: "ScheduleModel"})
MERGE (partner_lighting_user)-[:CAN_APPLY]->(scm);
MATCH (partner_api:Role {rolename: "partner_api"}), (scm:ScheduleModel:Model {scmid: "scmid", name: "ScheduleModel"})
MERGE (partner_api)-[:CAN_APPLY]->(scm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (scm:ScheduleModel:Model {scmid: "scmid", name: "ScheduleModel"})
MERGE (end_user_admin)-[:CAN_APPLY]->(scm);
MATCH (end_user_lighting_user:Role {rolename: "end_user_lighting_user"}), (scm:ScheduleModel:Model {scmid: "scmid", name: "ScheduleModel"})
MERGE (end_user_lighting_user)-[:CAN_APPLY]->(scm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (scm:ScheduleModel:Model {scmid: "scmid", name: "ScheduleModel"})
MERGE (end_user_api)-[:CAN_APPLY]->(scm);
MATCH (partner_read_only:Role {rolename: "partner_read_only"}), (scm:ScheduleModel:Model {scmid: "scmid", name: "ScheduleModel"})
MERGE (partner_read_only)-[:CAN_APPLY]->(scm);
MATCH (end_user_read_only:Role {rolename: "end_user_read_only"}), (scm:ScheduleModel:Model {scmid: "scmid", name: "ScheduleModel"})
MERGE (end_user_read_only)-[:CAN_APPLY]->(scm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (sensity_user)-[:CAN_ASSIGN_TO_LS_GROUP]->(nm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (partner_admin)-[:CAN_ASSIGN_TO_LS_GROUP]->(nm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (partner_deployment_user)-[:CAN_ASSIGN_TO_LS_GROUP]->(nm);
MATCH (partner_api:Role {rolename: "partner_api"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (partner_api)-[:CAN_ASSIGN_TO_LS_GROUP]->(nm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (end_user_admin)-[:CAN_ASSIGN_TO_LS_GROUP]->(nm);
MATCH (end_user_lighting_user:Role {rolename: "end_user_lighting_user"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (end_user_lighting_user)-[:CAN_ASSIGN_TO_LS_GROUP]->(nm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (end_user_api)-[:CAN_ASSIGN_TO_LS_GROUP]->(nm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (sensity_user)-[:CAN_ASSIGN_LM_DETECTION_TO_GROUP]->(nm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (partner_admin)-[:CAN_ASSIGN_LM_DETECTION_TO_GROUP]->(nm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (partner_deployment_user)-[:CAN_ASSIGN_LM_DETECTION_TO_GROUP]->(nm);
MATCH (partner_api:Role {rolename: "partner_api"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (partner_api)-[:CAN_ASSIGN_LM_DETECTION_TO_GROUP]->(nm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (end_user_admin)-[:CAN_ASSIGN_LM_DETECTION_TO_GROUP]->(nm);
MATCH (end_user_lighting_user:Role {rolename: "end_user_lighting_user"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (end_user_lighting_user)-[:CAN_ASSIGN_LM_DETECTION_TO_GROUP]->(nm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (end_user_api)-[:CAN_ASSIGN_LM_DETECTION_TO_GROUP]->(nm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (sensity_user)-[:CAN_MANAGE_LIGHTS]->(nm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (partner_admin)-[:CAN_MANAGE_LIGHTS]->(nm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (partner_deployment_user)-[:CAN_MANAGE_LIGHTS]->(nm);
MATCH (partner_lighting_user:Role {rolename: "partner_lighting_user"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (partner_lighting_user)-[:CAN_MANAGE_LIGHTS]->(nm);
MATCH (partner_api:Role {rolename: "partner_api"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (partner_api)-[:CAN_MANAGE_LIGHTS]->(nm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (end_user_admin)-[:CAN_MANAGE_LIGHTS]->(nm);
MATCH (end_user_lighting_user:Role {rolename: "end_user_lighting_user"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (end_user_lighting_user)-[:CAN_MANAGE_LIGHTS]->(nm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (end_user_api)-[:CAN_MANAGE_LIGHTS]->(nm);
MATCH (installer:Role {rolename: "installer"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (installer)-[:CAN_MANAGE_LIGHTS]->(nm);
MATCH (sensity_admin:Role:Admin {rolename: "sensity_admin"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (sensity_admin)-[:CAN_READ]->(nm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (sensity_user)-[:CAN_READ]->(nm);
MATCH (sensity_read_only:Role:Admin {rolename: "sensity_read_only"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (sensity_read_only)-[:CAN_READ]->(nm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (partner_admin)-[:CAN_READ]->(nm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (partner_deployment_user)-[:CAN_READ]->(nm);
MATCH (partner_lighting_user:Role {rolename: "partner_lighting_user"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (partner_lighting_user)-[:CAN_READ]->(nm);
MATCH (partner_sensor_user:Role {rolename: "partner_sensor_user"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (partner_sensor_user)-[:CAN_READ]->(nm);
MATCH (partner_api:Role {rolename: "partner_api"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (partner_api)-[:CAN_READ]->(nm);
MATCH (partner_read_only:Role {rolename: "partner_read_only"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (partner_read_only)-[:CAN_READ]->(nm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (end_user_admin)-[:CAN_READ]->(nm);
MATCH (end_user_lighting_user:Role {rolename: "end_user_lighting_user"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (end_user_lighting_user)-[:CAN_READ]->(nm);
MATCH (end_user_read_only:Role {rolename: "end_user_read_only"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (end_user_read_only)-[:CAN_READ]->(nm);
MATCH (end_user_sensor_user:Role {rolename: "end_user_sensor_user"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (end_user_sensor_user)-[:CAN_READ]->(nm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (end_user_api)-[:CAN_READ]->(nm);
MATCH (installer:Role {rolename: "installer"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (installer)-[:CAN_READ]->(nm);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (parking_owner_admin)-[:CAN_READ]->(nm);
MATCH (policy_authority:Role {rolename: "policy_authority"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (policy_authority)-[:CAN_READ]->(nm);
MATCH (parking_manager:Role {rolename: "parking_manager"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (parking_manager)-[:CAN_READ]->(nm);
MATCH (end_user_networking_user:Role {rolename: "end_user_networking_user"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (end_user_networking_user)-[:CAN_READ]->(nm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (partner_deployment_user)-[:CAN_READ]->(nm);
MATCH (partner_networking_user:Role {rolename: "partner_networking_user"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (partner_networking_user)-[:CAN_READ]->(nm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (sensity_user)-[:CAN_CREATE]->(nm);
MATCH (sensity_admin:Role:Admin {rolename: "sensity_admin"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (sensity_admin)-[:CAN_CREATE]->(nm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (partner_admin)-[:CAN_CREATE]->(nm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (partner_deployment_user)-[:CAN_CREATE]->(nm);
MATCH (partner_api:Role {rolename: "partner_api"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (partner_api)-[:CAN_CREATE]->(nm);
MATCH (installer:Role {rolename: "installer"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (installer)-[:CAN_CREATE]->(nm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (sensity_user)-[:CAN_CHANGE]->(nm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (partner_admin)-[:CAN_CHANGE]->(nm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (partner_deployment_user)-[:CAN_CHANGE]->(nm);
MATCH (partner_lighting_user:Role {rolename: "partner_lighting_user"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (partner_lighting_user)-[:CAN_CHANGE]->(nm);
MATCH (partner_sensor_user:Role {rolename: "partner_sensor_user"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (partner_sensor_user)-[:CAN_CHANGE]->(nm);
MATCH (partner_api:Role {rolename: "partner_api"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (partner_api)-[:CAN_CHANGE]->(nm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (end_user_admin)-[:CAN_CHANGE]->(nm);
MATCH (end_user_lighting_user:Role {rolename: "end_user_lighting_user"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (end_user_lighting_user)-[:CAN_CHANGE]->(nm);
MATCH (end_user_sensor_user:Role {rolename: "end_user_sensor_user"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (end_user_sensor_user)-[:CAN_CHANGE]->(nm);
MATCH (installer:Role {rolename: "installer"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (installer)-[:CAN_CHANGE]->(nm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (end_user_api)-[:CAN_CHANGE]->(nm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (sensity_user)-[:CAN_DEACTIVATE]->(nm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (partner_admin)-[:CAN_DEACTIVATE]->(nm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (partner_deployment_user)-[:CAN_DEACTIVATE]->(nm);
MATCH (partner_api:Role {rolename: "partner_api"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (partner_api)-[:CAN_DEACTIVATE]->(nm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (end_user_admin)-[:CAN_DEACTIVATE]->(nm);
MATCH (end_user_lighting_user:Role {rolename: "end_user_lighting_user"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (end_user_lighting_user)-[:CAN_DEACTIVATE]->(nm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (end_user_api)-[:CAN_DEACTIVATE]->(nm);
MATCH (installer:Role {rolename: "installer"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (installer)-[:CAN_DEACTIVATE]->(nm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (sensity_user)-[:CAN_OPERATE]->(nm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (partner_admin)-[:CAN_OPERATE]->(nm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (partner_deployment_user)-[:CAN_OPERATE]->(nm);
MATCH (partner_api:Role {rolename: "partner_api"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (partner_api)-[:CAN_OPERATE]->(nm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (end_user_admin)-[:CAN_OPERATE]->(nm);
MATCH (end_user_lighting_user:Role {rolename: "end_user_lighting_user"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (end_user_lighting_user)-[:CAN_OPERATE]->(nm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (end_user_api)-[:CAN_OPERATE]->(nm);
MATCH (installer:Role {rolename: "installer"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (installer)-[:CAN_OPERATE]->(nm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (sensity_user)-[:CAN_ASSIGN_TO_ORGS]->(nm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (partner_admin)-[:CAN_ASSIGN_TO_ORGS]->(nm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (partner_deployment_user)-[:CAN_ASSIGN_TO_ORGS]->(nm);
MATCH (partner_api:Role {rolename: "partner_api"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (partner_api)-[:CAN_ASSIGN_TO_ORGS]->(nm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (end_user_admin)-[:CAN_ASSIGN_TO_ORGS]->(nm);
MATCH (end_user_lighting_user:Role {rolename: "end_user_lighting_user"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (end_user_lighting_user)-[:CAN_ASSIGN_TO_ORGS]->(nm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (end_user_api)-[:CAN_ASSIGN_TO_ORGS]->(nm);
MATCH (installer:Role {rolename: "installer"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (installer)-[:CAN_ASSIGN_TO_ORGS]->(nm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (sensity_user)-[:CAN_ASSIGN_TO_SITES]->(nm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (partner_admin)-[:CAN_ASSIGN_TO_SITES]->(nm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (partner_deployment_user)-[:CAN_ASSIGN_TO_SITES]->(nm);
MATCH (partner_api:Role {rolename: "partner_api"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (partner_api)-[:CAN_ASSIGN_TO_SITES]->(nm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (end_user_admin)-[:CAN_ASSIGN_TO_SITES]->(nm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (end_user_api)-[:CAN_ASSIGN_TO_SITES]->(nm);
MATCH (installer:Role {rolename: "installer"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (installer)-[:CAN_ASSIGN_TO_SITES]->(nm);
MATCH (sensity_admin:Role:Admin {rolename: "sensity_admin"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (sensity_admin)-[:CAN_ASSIGN_TO_SITES]->(nm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (sensity_user)-[:CAN_ASSIGN_TO_GROUPS]->(nm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (partner_admin)-[:CAN_ASSIGN_TO_GROUPS]->(nm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (partner_deployment_user)-[:CAN_ASSIGN_TO_GROUPS]->(nm);
MATCH (partner_api:Role {rolename: "partner_api"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (partner_api)-[:CAN_ASSIGN_TO_GROUPS]->(nm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (end_user_admin)-[:CAN_ASSIGN_TO_GROUPS]->(nm);
MATCH (end_user_lighting_user:Role {rolename: "end_user_lighting_user"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (end_user_lighting_user)-[:CAN_ASSIGN_TO_GROUPS]->(nm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (end_user_api)-[:CAN_ASSIGN_TO_GROUPS]->(nm);
MATCH (installer:Role {rolename: "installer"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (installer)-[:CAN_ASSIGN_TO_GROUPS]->(nm);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (parking_owner_admin)-[:CAN_ASSIGN_TO_GROUPS]->(nm);
MATCH (policy_authority:Role {rolename: "policy_authority"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (policy_authority)-[:CAN_ASSIGN_TO_GROUPS]->(nm);
MATCH (parking_manager:Role {rolename: "parking_manager"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (parking_manager)-[:CAN_ASSIGN_TO_GROUPS]->(nm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (partner_admin)-[:CAN_UPGRADE_FIRMWARE]->(nm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (partner_deployment_user)-[:CAN_UPGRADE_FIRMWARE]->(nm);
MATCH (partner_api:Role {rolename: "partner_api"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (partner_api)-[:CAN_UPGRADE_FIRMWARE]->(nm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (end_user_admin)-[:CAN_UPGRADE_FIRMWARE]->(nm);
MATCH (end_user_lighting_user:Role {rolename: "end_user_lighting_user"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (end_user_lighting_user)-[:CAN_UPGRADE_FIRMWARE]->(nm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (end_user_api)-[:CAN_UPGRADE_FIRMWARE]->(nm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (sensity_user)-[:CAN_ASSIGN_FIXTURE]->(nm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (partner_admin)-[:CAN_ASSIGN_FIXTURE]->(nm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (partner_deployment_user)-[:CAN_ASSIGN_FIXTURE]->(nm);
MATCH (partner_api:Role {rolename: "partner_api"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (partner_api)-[:CAN_ASSIGN_FIXTURE]->(nm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (end_user_admin)-[:CAN_ASSIGN_FIXTURE]->(nm);
MATCH (end_user_lighting_user:Role {rolename: "end_user_lighting_user"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (end_user_lighting_user)-[:CAN_ASSIGN_FIXTURE]->(nm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (end_user_api)-[:CAN_ASSIGN_FIXTURE]->(nm);
MATCH (installer:Role {rolename: "installer"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (installer)-[:CAN_ASSIGN_FIXTURE]->(nm);
MATCH (partner_lighting_user:Role {rolename: "partner_lighting_user"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (partner_lighting_user)-[:CAN_ASSIGN_FIXTURE]->(nm);
MATCH (end_user_sensor_user:Role {rolename: "end_user_sensor_user"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (end_user_sensor_user)-[:CAN_ASSIGN_FIXTURE]->(nm);
MATCH (sensity_admin:Role:Admin {rolename: "sensity_admin"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (sensity_admin)-[:CAN_SEARCH]->(nm);
MATCH (sensity_read_only:Role:Admin {rolename: "sensity_read_only"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (sensity_read_only)-[:CAN_SEARCH]->(nm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (sensity_user)-[:CAN_SEARCH]->(nm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (partner_admin)-[:CAN_SEARCH]->(nm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (partner_deployment_user)-[:CAN_SEARCH]->(nm);
MATCH (partner_read_only:Role {rolename: "partner_read_only"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (partner_read_only)-[:CAN_SEARCH]->(nm);
MATCH (partner_api:Role {rolename: "partner_api"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (partner_api)-[:CAN_SEARCH]->(nm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (end_user_admin)-[:CAN_SEARCH]->(nm);
MATCH (end_user_lighting_user:Role {rolename: "end_user_lighting_user"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (end_user_lighting_user)-[:CAN_SEARCH]->(nm);
MATCH (end_user_read_only:Role {rolename: "end_user_read_only"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (end_user_read_only)-[:CAN_SEARCH]->(nm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (end_user_api)-[:CAN_SEARCH]->(nm);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (parking_owner_admin)-[:CAN_SEARCH]->(nm);
MATCH (policy_authority:Role {rolename: "policy_authority"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (policy_authority)-[:CAN_SEARCH]->(nm);
MATCH (parking_manager:Role {rolename: "parking_manager"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (parking_manager)-[:CAN_SEARCH]->(nm);
MATCH (end_user_networking_user:Role {rolename: "end_user_networking_user"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (end_user_networking_user)-[:CAN_SEARCH]->(nm);
MATCH (sensity_admin:Role:Admin {rolename: "sensity_admin"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (sensity_admin)-[:CAN_RETRIEVE_LOG_FILE]->(nm);
MATCH (sensity_read_only:Role:Admin {rolename: "sensity_read_only"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (sensity_read_only)-[:CAN_RETRIEVE_LOG_FILE]->(nm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (sensity_user)-[:CAN_RETRIEVE_LOG_FILE]->(nm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (partner_admin)-[:CAN_RETRIEVE_LOG_FILE]->(nm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (partner_deployment_user)-[:CAN_RETRIEVE_LOG_FILE]->(nm);
MATCH (partner_read_only:Role {rolename: "partner_read_only"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (partner_read_only)-[:CAN_RETRIEVE_LOG_FILE]->(nm);
MATCH (partner_api:Role {rolename: "partner_api"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (partner_api)-[:CAN_RETRIEVE_LOG_FILE]->(nm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (end_user_admin)-[:CAN_RETRIEVE_LOG_FILE]->(nm);
MATCH (end_user_lighting_user:Role {rolename: "end_user_lighting_user"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (end_user_lighting_user)-[:CAN_RETRIEVE_LOG_FILE]->(nm);
MATCH (end_user_read_only:Role {rolename: "end_user_read_only"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (end_user_read_only)-[:CAN_RETRIEVE_LOG_FILE]->(nm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (end_user_api)-[:CAN_RETRIEVE_LOG_FILE]->(nm);
MATCH (installer:Role {rolename: "installer"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (installer)-[:CAN_RETRIEVE_LOG_FILE]->(nm);
MATCH (sensity_admin:Role:Admin {rolename: "sensity_admin"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (sensity_admin)-[:CAN_RETRIEVE_ALARM]->(nm);
MATCH (sensity_read_only:Role:Admin {rolename: "sensity_read_only"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (sensity_read_only)-[:CAN_RETRIEVE_ALARM]->(nm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (sensity_user)-[:CAN_RETRIEVE_ALARM]->(nm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (partner_admin)-[:CAN_RETRIEVE_ALARM]->(nm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (partner_deployment_user)-[:CAN_RETRIEVE_ALARM]->(nm);
MATCH (partner_read_only:Role {rolename: "partner_read_only"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (partner_read_only)-[:CAN_RETRIEVE_ALARM]->(nm);
MATCH (partner_api:Role {rolename: "partner_api"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (partner_api)-[:CAN_RETRIEVE_ALARM]->(nm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (end_user_admin)-[:CAN_RETRIEVE_ALARM]->(nm);
MATCH (end_user_lighting_user:Role {rolename: "end_user_lighting_user"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (end_user_lighting_user)-[:CAN_RETRIEVE_ALARM]->(nm);
MATCH (end_user_read_only:Role {rolename: "end_user_read_only"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (end_user_read_only)-[:CAN_RETRIEVE_ALARM]->(nm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (end_user_api)-[:CAN_RETRIEVE_ALARM]->(nm);
MATCH (installer:Role {rolename: "installer"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (installer)-[:CAN_RETRIEVE_ALARM]->(nm);
MATCH (end_user_networking_user:Role {rolename: "end_user_networking_user"}), (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
MERGE (end_user_networking_user)-[:CAN_RETRIEVE_ALARM]->(nm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (cm:ConfigModel:Model {cmid: "cmid", name: "ConfigModel"})
MERGE (partner_admin)-[:CAN_CREATE]->(cm);
MATCH (partner_api:Role {rolename: "partner_api"}), (cm:ConfigModel:Model {cmid: "cmid", name: "ConfigModel"})
MERGE (partner_api)-[:CAN_CREATE]->(cm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (cm:ConfigModel:Model {cmid: "cmid", name: "ConfigModel"})
MERGE (sensity_user)-[:CAN_CREATE]->(cm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (cm:ConfigModel:Model {cmid: "cmid", name: "ConfigModel"})
MERGE (sensity_user)-[:CAN_UPDATE]->(cm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (cm:ConfigModel:Model {cmid: "cmid", name: "ConfigModel"})
MERGE (partner_admin)-[:CAN_UPDATE]->(cm);
MATCH (partner_api:Role {rolename: "partner_api"}), (cm:ConfigModel:Model {cmid: "cmid", name: "ConfigModel"})
MERGE (partner_api)-[:CAN_UPDATE]->(cm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (cm:ConfigModel:Model {cmid: "cmid", name: "ConfigModel"})
MERGE (sensity_user)-[:CAN_DEACTIVATE]->(cm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (cm:ConfigModel:Model {cmid: "cmid", name: "ConfigModel"})
MERGE (partner_admin)-[:CAN_DEACTIVATE]->(cm);
MATCH (partner_api:Role {rolename: "partner_api"}), (cm:ConfigModel:Model {cmid: "cmid", name: "ConfigModel"})
MERGE (partner_api)-[:CAN_DEACTIVATE]->(cm);
MATCH (sensity_read_only:Role:Admin {rolename: "sensity_read_only"}), (cm:ConfigModel:Model {cmid: "cmid", name: "ConfigModel"})
MERGE (sensity_read_only)-[:CAN_READ]->(cm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (cm:ConfigModel:Model {cmid: "cmid", name: "ConfigModel"})
MERGE (sensity_user)-[:CAN_READ]->(cm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (cm:ConfigModel:Model {cmid: "cmid", name: "ConfigModel"})
MERGE (partner_admin)-[:CAN_READ]->(cm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (cm:ConfigModel:Model {cmid: "cmid", name: "ConfigModel"})
MERGE (partner_deployment_user)-[:CAN_READ]->(cm);
MATCH (partner_lighting_user:Role {rolename: "partner_lighting_user"}), (cm:ConfigModel:Model {cmid: "cmid", name: "ConfigModel"})
MERGE (partner_lighting_user)-[:CAN_READ]->(cm);
MATCH (partner_sensor_user:Role {rolename: "partner_sensor_user"}), (cm:ConfigModel:Model {cmid: "cmid", name: "ConfigModel"})
MERGE (partner_sensor_user)-[:CAN_READ]->(cm);
MATCH (partner_read_only:Role {rolename: "partner_read_only"}), (cm:ConfigModel:Model {cmid: "cmid", name: "ConfigModel"})
MERGE (partner_read_only)-[:CAN_READ]->(cm);
MATCH (partner_api:Role {rolename: "partner_api"}), (cm:ConfigModel:Model {cmid: "cmid", name: "ConfigModel"})
MERGE (partner_api)-[:CAN_READ]->(cm);
MATCH (end_user_networking_user:Role {rolename: "end_user_networking_user"}), (cm:ConfigModel:Model {cmid: "cmid", name: "ConfigModel"})
MERGE (end_user_networking_user)-[:CAN_READ]->(cm);
MATCH (partner_networking_user:Role {rolename: "partner_networking_user"}), (cm:ConfigModel:Model {cmid: "cmid", name: "ConfigModel"})
MERGE (partner_networking_user)-[:CAN_READ]->(cm);
MATCH (parking_manager:Role {rolename: "parking_manager"}), (cm:ConfigModel:Model {cmid: "cmid", name: "ConfigModel"})
MERGE (parking_manager)-[:CAN_READ]->(cm);
MATCH (policy_authority:Role {rolename: "policy_authority"}), (cm:ConfigModel:Model {cmid: "cmid", name: "ConfigModel"})
MERGE (policy_authority)-[:CAN_READ]->(cm);
MATCH (sensity_admin:Role:Admin {rolename: "sensity_admin"}), (cm:ConfigModel:Model {cmid: "cmid", name: "ConfigModel"})
MERGE (sensity_admin)-[:CAN_READ]->(cm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (cm:ConfigModel:Model {cmid: "cmid", name: "ConfigModel"})
MERGE (sensity_user)-[:CAN_APPLY]->(cm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (cm:ConfigModel:Model {cmid: "cmid", name: "ConfigModel"})
MERGE (partner_admin)-[:CAN_APPLY]->(cm);
MATCH (partner_api:Role {rolename: "partner_api"}), (cm:ConfigModel:Model {cmid: "cmid", name: "ConfigModel"})
MERGE (partner_api)-[:CAN_APPLY]->(cm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (cm:ConfigModel:Model {cmid: "cmid", name: "ConfigModel"})
MERGE (end_user_admin)-[:CAN_APPLY]->(cm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (fm:FixtureModel:Model {fmid: "fmid", name: "FixtureModel"})
MERGE (sensity_user)-[:CAN_READ]->(fm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (fm:FixtureModel:Model {fmid: "fmid", name: "FixtureModel"})
MERGE (partner_admin)-[:CAN_READ]->(fm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (fm:FixtureModel:Model {fmid: "fmid", name: "FixtureModel"})
MERGE (partner_deployment_user)-[:CAN_READ]->(fm);
MATCH (partner_lighting_user:Role {rolename: "partner_lighting_user"}), (fm:FixtureModel:Model {fmid: "fmid", name: "FixtureModel"})
MERGE (partner_lighting_user)-[:CAN_READ]->(fm);
MATCH (partner_api:Role {rolename: "partner_api"}), (fm:FixtureModel:Model {fmid: "fmid", name: "FixtureModel"})
MERGE (partner_api)-[:CAN_READ]->(fm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (fm:FixtureModel:Model {fmid: "fmid", name: "FixtureModel"})
MERGE (end_user_admin)-[:CAN_READ]->(fm);
MATCH (end_user_lighting_user:Role {rolename: "end_user_lighting_user"}), (fm:FixtureModel:Model {fmid: "fmid", name: "FixtureModel"})
MERGE (end_user_lighting_user)-[:CAN_READ]->(fm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (fm:FixtureModel:Model {fmid: "fmid", name: "FixtureModel"})
MERGE (end_user_api)-[:CAN_READ]->(fm);
MATCH (installer:Role {rolename: "installer"}), (fm:FixtureModel:Model {fmid: "fmid", name: "FixtureModel"})
MERGE (installer)-[:CAN_READ]->(fm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (fm:FixtureModel:Model {fmid: "fmid", name: "FixtureModel"})
MERGE (partner_deployment_user)-[:CAN_READ]->(fm);
MATCH (sensity_read_only:Role:Admin {rolename: "sensity_read_only"}), (fm:FixtureModel:Model {fmid: "fmid", name: "FixtureModel"})
MERGE (sensity_read_only)-[:CAN_READ]->(fm);
MATCH (partner_read_only:Role {rolename: "partner_read_only"}), (fm:FixtureModel:Model {fmid: "fmid", name: "FixtureModel"})
MERGE (partner_read_only)-[:CAN_READ]->(fm);
MATCH (end_user_read_only:Role {rolename: "end_user_read_only"}), (fm:FixtureModel:Model {fmid: "fmid", name: "FixtureModel"})
MERGE (end_user_read_only)-[:CAN_READ]->(fm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (fm:FixtureModel:Model {fmid: "fmid", name: "FixtureModel"})
MERGE (sensity_user)-[:CAN_CREATE]->(fm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (fm:FixtureModel:Model {fmid: "fmid", name: "FixtureModel"})
MERGE (partner_admin)-[:CAN_CREATE]->(fm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (fm:FixtureModel:Model {fmid: "fmid", name: "FixtureModel"})
MERGE (partner_deployment_user)-[:CAN_CREATE]->(fm);
MATCH (partner_lighting_user:Role {rolename: "partner_lighting_user"}), (fm:FixtureModel:Model {fmid: "fmid", name: "FixtureModel"})
MERGE (partner_lighting_user)-[:CAN_CREATE]->(fm);
MATCH (partner_api:Role {rolename: "partner_api"}), (fm:FixtureModel:Model {fmid: "fmid", name: "FixtureModel"})
MERGE (partner_api)-[:CAN_CREATE]->(fm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (fm:FixtureModel:Model {fmid: "fmid", name: "FixtureModel"})
MERGE (end_user_admin)-[:CAN_CREATE]->(fm);
MATCH (end_user_lighting_user:Role {rolename: "end_user_lighting_user"}), (fm:FixtureModel:Model {fmid: "fmid", name: "FixtureModel"})
MERGE (end_user_lighting_user)-[:CAN_CREATE]->(fm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (fm:FixtureModel:Model {fmid: "fmid", name: "FixtureModel"})
MERGE (end_user_api)-[:CAN_CREATE]->(fm);
MATCH (installer:Role {rolename: "installer"}), (fm:FixtureModel:Model {fmid: "fmid", name: "FixtureModel"})
MERGE (installer)-[:CAN_CREATE]->(fm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (fm:FixtureModel:Model {fmid: "fmid", name: "FixtureModel"})
MERGE (sensity_user)-[:CAN_UPDATE]->(fm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (fm:FixtureModel:Model {fmid: "fmid", name: "FixtureModel"})
MERGE (partner_admin)-[:CAN_UPDATE]->(fm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (fm:FixtureModel:Model {fmid: "fmid", name: "FixtureModel"})
MERGE (partner_deployment_user)-[:CAN_UPDATE]->(fm);
MATCH (partner_lighting_user:Role {rolename: "partner_lighting_user"}), (fm:FixtureModel:Model {fmid: "fmid", name: "FixtureModel"})
MERGE (partner_lighting_user)-[:CAN_UPDATE]->(fm);
MATCH (partner_api:Role {rolename: "partner_api"}), (fm:FixtureModel:Model {fmid: "fmid", name: "FixtureModel"})
MERGE (partner_api)-[:CAN_UPDATE]->(fm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (fm:FixtureModel:Model {fmid: "fmid", name: "FixtureModel"})
MERGE (end_user_admin)-[:CAN_UPDATE]->(fm);
MATCH (end_user_lighting_user:Role {rolename: "end_user_lighting_user"}), (fm:FixtureModel:Model {fmid: "fmid", name: "FixtureModel"})
MERGE (end_user_lighting_user)-[:CAN_UPDATE]->(fm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (fm:FixtureModel:Model {fmid: "fmid", name: "FixtureModel"})
MERGE (end_user_api)-[:CAN_UPDATE]->(fm);
MATCH (installer:Role {rolename: "installer"}), (fm:FixtureModel:Model {fmid: "fmid", name: "FixtureModel"})
MERGE (installer)-[:CAN_UPDATE]->(fm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (fm:FixtureModel:Model {fmid: "fmid", name: "FixtureModel"})
MERGE (partner_deployment_user)-[:CAN_UPDATE]->(fm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (fm:FixtureModel:Model {fmid: "fmid", name: "FixtureModel"})
MERGE (sensity_user)-[:CAN_DELETE]->(fm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (fm:FixtureModel:Model {fmid: "fmid", name: "FixtureModel"})
MERGE (partner_admin)-[:CAN_DELETE]->(fm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (fm:FixtureModel:Model {fmid: "fmid", name: "FixtureModel"})
MERGE (partner_deployment_user)-[:CAN_DELETE]->(fm);
MATCH (partner_lighting_user:Role {rolename: "partner_lighting_user"}), (fm:FixtureModel:Model {fmid: "fmid", name: "FixtureModel"})
MERGE (partner_lighting_user)-[:CAN_DELETE]->(fm);
MATCH (partner_api:Role {rolename: "partner_api"}), (fm:FixtureModel:Model {fmid: "fmid", name: "FixtureModel"})
MERGE (partner_api)-[:CAN_DELETE]->(fm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (fm:FixtureModel:Model {fmid: "fmid", name: "FixtureModel"})
MERGE (end_user_admin)-[:CAN_DELETE]->(fm);
MATCH (end_user_lighting_user:Role {rolename: "end_user_lighting_user"}), (fm:FixtureModel:Model {fmid: "fmid", name: "FixtureModel"})
MERGE (end_user_lighting_user)-[:CAN_DELETE]->(fm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (fm:FixtureModel:Model {fmid: "fmid", name: "FixtureModel"})
MERGE (end_user_api)-[:CAN_DELETE]->(fm);
MATCH (installer:Role {rolename: "installer"}), (fm:FixtureModel:Model {fmid: "fmid", name: "FixtureModel"})
MERGE (installer)-[:CAN_DELETE]->(fm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (am:AuditModel:Model {amid: "amid", name: "AuditModel"})
MERGE (sensity_user)-[:CAN_CREATE]->(am);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (am:AuditModel:Model {amid: "amid", name: "AuditModel"})
MERGE (partner_admin)-[:CAN_CREATE]->(am);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (am:AuditModel:Model {amid: "amid", name: "AuditModel"})
MERGE (partner_deployment_user)-[:CAN_CREATE]->(am);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (am:AuditModel:Model {amid: "amid", name: "AuditModel"})
MERGE (end_user_admin)-[:CAN_CREATE]->(am);
MATCH (end_user_lighting_user:Role {rolename: "end_user_lighting_user"}), (am:AuditModel:Model {amid: "amid", name: "AuditModel"})
MERGE (end_user_lighting_user)-[:CAN_CREATE]->(am);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (am:AuditModel:Model {amid: "amid", name: "AuditModel"})
MERGE (end_user_api)-[:CAN_CREATE]->(am);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (am:AuditModel:Model {amid: "amid", name: "AuditModel"})
MERGE (parking_owner_admin)-[:CAN_CREATE]->(am);
MATCH (policy_authority:Role {rolename: "policy_authority"}), (am:AuditModel:Model {amid: "amid", name: "AuditModel"})
MERGE (policy_authority)-[:CAN_CREATE]->(am);
MATCH (parking_manager:Role {rolename: "parking_manager"}), (am:AuditModel:Model {amid: "amid", name: "AuditModel"})
MERGE (parking_manager)-[:CAN_CREATE]->(am);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (am:AuditModel:Model {amid: "amid", name: "AuditModel"})
MERGE (sensity_user)-[:CAN_UPDATE]->(am);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (am:AuditModel:Model {amid: "amid", name: "AuditModel"})
MERGE (partner_admin)-[:CAN_UPDATE]->(am);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (am:AuditModel:Model {amid: "amid", name: "AuditModel"})
MERGE (partner_deployment_user)-[:CAN_UPDATE]->(am);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (am:AuditModel:Model {amid: "amid", name: "AuditModel"})
MERGE (end_user_admin)-[:CAN_UPDATE]->(am);
MATCH (end_user_lighting_user:Role {rolename: "end_user_lighting_user"}), (am:AuditModel:Model {amid: "amid", name: "AuditModel"})
MERGE (end_user_lighting_user)-[:CAN_UPDATE]->(am);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (am:AuditModel:Model {amid: "amid", name: "AuditModel"})
MERGE (end_user_api)-[:CAN_UPDATE]->(am);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (am:AuditModel:Model {amid: "amid", name: "AuditModel"})
MERGE (sensity_user)-[:CAN_DELETE]->(am);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (am:AuditModel:Model {amid: "amid", name: "AuditModel"})
MERGE (partner_admin)-[:CAN_DELETE]->(am);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (am:AuditModel:Model {amid: "amid", name: "AuditModel"})
MERGE (partner_deployment_user)-[:CAN_DELETE]->(am);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (am:AuditModel:Model {amid: "amid", name: "AuditModel"})
MERGE (end_user_admin)-[:CAN_DELETE]->(am);
MATCH (end_user_lighting_user:Role {rolename: "end_user_lighting_user"}), (am:AuditModel:Model {amid: "amid", name: "AuditModel"})
MERGE (end_user_lighting_user)-[:CAN_DELETE]->(am);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (am:AuditModel:Model {amid: "amid", name: "AuditModel"})
MERGE (end_user_api)-[:CAN_DELETE]->(am);
MATCH (sensity_admin:Role:Admin {rolename: "sensity_admin"}), (am:AuditModel:Model {amid: "amid", name: "AuditModel"})
MERGE (sensity_admin)-[:CAN_READ]->(am);
MATCH (sensity_read_only:Role:Admin {rolename: "sensity_read_only"}), (am:AuditModel:Model {amid: "amid", name: "AuditModel"})
MERGE (sensity_read_only)-[:CAN_READ]->(am);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (am:AuditModel:Model {amid: "amid", name: "AuditModel"})
MERGE (sensity_user)-[:CAN_READ]->(am);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (am:AuditModel:Model {amid: "amid", name: "AuditModel"})
MERGE (partner_admin)-[:CAN_READ]->(am);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (am:AuditModel:Model {amid: "amid", name: "AuditModel"})
MERGE (partner_deployment_user)-[:CAN_READ]->(am);
MATCH (partner_lighting_user:Role {rolename: "partner_lighting_user"}), (am:AuditModel:Model {amid: "amid", name: "AuditModel"})
MERGE (partner_lighting_user)-[:CAN_READ]->(am);
MATCH (partner_read_only:Role {rolename: "partner_read_only"}), (am:AuditModel:Model {amid: "amid", name: "AuditModel"})
MERGE (partner_read_only)-[:CAN_READ]->(am);
MATCH (partner_api:Role {rolename: "partner_api"}), (am:AuditModel:Model {amid: "amid", name: "AuditModel"})
MERGE (partner_api)-[:CAN_READ]->(am);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (am:AuditModel:Model {amid: "amid", name: "AuditModel"})
MERGE (end_user_admin)-[:CAN_READ]->(am);
MATCH (end_user_lighting_user:Role {rolename: "end_user_lighting_user"}), (am:AuditModel:Model {amid: "amid", name: "AuditModel"})
MERGE (end_user_lighting_user)-[:CAN_READ]->(am);
MATCH (end_user_read_only:Role {rolename: "end_user_read_only"}), (am:AuditModel:Model {amid: "amid", name: "AuditModel"})
MERGE (end_user_read_only)-[:CAN_READ]->(am);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (am:AuditModel:Model {amid: "amid", name: "AuditModel"})
MERGE (end_user_api)-[:CAN_READ]->(am);
MATCH (installer:Role {rolename: "installer"}), (am:AuditModel:Model {amid: "amid", name: "AuditModel"})
MERGE (installer)-[:CAN_READ]->(am);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (am:AuditModel:Model {amid: "amid", name: "AuditModel"})
MERGE (parking_owner_admin)-[:CAN_READ]->(am);
MATCH (end_user_networking_user:Role {rolename: "end_user_networking_user"}), (am:AuditModel:Model {amid: "amid", name: "AuditModel"})
MERGE (end_user_networking_user)-[:CAN_READ]->(am);
MATCH (installer:Role {rolename: "installer"}), (am:AuditModel:Model {amid: "amid", name: "AuditModel"})
MERGE (installer)-[:CAN_READ]->(am);
MATCH (partner_sensor_user:Role {rolename: "partner_sensor_user"}), (am:AuditModel:Model {amid: "amid", name: "AuditModel"})
MERGE (partner_sensor_user)-[:CAN_READ]->(am);
MATCH (end_user_sensor_user:Role {rolename: "end_user_sensor_user"}), (am:AuditModel:Model {amid: "amid", name: "AuditModel"})
MERGE (end_user_sensor_user)-[:CAN_READ]->(am);
MATCH (partner_networking_user:Role {rolename: "partner_networking_user"}), (am:AuditModel:Model {amid: "amid", name: "AuditModel"})
MERGE (partner_networking_user)-[:CAN_READ]->(am);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (fwm:FirmwareModel:Model {fwmid: "fwmid", name: "FirmwareModel"})
MERGE (sensity_user)-[:CAN_CREATE]->(fwm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (fwm:FirmwareModel:Model {fwmid: "fwmid", name: "FirmwareModel"})
MERGE (partner_admin)-[:CAN_CREATE]->(fwm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (fwm:FirmwareModel:Model {fwmid: "fwmid", name: "FirmwareModel"})
MERGE (partner_deployment_user)-[:CAN_CREATE]->(fwm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (fwm:FirmwareModel:Model {fwmid: "fwmid", name: "FirmwareModel"})
MERGE (end_user_api)-[:CAN_CREATE]->(fwm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (fwm:FirmwareModel:Model {fwmid: "fwmid", name: "FirmwareModel"})
MERGE (sensity_user)-[:CAN_UPDATE]->(fwm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (fwm:FirmwareModel:Model {fwmid: "fwmid", name: "FirmwareModel"})
MERGE (partner_admin)-[:CAN_UPDATE]->(fwm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (fwm:FirmwareModel:Model {fwmid: "fwmid", name: "FirmwareModel"})
MERGE (partner_deployment_user)-[:CAN_UPDATE]->(fwm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (fwm:FirmwareModel:Model {fwmid: "fwmid", name: "FirmwareModel"})
MERGE (end_user_api)-[:CAN_UPDATE]->(fwm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (fwm:FirmwareModel:Model {fwmid: "fwmid", name: "FirmwareModel"})
MERGE (sensity_user)-[:CAN_DELETE]->(fwm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (fwm:FirmwareModel:Model {fwmid: "fwmid", name: "FirmwareModel"})
MERGE (partner_admin)-[:CAN_DELETE]->(fwm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (fwm:FirmwareModel:Model {fwmid: "fwmid", name: "FirmwareModel"})
MERGE (partner_deployment_user)-[:CAN_DELETE]->(fwm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (fwm:FirmwareModel:Model {fwmid: "fwmid", name: "FirmwareModel"})
MERGE (end_user_api)-[:CAN_DELETE]->(fwm);
MATCH (sensity_read_only:Role:Admin {rolename: "sensity_read_only"}), (fwm:FirmwareModel:Model {fwmid: "fwmid", name: "FirmwareModel"})
MERGE (sensity_read_only)-[:CAN_READ]->(fwm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (fwm:FirmwareModel:Model {fwmid: "fwmid", name: "FirmwareModel"})
MERGE (sensity_user)-[:CAN_READ]->(fwm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (fwm:FirmwareModel:Model {fwmid: "fwmid", name: "FirmwareModel"})
MERGE (partner_admin)-[:CAN_READ]->(fwm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (fwm:FirmwareModel:Model {fwmid: "fwmid", name: "FirmwareModel"})
MERGE (partner_deployment_user)-[:CAN_READ]->(fwm);
MATCH (partner_read_only:Role {rolename: "partner_read_only"}), (fwm:FirmwareModel:Model {fwmid: "fwmid", name: "FirmwareModel"})
MERGE (partner_read_only)-[:CAN_READ]->(fwm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (fwm:FirmwareModel:Model {fwmid: "fwmid", name: "FirmwareModel"})
MERGE (end_user_admin)-[:CAN_READ]->(fwm);
MATCH (end_user_lighting_user:Role {rolename: "end_user_lighting_user"}), (fwm:FirmwareModel:Model {fwmid: "fwmid", name: "FirmwareModel"})
MERGE (end_user_lighting_user)-[:CAN_READ]->(fwm);
MATCH (end_user_read_only:Role {rolename: "end_user_read_only"}), (fwm:FirmwareModel:Model {fwmid: "fwmid", name: "FirmwareModel"})
MERGE (end_user_read_only)-[:CAN_READ]->(fwm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (fwm:FirmwareModel:Model {fwmid: "fwmid", name: "FirmwareModel"})
MERGE (end_user_api)-[:CAN_READ]->(fwm);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (fwm:FirmwareModel:Model {fwmid: "fwmid", name: "FirmwareModel"})
MERGE (parking_owner_admin)-[:CAN_READ]->(fwm);
MATCH (installer:Role {rolename: "installer"}), (fwm:FirmwareModel:Model {fwmid: "fwmid", name: "FirmwareModel"})
MERGE (installer)-[:CAN_READ]->(fwm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (fwm:FirmwareModel:Model {fwmid: "fwmid", name: "FirmwareModel"})
MERGE (partner_deployment_user)-[:CAN_READ]->(fwm);
MATCH (partner_read_only:Role {rolename: "partner_read_only"}), (fwm:FirmwareModel:Model {fwmid: "fwmid", name: "FirmwareModel"})
MERGE (partner_read_only)-[:CAN_READ]->(fwm);
MATCH (end_user_read_only:Role {rolename: "end_user_read_only"}), (fwm:FirmwareModel:Model {fwmid: "fwmid", name: "FirmwareModel"})
MERGE (end_user_read_only)-[:CAN_READ]->(fwm);
MATCH (partner_lighting_user:Role {rolename: "partner_lighting_user"}), (fwm:FirmwareModel:Model {fwmid: "fwmid", name: "FirmwareModel"})
MERGE (partner_lighting_user)-[:CAN_READ]->(fwm);
MATCH (partner_networking_user:Role {rolename: "partner_networking_user"}), (fwm:FirmwareModel:Model {fwmid: "fwmid", name: "FirmwareModel"})
MERGE (partner_networking_user)-[:CAN_READ]->(fwm);
MATCH (parking_manager:Role {rolename: "parking_manager"}), (fwm:FirmwareModel:Model {fwmid: "fwmid", name: "FirmwareModel"})
MERGE (parking_manager)-[:CAN_READ]->(fwm);
MATCH (policy_authority:Role {rolename: "policy_authority"}), (fwm:FirmwareModel:Model {fwmid: "fwmid", name: "FirmwareModel"})
MERGE (policy_authority)-[:CAN_READ]->(fwm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (sensity_user)-[:CAN_CREATE]->(nfm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (partner_admin)-[:CAN_CREATE]->(nfm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (partner_deployment_user)-[:CAN_CREATE]->(nfm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (end_user_admin)-[:CAN_CREATE]->(nfm);
MATCH (partner_api:Role {rolename: "partner_api"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (partner_api)-[:CAN_CREATE]->(nfm);
MATCH (end_user_lighting_user:Role {rolename: "end_user_lighting_user"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (end_user_lighting_user)-[:CAN_CREATE]->(nfm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (end_user_api)-[:CAN_CREATE]->(nfm);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (parking_owner_admin)-[:CAN_CREATE]->(nfm);
MATCH (end_user_networking_user:Role {rolename: "end_user_networking_user"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (end_user_networking_user)-[:CAN_CREATE]->(nfm);
MATCH (installer:Role {rolename: "installer"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (installer)-[:CAN_CREATE]->(nfm);
MATCH (end_user_sensor_user:Role {rolename: "end_user_sensor_user"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (end_user_sensor_user)-[:CAN_CREATE]->(nfm);
MATCH (partner_lighting_user:Role {rolename: "partner_lighting_user"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (partner_lighting_user)-[:CAN_CREATE]->(nfm);
MATCH (parking_manager:Role {rolename: "parking_manager"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (parking_manager)-[:CAN_CREATE]->(nfm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (sensity_user)-[:CAN_UPDATE]->(nfm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (partner_admin)-[:CAN_UPDATE]->(nfm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (partner_deployment_user)-[:CAN_UPDATE]->(nfm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (end_user_admin)-[:CAN_UPDATE]->(nfm);
MATCH (partner_api:Role {rolename: "partner_api"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (partner_api)-[:CAN_UPDATE]->(nfm);
MATCH (end_user_lighting_user:Role {rolename: "end_user_lighting_user"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (end_user_lighting_user)-[:CAN_UPDATE]->(nfm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (end_user_api)-[:CAN_UPDATE]->(nfm);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (parking_owner_admin)-[:CAN_UPDATE]->(nfm);
MATCH (end_user_networking_user:Role {rolename: "end_user_networking_user"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (end_user_networking_user)-[:CAN_UPDATE]->(nfm);
MATCH (installer:Role {rolename: "installer"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (installer)-[:CAN_UPDATE]->(nfm);
MATCH (end_user_sensor_user:Role {rolename: "end_user_sensor_user"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (end_user_sensor_user)-[:CAN_UPDATE]->(nfm);
MATCH (partner_lighting_user:Role {rolename: "partner_lighting_user"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (partner_lighting_user)-[:CAN_UPDATE]->(nfm);
MATCH (parking_manager:Role {rolename: "parking_manager"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (parking_manager)-[:CAN_UPDATE]->(nfm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (sensity_user)-[:CAN_DELETE]->(nfm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (partner_admin)-[:CAN_DELETE]->(nfm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (partner_deployment_user)-[:CAN_DELETE]->(nfm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (end_user_admin)-[:CAN_DELETE]->(nfm);
MATCH (partner_api:Role {rolename: "partner_api"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (partner_api)-[:CAN_DELETE]->(nfm);
MATCH (end_user_lighting_user:Role {rolename: "end_user_lighting_user"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (end_user_lighting_user)-[:CAN_DELETE]->(nfm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (end_user_api)-[:CAN_DELETE]->(nfm);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (parking_owner_admin)-[:CAN_DELETE]->(nfm);
MATCH (end_user_networking_user:Role {rolename: "end_user_networking_user"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (end_user_networking_user)-[:CAN_DELETE]->(nfm);
MATCH (installer:Role {rolename: "installer"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (installer)-[:CAN_DELETE]->(nfm);
MATCH (end_user_sensor_user:Role {rolename: "end_user_sensor_user"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (end_user_sensor_user)-[:CAN_DELETE]->(nfm);
MATCH (partner_lighting_user:Role {rolename: "partner_lighting_user"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (partner_lighting_user)-[:CAN_DELETE]->(nfm);
MATCH (parking_manager:Role {rolename: "parking_manager"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (parking_manager)-[:CAN_DELETE]->(nfm);
MATCH (sensity_read_only:Role:Admin {rolename: "sensity_read_only"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (sensity_read_only)-[:CAN_READ]->(nfm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (sensity_user)-[:CAN_READ]->(nfm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (partner_admin)-[:CAN_READ]->(nfm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (partner_deployment_user)-[:CAN_READ]->(nfm);
MATCH (partner_read_only:Role {rolename: "partner_read_only"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (partner_read_only)-[:CAN_READ]->(nfm);
MATCH (partner_api:Role {rolename: "partner_api"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (partner_api)-[:CAN_READ]->(nfm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (end_user_admin)-[:CAN_READ]->(nfm);
MATCH (end_user_lighting_user:Role {rolename: "end_user_lighting_user"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (end_user_lighting_user)-[:CAN_READ]->(nfm);
MATCH (end_user_read_only:Role {rolename: "end_user_read_only"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (end_user_read_only)-[:CAN_READ]->(nfm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (end_user_api)-[:CAN_READ]->(nfm);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (parking_owner_admin)-[:CAN_READ]->(nfm);
MATCH (end_user_networking_user:Role {rolename: "end_user_networking_user"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (end_user_networking_user)-[:CAN_READ]->(nfm);
MATCH (installer:Role {rolename: "installer"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (installer)-[:CAN_READ]->(nfm);
MATCH (partner_sensor_user:Role {rolename: "partner_sensor_user"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (partner_sensor_user)-[:CAN_READ]->(nfm);
MATCH (end_user_sensor_user:Role {rolename: "end_user_sensor_user"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (end_user_sensor_user)-[:CAN_READ]->(nfm);
MATCH (partner_networking_user:Role {rolename: "partner_networking_user"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (partner_networking_user)-[:CAN_READ]->(nfm);
MATCH (partner_lighting_user:Role {rolename: "partner_lighting_user"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (partner_lighting_user)-[:CAN_READ]->(nfm);
MATCH (parking_manager:Role {rolename: "parking_manager"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (parking_manager)-[:CAN_READ]->(nfm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (sensity_user)-[:CAN_ACTIVATE]->(nfm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (partner_admin)-[:CAN_ACTIVATE]->(nfm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (partner_deployment_user)-[:CAN_ACTIVATE]->(nfm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (end_user_admin)-[:CAN_ACTIVATE]->(nfm);
MATCH (partner_api:Role {rolename: "partner_api"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (partner_api)-[:CAN_ACTIVATE]->(nfm);
MATCH (end_user_lighting_user:Role {rolename: "end_user_lighting_user"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (end_user_lighting_user)-[:CAN_ACTIVATE]->(nfm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (end_user_api)-[:CAN_ACTIVATE]->(nfm);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (parking_owner_admin)-[:CAN_ACTIVATE]->(nfm);
MATCH (end_user_networking_user:Role {rolename: "end_user_networking_user"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (end_user_networking_user)-[:CAN_ACTIVATE]->(nfm);
MATCH (installer:Role {rolename: "installer"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (installer)-[:CAN_ACTIVATE]->(nfm);
MATCH (end_user_sensor_user:Role {rolename: "end_user_sensor_user"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (end_user_sensor_user)-[:CAN_ACTIVATE]->(nfm);
MATCH (partner_lighting_user:Role {rolename: "partner_lighting_user"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (partner_lighting_user)-[:CAN_ACTIVATE]->(nfm);
MATCH (parking_manager:Role {rolename: "parking_manager"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (parking_manager)-[:CAN_ACTIVATE]->(nfm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (sensity_user)-[:CAN_DEACTIVATE]->(nfm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (partner_admin)-[:CAN_DEACTIVATE]->(nfm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (partner_deployment_user)-[:CAN_DEACTIVATE]->(nfm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (end_user_admin)-[:CAN_DEACTIVATE]->(nfm);
MATCH (partner_api:Role {rolename: "partner_api"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (partner_api)-[:CAN_DEACTIVATE]->(nfm);
MATCH (end_user_lighting_user:Role {rolename: "end_user_lighting_user"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (end_user_lighting_user)-[:CAN_DEACTIVATE]->(nfm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (end_user_api)-[:CAN_DEACTIVATE]->(nfm);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (parking_owner_admin)-[:CAN_DEACTIVATE]->(nfm);
MATCH (end_user_networking_user:Role {rolename: "end_user_networking_user"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (end_user_networking_user)-[:CAN_DEACTIVATE]->(nfm);
MATCH (installer:Role {rolename: "installer"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (installer)-[:CAN_DEACTIVATE]->(nfm);
MATCH (end_user_sensor_user:Role {rolename: "end_user_sensor_user"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (end_user_sensor_user)-[:CAN_DEACTIVATE]->(nfm);
MATCH (partner_lighting_user:Role {rolename: "partner_lighting_user"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (partner_lighting_user)-[:CAN_DEACTIVATE]->(nfm);
MATCH (parking_manager:Role {rolename: "parking_manager"}), (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
MERGE (parking_manager)-[:CAN_DEACTIVATE]->(nfm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (pz:ParkingZoneModel:Model {parkingzoneid: "parkingzoneid", name: "ParkingZoneModel"})
MERGE (sensity_user)-[:CAN_CREATE]->(pz);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (pz:ParkingZoneModel:Model {parkingzoneid: "parkingzoneid", name: "ParkingZoneModel"})
MERGE (partner_admin)-[:CAN_CREATE]->(pz);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (pz:ParkingZoneModel:Model {parkingzoneid: "parkingzoneid", name: "ParkingZoneModel"})
MERGE (partner_deployment_user)-[:CAN_CREATE]->(pz);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (pz:ParkingZoneModel:Model {parkingzoneid: "parkingzoneid", name: "ParkingZoneModel"})
MERGE (end_user_admin)-[:CAN_CREATE]->(pz);
MATCH (partner_api:Role {rolename: "partner_api"}), (pz:ParkingZoneModel:Model {parkingzoneid: "parkingzoneid", name: "ParkingZoneModel"})
MERGE (partner_api)-[:CAN_CREATE]->(pz);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (pz:ParkingZoneModel:Model {parkingzoneid: "parkingzoneid", name: "ParkingZoneModel"})
MERGE (end_user_api)-[:CAN_CREATE]->(pz);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (pz:ParkingZoneModel:Model {parkingzoneid: "parkingzoneid", name: "ParkingZoneModel"})
MERGE (parking_owner_admin)-[:CAN_CREATE]->(pz);
MATCH (policy_authority:Role {rolename: "policy_authority"}), (pz:ParkingZoneModel:Model {parkingzoneid: "parkingzoneid", name: "ParkingZoneModel"})
MERGE (policy_authority)-[:CAN_CREATE]->(pz);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (pz:ParkingZoneModel:Model {parkingzoneid: "parkingzoneid", name: "ParkingZoneModel"})
MERGE (sensity_user)-[:CAN_UPDATE]->(pz);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (pz:ParkingZoneModel:Model {parkingzoneid: "parkingzoneid", name: "ParkingZoneModel"})
MERGE (partner_admin)-[:CAN_UPDATE]->(pz);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (pz:ParkingZoneModel:Model {parkingzoneid: "parkingzoneid", name: "ParkingZoneModel"})
MERGE (partner_deployment_user)-[:CAN_UPDATE]->(pz);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (pz:ParkingZoneModel:Model {parkingzoneid: "parkingzoneid", name: "ParkingZoneModel"})
MERGE (end_user_admin)-[:CAN_UPDATE]->(pz);
MATCH (partner_api:Role {rolename: "partner_api"}), (pz:ParkingZoneModel:Model {parkingzoneid: "parkingzoneid", name: "ParkingZoneModel"})
MERGE (partner_api)-[:CAN_UPDATE]->(pz);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (pz:ParkingZoneModel:Model {parkingzoneid: "parkingzoneid", name: "ParkingZoneModel"})
MERGE (end_user_api)-[:CAN_UPDATE]->(pz);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (pz:ParkingZoneModel:Model {parkingzoneid: "parkingzoneid", name: "ParkingZoneModel"})
MERGE (parking_owner_admin)-[:CAN_UPDATE]->(pz);
MATCH (policy_authority:Role {rolename: "policy_authority"}), (pz:ParkingZoneModel:Model {parkingzoneid: "parkingzoneid", name: "ParkingZoneModel"})
MERGE (policy_authority)-[:CAN_UPDATE]->(pz);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (pz:ParkingZoneModel:Model {parkingzoneid: "parkingzoneid", name: "ParkingZoneModel"})
MERGE (sensity_user)-[:CAN_DELETE]->(pz);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (pz:ParkingZoneModel:Model {parkingzoneid: "parkingzoneid", name: "ParkingZoneModel"})
MERGE (partner_admin)-[:CAN_DELETE]->(pz);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (pz:ParkingZoneModel:Model {parkingzoneid: "parkingzoneid", name: "ParkingZoneModel"})
MERGE (partner_deployment_user)-[:CAN_DELETE]->(pz);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (pz:ParkingZoneModel:Model {parkingzoneid: "parkingzoneid", name: "ParkingZoneModel"})
MERGE (end_user_admin)-[:CAN_DELETE]->(pz);
MATCH (partner_api:Role {rolename: "partner_api"}), (pz:ParkingZoneModel:Model {parkingzoneid: "parkingzoneid", name: "ParkingZoneModel"})
MERGE (partner_api)-[:CAN_DELETE]->(pz);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (pz:ParkingZoneModel:Model {parkingzoneid: "parkingzoneid", name: "ParkingZoneModel"})
MERGE (end_user_api)-[:CAN_DELETE]->(pz);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (pz:ParkingZoneModel:Model {parkingzoneid: "parkingzoneid", name: "ParkingZoneModel"})
MERGE (parking_owner_admin)-[:CAN_DELETE]->(pz);
MATCH (policy_authority:Role {rolename: "policy_authority"}), (pz:ParkingZoneModel:Model {parkingzoneid: "parkingzoneid", name: "ParkingZoneModel"})
MERGE (policy_authority)-[:CAN_DELETE]->(pz);
MATCH (sensity_read_only:Role:Admin {rolename: "sensity_read_only"}), (pz:ParkingZoneModel:Model {parkingzoneid: "parkingzoneid", name: "ParkingZoneModel"})
MERGE (sensity_read_only)-[:CAN_READ]->(pz);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (pz:ParkingZoneModel:Model {parkingzoneid: "parkingzoneid", name: "ParkingZoneModel"})
MERGE (sensity_user)-[:CAN_READ]->(pz);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (pz:ParkingZoneModel:Model {parkingzoneid: "parkingzoneid", name: "ParkingZoneModel"})
MERGE (partner_admin)-[:CAN_READ]->(pz);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (pz:ParkingZoneModel:Model {parkingzoneid: "parkingzoneid", name: "ParkingZoneModel"})
MERGE (partner_deployment_user)-[:CAN_READ]->(pz);
MATCH (partner_read_only:Role {rolename: "partner_read_only"}), (pz:ParkingZoneModel:Model {parkingzoneid: "parkingzoneid", name: "ParkingZoneModel"})
MERGE (partner_read_only)-[:CAN_READ]->(pz);
MATCH (partner_api:Role {rolename: "partner_api"}), (pz:ParkingZoneModel:Model {parkingzoneid: "parkingzoneid", name: "ParkingZoneModel"})
MERGE (partner_api)-[:CAN_READ]->(pz);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (pz:ParkingZoneModel:Model {parkingzoneid: "parkingzoneid", name: "ParkingZoneModel"})
MERGE (end_user_admin)-[:CAN_READ]->(pz);
MATCH (end_user_read_only:Role {rolename: "end_user_read_only"}), (pz:ParkingZoneModel:Model {parkingzoneid: "parkingzoneid", name: "ParkingZoneModel"})
MERGE (end_user_read_only)-[:CAN_READ]->(pz);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (pz:ParkingZoneModel:Model {parkingzoneid: "parkingzoneid", name: "ParkingZoneModel"})
MERGE (end_user_api)-[:CAN_READ]->(pz);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (pz:ParkingZoneModel:Model {parkingzoneid: "parkingzoneid", name: "ParkingZoneModel"})
MERGE (parking_owner_admin)-[:CAN_READ]->(pz);
MATCH (policy_authority:Role {rolename: "policy_authority"}), (pz:ParkingZoneModel:Model {parkingzoneid: "parkingzoneid", name: "ParkingZoneModel"})
MERGE (policy_authority)-[:CAN_READ]->(pz);
MATCH (parking_manager:Role {rolename: "parking_manager"}), (pz:ParkingZoneModel:Model {parkingzoneid: "parkingzoneid", name: "ParkingZoneModel"})
MERGE (parking_manager)-[:CAN_READ]->(pz);
MATCH (sensity_read_only:Role:Admin {rolename: "sensity_read_only"}), (pz:ParkingZoneModel:Model {parkingzoneid: "parkingzoneid", name: "ParkingZoneModel"})
MERGE (sensity_read_only)-[:CAN_READ]->(pz);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (tom:TrafficObjectModel:Model {tomid: "tomid", name: "TrafficObjectModel"})
MERGE (sensity_user)-[:CAN_CREATE]->(tom);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (tom:TrafficObjectModel:Model {tomid: "tomid", name: "TrafficObjectModel"})
MERGE (partner_admin)-[:CAN_CREATE]->(tom);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (tom:TrafficObjectModel:Model {tomid: "tomid", name: "TrafficObjectModel"})
MERGE (partner_deployment_user)-[:CAN_CREATE]->(tom);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (tom:TrafficObjectModel:Model {tomid: "tomid", name: "TrafficObjectModel"})
MERGE (end_user_admin)-[:CAN_CREATE]->(tom);
MATCH (partner_api:Role {rolename: "partner_api"}), (tom:TrafficObjectModel:Model {tomid: "tomid", name: "TrafficObjectModel"})
MERGE (partner_api)-[:CAN_CREATE]->(tom);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (tom:TrafficObjectModel:Model {tomid: "tomid", name: "TrafficObjectModel"})
MERGE (end_user_api)-[:CAN_CREATE]->(tom);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (tom:TrafficObjectModel:Model {tomid: "tomid", name: "TrafficObjectModel"})
MERGE (sensity_user)-[:CAN_UPDATE]->(tom);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (tom:TrafficObjectModel:Model {tomid: "tomid", name: "TrafficObjectModel"})
MERGE (partner_admin)-[:CAN_UPDATE]->(tom);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (tom:TrafficObjectModel:Model {tomid: "tomid", name: "TrafficObjectModel"})
MERGE (partner_deployment_user)-[:CAN_UPDATE]->(tom);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (tom:TrafficObjectModel:Model {tomid: "tomid", name: "TrafficObjectModel"})
MERGE (end_user_admin)-[:CAN_UPDATE]->(tom);
MATCH (partner_api:Role {rolename: "partner_api"}), (tom:TrafficObjectModel:Model {tomid: "tomid", name: "TrafficObjectModel"})
MERGE (partner_api)-[:CAN_UPDATE]->(tom);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (tom:TrafficObjectModel:Model {tomid: "tomid", name: "TrafficObjectModel"})
MERGE (end_user_api)-[:CAN_UPDATE]->(tom);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (tom:TrafficObjectModel:Model {tomid: "tomid", name: "TrafficObjectModel"})
MERGE (sensity_user)-[:CAN_DELETE]->(tom);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (tom:TrafficObjectModel:Model {tomid: "tomid", name: "TrafficObjectModel"})
MERGE (partner_admin)-[:CAN_DELETE]->(tom);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (tom:TrafficObjectModel:Model {tomid: "tomid", name: "TrafficObjectModel"})
MERGE (partner_deployment_user)-[:CAN_DELETE]->(tom);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (tom:TrafficObjectModel:Model {tomid: "tomid", name: "TrafficObjectModel"})
MERGE (end_user_admin)-[:CAN_DELETE]->(tom);
MATCH (partner_api:Role {rolename: "partner_api"}), (tom:TrafficObjectModel:Model {tomid: "tomid", name: "TrafficObjectModel"})
MERGE (partner_api)-[:CAN_DELETE]->(tom);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (tom:TrafficObjectModel:Model {tomid: "tomid", name: "TrafficObjectModel"})
MERGE (end_user_api)-[:CAN_DELETE]->(tom);
MATCH (sensity_read_only:Role:Admin {rolename: "sensity_read_only"}), (tom:TrafficObjectModel:Model {tomid: "tomid", name: "TrafficObjectModel"})
MERGE (sensity_read_only)-[:CAN_READ]->(tom);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (tom:TrafficObjectModel:Model {tomid: "tomid", name: "TrafficObjectModel"})
MERGE (sensity_user)-[:CAN_READ]->(tom);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (tom:TrafficObjectModel:Model {tomid: "tomid", name: "TrafficObjectModel"})
MERGE (partner_admin)-[:CAN_READ]->(tom);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (tom:TrafficObjectModel:Model {tomid: "tomid", name: "TrafficObjectModel"})
MERGE (partner_deployment_user)-[:CAN_READ]->(tom);
MATCH (partner_read_only:Role {rolename: "partner_read_only"}), (tom:TrafficObjectModel:Model {tomid: "tomid", name: "TrafficObjectModel"})
MERGE (partner_read_only)-[:CAN_READ]->(tom);
MATCH (partner_api:Role {rolename: "partner_api"}), (tom:TrafficObjectModel:Model {tomid: "tomid", name: "TrafficObjectModel"})
MERGE (partner_api)-[:CAN_READ]->(tom);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (tom:TrafficObjectModel:Model {tomid: "tomid", name: "TrafficObjectModel"})
MERGE (end_user_admin)-[:CAN_READ]->(tom);
MATCH (end_user_read_only:Role {rolename: "end_user_read_only"}), (tom:TrafficObjectModel:Model {tomid: "tomid", name: "TrafficObjectModel"})
MERGE (end_user_read_only)-[:CAN_READ]->(tom);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (tom:TrafficObjectModel:Model {tomid: "tomid", name: "TrafficObjectModel"})
MERGE (end_user_api)-[:CAN_READ]->(tom);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (alm:AlertModel:Model {almid: "almid", name: "AlertModel"})
MERGE (sensity_user)-[:CAN_CREATE]->(alm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (alm:AlertModel:Model {almid: "almid", name: "AlertModel"})
MERGE (partner_admin)-[:CAN_CREATE]->(alm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (alm:AlertModel:Model {almid: "almid", name: "AlertModel"})
MERGE (partner_deployment_user)-[:CAN_CREATE]->(alm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (alm:AlertModel:Model {almid: "almid", name: "AlertModel"})
MERGE (end_user_admin)-[:CAN_CREATE]->(alm);
MATCH (end_user_lighting_user:Role {rolename: "end_user_lighting_user"}), (alm:AlertModel:Model {almid: "almid", name: "AlertModel"})
MERGE (end_user_lighting_user)-[:CAN_CREATE]->(alm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (alm:AlertModel:Model {almid: "almid", name: "AlertModel"})
MERGE (end_user_api)-[:CAN_CREATE]->(alm);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (alm:AlertModel:Model {almid: "almid", name: "AlertModel"})
MERGE (parking_owner_admin)-[:CAN_CREATE]->(alm);
MATCH (end_user_sensor_user:Role {rolename: "end_user_sensor_user"}), (alm:AlertModel:Model {almid: "almid", name: "AlertModel"})
MERGE (end_user_sensor_user)-[:CAN_CREATE]->(alm);
MATCH (partner_lighting_user:Role {rolename: "partner_lighting_user"}), (alm:AlertModel:Model {almid: "almid", name: "AlertModel"})
MERGE (partner_lighting_user)-[:CAN_CREATE]->(alm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (alm:AlertModel:Model {almid: "almid", name: "AlertModel"})
MERGE (sensity_user)-[:CAN_UPDATE]->(alm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (alm:AlertModel:Model {almid: "almid", name: "AlertModel"})
MERGE (partner_admin)-[:CAN_UPDATE]->(alm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (alm:AlertModel:Model {almid: "almid", name: "AlertModel"})
MERGE (partner_deployment_user)-[:CAN_UPDATE]->(alm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (alm:AlertModel:Model {almid: "almid", name: "AlertModel"})
MERGE (end_user_admin)-[:CAN_UPDATE]->(alm);
MATCH (end_user_lighting_user:Role {rolename: "end_user_lighting_user"}), (alm:AlertModel:Model {almid: "almid", name: "AlertModel"})
MERGE (end_user_lighting_user)-[:CAN_UPDATE]->(alm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (alm:AlertModel:Model {almid: "almid", name: "AlertModel"})
MERGE (end_user_api)-[:CAN_UPDATE]->(alm);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (alm:AlertModel:Model {almid: "almid", name: "AlertModel"})
MERGE (parking_owner_admin)-[:CAN_UPDATE]->(alm);
MATCH (end_user_sensor_user:Role {rolename: "end_user_sensor_user"}), (alm:AlertModel:Model {almid: "almid", name: "AlertModel"})
MERGE (end_user_sensor_user)-[:CAN_UPDATE]->(alm);
MATCH (partner_lighting_user:Role {rolename: "partner_lighting_user"}), (alm:AlertModel:Model {almid: "almid", name: "AlertModel"})
MERGE (partner_lighting_user)-[:CAN_UPDATE]->(alm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (alm:AlertModel:Model {almid: "almid", name: "AlertModel"})
MERGE (sensity_user)-[:CAN_DELETE]->(alm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (alm:AlertModel:Model {almid: "almid", name: "AlertModel"})
MERGE (partner_admin)-[:CAN_DELETE]->(alm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (alm:AlertModel:Model {almid: "almid", name: "AlertModel"})
MERGE (partner_deployment_user)-[:CAN_DELETE]->(alm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (alm:AlertModel:Model {almid: "almid", name: "AlertModel"})
MERGE (end_user_admin)-[:CAN_DELETE]->(alm);
MATCH (end_user_lighting_user:Role {rolename: "end_user_lighting_user"}), (alm:AlertModel:Model {almid: "almid", name: "AlertModel"})
MERGE (end_user_lighting_user)-[:CAN_DELETE]->(alm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (alm:AlertModel:Model {almid: "almid", name: "AlertModel"})
MERGE (end_user_api)-[:CAN_DELETE]->(alm);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (alm:AlertModel:Model {almid: "almid", name: "AlertModel"})
MERGE (parking_owner_admin)-[:CAN_DELETE]->(alm);
MATCH (end_user_sensor_user:Role {rolename: "end_user_sensor_user"}), (alm:AlertModel:Model {almid: "almid", name: "AlertModel"})
MERGE (end_user_sensor_user)-[:CAN_DELETE]->(alm);
MATCH (partner_lighting_user:Role {rolename: "partner_lighting_user"}), (alm:AlertModel:Model {almid: "almid", name: "AlertModel"})
MERGE (partner_lighting_user)-[:CAN_DELETE]->(alm);
MATCH (sensity_read_only:Role:Admin {rolename: "sensity_read_only"}), (alm:AlertModel:Model {almid: "almid", name: "AlertModel"})
MERGE (sensity_read_only)-[:CAN_READ]->(alm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (alm:AlertModel:Model {almid: "almid", name: "AlertModel"})
MERGE (sensity_user)-[:CAN_READ]->(alm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (alm:AlertModel:Model {almid: "almid", name: "AlertModel"})
MERGE (partner_admin)-[:CAN_READ]->(alm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (alm:AlertModel:Model {almid: "almid", name: "AlertModel"})
MERGE (partner_deployment_user)-[:CAN_READ]->(alm);
MATCH (partner_read_only:Role {rolename: "partner_read_only"}), (alm:AlertModel:Model {almid: "almid", name: "AlertModel"})
MERGE (partner_read_only)-[:CAN_READ]->(alm);
MATCH (partner_api:Role {rolename: "partner_api"}), (alm:AlertModel:Model {almid: "almid", name: "AlertModel"})
MERGE (partner_api)-[:CAN_READ]->(alm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (alm:AlertModel:Model {almid: "almid", name: "AlertModel"})
MERGE (end_user_admin)-[:CAN_READ]->(alm);
MATCH (end_user_lighting_user:Role {rolename: "end_user_lighting_user"}), (alm:AlertModel:Model {almid: "almid", name: "AlertModel"})
MERGE (end_user_lighting_user)-[:CAN_READ]->(alm);
MATCH (end_user_read_only:Role {rolename: "end_user_read_only"}), (alm:AlertModel:Model {almid: "almid", name: "AlertModel"})
MERGE (end_user_read_only)-[:CAN_READ]->(alm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (alm:AlertModel:Model {almid: "almid", name: "AlertModel"})
MERGE (end_user_api)-[:CAN_READ]->(alm);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (alm:AlertModel:Model {almid: "almid", name: "AlertModel"})
MERGE (parking_owner_admin)-[:CAN_READ]->(alm);
MATCH (end_user_networking_user:Role {rolename: "end_user_networking_user"}), (alm:AlertModel:Model {almid: "almid", name: "AlertModel"})
MERGE (end_user_networking_user)-[:CAN_READ]->(alm);
MATCH (installer:Role {rolename: "installer"}), (alm:AlertModel:Model {almid: "almid", name: "AlertModel"})
MERGE (installer)-[:CAN_READ]->(alm);
MATCH (partner_sensor_user:Role {rolename: "partner_sensor_user"}), (alm:AlertModel:Model {almid: "almid", name: "AlertModel"})
MERGE (partner_sensor_user)-[:CAN_READ]->(alm);
MATCH (end_user_sensor_user:Role {rolename: "end_user_sensor_user"}), (alm:AlertModel:Model {almid: "almid", name: "AlertModel"})
MERGE (end_user_sensor_user)-[:CAN_READ]->(alm);
MATCH (partner_networking_user:Role {rolename: "partner_networking_user"}), (alm:AlertModel:Model {almid: "almid", name: "AlertModel"})
MERGE (partner_networking_user)-[:CAN_READ]->(alm);
MATCH (partner_lighting_user:Role {rolename: "partner_lighting_user"}), (alm:AlertModel:Model {almid: "almid", name: "AlertModel"})
MERGE (partner_lighting_user)-[:CAN_READ]->(alm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (olm:OverlayModel:Model {olmid: "olmid", name: "OverlayModel"})
MERGE (sensity_user)-[:CAN_CREATE]->(olm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (olm:OverlayModel:Model {olmid: "olmid", name: "OverlayModel"})
MERGE (partner_admin)-[:CAN_CREATE]->(olm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (olm:OverlayModel:Model {olmid: "olmid", name: "OverlayModel"})
MERGE (partner_deployment_user)-[:CAN_CREATE]->(olm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (olm:OverlayModel:Model {olmid: "olmid", name: "OverlayModel"})
MERGE (end_user_admin)-[:CAN_CREATE]->(olm);
MATCH (end_user_lighting_user:Role {rolename: "end_user_lighting_user"}), (olm:OverlayModel:Model {olmid: "olmid", name: "OverlayModel"})
MERGE (end_user_lighting_user)-[:CAN_CREATE]->(olm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (olm:OverlayModel:Model {olmid: "olmid", name: "OverlayModel"})
MERGE (end_user_api)-[:CAN_CREATE]->(olm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (olm:OverlayModel:Model {olmid: "olmid", name: "OverlayModel"})
MERGE (sensity_user)-[:CAN_UPDATE]->(olm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (olm:OverlayModel:Model {olmid: "olmid", name: "OverlayModel"})
MERGE (partner_admin)-[:CAN_UPDATE]->(olm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (olm:OverlayModel:Model {olmid: "olmid", name: "OverlayModel"})
MERGE (partner_deployment_user)-[:CAN_UPDATE]->(olm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (olm:OverlayModel:Model {olmid: "olmid", name: "OverlayModel"})
MERGE (end_user_admin)-[:CAN_UPDATE]->(olm);
MATCH (end_user_lighting_user:Role {rolename: "end_user_lighting_user"}), (olm:OverlayModel:Model {olmid: "olmid", name: "OverlayModel"})
MERGE (end_user_lighting_user)-[:CAN_UPDATE]->(olm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (olm:OverlayModel:Model {olmid: "olmid", name: "OverlayModel"})
MERGE (end_user_api)-[:CAN_UPDATE]->(olm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (olm:OverlayModel:Model {olmid: "olmid", name: "OverlayModel"})
MERGE (sensity_user)-[:CAN_DELETE]->(olm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (olm:OverlayModel:Model {olmid: "olmid", name: "OverlayModel"})
MERGE (partner_admin)-[:CAN_DELETE]->(olm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (olm:OverlayModel:Model {olmid: "olmid", name: "OverlayModel"})
MERGE (partner_deployment_user)-[:CAN_DELETE]->(olm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (olm:OverlayModel:Model {olmid: "olmid", name: "OverlayModel"})
MERGE (end_user_admin)-[:CAN_DELETE]->(olm);
MATCH (end_user_lighting_user:Role {rolename: "end_user_lighting_user"}), (olm:OverlayModel:Model {olmid: "olmid", name: "OverlayModel"})
MERGE (end_user_lighting_user)-[:CAN_DELETE]->(olm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (olm:OverlayModel:Model {olmid: "olmid", name: "OverlayModel"})
MERGE (end_user_api)-[:CAN_DELETE]->(olm);
MATCH (sensity_read_only:Role:Admin {rolename: "sensity_read_only"}), (olm:OverlayModel:Model {olmid: "olmid", name: "OverlayModel"})
MERGE (sensity_read_only)-[:CAN_READ]->(olm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (olm:OverlayModel:Model {olmid: "olmid", name: "OverlayModel"})
MERGE (sensity_user)-[:CAN_READ]->(olm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (olm:OverlayModel:Model {olmid: "olmid", name: "OverlayModel"})
MERGE (partner_admin)-[:CAN_READ]->(olm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (olm:OverlayModel:Model {olmid: "olmid", name: "OverlayModel"})
MERGE (partner_deployment_user)-[:CAN_READ]->(olm);
MATCH (partner_read_only:Role {rolename: "partner_read_only"}), (olm:OverlayModel:Model {olmid: "olmid", name: "OverlayModel"})
MERGE (partner_read_only)-[:CAN_READ]->(olm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (olm:OverlayModel:Model {olmid: "olmid", name: "OverlayModel"})
MERGE (end_user_admin)-[:CAN_READ]->(olm);
MATCH (end_user_lighting_user:Role {rolename: "end_user_lighting_user"}), (olm:OverlayModel:Model {olmid: "olmid", name: "OverlayModel"})
MERGE (end_user_lighting_user)-[:CAN_READ]->(olm);
MATCH (end_user_read_only:Role {rolename: "end_user_read_only"}), (olm:OverlayModel:Model {olmid: "olmid", name: "OverlayModel"})
MERGE (end_user_read_only)-[:CAN_READ]->(olm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (olm:OverlayModel:Model {olmid: "olmid", name: "OverlayModel"})
MERGE (end_user_api)-[:CAN_READ]->(olm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (psm:ParkingSpotModel:Model {psmid: "psmid", name: "ParkingSpotModel"})
MERGE (sensity_user)-[:CAN_CREATE]->(psm);
MATCH (sensity_admin:Role:Admin {rolename: "sensity_admin"}), (psm:ParkingSpotModel:Model {psmid: "psmid", name: "ParkingSpotModel"})
MERGE (sensity_admin)-[:CAN_CREATE]->(psm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (psm:ParkingSpotModel:Model {psmid: "psmid", name: "ParkingSpotModel"})
MERGE (partner_admin)-[:CAN_CREATE]->(psm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (psm:ParkingSpotModel:Model {psmid: "psmid", name: "ParkingSpotModel"})
MERGE (partner_deployment_user)-[:CAN_CREATE]->(psm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (psm:ParkingSpotModel:Model {psmid: "psmid", name: "ParkingSpotModel"})
MERGE (end_user_admin)-[:CAN_CREATE]->(psm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (psm:ParkingSpotModel:Model {psmid: "psmid", name: "ParkingSpotModel"})
MERGE (end_user_api)-[:CAN_CREATE]->(psm);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (psm:ParkingSpotModel:Model {psmid: "psmid", name: "ParkingSpotModel"})
MERGE (parking_owner_admin)-[:CAN_CREATE]->(psm);
MATCH (policy_authority:Role {rolename: "policy_authority"}), (psm:ParkingSpotModel:Model {psmid: "psmid", name: "ParkingSpotModel"})
MERGE (policy_authority)-[:CAN_CREATE]->(psm);
MATCH (parking_manager:Role {rolename: "parking_manager"}), (psm:ParkingSpotModel:Model {psmid: "psmid", name: "ParkingSpotModel"})
MERGE (parking_manager)-[:CAN_CREATE]->(psm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (psm:ParkingSpotModel:Model {psmid: "psmid", name: "ParkingSpotModel"})
MERGE (sensity_user)-[:CAN_UPDATE]->(psm);
MATCH (sensity_admin:Role:Admin {rolename: "sensity_admin"}), (psm:ParkingSpotModel:Model {psmid: "psmid", name: "ParkingSpotModel"})
MERGE (sensity_admin)-[:CAN_UPDATE]->(psm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (psm:ParkingSpotModel:Model {psmid: "psmid", name: "ParkingSpotModel"})
MERGE (partner_admin)-[:CAN_UPDATE]->(psm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (psm:ParkingSpotModel:Model {psmid: "psmid", name: "ParkingSpotModel"})
MERGE (partner_deployment_user)-[:CAN_UPDATE]->(psm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (psm:ParkingSpotModel:Model {psmid: "psmid", name: "ParkingSpotModel"})
MERGE (end_user_admin)-[:CAN_UPDATE]->(psm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (psm:ParkingSpotModel:Model {psmid: "psmid", name: "ParkingSpotModel"})
MERGE (end_user_api)-[:CAN_UPDATE]->(psm);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (psm:ParkingSpotModel:Model {psmid: "psmid", name: "ParkingSpotModel"})
MERGE (parking_owner_admin)-[:CAN_UPDATE]->(psm);
MATCH (policy_authority:Role {rolename: "policy_authority"}), (psm:ParkingSpotModel:Model {psmid: "psmid", name: "ParkingSpotModel"})
MERGE (policy_authority)-[:CAN_UPDATE]->(psm);
MATCH (parking_manager:Role {rolename: "parking_manager"}), (psm:ParkingSpotModel:Model {psmid: "psmid", name: "ParkingSpotModel"})
MERGE (parking_manager)-[:CAN_UPDATE]->(psm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (psm:ParkingSpotModel:Model {psmid: "psmid", name: "ParkingSpotModel"})
MERGE (sensity_user)-[:CAN_DELETE]->(psm);
MATCH (sensity_admin:Role:Admin {rolename: "sensity_admin"}), (psm:ParkingSpotModel:Model {psmid: "psmid", name: "ParkingSpotModel"})
MERGE (sensity_admin)-[:CAN_DELETE]->(psm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (psm:ParkingSpotModel:Model {psmid: "psmid", name: "ParkingSpotModel"})
MERGE (partner_admin)-[:CAN_DELETE]->(psm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (psm:ParkingSpotModel:Model {psmid: "psmid", name: "ParkingSpotModel"})
MERGE (partner_deployment_user)-[:CAN_DELETE]->(psm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (psm:ParkingSpotModel:Model {psmid: "psmid", name: "ParkingSpotModel"})
MERGE (end_user_admin)-[:CAN_DELETE]->(psm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (psm:ParkingSpotModel:Model {psmid: "psmid", name: "ParkingSpotModel"})
MERGE (end_user_api)-[:CAN_DELETE]->(psm);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (psm:ParkingSpotModel:Model {psmid: "psmid", name: "ParkingSpotModel"})
MERGE (parking_owner_admin)-[:CAN_DELETE]->(psm);
MATCH (policy_authority:Role {rolename: "policy_authority"}), (psm:ParkingSpotModel:Model {psmid: "psmid", name: "ParkingSpotModel"})
MERGE (policy_authority)-[:CAN_DELETE]->(psm);
MATCH (parking_manager:Role {rolename: "parking_manager"}), (psm:ParkingSpotModel:Model {psmid: "psmid", name: "ParkingSpotModel"})
MERGE (parking_manager)-[:CAN_DELETE]->(psm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (psm:ParkingSpotModel:Model {psmid: "psmid", name: "ParkingSpotModel"})
MERGE (sensity_user)-[:CAN_READ]->(psm);
MATCH (sensity_admin:Role:Admin {rolename: "sensity_admin"}), (psm:ParkingSpotModel:Model {psmid: "psmid", name: "ParkingSpotModel"})
MERGE (sensity_admin)-[:CAN_READ]->(psm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (psm:ParkingSpotModel:Model {psmid: "psmid", name: "ParkingSpotModel"})
MERGE (partner_admin)-[:CAN_READ]->(psm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (psm:ParkingSpotModel:Model {psmid: "psmid", name: "ParkingSpotModel"})
MERGE (partner_deployment_user)-[:CAN_READ]->(psm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (psm:ParkingSpotModel:Model {psmid: "psmid", name: "ParkingSpotModel"})
MERGE (end_user_admin)-[:CAN_READ]->(psm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (psm:ParkingSpotModel:Model {psmid: "psmid", name: "ParkingSpotModel"})
MERGE (end_user_api)-[:CAN_READ]->(psm);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (psm:ParkingSpotModel:Model {psmid: "psmid", name: "ParkingSpotModel"})
MERGE (parking_owner_admin)-[:CAN_READ]->(psm);
MATCH (policy_authority:Role {rolename: "policy_authority"}), (psm:ParkingSpotModel:Model {psmid: "psmid", name: "ParkingSpotModel"})
MERGE (policy_authority)-[:CAN_READ]->(psm);
MATCH (parking_manager:Role {rolename: "parking_manager"}), (psm:ParkingSpotModel:Model {psmid: "psmid", name: "ParkingSpotModel"})
MERGE (parking_manager)-[:CAN_READ]->(psm);
MATCH (sensity_read_only:Role:Admin {rolename: "sensity_read_only"}), (psm:ParkingSpotModel:Model {psmid: "psmid", name: "ParkingSpotModel"})
MERGE (sensity_read_only)-[:CAN_READ]->(psm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (ppm:ParkingPolicyModel:Model {ppmid: "ppmid", name: "ParkingPolicyModel"})
MERGE (sensity_user)-[:CAN_CREATE]->(ppm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (ppm:ParkingPolicyModel:Model {ppmid: "ppmid", name: "ParkingPolicyModel"})
MERGE (partner_admin)-[:CAN_CREATE]->(ppm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (ppm:ParkingPolicyModel:Model {ppmid: "ppmid", name: "ParkingPolicyModel"})
MERGE (partner_deployment_user)-[:CAN_CREATE]->(ppm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (ppm:ParkingPolicyModel:Model {ppmid: "ppmid", name: "ParkingPolicyModel"})
MERGE (end_user_admin)-[:CAN_CREATE]->(ppm);
MATCH (end_user_lighting_user:Role {rolename: "end_user_lighting_user"}), (ppm:ParkingPolicyModel:Model {ppmid: "ppmid", name: "ParkingPolicyModel"})
MERGE (end_user_lighting_user)-[:CAN_CREATE]->(ppm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (ppm:ParkingPolicyModel:Model {ppmid: "ppmid", name: "ParkingPolicyModel"})
MERGE (end_user_api)-[:CAN_CREATE]->(ppm);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (ppm:ParkingPolicyModel:Model {ppmid: "ppmid", name: "ParkingPolicyModel"})
MERGE (parking_owner_admin)-[:CAN_CREATE]->(ppm);
MATCH (policy_authority:Role {rolename: "policy_authority"}), (ppm:ParkingPolicyModel:Model {ppmid: "ppmid", name: "ParkingPolicyModel"})
MERGE (policy_authority)-[:CAN_CREATE]->(ppm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (ppm:ParkingPolicyModel:Model {ppmid: "ppmid", name: "ParkingPolicyModel"})
MERGE (sensity_user)-[:CAN_UPDATE]->(ppm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (ppm:ParkingPolicyModel:Model {ppmid: "ppmid", name: "ParkingPolicyModel"})
MERGE (partner_admin)-[:CAN_UPDATE]->(ppm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (ppm:ParkingPolicyModel:Model {ppmid: "ppmid", name: "ParkingPolicyModel"})
MERGE (partner_deployment_user)-[:CAN_UPDATE]->(ppm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (ppm:ParkingPolicyModel:Model {ppmid: "ppmid", name: "ParkingPolicyModel"})
MERGE (end_user_admin)-[:CAN_UPDATE]->(ppm);
MATCH (end_user_lighting_user:Role {rolename: "end_user_lighting_user"}), (ppm:ParkingPolicyModel:Model {ppmid: "ppmid", name: "ParkingPolicyModel"})
MERGE (end_user_lighting_user)-[:CAN_UPDATE]->(ppm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (ppm:ParkingPolicyModel:Model {ppmid: "ppmid", name: "ParkingPolicyModel"})
MERGE (end_user_api)-[:CAN_UPDATE]->(ppm);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (ppm:ParkingPolicyModel:Model {ppmid: "ppmid", name: "ParkingPolicyModel"})
MERGE (parking_owner_admin)-[:CAN_UPDATE]->(ppm);
MATCH (policy_authority:Role {rolename: "policy_authority"}), (ppm:ParkingPolicyModel:Model {ppmid: "ppmid", name: "ParkingPolicyModel"})
MERGE (policy_authority)-[:CAN_UPDATE]->(ppm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (ppm:ParkingPolicyModel:Model {ppmid: "ppmid", name: "ParkingPolicyModel"})
MERGE (sensity_user)-[:CAN_DELETE]->(ppm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (ppm:ParkingPolicyModel:Model {ppmid: "ppmid", name: "ParkingPolicyModel"})
MERGE (partner_admin)-[:CAN_DELETE]->(ppm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (ppm:ParkingPolicyModel:Model {ppmid: "ppmid", name: "ParkingPolicyModel"})
MERGE (partner_deployment_user)-[:CAN_DELETE]->(ppm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (ppm:ParkingPolicyModel:Model {ppmid: "ppmid", name: "ParkingPolicyModel"})
MERGE (end_user_admin)-[:CAN_DELETE]->(ppm);
MATCH (end_user_lighting_user:Role {rolename: "end_user_lighting_user"}), (ppm:ParkingPolicyModel:Model {ppmid: "ppmid", name: "ParkingPolicyModel"})
MERGE (end_user_lighting_user)-[:CAN_DELETE]->(ppm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (ppm:ParkingPolicyModel:Model {ppmid: "ppmid", name: "ParkingPolicyModel"})
MERGE (end_user_api)-[:CAN_DELETE]->(ppm);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (ppm:ParkingPolicyModel:Model {ppmid: "ppmid", name: "ParkingPolicyModel"})
MERGE (parking_owner_admin)-[:CAN_DELETE]->(ppm);
MATCH (policy_authority:Role {rolename: "policy_authority"}), (ppm:ParkingPolicyModel:Model {ppmid: "ppmid", name: "ParkingPolicyModel"})
MERGE (policy_authority)-[:CAN_DELETE]->(ppm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (ppm:ParkingPolicyModel:Model {ppmid: "ppmid", name: "ParkingPolicyModel"})
MERGE (sensity_user)-[:CAN_READ]->(ppm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (ppm:ParkingPolicyModel:Model {ppmid: "ppmid", name: "ParkingPolicyModel"})
MERGE (partner_admin)-[:CAN_READ]->(ppm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (ppm:ParkingPolicyModel:Model {ppmid: "ppmid", name: "ParkingPolicyModel"})
MERGE (partner_deployment_user)-[:CAN_READ]->(ppm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (ppm:ParkingPolicyModel:Model {ppmid: "ppmid", name: "ParkingPolicyModel"})
MERGE (end_user_admin)-[:CAN_READ]->(ppm);
MATCH (end_user_lighting_user:Role {rolename: "end_user_lighting_user"}), (ppm:ParkingPolicyModel:Model {ppmid: "ppmid", name: "ParkingPolicyModel"})
MERGE (end_user_lighting_user)-[:CAN_READ]->(ppm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (ppm:ParkingPolicyModel:Model {ppmid: "ppmid", name: "ParkingPolicyModel"})
MERGE (end_user_api)-[:CAN_READ]->(ppm);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (ppm:ParkingPolicyModel:Model {ppmid: "ppmid", name: "ParkingPolicyModel"})
MERGE (parking_owner_admin)-[:CAN_READ]->(ppm);
MATCH (policy_authority:Role {rolename: "policy_authority"}), (ppm:ParkingPolicyModel:Model {ppmid: "ppmid", name: "ParkingPolicyModel"})
MERGE (policy_authority)-[:CAN_READ]->(ppm);
MATCH (parking_manager:Role {rolename: "parking_manager"}), (ppm:ParkingPolicyModel:Model {ppmid: "ppmid", name: "ParkingPolicyModel"})
MERGE (parking_manager)-[:CAN_READ]->(ppm);
MATCH (sensity_read_only:Role:Admin {rolename: "sensity_read_only"}), (ppm:ParkingPolicyModel:Model {ppmid: "ppmid", name: "ParkingPolicyModel"})
MERGE (sensity_read_only)-[:CAN_READ]->(ppm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (pcm:PolicyCategoryModel:Model {pcmid: "pcmid", name: "PolicyCategoryModel"})
MERGE (sensity_user)-[:CAN_CREATE]->(pcm);
MATCH (sensity_admin:Role:Admin {rolename: "sensity_admin"}), (pcm:PolicyCategoryModel:Model {pcmid: "pcmid", name: "PolicyCategoryModel"})
MERGE (sensity_admin)-[:CAN_CREATE]->(pcm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (pcm:PolicyCategoryModel:Model {pcmid: "pcmid", name: "PolicyCategoryModel"})
MERGE (partner_admin)-[:CAN_CREATE]->(pcm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (pcm:PolicyCategoryModel:Model {pcmid: "pcmid", name: "PolicyCategoryModel"})
MERGE (partner_deployment_user)-[:CAN_CREATE]->(pcm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (pcm:PolicyCategoryModel:Model {pcmid: "pcmid", name: "PolicyCategoryModel"})
MERGE (end_user_admin)-[:CAN_CREATE]->(pcm);
MATCH (end_user_lighting_user:Role {rolename: "end_user_lighting_user"}), (pcm:PolicyCategoryModel:Model {pcmid: "pcmid", name: "PolicyCategoryModel"})
MERGE (end_user_lighting_user)-[:CAN_CREATE]->(pcm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (pcm:PolicyCategoryModel:Model {pcmid: "pcmid", name: "PolicyCategoryModel"})
MERGE (end_user_api)-[:CAN_CREATE]->(pcm);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (pcm:PolicyCategoryModel:Model {pcmid: "pcmid", name: "PolicyCategoryModel"})
MERGE (parking_owner_admin)-[:CAN_CREATE]->(pcm);
MATCH (policy_authority:Role {rolename: "policy_authority"}), (pcm:PolicyCategoryModel:Model {pcmid: "pcmid", name: "PolicyCategoryModel"})
MERGE (policy_authority)-[:CAN_CREATE]->(pcm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (pcm:PolicyCategoryModel:Model {pcmid: "pcmid", name: "PolicyCategoryModel"})
MERGE (sensity_user)-[:CAN_UPDATE]->(pcm);
MATCH (sensity_admin:Role:Admin {rolename: "sensity_admin"}), (pcm:PolicyCategoryModel:Model {pcmid: "pcmid", name: "PolicyCategoryModel"})
MERGE (sensity_admin)-[:CAN_UPDATE]->(pcm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (pcm:PolicyCategoryModel:Model {pcmid: "pcmid", name: "PolicyCategoryModel"})
MERGE (partner_admin)-[:CAN_UPDATE]->(pcm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (pcm:PolicyCategoryModel:Model {pcmid: "pcmid", name: "PolicyCategoryModel"})
MERGE (partner_deployment_user)-[:CAN_UPDATE]->(pcm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (pcm:PolicyCategoryModel:Model {pcmid: "pcmid", name: "PolicyCategoryModel"})
MERGE (end_user_admin)-[:CAN_UPDATE]->(pcm);
MATCH (end_user_lighting_user:Role {rolename: "end_user_lighting_user"}), (pcm:PolicyCategoryModel:Model {pcmid: "pcmid", name: "PolicyCategoryModel"})
MERGE (end_user_lighting_user)-[:CAN_UPDATE]->(pcm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (pcm:PolicyCategoryModel:Model {pcmid: "pcmid", name: "PolicyCategoryModel"})
MERGE (end_user_api)-[:CAN_UPDATE]->(pcm);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (pcm:PolicyCategoryModel:Model {pcmid: "pcmid", name: "PolicyCategoryModel"})
MERGE (parking_owner_admin)-[:CAN_UPDATE]->(pcm);
MATCH (policy_authority:Role {rolename: "policy_authority"}), (pcm:PolicyCategoryModel:Model {pcmid: "pcmid", name: "PolicyCategoryModel"})
MERGE (policy_authority)-[:CAN_UPDATE]->(pcm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (pcm:PolicyCategoryModel:Model {pcmid: "pcmid", name: "PolicyCategoryModel"})
MERGE (sensity_user)-[:CAN_DELETE]->(pcm);
MATCH (sensity_admin:Role:Admin {rolename: "sensity_admin"}), (pcm:PolicyCategoryModel:Model {pcmid: "pcmid", name: "PolicyCategoryModel"})
MERGE (sensity_admin)-[:CAN_DELETE]->(pcm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (pcm:PolicyCategoryModel:Model {pcmid: "pcmid", name: "PolicyCategoryModel"})
MERGE (partner_admin)-[:CAN_DELETE]->(pcm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (pcm:PolicyCategoryModel:Model {pcmid: "pcmid", name: "PolicyCategoryModel"})
MERGE (partner_deployment_user)-[:CAN_DELETE]->(pcm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (pcm:PolicyCategoryModel:Model {pcmid: "pcmid", name: "PolicyCategoryModel"})
MERGE (end_user_admin)-[:CAN_DELETE]->(pcm);
MATCH (end_user_lighting_user:Role {rolename: "end_user_lighting_user"}), (pcm:PolicyCategoryModel:Model {pcmid: "pcmid", name: "PolicyCategoryModel"})
MERGE (end_user_lighting_user)-[:CAN_DELETE]->(pcm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (pcm:PolicyCategoryModel:Model {pcmid: "pcmid", name: "PolicyCategoryModel"})
MERGE (end_user_api)-[:CAN_DELETE]->(pcm);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (pcm:PolicyCategoryModel:Model {pcmid: "pcmid", name: "PolicyCategoryModel"})
MERGE (parking_owner_admin)-[:CAN_DELETE]->(pcm);
MATCH (policy_authority:Role {rolename: "policy_authority"}), (pcm:PolicyCategoryModel:Model {pcmid: "pcmid", name: "PolicyCategoryModel"})
MERGE (policy_authority)-[:CAN_DELETE]->(pcm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (pcm:PolicyCategoryModel:Model {pcmid: "pcmid", name: "PolicyCategoryModel"})
MERGE (sensity_user)-[:CAN_READ]->(pcm);
MATCH (sensity_admin:Role:Admin {rolename: "sensity_admin"}), (pcm:PolicyCategoryModel:Model {pcmid: "pcmid", name: "PolicyCategoryModel"})
MERGE (sensity_admin)-[:CAN_READ]->(pcm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (pcm:PolicyCategoryModel:Model {pcmid: "pcmid", name: "PolicyCategoryModel"})
MERGE (partner_admin)-[:CAN_READ]->(pcm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (pcm:PolicyCategoryModel:Model {pcmid: "pcmid", name: "PolicyCategoryModel"})
MERGE (partner_deployment_user)-[:CAN_READ]->(pcm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (pcm:PolicyCategoryModel:Model {pcmid: "pcmid", name: "PolicyCategoryModel"})
MERGE (end_user_admin)-[:CAN_READ]->(pcm);
MATCH (end_user_lighting_user:Role {rolename: "end_user_lighting_user"}), (pcm:PolicyCategoryModel:Model {pcmid: "pcmid", name: "PolicyCategoryModel"})
MERGE (end_user_lighting_user)-[:CAN_READ]->(pcm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (pcm:PolicyCategoryModel:Model {pcmid: "pcmid", name: "PolicyCategoryModel"})
MERGE (end_user_api)-[:CAN_READ]->(pcm);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (pcm:PolicyCategoryModel:Model {pcmid: "pcmid", name: "PolicyCategoryModel"})
MERGE (parking_owner_admin)-[:CAN_READ]->(pcm);
MATCH (policy_authority:Role {rolename: "policy_authority"}), (pcm:PolicyCategoryModel:Model {pcmid: "pcmid", name: "PolicyCategoryModel"})
MERGE (policy_authority)-[:CAN_READ]->(pcm);
MATCH (parking_manager:Role {rolename: "parking_manager"}), (pcm:PolicyCategoryModel:Model {pcmid: "pcmid", name: "PolicyCategoryModel"})
MERGE (parking_manager)-[:CAN_READ]->(pcm);
MATCH (sensity_read_only:Role:Admin {rolename: "sensity_read_only"}), (pcm:PolicyCategoryModel:Model {pcmid: "pcmid", name: "PolicyCategoryModel"})
MERGE (sensity_read_only)-[:CAN_READ]->(pcm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (pgm:ParkingGroupModel:Model {pgmid: "pgmid", name: "ParkingGroupModel"})
MERGE (sensity_user)-[:CAN_UPDATE]->(pgm);
MATCH (sensity_admin:Role:Admin {rolename: "sensity_admin"}), (pgm:ParkingGroupModel:Model {pgmid: "pgmid", name: "ParkingGroupModel"})
MERGE (sensity_admin)-[:CAN_UPDATE]->(pgm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (pgm:ParkingGroupModel:Model {pgmid: "pgmid", name: "ParkingGroupModel"})
MERGE (partner_admin)-[:CAN_UPDATE]->(pgm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (pgm:ParkingGroupModel:Model {pgmid: "pgmid", name: "ParkingGroupModel"})
MERGE (partner_deployment_user)-[:CAN_UPDATE]->(pgm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (pgm:ParkingGroupModel:Model {pgmid: "pgmid", name: "ParkingGroupModel"})
MERGE (end_user_admin)-[:CAN_UPDATE]->(pgm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (pgm:ParkingGroupModel:Model {pgmid: "pgmid", name: "ParkingGroupModel"})
MERGE (end_user_api)-[:CAN_UPDATE]->(pgm);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (pgm:ParkingGroupModel:Model {pgmid: "pgmid", name: "ParkingGroupModel"})
MERGE (parking_owner_admin)-[:CAN_UPDATE]->(pgm);
MATCH (parking_manager:Role {rolename: "parking_manager"}), (pgm:ParkingGroupModel:Model {pgmid: "pgmid", name: "ParkingGroupModel"})
MERGE (parking_manager)-[:CAN_UPDATE]->(pgm);
MATCH (policy_authority:Role {rolename: "policy_authority"}), (pgm:ParkingGroupModel:Model {pgmid: "pgmid", name: "ParkingGroupModel"})
MERGE (policy_authority)-[:CAN_UPDATE]->(pgm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (pgm:ParkingGroupModel:Model {pgmid: "pgmid", name: "ParkingGroupModel"})
MERGE (sensity_user)-[:CAN_DELETE]->(pgm);
MATCH (sensity_admin:Role:Admin {rolename: "sensity_admin"}), (pgm:ParkingGroupModel:Model {pgmid: "pgmid", name: "ParkingGroupModel"})
MERGE (sensity_admin)-[:CAN_DELETE]->(pgm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (pgm:ParkingGroupModel:Model {pgmid: "pgmid", name: "ParkingGroupModel"})
MERGE (partner_admin)-[:CAN_DELETE]->(pgm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (pgm:ParkingGroupModel:Model {pgmid: "pgmid", name: "ParkingGroupModel"})
MERGE (partner_deployment_user)-[:CAN_DELETE]->(pgm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (pgm:ParkingGroupModel:Model {pgmid: "pgmid", name: "ParkingGroupModel"})
MERGE (end_user_admin)-[:CAN_DELETE]->(pgm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (pgm:ParkingGroupModel:Model {pgmid: "pgmid", name: "ParkingGroupModel"})
MERGE (end_user_api)-[:CAN_DELETE]->(pgm);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (pgm:ParkingGroupModel:Model {pgmid: "pgmid", name: "ParkingGroupModel"})
MERGE (parking_owner_admin)-[:CAN_DELETE]->(pgm);
MATCH (policy_authority:Role {rolename: "policy_authority"}), (pgm:ParkingGroupModel:Model {pgmid: "pgmid", name: "ParkingGroupModel"})
MERGE (policy_authority)-[:CAN_DELETE]->(pgm);
MATCH (parking_manager:Role {rolename: "parking_manager"}), (pgm:ParkingGroupModel:Model {pgmid: "pgmid", name: "ParkingGroupModel"})
MERGE (parking_manager)-[:CAN_DELETE]->(pgm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (pgm:ParkingGroupModel:Model {pgmid: "pgmid", name: "ParkingGroupModel"})
MERGE (sensity_user)-[:CAN_READ]->(pgm);
MATCH (sensity_admin:Role:Admin {rolename: "sensity_admin"}), (pgm:ParkingGroupModel:Model {pgmid: "pgmid", name: "ParkingGroupModel"})
MERGE (sensity_admin)-[:CAN_READ]->(pgm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (pgm:ParkingGroupModel:Model {pgmid: "pgmid", name: "ParkingGroupModel"})
MERGE (partner_admin)-[:CAN_READ]->(pgm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (pgm:ParkingGroupModel:Model {pgmid: "pgmid", name: "ParkingGroupModel"})
MERGE (partner_deployment_user)-[:CAN_READ]->(pgm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (pgm:ParkingGroupModel:Model {pgmid: "pgmid", name: "ParkingGroupModel"})
MERGE (end_user_admin)-[:CAN_READ]->(pgm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (pgm:ParkingGroupModel:Model {pgmid: "pgmid", name: "ParkingGroupModel"})
MERGE (end_user_api)-[:CAN_READ]->(pgm);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (pgm:ParkingGroupModel:Model {pgmid: "pgmid", name: "ParkingGroupModel"})
MERGE (parking_owner_admin)-[:CAN_READ]->(pgm);
MATCH (policy_authority:Role {rolename: "policy_authority"}), (pgm:ParkingGroupModel:Model {pgmid: "pgmid", name: "ParkingGroupModel"})
MERGE (policy_authority)-[:CAN_READ]->(pgm);
MATCH (parking_manager:Role {rolename: "parking_manager"}), (pgm:ParkingGroupModel:Model {pgmid: "pgmid", name: "ParkingGroupModel"})
MERGE (parking_manager)-[:CAN_READ]->(pgm);
MATCH (sensity_read_only:Role:Admin {rolename: "sensity_read_only"}), (pgm:ParkingGroupModel:Model {pgmid: "pgmid", name: "ParkingGroupModel"})
MERGE (sensity_read_only)-[:CAN_READ]->(pgm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (srm:SummaryReportModel:Model {srmid: "srmid", name: "SummaryReportModel"})
MERGE (sensity_user)-[:CAN_CREATE]->(srm);
MATCH (sensity_admin:Role:Admin {rolename: "sensity_admin"}), (srm:SummaryReportModel:Model {srmid: "srmid", name: "SummaryReportModel"})
MERGE (sensity_admin)-[:CAN_CREATE]->(srm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (srm:SummaryReportModel:Model {srmid: "srmid", name: "SummaryReportModel"})
MERGE (partner_admin)-[:CAN_CREATE]->(srm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (srm:SummaryReportModel:Model {srmid: "srmid", name: "SummaryReportModel"})
MERGE (partner_deployment_user)-[:CAN_CREATE]->(srm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (srm:SummaryReportModel:Model {srmid: "srmid", name: "SummaryReportModel"})
MERGE (end_user_admin)-[:CAN_CREATE]->(srm);
MATCH (end_user_lighting_user:Role {rolename: "end_user_lighting_user"}), (srm:SummaryReportModel:Model {srmid: "srmid", name: "SummaryReportModel"})
MERGE (end_user_lighting_user)-[:CAN_CREATE]->(srm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (srm:SummaryReportModel:Model {srmid: "srmid", name: "SummaryReportModel"})
MERGE (end_user_api)-[:CAN_CREATE]->(srm);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (srm:SummaryReportModel:Model {srmid: "srmid", name: "SummaryReportModel"})
MERGE (parking_owner_admin)-[:CAN_CREATE]->(srm);
MATCH (policy_authority:Role {rolename: "policy_authority"}), (srm:SummaryReportModel:Model {srmid: "srmid", name: "SummaryReportModel"})
MERGE (policy_authority)-[:CAN_CREATE]->(srm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (srm:SummaryReportModel:Model {srmid: "srmid", name: "SummaryReportModel"})
MERGE (sensity_user)-[:CAN_UPDATE]->(srm);
MATCH (sensity_admin:Role:Admin {rolename: "sensity_admin"}), (srm:SummaryReportModel:Model {srmid: "srmid", name: "SummaryReportModel"})
MERGE (sensity_admin)-[:CAN_UPDATE]->(srm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (srm:SummaryReportModel:Model {srmid: "srmid", name: "SummaryReportModel"})
MERGE (partner_admin)-[:CAN_UPDATE]->(srm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (srm:SummaryReportModel:Model {srmid: "srmid", name: "SummaryReportModel"})
MERGE (partner_deployment_user)-[:CAN_UPDATE]->(srm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (srm:SummaryReportModel:Model {srmid: "srmid", name: "SummaryReportModel"})
MERGE (end_user_admin)-[:CAN_UPDATE]->(srm);
MATCH (end_user_lighting_user:Role {rolename: "end_user_lighting_user"}), (srm:SummaryReportModel:Model {srmid: "srmid", name: "SummaryReportModel"})
MERGE (end_user_lighting_user)-[:CAN_UPDATE]->(srm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (srm:SummaryReportModel:Model {srmid: "srmid", name: "SummaryReportModel"})
MERGE (end_user_api)-[:CAN_UPDATE]->(srm);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (srm:SummaryReportModel:Model {srmid: "srmid", name: "SummaryReportModel"})
MERGE (parking_owner_admin)-[:CAN_UPDATE]->(srm);
MATCH (policy_authority:Role {rolename: "policy_authority"}), (srm:SummaryReportModel:Model {srmid: "srmid", name: "SummaryReportModel"})
MERGE (policy_authority)-[:CAN_UPDATE]->(srm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (srm:SummaryReportModel:Model {srmid: "srmid", name: "SummaryReportModel"})
MERGE (sensity_user)-[:CAN_DELETE]->(srm);
MATCH (sensity_admin:Role:Admin {rolename: "sensity_admin"}), (srm:SummaryReportModel:Model {srmid: "srmid", name: "SummaryReportModel"})
MERGE (sensity_admin)-[:CAN_DELETE]->(srm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (srm:SummaryReportModel:Model {srmid: "srmid", name: "SummaryReportModel"})
MERGE (partner_admin)-[:CAN_DELETE]->(srm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (srm:SummaryReportModel:Model {srmid: "srmid", name: "SummaryReportModel"})
MERGE (partner_deployment_user)-[:CAN_DELETE]->(srm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (srm:SummaryReportModel:Model {srmid: "srmid", name: "SummaryReportModel"})
MERGE (end_user_admin)-[:CAN_DELETE]->(srm);
MATCH (end_user_lighting_user:Role {rolename: "end_user_lighting_user"}), (srm:SummaryReportModel:Model {srmid: "srmid", name: "SummaryReportModel"})
MERGE (end_user_lighting_user)-[:CAN_DELETE]->(srm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (srm:SummaryReportModel:Model {srmid: "srmid", name: "SummaryReportModel"})
MERGE (end_user_api)-[:CAN_DELETE]->(srm);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (srm:SummaryReportModel:Model {srmid: "srmid", name: "SummaryReportModel"})
MERGE (parking_owner_admin)-[:CAN_DELETE]->(srm);
MATCH (policy_authority:Role {rolename: "policy_authority"}), (srm:SummaryReportModel:Model {srmid: "srmid", name: "SummaryReportModel"})
MERGE (policy_authority)-[:CAN_DELETE]->(srm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (srm:SummaryReportModel:Model {srmid: "srmid", name: "SummaryReportModel"})
MERGE (sensity_user)-[:CAN_READ]->(srm);
MATCH (sensity_admin:Role:Admin {rolename: "sensity_admin"}), (srm:SummaryReportModel:Model {srmid: "srmid", name: "SummaryReportModel"})
MERGE (sensity_admin)-[:CAN_READ]->(srm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (srm:SummaryReportModel:Model {srmid: "srmid", name: "SummaryReportModel"})
MERGE (partner_admin)-[:CAN_READ]->(srm);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (srm:SummaryReportModel:Model {srmid: "srmid", name: "SummaryReportModel"})
MERGE (partner_deployment_user)-[:CAN_READ]->(srm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (srm:SummaryReportModel:Model {srmid: "srmid", name: "SummaryReportModel"})
MERGE (end_user_admin)-[:CAN_READ]->(srm);
MATCH (end_user_lighting_user:Role {rolename: "end_user_lighting_user"}), (srm:SummaryReportModel:Model {srmid: "srmid", name: "SummaryReportModel"})
MERGE (end_user_lighting_user)-[:CAN_READ]->(srm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (srm:SummaryReportModel:Model {srmid: "srmid", name: "SummaryReportModel"})
MERGE (end_user_api)-[:CAN_READ]->(srm);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (srm:SummaryReportModel:Model {srmid: "srmid", name: "SummaryReportModel"})
MERGE (parking_owner_admin)-[:CAN_READ]->(srm);
MATCH (policy_authority:Role {rolename: "policy_authority"}), (srm:SummaryReportModel:Model {srmid: "srmid", name: "SummaryReportModel"})
MERGE (policy_authority)-[:CAN_READ]->(srm);
MATCH (parking_manager:Role {rolename: "parking_manager"}), (srm:SummaryReportModel:Model {srmid: "srmid", name: "SummaryReportModel"})
MERGE (parking_manager)-[:CAN_READ]->(srm);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (bam:BusinessAlertModel:Model {bamid: "bamid", name: "BusinessAlertModel"})
MERGE (parking_owner_admin)-[:CAN_CREATE]->(bam);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (bam:BusinessAlertModel:Model {bamid: "bamid", name: "BusinessAlertModel"})
MERGE (end_user_admin)-[:CAN_CREATE]->(bam);
MATCH (policy_authority:Role {rolename: "policy_authority"}), (bam:BusinessAlertModel:Model {bamid: "bamid", name: "BusinessAlertModel"})
MERGE (policy_authority)-[:CAN_CREATE]->(bam);
MATCH (parking_manager:Role {rolename: "parking_manager"}), (bam:BusinessAlertModel:Model {bamid: "bamid", name: "BusinessAlertModel"})
MERGE (parking_manager)-[:CAN_CREATE]->(bam);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (bam:BusinessAlertModel:Model {bamid: "bamid", name: "BusinessAlertModel"})
MERGE (end_user_api)-[:CAN_CREATE]->(bam);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (bam:BusinessAlertModel:Model {bamid: "bamid", name: "BusinessAlertModel"})
MERGE (partner_admin)-[:CAN_CREATE]->(bam);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (bam:BusinessAlertModel:Model {bamid: "bamid", name: "BusinessAlertModel"})
MERGE (sensity_user)-[:CAN_CREATE]->(bam);
MATCH (sensity_admin:Role:Admin {rolename: "sensity_admin"}), (bam:BusinessAlertModel:Model {bamid: "bamid", name: "BusinessAlertModel"})
MERGE (sensity_admin)-[:CAN_CREATE]->(bam);
MATCH (partner_api:Role {rolename: "partner_api"}), (bam:BusinessAlertModel:Model {bamid: "bamid", name: "BusinessAlertModel"})
MERGE (partner_api)-[:CAN_CREATE]->(bam);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (bam:BusinessAlertModel:Model {bamid: "bamid", name: "BusinessAlertModel"})
MERGE (parking_owner_admin)-[:CAN_UPDATE]->(bam);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (bam:BusinessAlertModel:Model {bamid: "bamid", name: "BusinessAlertModel"})
MERGE (end_user_admin)-[:CAN_UPDATE]->(bam);
MATCH (policy_authority:Role {rolename: "policy_authority"}), (bam:BusinessAlertModel:Model {bamid: "bamid", name: "BusinessAlertModel"})
MERGE (policy_authority)-[:CAN_UPDATE]->(bam);
MATCH (parking_manager:Role {rolename: "parking_manager"}), (bam:BusinessAlertModel:Model {bamid: "bamid", name: "BusinessAlertModel"})
MERGE (parking_manager)-[:CAN_UPDATE]->(bam);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (bam:BusinessAlertModel:Model {bamid: "bamid", name: "BusinessAlertModel"})
MERGE (end_user_api)-[:CAN_UPDATE]->(bam);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (bam:BusinessAlertModel:Model {bamid: "bamid", name: "BusinessAlertModel"})
MERGE (partner_admin)-[:CAN_UPDATE]->(bam);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (bam:BusinessAlertModel:Model {bamid: "bamid", name: "BusinessAlertModel"})
MERGE (sensity_user)-[:CAN_UPDATE]->(bam);
MATCH (sensity_admin:Role:Admin {rolename: "sensity_admin"}), (bam:BusinessAlertModel:Model {bamid: "bamid", name: "BusinessAlertModel"})
MERGE (sensity_admin)-[:CAN_UPDATE]->(bam);
MATCH (partner_api:Role {rolename: "partner_api"}), (bam:BusinessAlertModel:Model {bamid: "bamid", name: "BusinessAlertModel"})
MERGE (partner_api)-[:CAN_UPDATE]->(bam);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (bam:BusinessAlertModel:Model {bamid: "bamid", name: "BusinessAlertModel"})
MERGE (parking_owner_admin)-[:CAN_DELETE]->(bam);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (bam:BusinessAlertModel:Model {bamid: "bamid", name: "BusinessAlertModel"})
MERGE (end_user_admin)-[:CAN_DELETE]->(bam);
MATCH (policy_authority:Role {rolename: "policy_authority"}), (bam:BusinessAlertModel:Model {bamid: "bamid", name: "BusinessAlertModel"})
MERGE (policy_authority)-[:CAN_DELETE]->(bam);
MATCH (parking_manager:Role {rolename: "parking_manager"}), (bam:BusinessAlertModel:Model {bamid: "bamid", name: "BusinessAlertModel"})
MERGE (parking_manager)-[:CAN_DELETE]->(bam);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (bam:BusinessAlertModel:Model {bamid: "bamid", name: "BusinessAlertModel"})
MERGE (end_user_api)-[:CAN_DELETE]->(bam);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (bam:BusinessAlertModel:Model {bamid: "bamid", name: "BusinessAlertModel"})
MERGE (partner_admin)-[:CAN_DELETE]->(bam);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (bam:BusinessAlertModel:Model {bamid: "bamid", name: "BusinessAlertModel"})
MERGE (sensity_user)-[:CAN_DELETE]->(bam);
MATCH (sensity_admin:Role:Admin {rolename: "sensity_admin"}), (bam:BusinessAlertModel:Model {bamid: "bamid", name: "BusinessAlertModel"})
MERGE (sensity_admin)-[:CAN_DELETE]->(bam);
MATCH (partner_api:Role {rolename: "partner_api"}), (bam:BusinessAlertModel:Model {bamid: "bamid", name: "BusinessAlertModel"})
MERGE (partner_api)-[:CAN_DELETE]->(bam);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (bam:BusinessAlertModel:Model {bamid: "bamid", name: "BusinessAlertModel"})
MERGE (parking_owner_admin)-[:CAN_READ]->(bam);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (bam:BusinessAlertModel:Model {bamid: "bamid", name: "BusinessAlertModel"})
MERGE (end_user_admin)-[:CAN_READ]->(bam);
MATCH (policy_authority:Role {rolename: "policy_authority"}), (bam:BusinessAlertModel:Model {bamid: "bamid", name: "BusinessAlertModel"})
MERGE (policy_authority)-[:CAN_READ]->(bam);
MATCH (parking_manager:Role {rolename: "parking_manager"}), (bam:BusinessAlertModel:Model {bamid: "bamid", name: "BusinessAlertModel"})
MERGE (parking_manager)-[:CAN_READ]->(bam);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (bam:BusinessAlertModel:Model {bamid: "bamid", name: "BusinessAlertModel"})
MERGE (end_user_api)-[:CAN_READ]->(bam);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (bam:BusinessAlertModel:Model {bamid: "bamid", name: "BusinessAlertModel"})
MERGE (partner_admin)-[:CAN_READ]->(bam);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (bam:BusinessAlertModel:Model {bamid: "bamid", name: "BusinessAlertModel"})
MERGE (sensity_user)-[:CAN_READ]->(bam);
MATCH (sensity_admin:Role:Admin {rolename: "sensity_admin"}), (bam:BusinessAlertModel:Model {bamid: "bamid", name: "BusinessAlertModel"})
MERGE (sensity_admin)-[:CAN_READ]->(bam);
MATCH (partner_api:Role {rolename: "partner_api"}), (bam:BusinessAlertModel:Model {bamid: "bamid", name: "BusinessAlertModel"})
MERGE (partner_api)-[:CAN_READ]->(bam);
MATCH (sensity_read_only:Role:Admin {rolename: "sensity_read_only"}), (bam:BusinessAlertModel:Model {bamid: "bamid", name: "BusinessAlertModel"})
MERGE (sensity_read_only)-[:CAN_READ]->(bam);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (tm:TriggerModel:Model {tmid: "tmid", name: "TriggerModel"})
MERGE (parking_owner_admin)-[:CAN_CREATE]->(tm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (tm:TriggerModel:Model {tmid: "tmid", name: "TriggerModel"})
MERGE (end_user_admin)-[:CAN_CREATE]->(tm);
MATCH (policy_authority:Role {rolename: "policy_authority"}), (tm:TriggerModel:Model {tmid: "tmid", name: "TriggerModel"})
MERGE (policy_authority)-[:CAN_CREATE]->(tm);
MATCH (parking_manager:Role {rolename: "parking_manager"}), (tm:TriggerModel:Model {tmid: "tmid", name: "TriggerModel"})
MERGE (parking_manager)-[:CAN_CREATE]->(tm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (tm:TriggerModel:Model {tmid: "tmid", name: "TriggerModel"})
MERGE (end_user_api)-[:CAN_CREATE]->(tm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (tm:TriggerModel:Model {tmid: "tmid", name: "TriggerModel"})
MERGE (partner_admin)-[:CAN_CREATE]->(tm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (tm:TriggerModel:Model {tmid: "tmid", name: "TriggerModel"})
MERGE (sensity_user)-[:CAN_CREATE]->(tm);
MATCH (sensity_admin:Role:Admin {rolename: "sensity_admin"}), (tm:TriggerModel:Model {tmid: "tmid", name: "TriggerModel"})
MERGE (sensity_admin)-[:CAN_CREATE]->(tm);
MATCH (partner_api:Role {rolename: "partner_api"}), (tm:TriggerModel:Model {tmid: "tmid", name: "TriggerModel"})
MERGE (partner_api)-[:CAN_CREATE]->(tm);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (tm:TriggerModel:Model {tmid: "tmid", name: "TriggerModel"})
MERGE (parking_owner_admin)-[:CAN_UPDATE]->(tm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (tm:TriggerModel:Model {tmid: "tmid", name: "TriggerModel"})
MERGE (end_user_admin)-[:CAN_UPDATE]->(tm);
MATCH (policy_authority:Role {rolename: "policy_authority"}), (tm:TriggerModel:Model {tmid: "tmid", name: "TriggerModel"})
MERGE (policy_authority)-[:CAN_UPDATE]->(tm);
MATCH (parking_manager:Role {rolename: "parking_manager"}), (tm:TriggerModel:Model {tmid: "tmid", name: "TriggerModel"})
MERGE (parking_manager)-[:CAN_UPDATE]->(tm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (tm:TriggerModel:Model {tmid: "tmid", name: "TriggerModel"})
MERGE (end_user_api)-[:CAN_UPDATE]->(tm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (tm:TriggerModel:Model {tmid: "tmid", name: "TriggerModel"})
MERGE (partner_admin)-[:CAN_UPDATE]->(tm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (tm:TriggerModel:Model {tmid: "tmid", name: "TriggerModel"})
MERGE (sensity_user)-[:CAN_UPDATE]->(tm);
MATCH (sensity_admin:Role:Admin {rolename: "sensity_admin"}), (tm:TriggerModel:Model {tmid: "tmid", name: "TriggerModel"})
MERGE (sensity_admin)-[:CAN_UPDATE]->(tm);
MATCH (partner_api:Role {rolename: "partner_api"}), (tm:TriggerModel:Model {tmid: "tmid", name: "TriggerModel"})
MERGE (partner_api)-[:CAN_UPDATE]->(tm);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (tm:TriggerModel:Model {tmid: "tmid", name: "TriggerModel"})
MERGE (parking_owner_admin)-[:CAN_DELETE]->(tm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (tm:TriggerModel:Model {tmid: "tmid", name: "TriggerModel"})
MERGE (end_user_admin)-[:CAN_DELETE]->(tm);
MATCH (policy_authority:Role {rolename: "policy_authority"}), (tm:TriggerModel:Model {tmid: "tmid", name: "TriggerModel"})
MERGE (policy_authority)-[:CAN_DELETE]->(tm);
MATCH (parking_manager:Role {rolename: "parking_manager"}), (tm:TriggerModel:Model {tmid: "tmid", name: "TriggerModel"})
MERGE (parking_manager)-[:CAN_DELETE]->(tm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (tm:TriggerModel:Model {tmid: "tmid", name: "TriggerModel"})
MERGE (end_user_api)-[:CAN_DELETE]->(tm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (tm:TriggerModel:Model {tmid: "tmid", name: "TriggerModel"})
MERGE (partner_admin)-[:CAN_DELETE]->(tm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (tm:TriggerModel:Model {tmid: "tmid", name: "TriggerModel"})
MERGE (sensity_user)-[:CAN_DELETE]->(tm);
MATCH (sensity_admin:Role:Admin {rolename: "sensity_admin"}), (tm:TriggerModel:Model {tmid: "tmid", name: "TriggerModel"})
MERGE (sensity_admin)-[:CAN_DELETE]->(tm);
MATCH (partner_api:Role {rolename: "partner_api"}), (tm:TriggerModel:Model {tmid: "tmid", name: "TriggerModel"})
MERGE (partner_api)-[:CAN_DELETE]->(tm);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (tm:TriggerModel:Model {tmid: "tmid", name: "TriggerModel"})
MERGE (parking_owner_admin)-[:CAN_READ]->(tm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (tm:TriggerModel:Model {tmid: "tmid", name: "TriggerModel"})
MERGE (end_user_admin)-[:CAN_READ]->(tm);
MATCH (policy_authority:Role {rolename: "policy_authority"}), (tm:TriggerModel:Model {tmid: "tmid", name: "TriggerModel"})
MERGE (policy_authority)-[:CAN_READ]->(tm);
MATCH (parking_manager:Role {rolename: "parking_manager"}), (tm:TriggerModel:Model {tmid: "tmid", name: "TriggerModel"})
MERGE (parking_manager)-[:CAN_READ]->(tm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (tm:TriggerModel:Model {tmid: "tmid", name: "TriggerModel"})
MERGE (end_user_api)-[:CAN_READ]->(tm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (tm:TriggerModel:Model {tmid: "tmid", name: "TriggerModel"})
MERGE (partner_admin)-[:CAN_READ]->(tm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (tm:TriggerModel:Model {tmid: "tmid", name: "TriggerModel"})
MERGE (sensity_user)-[:CAN_READ]->(tm);
MATCH (sensity_admin:Role:Admin {rolename: "sensity_admin"}), (tm:TriggerModel:Model {tmid: "tmid", name: "TriggerModel"})
MERGE (sensity_admin)-[:CAN_READ]->(tm);
MATCH (partner_api:Role {rolename: "partner_api"}), (tm:TriggerModel:Model {tmid: "tmid", name: "TriggerModel"})
MERGE (partner_api)-[:CAN_READ]->(tm);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (wam:WhatIfAnalysisModel:Model {wamid: "wamid", name: "WhatIfAnalysisModel"})
MERGE (parking_owner_admin)-[:CAN_CREATE]->(wam);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (wam:WhatIfAnalysisModel:Model {wamid: "wamid", name: "WhatIfAnalysisModel"})
MERGE (end_user_admin)-[:CAN_CREATE]->(wam);
MATCH (policy_authority:Role {rolename: "policy_authority"}), (wam:WhatIfAnalysisModel:Model {wamid: "wamid", name: "WhatIfAnalysisModel"})
MERGE (policy_authority)-[:CAN_CREATE]->(wam);
MATCH (parking_manager:Role {rolename: "parking_manager"}), (wam:WhatIfAnalysisModel:Model {wamid: "wamid", name: "WhatIfAnalysisModel"})
MERGE (parking_manager)-[:CAN_CREATE]->(wam);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (wam:WhatIfAnalysisModel:Model {wamid: "wamid", name: "WhatIfAnalysisModel"})
MERGE (end_user_api)-[:CAN_CREATE]->(wam);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (wam:WhatIfAnalysisModel:Model {wamid: "wamid", name: "WhatIfAnalysisModel"})
MERGE (partner_admin)-[:CAN_CREATE]->(wam);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (wam:WhatIfAnalysisModel:Model {wamid: "wamid", name: "WhatIfAnalysisModel"})
MERGE (sensity_user)-[:CAN_CREATE]->(wam);
MATCH (sensity_admin:Role:Admin {rolename: "sensity_admin"}), (wam:WhatIfAnalysisModel:Model {wamid: "wamid", name: "WhatIfAnalysisModel"})
MERGE (sensity_admin)-[:CAN_CREATE]->(wam);
MATCH (partner_api:Role {rolename: "partner_api"}), (wam:WhatIfAnalysisModel:Model {wamid: "wamid", name: "WhatIfAnalysisModel"})
MERGE (partner_api)-[:CAN_CREATE]->(wam);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (wam:WhatIfAnalysisModel:Model {wamid: "wamid", name: "WhatIfAnalysisModel"})
MERGE (parking_owner_admin)-[:CAN_UPDATE]->(wam);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (wam:WhatIfAnalysisModel:Model {wamid: "wamid", name: "WhatIfAnalysisModel"})
MERGE (end_user_admin)-[:CAN_UPDATE]->(wam);
MATCH (policy_authority:Role {rolename: "policy_authority"}), (wam:WhatIfAnalysisModel:Model {wamid: "wamid", name: "WhatIfAnalysisModel"})
MERGE (policy_authority)-[:CAN_UPDATE]->(wam);
MATCH (parking_manager:Role {rolename: "parking_manager"}), (wam:WhatIfAnalysisModel:Model {wamid: "wamid", name: "WhatIfAnalysisModel"})
MERGE (parking_manager)-[:CAN_UPDATE]->(wam);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (wam:WhatIfAnalysisModel:Model {wamid: "wamid", name: "WhatIfAnalysisModel"})
MERGE (end_user_api)-[:CAN_UPDATE]->(wam);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (wam:WhatIfAnalysisModel:Model {wamid: "wamid", name: "WhatIfAnalysisModel"})
MERGE (partner_admin)-[:CAN_UPDATE]->(wam);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (wam:WhatIfAnalysisModel:Model {wamid: "wamid", name: "WhatIfAnalysisModel"})
MERGE (sensity_user)-[:CAN_UPDATE]->(wam);
MATCH (sensity_admin:Role:Admin {rolename: "sensity_admin"}), (wam:WhatIfAnalysisModel:Model {wamid: "wamid", name: "WhatIfAnalysisModel"})
MERGE (sensity_admin)-[:CAN_UPDATE]->(wam);
MATCH (partner_api:Role {rolename: "partner_api"}), (wam:WhatIfAnalysisModel:Model {wamid: "wamid", name: "WhatIfAnalysisModel"})
MERGE (partner_api)-[:CAN_UPDATE]->(wam);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (wam:WhatIfAnalysisModel:Model {wamid: "wamid", name: "WhatIfAnalysisModel"})
MERGE (parking_owner_admin)-[:CAN_DELETE]->(wam);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (wam:WhatIfAnalysisModel:Model {wamid: "wamid", name: "WhatIfAnalysisModel"})
MERGE (end_user_admin)-[:CAN_DELETE]->(wam);
MATCH (policy_authority:Role {rolename: "policy_authority"}), (wam:WhatIfAnalysisModel:Model {wamid: "wamid", name: "WhatIfAnalysisModel"})
MERGE (policy_authority)-[:CAN_DELETE]->(wam);
MATCH (parking_manager:Role {rolename: "parking_manager"}), (wam:WhatIfAnalysisModel:Model {wamid: "wamid", name: "WhatIfAnalysisModel"})
MERGE (parking_manager)-[:CAN_DELETE]->(wam);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (wam:WhatIfAnalysisModel:Model {wamid: "wamid", name: "WhatIfAnalysisModel"})
MERGE (end_user_api)-[:CAN_DELETE]->(wam);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (wam:WhatIfAnalysisModel:Model {wamid: "wamid", name: "WhatIfAnalysisModel"})
MERGE (partner_admin)-[:CAN_DELETE]->(wam);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (wam:WhatIfAnalysisModel:Model {wamid: "wamid", name: "WhatIfAnalysisModel"})
MERGE (sensity_user)-[:CAN_DELETE]->(wam);
MATCH (sensity_admin:Role:Admin {rolename: "sensity_admin"}), (wam:WhatIfAnalysisModel:Model {wamid: "wamid", name: "WhatIfAnalysisModel"})
MERGE (sensity_admin)-[:CAN_DELETE]->(wam);
MATCH (partner_api:Role {rolename: "partner_api"}), (wam:WhatIfAnalysisModel:Model {wamid: "wamid", name: "WhatIfAnalysisModel"})
MERGE (partner_api)-[:CAN_DELETE]->(wam);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (wam:WhatIfAnalysisModel:Model {wamid: "wamid", name: "WhatIfAnalysisModel"})
MERGE (parking_owner_admin)-[:CAN_READ]->(wam);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (wam:WhatIfAnalysisModel:Model {wamid: "wamid", name: "WhatIfAnalysisModel"})
MERGE (end_user_admin)-[:CAN_READ]->(wam);
MATCH (policy_authority:Role {rolename: "policy_authority"}), (wam:WhatIfAnalysisModel:Model {wamid: "wamid", name: "WhatIfAnalysisModel"})
MERGE (policy_authority)-[:CAN_READ]->(wam);
MATCH (parking_manager:Role {rolename: "parking_manager"}), (wam:WhatIfAnalysisModel:Model {wamid: "wamid", name: "WhatIfAnalysisModel"})
MERGE (parking_manager)-[:CAN_READ]->(wam);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (wam:WhatIfAnalysisModel:Model {wamid: "wamid", name: "WhatIfAnalysisModel"})
MERGE (end_user_api)-[:CAN_READ]->(wam);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (wam:WhatIfAnalysisModel:Model {wamid: "wamid", name: "WhatIfAnalysisModel"})
MERGE (partner_admin)-[:CAN_READ]->(wam);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (wam:WhatIfAnalysisModel:Model {wamid: "wamid", name: "WhatIfAnalysisModel"})
MERGE (sensity_user)-[:CAN_READ]->(wam);
MATCH (sensity_admin:Role:Admin {rolename: "sensity_admin"}), (wam:WhatIfAnalysisModel:Model {wamid: "wamid", name: "WhatIfAnalysisModel"})
MERGE (sensity_admin)-[:CAN_READ]->(wam);
MATCH (partner_api:Role {rolename: "partner_api"}), (wam:WhatIfAnalysisModel:Model {wamid: "wamid", name: "WhatIfAnalysisModel"})
MERGE (partner_api)-[:CAN_READ]->(wam);
MATCH (sensity_read_only:Role:Admin {rolename: "sensity_read_only"}), (wam:WhatIfAnalysisModel:Model {wamid: "wamid", name: "WhatIfAnalysisModel"})
MERGE (sensity_read_only)-[:CAN_READ]->(wam);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (wpm:WhatIfPolicyModel:Model {wpmid: "wpmid", name: "WhatIfPolicyModel"})
MERGE (parking_owner_admin)-[:CAN_CREATE]->(wpm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (wpm:WhatIfPolicyModel:Model {wpmid: "wpmid", name: "WhatIfPolicyModel"})
MERGE (end_user_admin)-[:CAN_CREATE]->(wpm);
MATCH (policy_authority:Role {rolename: "policy_authority"}), (wpm:WhatIfPolicyModel:Model {wpmid: "wpmid", name: "WhatIfPolicyModel"})
MERGE (policy_authority)-[:CAN_CREATE]->(wpm);
MATCH (parking_manager:Role {rolename: "parking_manager"}), (wpm:WhatIfPolicyModel:Model {wpmid: "wpmid", name: "WhatIfPolicyModel"})
MERGE (parking_manager)-[:CAN_CREATE]->(wpm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (wpm:WhatIfPolicyModel:Model {wpmid: "wpmid", name: "WhatIfPolicyModel"})
MERGE (end_user_api)-[:CAN_CREATE]->(wpm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (wpm:WhatIfPolicyModel:Model {wpmid: "wpmid", name: "WhatIfPolicyModel"})
MERGE (partner_admin)-[:CAN_CREATE]->(wpm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (wpm:WhatIfPolicyModel:Model {wpmid: "wpmid", name: "WhatIfPolicyModel"})
MERGE (sensity_user)-[:CAN_CREATE]->(wpm);
MATCH (sensity_admin:Role:Admin {rolename: "sensity_admin"}), (wpm:WhatIfPolicyModel:Model {wpmid: "wpmid", name: "WhatIfPolicyModel"})
MERGE (sensity_admin)-[:CAN_CREATE]->(wpm);
MATCH (partner_api:Role {rolename: "partner_api"}), (wpm:WhatIfPolicyModel:Model {wpmid: "wpmid", name: "WhatIfPolicyModel"})
MERGE (partner_api)-[:CAN_CREATE]->(wpm);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (wpm:WhatIfPolicyModel:Model {wpmid: "wpmid", name: "WhatIfPolicyModel"})
MERGE (parking_owner_admin)-[:CAN_UPDATE]->(wpm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (wpm:WhatIfPolicyModel:Model {wpmid: "wpmid", name: "WhatIfPolicyModel"})
MERGE (end_user_admin)-[:CAN_UPDATE]->(wpm);
MATCH (policy_authority:Role {rolename: "policy_authority"}), (wpm:WhatIfPolicyModel:Model {wpmid: "wpmid", name: "WhatIfPolicyModel"})
MERGE (policy_authority)-[:CAN_UPDATE]->(wpm);
MATCH (parking_manager:Role {rolename: "parking_manager"}), (wpm:WhatIfPolicyModel:Model {wpmid: "wpmid", name: "WhatIfPolicyModel"})
MERGE (parking_manager)-[:CAN_UPDATE]->(wpm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (wpm:WhatIfPolicyModel:Model {wpmid: "wpmid", name: "WhatIfPolicyModel"})
MERGE (end_user_api)-[:CAN_UPDATE]->(wpm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (wpm:WhatIfPolicyModel:Model {wpmid: "wpmid", name: "WhatIfPolicyModel"})
MERGE (partner_admin)-[:CAN_UPDATE]->(wpm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (wpm:WhatIfPolicyModel:Model {wpmid: "wpmid", name: "WhatIfPolicyModel"})
MERGE (sensity_user)-[:CAN_UPDATE]->(wpm);
MATCH (sensity_admin:Role:Admin {rolename: "sensity_admin"}), (wpm:WhatIfPolicyModel:Model {wpmid: "wpmid", name: "WhatIfPolicyModel"})
MERGE (sensity_admin)-[:CAN_UPDATE]->(wpm);
MATCH (partner_api:Role {rolename: "partner_api"}), (wpm:WhatIfPolicyModel:Model {wpmid: "wpmid", name: "WhatIfPolicyModel"})
MERGE (partner_api)-[:CAN_UPDATE]->(wpm);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (wpm:WhatIfPolicyModel:Model {wpmid: "wpmid", name: "WhatIfPolicyModel"})
MERGE (parking_owner_admin)-[:CAN_DELETE]->(wpm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (wpm:WhatIfPolicyModel:Model {wpmid: "wpmid", name: "WhatIfPolicyModel"})
MERGE (end_user_admin)-[:CAN_DELETE]->(wpm);
MATCH (policy_authority:Role {rolename: "policy_authority"}), (wpm:WhatIfPolicyModel:Model {wpmid: "wpmid", name: "WhatIfPolicyModel"})
MERGE (policy_authority)-[:CAN_DELETE]->(wpm);
MATCH (parking_manager:Role {rolename: "parking_manager"}), (wpm:WhatIfPolicyModel:Model {wpmid: "wpmid", name: "WhatIfPolicyModel"})
MERGE (parking_manager)-[:CAN_DELETE]->(wpm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (wpm:WhatIfPolicyModel:Model {wpmid: "wpmid", name: "WhatIfPolicyModel"})
MERGE (end_user_api)-[:CAN_DELETE]->(wpm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (wpm:WhatIfPolicyModel:Model {wpmid: "wpmid", name: "WhatIfPolicyModel"})
MERGE (partner_admin)-[:CAN_DELETE]->(wpm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (wpm:WhatIfPolicyModel:Model {wpmid: "wpmid", name: "WhatIfPolicyModel"})
MERGE (sensity_user)-[:CAN_DELETE]->(wpm);
MATCH (sensity_admin:Role:Admin {rolename: "sensity_admin"}), (wpm:WhatIfPolicyModel:Model {wpmid: "wpmid", name: "WhatIfPolicyModel"})
MERGE (sensity_admin)-[:CAN_DELETE]->(wpm);
MATCH (partner_api:Role {rolename: "partner_api"}), (wpm:WhatIfPolicyModel:Model {wpmid: "wpmid", name: "WhatIfPolicyModel"})
MERGE (partner_api)-[:CAN_DELETE]->(wpm);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (wpm:WhatIfPolicyModel:Model {wpmid: "wpmid", name: "WhatIfPolicyModel"})
MERGE (parking_owner_admin)-[:CAN_READ]->(wpm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (wpm:WhatIfPolicyModel:Model {wpmid: "wpmid", name: "WhatIfPolicyModel"})
MERGE (end_user_admin)-[:CAN_READ]->(wpm);
MATCH (policy_authority:Role {rolename: "policy_authority"}), (wpm:WhatIfPolicyModel:Model {wpmid: "wpmid", name: "WhatIfPolicyModel"})
MERGE (policy_authority)-[:CAN_READ]->(wpm);
MATCH (parking_manager:Role {rolename: "parking_manager"}), (wpm:WhatIfPolicyModel:Model {wpmid: "wpmid", name: "WhatIfPolicyModel"})
MERGE (parking_manager)-[:CAN_READ]->(wpm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (wpm:WhatIfPolicyModel:Model {wpmid: "wpmid", name: "WhatIfPolicyModel"})
MERGE (end_user_api)-[:CAN_READ]->(wpm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (wpm:WhatIfPolicyModel:Model {wpmid: "wpmid", name: "WhatIfPolicyModel"})
MERGE (partner_admin)-[:CAN_READ]->(wpm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (wpm:WhatIfPolicyModel:Model {wpmid: "wpmid", name: "WhatIfPolicyModel"})
MERGE (sensity_user)-[:CAN_READ]->(wpm);
MATCH (sensity_admin:Role:Admin {rolename: "sensity_admin"}), (wpm:WhatIfPolicyModel:Model {wpmid: "wpmid", name: "WhatIfPolicyModel"})
MERGE (sensity_admin)-[:CAN_READ]->(wpm);
MATCH (partner_api:Role {rolename: "partner_api"}), (wpm:WhatIfPolicyModel:Model {wpmid: "wpmid", name: "WhatIfPolicyModel"})
MERGE (partner_api)-[:CAN_READ]->(wpm);
MATCH (sensity_read_only:Role:Admin {rolename: "sensity_read_only"}), (wpm:WhatIfPolicyModel:Model {wpmid: "wpmid", name: "WhatIfPolicyModel"})
MERGE (sensity_read_only)-[:CAN_READ]->(wpm);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (wtm:WhatIfTagModel:Model {wtmid: "wtmid", name: "WhatIfTagModel"})
MERGE (parking_owner_admin)-[:CAN_CREATE]->(wtm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (wtm:WhatIfTagModel:Model {wtmid: "wtmid", name: "WhatIfTagModel"})
MERGE (end_user_admin)-[:CAN_CREATE]->(wtm);
MATCH (policy_authority:Role {rolename: "policy_authority"}), (wtm:WhatIfTagModel:Model {wtmid: "wtmid", name: "WhatIfTagModel"})
MERGE (policy_authority)-[:CAN_CREATE]->(wtm);
MATCH (parking_manager:Role {rolename: "parking_manager"}), (wtm:WhatIfTagModel:Model {wtmid: "wtmid", name: "WhatIfTagModel"})
MERGE (parking_manager)-[:CAN_CREATE]->(wtm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (wtm:WhatIfTagModel:Model {wtmid: "wtmid", name: "WhatIfTagModel"})
MERGE (end_user_api)-[:CAN_CREATE]->(wtm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (wtm:WhatIfTagModel:Model {wtmid: "wtmid", name: "WhatIfTagModel"})
MERGE (partner_admin)-[:CAN_CREATE]->(wtm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (wtm:WhatIfTagModel:Model {wtmid: "wtmid", name: "WhatIfTagModel"})
MERGE (sensity_user)-[:CAN_CREATE]->(wtm);
MATCH (sensity_admin:Role:Admin {rolename: "sensity_admin"}), (wtm:WhatIfTagModel:Model {wtmid: "wtmid", name: "WhatIfTagModel"})
MERGE (sensity_admin)-[:CAN_CREATE]->(wtm);
MATCH (partner_api:Role {rolename: "partner_api"}), (wtm:WhatIfTagModel:Model {wtmid: "wtmid", name: "WhatIfTagModel"})
MERGE (partner_api)-[:CAN_CREATE]->(wtm);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (wtm:WhatIfTagModel:Model {wtmid: "wtmid", name: "WhatIfTagModel"})
MERGE (parking_owner_admin)-[:CAN_UPDATE]->(wtm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (wtm:WhatIfTagModel:Model {wtmid: "wtmid", name: "WhatIfTagModel"})
MERGE (end_user_admin)-[:CAN_UPDATE]->(wtm);
MATCH (policy_authority:Role {rolename: "policy_authority"}), (wtm:WhatIfTagModel:Model {wtmid: "wtmid", name: "WhatIfTagModel"})
MERGE (policy_authority)-[:CAN_UPDATE]->(wtm);
MATCH (parking_manager:Role {rolename: "parking_manager"}), (wtm:WhatIfTagModel:Model {wtmid: "wtmid", name: "WhatIfTagModel"})
MERGE (parking_manager)-[:CAN_UPDATE]->(wtm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (wtm:WhatIfTagModel:Model {wtmid: "wtmid", name: "WhatIfTagModel"})
MERGE (end_user_api)-[:CAN_UPDATE]->(wtm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (wtm:WhatIfTagModel:Model {wtmid: "wtmid", name: "WhatIfTagModel"})
MERGE (partner_admin)-[:CAN_UPDATE]->(wtm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (wtm:WhatIfTagModel:Model {wtmid: "wtmid", name: "WhatIfTagModel"})
MERGE (sensity_user)-[:CAN_UPDATE]->(wtm);
MATCH (sensity_admin:Role:Admin {rolename: "sensity_admin"}), (wtm:WhatIfTagModel:Model {wtmid: "wtmid", name: "WhatIfTagModel"})
MERGE (sensity_admin)-[:CAN_UPDATE]->(wtm);
MATCH (partner_api:Role {rolename: "partner_api"}), (wtm:WhatIfTagModel:Model {wtmid: "wtmid", name: "WhatIfTagModel"})
MERGE (partner_api)-[:CAN_UPDATE]->(wtm);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (wtm:WhatIfTagModel:Model {wtmid: "wtmid", name: "WhatIfTagModel"})
MERGE (parking_owner_admin)-[:CAN_DELETE]->(wtm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (wtm:WhatIfTagModel:Model {wtmid: "wtmid", name: "WhatIfTagModel"})
MERGE (end_user_admin)-[:CAN_DELETE]->(wtm);
MATCH (policy_authority:Role {rolename: "policy_authority"}), (wtm:WhatIfTagModel:Model {wtmid: "wtmid", name: "WhatIfTagModel"})
MERGE (policy_authority)-[:CAN_DELETE]->(wtm);
MATCH (parking_manager:Role {rolename: "parking_manager"}), (wtm:WhatIfTagModel:Model {wtmid: "wtmid", name: "WhatIfTagModel"})
MERGE (parking_manager)-[:CAN_DELETE]->(wtm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (wtm:WhatIfTagModel:Model {wtmid: "wtmid", name: "WhatIfTagModel"})
MERGE (end_user_api)-[:CAN_DELETE]->(wtm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (wtm:WhatIfTagModel:Model {wtmid: "wtmid", name: "WhatIfTagModel"})
MERGE (partner_admin)-[:CAN_DELETE]->(wtm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (wtm:WhatIfTagModel:Model {wtmid: "wtmid", name: "WhatIfTagModel"})
MERGE (sensity_user)-[:CAN_DELETE]->(wtm);
MATCH (sensity_admin:Role:Admin {rolename: "sensity_admin"}), (wtm:WhatIfTagModel:Model {wtmid: "wtmid", name: "WhatIfTagModel"})
MERGE (sensity_admin)-[:CAN_DELETE]->(wtm);
MATCH (partner_api:Role {rolename: "partner_api"}), (wtm:WhatIfTagModel:Model {wtmid: "wtmid", name: "WhatIfTagModel"})
MERGE (partner_api)-[:CAN_DELETE]->(wtm);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (wtm:WhatIfTagModel:Model {wtmid: "wtmid", name: "WhatIfTagModel"})
MERGE (parking_owner_admin)-[:CAN_READ]->(wtm);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (wtm:WhatIfTagModel:Model {wtmid: "wtmid", name: "WhatIfTagModel"})
MERGE (end_user_admin)-[:CAN_READ]->(wtm);
MATCH (policy_authority:Role {rolename: "policy_authority"}), (wtm:WhatIfTagModel:Model {wtmid: "wtmid", name: "WhatIfTagModel"})
MERGE (policy_authority)-[:CAN_READ]->(wtm);
MATCH (parking_manager:Role {rolename: "parking_manager"}), (wtm:WhatIfTagModel:Model {wtmid: "wtmid", name: "WhatIfTagModel"})
MERGE (parking_manager)-[:CAN_READ]->(wtm);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (wtm:WhatIfTagModel:Model {wtmid: "wtmid", name: "WhatIfTagModel"})
MERGE (end_user_api)-[:CAN_READ]->(wtm);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (wtm:WhatIfTagModel:Model {wtmid: "wtmid", name: "WhatIfTagModel"})
MERGE (partner_admin)-[:CAN_READ]->(wtm);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (wtm:WhatIfTagModel:Model {wtmid: "wtmid", name: "WhatIfTagModel"})
MERGE (sensity_user)-[:CAN_READ]->(wtm);
MATCH (sensity_admin:Role:Admin {rolename: "sensity_admin"}), (wtm:WhatIfTagModel:Model {wtmid: "wtmid", name: "WhatIfTagModel"})
MERGE (sensity_admin)-[:CAN_READ]->(wtm);
MATCH (partner_api:Role {rolename: "partner_api"}), (wtm:WhatIfTagModel:Model {wtmid: "wtmid", name: "WhatIfTagModel"})
MERGE (partner_api)-[:CAN_READ]->(wtm);
MATCH (sensity_read_only:Role:Admin {rolename: "sensity_read_only"}), (wtm:WhatIfTagModel:Model {wtmid: "wtmid", name: "WhatIfTagModel"})
MERGE (sensity_read_only)-[:CAN_READ]->(wtm);
MATCH (sensity_read_only:Role:Admin {rolename: "sensity_read_only"}), (tm:TriggerModel:Model {tmid: "tmid", name: "TriggerModel"})
MERGE (sensity_read_only)-[:CAN_READ]->(tm);
MATCH (sensity_admin:Role:Admin {rolename: "sensity_admin"}), (gps:GpsModel:Model {gpsid: "gpsid", name: "GpsModel"})
MERGE (sensity_admin)-[:CAN_READ]->(gps);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (gps:GpsModel:Model {gpsid: "gpsid", name: "GpsModel"})
MERGE (sensity_user)-[:CAN_READ]->(gps);
MATCH (sensity_read_only:Role:Admin {rolename: "sensity_read_only"}), (gps:GpsModel:Model {gpsid: "gpsid", name: "GpsModel"})
MERGE (sensity_read_only)-[:CAN_READ]->(gps);
MATCH (partner_admin:Role {rolename: "partner_admin"}), (gps:GpsModel:Model {gpsid: "gpsid", name: "GpsModel"})
MERGE (partner_admin)-[:CAN_READ]->(gps);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (gps:GpsModel:Model {gpsid: "gpsid", name: "GpsModel"})
MERGE (partner_deployment_user)-[:CAN_READ]->(gps);
MATCH (partner_lighting_user:Role {rolename: "partner_lighting_user"}), (gps:GpsModel:Model {gpsid: "gpsid", name: "GpsModel"})
MERGE (partner_lighting_user)-[:CAN_READ]->(gps);
MATCH (partner_sensor_user:Role {rolename: "partner_sensor_user"}), (gps:GpsModel:Model {gpsid: "gpsid", name: "GpsModel"})
MERGE (partner_sensor_user)-[:CAN_READ]->(gps);
MATCH (partner_api:Role {rolename: "partner_api"}), (gps:GpsModel:Model {gpsid: "gpsid", name: "GpsModel"})
MERGE (partner_api)-[:CAN_READ]->(gps);
MATCH (partner_read_only:Role {rolename: "partner_read_only"}), (gps:GpsModel:Model {gpsid: "gpsid", name: "GpsModel"})
MERGE (partner_read_only)-[:CAN_READ]->(gps);
MATCH (end_user_admin:Role {rolename: "end_user_admin"}), (gps:GpsModel:Model {gpsid: "gpsid", name: "GpsModel"})
MERGE (end_user_admin)-[:CAN_READ]->(gps);
MATCH (end_user_lighting_user:Role {rolename: "end_user_lighting_user"}), (gps:GpsModel:Model {gpsid: "gpsid", name: "GpsModel"})
MERGE (end_user_lighting_user)-[:CAN_READ]->(gps);
MATCH (end_user_read_only:Role {rolename: "end_user_read_only"}), (gps:GpsModel:Model {gpsid: "gpsid", name: "GpsModel"})
MERGE (end_user_read_only)-[:CAN_READ]->(gps);
MATCH (end_user_sensor_user:Role {rolename: "end_user_sensor_user"}), (gps:GpsModel:Model {gpsid: "gpsid", name: "GpsModel"})
MERGE (end_user_sensor_user)-[:CAN_READ]->(gps);
MATCH (end_user_api:Role {rolename: "end_user_api"}), (gps:GpsModel:Model {gpsid: "gpsid", name: "GpsModel"})
MERGE (end_user_api)-[:CAN_READ]->(gps);
MATCH (installer:Role {rolename: "installer"}), (gps:GpsModel:Model {gpsid: "gpsid", name: "GpsModel"})
MERGE (installer)-[:CAN_READ]->(gps);
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"}), (gps:GpsModel:Model {gpsid: "gpsid", name: "GpsModel"})
MERGE (parking_owner_admin)-[:CAN_READ]->(gps);
MATCH (policy_authority:Role {rolename: "policy_authority"}), (gps:GpsModel:Model {gpsid: "gpsid", name: "GpsModel"})
MERGE (policy_authority)-[:CAN_READ]->(gps);
MATCH (parking_manager:Role {rolename: "parking_manager"}), (gps:GpsModel:Model {gpsid: "gpsid", name: "GpsModel"})
MERGE (parking_manager)-[:CAN_READ]->(gps);
MATCH (end_user_networking_user:Role {rolename: "end_user_networking_user"}), (gps:GpsModel:Model {gpsid: "gpsid", name: "GpsModel"})
MERGE (end_user_networking_user)-[:CAN_READ]->(gps);
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"}), (gps:GpsModel:Model {gpsid: "gpsid", name: "GpsModel"})
MERGE (partner_deployment_user)-[:CAN_READ]->(gps);
MATCH (partner_networking_user:Role {rolename: "partner_networking_user"}), (gps:GpsModel:Model {gpsid: "gpsid", name: "GpsModel"})
MERGE (partner_networking_user)-[:CAN_READ]->(gps);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (ufam:UFAlarmModel:Model {ufamid: "ufamid", name: "UFAlarmModel"})
MERGE (sensity_user)-[:CAN_CREATE]->(ufam);
MATCH (sensity_admin:Role:Admin {rolename: "sensity_admin"}), (ufam:UFAlarmModel:Model {ufamid: "ufamid", name: "UFAlarmModel"})
MERGE (sensity_admin)-[:CAN_CREATE]->(ufam);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (ufam:UFAlarmModel:Model {ufamid: "ufamid", name: "UFAlarmModel"})
MERGE (sensity_user)-[:CAN_UPDATE]->(ufam);
MATCH (sensity_admin:Role:Admin {rolename: "sensity_admin"}), (ufam:UFAlarmModel:Model {ufamid: "ufamid", name: "UFAlarmModel"})
MERGE (sensity_admin)-[:CAN_UPDATE]->(ufam);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (ufam:UFAlarmModel:Model {ufamid: "ufamid", name: "UFAlarmModel"})
MERGE (sensity_user)-[:CAN_DELETE]->(ufam);
MATCH (sensity_admin:Role:Admin {rolename: "sensity_admin"}), (ufam:UFAlarmModel:Model {ufamid: "ufamid", name: "UFAlarmModel"})
MERGE (sensity_admin)-[:CAN_DELETE]->(ufam);
MATCH (sensity_user:Role:Admin {rolename: "sensity_user"}), (ufam:UFAlarmModel:Model {ufamid: "ufamid", name: "UFAlarmModel"})
MERGE (sensity_user)-[:CAN_READ]->(ufam);
MATCH (sensity_admin:Role:Admin {rolename: "sensity_admin"}), (ufam:UFAlarmModel:Model {ufamid: "ufamid", name: "UFAlarmModel"})
MERGE (sensity_admin)-[:CAN_READ]->(ufam);



MATCH (sensity_admin:Role:Admin {rolename: "sensity_admin"})
MATCH (sensity_user:Role {rolename: "sensity_user"})
MATCH (sensity_read_only:Role {rolename: "sensity_read_only"})
MATCH (partner_admin:Role {rolename: "partner_admin"})
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"})
MATCH (partner_lighting_user:Role {rolename: "partner_lighting_user"})
MATCH (partner_sensor_user:Role {rolename: "partner_sensor_user"})
MATCH (partner_networking_user:Role {rolename: "partner_networking_user"})
MATCH (partner_read_only:Role {rolename: "partner_read_only"})
MATCH (partner_api:Role {rolename: "partner_api"})
MATCH (end_user_admin:Role {rolename : "end_user_admin"})
MATCH (end_user_lighting_user:Role {rolename: "end_user_lighting_user"})
MATCH (end_user_sensor_user:Role {rolename: "end_user_sensor_user"})
MATCH (end_user_networking_user:Role {rolename: "end_user_networking_user"})
MATCH (end_user_read_only:Role {rolename: "end_user_read_only"})
MATCH (end_user_api:Role {rolename: "end_user_api"})
MATCH (installer:Role {rolename: "installer"})
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"})
MATCH (parking_manager:Role {rolename: "parking_manager"})
MATCH (policy_authority:Role {rolename: "policy_authority"})

MERGE (parking_owner_admin)-[:CAN_CREATE]->(parking_owner_admin)
MERGE (parking_owner_admin)-[:CAN_CREATE]->(end_user_lighting_user)
MERGE (parking_owner_admin)-[:CAN_CREATE]->(end_user_sensor_user)
MERGE (parking_owner_admin)-[:CAN_CREATE]->(end_user_networking_user)
MERGE (parking_owner_admin)-[:CAN_CREATE]->(end_user_read_only)
MERGE (parking_owner_admin)-[:CAN_CREATE]->(end_user_api)
MERGE (parking_owner_admin)-[:CAN_CREATE]->(installer)

MERGE (parking_owner_admin)-[:CAN_DELETE]->(parking_owner_admin)
MERGE (parking_owner_admin)-[:CAN_DELETE]->(end_user_lighting_user)
MERGE (parking_owner_admin)-[:CAN_DELETE]->(end_user_sensor_user)
MERGE (parking_owner_admin)-[:CAN_DELETE]->(end_user_networking_user)
MERGE (parking_owner_admin)-[:CAN_DELETE]->(end_user_read_only)
MERGE (parking_owner_admin)-[:CAN_DELETE]->(end_user_api)
MERGE (parking_owner_admin)-[:CAN_DELETE]->(installer)

MERGE (parking_owner_admin)-[:CAN_CHANGE]->(parking_owner_admin)
MERGE (parking_owner_admin)-[:CAN_CHANGE]->(end_user_lighting_user)
MERGE (parking_owner_admin)-[:CAN_CHANGE]->(end_user_sensor_user)
MERGE (parking_owner_admin)-[:CAN_CHANGE]->(end_user_networking_user)
MERGE (parking_owner_admin)-[:CAN_CHANGE]->(end_user_read_only)
MERGE (parking_owner_admin)-[:CAN_CHANGE]->(end_user_api)
MERGE (parking_owner_admin)-[:CAN_CHANGE]->(installer)


MERGE (policy_authority)-[:CAN_CREATE]->(policy_authority)
MERGE (policy_authority)-[:CAN_CREATE]->(end_user_lighting_user)
MERGE (policy_authority)-[:CAN_CREATE]->(end_user_sensor_user)
MERGE (policy_authority)-[:CAN_CREATE]->(end_user_networking_user)
MERGE (policy_authority)-[:CAN_CREATE]->(end_user_read_only)
MERGE (policy_authority)-[:CAN_CREATE]->(end_user_api)
MERGE (policy_authority)-[:CAN_CREATE]->(installer)

MERGE (policy_authority)-[:CAN_DELETE]->(policy_authority)
MERGE (policy_authority)-[:CAN_DELETE]->(end_user_lighting_user)
MERGE (policy_authority)-[:CAN_DELETE]->(end_user_sensor_user)
MERGE (policy_authority)-[:CAN_DELETE]->(end_user_networking_user)
MERGE (policy_authority)-[:CAN_DELETE]->(end_user_read_only)
MERGE (policy_authority)-[:CAN_DELETE]->(end_user_api)
MERGE (policy_authority)-[:CAN_DELETE]->(installer)

MERGE (policy_authority)-[:CAN_CHANGE]->(policy_authority)
MERGE (policy_authority)-[:CAN_CHANGE]->(end_user_lighting_user)
MERGE (policy_authority)-[:CAN_CHANGE]->(end_user_sensor_user)
MERGE (policy_authority)-[:CAN_CHANGE]->(end_user_networking_user)
MERGE (policy_authority)-[:CAN_CHANGE]->(end_user_read_only)
MERGE (policy_authority)-[:CAN_CHANGE]->(end_user_api)
MERGE (policy_authority)-[:CAN_CHANGE]->(installer)


MERGE (sensity_admin)-[:CAN_CREATE]->(sensity_admin)
MERGE (sensity_admin)-[:CAN_CREATE]->(sensity_user)
MERGE (sensity_admin)-[:CAN_CREATE]->(sensity_read_only)
MERGE (sensity_admin)-[:CAN_CREATE]->(partner_admin)
MERGE (sensity_admin)-[:CAN_CREATE]->(parking_owner_admin)
MERGE (sensity_admin)-[:CAN_CREATE]->(parking_manager)
MERGE (sensity_admin)-[:CAN_CREATE]->(policy_authority)

MERGE (sensity_admin)-[:CAN_DELETE]->(sensity_admin)
MERGE (sensity_admin)-[:CAN_DELETE]->(sensity_user)
MERGE (sensity_admin)-[:CAN_DELETE]->(sensity_read_only)
MERGE (sensity_admin)-[:CAN_DELETE]->(partner_admin)
MERGE (sensity_admin)-[:CAN_DELETE]->(parking_owner_admin)
MERGE (sensity_admin)-[:CAN_DELETE]->(parking_manager)
MERGE (sensity_admin)-[:CAN_DELETE]->(policy_authority)

MERGE (sensity_admin)-[:CAN_UPDATE]->(sensity_admin)
MERGE (sensity_admin)-[:CAN_UPDATE]->(sensity_user)
MERGE (sensity_admin)-[:CAN_UPDATE]->(sensity_read_only)
MERGE (sensity_admin)-[:CAN_UPDATE]->(partner_admin)
MERGE (sensity_admin)-[:CAN_UPDATE]->(parking_owner_admin)
MERGE (sensity_admin)-[:CAN_UPDATE]->(parking_manager)
MERGE (sensity_admin)-[:CAN_UPDATE]->(policy_authority)

MERGE (sensity_admin)-[:CAN_CHANGE]->(parking_owner_admin)
MERGE (sensity_admin)-[:CAN_CHANGE]->(policy_authority)
MERGE (sensity_admin)-[:CAN_CHANGE]->(parking_manager)


MERGE (sensity_user)-[:CAN_CREATE]->(sensity_user)
MERGE (sensity_user)-[:CAN_CREATE]->(sensity_read_only)
MERGE (sensity_user)-[:CAN_CREATE]->(partner_admin)
MERGE (sensity_user)-[:CAN_CREATE]->(partner_deployment_user)
MERGE (sensity_user)-[:CAN_CREATE]->(partner_lighting_user)
MERGE (sensity_user)-[:CAN_CREATE]->(partner_sensor_user)
MERGE (sensity_user)-[:CAN_CREATE]->(partner_networking_user)
MERGE (sensity_user)-[:CAN_CREATE]->(partner_read_only)
MERGE (sensity_user)-[:CAN_CREATE]->(partner_api)
MERGE (sensity_user)-[:CAN_CREATE]->(end_user_admin)
MERGE (sensity_user)-[:CAN_CREATE]->(end_user_lighting_user)
MERGE (sensity_user)-[:CAN_CREATE]->(end_user_sensor_user)
MERGE (sensity_user)-[:CAN_CREATE]->(end_user_networking_user)
MERGE (sensity_user)-[:CAN_CREATE]->(end_user_read_only)
MERGE (sensity_user)-[:CAN_CREATE]->(end_user_api)
MERGE (sensity_user)-[:CAN_CREATE]->(installer)
MERGE (sensity_user)-[:CAN_CREATE]->(parking_owner_admin)
MERGE (sensity_user)-[:CAN_CREATE]->(parking_manager)
MERGE (sensity_user)-[:CAN_CREATE]->(policy_authority)

MERGE (sensity_user)-[:CAN_DELETE]->(sensity_user)
MERGE (sensity_user)-[:CAN_DELETE]->(sensity_read_only)
MERGE (sensity_user)-[:CAN_DELETE]->(partner_admin)
MERGE (sensity_user)-[:CAN_DELETE]->(partner_deployment_user)
MERGE (sensity_user)-[:CAN_DELETE]->(partner_lighting_user)
MERGE (sensity_user)-[:CAN_DELETE]->(partner_sensor_user)
MERGE (sensity_user)-[:CAN_DELETE]->(partner_networking_user)
MERGE (sensity_user)-[:CAN_DELETE]->(partner_read_only)
MERGE (sensity_user)-[:CAN_DELETE]->(partner_api)
MERGE (sensity_user)-[:CAN_DELETE]->(end_user_admin)
MERGE (sensity_user)-[:CAN_DELETE]->(end_user_lighting_user)
MERGE (sensity_user)-[:CAN_DELETE]->(end_user_sensor_user)
MERGE (sensity_user)-[:CAN_DELETE]->(end_user_networking_user)
MERGE (sensity_user)-[:CAN_DELETE]->(end_user_read_only)
MERGE (sensity_user)-[:CAN_DELETE]->(end_user_api)
MERGE (sensity_user)-[:CAN_DELETE]->(installer)
MERGE (sensity_user)-[:CAN_DELETE]->(parking_owner_admin)
MERGE (sensity_user)-[:CAN_DELETE]->(parking_manager)
MERGE (sensity_user)-[:CAN_DELETE]->(policy_authority)

MERGE (sensity_user)-[:CAN_CHANGE]->(sensity_user)
MERGE (sensity_user)-[:CAN_CHANGE]->(sensity_read_only)
MERGE (sensity_user)-[:CAN_CHANGE]->(partner_admin)
MERGE (sensity_user)-[:CAN_CHANGE]->(partner_deployment_user)
MERGE (sensity_user)-[:CAN_CHANGE]->(partner_lighting_user)
MERGE (sensity_user)-[:CAN_CHANGE]->(partner_sensor_user)
MERGE (sensity_user)-[:CAN_CHANGE]->(partner_networking_user)
MERGE (sensity_user)-[:CAN_CHANGE]->(partner_read_only)
MERGE (sensity_user)-[:CAN_CHANGE]->(partner_api)
MERGE (sensity_user)-[:CAN_CHANGE]->(end_user_admin)
MERGE (sensity_user)-[:CAN_CHANGE]->(end_user_lighting_user)
MERGE (sensity_user)-[:CAN_CHANGE]->(end_user_sensor_user)
MERGE (sensity_user)-[:CAN_CHANGE]->(end_user_networking_user)
MERGE (sensity_user)-[:CAN_CHANGE]->(end_user_read_only)
MERGE (sensity_user)-[:CAN_CHANGE]->(end_user_api)
MERGE (sensity_user)-[:CAN_CHANGE]->(installer)
MERGE (sensity_user)-[:CAN_CHANGE]->(parking_owner_admin)
MERGE (sensity_user)-[:CAN_CHANGE]->(parking_manager)
MERGE (sensity_user)-[:CAN_CHANGE]->(policy_authority)


MERGE (partner_admin)-[:CAN_CREATE]->(sensity_read_only)
MERGE (partner_admin)-[:CAN_CREATE]->(partner_admin)
MERGE (partner_admin)-[:CAN_CREATE]->(partner_deployment_user)
MERGE (partner_admin)-[:CAN_CREATE]->(partner_lighting_user)
MERGE (partner_admin)-[:CAN_CREATE]->(partner_sensor_user)
MERGE (partner_admin)-[:CAN_CREATE]->(partner_networking_user)
MERGE (partner_admin)-[:CAN_CREATE]->(partner_read_only)
MERGE (partner_admin)-[:CAN_CREATE]->(partner_api)
MERGE (partner_admin)-[:CAN_CREATE]->(end_user_admin)
MERGE (partner_admin)-[:CAN_CREATE]->(end_user_lighting_user)
MERGE (partner_admin)-[:CAN_CREATE]->(end_user_sensor_user)
MERGE (partner_admin)-[:CAN_CREATE]->(end_user_networking_user)
MERGE (partner_admin)-[:CAN_CREATE]->(end_user_read_only)
MERGE (partner_admin)-[:CAN_CREATE]->(parking_owner_admin)
MERGE (partner_admin)-[:CAN_CREATE]->(parking_manager)
MERGE (partner_admin)-[:CAN_CREATE]->(policy_authority)

MERGE (partner_admin)-[:CAN_DELETE]->(sensity_read_only)
MERGE (partner_admin)-[:CAN_DELETE]->(partner_admin)
MERGE (partner_admin)-[:CAN_DELETE]->(partner_deployment_user)
MERGE (partner_admin)-[:CAN_DELETE]->(partner_lighting_user)
MERGE (partner_admin)-[:CAN_DELETE]->(partner_sensor_user)
MERGE (partner_admin)-[:CAN_DELETE]->(partner_networking_user)
MERGE (partner_admin)-[:CAN_DELETE]->(partner_read_only)
MERGE (partner_admin)-[:CAN_DELETE]->(partner_api)
MERGE (partner_admin)-[:CAN_DELETE]->(end_user_admin)
MERGE (partner_admin)-[:CAN_DELETE]->(end_user_lighting_user)
MERGE (partner_admin)-[:CAN_DELETE]->(end_user_sensor_user)
MERGE (partner_admin)-[:CAN_DELETE]->(end_user_networking_user)
MERGE (partner_admin)-[:CAN_DELETE]->(end_user_read_only)
MERGE (partner_admin)-[:CAN_DELETE]->(parking_owner_admin)
MERGE (partner_admin)-[:CAN_DELETE]->(parking_manager)
MERGE (partner_admin)-[:CAN_DELETE]->(policy_authority)

MERGE (partner_admin)-[:CAN_CHANGE]->(sensity_read_only)
MERGE (partner_admin)-[:CAN_CHANGE]->(partner_admin)
MERGE (partner_admin)-[:CAN_CHANGE]->(partner_deployment_user)
MERGE (partner_admin)-[:CAN_CHANGE]->(partner_lighting_user)
MERGE (partner_admin)-[:CAN_CHANGE]->(partner_sensor_user)
MERGE (partner_admin)-[:CAN_CHANGE]->(partner_networking_user)
MERGE (partner_admin)-[:CAN_CHANGE]->(partner_read_only)
MERGE (partner_admin)-[:CAN_CHANGE]->(partner_api)
MERGE (partner_admin)-[:CAN_CHANGE]->(end_user_admin)
MERGE (partner_admin)-[:CAN_CHANGE]->(end_user_lighting_user)
MERGE (partner_admin)-[:CAN_CHANGE]->(end_user_sensor_user)
MERGE (partner_admin)-[:CAN_CHANGE]->(end_user_networking_user)
MERGE (partner_admin)-[:CAN_CHANGE]->(end_user_read_only)
MERGE (partner_admin)-[:CAN_CHANGE]->(parking_owner_admin)
MERGE (partner_admin)-[:CAN_CHANGE]->(parking_manager)
MERGE (partner_admin)-[:CAN_CHANGE]->(policy_authority)


MERGE (end_user_admin)-[:CAN_CREATE]->(end_user_admin)
MERGE (end_user_admin)-[:CAN_CREATE]->(end_user_lighting_user)
MERGE (end_user_admin)-[:CAN_CREATE]->(end_user_sensor_user)
MERGE (end_user_admin)-[:CAN_CREATE]->(end_user_networking_user)
MERGE (end_user_admin)-[:CAN_CREATE]->(end_user_read_only)
MERGE (end_user_admin)-[:CAN_CREATE]->(end_user_api)
MERGE (end_user_admin)-[:CAN_CREATE]->(installer)
MERGE (end_user_admin)-[:CAN_CREATE]->(parking_owner_admin)
MERGE (end_user_admin)-[:CAN_CREATE]->(parking_manager)
MERGE (end_user_admin)-[:CAN_CREATE]->(policy_authority)

MERGE (end_user_admin)-[:CAN_DELETE]->(end_user_admin)
MERGE (end_user_admin)-[:CAN_DELETE]->(end_user_lighting_user)
MERGE (end_user_admin)-[:CAN_DELETE]->(end_user_sensor_user)
MERGE (end_user_admin)-[:CAN_DELETE]->(end_user_networking_user)
MERGE (end_user_admin)-[:CAN_DELETE]->(end_user_read_only)
MERGE (end_user_admin)-[:CAN_DELETE]->(end_user_api)
MERGE (end_user_admin)-[:CAN_DELETE]->(installer)
MERGE (end_user_admin)-[:CAN_DELETE]->(parking_owner_admin)
MERGE (end_user_admin)-[:CAN_DELETE]->(parking_manager)
MERGE (end_user_admin)-[:CAN_DELETE]->(policy_authority)

MERGE (end_user_admin)-[:CAN_CHANGE]->(end_user_admin)
MERGE (end_user_admin)-[:CAN_CHANGE]->(end_user_lighting_user)
MERGE (end_user_admin)-[:CAN_CHANGE]->(end_user_sensor_user)
MERGE (end_user_admin)-[:CAN_CHANGE]->(end_user_networking_user)
MERGE (end_user_admin)-[:CAN_CHANGE]->(end_user_read_only)
MERGE (end_user_admin)-[:CAN_CHANGE]->(end_user_api)
MERGE (end_user_admin)-[:CAN_CHANGE]->(installer)
MERGE (end_user_admin)-[:CAN_CHANGE]->(parking_owner_admin)
MERGE (end_user_admin)-[:CAN_CHANGE]->(parking_manager)
MERGE (end_user_admin)-[:CAN_CHANGE]->(policy_authority)


;
