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
CREATE CONSTRAINT ON (nfm:NotificationModel) ASSERT nfm.nfmid IS UNIQUE;
CREATE CONSTRAINT ON (n:Notification) ASSERT n.notificationid IS UNIQUE;
CREATE CONSTRAINT ON (pz:ParkingZoneModel) ASSERT pz.parkingzoneid IS UNIQUE;
CREATE CONSTRAINT ON (tom:TrafficObjectModel) ASSERT tom.tomid is UNIQUE;
CREATE CONSTRAINT ON (alm:AlertModel) ASSERT alm.almid IS UNIQUE;
CREATE CONSTRAINT ON (gm:GroupModel) ASSERT gm.gmid IS UNIQUE;
CREATE CONSTRAINT ON (scm:ScheduleModel) ASSERT scm.scmid is UNIQUE;
CREATE CONSTRAINT ON (pm:PartnerModel) ASSERT pm.pmid is UNIQUE;
CREATE CONSTRAINT ON (rm:ReportModel) ASSERT rm.rmid is UNIQUE;
CREATE CONSTRAINT ON (lcm:LicenseModel) ASSERT lcm.lcmid is UNIQUE;
CREATE CONSTRAINT ON (psm:ParkingSpotModel) ASSERT psm.psmid is UNIQUE;
CREATE CONSTRAINT ON (ppm:ParkingPolicyModel) ASSERT ppm.ppmid is UNIQUE;
CREATE CONSTRAINT ON (pcm:PolicyCategoryModel) ASSERT pcm.pcmid is UNIQUE;
CREATE CONSTRAINT ON (pgm:ParkingGroupModel) ASSERT pgm.pgmid is UNIQUE;
CREATE CONSTRAINT ON (bam:BusinessAlertModel) ASSERT bam.bamid IS UNIQUE;
CREATE CONSTRAINT ON (tm:TriggerModel) ASSERT tm.tmid IS UNIQUE;
CREATE CONSTRAINT ON (srm:SummaryReportModel) ASSERT srm.srmid is UNIQUE;
CREATE CONSTRAINT ON (gps:GpsModel) ASSERT gps.gpsid IS UNIQUE;
CREATE CONSTRAINT ON (ufam:UFAlarmModel) ASSERT ufam.ufamid IS UNIQUE;
CREATE CONSTRAINT ON (wam:WhatIfAnalysisModel) ASSERT wam.wamid IS UNIQUE;
CREATE CONSTRAINT ON (wpm:WhatIfPolicyModel) ASSERT wpm.wpmid IS UNIQUE;
CREATE CONSTRAINT ON (wtm:WhatIfTagModel) ASSERT wtm.wtmid IS UNIQUE;

MERGE (om:OrgModel:Model {omid: "omid", name: "OrgModel"})
     ON CREATE SET om.created=timestamp()
MERGE (sm:SiteModel:Model {smid: "smid", name: "SiteModel"})
    ON CREATE SET sm.created=timestamp()
MERGE (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
    ON CREATE SET nm.created=timestamp()
MERGE (cm:ConfigModel:Model {cmid: "cmid", name: "ConfigModel"})
    ON CREATE SET cm.created=timestamp()
MERGE (um:UserModel:Model {umid: "umid", name: "UserModel"})
    ON CREATE SET um.created=timestamp()
MERGE(olm:OverlayModel:Model {olmid: "olmid", name: "OverlayModel"})
    ON CREATE SET olm.created=timestamp()
MERGE(fwm:FirmwareModel:Model {fwmid: "fwmid", name: "FirmwareModel"})
    ON CREATE SET fwm.created=timestamp()
MERGE(am:AuditModel:Model {amid: "amid", name: "AuditModel"})
    ON CREATE SET am.created=timestamp()
MERGE(fm:FixtureModel:Model {fmid: "fmid", name: "FixtureModel"})
    ON CREATE SET fm.created=timestamp()
MERGE(nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
    ON CREATE SET nfm.created=timestamp()
MERGE(pz:ParkingZoneModel:Model {parkingzoneid: "parkingzoneid", name: "ParkingZoneModel"})
    ON CREATE SET pz.created=timestamp()
MERGE(tom:TrafficObjectModel:Model {tomid: "tomid", name: "TrafficObjectModel"})
    ON CREATE SET tom.created=timestamp()
MERGE(alm:AlertModel:Model {almid: "almid", name: "AlertModel"})
    ON CREATE SET alm.created=timestamp()
MERGE(gm:GroupModel:Model {gmid: "gmid", name: "GroupModel"})
    ON CREATE SET gm.created=timestamp()
MERGE(scm:ScheduleModel:Model {scmid: "scmid", name: "ScheduleModel"})
    ON CREATE SET scm.created=timestamp()
MERGE(pm:PartnerModel:Model {pmid: "pmid", name: "PartnerModel"})
    ON CREATE SET pm.created=timestamp()

MERGE (ro_ad:Role {rolename: "customer_admin"})
    ON CREATE SET ro_ad.created=timestamp()
MERGE (ro_pa:Role {rolename: "partner_api"})
    ON CREATE SET ro_pa.created=timestamp()


MERGE (pa_ad:Role {rolename: "parking_owner_admin"})
    ON CREATE SET pa_ad.created=timestamp()

MERGE (o0:Org:Active {orgid: "uberorg", name: "Uber Org", street1: "1455 Market St.", street2: "Suite 200", city: "San Francisco", state: "CA", postal_code: "94107", country: "USA"})
    ON CREATE SET o0.created=timestamp()

MERGE (u1:User:Active {userid: "parkinguser", name: "Parking User", email:"parkinguser@sensity.com", phone: "+1 234 555 666", roles: "parking_owner_admin"})
    ON CREATE SET u1 += { created:timestamp() , updated:timestamp() }

MERGE (s0:Site:Active {siteid: "ubersite", name: "Uber Site", street1: "1455 Market St.", street2: "Suite 200", city: "San Francisco", state: "CA", postal_code: "94107", country: "USA", country_code: "UK", latitude: "37.7756123", longitude: "-122.4192791", time_zone: "America/Los_Angeles"})
    ON CREATE SET s0.created=timestamp()

CREATE UNIQUE (s0)-[:BELONGS_TO]->(o0)
CREATE UNIQUE (o0)-[:HAS]->(s0)

CREATE UNIQUE (u1)-[:IS]->(pa_ad)
CREATE UNIQUE (pa_ad)-[:HAS]->(u1)


CREATE UNIQUE (u1)-[:IS_USER_OF]->(o0)

CREATE UNIQUE (o0)-[:HAS_USER]->(u1)






CREATE UNIQUE (ro_ad)-[:CAN_CREATE]->(olm)
CREATE UNIQUE (ro_ad)-[:CAN_UPDATE]->(olm)
CREATE UNIQUE (ro_ad)-[:CAN_READ]->(olm)
CREATE UNIQUE (ro_ad)-[:CAN_DELETE]->(olm)

CREATE UNIQUE (ro_ad)-[:CAN_CREATE]->(pz)
CREATE UNIQUE (ro_ad)-[:CAN_UPDATE]->(pz)
CREATE UNIQUE (ro_ad)-[:CAN_READ]->(pz)
CREATE UNIQUE (ro_ad)-[:CAN_DELETE]->(pz)

CREATE UNIQUE (ro_ad)-[:CAN_CREATE]->(fwm)
CREATE UNIQUE (ro_ad)-[:CAN_UPDATE]->(fwm)
CREATE UNIQUE (ro_ad)-[:CAN_READ]->(fwm)
CREATE UNIQUE (ro_ad)-[:CAN_DELETE]->(fwm)

CREATE UNIQUE (ro_ad)-[:CAN_CREATE]->(am)
CREATE UNIQUE (ro_ad)-[:CAN_UPDATE]->(am)
CREATE UNIQUE (ro_ad)-[:CAN_READ]->(am)
CREATE UNIQUE (ro_ad)-[:CAN_DELETE]->(am)

CREATE UNIQUE (ro_ad)-[:CAN_CREATE]->(fm)
CREATE UNIQUE (ro_ad)-[:CAN_UPDATE]->(fm)
CREATE UNIQUE (ro_ad)-[:CAN_READ]->(fm)
CREATE UNIQUE (ro_ad)-[:CAN_DELETE]->(fm)

CREATE UNIQUE (ro_ad)-[:CAN_CREATE]->(tom)
CREATE UNIQUE (ro_ad)-[:CAN_UPDATE]->(tom)
CREATE UNIQUE (ro_ad)-[:CAN_READ]->(tom)
CREATE UNIQUE (ro_ad)-[:CAN_DELETE]->(tom)

CREATE UNIQUE (ro_ad)-[:CAN_CREATE]->(nm)
CREATE UNIQUE (ro_ad)-[:CAN_CHANGE]->(nm)
CREATE UNIQUE (ro_ad)-[:CAN_MOVE]->(nm)
CREATE UNIQUE (ro_ad)-[:CAN_READ]->(nm)
CREATE UNIQUE (ro_ad)-[:CAN_MANAGE_LIGHTS]->(nm)
CREATE UNIQUE (ro_ad)-[:CAN_ASSIGN_TO_ORGS]->(nm)
CREATE UNIQUE (ro_ad)-[:CAN_ASSIGN_TO_SITES]->(nm)
CREATE UNIQUE (ro_ad)-[:CAN_ASSIGN_TO_GROUPS]->(nm)
CREATE UNIQUE (ro_ad)-[:CAN_ACTIVATE]->(nm)
CREATE UNIQUE (ro_ad)-[:CAN_DELETE]->(nm)
CREATE UNIQUE (ro_ad)-[:CAN_DEACTIVATE]->(nm)
CREATE UNIQUE (ro_ad)-[:CAN_UPGRADE_FIRMWARE]->(nm)
CREATE UNIQUE (ro_ad)-[:CAN_ASSIGN_FIXTURE]->(nm)

CREATE UNIQUE (ro_ad)-[:CAN_CREATE]->(cm)
CREATE UNIQUE (ro_ad)-[:CAN_UPDATE]->(cm)
CREATE UNIQUE (ro_ad)-[:CAN_READ]->(cm)
CREATE UNIQUE (ro_ad)-[:CAN_DEACTIVATE]->(cm)
CREATE UNIQUE (ro_ad)-[:CAN_APPLY]->(cm)

CREATE UNIQUE (ro_ad)-[:CAN_CREATE]->(sm)
CREATE UNIQUE (ro_ad)-[:CAN_CHANGE]->(sm)
CREATE UNIQUE (ro_ad)-[:CAN_READ]->(sm)
CREATE UNIQUE (ro_ad)-[:CAN_DELETE]->(sm)
CREATE UNIQUE (ro_ad)-[:CAN_SUSPEND]->(sm)
CREATE UNIQUE (ro_ad)-[:CAN_GEN_REPORT]->(sm)
CREATE UNIQUE (ro_ad)-[:CAN_SEE_ALL_ACTIVITY_LOG]->(sm)
CREATE UNIQUE (ro_ad)-[:CAN_SEE_OWN_ACTIVITY_LOG]->(sm)

CREATE UNIQUE (ro_ad)-[:CAN_CREATE]->(om)
CREATE UNIQUE (ro_ad)-[:CAN_READ]->(om)
CREATE UNIQUE (ro_ad)-[:CAN_DELETE]->(om)
CREATE UNIQUE (ro_ad)-[:CAN_SUSPEND]->(om)
CREATE UNIQUE (ro_ad)-[:CAN_CHANGE]->(om)
CREATE UNIQUE (ro_ad)-[:CAN_ADD_SITE]->(om)

CREATE UNIQUE (ro_ad)-[:CAN_CREATE]->(nfm)
CREATE UNIQUE (ro_ad)-[:CAN_READ]->(nfm)
CREATE UNIQUE (ro_ad)-[:CAN_DELETE]->(nfm)
CREATE UNIQUE (ro_ad)-[:CAN_UPDATE]->(nfm)
CREATE UNIQUE (ro_ad)-[:CAN_ADD_SITE]->(nfm)

CREATE UNIQUE (ro_ad)-[:CAN_CREATE]->(alm)
CREATE UNIQUE (ro_ad)-[:CAN_READ]->(alm)
CREATE UNIQUE (ro_ad)-[:CAN_DELETE]->(alm)
CREATE UNIQUE (ro_ad)-[:CAN_UPDATE]->(alm)
CREATE UNIQUE (ro_ad)-[:CAN_ADD_SITE]->(alm)

CREATE UNIQUE (ro_ad)-[:CAN_CREATE]->(scm)
CREATE UNIQUE (ro_ad)-[:CAN_UPDATE]->(scm)
CREATE UNIQUE (ro_ad)-[:CAN_READ]->(scm)
CREATE UNIQUE (ro_ad)-[:CAN_DELETE]->(scm)
CREATE UNIQUE (ro_ad)-[:CAN_APPLY]->(scm)

CREATE UNIQUE (ro_ad)-[:CAN_CREATE]->(gm)
CREATE UNIQUE (ro_ad)-[:CAN_UPDATE]->(gm)
CREATE UNIQUE (ro_ad)-[:CAN_READ]->(gm)
CREATE UNIQUE (ro_ad)-[:CAN_DELETE]->(gm)
CREATE UNIQUE (ro_ad)-[:CAN_REMOVE_NODE_FROM_GROUP]->(gm)

// CREATE UNIQUE (ro_ad)-[:CAN_READ_PARTNER_USER]->(um)
// CREATE UNIQUE (ro_ad)-[:CAN_CREATE_PARTNER_USER]->(um)
// CREATE UNIQUE (ro_ad)-[:CAN_SUSPEND_PARTNER_USER]->(um)
// CREATE UNIQUE (ro_ad)-[:CAN_CHANGE_PARTNER_USER]->(um)
CREATE UNIQUE (ro_ad)-[:CAN_READ_ORG_USER]->(um)
CREATE UNIQUE (ro_ad)-[:CAN_CREATE_ORG_USER]->(um)
CREATE UNIQUE (ro_ad)-[:CAN_SUSPEND_ORG_USER]->(um)
CREATE UNIQUE (ro_ad)-[:CAN_CHANGE_ORG_USER]->(um)
CREATE UNIQUE (ro_ad)-[:CAN_GENERATE_API_KEY]->(um)


CREATE UNIQUE (ro_ad)-[:CAN_CREATE]->(pm)
CREATE UNIQUE (ro_ad)-[:CAN_READ]->(pm)
CREATE UNIQUE (ro_ad)-[:CAN_DELETE]->(pm)
CREATE UNIQUE (ro_ad)-[:CAN_SUSPEND]->(pm)
CREATE UNIQUE (ro_ad)-[:CAN_CHANGE]->(pm)
CREATE UNIQUE (ro_ad)-[:CAN_ADD_SITE]->(pm)



MERGE (ppm:ParkingPolicyModel:Model {ppmid: "ppmid", name: "ParkingPolicyModel"}) ON CREATE SET ppm.created=timestamp()
MERGE (pcm:PolicyCategoryModel:Model {pcmid: "pcmid", name: "PolicyCategoryModel"}) ON CREATE SET pcm.created=timestamp()
MERGE (pgm:ParkingGroupModel:Model {pgmid: "pgmid", name: "ParkingGroupModel"}) ON CREATE SET pgm.created=timestamp()
MERGE (srm:SummaryReportModel:Model {srmid: "srmid", name: "SummaryReportModel"}) ON CREATE SET srm.created=timestamp()
MERGE (bam:BusinessAlertModel:Model {bamid: "bamid", name: "BusinessAlertModel"}) ON CREATE SET bam.created = timestamp()
MERGE (tm:TriggerModel:Model {tmid: "tmid", name: "TriggerModel"}) ON CREATE SET tm.created = timestamp()
MERGE (psm:ParkingSpotModel:Model {psmid: "psmid", name: "ParkingSpotModel"}) ON CREATE SET psm.created=timestamp()
MERGE (gps:GpsModel:Model {gpsid: "gpsid", name: "GpsModel"}) ON CREATE SET gps.created = timestamp()
MERGE (ufam:UFAlarmModel:Model {ufamid: "ufamid", name: "UFAlarmModel"}) ON CREATE SET ufam.created = timestamp()
MERGE (wam:WhatIfAnalysisModel:Model {wamid: "wamid", name: "WhatIfAnalysisModel"}) ON CREATE SET wam.created=timestamp()
MERGE (wpm:WhatIfPolicyModel:Model {wpmid: "wpmid", name: "WhatIfPolicyModel"}) ON CREATE SET wpm.created = timestamp()
MERGE (wtm:WhatIfTagModel:Model {wtmid: "wtmid", name: "WhatIfTagModel"}) ON CREATE SET wtm.created = timestamp()


MERGE (parking_owner_admin:Role {rolename: "parking_owner_admin"}) ON CREATE SET parking_owner_admin.created=timestamp()
MERGE (parking_manager:Role {rolename: "parking_manager"}) ON CREATE SET parking_manager.created=timestamp()
MERGE (policy_authority:Role {rolename: "policy_authority"}) ON CREATE SET policy_authority.created=timestamp()



CREATE  (ro_ad)-[:CAN_READ]->(psm)
CREATE  (ro_ad)-[:CAN_CREATE]->(psm)
CREATE  (ro_ad)-[:CAN_UPDATE]->(psm)
CREATE  (ro_ad)-[:CAN_DELETE]->(psm)
       
CREATE  (ro_ad)-[:CAN_READ]->(ppm)
CREATE  (ro_ad)-[:CAN_CREATE]->(ppm)
CREATE  (ro_ad)-[:CAN_UPDATE]->(ppm)
CREATE  (ro_ad)-[:CAN_DELETE]->(ppm)

CREATE  (ro_ad)-[:CAN_READ]->(pcm)
CREATE  (ro_ad)-[:CAN_CREATE]->(pcm)
CREATE  (ro_ad)-[:CAN_UPDATE]->(pcm)
CREATE  (ro_ad)-[:CAN_DELETE]->(pcm)

CREATE  (ro_ad)-[:CAN_READ]->(bam)
CREATE  (ro_ad)-[:CAN_CREATE]->(bam)
CREATE  (ro_ad)-[:CAN_UPDATE]->(bam)
CREATE  (ro_ad)-[:CAN_DELETE]->(bam)

CREATE  (ro_ad)-[:CAN_READ]->(tm)
CREATE  (ro_ad)-[:CAN_CREATE]->(tm)
CREATE  (ro_ad)-[:CAN_UPDATE]->(tm)
CREATE  (ro_ad)-[:CAN_DELETE]->(tm)


CREATE  (ro_ad)-[:CAN_READ]->(pgm)
CREATE  (ro_ad)-[:CAN_CREATE]->(pgm)
CREATE  (ro_ad)-[:CAN_UPDATE]->(pgm)
CREATE  (ro_ad)-[:CAN_DELETE]->(pgm)


CREATE  (ro_ad)-[:CAN_READ]->(gps)

CREATE  (ro_ad)-[:CAN_READ]->(wam)
CREATE  (ro_ad)-[:CAN_CREATE]->(wam)
CREATE  (ro_ad)-[:CAN_UPDATE]->(wam)
CREATE  (ro_ad)-[:CAN_DELETE]->(wam)

CREATE  (ro_ad)-[:CAN_READ]->(wpm)
CREATE  (ro_ad)-[:CAN_CREATE]->(wpm)
CREATE  (ro_ad)-[:CAN_UPDATE]->(wpm)
CREATE  (ro_ad)-[:CAN_DELETE]->(wpm)

CREATE  (ro_ad)-[:CAN_READ]->(wtm)
CREATE  (ro_ad)-[:CAN_CREATE]->(wtm)
CREATE  (ro_ad)-[:CAN_UPDATE]->(wtm)
CREATE  (ro_ad)-[:CAN_DELETE]->(wtm)

CREATE  (parking_owner_admin)-[:CAN_CREATE]->(end_user_lighting_user)
CREATE  (parking_owner_admin)-[:CAN_CREATE]->(end_user_sensor_user)
CREATE  (parking_owner_admin)-[:CAN_CREATE]->(end_user_networking_user)
CREATE  (parking_owner_admin)-[:CAN_CREATE]->(end_user_read_only)
CREATE  (parking_owner_admin)-[:CAN_CREATE]->(end_user_api)
CREATE  (parking_owner_admin)-[:CAN_CREATE]->(installer)
CREATE  (parking_owner_admin)-[:CAN_DELETE]->(parking_owner_admin)
CREATE  (parking_owner_admin)-[:CAN_DELETE]->(end_user_lighting_user)
CREATE  (parking_owner_admin)-[:CAN_DELETE]->(end_user_sensor_user)
CREATE  (parking_owner_admin)-[:CAN_DELETE]->(end_user_networking_user)
CREATE  (parking_owner_admin)-[:CAN_DELETE]->(end_user_read_only)
CREATE  (parking_owner_admin)-[:CAN_DELETE]->(end_user_api)
CREATE  (parking_owner_admin)-[:CAN_DELETE]->(installer)
CREATE  (parking_owner_admin)-[:CAN_CHANGE]->(parking_owner_admin)
CREATE  (parking_owner_admin)-[:CAN_CHANGE]->(end_user_lighting_user)
CREATE  (parking_owner_admin)-[:CAN_CHANGE]->(end_user_sensor_user)
CREATE  (parking_owner_admin)-[:CAN_CHANGE]->(end_user_networking_user)
CREATE  (parking_owner_admin)-[:CAN_CHANGE]->(end_user_read_only)
CREATE  (parking_owner_admin)-[:CAN_CHANGE]->(end_user_api)
CREATE  (parking_owner_admin)-[:CAN_CHANGE]->(installer)
CREATE  (parking_owner_admin)-[:CAN_CHANGE]->(om)
CREATE  (parking_owner_admin)-[:CAN_DELETE]->(om)
CREATE  (parking_owner_admin)-[:CAN_READ]->(om)
CREATE  (parking_owner_admin)-[:CAN_CREATE]->(sm)
CREATE  (parking_owner_admin)-[:CAN_CHANGE]->(sm)
CREATE  (parking_owner_admin)-[:CAN_SUSPEND]->(sm)
CREATE  (parking_owner_admin)-[:CAN_READ]->(sm)
CREATE  (parking_owner_admin)-[:CAN_CREATE_ORG_USER]->(um)
CREATE  (parking_owner_admin)-[:CAN_CHANGE_ORG_USER]->(um)
CREATE  (parking_owner_admin)-[:CAN_SUSPEND_ORG_USER]->(um)
CREATE  (parking_owner_admin)-[:CAN_READ_ORG_USER]->(um)
CREATE  (parking_owner_admin)-[:CAN_GENERATE_API_KEY]->(um)
CREATE  (parking_owner_admin)-[:CAN_CREATE]->(gm)
CREATE  (parking_owner_admin)-[:CAN_UPDATE]->(gm)
CREATE  (parking_owner_admin)-[:CAN_DELETE]->(gm)
CREATE  (parking_owner_admin)-[:CAN_READ]->(gm)
CREATE  (parking_owner_admin)-[:CAN_CREATE]->(lcm)
CREATE  (parking_owner_admin)-[:CAN_READ]->(lcm)
CREATE  (parking_owner_admin)-[:CAN_READ_USAGE]->(lcm)
CREATE  (parking_owner_admin)-[:CAN_DELETE]->(lcm)
CREATE  (parking_owner_admin)-[:CAN_CREATE]->(scm)
CREATE  (parking_owner_admin)-[:CAN_UPDATE]->(scm)
CREATE  (parking_owner_admin)-[:CAN_DELETE]->(scm)
CREATE  (parking_owner_admin)-[:CAN_READ]->(scm)
CREATE  (parking_owner_admin)-[:CAN_APPLY]->(scm)
CREATE  (parking_owner_admin)-[:CAN_ASSIGN_TO_LS_GROUP]->(nm)
CREATE  (parking_owner_admin)-[:CAN_ASSIGN_LM_DETECTION_TO_GROUP]->(nm)
CREATE  (parking_owner_admin)-[:CAN_MANAGE_LIGHTS]->(nm)
CREATE  (parking_owner_admin)-[:CAN_READ]->(nm)
CREATE  (parking_owner_admin)-[:CAN_CREATE]->(nm)
CREATE  (parking_owner_admin)-[:CAN_CHANGE]->(nm)
CREATE  (parking_owner_admin)-[:CAN_DEACTIVATE]->(nm)
CREATE  (parking_owner_admin)-[:CAN_OPERATE]->(nm)
CREATE  (parking_owner_admin)-[:CAN_ASSIGN_TO_ORGS]->(nm)
CREATE  (parking_owner_admin)-[:CAN_ASSIGN_TO_SITES]->(nm)
CREATE  (parking_owner_admin)-[:CAN_ASSIGN_TO_GROUPS]->(nm)
CREATE  (parking_owner_admin)-[:CAN_UPGRADE_FIRMWARE]->(nm)
CREATE  (parking_owner_admin)-[:CAN_ASSIGN_FIXTURE]->(nm)
CREATE  (parking_owner_admin)-[:CAN_SEARCH]->(nm)
CREATE  (parking_owner_admin)-[:CAN_RETRIEVE_LOG_FILE]->(nm)
CREATE  (parking_owner_admin)-[:CAN_RETRIEVE_ALARM]->(nm)
CREATE  (parking_owner_admin)-[:CAN_CREATE]->(cm)
CREATE  (parking_owner_admin)-[:CAN_UPDATE]->(cm)
CREATE  (parking_owner_admin)-[:CAN_DEACTIVATE]->(cm)
CREATE  (parking_owner_admin)-[:CAN_READ]->(cm)
CREATE  (parking_owner_admin)-[:CAN_APPLY]->(cm)
CREATE  (parking_owner_admin)-[:CAN_READ]->(fm)
CREATE  (parking_owner_admin)-[:CAN_CREATE]->(fm)
CREATE  (parking_owner_admin)-[:CAN_UPDATE]->(fm)
CREATE  (parking_owner_admin)-[:CAN_DELETE]->(fm)
CREATE  (parking_owner_admin)-[:CAN_CREATE]->(am)
CREATE  (parking_owner_admin)-[:CAN_UPDATE]->(am)
CREATE  (parking_owner_admin)-[:CAN_DELETE]->(am)
CREATE  (parking_owner_admin)-[:CAN_READ]->(am)
CREATE  (parking_owner_admin)-[:CAN_CREATE]->(fwm)
CREATE  (parking_owner_admin)-[:CAN_UPDATE]->(fwm)
CREATE  (parking_owner_admin)-[:CAN_DELETE]->(fwm)
CREATE  (parking_owner_admin)-[:CAN_READ]->(fwm)
CREATE  (parking_owner_admin)-[:CAN_CREATE]->(nfm)
CREATE  (parking_owner_admin)-[:CAN_UPDATE]->(nfm)
CREATE  (parking_owner_admin)-[:CAN_DELETE]->(nfm)
CREATE  (parking_owner_admin)-[:CAN_READ]->(nfm)
CREATE  (parking_owner_admin)-[:CAN_CREATE]->(pz)
CREATE  (parking_owner_admin)-[:CAN_UPDATE]->(pz)
CREATE  (parking_owner_admin)-[:CAN_DELETE]->(pz)
CREATE  (parking_owner_admin)-[:CAN_READ]->(pz)
CREATE  (parking_owner_admin)-[:CAN_CREATE]->(tom)
CREATE  (parking_owner_admin)-[:CAN_UPDATE]->(tom)
CREATE  (parking_owner_admin)-[:CAN_DELETE]->(tom)
CREATE  (parking_owner_admin)-[:CAN_READ]->(tom)
CREATE  (parking_owner_admin)-[:CAN_CREATE]->(alm)
CREATE  (parking_owner_admin)-[:CAN_UPDATE]->(alm)
CREATE  (parking_owner_admin)-[:CAN_DELETE]->(alm)
CREATE  (parking_owner_admin)-[:CAN_READ]->(alm)
CREATE  (parking_owner_admin)-[:CAN_CREATE]->(olm)
CREATE  (parking_owner_admin)-[:CAN_UPDATE]->(olm)
CREATE  (parking_owner_admin)-[:CAN_DELETE]->(olm)
CREATE  (parking_owner_admin)-[:CAN_READ]->(olm)
CREATE  (parking_owner_admin)-[:CAN_READ]->(gps)

CREATE UNIQUE (sensity_user)-[:CAN_READ]->(psm)
CREATE UNIQUE (sensity_user)-[:CAN_CREATE]->(psm)
CREATE UNIQUE (sensity_user)-[:CAN_UPDATE]->(psm)
CREATE UNIQUE (sensity_user)-[:CAN_DELETE]->(psm)
CREATE UNIQUE (sensity_user)-[:CAN_READ]->(ppm)
CREATE UNIQUE (sensity_user)-[:CAN_CREATE]->(ppm)
CREATE UNIQUE (sensity_user)-[:CAN_UPDATE]->(ppm)
CREATE UNIQUE (sensity_user)-[:CAN_DELETE]->(ppm)
CREATE UNIQUE (sensity_user)-[:CAN_READ]->(pcm)
CREATE UNIQUE (sensity_user)-[:CAN_CREATE]->(pcm)
CREATE UNIQUE (sensity_user)-[:CAN_UPDATE]->(pcm)
CREATE UNIQUE (sensity_user)-[:CAN_DELETE]->(pcm)
CREATE UNIQUE (sensity_user)-[:CAN_READ]->(pgm)
CREATE UNIQUE (sensity_user)-[:CAN_CREATE]->(pgm)
CREATE UNIQUE (sensity_user)-[:CAN_UPDATE]->(pgm)
CREATE UNIQUE (sensity_user)-[:CAN_DELETE]->(pgm)
CREATE UNIQUE (sensity_user)-[:CAN_READ]->(bam)
CREATE UNIQUE (sensity_user)-[:CAN_CREATE]->(bam)
CREATE UNIQUE (sensity_user)-[:CAN_UPDATE]->(bam)
CREATE UNIQUE (sensity_user)-[:CAN_DELETE]->(bam)
CREATE UNIQUE (sensity_user)-[:CAN_READ]->(wam)
CREATE UNIQUE (sensity_user)-[:CAN_CREATE]->(wam)
CREATE UNIQUE (sensity_user)-[:CAN_UPDATE]->(wam)
CREATE UNIQUE (sensity_user)-[:CAN_DELETE]->(wam)
CREATE UNIQUE (sensity_user)-[:CAN_READ]->(wpm)
CREATE UNIQUE (sensity_user)-[:CAN_CREATE]->(wpm)
CREATE UNIQUE (sensity_user)-[:CAN_UPDATE]->(wpm)
CREATE UNIQUE (sensity_user)-[:CAN_DELETE]->(wpm)
CREATE UNIQUE (sensity_user)-[:CAN_READ]->(wtm)
CREATE UNIQUE (sensity_user)-[:CAN_CREATE]->(wtm)
CREATE UNIQUE (sensity_user)-[:CAN_UPDATE]->(wtm)
CREATE UNIQUE (sensity_user)-[:CAN_DELETE]->(wtm)
CREATE UNIQUE (sensity_user)-[:CAN_READ]->(tm)
CREATE UNIQUE (sensity_user)-[:CAN_CREATE]->(tm)
CREATE UNIQUE (sensity_user)-[:CAN_UPDATE]->(tm)
CREATE UNIQUE (sensity_user)-[:CAN_DELETE]->(tm)
CREATE UNIQUE (sensity_user)-[:CAN_READ]->(srm)
CREATE UNIQUE (sensity_user)-[:CAN_CREATE]->(srm)
CREATE UNIQUE (sensity_user)-[:CAN_UPDATE]->(srm)
CREATE UNIQUE (sensity_user)-[:CAN_DELETE]->(srm)
CREATE UNIQUE (sensity_user)-[:CAN_READ]->(gps)
CREATE UNIQUE (sensity_user)-[:CAN_READ]->(ufam)
CREATE UNIQUE (sensity_user)-[:CAN_CREATE]->(ufam)
CREATE UNIQUE (sensity_user)-[:CAN_UPDATE]->(ufam)
CREATE UNIQUE (sensity_user)-[:CAN_DELETE]->(ufam)
CREATE UNIQUE (sensity_admin)-[:CAN_READ]->(psm)
CREATE UNIQUE (sensity_admin)-[:CAN_CREATE]->(psm)
CREATE UNIQUE (sensity_admin)-[:CAN_UPDATE]->(psm)
CREATE UNIQUE (sensity_admin)-[:CAN_DELETE]->(psm)
CREATE UNIQUE (sensity_admin)-[:CAN_READ]->(ppm)
CREATE UNIQUE (sensity_admin)-[:CAN_CREATE]->(ppm)
CREATE UNIQUE (sensity_admin)-[:CAN_UPDATE]->(ppm)
CREATE UNIQUE (sensity_admin)-[:CAN_DELETE]->(ppm)
CREATE UNIQUE (sensity_admin)-[:CAN_READ]->(pcm)
CREATE UNIQUE (sensity_admin)-[:CAN_CREATE]->(pcm)
CREATE UNIQUE (sensity_admin)-[:CAN_UPDATE]->(pcm)
CREATE UNIQUE (sensity_admin)-[:CAN_DELETE]->(pcm)
CREATE UNIQUE (sensity_admin)-[:CAN_READ]->(pgm)
CREATE UNIQUE (sensity_admin)-[:CAN_CREATE]->(pgm)
CREATE UNIQUE (sensity_admin)-[:CAN_UPDATE]->(pgm)
CREATE UNIQUE (sensity_admin)-[:CAN_DELETE]->(pgm)
CREATE UNIQUE (sensity_admin)-[:CAN_READ]->(bam)
CREATE UNIQUE (sensity_admin)-[:CAN_CREATE]->(bam)
CREATE UNIQUE (sensity_admin)-[:CAN_UPDATE]->(bam)
CREATE UNIQUE (sensity_admin)-[:CAN_DELETE]->(bam)
CREATE UNIQUE (sensity_admin)-[:CAN_READ]->(wam)
CREATE UNIQUE (sensity_admin)-[:CAN_CREATE]->(wam)
CREATE UNIQUE (sensity_admin)-[:CAN_UPDATE]->(wam)
CREATE UNIQUE (sensity_admin)-[:CAN_DELETE]->(wam)
CREATE UNIQUE (sensity_admin)-[:CAN_READ]->(wpm)
CREATE UNIQUE (sensity_admin)-[:CAN_CREATE]->(wpm)
CREATE UNIQUE (sensity_admin)-[:CAN_UPDATE]->(wpm)
CREATE UNIQUE (sensity_admin)-[:CAN_DELETE]->(wpm)
CREATE UNIQUE (sensity_admin)-[:CAN_READ]->(wtm)
CREATE UNIQUE (sensity_admin)-[:CAN_CREATE]->(wtm)
CREATE UNIQUE (sensity_admin)-[:CAN_UPDATE]->(wtm)
CREATE UNIQUE (sensity_admin)-[:CAN_DELETE]->(wtm)
CREATE UNIQUE (sensity_admin)-[:CAN_READ]->(tm)
CREATE UNIQUE (sensity_admin)-[:CAN_CREATE]->(tm)
CREATE UNIQUE (sensity_admin)-[:CAN_UPDATE]->(tm)
CREATE UNIQUE (sensity_admin)-[:CAN_DELETE]->(tm)
CREATE UNIQUE (sensity_admin)-[:CAN_READ]->(srm)
CREATE UNIQUE (sensity_admin)-[:CAN_CREATE]->(srm)
CREATE UNIQUE (sensity_admin)-[:CAN_UPDATE]->(srm)
CREATE UNIQUE (sensity_admin)-[:CAN_DELETE]->(srm)
CREATE UNIQUE (sensity_admin)-[:CAN_READ]->(gps)
CREATE UNIQUE (sensity_admin)-[:CAN_READ]->(ufam)
CREATE UNIQUE (sensity_admin)-[:CAN_CREATE]->(ufam)
CREATE UNIQUE (sensity_admin)-[:CAN_UPDATE]->(ufam)
CREATE UNIQUE (sensity_admin)-[:CAN_DELETE]->(ufam)
CREATE UNIQUE (end_user_admin)-[:CAN_READ]->(psm)
CREATE UNIQUE (end_user_admin)-[:CAN_CREATE]->(psm)
CREATE UNIQUE (end_user_admin)-[:CAN_UPDATE]->(psm)
CREATE UNIQUE (end_user_admin)-[:CAN_DELETE]->(psm)
CREATE UNIQUE (end_user_admin)-[:CAN_READ]->(ppm)
CREATE UNIQUE (end_user_admin)-[:CAN_CREATE]->(ppm)
CREATE UNIQUE (end_user_admin)-[:CAN_UPDATE]->(ppm)
CREATE UNIQUE (end_user_admin)-[:CAN_DELETE]->(ppm)
CREATE UNIQUE (end_user_admin)-[:CAN_READ]->(pcm)
CREATE UNIQUE (end_user_admin)-[:CAN_CREATE]->(pcm)
CREATE UNIQUE (end_user_admin)-[:CAN_UPDATE]->(pcm)
CREATE UNIQUE (end_user_admin)-[:CAN_DELETE]->(pcm)
CREATE UNIQUE (end_user_admin)-[:CAN_READ]->(pgm)
CREATE UNIQUE (end_user_admin)-[:CAN_CREATE]->(pgm)
CREATE UNIQUE (end_user_admin)-[:CAN_UPDATE]->(pgm)
CREATE UNIQUE (end_user_admin)-[:CAN_DELETE]->(pgm)
CREATE UNIQUE (end_user_admin)-[:CAN_READ]->(bam)
CREATE UNIQUE (end_user_admin)-[:CAN_CREATE]->(bam)
CREATE UNIQUE (end_user_admin)-[:CAN_UPDATE]->(bam)
CREATE UNIQUE (end_user_admin)-[:CAN_DELETE]->(bam)
CREATE UNIQUE (end_user_admin)-[:CAN_READ]->(wam)
CREATE UNIQUE (end_user_admin)-[:CAN_CREATE]->(wam)
CREATE UNIQUE (end_user_admin)-[:CAN_UPDATE]->(wam)
CREATE UNIQUE (end_user_admin)-[:CAN_DELETE]->(wam)
CREATE UNIQUE (end_user_admin)-[:CAN_READ]->(wpm)
CREATE UNIQUE (end_user_admin)-[:CAN_CREATE]->(wpm)
CREATE UNIQUE (end_user_admin)-[:CAN_UPDATE]->(wpm)
CREATE UNIQUE (end_user_admin)-[:CAN_DELETE]->(wpm)
CREATE UNIQUE (end_user_admin)-[:CAN_READ]->(wtm)
CREATE UNIQUE (end_user_admin)-[:CAN_CREATE]->(wtm)
CREATE UNIQUE (end_user_admin)-[:CAN_UPDATE]->(wtm)
CREATE UNIQUE (end_user_admin)-[:CAN_DELETE]->(wtm)
CREATE UNIQUE (end_user_admin)-[:CAN_READ]->(tm)
CREATE UNIQUE (end_user_admin)-[:CAN_CREATE]->(tm)
CREATE UNIQUE (end_user_admin)-[:CAN_UPDATE]->(tm)
CREATE UNIQUE (end_user_admin)-[:CAN_DELETE]->(tm)
CREATE UNIQUE (end_user_admin)-[:CAN_READ]->(srm)
CREATE UNIQUE (end_user_admin)-[:CAN_CREATE]->(srm)
CREATE UNIQUE (end_user_admin)-[:CAN_UPDATE]->(srm)
CREATE UNIQUE (end_user_admin)-[:CAN_DELETE]->(srm)
CREATE UNIQUE (end_user_admin)-[:CAN_READ]->(gps)
CREATE UNIQUE (partner_admin)-[:CAN_READ]->(psm)
CREATE UNIQUE (partner_admin)-[:CAN_CREATE]->(psm)
CREATE UNIQUE (partner_admin)-[:CAN_UPDATE]->(psm)
CREATE UNIQUE (partner_admin)-[:CAN_DELETE]->(psm)
CREATE UNIQUE (partner_admin)-[:CAN_READ]->(ppm)
CREATE UNIQUE (partner_admin)-[:CAN_CREATE]->(ppm)
CREATE UNIQUE (partner_admin)-[:CAN_UPDATE]->(ppm)
CREATE UNIQUE (partner_admin)-[:CAN_DELETE]->(ppm)
CREATE UNIQUE (partner_admin)-[:CAN_READ]->(pcm)
CREATE UNIQUE (partner_admin)-[:CAN_CREATE]->(pcm)
CREATE UNIQUE (partner_admin)-[:CAN_UPDATE]->(pcm)
CREATE UNIQUE (partner_admin)-[:CAN_DELETE]->(pcm)
CREATE UNIQUE (partner_admin)-[:CAN_READ]->(pgm)
CREATE UNIQUE (partner_admin)-[:CAN_CREATE]->(pgm)
CREATE UNIQUE (partner_admin)-[:CAN_UPDATE]->(pgm)
CREATE UNIQUE (partner_admin)-[:CAN_DELETE]->(pgm)
CREATE UNIQUE (partner_admin)-[:CAN_READ]->(bam)
CREATE UNIQUE (partner_admin)-[:CAN_CREATE]->(bam)
CREATE UNIQUE (partner_admin)-[:CAN_UPDATE]->(bam)
CREATE UNIQUE (partner_admin)-[:CAN_DELETE]->(bam)
CREATE UNIQUE (partner_admin)-[:CAN_READ]->(wam)
CREATE UNIQUE (partner_admin)-[:CAN_CREATE]->(wam)
CREATE UNIQUE (partner_admin)-[:CAN_UPDATE]->(wam)
CREATE UNIQUE (partner_admin)-[:CAN_DELETE]->(wam)
CREATE UNIQUE (partner_admin)-[:CAN_READ]->(wpm)
CREATE UNIQUE (partner_admin)-[:CAN_CREATE]->(wpm)
CREATE UNIQUE (partner_admin)-[:CAN_UPDATE]->(wpm)
CREATE UNIQUE (partner_admin)-[:CAN_DELETE]->(wpm)
CREATE UNIQUE (partner_admin)-[:CAN_READ]->(wtm)
CREATE UNIQUE (partner_admin)-[:CAN_CREATE]->(wtm)
CREATE UNIQUE (partner_admin)-[:CAN_UPDATE]->(wtm)
CREATE UNIQUE (partner_admin)-[:CAN_DELETE]->(wtm)
CREATE UNIQUE (partner_admin)-[:CAN_READ]->(tm)
CREATE UNIQUE (partner_admin)-[:CAN_CREATE]->(tm)
CREATE UNIQUE (partner_admin)-[:CAN_UPDATE]->(tm)
CREATE UNIQUE (partner_admin)-[:CAN_DELETE]->(tm)
CREATE UNIQUE (partner_admin)-[:CAN_READ]->(srm)
CREATE UNIQUE (partner_admin)-[:CAN_CREATE]->(srm)
CREATE UNIQUE (partner_admin)-[:CAN_UPDATE]->(srm)
CREATE UNIQUE (partner_admin)-[:CAN_READ]->(gps)

        
CREATE  (parking_owner_admin)-[:CAN_READ]->(psm)
CREATE  (parking_owner_admin)-[:CAN_CREATE]->(psm)
CREATE  (parking_owner_admin)-[:CAN_UPDATE]->(psm)
CREATE  (parking_owner_admin)-[:CAN_DELETE]->(psm)
        
CREATE  (parking_owner_admin)-[:CAN_READ]->(ppm)
CREATE  (parking_owner_admin)-[:CAN_CREATE]->(ppm)
CREATE  (parking_owner_admin)-[:CAN_UPDATE]->(ppm)
CREATE  (parking_owner_admin)-[:CAN_DELETE]->(ppm)

CREATE  (parking_owner_admin)-[:CAN_READ]->(pcm)
CREATE  (parking_owner_admin)-[:CAN_CREATE]->(pcm)
CREATE  (parking_owner_admin)-[:CAN_UPDATE]->(pcm)
CREATE  (parking_owner_admin)-[:CAN_DELETE]->(pcm)

CREATE  (parking_owner_admin)-[:CAN_READ]->(pgm)
CREATE  (parking_owner_admin)-[:CAN_CREATE]->(pgm)
CREATE  (parking_owner_admin)-[:CAN_UPDATE]->(pgm)
CREATE  (parking_owner_admin)-[:CAN_DELETE]->(pgm)

CREATE  (parking_owner_admin)-[:CAN_READ]->(bam)
CREATE  (parking_owner_admin)-[:CAN_CREATE]->(bam)
CREATE  (parking_owner_admin)-[:CAN_UPDATE]->(bam)
CREATE  (parking_owner_admin)-[:CAN_DELETE]->(bam)

CREATE  (parking_owner_admin)-[:CAN_READ]->(wam)
CREATE  (parking_owner_admin)-[:CAN_CREATE]->(wam)
CREATE  (parking_owner_admin)-[:CAN_UPDATE]->(wam)
CREATE  (parking_owner_admin)-[:CAN_DELETE]->(wam)

CREATE  (parking_owner_admin)-[:CAN_READ]->(wpm)
CREATE  (parking_owner_admin)-[:CAN_CREATE]->(wpm)
CREATE  (parking_owner_admin)-[:CAN_UPDATE]->(wpm)
CREATE  (parking_owner_admin)-[:CAN_DELETE]->(wpm)

CREATE  (parking_owner_admin)-[:CAN_READ]->(wtm)
CREATE  (parking_owner_admin)-[:CAN_CREATE]->(wtm)
CREATE  (parking_owner_admin)-[:CAN_UPDATE]->(wtm)
CREATE  (parking_owner_admin)-[:CAN_DELETE]->(wtm)

CREATE  (parking_owner_admin)-[:CAN_READ]->(tm)
CREATE  (parking_owner_admin)-[:CAN_CREATE]->(tm)
CREATE  (parking_owner_admin)-[:CAN_UPDATE]->(tm)
CREATE  (parking_owner_admin)-[:CAN_DELETE]->(tm)

CREATE  (parking_owner_admin)-[:CAN_READ]->(srm)
CREATE  (parking_owner_admin)-[:CAN_CREATE]->(srm)
CREATE  (parking_owner_admin)-[:CAN_UPDATE]->(srm)
CREATE  (parking_owner_admin)-[:CAN_DELETE]->(srm)

CREATE (parking_owner_admin)-[:CAN_READ]->(gps)

CREATE  (parking_manager)-[:CAN_CREATE]->(sm)
CREATE  (parking_manager)-[:CAN_CHANGE]->(sm)
CREATE  (parking_manager)-[:CAN_SUSPEND]->(sm)
CREATE  (parking_manager)-[:CAN_READ]->(sm)
CREATE  (parking_manager)-[:CAN_READ_ORG_USER]->(um)
CREATE  (parking_manager)-[:CAN_CREATE]->(gm)
CREATE  (parking_manager)-[:CAN_UPDATE]->(gm)
CREATE  (parking_manager)-[:CAN_DELETE]->(gm)
CREATE  (parking_manager)-[:CAN_READ]->(gm)
CREATE  (parking_manager)-[:CAN_READ]->(scm)
CREATE  (parking_manager)-[:CAN_READ]->(nm)
CREATE  (parking_manager)-[:CAN_CHANGE]->(nm)
CREATE  (parking_manager)-[:CAN_READ]->(cm)

CREATE  (parking_manager)-[:CAN_READ]->(psm)

CREATE  (parking_manager)-[:CAN_READ]->(ppm)

CREATE  (parking_manager)-[:CAN_READ]->(pcm)

CREATE  (parking_manager)-[:CAN_READ]->(pgm)

CREATE  (parking_manager)-[:CAN_CREATE]->(bam)
CREATE  (parking_manager)-[:CAN_UPDATE]->(bam)
CREATE  (parking_manager)-[:CAN_DELETE]->(bam)
CREATE  (parking_manager)-[:CAN_READ]->(bam)

CREATE  (parking_manager)-[:CAN_CREATE]->(wam)
CREATE  (parking_manager)-[:CAN_UPDATE]->(wam)
CREATE  (parking_manager)-[:CAN_DELETE]->(wam)
CREATE  (parking_manager)-[:CAN_READ]->(wam)

CREATE  (parking_manager)-[:CAN_CREATE]->(wpm)
CREATE  (parking_manager)-[:CAN_UPDATE]->(wpm)
CREATE  (parking_manager)-[:CAN_DELETE]->(wpm)
CREATE  (parking_manager)-[:CAN_READ]->(wpm)

CREATE  (parking_manager)-[:CAN_CREATE]->(wtm)
CREATE  (parking_manager)-[:CAN_UPDATE]->(wtm)
CREATE  (parking_manager)-[:CAN_DELETE]->(wtm)
CREATE  (parking_manager)-[:CAN_READ]->(wtm)

CREATE  (parking_manager)-[:CAN_CREATE]->(tm)
CREATE  (parking_manager)-[:CAN_UPDATE]->(tm)
CREATE  (parking_manager)-[:CAN_DELETE]->(tm)
CREATE  (parking_manager)-[:CAN_READ]->(tm)

CREATE  (parking_manager)-[:CAN_READ]->(srm)

CREATE (parking_manager)-[:CAN_READ]->(gps)

CREATE  (policy_authority)-[:CAN_CREATE]->(sm)
CREATE  (policy_authority)-[:CAN_CHANGE]->(sm)
CREATE  (policy_authority)-[:CAN_SUSPEND]->(sm)
CREATE  (policy_authority)-[:CAN_READ]->(sm)
CREATE  (policy_authority)-[:CAN_READ_ORG_USER]->(um)
CREATE  (policy_authority)-[:CAN_CREATE]->(gm)
CREATE  (policy_authority)-[:CAN_UPDATE]->(gm)
CREATE  (policy_authority)-[:CAN_DELETE]->(gm)
CREATE  (policy_authority)-[:CAN_READ]->(gm)
CREATE  (policy_authority)-[:CAN_READ]->(scm)
CREATE  (policy_authority)-[:CAN_READ]->(nm)
CREATE  (policy_authority)-[:CAN_CHANGE]->(nm)
CREATE  (policy_authority)-[:CAN_READ]->(cm)

CREATE  (policy_authority)-[:CAN_READ]->(psm)
CREATE  (policy_authority)-[:CAN_CREATE]->(psm)
CREATE  (policy_authority)-[:CAN_UPDATE]->(psm)
CREATE  (policy_authority)-[:CAN_DELETE]->(psm)


CREATE  (policy_authority)-[:CAN_READ]->(ppm)
CREATE  (policy_authority)-[:CAN_CREATE]->(ppm)
CREATE  (policy_authority)-[:CAN_UPDATE]->(ppm)
CREATE  (policy_authority)-[:CAN_DELETE]->(ppm)

CREATE  (policy_authority)-[:CAN_READ]->(pcm)
CREATE  (policy_authority)-[:CAN_CREATE]->(pcm)
CREATE  (policy_authority)-[:CAN_UPDATE]->(pcm)
CREATE  (policy_authority)-[:CAN_DELETE]->(pcm)

CREATE  (policy_authority)-[:CAN_READ]->(pgm)
CREATE  (policy_authority)-[:CAN_CREATE]->(pgm)
CREATE  (policy_authority)-[:CAN_UPDATE]->(pgm)
CREATE  (policy_authority)-[:CAN_DELETE]->(pgm)

CREATE  (policy_authority)-[:CAN_READ]->(bam)
CREATE  (policy_authority)-[:CAN_CREATE]->(bam)
CREATE  (policy_authority)-[:CAN_UPDATE]->(bam)
CREATE  (policy_authority)-[:CAN_DELETE]->(bam)

CREATE  (policy_authority)-[:CAN_READ]->(wam)
CREATE  (policy_authority)-[:CAN_CREATE]->(wam)
CREATE  (policy_authority)-[:CAN_UPDATE]->(wam)
CREATE  (policy_authority)-[:CAN_DELETE]->(wam)

CREATE  (policy_authority)-[:CAN_READ]->(wpm)
CREATE  (policy_authority)-[:CAN_CREATE]->(wpm)
CREATE  (policy_authority)-[:CAN_UPDATE]->(wpm)
CREATE  (policy_authority)-[:CAN_DELETE]->(wpm)

CREATE  (policy_authority)-[:CAN_READ]->(wtm)
CREATE  (policy_authority)-[:CAN_CREATE]->(wtm)
CREATE  (policy_authority)-[:CAN_UPDATE]->(wtm)
CREATE  (policy_authority)-[:CAN_DELETE]->(wtm)

CREATE  (policy_authority)-[:CAN_READ]->(tm)
CREATE  (policy_authority)-[:CAN_CREATE]->(tm)
CREATE  (policy_authority)-[:CAN_UPDATE]->(tm)
CREATE  (policy_authority)-[:CAN_DELETE]->(tm)

CREATE  (policy_authority)-[:CAN_READ]->(srm)
CREATE  (policy_authority)-[:CAN_CREATE]->(srm)
CREATE  (policy_authority)-[:CAN_UPDATE]->(srm)
CREATE  (policy_authority)-[:CAN_DELETE]->(srm)

CREATE (policy_authority)-[:CAN_READ]->(gps)



;

// Add Uber Test
MATCH (ro_ad) WHERE ro_ad.rolename="customer_admin"
MERGE (o0:Org:Active {orgid: "uberorg", name: "Uber Org", street1: "1455 Market St.", street2: "Suite 200", city: "San Francisco", state: "CA", postal_code: "94107", country: "USA"})
    ON CREATE SET o0.created=timestamp()
MERGE (u0:User:Active {userid: "uberuser", name: "Uber User", email:"uberuser@sensity.com", phone: "+1 234 555 666", roles: "sensity_user"})
    ON CREATE SET u0 += { created:timestamp() , updated:timestamp() }
MERGE (s0:Site:Active {siteid: "ubersite", name: "Uber Site", street1: "1455 Market St.", street2: "Suite 200", city: "San Francisco", state: "CA", postal_code: "94107", country: "USA", country_code: "UK", latitude: "37.7756123", longitude: "-122.4192791", time_zone: "America/Los_Angeles"})
    ON CREATE SET s0.created=timestamp()
MERGE (slg0:Group:LightingGroup:SiteLightingGroup {groupid: "ubersitelightinggroup",
                                                   name: "Site Lighting Group",
                                                   nodeList: []})
CREATE UNIQUE (s0)-[:BELONGS_TO]->(o0)
CREATE UNIQUE (o0)-[:HAS]->(s0)
CREATE UNIQUE (u0)-[:IS]->(ro_ad)
CREATE UNIQUE (ro_ad)-[:HAS]->(u0)
CREATE UNIQUE (u0)-[:IS_USER_OF]->(o0)
CREATE UNIQUE (o0)-[:HAS_USER]->(u0)
CREATE UNIQUE (s0)-[:HAS]->(slg0)
CREATE UNIQUE (slg0)-[:BELONGS_TO]->(s0)

CREATE (sch:Schedule {scheduleid: "default", name: "Default Schedule"})
CREATE (ce:CalendarEvents {days: ["mon","tue","wed","thu","fri","sat","sun"], photocell_enabled: true, photocell_highLevel: 100, photocell_lowLevel: 0})
CREATE UNIQUE (ce)-[:BELONGS_TO]->(sch)
CREATE UNIQUE (sch)-[:HAS]->(ce)
CREATE (a:Action {id: 2, time: "00:00:00", level: 100})
CREATE UNIQUE (ce)-[:HAS]->(a)
CREATE UNIQUE (a)-[:BELONGS_TO]->(ce)
CREATE (nnw:NoNetwork {photocell_enabled: true, photocell_highLevel: 100, photocell_lowLevel: 0})
CREATE UNIQUE (sch)-[:HAS]->(nnw)-[:BELONGS_TO]->(sch)

CREATE (c:Config {
    configid: "default"
})

// Add Schedule for ubersite
CREATE (sch0:Schedule {scheduleid: "uberschedule", name: "Default Schedule"})
CREATE (ce0:CalendarEvents {days: ["mon","tue","wed","thu","fri","sat","sun"], photocell_enabled: true, photocell_highLevel: 100, photocell_lowLevel: 0})
CREATE UNIQUE (ce0)-[:BELONGS_TO]->(sch0)
CREATE UNIQUE (sch0)-[:HAS]->(ce0)
CREATE (a0:Action {id: 2, time: "00:00:00", level: 100})
CREATE UNIQUE (ce0)-[:HAS]->(a0)
CREATE UNIQUE (a0)-[:BELONGS_TO]->(ce0)
CREATE (nnw0:NoNetwork {photocell_enabled: true, photocell_highLevel: 100, photocell_lowLevel: 0})
CREATE UNIQUE (sch0)-[:HAS]->(nnw0)-[:BELONGS_TO]->(sch0)

CREATE UNIQUE (sch0)-[:BELONGS_TO]->(s0)
CREATE UNIQUE (s0)-[:HAS]->(sch0)
CREATE UNIQUE (slg0)<-[:LINKED]-(sch0);
