//CREATE KEYSPACE IF NOT EXISTS farallones with replication = { 'class' : 'SimpleStrategy', 'replication_factor' : 1 };

//PARKING DB MIGRATION
// ///////////////////////////////////////////////////////////////////////////////
// Resource: Tag
// Owner: parking-policy-service
// Description: An instance of a Tag. Used to tag policies
// PrimaryKey: uid
// Indexed on: orgid,siteid
CREATE TABLE IF NOT EXISTS parkingtag(
uid varchar,
name varchar,
createdon bigint,
lastupdated bigint,
description varchar,
orgid varchar,
siteid varchar,
isdeleted boolean,
PRIMARY KEY(uid)
);

CREATE INDEX IF NOT EXISTS tag_orgid_idx ON parkingtag (orgid);
CREATE INDEX IF NOT EXISTS tag_siteid_idx ON parkingtag (siteid);
CREATE INDEX IF NOT EXISTS tag_isdeleted_idx ON parkingtag (isdeleted);
// ///////////////////////////////////////////////////////////////////////////////
// Resource: chargeduration, period, dateperiod, maxduration, parkingcharge, violationfine,
//          parkingpenalty,timerange, parkingschedule, policyrule
// Owner: parking-policy-service, parking-group-policy-service
// Description: UDT's for policy

CREATE TYPE IF NOT EXISTS chargeduration (
name text,
coarseduration double,
sliceduration double,
units text,
price double
);

CREATE TYPE IF NOT EXISTS period (
startdatetime text,
enddatetime text
);

CREATE TYPE IF NOT EXISTS dateperiod (
period frozen<period>,
occurs text
);

CREATE TYPE IF NOT EXISTS maxduration (
duration double,
units text
);

CREATE TYPE IF NOT EXISTS parkingcharge (
name text,
description text,
chargeduration list<frozen<chargeduration>>,
maxduration frozen<maxduration>,
maxcharge double
);

CREATE TYPE IF NOT EXISTS violationfine (
violationtype text,
violationfee double,
recurrence text
);

CREATE TYPE IF NOT EXISTS parkingpenalty (
description text,
violationfine set<frozen<violationfine>>
);

CREATE TYPE IF NOT EXISTS parkingrule (
description text,
parkingallowed boolean,
parkingcharge frozen<parkingcharge>,
parkingpenalty frozen<parkingpenalty>
);

CREATE TYPE IF NOT EXISTS timerange (
starttime text,
endtime text
);

CREATE TYPE IF NOT EXISTS parkingschedule (
description text,
daysofweek set<text>,
months set<text>,
dateperiod frozen<dateperiod>,
timerange set<frozen<timerange>>
);

CREATE TYPE IF NOT EXISTS policyrule (
policyRuleId text,
name text,
description text,
priority int,
parkingschedule frozen<parkingschedule>,
parkingrule frozen<parkingrule>
);

CREATE TYPE IF NOT EXISTS policyLevelViolation (
policyViolationId text,
policyViolationName text,
policyViolationDescription text,
policyViolationType text,
policyViolationFee double,
policyViolationFeeRecurrence text
);

// ///////////////////////////////////////////////////////////////////////////////
// Resource: Policy
// Owner: parking-policy-service, parking-group-policy-service
// Description: An instance of a Policy. Used to build policies in parkingOptimization
// PrimaryKey: Partition key - uid , Clustering key - version
// Indexed on: tags,orgid,siteid,name
CREATE TABLE IF NOT EXISTS parkingpolicy (
uid text,
version int,
tags set<text>,
policyLevelViolations set<frozen<policyLevelViolation>>,
createdon bigint,
hashvalue text,
isdeleted boolean,
islibrarypolicy boolean,
name text,
description text,
policyauthorizerid text,
policyrule set<frozen<policyrule>>,
siteid text,
orgid text,
lastupdated bigint,
timezone text,
state text,
PRIMARY KEY (uid, version)
);

CREATE INDEX IF NOT EXISTS policy_isdeleted_idx ON parkingpolicy (isdeleted);
CREATE INDEX IF NOT EXISTS policy_tag_idx ON parkingpolicy (tags);
CREATE INDEX IF NOT EXISTS policy_orgid_idx ON parkingpolicy (orgid);
CREATE INDEX IF NOT EXISTS policy_siteid_idx ON parkingpolicy (siteid);
CREATE INDEX IF NOT EXISTS policy_name_idx ON parkingpolicy (name);


// ///////////////////////////////////////////////////////////////////////////////
// Resource: Policygrouptimeline
// Owner: parking-policy-service, parking-group-policy-service
// Description: An instance of a policygrouptimeline which tracks timeline of policies linking to parkingGroups
// PrimaryKey: uid

CREATE TABLE IF NOT EXISTS policygrouptimeline (
uid text PRIMARY KEY,
endtime bigint,
parkinggroupid text,
policyid text,
starttime bigint,
version int
);


CREATE INDEX IF NOT EXISTS policygrouptimeline_endtime_idx ON policygrouptimeline (endtime);
CREATE INDEX IF NOT EXISTS policygrouptimeline_parkinggroupid_idx ON policygrouptimeline (parkinggroupid);
CREATE INDEX IF NOT EXISTS policygrouptimeline_policyid_idx ON policygrouptimeline (policyid);

// ///////////////////////////////////////////////////////////////////////////////
// Resource: corner, geocoordinate
// Owner: parking-group-policy-service
// Description: UDT's for space_attributes

CREATE TYPE IF NOT EXISTS geocoordinate (
latitude double,
longitude double
);

CREATE TYPE IF NOT EXISTS space_geometry (
p1 frozen<geocoordinate>,
p2 frozen<geocoordinate>,
p3 frozen<geocoordinate>,
p4 frozen<geocoordinate>
);

// ///////////////////////////////////////////////////////////////////////////////
// Resource: space_attributes
// Owner: parking-group-policy-service
// Description:  An instance of a parkingspaceid. Used to add properties to existing parkingspaces
// PrimaryKey: parkingspaceid
CREATE TABLE IF NOT EXISTS space_attributes (
parkingspaceid text PRIMARY KEY,
active boolean,
areaoftype list<text>,
businessuse text,
createdon bigint,
lastupdated bigint,
parkingspacetype text,
geocoordinates frozen<space_geometry>,
handicap boolean,
howmetered text,
isdeleted boolean,
monitoringsensorid text,
name text,
level text,
paystationid text,
ppv boolean,
reservation boolean,
meterid text,
typeofvehicle list<text>
);

// ///////////////////////////////////////////////////////////////////////////////
// Resource: SSE Subscribe & Filter for streaming
// Owner: Server Sent Event (SSE) Micro service
// Description:  The SSE micro service uses for subscribe/heartbeat/disconnect
//                  for streaming the node event data

CREATE TABLE IF NOT EXISTS sse_filter(
orgid text,
siteid text,
nodeid text,
eventtype text,
topicname text,
counter int,
PRIMARY KEY(orgid,siteid,nodeid,eventtype));

// ///////////////////////////////////////////////////////////////////////////////
// Resource: SSE Subscribe & Filter for businessalert streaming
// Owner: Server Sent Event (SSE) Micro service
// Description:  The SSE micro service uses for subscribe/heartbeat/disconnect
//                  for streaming the businessalert event data

CREATE TABLE IF NOT EXISTS sse_filter_businessalerts(
orgid text,
siteid text,
application text,
triggerid text,
topicname text,
counter int,
PRIMARY KEY(orgid,siteid,application,triggerid));

// ///////////////////////////////////////////////////////////////////////////////
// Resource: RefreshDWH
// Owner: All parking reports and applications
// Description:  A table for associating parkig zones with parking groups
// PrimaryKey: parkingzoneid,parkinggroupid
CREATE TABLE IF NOT EXISTS parking_zone_group (
    parkingzoneid text,
    parkinggroupid text,
    PRIMARY KEY (parkingzoneid, parkinggroupid)
);


// ///////////////////////////////////////////////////////////////////////////////
// Resource: RefreshDWH
// Owner: All parking reports and applications
// Description:  A table for parking group details
// PrimaryKey: parkinggroupid
CREATE TABLE IF NOT EXISTS parking_group (
    parkinggroupid text PRIMARY KEY,
    created text,
    description text,
    name text,
    parkingzones text,
    policy text,
    vehicle_types text
);

// ///////////////////////////////////////////////////////////////////////////////////////////
// Resource: Alerts, User Friendly Alarms and Notifications CRUD operations
// Owner: Alert Micro service
// Description:  The Device_Alerts and Notifications tables are used by Alert Microservice
//                to perform CRUD operations on Alerts and Notifications
//                Alerts_By_NodeId table is used for Alert Ingestion Service
//                uf_alarms table is used for uf_alarms CRUD and alarm type to uf_name mapping

//Old Alerts Table: alerts table is no longer used since 3.0.7.
//Need it for migration jobs. Should be removed in future releases

CREATE TABLE IF NOT EXISTS alerts (
alertid text,
name text,
active boolean,
alarmtype text,
bssid text,
created text,
msg text,
nodehw text,
nodeid text,
nodename text,
orgid text,
orgname text,
severity text,
category text,
siteaddress text,
siteid text,
sitename text,
updated text,
PRIMARY KEY(alertid, orgid)
);

CREATE INDEX IF NOT EXISTS alerts_siteid_idx ON alerts(siteid);
CREATE INDEX IF NOT EXISTS alerts_nodeid_idx ON alerts(nodeid);
CREATE INDEX IF NOT EXISTS alerts_active_idx ON alerts(active);

CREATE TABLE IF NOT EXISTS device_alerts (
alertid text,
name text,
active boolean,
alarmtype text,
bssid text,
created text,
msg text,
nodehw text,
nodeid text,
nodename text,
orgid text,
orgname text,
severity text,
category text,
siteaddress text,
siteid text,
sitename text,
updated text,
PRIMARY KEY((orgid, siteid), alertid)
);

CREATE TABLE IF NOT EXISTS alerts_by_nodeid (
alertid text,
name text,
active boolean,
alarmtype text,
bssid text,
created text,
msg text,
nodehw text,
nodeid text,
nodename text,
orgid text,
orgname text,
severity text,
category text,
siteaddress text,
siteid text,
sitename text,
updated text,
PRIMARY KEY(nodeid, alarmtype, orgid, siteid)
);

CREATE TABLE IF NOT EXISTS notifications (
notificationid text,
orgid text,
active boolean,
siteid text,
name text,
msg text,
description text,
scope text,
window text,
hold_off int,
resend_interval int,
notificationtype SET<text>,
severity SET<text>,
emailuserslist SET<text>,
additionalemails SET<text>,
smsuserslist SET <text>,
created bigint,
updated bigint,
PRIMARY KEY((orgid, siteid), notificationid)
);


CREATE TABLE IF NOT EXISTS uf_alarms (
mappingid text,
alarmtype text PRIMARY KEY,
created bigint,
description text,
displaytocustomer boolean,
displaytopartner boolean,
nodemodels set<text>,
ufname text,
updated bigint
);

CREATE TABLE IF NOT EXISTS uf_alarms_by_id (
mappingid text PRIMARY KEY,
alarmtype text,
created bigint,
description text,
displaytocustomer boolean,
displaytopartner boolean,
nodemodels set<text>,
ufname text,
updated bigint
);


// ///////////////////////////////////////////////////////////////////////////////
// Resource: AppUserData
// Owner: // ToDo which owner it belongs to
// Description:  An instance of a parkingspot. Used to add properties to existing parkingspots
// PrimaryKey: appid

CREATE TABLE IF NOT EXISTS appuserdata (
appid text,
userid text,
userdataid text,
datavalue text,
createdon bigint,
lastupdated bigint,
isdeleted boolean,
PRIMARY KEY(userdataid,appid)
);

CREATE INDEX IF NOT EXISTS appuserdata_userid_idx ON appuserdata(userid);
CREATE INDEX IF NOT EXISTS appuserdata_isdeleted_idx ON appuserdata(isdeleted);

// ///////////////////////////////////////////////////////////////////////////////
// Resource: Parking_Aggregation_Stats
// Owner: parking-spark-aggregation-service
// Description: Parking aggregation spark job status.
// PrimaryKey: Partition key - applicationid , Clustering key - agglevel,rollupbucket
CREATE TABLE IF NOT EXISTS parking_aggregation_stats (
applicationid text,
agglevel text,
rollupbucket text,
endtime text,
starttime text,
PRIMARY KEY (applicationid, agglevel, rollupbucket)
);

CREATE INDEX IF NOT EXISTS aggregation_stats_agglevel_idx ON parking_aggregation_stats (agglevel);
CREATE INDEX IF NOT EXISTS aggregation_stats_rollupbucket_idx ON parking_aggregation_stats (rollupbucket);

CREATE TABLE IF NOT EXISTS activity_logs_user (
userid text,
when timeuuid,
activity text,
message text,
targetid text,
targettype text,
PRIMARY KEY (userid, when)
);

CREATE TABLE IF NOT EXISTS activity_logs_site (
siteid text,
when timeuuid,
activity text,
message text,
targetid text,
targettype text,
userid text,
PRIMARY KEY (siteid, when)
);

CREATE TABLE IF NOT EXISTS activity_logs_login (
targetid text,
targettype text,
activity text,
when timeuuid,
ip text,
message text,
PRIMARY KEY (targetid, targettype, activity, when)
);

CREATE TABLE IF NOT EXISTS ota_logs(
orgid text,
siteid text,
jobid text,
when timeuuid,
description text,
firmwareid text,
model text,
node_count int,
nodeid text,
status text,
success boolean,
target_id text,
target_type text,
PRIMARY KEY (orgid, siteid, jobid, when)
);

CREATE TABLE IF NOT EXISTS ota_ids_logs (
jobid text,
when timeuuid,
model text,
node_count int,
orgid text,
siteid text,
target_id text,
target_type text,
PRIMARY KEY (jobid, when)
);

CREATE TABLE IF NOT EXISTS light (
nodeid text PRIMARY KEY,
driver int,
harvest_trigger boolean,
isscheduled boolean,
policy text,
priority int,
schedule_id text,
schedule_time bigint,
startdt timestamp
);

ALTER TABLE light WITH compression={
'chunk_length_kb': '4',
'sstable_compression': 'LZ4Compressor'};

ALTER TABLE light WITH compaction={
'sstable_size_in_mb': '256',
'unchecked_tombstone_compaction': 'true',
'tombstone_threshold': '0.1',
'tombstone_compaction_interval':'3600',
'class': 'LeveledCompactionStrategy'};

CREATE TABLE IF NOT EXISTS connection_status (
nodeid text,
since timeuuid,
isconnected boolean,
PRIMARY KEY (nodeid, since)
);

CREATE TABLE IF NOT EXISTS node_status (
nodeid text PRIMARY KEY,
lig_stat text,
net_stat int,
orgid text,
sen_stat text,
since timeuuid,
siteid text
);

CREATE INDEX IF NOT EXISTS node_status_site ON node_status (siteid);

ALTER TABLE node_status WITH compression={
'chunk_length_kb': '4',
'sstable_compression': 'LZ4Compressor'};

ALTER TABLE node_status WITH compaction={
'sstable_size_in_mb': '256',
'unchecked_tombstone_compaction': 'true',
'tombstone_threshold': '0.1',
'tombstone_compaction_interval':'3600',
'class': 'LeveledCompactionStrategy'};

CREATE TABLE IF NOT EXISTS orghierarchy_by_nodeid(
nodeid text PRIMARY KEY,
bssid text,
nodehw text,
nodename text,
orgid text,
orgname text,
siteaddress text,
siteid text,
sitename text
);

ALTER TABLE orghierarchy_by_nodeid WITH compression={
'chunk_length_kb': '4',
'sstable_compression': 'LZ4Compressor'};

ALTER TABLE orghierarchy_by_nodeid WITH compaction={
'sstable_size_in_mb': '256',
'unchecked_tombstone_compaction': 'true',
'tombstone_threshold': '0.1',
'tombstone_compaction_interval':'3600',
'class': 'LeveledCompactionStrategy'};

CREATE TABLE IF NOT EXISTS device_alarms (
nodeid text,
alarmtype text,
date timeuuid,
category text,
message text,
severitycode text,
PRIMARY KEY (nodeid, alarmtype, date)
);

// The category field was added in 3.0.4
//ALTER TABLE device_alarms ADD category text;

CREATE TABLE IF NOT EXISTS parking_spot_current (
parkingspotid text PRIMARY KEY,
active boolean,
activesince bigint,
address text,
altitude int,
channel int,
demarcated boolean,
description text,
lat1 float,
lat2 float,
lat3 float,
lat4 float,
lng1 float,
lng2 float,
lng3 float,
lng4 float,
nodeid text,
objectid text,
occupancy boolean,
orgid text,
parkinggroupid text,
parkingzoneid text,
since bigint,
siteid text,
tags text,
co text,
x1 float,
x2 float,
x3 float,
x4 float,
y1 float,
y2 float,
y3 float,
y4 float
);
CREATE INDEX IF NOT EXISTS parking_spot_zone ON parking_spot_current (parkingzoneid);
CREATE INDEX IF NOT EXISTS parking_spot_site ON parking_spot_current (siteid);

CREATE TABLE IF NOT EXISTS parking_spot_historic (
date text,
siteid text,
since bigint,
parkingspotid text,
channel int,
nodeid text,
objectid text,
occupancy boolean,
orgid text,
parkinggroupid text,
parkingzoneid text,
PRIMARY KEY ((date, nodeid), since, parkingspotid)
);


ALTER TABLE parking_spot_current WITH
compression={
'chunk_length_kb': '4',
'sstable_compression': 'LZ4Compressor'}
AND compaction={
'sstable_size_in_mb': '256',
'unchecked_tombstone_compaction': 'true',
'tombstone_threshold': '0.1',
'tombstone_compaction_interval':'3600',
'class': 'LeveledCompactionStrategy'};

ALTER TABLE parking_spot_historic WITH
compression={
'chunk_length_kb': '64',
'sstable_compression': 'LZ4Compressor'}
AND compaction={'class': 'org.apache.cassandra.db.compaction.SizeTieredCompactionStrategy',
'tombstone_compaction_interval': '86400',
'unchecked_tombstone_compaction': 'false',
'tombstone_threshold': '0.2'}
AND default_time_to_live=259200  // TTL on parking_spot_historic is 3 days
AND gc_grace_seconds=250000
AND dclocal_read_repair_chance=0.0;

CREATE TABLE IF NOT EXISTS parking_zone_status (
siteid text,
parkingzoneid text,
config text,
orgid text,
parkinggroupid text,
state text,
type text,
PRIMARY KEY (siteid, parkingzoneid)
);

// The parking_zone_status_zone index was added in 3.0.4
CREATE INDEX IF NOT EXISTS parking_zone_status_zone ON parking_zone_status (parkingzoneid);

CREATE TABLE IF NOT EXISTS traffic_point (
trafficpointid uuid PRIMARY KEY,
altitude double,
created timeuuid,
detection_areas list<text>,
latitude double,
longitude double,
modified timeuuid,
openstreetmap_nodeid text,
orgid text,
siteid text,
tags list<text>
);
CREATE INDEX IF NOT EXISTS traffic_point_longitude ON traffic_point (longitude);
CREATE INDEX IF NOT EXISTS traffic_point_openstreetmap_nodeid ON traffic_point (openstreetmap_nodeid);
CREATE INDEX IF NOT EXISTS traffic_point_site ON traffic_point (siteid);
CREATE INDEX IF NOT EXISTS traffic_point_latitude ON traffic_point (latitude);
CREATE INDEX IF NOT EXISTS traffic_point_tags ON traffic_point (tags);
CREATE INDEX IF NOT EXISTS traffic_point_org ON traffic_point (orgid);

CREATE TABLE IF NOT EXISTS traffic_way (
trafficwayid uuid PRIMARY KEY,
created timeuuid,
detection_areas list<text>,
detection_events list<text>,
lanes_backward int,
lanes_backward_events text,
lanes_forward int,
lanes_forward_events text,
modified timeuuid,
openstreetmap_nodeid text,
orgid text,
points list<uuid>,
siteid text,
tags list<text>
);
CREATE INDEX IF NOT EXISTS traffic_way_tags ON traffic_way (tags);
CREATE INDEX IF NOT EXISTS traffic_way_site ON traffic_way (siteid);
CREATE INDEX IF NOT EXISTS traffic_way_openstreetmap_nodeid ON traffic_way (openstreetmap_nodeid);
CREATE INDEX IF NOT EXISTS traffic_way_org ON traffic_way (orgid);


CREATE TABLE IF NOT EXISTS traffic_detection_area (
trafficdetectionareaid uuid PRIMARY KEY,
created timeuuid,
detection_areas list<text>,
detection_events list<text>,
enabled boolean,
lanes text,
modified timeuuid,
orgid text,
points list<uuid>,
siteid text,
tags list<text>
);
CREATE INDEX IF NOT EXISTS traffic_detection_area_tags ON traffic_detection_area (tags);
CREATE INDEX IF NOT EXISTS traffic_detection_area_org ON traffic_detection_area (orgid);
CREATE INDEX IF NOT EXISTS traffic_detection_area_site ON traffic_detection_area (siteid);

CREATE TABLE IF NOT EXISTS traffic_config (
eventid text PRIMARY KEY,
active boolean,
channel int,
data text,
name text,
nodeid text,
orgid text,
siteid text,
tags list<text>,
time bigint,
type text,
updated timeuuid
);
CREATE INDEX IF NOT EXISTS traffic_config_name ON traffic_config (name);
CREATE INDEX IF NOT EXISTS traffic_config_node ON traffic_config (nodeid);
//CREATE INDEX IF NOT EXISTS traffic_config_tags ON traffic_config (tags);
CREATE INDEX IF NOT EXISTS traffic_config_site ON traffic_config (siteid);
CREATE INDEX IF NOT EXISTS traffic_config_type ON traffic_config (type);
//CREATE INDEX IF NOT EXISTS traffic_config_org ON traffic_config (orgid);

CREATE TABLE IF NOT EXISTS traffic_current_status (
trafficdetectioneventid text PRIMARY KEY,
active boolean,
aggregated_count bigint,
channel int,
count bigint,
data text,
name text,
nodeid text,
orgid text,
siteid text,
tags list<text>,
time bigint,
type text,
updated timeuuid
);
CREATE INDEX IF NOT EXISTS traffic_current_status_name ON traffic_current_status (name);
CREATE INDEX IF NOT EXISTS traffic_current_status_time ON traffic_current_status (time);
//CREATE INDEX IF NOT EXISTS traffic_current_status_tags ON traffic_current_status (tags);
CREATE INDEX IF NOT EXISTS traffic_current_status_node ON traffic_current_status (nodeid);
CREATE INDEX IF NOT EXISTS traffic_current_status_type ON traffic_current_status (type);
CREATE INDEX IF NOT EXISTS traffic_current_status_site ON traffic_current_status (siteid);
//CREATE INDEX IF NOT EXISTS traffic_current_status_org ON traffic_current_status (orgid);

ALTER TABLE traffic_current_status WITH compression={
'chunk_length_kb': '4',
'sstable_compression': 'LZ4Compressor'};

ALTER TABLE traffic_current_status WITH compaction={
'sstable_size_in_mb': '256',
'unchecked_tombstone_compaction': 'true',
'tombstone_threshold': '0.1',
'tombstone_compaction_interval':'3600',
'class': 'LeveledCompactionStrategy'};


CREATE TABLE IF NOT EXISTS traffic_status (
date text,
nodeid text,
time bigint,
active boolean,
channel int,
count bigint,
data text,
name text,
orgid text,
siteid text,
tags list<text>,
trafficdetectioneventid text,
type text,
updated timeuuid,
PRIMARY KEY ((date, nodeid), time)
);

ALTER TABLE traffic_status WITH
compression={
'chunk_length_kb': '64',
'sstable_compression': 'LZ4Compressor'}
AND compaction={
'tombstone_compaction_interval': '86400',
'unchecked_tombstone_compaction': 'false',
'tombstone_threshold': '0.2',
'class': 'org.apache.cassandra.db.compaction.SizeTieredCompactionStrategy'}
AND default_time_to_live=259200     // TTL on traffic_status is 3 days
AND gc_grace_seconds=250000
AND dclocal_read_repair_chance=0.0;


CREATE TABLE IF NOT EXISTS device_sensor_samples (
nodeid text,
sensor text,
time bigint,
date timeuuid,
value double,
PRIMARY KEY (nodeid, sensor, time)
);



CREATE TABLE IF NOT EXISTS aggregation_control (
type text,
key text,
val text,
PRIMARY KEY (type, key)
);


CREATE TABLE IF NOT EXISTS aggregation_parking_status (
siteid text,
starttime bigint,
parkingspotid text,
occupancy boolean,
PRIMARY KEY ((siteid, starttime), parkingspotid)
);


CREATE TABLE IF NOT EXISTS aggregation_filters (
type text,
obj text,
PRIMARY KEY (type, obj)
);

CREATE TABLE IF NOT EXISTS aggregation_connection_status (
nodeid text,
starttime bigint,
isconnected boolean,
PRIMARY KEY (nodeid, starttime)
);


CREATE TABLE IF NOT EXISTS aggregation_connection_node (
nodeid text,
starttime bigint,
conduration bigint,
conpercent double,
disconnects int,
enddt text,
endtime bigint,
startday text,
startdt text,
starthr text,
ts bigint,
PRIMARY KEY (nodeid, starttime)
);

CREATE TABLE IF NOT EXISTS aggregation_parking_spot (
siteid text,
parkingtype text,
starttime bigint,
parkingspotid text,
enddt text,
endtime bigint,
groupid text,
occduration bigint,
occpercent double,
startday text,
startdt text,
starthr text,
ts bigint,
turnovers int,
zoneid text,
PRIMARY KEY ((siteid, parkingtype), starttime, parkingspotid)
);


CREATE TABLE IF NOT EXISTS aggregation_parking_zone (
siteid text,
zoneid text,
starttime bigint,
enddt text,
endtime bigint,
groupid text,
occduration bigint,
occpercent double,
parkingtype text,
startday text,
startdt text,
starthr text,
ts bigint,
turnovers int,
PRIMARY KEY ((siteid, zoneid), starttime)
);

CREATE TABLE IF NOT EXISTS aggregation_parking_group (
siteid text,
groupid text,
parkingtype text,
starttime bigint,
enddt text,
endtime bigint,
occduration bigint,
occpercent double,
startday text,
startdt text,
starthr text,
ts bigint,
turnovers int,
PRIMARY KEY ((siteid, groupid, parkingtype), starttime)
);

CREATE TABLE IF NOT EXISTS aggregation_parking_site (
siteid text,
parkingtype text,
starttime bigint,
enddt text,
endtime bigint,
occduration bigint,
occpercent double,
startday text,
startdt text,
starthr text,
ts bigint,
turnovers int,
PRIMARY KEY ((siteid, parkingtype), starttime)
);

CREATE TABLE IF NOT EXISTS device_energy_settings (
siteid text,
nodeid text,
legacy_power double,
max_led_power double,
PRIMARY KEY (siteid, nodeid)
);


CREATE TABLE IF NOT EXISTS aggregation_traffic_events (
siteid text,
eventid text,
aggregation_type text,
starttime bigint,
endtime bigint,
type text,
objectclass text,
avgdwell double,
avgvelocity double,
enddt text,
eventcnt bigint,
medianvelocity double,
name text,
nodeid text,
p85velocity double,
startday text,
startdt text,
starthr text,
ts bigint,
PRIMARY KEY ((siteid, eventid, aggregation_type), starttime, endtime, type, objectclass)
);
CREATE INDEX IF NOT EXISTS aggregation_traffic_events_node ON aggregation_traffic_events (nodeid);
CREATE INDEX IF NOT EXISTS aggregation_traffic_events_aggregation_type ON aggregation_traffic_events (aggregation_type);
CREATE INDEX IF NOT EXISTS aggregation_traffic_events_objectclass ON aggregation_traffic_events (objectclass);
CREATE INDEX IF NOT EXISTS aggregation_traffic_events_eventid ON aggregation_traffic_events (eventid);
CREATE INDEX IF NOT EXISTS aggregation_traffic_events_site ON aggregation_traffic_events (siteid);

CREATE TABLE IF NOT EXISTS aggregation_traffic_events_site (
siteid text,
aggregation_type text,
starttime bigint,
endtime bigint,
type text,
objectclass text,
avgdwell double,
avgvelocity double,
enddt text,
eventcnt bigint,
medianvelocity double,
p85velocity double,
startday text,
startdt text,
starthr text,
ts bigint,
PRIMARY KEY ((siteid, aggregation_type), starttime, endtime, type, objectclass)
);
CREATE INDEX IF NOT EXISTS aggregation_traffic_events_site_aggregation_type ON aggregation_traffic_events_site (aggregation_type);
CREATE INDEX IF NOT EXISTS aggregation_traffic_events_site_objectclass ON aggregation_traffic_events_site (objectclass);
CREATE INDEX IF NOT EXISTS aggregation_traffic_events_site_site ON aggregation_traffic_events_site (siteid);

CREATE TABLE IF NOT EXISTS aggregation_energy_savings_site (
siteid text,
aggregation_type text,
starttime bigint,
actual_energy_consumption double,
enddt text,
endtime bigint,
led_energy_consumption double,
legacy_energy_consumption double,
savings_legacy_vs_actual double,
savings_legacy_vs_led double,
startday text,
startdt text,
starthr text,
ts bigint,
PRIMARY KEY ((siteid, aggregation_type), starttime)
);

CREATE TABLE IF NOT EXISTS aggregation_energy_savings_node (
siteid text,
nodeid text,
aggregation_type text,
starttime bigint,
actual_energy_consumption double,
enddt text,
endtime bigint,
led_energy_consumption double,
legacy_energy_consumption double,
savings_legacy_vs_actual double,
savings_legacy_vs_led double,
startday text,
startdt text,
starthr text,
ts bigint,
PRIMARY KEY ((siteid, nodeid, aggregation_type), starttime)
);

CREATE TABLE IF NOT EXISTS configs (
configid text PRIMARY KEY,
body text,
cfgname text,
cfgtoken text,
model text,
orgid text,
siteid text
);
CREATE INDEX IF NOT EXISTS configs_orgid_idx ON configs (orgid);
CREATE INDEX IF NOT EXISTS configs_siteid_idx ON configs (siteid);

CREATE TABLE IF NOT EXISTS deviceconfigs (
nodeid text PRIMARY KEY,
cfg text,
cfgtoken text,
configid text
);
CREATE INDEX IF NOT EXISTS deviceconfigs_configid_idx ON deviceconfigs (configid);

// ///////////////////////////////////////////////////////////////////////////////
// Resource: SIC
// Owner: media-service
// Description: Stores SIC request Status for Media-service
// PrimaryKey: Partition key - msgId , Clustering keys - msgSrc, msgType, state
CREATE TABLE IF NOT EXISTS sic_status (
msgId text,
msgSrc text,
msgType text,
state text,
id timeuuid,
ts timestamp,
count bigint,
reason text,
PRIMARY KEY ((msgId, msgSrc, msgType), id)
)  WITH CLUSTERING ORDER BY (id DESC) ;

CREATE TABLE IF NOT EXISTS ota_job_info (
    jobid text,
    when timeuuid,
    count int,
    description text,
    firmwareid text,
    targetid text,
    targettype text,
    PRIMARY KEY (jobid, when)
);

CREATE TABLE IF NOT EXISTS ota_job_relation (
    orgid text,
    siteid text,
    when timeuuid,
    jobid text,
    PRIMARY KEY ((orgid, siteid), when)
);

CREATE TABLE IF NOT EXISTS ota_job_status (
    jobid text,
    when timeuuid,
    status text,
    PRIMARY KEY (jobid, when)
);

CREATE TABLE IF NOT EXISTS ota_node_status (
    jobid text,
    when timeuuid,
    nodeid text,
    progress bigint,
    status text,
    PRIMARY KEY (jobid, when)
);

CREATE TABLE IF NOT EXISTS business_alerts(
businessalertid text,
triggername text,
triggerid text,
triggercategory text,
triggersubcategory text,
triggeruserid text,
siteid text,
orgid text,
resourceid text,
resourcename text,
active boolean,
createdon bigint,
lastupdated bigint,
lastclearedat bigint,
lastclearedby text,
message text,
severity text,
PRIMARY KEY ((orgid, siteid), businessalertid)
);

CREATE TABLE IF NOT EXISTS business_alerts_history(
businessalertid text,
triggername text,
triggerid text,
triggercategory text,
triggersubcategory text,
triggeruserid text,
siteid text,
orgid text,
resourceid text,
resourcename text,
active boolean,
createdon bigint,
lastclearedat bigint,
lastclearedby text,
message text,
severity text,
PRIMARY KEY ((orgid, siteid), createdon)
);

CREATE TABLE IF NOT EXISTS business_triggers(
triggerid text,
triggername text,
userid text,
createdon bigint,
lastupdated bigint,
triggercategory text,
triggersubcategory text,
resourcelistjson text,
siteid text,
orgid text,
timeperiod text,
triggervariable text,
comparisonvariableoperator text,
comparisonvariable text,
comparisonoperator text,
comparisonvalue text,
usermessage text,
additionalusermessage text,
severity text,
isdeleted boolean,
PRIMARY KEY ((orgid, siteid), triggerid)
);

// ///////////////////////////////////////////////////////////////////////////////
// Resource: business_alerts_job_status
// Owner: business-alert-sparkjob
// Description: Used to track the status of business alert spark job (Internal purpose and no business use)

CREATE TABLE IF NOT EXISTS business_alerts_job_status(
applicationid text,
triggercategory text,
triggersubcategory text,
timeperiod text,
starttime text,
endtime text,
status boolean,
message text,
createdon bigint,
PRIMARY KEY ((triggercategory, triggersubcategory, timeperiod), applicationid)
) WITH default_time_to_live = 864000; // TTL is 10days


// Resource: Gps
CREATE TABLE IF NOT EXISTS gps (
nodeid text,
name text,
orgid text,
siteid text,
lat double,
lon double,
latuseradded double,
lonuseradded double,
created bigint,
updated bigint,
PRIMARY KEY((orgid,siteid),nodeid));
// ///////////////////////////////////////////////////////////////////////////////
// Resource: migration_status
// Owner: data-migration (sub-module of dbinit)
// Description: Used to track the status of Data Migration job (Internal purpose and no business use)

CREATE TABLE IF NOT EXISTS migration_status (
script_name text,
version int,
applied_successful boolean,
executed_at text,
PRIMARY KEY(script_name, version));

CREATE TABLE IF NOT EXISTS parking_whatIf_analysis_job (
orgid text,
siteid text,
jobid text,
aborttime bigint,
appid text,
batchid int,
createdat bigint,
createdbyuserid text,
endtime bigint,
fromtime bigint,
jobstatus text,
lastupdatedbyuserid text,
name text,
parkinggroups list<text>,
policieswithversion list<text>,
starttime bigint,
submittime bigint,
totime bigint,
updatedat bigint,
mode text,
additionalmessage text,
email text,
intervalperiod text,
PRIMARY KEY ((orgid, siteid), jobid)
);


CREATE TABLE IF NOT EXISTS parking_whatif_tag(
tagid varchar,
name varchar,
createdon bigint,
lastupdated bigint,
description varchar,
orgid varchar,
siteid varchar,
isdeleted boolean,
PRIMARY KEY((orgid,siteid),tagid)
);

CREATE TABLE IF NOT EXISTS parking_whatif_policy (
policyid text,
version int,
tags set<text>,
policyLevelViolations set<frozen<policyLevelViolation>>,
createdon bigint,
hashvalue text,
isdeleted boolean,
islibrarypolicy boolean,
name text,
description text,
userid text,
policyrule set<frozen<policyrule>>,
siteid text,
orgid text,
lastupdated bigint,
timezone text,
state text,
PRIMARY KEY ((orgid,siteid),policyid, version)
);
