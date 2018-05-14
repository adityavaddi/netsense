MATCH(n:Node {nodeid: {props}.nodeid})
OPTIONAL MATCH (n)-[:BELONGS_TO]->(f:Fixture)
OPTIONAL MATCH (n)-[:BELONGS_TO]->(g:Group)
OPTIONAL MATCH (n)-[:BELONGS_TO]->(sch:Schedule)
OPTIONAL MATCH (n)-[:BELONGS_TO]->(fm:Firmware)
OPTIONAL MATCH (n)-[:BELONGS_TO]->(pd:PDProfile)
OPTIONAL MATCH (n)-[:BELONGS_TO]->(etdh:ETDHProfile)
OPTIONAL MATCH (n)-[:BELONGS_TO]->(dh:DHProfile)
OPTIONAL MATCH (n)-[:HAS]->(c:Config)
RETURN {
    nodeid: n.nodeid,
    name: n.name,
    model: n.model,
    altitude: n.altitude,
    latitude: n.latitude,
    longitude: n.longitude,
    latitude_gps: n.latitude_gps,
    longitude_gps: n.longitude_gps,
    ip: n.ip,
    building: n.building,
    level: n.level,
    meshId: n.meshId,
    note: n.note,
    baseStation: n.baseStation,
    publicKey: n.publicKey,
    signature: n.signature,
    remoteNetwork: n.remoteNetwork,
    bssid: n.bssid,
    configToken: n.configToken,
    softwareVersion: n.softwareVersion,
    firmwareLastUpdated: n.firmwareLastUpdated,
    firmwareUpdateAttempt: n.firmwareUpdateAttempt,
    mfgDate: n.mfgDate,
    circuit: n.circuit,
    fixtureid: f.fixtureid,
    fixturename: f.name,
    fixtureType: f.fixtureType,
    scheduleid: sch.scheduleid,
    schedulename: sch.name,
    firmwareid: fm.firmwareid,
    firmwarename: fm.name,
    apn: n.apn,
    channel: n.channel,
    iccid: n.iccid,
    imei: n.imei,
    imsi: n.imsi,
    auth: n.auth,
    mac: n.mac,
    time_zone: n.time_zone,
    country_code: n.country_code,
    subType: n.subType,
    voltageType: n.voltageType,
    modemRevEd: n.modemRevEd,
    groupnamelist: COLLECT(DISTINCT(g.name)),
    groupidlist: COLLECT(DISTINCT(g.groupid)),
    etdhprofileid: etdh.etdhprofileid,
    etdhprofilename: etdh.name,
    dhprofileid: dh.dhprofileid,
    dhprofilename: dh.name,
    pdprofileid: pd.pdprofileid,
    pdprofilename: pd.name,
    configid: c.configid,
    configname: c.name
} AS node
