MATCH (s:Site)-[:HAS]-(slg:SiteLightingGroup)
WHERE s.siteid = {props}.siteid
OPTIONAL MATCH (slg)<-[:LINKED]-(sch:Schedule)
OPTIONAL MATCH (slg)<-[:LINKED]-(etdhprofile:ETDHProfile)
OPTIONAL MATCH (slg)<-[:LINKED]-(dhprofile:DHProfile)
OPTIONAL MATCH (slg)<-[:LINKED]-(pdprofile:PDProfile)
RETURN {
    siteid: s.siteid,
    groupid: slg.groupid,
    scheduleid: sch.scheduleid,
    etdhprofileid: etdhprofile.etdhprofileid,
    dhprofileid: dhprofile.dhprofileid,
    pdprofileid: pdprofile.pdprofileid
} AS result
