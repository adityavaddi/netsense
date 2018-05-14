MATCH (n:Node:Active)-[r:BELONGS_TO]->(g:Group)
WHERE g.groupid IN {props}.groupids
OPTIONAL MATCH (n)-[:BELONGS_TO]->(fm:Firmware)
RETURN DISTINCT {
    nodeid: n.nodeid,
    latitude: n.latitude,
    longitude: n.longitude,
    firmwareid: fm.firmwareid,
    model: n.model,
    softwareVersion: n.softwareVersion
} AS items
