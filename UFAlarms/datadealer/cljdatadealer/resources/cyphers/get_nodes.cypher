MATCH (n:Node)
WHERE n.nodeid IN {props}.nodeids
RETURN DISTINCT {
    nodeid: n.nodeid,
    latitude: n.latitude,
    longitude: n.longitude
} AS items
