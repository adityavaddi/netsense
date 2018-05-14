MATCH (p:PDProfile {pdprofileid: {props}.pdprofileid})-[:HAS]->(n:Node:PDController)
WHERE n.model <> 'cnext'
WITH {
     nodeid: n.nodeid,
     lat: n.latitude,
     lon: n.longitude
} AS nodes
RETURN nodes
