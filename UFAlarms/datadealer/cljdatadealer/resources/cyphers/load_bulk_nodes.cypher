UNWIND {props}.nodes AS row
MATCH (o:Org)-[:HAS]->(s:Site {siteid: row.siteid})-[:HAS]->(slg:SiteLightingGroup)
MERGE (n:Node {nodeid: row.nodeid})
SET n += row
REMOVE n.orgid, n.siteid
MERGE (s)-[:HAS]->(n)-[:BELONGS_TO]->(s)
ON CREATE SET n:Node:Active:Assigned,
              n.groupid = slg.groupid
REMOVE n:Inactive:Unassigned
RETURN COLLECT( DISTINCT {
         nodeid: n.nodeid,
         model: n.model,
         siteid: s.siteid,
         sitename: s.name,
         groupid: slg.groupid,
         orgid: o.orgid,
         orgname: o.name,
         latitude: n.latitude,
         longitude: n.longitude
       }) AS nodes