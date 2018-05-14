MATCH (o:Org)-[:HAS]->(s:Site {siteid:{props}.siteid})-[:HAS]->(slg:SiteLightingGroup),
      (n:Node {nodeid:{props}.nodeid})
OPTIONAL MATCH (s1)-[oldHas:HAS]->(n)-[oldBelongs:BELONGS_TO]->(s1)
OPTIONAL MATCH (n1:Node)
WHERE n1.model IN ["unode-v2", "unode-v3", "unode-v4", "unode-v5", "unode-v6"] AND n1.nodeid = n.nodeid
DELETE oldHas, oldBelongs
CREATE UNIQUE (n)-[:BELONGS_TO]->(s)
CREATE UNIQUE (s)-[:HAS]->(n)
MERGE (n1)-[:BELONGS_TO]->(slg)
MERGE (slg)-[:HAS]->(n1)
       REMOVE n:Node:Unassigned
       REMOVE n:Node:Inactive
SET n:Node:Assigned
SET n:Node:Active
SET n.country_code = {props}.country_code
SET n.time_zone = {props}.time_zone
RETURN {
    nodeid: n.nodeid,
    model: n.model,
    siteid: s.siteid,
    sitename: s.name,
    groupid: slg.groupid,
    orgid: o.orgid,
    orgname: o.name
} AS node
