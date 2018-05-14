MATCH (s:Site)-[:HAS]->(g:Group:LightingGroup),
      (s)-[:HAS]->(p:DHProfile)
WHERE s.siteid = {props}.siteid AND
      p.dhprofileid = {props}.dhprofileid AND
      g.groupid IN {props}.groupids
OPTIONAL MATCH (another:DHProfile)-[oldLink:LINKED]->(g) WHERE another <> p
OPTIONAL MATCH (g)-[:HAS]->(n:Node)
DELETE oldLink
MERGE (p)-[:LINKED]->(g)
WITH COLLECT( DISTINCT
 CASE WHEN n.nodeid IS NULL
 THEN NULL
 ELSE {
     nodeid: n.nodeid
 }
 END ) AS nodes, p
RETURN {nodes: nodes, dhprofileid: p.dhprofileid} as result
