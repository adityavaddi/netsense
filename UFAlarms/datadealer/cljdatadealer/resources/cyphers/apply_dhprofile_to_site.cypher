MATCH (s:Site)-[:HAS]->(g:SiteLightingGroup),
      (s)-[:HAS]->(p:DHProfile)
WHERE s.siteid = {props}.siteid AND
      p.dhprofileid = {props}.dhprofileid
OPTIONAL MATCH (other:DHProfile)-[old:LINKED]->(g) WHERE other <> p
OPTIONAL MATCH (g)-[:HAS]->(n:Node)
DELETE old
MERGE (p)-[:LINKED]->(g)
WITH COLLECT( DISTINCT
 CASE WHEN n.nodeid IS NULL
 THEN NULL
 ELSE {
     nodeid: n.nodeid
 }
 END ) AS nodes, p
RETURN {nodes: nodes, dhprofileid: p.dhprofileid} as result
