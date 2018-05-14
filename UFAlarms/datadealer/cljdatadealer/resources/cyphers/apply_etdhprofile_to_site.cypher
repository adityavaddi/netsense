MATCH (s:Site)-[:HAS]->(g:SiteLightingGroup),
      (s)-[:HAS]->(p:ETDHProfile)
WHERE s.siteid = {props}.siteid AND
      p.etdhprofileid = {props}.etdhprofileid
OPTIONAL MATCH (other:ETDHProfile)-[old:LINKED]->(g) WHERE other <> p
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
RETURN {nodes: nodes, etdhprofileid: p.etdhprofileid} as result
