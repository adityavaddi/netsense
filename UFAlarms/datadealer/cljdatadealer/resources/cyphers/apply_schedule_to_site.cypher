MATCH (s:Site)-[:HAS]->(g:SiteLightingGroup),
      (s)-[:HAS]->(sch:Schedule)
WHERE s.siteid = {props}.siteid
      AND sch.scheduleid = {props}.scheduleid
OPTIONAL MATCH (otherSchedule:Schedule)-[old:LINKED]->(g) WHERE otherSchedule <> sch
OPTIONAL MATCH (g)-[:HAS]->(n:Node)
DELETE old
MERGE (sch)-[:LINKED]->(g)
WITH COLLECT( DISTINCT
 CASE WHEN n.nodeid IS NULL
 THEN NULL
 ELSE {
     nodeid: n.nodeid,
     model: n.model
 }
 END ) AS items, s
RETURN items, s.siteid
