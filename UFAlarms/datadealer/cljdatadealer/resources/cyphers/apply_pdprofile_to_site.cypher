MATCH (s:Site)-[:HAS]->(g:SiteLightingGroup),
      (s)-[:HAS]->(p:PDProfile)
WHERE s.siteid = {props}.siteid AND
      p.pdprofileid = {props}.pdprofileid
      OPTIONAL MATCH (another:PDProfile)-[old:LINKED]->(g) WHERE another <> p
      OPTIONAL MATCH (g)-[:HAS]->(n:Node)
DELETE old
       MERGE (p)-[:LINKED]->(g)
WITH COLLECT( DISTINCT
             CASE WHEN n.nodeid IS NULL
             THEN NULL
             ELSE {
     nodeid: n.nodeid, model: n.model
     }
             END ) AS nodes,
     COLLECT( DISTINCT
             CASE WHEN another.pdprofileid IS NULL
             THEN NULL
             ELSE {
     pdprofileid: another.pdprofileid
     }
             END ) AS pdprofiles
RETURN {nodes: nodes,
        pdprofiles: pdprofiles}
       AS result
