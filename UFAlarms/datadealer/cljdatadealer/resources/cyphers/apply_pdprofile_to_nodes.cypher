MATCH (p:PDProfile {pdprofileid: {props}.pdprofileid}),
      (n:Node)
WHERE n.nodeid IN {props}.nodeids
      OPTIONAL MATCH (:PDProfile)-[rHas:HAS]->(n)-[rBelongs:BELONGS_TO]->(:PDProfile)
DELETE rHas, rBelongs
CREATE (n)-[:BELONGS_TO]->(p),
       (p)-[:HAS]->(n)
SET n:PDController
WITH p, n
RETURN {
       pdprofileid: p.pdprofileid,
       nodes: COLLECT( DISTINCT {nodeid: n.nodeid, name: n.name, model: n.model})
       } AS result
