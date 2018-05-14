MATCH (p:ETDHProfile {etdhprofileid: {props}.etdhprofileid})
SET p.autocalibrate = true
WITH { props } AS props, p
UNWIND props.nodeids AS nodeid
    MATCH (n:Node {nodeid: nodeid})
    OPTIONAL MATCH (other:ETDHProfile)-[oldr:HAS|BELONGS_TO|ETDH]-(n) WHERE other <> p
    DELETE oldr
    SET other.autocalibrate = true
    CREATE (n)-[:BELONGS_TO]->(p),
           (p)-[:HAS]->(n),
           (p)-[:ETDH {trigger: true}]->(n)
    WITH p, n
RETURN {
    etdhprofileid: p.etdhprofileid,
    nodes: COLLECT( DISTINCT {nodeid: n.nodeid, name: n.name})
} AS nodes
