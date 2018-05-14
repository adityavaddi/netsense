MATCH (p:DHProfile {dhprofileid: {props}.dhprofileid})
SET p.autocalibrate = true
WITH { props } AS props, p
UNWIND props.nodeids AS nodeid
    MATCH (n:Node {nodeid: nodeid})
    OPTIONAL MATCH (other:DHProfile)-[oldr:HAS|BELONGS_TO|DH]-(n) WHERE other <> p
    DELETE oldr
    SET other.autocalibrate = true
    CREATE (n)-[:BELONGS_TO]->(p),
           (p)-[:HAS]->(n),
           (p)-[:DH {trigger: true}]->(n)
    WITH p, n
RETURN {
    dhprofileid: p.dhprofileid,
    nodes: COLLECT( DISTINCT {nodeid: n.nodeid, name: n.name})
} AS nodes
