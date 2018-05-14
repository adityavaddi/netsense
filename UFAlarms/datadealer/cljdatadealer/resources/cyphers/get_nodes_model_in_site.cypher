MATCH (s:Site {siteid: {props}.siteid})-[:HAS]->(n:Node)
RETURN COLLECT( DISTINCT {
    nodeid: n.nodeid,
    model: n.model
}) AS nodes
