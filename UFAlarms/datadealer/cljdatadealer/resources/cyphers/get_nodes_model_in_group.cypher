MATCH (s:Group {groupid: {props}.groupid})-[:HAS]->(n:Node)
RETURN COLLECT( DISTINCT {
    nodeid: n.nodeid,
    model: n.model
}) AS nodes
