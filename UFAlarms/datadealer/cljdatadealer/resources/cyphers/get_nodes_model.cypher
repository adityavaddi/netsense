MATCH (n:Node)
WHERE n.nodeid IN {props}.nodeids
RETURN COLLECT( DISTINCT {
    nodeid: n.nodeid,
    model: n.model
}) AS nodes