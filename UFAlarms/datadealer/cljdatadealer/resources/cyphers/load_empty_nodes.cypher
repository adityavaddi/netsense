UNWIND {props}.nodes AS row
MERGE (n:Node {nodeid: row.nodeid})
ON CREATE SET n:Node:Inactive:Unassigned
SET n += row
REMOVE n.orgid, n.siteid
RETURN COLLECT( DISTINCT {
  nodeid: n.nodeid
}) AS nodes