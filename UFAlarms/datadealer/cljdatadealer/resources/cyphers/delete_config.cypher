MATCH (c:Config {configid: {props}.configid})-[:BELONGS_TO]->(s:Site {siteid: {props}.siteid})
OPTIONAL MATCH (c:Config)-[old:BELONGS_TO|HAS]-(n:Node)
REMOVE c:Active
SET c:Inactive
DELETE old
WITH COLLECT( DISTINCT n.nodeid) AS nodes
RETURN nodes