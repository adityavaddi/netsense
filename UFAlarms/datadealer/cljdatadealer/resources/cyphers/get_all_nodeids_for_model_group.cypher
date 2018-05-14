MATCH (s:Site {siteid:{props}.siteid})-[:HAS]->(n:Node:Active)-[r:BELONGS_TO]->(g:Group)
  WHERE g.groupid = {props}.groupid AND n.model IN {props}.model
  RETURN n.nodeid AS items
