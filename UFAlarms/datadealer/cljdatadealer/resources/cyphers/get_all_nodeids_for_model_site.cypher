MATCH (s:Site {siteid:{props}.siteid})-[:HAS]->(n:Node:Active)
  WHERE n.model IN {props}.model
  RETURN n.nodeid AS items
