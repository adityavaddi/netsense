MATCH (n:Node)
WHERE n.nodeid = {props}.id
RETURN n.name AS name
