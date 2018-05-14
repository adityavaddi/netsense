MATCH (p:DHProfile)-[r]-(n:Node)
WHERE n.nodeid IN {props}.incoming
DELETE r
SET p.autocalibrate=true
