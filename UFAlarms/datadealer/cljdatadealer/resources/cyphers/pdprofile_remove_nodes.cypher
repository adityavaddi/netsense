MATCH (p:PDProfile)-[r]-(n:Node)
WHERE n.nodeid IN {props}.incoming
DELETE r
