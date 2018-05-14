MATCH (p:ETDHProfile)-[r]-(n:Node)
WHERE n.nodeid IN {props}.incoming
DELETE r
