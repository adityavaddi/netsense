MATCH (p:PDProfile)-[:HAS]->(n:Node:PDController {nodeid: {props}.nodeid})
RETURN p.pdprofileid AS pdprofileid
