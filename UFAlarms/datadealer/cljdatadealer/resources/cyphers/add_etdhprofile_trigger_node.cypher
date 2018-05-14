MATCH (etdhprofile:ETDHProfile)
WHERE etdhprofile.etdhprofileid = {props}.etdhprofileid
OPTIONAL MATCH (etdhprofile)-[r:ETDH]-(node:Node)
WHERE node.nodeid IN {props}.nodeids
OPTIONAL MATCH (alien:Node)
WHERE alien.nodeid IN {props}.nodeids AND
      NOT (etdhprofile)-[:ETDH]-(alien:Node)
SET r.trigger = true
RETURN alien, node, etdhprofile
