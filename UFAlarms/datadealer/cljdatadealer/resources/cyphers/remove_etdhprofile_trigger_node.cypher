MATCH (etdhprofile:ETDHProfile)
WHERE etdhprofile.etdhprofileid = {props}.etdhprofileid
OPTIONAL MATCH (etdhprofile)-[r:ETDH {trigger: true}]-(node:Node)
WHERE node.nodeid in {props}.nodeids
OPTIONAL MATCH (alien:Node)
WHERE alien.nodeid in {props}.nodeids AND
      NOT (etdhprofile)-[:ETDH]-(alien:Node)
REMOVE r.trigger
RETURN alien, node, etdhprofile
