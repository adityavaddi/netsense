MATCH (n:Node)
      WHERE n.nodeid = {props}.nodeid
OPTIONAL MATCH (n)-[r]-()
      REMOVE n:Node:Active, n:Node:Assigned
      SET n:Node:Inactive, n:Node:Unassigned
      DELETE r
