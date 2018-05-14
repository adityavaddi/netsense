MATCH (n:Node {nodeid: {props}.nodeid})
SET n += { props }