MATCH (n:Node {nodeid: {props}.nodeid})
RETURN n.model as model
