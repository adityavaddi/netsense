MATCH (n:Node {nodeid: {props}.nodeid})
RETURN {nodeid: n.nodeid,
        lat: n.latitude,
        lon: n.longitude} AS node
