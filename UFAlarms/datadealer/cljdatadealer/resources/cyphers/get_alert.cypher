MATCH (o:Org)-[:HAS]->(s:Site {siteid:{props}.siteid})-[:HAS]->(a:Alert {alertid:{props}.alertid})
RETURN {
    alertid: a.alertid,
    nodeid: a.nodeid,
    siteid: s.siteid,
    orgid: o.orgid,
    msg: a.msg,
    name: a.name,
    type: a.type,
    severity: a.severity,
    created: a.created,
    updated: a.updated,
    category: a.category
} AS alert
