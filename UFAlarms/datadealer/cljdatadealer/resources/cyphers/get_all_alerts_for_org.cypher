WITH {props} as props
MATCH (o:Org {orgid: props.orgid})-[:HAS]->(s:Site)-[:HAS]->(a:Alert:Active)
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
} AS items