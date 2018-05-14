WITH {props} as props
MERGE (o:Org)-[:HAS]->(s:Site {siteid: props.siteid})-[:HAS]->(a:Alert {alertid: props.alertid})
ON MATCH SET
    a.nodeid = props.nodeid,
    a.msg = props.msg,
    a.name = props.name,
    a.type = props.type,
    a.severity = props.severity,
    a.updated = timestamp(),
    a.category = props.category
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
