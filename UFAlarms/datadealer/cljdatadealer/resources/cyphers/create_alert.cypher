WITH {props} as props
MATCH (o:Org)-[:HAS]->(s:Site {siteid:props.siteid})
CREATE UNIQUE (a:Alert:Active {
    alertid: props.alertid,
    nodeid: props.nodeid,
    msg:props.msg,
    name:props.name,
    type: props.type,
    severity: props.severity,
    created: timestamp(),
    updated: timestamp(),
    category: props.category
})-[:BELONGS_TO]->(s)
CREATE UNIQUE (s)-[:HAS]->(a)
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
