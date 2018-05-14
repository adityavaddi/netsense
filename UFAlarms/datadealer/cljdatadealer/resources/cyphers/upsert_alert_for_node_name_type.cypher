WITH {props} AS props
MATCH (o:Org)-[:HAS]->(s:Site {siteid: props.siteid})
MERGE (s)-[:HAS]->(a:Alert {nodeid: props.nodeid, name: props.name, type: props.type})-[:BELONGS_TO]->(s)
ON CREATE SET
    a.alertid = props.alertid,
    a.msg = props.msg,
    a.severity = props.severity,
    a.category = props.category,
    a.updated = timestamp(),
    a.created = timestamp()
ON MATCH SET
    a.msg = props.msg,
    a.severity = props.severity,
    a.updated = timestamp()
SET a:Active
REMOVE a:Inactive
RETURN COLLECT({
    alertid: a.alertid,
    nodeid: a.nodeid,
    siteid: s.siteid,
    orgid: o.orgid,
    msg: a.msg,
    name: a.name,
    type: a.type,
    category: a.category,
    severity: a.severity,
    created: a.created,
    updated: a.updated,
    sitename: props.sitename,
    nodename: props.nodename,
    orgname: props.orgname,
    siteaddress: props.siteaddress,
    bssid: props.bssid,
    nodehw: props.nodehw
}) AS alerts