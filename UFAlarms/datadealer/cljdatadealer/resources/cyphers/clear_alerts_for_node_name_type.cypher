WITH {props} as props
MATCH (o:Org)-[:HAS]->(s:Site {siteid: props.siteid})
MATCH (a:Alert {nodeid: props.nodeid, name: props.name, type: props.type})
SET a:Inactive
REMOVE a:Active
RETURN COLLECT({
    nodeid: a.nodeid,
    name: a.name,
    type: a.type,
    alertid: a.alertid,
    siteid: s.siteid,
    orgid: o.orgid,
    msg: a.msg,
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
