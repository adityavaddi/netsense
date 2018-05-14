WITH {props} as props
MATCH (o:Org)-[:HAS]->(s:Site {siteid: props.siteid})-[:HAS]->(a:Alert {alertid: props.alertid})
SET
    a.severity = "Clear",
    a.updated = timestamp()
WITH {alertid: a.alertid} AS result
RETURN result
