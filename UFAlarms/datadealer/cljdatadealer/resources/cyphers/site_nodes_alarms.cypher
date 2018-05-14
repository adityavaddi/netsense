MATCH (n:Node)-[:BELONGS_TO]->(s:Site {siteid:{props}.siteid})
        OPTIONAL MATCH (s)-[:HAS]->(a:Alert:Active {nodeid:n.nodeid})
WITH n, {nodeid: n.nodeid, alerts: COLLECT (
        CASE WHEN n.nodeid <> a.nodeid
        THEN NULL
        WHEN a IS NULL
        THEN NULL
        ELSE {
                alertid: a.alertid,
                nodeid: a.nodeid,
                siteid: s.siteid,
                //orgid: o.orgid,
                msg: a.msg,
                name: a.name,
                type: a.type,
                severity: a.severity,
                created: a.created,
                updated: a.updated,
                category: a.category
             }
        END
    )} as node_with_alerts
return COLLECT(node_with_alerts) as items