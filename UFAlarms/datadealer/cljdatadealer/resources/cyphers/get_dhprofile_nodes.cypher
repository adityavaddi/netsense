MATCH(p:DHProfile {dhprofileid: {props}.dhprofileid})-[:DH]-(n:Node)
     OPTIONAL MATCH (p)-[:DH {trigger: true}]-(trigger:Node)
WITH COLLECT( DISTINCT
             CASE WHEN n.nodeid IS NULL
             THEN NULL
             ELSE {
     nodeid: n.nodeid
     }
             END ) AS controllers,
     COLLECT( DISTINCT
             CASE WHEN trigger.nodeid IS NULL
             THEN NULL
             ELSE {
     nodeid: trigger.nodeid
     }
             END
             ) AS triggers
RETURN controllers, triggers
