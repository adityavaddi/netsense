MATCH(p:ETDHProfile {etdhprofileid: {props}.etdhprofileid})-[:ETDH]-(n:Node)
     OPTIONAL MATCH (p)-[:ETDH {trigger: true}]-(trigger:Node)
WHERE n.model <> 'cnext'
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
