MATCH (o:Org)-[:HAS]->(s:Site {siteid:{props}.siteid})-[:HAS]->(a:Alert {alertid: {props}.alertid})
WITH a,
     {alertid: a.alertid} AS result
      DETACH DELETE a
RETURN result
