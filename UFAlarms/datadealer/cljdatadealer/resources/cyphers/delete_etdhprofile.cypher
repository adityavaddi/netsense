MATCH (:Org {orgid: {props}.orgid})-[:HAS]->(:Site {siteid: {props}.siteid})-[:HAS]->(p:ETDHProfile {etdhprofileid: {props}.etdhprofileid})
WITH p,
     {etdhprofileid: p.etdhprofileid} AS result
DETACH DELETE p
RETURN result
