MATCH (:Org {orgid: {props}.orgid})-[:HAS]->(:Site {siteid: {props}.siteid})-[:HAS]->(p:DHProfile {dhprofileid: {props}.dhprofileid})
WITH p,
     {dhprofileid: p.dhprofileid} AS result
DETACH DELETE p
RETURN result
