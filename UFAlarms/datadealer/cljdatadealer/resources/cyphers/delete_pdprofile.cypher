MATCH (:Org {orgid: {props}.orgid})-[:HAS]->(:Site {siteid: {props}.siteid})-[:HAS]->(p:PDProfile {pdprofileid: {props}.pdprofileid})
WITH p,
     {pdprofileid: p.pdprofileid} AS result
DETACH DELETE p
RETURN result
