MATCH(t:Node)-[:ETDH {trigger: true}]-(p:ETDHProfile)
WHERE t.nodeid = {props}.nodeid
WITH COLLECT ( DISTINCT( {
     etdhprofileid: p.etdhprofileid
     }
                        ))
     AS profile
RETURN profile
