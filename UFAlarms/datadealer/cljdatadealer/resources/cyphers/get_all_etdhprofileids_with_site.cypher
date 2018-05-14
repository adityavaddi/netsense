MATCH (:Site)-[:HAS]->(p:ETDHProfile)
RETURN COLLECT( DISTINCT {
     etdhprofileid: p.etdhprofileid} ) AS items
