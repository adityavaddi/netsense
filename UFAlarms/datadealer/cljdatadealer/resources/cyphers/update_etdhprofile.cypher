MATCH (p:ETDHProfile {etdhprofileid: {props}.etdhprofileid})
WITH p, p.config AS c
SET p = { props }
SET p.config = c
