MATCH (s:Site {siteid: {props}.siteid})
CREATE (p:ETDHProfile)
SET p = {props}.etdhprofile
CREATE UNIQUE (p)-[:BELONGS_TO]->(s)
CREATE UNIQUE (p)<-[:HAS]-(s)
RETURN s.siteid AS siteid
