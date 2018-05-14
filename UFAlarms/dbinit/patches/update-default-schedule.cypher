MATCH (ce:CalendarEvents)-[:BELONGS_TO]->(sch:Schedule {scheduleid: "default"})
SET ce.photocell_enabled = coalesce(ce.photocell_enabled,true)
SET ce.photocell_highLevel = coalesce(ce.photocell_highLevel,100)
SET ce.photocell_lowLevel = coalesce(ce.photocell_lowLevel,0);
MATCH (sch:Schedule {scheduleid: "default"})
MERGE (sch)-[:HAS]->(nnw:NoNetwork {photocell_enabled: true, photocell_highLevel: 100, photocell_lowLevel: 0})-[:BELONGS_TO]->(sch);


