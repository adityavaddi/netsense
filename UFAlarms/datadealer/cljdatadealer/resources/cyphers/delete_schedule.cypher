MATCH (s:Schedule {scheduleid: {props}.scheduleid})
WHERE NOT (s)-[:LINKED]->(:LightingGroup)
OPTIONAL MATCH (s)-[:HAS]->(ce:CalendarEvents)-[:HAS]->(a:Action)
OPTIONAL MATCH (s)-[:HAS]->(nnw:NoNetwork)-[:BELONGS_TO]->(s)
DETACH DELETE s, ce, a, nnw
