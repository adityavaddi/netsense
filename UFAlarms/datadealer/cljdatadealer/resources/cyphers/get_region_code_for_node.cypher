MATCH (n:Node {nodeid: {props}.nodeid})-[:BELONGS_TO]->(s:Site)
RETURN s.country_code AS country_code
