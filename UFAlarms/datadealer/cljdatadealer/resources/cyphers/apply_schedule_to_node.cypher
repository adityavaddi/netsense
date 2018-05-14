MATCH (sch:Schedule {scheduleid: {props}.scheduleid})-[:HAS]->(ce:CalendarEvents)
OPTIONAL MATCH (ce)-[:HAS]->(a:Action),
      (n:Node {nodeid: {props}.nodeid})
OPTIONAL MATCH (sch)-[:HAS]->(nnw:NoNetwork)
OPTIONAL MATCH (other:Schedule)-[old:HAS|BELONGS_TO]-(n) WHERE other <> sch
DELETE old
MERGE (n)-[:BELONGS_TO]->(sch)
MERGE (sch)-[:HAS]->(n)
WITH sch, ce, a, COLLECT({
     nodeid: n.nodeid,
     model: n.model,
     latitude: n.latitude,
     longitude: n.longitude
 }) AS items, nnw
ORDER BY a.id
WITH COLLECT( DISTINCT
  CASE WHEN a.time IS NULL
  THEN NULL
  ELSE {
    time: a.time,
    level: a.level
  }
  END) AS actions, sch, ce, nnw, items
ORDER BY ce.id
WITH COLLECT( DISTINCT {
     date: ce.date,
     days: ce.days,
     actions: actions,
     photocell_enabled: coalesce(ce.photocell_enabled, false),
     photocell_highLevel: coalesce(ce.photocell_highLevel, 100),
     photocell_lowLevel: coalesce(ce.photocell_lowLevel, 0)
     }) AS events, sch, nnw, items
WITH collect( distinct {
     highTime: nnw.highTime,
     highLevel: nnw.highLevel,
     lowTime: nnw.lowTime,
     lowLevel: nnw.lowLevel,
     photocell_enabled: nnw.photocell_enabled,
     photocell_highLevel: nnw.photocell_highLevel,
     photocell_lowLevel: nnw.photocell_lowLevel
     })[0] as network, events, sch, items
WITH {
    scheduleid: sch.scheduleid,
    name: sch.name,
    description: sch.description,
    events: events,
    network: network
} AS schedule, items
RETURN DISTINCT schedule, items
