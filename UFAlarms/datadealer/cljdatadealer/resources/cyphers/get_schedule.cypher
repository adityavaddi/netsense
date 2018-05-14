MATCH (sch:Schedule {scheduleid: {props}.scheduleid})-[:HAS]->(ce:CalendarEvents)
OPTIONAL MATCH (ce)-[:HAS]->(a:Action)

WITH COLLECT( DISTINCT [1,2]) AS useless, sch, ce, a
ORDER BY a.id

WITH COLLECT( DISTINCT
  CASE WHEN a.time IS NULL
  THEN NULL
  ELSE {
      time: a.time,
      level: a.level
  }
  END) AS actions, sch, ce
ORDER BY ce.id

WITH COLLECT( DISTINCT {
    date: ce.date,
    days: ce.days,
    actions: actions,
    photocell_enabled: coalesce(ce.photocell_enabled, false),
    photocell_highLevel: coalesce(ce.photocell_highLevel, 100),
    photocell_lowLevel: coalesce(ce.photocell_lowLevel, 0)
}) AS events, sch

OPTIONAL MATCH (sch)-[:LINKED]->(slg:SiteLightingGroup)
WITH COLLECT( DISTINCT
    CASE WHEN slg.groupid IS NULL
        THEN NULL
        ELSE {
            groupid: slg.groupid,
            name: slg.name
        }
    END)
AS sitesLG, events, sch

OPTIONAL MATCH (sch)-[:LINKED]->(g:Group:LightingGroup) WHERE NOT g:SiteLightingGroup
WITH COLLECT( DISTINCT
    CASE WHEN g.groupid IS NULL
        THEN NULL
        ELSE {
            groupid: g.groupid,
            name: g.name
        }
    END)
AS groups, sitesLG, events, sch

OPTIONAL MATCH (sch)-[:HAS]->(n:Node)
WITH COLLECT( DISTINCT
	 CASE WHEN n.nodeid IS NULL
        THEN NULL
        ELSE {
        	nodeid: n.nodeid,
		      name: n.name
        }
    END
) AS nodes, groups, sitesLG, events, sch

OPTIONAL MATCH (sch)-[:HAS]->(nnw:NoNetwork)
WITH COLLECT( DISTINCT {
    highTime: nnw.highTime,
    highLevel: nnw.highLevel,
    lowTime: nnw.lowTime,
    lowLevel: nnw.lowLevel,
    photocell_enabled: coalesce(nnw.photocell_enabled, false),
    photocell_highLevel: coalesce(nnw.photocell_highLevel, 100),
    photocell_lowLevel: coalesce(nnw.photocell_lowLevel, 0)
})[0] as network, nodes, groups, sitesLG, events, sch

RETURN DISTINCT {
    scheduleid: sch.scheduleid,
    name: sch.name,
    description: sch.description,
    events: events,
    network: network,
    sites: sitesLG,
    groups: groups,
    nodes: nodes
} AS schedule
