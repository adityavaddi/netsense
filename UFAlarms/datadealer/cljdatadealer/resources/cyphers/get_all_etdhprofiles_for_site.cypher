MATCH (:Site {siteid: {props}.siteid})-[:HAS]->(p:ETDHProfile)
      OPTIONAL MATCH (p)-[:LINKED]->(slg:SiteLightingGroup)
      OPTIONAL MATCH (p)-[:LINKED]->(g:Group:LightingGroup) WHERE NOT g:SiteLightingGroup
      OPTIONAL MATCH (p)-[:HAS]->(n:Node)
WITH
    COLLECT( DISTINCT
            CASE WHEN slg.groupid IS NULL
            THEN NULL
            ELSE {
    groupid: slg.groupid,
    name: slg.name
    }
            END
            ) AS sites,

    COLLECT( DISTINCT
            CASE WHEN g.groupid IS NULL
            THEN NULL
            ELSE {
    groupid: g.groupid,
    name: g.name
    }
            END
            ) AS groups,

    COLLECT( DISTINCT
            CASE WHEN n.nodeid IS NULL
            THEN NULL
            ELSE {
    nodeid: n.nodeid,
    name: n.name
    }
            END
            ) AS nodes, p
RETURN DISTINCT {
     etdhprofileid: p.etdhprofileid,
     name: p.name,
     `high-lux`: p.`high-lux`,
     `high-driver`: p.`high-driver`,
     `low-lux`: p.`low-lux`,
     `low-driver`: p.`low-driver`,
     `min-lux`: p.`min-lux`,
     `min-driver`: p.`min-driver`,
     `fast-poll`: p.`fast-poll`,
     `slow-poll`: p.`slow-poll`,
     scheduled: p.scheduled,
     sites: sites,
     groups: groups,
     nodes: nodes
     } AS items
