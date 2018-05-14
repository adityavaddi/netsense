MATCH (p:PDProfile {pdprofileid: {props}.pdprofileid})-[:BELONGS_TO]->(s:Site)
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
            ) AS nodes, p, s
WITH {
     pdprofileid: p.pdprofileid,
     name: p.name,
     description: p.description,
     minLevel: p.minLevel,
     maxLevel: p.maxLevel,
     beginTime: p.beginTime,
     endTime: p.endTime,
     mode: coalesce(p.mode, case when p.radius is null then "no-radius" else "radius" end),
     radius: p.radius,
     detection_duration: p.detection_duration,
     sites: sites,
     groups: groups,
     nodes: nodes
     } AS pdprofile,
     {
     latitude: s.latitude,
     longitude: s.longitude
     } AS site
RETURN pdprofile, site
