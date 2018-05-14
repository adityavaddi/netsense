MATCH (p:DHProfile {dhprofileid: {props}.dhprofileid})-[:BELONGS_TO]->(s:Site)
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
     dhprofileid: p.dhprofileid,
     name: p.name,
     description: p.description,
     autocalibrate: p.autocalibrate,
     autocalibrateoptout: p.autocalibrateoptout,
     gain: p.gain,
     resetTime: p.resetTime,
     minDrive: p.minDrive,
     maxDrive: p.maxDrive,
     slewRate: p.slewRate,
     beginTime: p.beginTime,
     endTime: p.endTime,
setPoint: p.setPoint,
   sites: sites,
   groups: groups,
   nodes: nodes
   } AS dhprofile,
   {
   latitude: s.latitude,
   longitude: s.longitude
   } AS site
RETURN dhprofile, site
