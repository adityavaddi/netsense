MATCH (g:Group {groupid: {props}.groupid})
      OPTIONAL MATCH (g)-[:HAS]->(n:Node)
      OPTIONAL MATCH (sch:Schedule)-[:LINKED]->(g)
      OPTIONAL MATCH (etdh:ETDHProfile)-[:LINKED]->(g)
      OPTIONAL MATCH (dh:DHProfile)-[:LINKED]->(g)
      OPTIONAL MATCH (pd:PDProfile)-[:LINKED]->(g)
WITH COLLECT( DISTINCT
             CASE WHEN sch.scheduleid IS NULL
             THEN NULL
             ELSE {
     scheduleid: sch.scheduleid,
     name: sch.name
     }
             END) AS schedules,
     COLLECT( DISTINCT
             CASE WHEN pd.pdprofileid IS NULL
             THEN NULL
             ELSE {
     pdprofileid: pd.pdprofileid,
     name: pd.name
     }
             END) AS pdprofiles,
     COLLECT( DISTINCT
             CASE WHEN etdh.etdhprofileid IS NULL
             THEN NULL
             ELSE {
     etdhprofileid: etdh.etdhprofileid,
     name: etdh.name
     }
             END) AS etdhprofiles,
     COLLECT( DISTINCT
             CASE WHEN dh.dhprofileid IS NULL
             THEN NULL
             ELSE {
     dhprofileid: dh.dhprofileid,
     name: dh.name
     }
             END) AS dhprofiles,
     CASE
     WHEN g:SiteLightingGroup THEN "site-lighting"
     WHEN g:LightingGroup THEN "lighting"
     ELSE "organizational"
     END AS groupType, g, n
ORDER BY n.nodeid
RETURN {
       groupid: g.groupid,
       name: g.name,
       description: g.description,
       type: groupType,
       nodeList: COLLECT(n.nodeid),
       nodes: COLLECT({nodeid: n.nodeid, model: n.model}),
       schedules: schedules,
       pdprofiles: pdprofiles,
       etdhprofiles: etdhprofiles,
       dhprofiles: dhprofiles
       } AS group
