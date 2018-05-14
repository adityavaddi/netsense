MATCH (o:Org)-[:HAS]->(s:Site),
      (s)-[:HAS]->(g:Group),
      (s)-[:HAS]->(n:Node)
WHERE o.orgid = {props}.orgid AND
      s.siteid = {props}.siteid AND
      g.groupid = {props}.groupid AND
      n.nodeid IN {props}.nodeids AND
      (NOT "LightingGroup" IN labels(g) OR NOT n.model IN ["vdkmaster", "merlin", "falcon-q"])
OPTIONAL MATCH (g)<-[:LINKED]-(sch:Schedule)
OPTIONAL MATCH (g)<-[:LINKED]-(etdhprofile:ETDHProfile)
OPTIONAL MATCH (g)<-[:LINKED]-(dhprofile:DHProfile)
OPTIONAL MATCH (g)<-[:LINKED]-(pdprofile:PDProfile)
OPTIONAL MATCH (n)-[old:HAS|BELONGS_TO]-(other:LightingGroup)
WHERE other <> g AND (g:LightingGroup)
DELETE old
WITH n, g, sch, etdhprofile, dhprofile, pdprofile
CREATE UNIQUE (n)-[:BELONGS_TO]->(g)
CREATE UNIQUE (g)-[:HAS]->(n)
RETURN DISTINCT {
    lighting: g:LightingGroup,
    scheduleid: sch.scheduleid,
    etdhprofileid: etdhprofile.etdhprofileid,
    dhprofileid: dhprofile.dhprofileid,
    pdprofileid: pdprofile.pdprofileid,
    nodeid: n.nodeid,
    groupid: g.groupid
} AS result
