MATCH (g:Group)-[old:HAS|BELONGS_TO]-(n:Node)
WHERE g.groupid = {props}.groupid AND
      NOT (g:SiteLightingGroup) AND
      n.nodeid IN {props}.nodeids
OPTIONAL MATCH (n)-[schlink:HAS|BELONGS_TO]-(sch:Schedule) WHERE (g:LightingGroup)
OPTIONAL MATCH (n)-[etdhlink:HAS|BELONGS_TO]-(etdhprofile:ETDHProfile) WHERE (g:LightingGroup)
OPTIONAL MATCH (n)-[dhlink:HAS|BELONGS_TO]-(dhprofile:DHProfile) WHERE (g:LightingGroup)
OPTIONAL MATCH (n)-[pdlink:HAS|BELONGS_TO]-(pdprofile:PDProfile) WHERE (g:LightingGroup)
DELETE old, schlink, etdhlink, dhlink, pdlink
RETURN DISTINCT {
    lighting: g:LightingGroup,
    nodeid: n.nodeid,
    groupid: g.groupid
} AS result
