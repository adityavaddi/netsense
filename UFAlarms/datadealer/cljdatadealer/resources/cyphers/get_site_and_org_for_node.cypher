MATCH (n:Node)
WHERE n.nodeid={props}.nodeid
OPTIONAL MATCH (n)-[:BELONGS_TO]->(s:Site)-[:BELONGS_TO]->(o:Org)
OPTIONAL MATCH (sch:Schedule)-[:HAS]->(n)
OPTIONAL MATCH (c:Config)-[:BELONGS_TO]->(n)
RETURN DISTINCT {
    nodeid: n.nodeid,
    nodebssid: n.bssid,
    nodehw: n.model,
    softwareVersion: n.softwareVersion,
    nodename: n.name,
    nodelabels: labels(n),
    siteid: s.siteid,
    sitename: s.name,
    siteaddress: s.address,
    sitelabels: labels(s),
    orgid: o.orgid,
    orgname: o.name,
    scheduleid: sch.scheduleid,
    configid: c.configid
} AS data
