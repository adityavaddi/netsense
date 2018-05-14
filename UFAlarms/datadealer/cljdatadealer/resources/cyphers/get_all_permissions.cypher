MATCH(o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User:Active {email: {props}.email})-[:IS]->(ro:Role)-[rel]->(m:Model)
  OPTIONAL MATCH (o)-[:HAS]->(s:Site:Active)
 OPTIONAL MATCH (o)-[:IS_CUSTOMER_OF]->(o2:Org)
  WITH {
  type: ro.rolename,
  model: m.name,
 allowed: COLLECT( DISTINCT TYPE(rel) )
 } AS auth, {
  email: u.email,
  name: u.name,
  userid: u.userid,
  orgNames: COLLECT( DISTINCT o.name ),
  orgs: COLLECT( DISTINCT o.orgid ),
  sites: COLLECT( DISTINCT s.siteid ),
  siteNames: COLLECT( DISTINCT s.name ),
  hierarchy: COLLECT( DISTINCT { orgid: o.orgid, siteid: s.siteid } ),
  orghierarchy: COLLECT( DISTINCT {customer: o.orgid, provider: o2.orgid} )
  } AS userdata
  RETURN
  COLLECT( DISTINCT auth ) AS authorization,
 COLLECT( DISTINCT userdata ) AS user
