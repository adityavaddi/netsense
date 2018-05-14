MATCH(o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User:Active {email: {props}.email})
OPTIONAL MATCH (o)-[:HAS]->(s:Site:Active)
OPTIONAL MATCH (o)-[:IS_CUSTOMER_OF]->(o2:Org)
RETURN {
  email: u.email,
  name: u.name,
  userid: u.userid,
  orgNames: COLLECT( DISTINCT o.name ),
  orgs: COLLECT( DISTINCT o.orgid ),
  sites: COLLECT( DISTINCT s.siteid ),
  siteNames: COLLECT( s.name ),
  hierarchy: COLLECT( DISTINCT { orgid: o.orgid, siteid: s.siteid } ),
  orghierarchy: COLLECT( DISTINCT {customer: o.orgid, provider: o2.orgid} )
} AS user
