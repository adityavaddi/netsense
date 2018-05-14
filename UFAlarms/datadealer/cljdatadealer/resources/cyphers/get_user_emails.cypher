MATCH (u:User)-[:IS]->(ro:Role)
  WHERE u.userid IN {props}.userids
RETURN DISTINCT {
  userid: u.userid,
  name: u.name,
  email: u.email,
  phone: u.phone,
  title: u.title,
  type: ro.rolename
} AS items
