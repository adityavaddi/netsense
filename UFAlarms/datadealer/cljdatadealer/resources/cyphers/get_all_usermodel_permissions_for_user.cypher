MATCH (u:User:Active {email: {props}.email})-[:IS]->(ro:Role)-[rel]->(m:Model)
RETURN {
  type: ro.rolename,
  model: m.name,
  allowed: COLLECT( DISTINCT TYPE(rel) )
} AS authorization
