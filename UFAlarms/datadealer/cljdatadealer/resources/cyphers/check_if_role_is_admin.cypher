MATCH (n:ParkingGroup {parkinggroupid: {props}.parkinggroupid})-[:BELONGS_TO]->(s:Site)-[:BELONGS_TO]->(o:Org)-[:IS_CUSTOMER_OF*0..4]->(po:Org)-[:HAS_USER]->(u:User {userid: {props}.userid})-[:IS]->(role:Role) 
WHERE  role.rolename in ["sensity_user","parking_owner_admin", "sensity_admin", "end_user_admin", "partner_admin"] 
RETURN role.rolename
