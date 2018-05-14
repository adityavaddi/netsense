match (g:ParkingGroup)-[r:PARKING_GROUP_TO_USERS]->(u:User)-[s:USERS_TO_PARKING_GROUP]->(g:ParkingGroup) 
where u.userid = {props}.userid and g.parkinggroupid = {props}.parkinggroupid 
return type(r),type(s)
