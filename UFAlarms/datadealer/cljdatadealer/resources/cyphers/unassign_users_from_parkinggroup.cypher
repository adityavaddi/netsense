match (g:ParkingGroup)-[r:PARKING_GROUP_TO_USERS]->(u:User)-[s:USERS_TO_PARKING_GROUP]->(g:ParkingGroup) 
where u.userid in {props}.userIdsToLink and g.parkinggroupid = {props}.parkinggroupid 
delete s
delete r 
return g