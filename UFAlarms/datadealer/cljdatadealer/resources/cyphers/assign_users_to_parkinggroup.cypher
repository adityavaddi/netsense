match (u:User), (g:ParkingGroup) where u.userid in {props}.userIdsToLink and g.parkinggroupid = {props}.parkinggroupid
create UNIQUE (g)-[r:PARKING_GROUP_TO_USERS]->(u) create UNIQUE (u)-[s:USERS_TO_PARKING_GROUP]->(g) return type(r),type(s)
