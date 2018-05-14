MATCH (lg:LightingGroup)<-[link_r:LINKED]-(p:DHProfile)
WHERE lg.groupid = {props}.groupid
      OPTIONAL MATCH (p)-[r]-(n:Node)
WHERE (lg)--(n)
DELETE link_r, r
SET p.autocalibrate=true
