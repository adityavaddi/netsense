MATCH (g:Group)
WHERE g.groupid = {props}.id
RETURN g.name AS name
