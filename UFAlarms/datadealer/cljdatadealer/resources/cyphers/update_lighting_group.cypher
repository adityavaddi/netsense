// Confirm the group exists under the site
MATCH (s:Site {siteid: {props}.siteid})-[:HAS]->(g:Group:LightingGroup {groupid: {props}.groupid})
// Collect the current nodes the group has
OPTIONAL MATCH (o:Node)-[:BELONGS_TO]->(g)
WITH DISTINCT COLLECT(o.nodeid) AS current, s, g
// Find the relationships for the current nodes to this group to later remove
OPTIONAL MATCH (:Node)-[oldN:BELONGS_TO|HAS]-(g)
// Clean out *all* nodes from this group. We will re-establish them later.
DELETE oldN
WITH current, s, g
// Optionally find any schedule/profiles this group has
OPTIONAL MATCH (sch:Schedule)-[:LINKED]->(g)
OPTIONAL MATCH (etdh:ETDHProfile)-[:LINKED]->(g)
OPTIONAL MATCH (dh:DHProfile)-[:LINKED]->(g)
OPTIONAL MATCH (pd:PDProfile)-[:LINKED]->(g)
// Update group properties
SET
    g.description = {props}.description,
    g.name = {props}.name
WITH DISTINCT { props } AS props, g, s, sch, etdh, dh, pd, current
        // Resolve props.nodeList to Neo4j nodes
        OPTIONAL MATCH (s)-[:HAS]->(n:Node) WHERE n.nodeid IN props.nodeList
        // Prune all relationships for the incoming nodes. We will re-establish them later.
        OPTIONAL MATCH (:Group:LightingGroup)-[oldGB:BELONGS_TO]-(n)
        OPTIONAL MATCH (:Group:LightingGroup)-[oldGH:HAS]-(n)
        OPTIONAL MATCH (:Schedule)-[oldSB:BELONGS_TO]-(n)
        OPTIONAL MATCH (:Schedule)-[oldSH:HAS]-(n)
        OPTIONAL MATCH (:ETDHProfile)-[oldTEDHB:BELONGS_TO]-(n)
        OPTIONAL MATCH (:ETDHProfile)-[oldTEDHH:HAS]-(n)
        OPTIONAL MATCH (:DHProfile)-[oldDHB:BELONGS_TO]-(n)
        OPTIONAL MATCH (:DHProfile)-[oldDHH:HAS]-(n)
        OPTIONAL MATCH (:PDProfile)-[oldPDB:BELONGS_TO]-(n)
        OPTIONAL MATCH (:PDProfile)-[oldPDH:HAS]-(n)
        DELETE oldGB, oldGH, oldSB, oldSH, oldTEDHB, oldTEDHH, oldDHB, oldDHH, oldPDB, oldPDH
        // As promised, re-stablish link between incoming nodes and this group.
        FOREACH (node IN
                CASE WHEN n IS NULL
                    THEN []
                    ELSE [n]
                END |
                CREATE UNIQUE (node)-[:BELONGS_TO]->(g)
                CREATE UNIQUE (g)-[:HAS]->(node))
        // If the lighting group has a schedules, assign it.
        FOREACH (node IN
                CASE WHEN sch IS NULL OR n IS NULL
                    THEN []
                    ELSE [n]
                END |
                CREATE UNIQUE (node)-[:BELONGS_TO]->(sch)
                CREATE UNIQUE (sch)-[:HAS]->(node))
        // If the lighting group has profiles, assign them.
        FOREACH (node IN
                CASE WHEN etdh IS NULL OR n IS NULL
                    THEN []
                    ELSE [n]
                END |
                CREATE UNIQUE (node)-[:BELONGS_TO]->(etdh)
                CREATE UNIQUE (etdh)-[:HAS]->(node))
        FOREACH (node IN
                CASE WHEN dh IS NULL OR n IS NULL
                    THEN []
                    ELSE [n]
                END |
                CREATE UNIQUE (node)-[:BELONGS_TO]->(dh)
                CREATE UNIQUE (dh)-[:HAS]->(node))
        FOREACH (node IN
                CASE WHEN pd IS NULL OR n IS NULL
                    THEN []
                    ELSE [n]
                END |
                CREATE UNIQUE (node)-[:BELONGS_TO]->(pd)
                CREATE UNIQUE (pd)-[:HAS]->(node))
        WITH DISTINCT n, g, current
            ORDER BY n.nodeid
            WITH COLLECT(n.nodeid) AS nodeList, g, current
                // Calculate the nodes joining this group and the nodes leaving this group.
                WITH [nodeid in nodeList WHERE NOT(nodeid IN current) | nodeid] AS incoming,
                     [nodeid in current WHERE NOT(nodeid IN nodeList) | nodeid] AS outgoing,
                     nodeList, g
                    RETURN DISTINCT
                        {
                            groupid: g.groupid,
                            name: g.name,
                            description: g.description,
                            nodeList: nodeList
                        } AS group,
                        {
                            incoming: incoming,
                            outgoing: outgoing
                        } AS diffs
