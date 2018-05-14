// CREATE all the constraints
CREATE CONSTRAINT ON (gn:GNode) ASSERT gn.id IS UNIQUE; // We probably dont need it
CREATE CONSTRAINT ON (org:Org) ASSERT org.orgid IS UNIQUE;
CREATE CONSTRAINT ON (org:Org) ASSERT org.name IS UNIQUE;
CREATE CONSTRAINT ON (site:Site) ASSERT site.siteid IS UNIQUE;
CREATE CONSTRAINT ON (site:Site) ASSERT site.name IS UNIQUE;
CREATE CONSTRAINT ON  (node:Node) ASSERT node.nodeid IS UNIQUE;
CREATE CONSTRAINT ON  (node:Node) ASSERT node.name IS UNIQUE;
CREATE CONSTRAINT ON (f:Fixture) ASSERT f.fixtureid IS UNIQUE;
CREATE CONSTRAINT ON (s:Sensor) ASSERT s.sensorid IS UNIQUE;
CREATE CONSTRAINT ON (pole:Pole) ASSERT pole.poleid IS UNIQUE;
CREATE CONSTRAINT ON (ap:AP) ASSERT ap.apid IS UNIQUE;
CREATE CONSTRAINT ON (so:SalesOrder) ASSERT so.soid IS UNIQUE;
CREATE CONSTRAINT ON (sku:SKU) ASSERT sku.skuid IS UNIQUE;
CREATE CONSTRAINT ON (u:User) ASSERT u.userid IS UNIQUE;
CREATE CONSTRAINT ON (r:Role) ASSERT r.name IS UNIQUE;
CREATE CONSTRAINT ON (sm:SiteModel) ASSERT sm.smid IS UNIQUE;
CREATE CONSTRAINT ON (nm:NodeModel) ASSERT nm.nmid IS UNIQUE;
CREATE CONSTRAINT ON (um:UserModel) ASSERT um.umid IS UNIQUE;
CREATE CONSTRAINT ON (om:OrgModel) ASSERT om.omid IS UNIQUE;

// if role name is modeled as label - we may not need this constraint - additional constraint does not though
// Create provider - thats the root
// A temporary super user - has to be decommissioned after initial setup

MERGE (o:Org:Provider {orgid: 1, name: "Sensity Systems"})-[:HAS_USER]->(u:User:SuperUser {userid: "superuser"})
    ON CREATE SET o.created=timestamp()
CREATE UNIQUE (u)-[:IS_USER_OF]->(o);
// A super user needs no role subscription

// Create the sitemodel - refer the wiki page and diagram that talks about the need of an abstract model
// smid value can be just smid as there is only one Gnode of this kind
MERGE (sm:SiteModel {smid: "smid"})
    ON CREATE SET sm.created=timestamp();
// Create a Node Model
MERGE (nm:NodeModel {nmid: "nmid"})
    ON CREATE SET nm.created=timestamp();
// Create User Model
MERGE (um:UserModel {umid: "umid"})
    ON CREATE SET um.created=timestamp();
MERGE (om:OrgModel {omid: "omid"})
     ON CREATE SET om.created=timestamp()

// CREATE a user, a role, provide site permissions, org permissions, and node permissions
MERGE (ro:Role {rolename: "ADMIN"})
    ON CREATE SET ro.created=timestamp()
MERGE (u:User:Active {userid: "uberuser", name: "Uber User"})
    ON CREATE SET u.created=timestamp()
MERGE (o:Org:Active {orgid: "uberorg", name: "Uber Org"})
    ON CREATE SET o.created=timestamp()
MERGE (s:Site:Active {siteid: "ubersite", sitename: "Uber Site"})
    ON CREATE SET s.created=timestamp()
CREATE UNIQUE (s)-[:BELONGS_TO]->(o)
CREATE UNIQUE (o)-[:HAS]->(s)
CREATE UNIQUE (u)-[:IS]->(ro)
CREATE UNIQUE (ro)-[:HAS]->(u)
CREATE UNIQUE (u)-[:IS_USER_OF]->(o)
CREATE UNIQUE (o)-[:HAS_USER]->(u)
MERGE (nm:NodeModel {nmid: "nmid"})
    ON CREATE SET nm.created=timestamp()
CREATE UNIQUE (ro)-[:CAN_CREATE]->(nm)
CREATE UNIQUE (ro)-[:CAN_CHANGE]->(nm)
CREATE UNIQUE (ro)-[:CAN_MOVE]->(nm)
CREATE UNIQUE (ro)-[:CAN_READ]->(nm)
CREATE UNIQUE (ro)-[:CAN_MANAGE_LIGHTS]->(nm)
CREATE UNIQUE (ro)-[:CAN_DEACTIVATE]->(nm)
// CREATE UNIQUE (ro)-[:CAN_CREATE_REPORT]->(nm) // Not required at the node level
MERGE (sm:SiteModel {smid: "nmid"})
    ON CREATE SET sm.created=timestamp()
CREATE UNIQUE (ro)-[:CAN_CREATE]->(sm)
CREATE UNIQUE (ro)-[:CAN_CHANGE]->(sm)
CREATE UNIQUE (ro)-[:CAN_READ]->(sm)
CREATE UNIQUE (ro)-[:CAN_GEN_REPORT]->(sm)
CREATE UNIQUE (ro)-[:CAN_SEE_ALL_ACTIVITY_LOG]->(sm)
CREATE UNIQUE (ro)-[:CAN_SEE_OWN_ACTIVITY_LOG]->(sm)

MERGE (um:UserModel {umid: "umid"})
     ON CREATE SET um.created=timestamp()
CREATE UNIQUE (ro)-[:CAN_CREATE_USER]->(um)
CREATE UNIQUE (ro)-[:CAN_DELETE_USER]->(um)
CREATE UNIQUE (ro)-[:CAN_CHANGE_USER]->(um)

CREATE UNIQUE (ro)-[:CAN_CREATE]->(om)
CREATE UNIQUE (ro)-[:CAN_READ]->(om)
CREATE UNIQUE (ro)-[:CAN_DELETE]->(om)
CREATE UNIQUE (ro)-[:CAN_CHANGE]->(om)
CREATE UNIQUE (ro)-[:CAN_ADD_SITE]->(om)
;