CREATE (sensity:Org:Active {orgid: "efe5bdb3-baac-5d8e-6cae57771c13", name: "Sensity Systems", street1: "1237 East Arques Ave", street2: "", city: "Sunnyvale", state: "CA", postal_code: "94085", country: "USA", created: timestamp()})

CREATE (sensity_admin_user:User:Active {userid: "114ad560-a046-11e5-a57f-ef24ae600576", email: "sensity_admin@sensity.com", name: "Sensity Admin", roles: "sensity_admin", created: timestamp(), updated:timestamp()})
CREATE (sensity_admin:Role:Admin {rolename: "sensity_admin", created: timestamp()})

CREATE UNIQUE (sensity_admin_user)-[:IS]->(sensity_admin)
CREATE UNIQUE (sensity_admin)-[:HAS]->(sensity_admin_user)
CREATE UNIQUE (sensity_admin_user)-[:IS_USER_OF]->(sensity)
CREATE UNIQUE (sensity)-[:HAS_USER]->(sensity_admin_user)

CREATE (sensity_user_user:User:Active {userid: "507ef3cc-cd2d-46d8-ae6d-7ccf430c1110", email: "sensity_user@sensity.com", name: "Sensity User", roles: "sensity_user", created: timestamp(), updated:timestamp()})
CREATE (sensity_user:Role:Admin {rolename: "sensity_user", created: timestamp()})

CREATE UNIQUE (sensity_user_user)-[:IS]->(sensity_user)
CREATE UNIQUE (sensity_user)-[:HAS]->(sensity_user_user)
CREATE UNIQUE (sensity_user_user)-[:IS_USER_OF]->(sensity)
CREATE UNIQUE (sensity)-[:HAS_USER]->(sensity_user_user)

CREATE (sensity_read_only_user:User:Active {userid: "47e3e2f2-b93b-4557-b668-271d43d028ac", email: "sensity_read_only@sensity.com", name: "Sensity Read Only", roles: "sensity_read_only", created: timestamp(), updated:timestamp()})
CREATE (sensity_read_only:Role:Admin {rolename: "sensity_read_only", created: timestamp()})

CREATE UNIQUE (sensity_read_only_user)-[:IS]->(sensity_read_only)
CREATE UNIQUE (sensity_read_only)-[:HAS]->(sensity_read_only_user)
CREATE UNIQUE (sensity_read_only_user)-[:IS_USER_OF]->(sensity)
CREATE UNIQUE (sensity)-[:HAS_USER]->(sensity_read_only_user)
;

CREATE (sch:Schedule {scheduleid: "default", name: "Default Schedule"})
CREATE (ce:CalendarEvents {days: ["mon","tue","wed","thu","fri","sat","sun"], photocell_enabled: true, photocell_highLevel: 100, photocell_lowLevel: 0})
CREATE UNIQUE (ce)-[:BELONGS_TO]->(sch)
CREATE UNIQUE (sch)-[:HAS]->(ce)
CREATE (a:Action {id: 2, time: "00:00:00", level: 100})
CREATE UNIQUE (ce)-[:HAS]->(a)
CREATE UNIQUE (a)-[:BELONGS_TO]->(ce)
CREATE (nnw:NoNetwork {photocell_enabled: true, photocell_highLevel: 100, photocell_lowLevel: 0})
CREATE (sch)-[:HAS]->(nnw)-[:BELONGS_TO]->(sch)
;

CREATE (c:Config {configid: "default", networkXSSID: "SensityDefault", networkXSecurity: "wpa2p", networkXPasskey: "netsense", networkYSSID: "XeraL2", networkYSecurity: "wpa2p", networkYPasskey: "kentspeed"})
;
