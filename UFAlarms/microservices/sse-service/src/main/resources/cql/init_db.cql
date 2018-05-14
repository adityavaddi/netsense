//) WITH compaction= { 'class': 'MemoryOnlyStrategy' }
//AND caching = { 'keys' : 'NONE', 'rows_per_partition' : 'NONE'};
//) WITH compaction= { 'class': 'MemoryOnlyStrategy' }
//AND caching = { 'keys' : 'ALL', 'rows_per_partition' : 'NONE'};
CREATE KEYSPACE IF NOT EXISTS farallones WITH REPLICATION = { 'class': 'SimpleStrategy', 'replication_factor': 1 };


CREATE TABLE IF NOT EXISTS farallones.orghierarchy_by_nodeid(
orgid text,
siteid text,
nodeid text,
nodename text,
orgname text,
sitename text,
siteaddress text,
bssid text,
nodehw text,
PRIMARY KEY(nodeid));


CREATE TABLE IF NOT EXISTS farallones.sse_filter(
orgid text,
siteid text,
nodeid text,
eventtype text,
topicname text,
counter int,
PRIMARY KEY(orgid,siteid,nodeid,eventtype));


CREATE TABLE IF NOT EXISTS farallones.sse_filter_businessalerts(
orgid text,
siteid text,
application text,
triggerid text,
topicname text,
counter int,
PRIMARY KEY(orgid,siteid,application,triggerid));
