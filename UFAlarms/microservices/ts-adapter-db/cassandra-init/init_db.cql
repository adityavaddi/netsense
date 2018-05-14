
CREATE KEYSPACE IF NOT EXISTS sensity_device_mapping WITH REPLICATION = { 'class': 'SimpleStrategy', 'replication_factor': 1 };


CREATE TABLE IF NOT EXISTS sensity_device_mapping.devicemap(
  nodeid varchar,
  tsdeviceid varchar,
  createdon timestamp,
  PRIMARY KEY(nodeid)
);
