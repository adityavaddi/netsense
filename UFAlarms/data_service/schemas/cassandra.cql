CREATE KEYSPACE farallones WITH REPLICATION = { 'class' : 'SimpleStrategy', 'replication_factor' : 1 };

USE farallones;

CREATE TABLE log_entries (
  key uuid PRIMARY KEY,
  app_start_time bigint,
  app_name text,
  host_name text,
  host_ip text,
  class_name text,
  file_name text,
  level text,
  line_number text,
  timestamp bigint,
  logger_class_name text,
  message text,
  method_name text,
  ndc text,
  thread_name text,
  throwable_str_repr text
) WITH
  comment='' AND
  read_repair_chance=1.000000 AND
  gc_grace_seconds=864000;
