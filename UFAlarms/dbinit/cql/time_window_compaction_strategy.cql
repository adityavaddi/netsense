
ALTER TABLE parking_spot_historic WITH
  compaction={
    'tombstone_compaction_interval': '86400',
    'unchecked_tombstone_compaction': 'false',
    'compaction_window_size': '3',
    'compaction_window_unit': 'HOURS',
    'tombstone_threshold': '0.2',
    'class': 'com.jeffjirsa.cassandra.db.compaction.TimeWindowCompactionStrategy'}
AND compression={'chunk_length_kb': '64', 'sstable_compression': 'org.apache.cassandra.io.compress.LZ4Compressor'}
AND dclocal_read_repair_chance = 0.0
AND default_time_to_live = 259200
AND gc_grace_seconds = 250000;

ALTER TABLE traffic_status WITH
  compaction={
    'tombstone_compaction_interval': '86400',
    'unchecked_tombstone_compaction': 'false',
    'compaction_window_size': '3',
    'compaction_window_unit': 'HOURS',
    'tombstone_threshold': '0.2',
    'class': 'com.jeffjirsa.cassandra.db.compaction.TimeWindowCompactionStrategy'}
AND compression = {'chunk_length_kb': '64', 'sstable_compression': 'org.apache.cassandra.io.compress.LZ4Compressor'}
AND dclocal_read_repair_chance = 0.0
AND default_time_to_live = 172800
AND gc_grace_seconds = 250000;
