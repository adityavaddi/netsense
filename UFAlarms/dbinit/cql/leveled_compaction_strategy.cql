ALTER TABLE traffic_current_status
WITH compaction = {'sstable_size_in_mb': '256', 'unchecked_tombstone_compaction': 'true', 'tombstone_compaction_interval': '3600', 'tombstone_threshold': '0.1', 'class': 'org.apache.cassandra.db.compaction.LeveledCompactionStrategy'}
AND compression = {'chunk_length_kb': '4', 'sstable_compression': 'org.apache.cassandra.io.compress.LZ4Compressor'};


ALTER TABLE light
WITH compaction = {'sstable_size_in_mb': '256', 'unchecked_tombstone_compaction': 'true', 'tombstone_compaction_interval': '3600', 'tombstone_threshold': '0.1', 'class': 'org.apache.cassandra.db.compaction.LeveledCompactionStrategy'}
AND compression = {'chunk_length_kb': '4', 'sstable_compression': 'org.apache.cassandra.io.compress.LZ4Compressor'};


ALTER TABLE node_status
WITH compaction = {'sstable_size_in_mb': '256', 'unchecked_tombstone_compaction': 'true', 'tombstone_compaction_interval': '3600', 'tombstone_threshold': '0.1', 'class': 'org.apache.cassandra.db.compaction.LeveledCompactionStrategy'}
AND compression = {'chunk_length_kb': '4', 'sstable_compression': 'org.apache.cassandra.io.compress.LZ4Compressor'};


ALTER TABLE parking_spot_current
WITH compaction = {'sstable_size_in_mb': '256', 'unchecked_tombstone_compaction': 'true', 'tombstone_compaction_interval': '3600', 'tombstone_threshold': '0.1', 'class': 'org.apache.cassandra.db.compaction.LeveledCompactionStrategy'}
AND compression = {'chunk_length_kb': '4', 'sstable_compression': 'org.apache.cassandra.io.compress.LZ4Compressor'};


ALTER TABLE orghierarchy_by_nodeid
WITH compaction = {'sstable_size_in_mb': '256', 'unchecked_tombstone_compaction': 'true', 'tombstone_compaction_interval': '3600', 'tombstone_threshold': '0.1', 'class': 'org.apache.cassandra.db.compaction.LeveledCompactionStrategy'}
AND compression = {'chunk_length_kb': '4', 'sstable_compression': 'org.apache.cassandra.io.compress.LZ4Compressor'};
