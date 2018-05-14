-record(nodes, {nodeid, org, site, group, swid, loginmap}).
-record(connected_nodes,{nodeid}).
-record(cql_query, {
    statement   = <<>>      :: iodata(),
    values      = [],

    reusable    = undefined :: undefined | boolean(),
    named       = false     :: boolean(),
    
    page_size   = 100       :: integer(),
    page_state              :: binary() | undefined,
    
    consistency = 1,
    serial_consistency = undefined,

    value_encode_handler = undefined
}).

%Record.defrecord :cql_query, statement: nil, values: [], reusable: :undefined, named: false, page_size: 100, page_state: :undefined, consistency: 1, serial_consistency: :undefined, value_encode_handler: :undefined