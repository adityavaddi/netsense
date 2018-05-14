
var configManager = require('kea-config');
configManager.init('./../config/main.conf.js');
var settings = configManager.get('cassandra');
var fs = require('fs');
var parse = require('csv-parse');
var cassandra = require('cassandra-driver');

var client = new cassandra.Client({
    contactPoints: settings.contactPoints,
    policies: {
        loadBalancing: new cassandra.policies.loadBalancing.RoundRobinPolicy(),
        reconnection: new cassandra.policies.reconnection.ConstantReconnectionPolicy(5000)
    },
    keyspace: settings.keyspace
});


try {
    console.log('Connecting to cassandra');
    client.connect()
        .then(function () {
            //console.log('Connected to cluster with %d host(s): %j', client.hosts.length, client.hosts.keys());
            //console.log('Keyspaces: %j', Object.keys(settings.keyspace));
            doImport();
        })
        .catch(function (err) {
            console.error("Cassandra connect failed: ", err);
            return (client)?client.showdown():null;
        });


} catch (err) {
    console.error("error when initializing cassandra client: "+err.message, err.stack, settings);
}

/*
Load these
    cqlsh cassandra_service -e "COPY  ${cassandra_keyspace}.aggregation_traffic_events (siteid,nodeid,aggregation_type,starttime,type,eventid,objectclass,avgdwell,avgvelocity,enddt,endtime,eventcnt,medianvelocity,p85velocity,startday,startdt,starthr,ts) FROM './test/data/aggregation_traffic_events.csv';" --connect-timeout=3600
    cqlsh cassandra_service -e "COPY  ${cassandra_keyspace}.aggregation_traffic_events_site (siteid,aggregation_type,starttime,type,objectclass,avgdwell,avgvelocity,enddt,endtime,eventcnt,medianvelocity,p85velocity,startday,startdt,starthr,ts) FROM './test/data/aggregation_traffic_events_site.csv';" --connect-timeout=3600
    cqlsh cassandra_service -e "COPY  ${cassandra_keyspace}.traffic_status  (nodeid, time, active, channel, count, data, type, orgid, siteid, tags, trafficdetectioneventid,  updated) FROM './test/data/traffic_status.csv';" --connect-timeout=3600
    cqlsh cassandra_service -e "COPY  ${cassandra_keyspace}.traffic_current_status (nodeid, time, active, channel, count, data, type, orgid, siteid, tags, trafficdetectioneventid, updated, aggregated_count) FROM './test/data/traffic_current_status.csv';" --connect-timeout=3600
    cqlsh cassandra_service -e "COPY  ${cassandra_keyspace}.traffic_config (eventid, active, channel, data, type, nodeid, orgid, siteid, tags, time, updated) FROM './test/data/traffic_config.csv';" --connect-timeout=3600
    cqlsh cassandra_service -e "COPY  ${cassandra_keyspace}.device_sensor_samples FROM './test/data/device_sensor_samples.csv';" --connect-timeout=3600
    cqlsh cassandra_service -e "COPY  ${cassandra_keyspace}.parking_zone_status (siteid,parkingzoneid,config,orgid,parkinggroupid,state,type) FROM './test/data/parking_zone_status.csv';" --connect-timeout=3600

 */
function batch_row(table, data) {
    var query;
    var fixed_data=data;
    var integers = null;
    var doubles = null;
    var booleans = null;
    switch(table) {
        case 'aggregation_traffic_events':
            query = 'INSERT INTO aggregation_traffic_events ' +
                '(siteid,nodeid,aggregation_type,starttime,type,eventid,objectclass,avgdwell,avgvelocity,enddt,endtime,eventcnt,medianvelocity,p85velocity,startday,startdt,starthr,ts) ' +
                'VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)';
            doubles = {
                'avgdwell': 7,
                'avgvelocity': 8,
                'medianvelocity': 12,
                'p85velocity': 13,
            };
            integers = {
                'starttime': 3,
                'endtime': 10,
                'eventcnt': 11,
                'ts': 17,
            };
            break;
        case 'aggregation_traffic_events_site':
            query = 'INSERT INTO aggregation_traffic_events_site ' +
                '(siteid,aggregation_type,starttime,type,objectclass,avgdwell,avgvelocity,enddt,endtime,eventcnt,medianvelocity,p85velocity,startday,startdt,starthr,ts) ' +
                'VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)';
            doubles = {
                'avgdwell': 5,
                'avgvelocity': 6,
                'medianvelocity': 10,
                'p85velocity': 11,
            };
            integers = {
                'starttime': 2,
                'endtime': 8,
                'eventcnt': 9,
                'ts': 15,
            };
            break;
        case 'traffic_status':
            query = 'INSERT INTO traffic_status ' +
                '(nodeid, time, active, channel, count, data, type, orgid, siteid, tags, trafficdetectioneventid,  updated) ' +
                'VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)';
            doubles = null;
            integers = {
                'time': 1,
                'channel': 3,
                'count': 4,
                'ts': 15,
            };
            booleans = {
                'active': 2,
            };
            fixed_data[5] = eval(data[5]);
            break;
        case 'traffic_current_status':
            query = 'INSERT INTO traffic_current_status ' +
                '(nodeid, time, active, channel, count, data, type, orgid, siteid, tags, trafficdetectioneventid, updated, aggregated_count) ' +
                'VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)';
            break;
        case 'traffic_config':
            query = 'INSERT INTO traffic_config ' +
                '(eventid, active, channel, data, type, nodeid, orgid, siteid, tags, time, updated) ' +
                'VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)';
            break;
        /*case 'device_sensor_samples':
            query = 'INSERT INTO device_sensor_samples ' +
                '() ' +
                'VALUES (?, ?, ?, ?, ?)';
            break;*/
        case 'parking_zone_status':
            query = 'INSERT INTO parking_zone_status ' +
                '(siteid,parkingzoneid,config,orgid,parkinggroupid,state,type) ' +
                'VALUES (?, ?, ?, ?, ?, ?, ?)';
            break;       
    }
    if(doubles){
        for(var key in doubles) {
            if(data[doubles[key]]=='')
                fixed_data[doubles[key]] = null;
            else
                fixed_data[doubles[key]] = parseFloat(data[doubles[key]]);
        }
    }
    if(integers){
        for(var key in integers) {
            if(data[integers[key]]=='')
                fixed_data[integers[key]] = null;
            else
                fixed_data[integers[key]] = parseInt(data[integers[key]]);
        }
    }
    if(booleans){
        for(var key in booleans) {
            if(data[booleans[key]]=='')
                fixed_data[booleans[key]] = null;
            else
                fixed_data[booleans[key]] = eval(data[booleans[key]]);
        }
    }
    return {
        query: query,
        params: fixed_data
    }
    // client.execute(query, [ id, 'Hello!', 100 ], { prepare: true}, next);
}
var imported = 0;
var imported_tables = 0;
var should_import = 0;
var csvs = [
    //'aggregation_traffic_events',
    //'aggregation_traffic_events_site',
    'traffic_status',
    // 'traffic_current_status',
    // 'traffic_config',
    // 'device_sensor_samples',
    // 'parking_zone_status',
];


function importTable(table) {

    var batch=[];

    try {

        client.execute('truncate '+table+';', [  ], { prepare: true},function () {

            // For each file..
            var file = './cassandra/'+table+'.csv';

            console.log('Processing file '+file);
            //continue;
            // load data
            fs.createReadStream(file)
                .pipe(parse({delimiter: ',', auto_parse:true,escape:'\\'}))
                .on('error', function(err) {
                    console.error("error when processing csv data: "+err.message, table, err.stack, file);
                })
                .on('data', function(csvrow) {
                    try {
                        console.log(file, csvrow);
                        //do something with csvrow
                        batch.push(batch_row(table, csvrow));
                    } catch(err){

                        console.error("error when processing csv data: "+err.message, err.stack, file);
                    }
                })
                .on('end',function() {
                    //do something wiht csvData
                    console.log('Loaded table ',table);

                    //runBatch(batch, table);
                });
        });

    } catch(err){

        console.error("Import error: "+err.message, err.stack);
    }
}
function doImport(){

    for(var i=0;i<csvs.length;i++){
        importTable(csvs[i])
    }
}

function runBatch(batch, table){
console.log('Loading bach for table', table, batch.length)
    try {
        should_import += batch.length;
        // NOTE: Batch to large
        // client.batch(batch, { prepare: true })
        //     .then(result => console.log('Imported data ',table,  batch.length))
        //     .catch( function (err) {
        //         console.error('There was an error when importing table ', table, err);
        //     });
        batch.forEach((row) => {
            client.execute(row.query, row.params, function(err, result) {
                if(err)
                    console.error('There was an error when importing table ', table, row, err);
                imported++;
            });
        })
        imported_tables++;
        doneImport();
    } catch(err){
        console.error("error when importing data to cassandra: "+err.message, err.stack);
    }
}

function doneImport(){

    if(imported_tables==csvs.length && should_import==imported)
        exit();
}
