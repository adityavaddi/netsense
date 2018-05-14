var zmq = require('zmq'),
    jsrequester_act_log = zmq.socket('dealer'),
    msgpack = require('msgpack5')(),
    Freezer = require('freezer-js'),
    Map = require('collections/map'),
    configManager = require('kea-config'),
    metrics = require('metrics'),
    ddReporting = require('./../metrics/dd-report.js'),
    uuid = require('uuid');

var cluster = require('cluster');
var http = require('http');

configManager.init('./config/main.conf.js');



// DD graphite reporter
var graphite_settings = configManager.get('graphite');
reporter = new ddReporting.GraphiteReporter(ddReporting.report, "interface", graphite_settings.host, graphite_settings.port);
reporter.on('log', function(level, msg, exc) {
    if(exc) {
        console.log('%s -- %s (%s)', level, msg, exc);
    } else {
        console.log('%s -- %s', level, msg);
    }
});
reporter.start(graphite_settings.reporting_interval);

var map = new Map({}),
    enc = msgpack.encode,
    dec = msgpack.decode

// TODO: Get all these from configuration file

var jsrequester_act_log_URL = configManager.get('datadealer.URL.jsrequester_act_log');

jsrequester_act_log.setsockopt(zmq.ZMQ_TCP_KEEPALIVE, 1);

// Register to monitoring events
jsrequester_act_log.on('connect', function(fd, ep) {
    global.log.debug('connect, endpoint:', ep);
});
jsrequester_act_log.on('connect_delay', function(fd, ep) {
    global.log.debug('connect_delay, endpoint:', ep);
});
jsrequester_act_log.on('connect_retry', function(fd, ep) {
    global.log.debug('connect_retry, endpoint:', ep);
});
jsrequester_act_log.on('accept', function(fd, ep) {
    global.log.debug('accept, endpoint:', ep);
});
jsrequester_act_log.on('accept_error', function(fd, ep) {
    global.log.debug('accept_error, endpoint:', ep);
    jsrequester_act_log.disconnect();
    jsrequester_act_log.unmonitor();
    jsrequester_act_log.monitor(500, 0);
    jsrequester_act_log.connect(jsrequester_act_log_URL);
});
jsrequester_act_log.on('close', function(fd, ep) {
    global.log.debug('close, endpoint:', ep);
});
jsrequester_act_log.on('close_error', function(fd, ep) {
    global.log.debug('close_error, endpoint:', ep);
});
jsrequester_act_log.on('disconnect', function(fd, ep) {
    global.log.debug('disconnect, endpoint:', ep);
    jsrequester_act_log.unmonitor();
    jsrequester_act_log.monitor(500, 0);
    jsrequester_act_log.connect(jsrequester_act_log_URL);
});

jsrequester_act_log.monitor(500, 0);
jsrequester_act_log.connect(jsrequester_act_log_URL);

jsrequester_act_log.on('message', function (msg) {
  try {
    jsonObj = JSON.parse(new Buffer(msg).toString('ascii'));
    var state = map.get(jsonObj.messageid);
    if(state) {
        var delta = process.hrtime(state.starttime);
        if(delta){
            //console.log('DD execution time for %s', jsonObj.messageid, delta, +(delta[0]*1e3+delta[1]/1e6).toFixed(2));
            ddReporting.timer.update(+(delta[0]*1e3+delta[1]/1e6).toFixed(2));
        }
        state.set('result', jsonObj.response);
    } else {
        global.log.error('No state for ', jsonObj.messageid);
    }
  } catch(exc) {
    global.log.error("Problem receiving from datadealer: ", exc.message);
    state.set('result', {error: true, message: exc.message, status: 500});
  }
});

var senddata = function (data, requester, callback) {
  try {
    var messageid = uuid.v4();
    var startime = process.hrtime();
    var freezer = new Freezer({starttime: startime});
    var state = freezer.get();
    //console.log('starttime for %s', messageid, startime);
    map.set(messageid, state);
    freezer.on('update', function (newValue) {
        //global.log.info("change happened")
        var result = freezer.get().result;
        global.log.info("interface <= datadealer ", JSON.stringify(result).substring(0,50));
        callback(result);
        map.delete(messageid)
    })
    // Inject messageid into the data object
    data.messageid = messageid;
    global.log.info("interface => datadealer ", JSON.stringify(data).substring(0,50));
    requester.send(JSON.stringify(data))
  } catch (exc) {
    global.log.error("Problem sending data to datadealer: ", exc.message);
  }
}

exports.Send = function (msg, callback) {

    var data = {
        request: msg
    }
    senddata(data, jsrequester_act_log, callback)
}

/***
 The bellow commented code block is for reference - this is how the function should be called

 getActivityLogs('uberuser', function(msg) {

    global.log.info('Msg: ' +msg)
})*/
