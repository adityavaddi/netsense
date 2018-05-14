var Report = require('metrics').Report,
    Counter = require('metrics').Counter,
    Timer = require('metrics').Timer,
    Meter = require('metrics').Meter,
    EventEmitter = require('events').EventEmitter,
    util = require('util'),
    Socket = require('net').Socket;

var reconnecting = false;

/**
 * A custom reporter that sends metrics to a graphite server on the carbon tcp interface.
 * @param {Report} registry report instance whose metrics to report on.
 * @param {String} prefix A string to prefix on each metric (i.e. app.hostserver)
 * @param {String} host The ip or hostname of the target graphite server.
 * @param {String} port The port graphite is running on, defaults to 2003 if not specified.
 * @constructor
 */
function GraphiteReporter(registry, prefix, host, port) {
    GraphiteReporter.super_.call(this);
    this.registry = registry;
    this.prefix = prefix;
    this.host = host;
    this.port = port || 2003;
}

util.inherits(GraphiteReporter, EventEmitter);

GraphiteReporter.prototype.start = function(intervalInMs) {
    var self = this;
    this.socket = new Socket();
    this.socket.on('error', function(exc) {
        if(!reconnecting) {
            reconnecting = true;
            self.emit('log', 'warn', util.format('Lost connection to %s. Will reconnect in 10 seconds.', self.host), exc);
            // Stop the reporter and try again in 1 second.
            self.stop();
            setTimeout(function () {
                reconnecting = false;
                self.start(intervalInMs);
            }, 10000);
        }
    });

    self.emit('log', 'verbose', util.format("Connecting to graphite @ %s:%d", this.host, this.port));
    this.socket.connect(this.port, this.host, function() {
        self.emit('log', 'verbose', util.format('Successfully connected to graphite @ %s:%d.', self.host, self.port));
        //GraphiteReporter.super_.prototype.start.call(self, intervalInMs);
        this.interval = setInterval(self.report.bind(self), intervalInMs);
    });
};

GraphiteReporter.prototype.stop = function() {
    //GraphiteReporter.super_.prototype.stop.call(this);
    if('interval' in this) {
        clearInterval(this.interval);
    }
    this.socket.end();
};

GraphiteReporter.prototype.report = function() {
    // Don't report while reconnecting.
    if(reconnecting) {
        return;
    }
    var metrics = this.getMetrics();
    var self = this;
    var timestamp = (new Date).getTime() / 1000;

    if(metrics.counters.length != 0) {
        metrics.counters.forEach(function (count) {
            self.reportCounter.bind(self)(count, timestamp);
        })
    }

    if(metrics.meters.length != 0) {
        metrics.meters.forEach(function (meter) {
            self.reportMeter.bind(self)(meter, timestamp);
        })
    }

    if(metrics.timers.length != 0) {
        metrics.timers.forEach(function (timer) {
            // Don't log timer if its recorded no metrics.
            if(timer.min() != null) {
                self.reportTimer.bind(self)(timer, timestamp);
            }
        })
    }
};

GraphiteReporter.prototype.send = function(name, value, timestamp) {
    if(reconnecting) {
        return;
    }
    this.socket.write(util.format('%s.%s %s %s\n', this.prefix, name, value,
        timestamp));
};

GraphiteReporter.prototype.reportCounter = function(counter, timestamp) {
    var send = this.send.bind(this);

    send(counter.name, counter.count, timestamp);
};

GraphiteReporter.prototype.reportMeter = function(meter, timestamp) {
    var send = this.send.bind(this);

    send(util.format('%s.%s', meter.name, 'count'), meter.count, timestamp);
    send(util.format('%s.%s', meter.name, 'mean_rate'), meter.meanRate(), timestamp);
    send(util.format('%s.%s', meter.name, 'm1_rate'), meter.oneMinuteRate(),
        timestamp);
    send(util.format('%s.%s', meter.name, 'm5_rate'), meter.fiveMinuteRate(),
        timestamp);
    send(util.format('%s.%s', meter.name, 'm15_rate'), meter.fifteenMinuteRate(),
        timestamp);
};

GraphiteReporter.prototype.reportTimer = function(timer, timestamp) {
    var send = this.send.bind(this);
    send(util.format('%s.%s', timer.name, 'count'), timer.count(), timestamp);
    send(util.format('%s.%s', timer.name, 'mean_rate'), timer.meanRate(), timestamp);
    send(util.format('%s.%s', timer.name, 'm1_rate'), timer.oneMinuteRate(),
        timestamp);
    send(util.format('%s.%s', timer.name, 'm5_rate'), timer.fiveMinuteRate(),
        timestamp);
    send(util.format('%s.%s', timer.name, 'm15_rate'), timer.fifteenMinuteRate(),
        timestamp);

    var percentiles = timer.percentiles([.50,.75,.95,.98,.99,.999]);
    send(util.format('%s.%s', timer.name, 'min'), timer.min(), timestamp);
    send(util.format('%s.%s', timer.name, 'mean'), timer.mean(), timestamp);
    send(util.format('%s.%s', timer.name, 'max'), timer.max(), timestamp);
    send(util.format('%s.%s', timer.name, 'stddev'), timer.stdDev(), timestamp);
    send(util.format('%s.%s', timer.name, 'p50'), percentiles[.50], timestamp);
    send(util.format('%s.%s', timer.name, 'p75'), percentiles[.75], timestamp);
    send(util.format('%s.%s', timer.name, 'p95'), percentiles[.95], timestamp);
    send(util.format('%s.%s', timer.name, 'p98'), percentiles[.98], timestamp);
    send(util.format('%s.%s', timer.name, 'p99'), percentiles[.99], timestamp);
    send(util.format('%s.%s', timer.name, 'p999'), percentiles[.999], timestamp);
};

/**
 * Retrieve the metrics associated with the report given to this reporter in a format that's easy to consume
 * by reporters.  That is an object with separate references for meters, timers and counters.
 * @returns {{meters: Array, timers: Array, counters: Array}}
 */
GraphiteReporter.prototype.getMetrics = function() {
    var meters = [];
    var timers = [];
    var counters = [];

    var trackedMetrics = this.registry.trackedMetrics;
    // Flatten metric name to be namespace.name is has a namespace and separate out metrics
    // by type.
    for (var namespace in trackedMetrics) {
        for (var name in trackedMetrics[namespace]) {
            var metric = trackedMetrics[namespace][name];
            if (namespace.length > 0) {
                metric.name = namespace + '.' + name;
            } else {
                metric.name = name;
            }
            var metricType = Object.getPrototypeOf(metric);
            if (metricType === Meter.prototype) {
                meters.push(metric);
            } else if (metricType == Timer.prototype) {
                timers.push(metric);
            } else if (metricType == Counter.prototype) {
                counters.push(metric);
            }
        }
    }

    return { meters: meters, timers: timers, counters: counters };
}

module.exports = GraphiteReporter;

function getDDReport() {
    var timer = new Timer();
    var report = new Report();

    report.addMetric("dd.execTimer", timer);

    return {
        report: report,
        timer: timer,
        GraphiteReporter: GraphiteReporter
    };
}
 
exports = module.exports = getDDReport();