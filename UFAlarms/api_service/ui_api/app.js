'use strict';
// Set instance id
global.instance_id = process.env.NODE_APP_INSTANCE;
if(!process.env.NODE_APP_INSTANCE){
    global.instance_id = 'IS-'+require('ip').address();
}


var fs = require('fs')
    , https = require('https')
    , url = require('url')
    , keys_dir = './keys/'
    , server_options = {
        key: fs.readFileSync(keys_dir + 'farallones_key.pem'),
        ca: fs.readFileSync(keys_dir + 'farallones_ca_cert.pem'),
        cert: fs.readFileSync(keys_dir + 'farallones_server_cert.pem'),
        ciphers: [
            "ECDHE-RSA-AES256-SHA384",
            "DHE-RSA-AES256-SHA384",
            "ECDHE-RSA-AES256-SHA256",
            "DHE-RSA-AES256-SHA256",
            "ECDHE-RSA-AES128-SHA256",
            "DHE-RSA-AES128-SHA256",
            "HIGH",
            "!aNULL",
            "!eNULL",
            "!EXPORT",
            "!DES",
            "!RC4",
            "!MD5",
            "!PSK",
            "!SRP",
            "!CAMELLIA"
        ].join(':'),
        honorCipherOrder: true
    };

var SwaggerExpress = require('swagger-express-mw');
var app = require('express')();
var cors = require('cors');
var Logger = require('bunyan');
var RotatingFileStream = require('bunyan-rotating-file-stream');
var LumberjackStream = require("bunyan-lumberjack");

var helmet = require('helmet');

var configManager = require('kea-config');
configManager.init('./config/main.conf.js');

var streams = [
    {
        type: 'raw',
        level: configManager.get('log.is_log_level'),
        stream: new RotatingFileStream({
            path: '/var/log/apis/api_service.log',
            threshold: configManager.get('log.is_log_threshold'),
            gzip: true,
            totalFiles: parseInt(configManager.get('log.is_log_total_files'))
        })
    }
];
var elk_service = process.env.elk_service || 'localhost';
if (process.env.elk_service) {
    //console.log("Using elk_service", elk_service)
    streams = [
        {
            stream: process.stdout,
            level: configManager.get('log.elk_log_level')
        }
        // },
        // {
        //     level: 'info',
        //     type: 'raw',
        //     stream: LumberjackStream({
        //         tlsOptions: {
        //             host: elk_service,
        //             port: 5043,
        //             ca: [fs.readFileSync('./lumberjack.crt', {encoding: 'utf-8'})]
        //         },
        //         lumberjackOptions: {
        //             allowDrop: function(logEntry) {
        //                 // If we have to drop logs, drop INFO level logs and lower - keep errors.
        //                 return logEntry.level <= Logger.INFO
        //             }
        //         },
        //         metadata:{beat:"example",type:"default"}
        //     })
        // }
    ];
}

global.log = new Logger({
    name: global.instance_id,
    src: true,
    streams: streams
});
global.log.info('Initializing IS instance %s', global.instance_id);

var multer = require('multer');//({ dest: '/tmp/' });
var storage = multer.memoryStorage()
var upload = multer({storage: storage})
var mqtt = require('mqtt');

// For Swagger UI

// env variable "ENABLE_SWAGGER_UI" should be true to have swagger ui
// TODO - Set this to false for PROD
var enable_swaggerui = process.env.ENABLE_SWAGGER_UI || 'true';

var swaggerTools = require('swagger-tools');
var YAML = require('yamljs');
var swaggerDoc = YAML.load('./api/swagger/swagger.yaml');
// add swaggerUi options
var uiOptions = {
    swaggerUi: '/v3.0/swagger-ui.html', // <-- path to serve Swagger UI from
    apiDocs: '/v3.0/api-docs' // <-- path to serve Swagger documents in JSON
  };

module.exports = app; // for testing

var timeoutInSec = configManager.get('request.timeoutInSec');

var timeout = require('connect-timeout');
app.use(timeout(`${timeoutInSec}s`));
var onTimedout = function (req, res, next) {
    if (req.timedout)
        global.log.info('Request has timed out.');
    else
        next();
    //else
}
app.use(onTimedout);
var cors_options = {
    //"origin": "http://localhost:8080",
    "origin": true,
    "methods": "GET,HEAD,PUT,PATCH,POST,DELETE",
    "preflightContinue": false,
    "credentials": true,
    "exposedHeaders":["netsense-csrf-token"]
    //"allowedHeaders": "X-Requested-With"
}
app.use(cors(cors_options));
app.use(onTimedout);

app.use(helmet());
app.use(onTimedout);

var passport = require("passport");
var LocalStrategy = require('passport-local').Strategy;
var WinLiveStrategy = require('passport-windowslive').Strategy;
var cookieParser = require('cookie-parser');
var bodyParser = require('body-parser');
var session = require('express-session');

var csrf = require('csurf');

var sessionstore = require('sessionstore');
app.use(cookieParser('jhf8r8rhuifdjfdghvcmksfdzihnvcdvfsd8re')); // read cookies (needed for auth)
app.use(onTimedout);
app.use(bodyParser.json({
    extended: true
}));
app.use(onTimedout);
// get information from html forms
app.use(session({
    secret: 'jhf8r8rhuifdjfdghvcmksfdzihnvcdvfsd8re',
    name: 'sess',
    store: sessionstore.createSessionStore(),
    proxy: true,
    resave: true,
    saveUninitialized: true,
    cookie: { httpOnly: true, secure: true }
})); // session secret
app.use(onTimedout);

if (process.env.NO_LDAP) {
    global.log.info("Won't start ldap  server");
} else {
    var ldap_server = require('./../../api_tests/ldap/app.js');
    ldap_server.Run();
}


var lm = require('./auth/LoginManager.js');
var acl = require('./auth/AclManager.js').AclManager;
var security = require('./api/helpers/securityHandlers.js');

const helpers = require('./api/helpers/helpers.js');

global.ldapClient = require('./services/ldap-client.js');

if (process.env.NO_GRAPHITE) {
    global.log.info("Won't start graphite server");
} else {
    global.fake_graphite_server = require('./services/graphite-server.js');
}
passport.serializeUser(function (user, done) {
    done(null, user);
});

passport.deserializeUser(function (user, done) {
    done(null, user);
});

app.use(passport.initialize());
app.use(onTimedout);
app.use(passport.session()); // persistent login sessions
app.use(onTimedout);
// Set x-request-if if not exists and copy to response
app.use(function (req, res, next) {
    //console.error("debug headers keys from interface: " + JSON.stringify(Object.keys(req.headers)));
    //console.error("debug headers: from interface" + JSON.stringify(req.headers));

    if (!req.headers['x-request-id']) {
        var uuid = require('uuid');
        req.headers['x-request-id'] = uuid.v1();
    }
    req.request_id = req.headers['x-request-id'];
    res.setHeader('x-request-id', req.headers['x-request-id']);
    next();

});

// CSRF setup for all routes
app.use(bodyParser.urlencoded({ extended: false }));
var csrfProtection = csrf();

app.use(csrfProtection);
app.use(onTimedout);

const ignoreCsrfApis = ['/v3.0/login', '/v3.0/user/update-password', '/v3.0/forgot-password'];
// CSRF error handler
app.use(function (err, req, res, next) {
    if (err.code !== 'EBADCSRFTOKEN') return next(err);

    // Ignore CSRF check for request with api_key in header
    if (req.headers['api_key']) return next();

    if (ignoreCsrfApis.indexOf(req.originalUrl) !== -1) {
        res.set('netsense-csrf-token', req.csrfToken()); // Send CSRF token in response header
        return next();
    }

    // Handle CSRF token error
    global.log.error('CSRF Token Validation error', JSON.stringify(err));
    res.status(403).send({ error: true, message: 'Access denied', status: 403 });
});

// To send updated CSRF token when requested with valid CSRF token
app.post(ignoreCsrfApis, (req, res, next) => {
    res.set('netsense-csrf-token', req.csrfToken()); // Send CSRF token in response header
    next();
});

// GET call to get CSRF token. Not used for now, but useful api to have
app.get('/v3.0/csrftoken', (req, res) => {
    global.log.info('CSRF token sent in header:', req.csrfToken());
    res.set('netsense-csrf-token', req.csrfToken());
    res.status(200).send();
});

// To accept an array of files, all with `jsonAlarmList` name
app.post('/v3.0/manage/bulkalarms', upload.array('jsonAlarmList'), (req, res, next) => {
    next();
});

// To accept an array of files, all with `csvNodeList` name
const csvNodeListApis = ['/v3.0/nodes', '/v3.0/customers/:orgid/sites/:siteid/nodes/assign', '/v3.0/customers/:orgid/sites/:siteid/nodes/delete'];
app.post(csvNodeListApis, upload.array('csvNodeList'), (req, res, next) => {
    next();
});

app.use(function (req, res, next) {
    security.apiKey(req, null, null, function () {
        if (!req.getCurrentUser) {
            req.getCurrentUser = function () {
                return req.user || ((req.session && req.session.CurrentUser) ? JSON.parse(JSON.stringify(req.session.CurrentUser)) : null);
            }
        }

        next();
    });
});
app.use(onTimedout);


/*app.use(function(req, res, next){
 res.setTimeout(180000, function(){
 console.log('Request has timed out.');
 res.sendStatus(408);
 });

 next();
 });*/

var swagger_config = {
    appRoot: __dirname, // required config
};

//To use Kafka in data stream
var msck = require('./dsi/kafka-msconnector');
var kafka_settings = configManager.get('kafka');

if (process.env.NO_MOSCA || configManager.get('mosca.skip')) {
    global.log.info("Won't start mosca server");
} else {
    global.moscaServer = require('./services/mosca-server.js');
}

global.alertListener = require('./notifications/dd-mqtt-client.js');

/** To add streaming (SSE with MQTT) support **/
app.param('cust', function (req, res, next, customer) {
    var user = req.getCurrentUser();
    if (!user) {
        global.log.error("NOT LOGGED IN...");
        res.setHeader('Content-Type', 'application/json');
        res.status(403).json({ "error": true, "message": "Please log in" });
        return;
    }
    lm.ReloadUserData(user.user.email, function (err, currentUser) {
        try {
            if(currentUser) {
                user = currentUser;
                //Refreshing req.user after reloading user data
                req.logIn(user, function(err) {
                    global.log.info('refresh user data', user);
                });
            }
            if (customer === '+') {
                global.log.error("The plus syntax is forbidden");
                res.write('{"error": true, "message": "The plus syntax is forbidden"}\n\n');
                return;
            }

            var allowed = false;
            global.log.info("Cust User", JSON.stringify(user));
            if (user && user.user.orgs) {
                console.log('App customer value: ', customer, " customers ", user.user.orgs);
                for (var ii = 0; ii < user.user.orgs.length; ii++) {
                    if (customer === user.user.orgs[ii]) {
                        allowed = true;
                        next();
                    }
                }
            }
            if (!allowed) {
                global.log.error("Not allowed to access orgid %s", customer);
                res.setHeader('Content-Type', 'application/json');
                res.status(403).json({ "error": true, "message": "Not allowed to access requested org" });
                return;
            }
        } catch (e) {
            res.setHeader('Content-Type', 'application/json');
            res.status(500).json({ "error": true, "message": e.message });
            return
        }
    });
});

app.param('site', function (req, res, next, site) {
    var user = req.getCurrentUser();
    if (!user) {
        global.log.error("NOT LOGGED IN...");
        res.setHeader('Content-Type', 'application/json');
        res.status(403).json({ "error": true, "message": "Please log in" });
        return;
    }
    else if (site === '+') {
        global.log.error("The plus syntax is forbidden");
        res.write('{"error": true, "message": "The plus syntax is forbidden"}\n\n');
        return;
    }

    var allowed = false;
    global.log.info("Site User", JSON.stringify(user));
    if (user && user.user.sites) {
        for (var ii = 0; ii < user.user.sites.length; ii++) {
            if (site === user.user.sites[ii]) {
                allowed = true;
                next();
            }
        }
    }
    if(!allowed){
        global.log.error("Not allowed to access siteid %s", site);
        res.setHeader('Content-Type', 'application/json');
        res.status(403).json({"error": true, "message": "Not allowed to access requested site"});
        return;
    }
});

// Business alerts SSE stream
app.get('/streamv1/:cust/:site/businessalert/:application/:triggerid', function (req, res) {
    const user = req.getCurrentUser();
    if (!user) {
        global.log.error("NOT LOGGED IN...");
        res.setHeader('Content-Type', 'application/json');
        res.status(403).json({ "error": true, "message": "Please log in1" });
        return;
    }

    const topic = url.parse(req.originalUrl).pathname;

    streamData(req, res, true, null, topic);
});

app.get('/streamv1/:cust/:site/:node/:event/:etype?', function (req, res) {
    var user = req.getCurrentUser();
    if (!user) {
        global.log.error("NOT LOGGED IN...");
        res.setHeader('Content-Type', 'application/json');
        res.status(403).json({ "error": true, "message": "Please log in" });
        return;
    }
    var parsedurl = url.parse(req.originalUrl);
    var event = req.params.event;
    var isSSEDataStream = false;
    var etype = req.params.etype;
    var topic = (req.params.etype) ?
        parsedurl.pathname :
        parsedurl.pathname.replace('/Traffic', '/traffic/Traffic').replace('/Parking', '/parking/Parking');


    const sseEvents = ['SensorSample', 'ConnectionStatus', 'DeviceAlarm', 'LoginReq', 'GpsSample', '+'];
        if (!etype) {
            etype = event;
            // Check if it's a request for SSE
            if(sseEvents.indexOf(event) !== -1) {
            isSSEDataStream = true;
        } else {
            event = parsedurl.pathname.indexOf('arking') !== -1 ? 'parking' : (parsedurl.pathname.indexOf('affic') !== -1 ? 'traffic' : 'other');
        }
    }
    global.log.info('Topic: %s, Pathname: %s, Complete URL: %s, url: %s', topic, parsedurl.pathname, req.originalUrl, req.url);
    streamData(req, res, isSSEDataStream, event, topic);
});

function streamData(req, res, isSSEDataStream, event, topic) {
    const user = req.getCurrentUser();
    let sseDataStreamSubscribed = false;

    const isBusinessAlert = (topic.indexOf('businessalert') !== -1) ? true : false;

    // set timeout as high as possible
    req.socket.setTimeout(0);

    // send headers for event-stream connection
    // see spec for more information
    res.writeHead(200, {
        'Content-Type': 'text/event-stream',
        'Cache-Control': 'no-cache',
        'Connection': 'keep-alive'
    });
    res.write('\n');

    // Timeout timer, send a comment line every 20 sec
    var timer = setInterval(function () {
        res.write(':' + '\n');
    }, 20000);

    // Timer to send heartbeat to MS every 30 mins (1800000 ms)
    var msHeartbeatTimer = setInterval(function () {
        if(isSSEDataStream && sseDataStreamSubscribed){
            sendDataStreamRequest(req, "heartbeat", res, isBusinessAlert, function (err, result) {
                global.log.info('Hearbeat request sent to MS');
            });
        }
    }, 1800000);

    var moscaPort = configManager.get('mosca.port');
    var moscaHost = configManager.get('mosca.host');
    var client = mqtt.connect("mqtt://" + moscaHost + ":" + moscaPort);
    var psid = req.query && req.query.psid ? req.query.psid : false;
    var pzid = req.query && req.query.pzid ? req.query.pzid : false;
    var pgid = req.query && req.query.pgid? req.query.pgid : false;
    var trafficdetectioneventid = req.query && req.query.trafficdetectioneventid ? req.query.trafficdetectioneventid : false;
    var type = req.query && req.query.type ? req.query.type : false;
    var object_class = req.query && req.query.object_class ? req.query.object_class : false;
    //var includeconfig = req.query && req.query.includeconfig? (req.query.includeconfig.toLowerCase()=='true') : false;
    var includeheartbeat = req.query && req.query.includeheartbeat? (req.query.includeheartbeat.toLowerCase()=='true') : false;

    client.on('connect', function () {
        global.log.info('mqtt client connected')
        //Check if it's for SSE events and subscribe for data stream
        if(isSSEDataStream) {
            sendDataStreamRequest(req, "subscribe", res, isBusinessAlert, function (err, result) {
                if(result){
                    sseDataStreamSubscribed = true;
                }
                // SSE MS just verify only org id and site id which is already taken care here.
            });
        }
        client.subscribe(topic, function () {
            client.on('message', function (topic, packed, pkt) {
                try{
                    var msg = JSON.parse(packed.toString());
                    var is_config = msg.type && msg.type.indexOf('Config') !== -1;
                    //console.log('>>>>>>>>><', msg, packed, packed.toString())
                    if ( event === 'parking' &&
                        (msg.type === type || !type) &&
                        (packed.toString().indexOf(psid) !== -1 || msg.parkingzoneid === pzid ||
                         msg.parkinggroupid === pgid ||
                         !( psid || pgid || pzid ) ) ){
                            if(includeheartbeat){
                                res.write('data:' + packed + '\n\n');
                            } else if(msg.type!=="DemarcatedParkingConfig" && msg.type!=="NonDemarcatedParkingConfig" && msg.spots!=='' && msg.spots.o!==''){
                                res.write('data:' + packed + '\n\n');
                            }

                    }
                    else if ( event === 'traffic' &&
                        (msg.type === type || !type ) &&
                        (!trafficdetectioneventid || msg.trafficdetectioneventid === trafficdetectioneventid)
                    ){
                        if (is_config)
                            res.write('data:' + packed + '\n\n');
                        else if (!object_class && includeheartbeat)
                            res.write('data:' + packed + '\n\n');
                        else if (!object_class) {
                            if(!msg.detected_objects || msg.detected_objects.length>0){
                                res.write('data:' + packed + '\n\n');
                            }
                        }
                        else if(msg.detected_objects && msg.detected_objects.length>0) {
                            for(var i = 0; i<msg.detected_objects.length; i++){
                                if(msg.detected_objects[i].class===object_class){
                                    res.write('data:' + packed + '\n\n');
                                    break;
                                }
                            }
                        }
                    } else if (isSSEDataStream) {
                        if(msg.name && msg.name.toLowerCase() === 'devicealarm') {
                            // Filter out events based on user role (usertype)
                            const userType = helpers.getUserType(user);
                            global.log.info('SSE DeviceAlarm event: %s & user type:', packed, userType);
                            switch (userType) {
                                case 'sensity':
                                    res.write('data:' + packed + '\n\n');
                                    break;
                                case 'partner':
                                    if(msg.displaytopartner) res.write('data:' + packed + '\n\n');
                                    break;
                                default: // consider it as customer
                                    if(msg.displaytocustomer) res.write('data:' + packed + '\n\n');
                            }
                        } else {
                            res.write('data:' + packed + '\n\n');
                        }
                    } else if ((!type || (msg.type === type)) && (event !== 'parking' && event !== 'traffic')) {
                        res.write('data:' + packed + '\n\n');
                    }
                } catch (err) {
                    global.log.error(err.message, err.stack)
                }

            });
        });
    });

    // When the request is closed, e.g. the browser window
    // is closed. We search through the open connections
    // array and remove this connection.
    req.on("close", function () {
        clearTimeout(timer);
        if(isSSEDataStream && sseDataStreamSubscribed){
            clearTimeout(msHeartbeatTimer);
            sendDataStreamRequest(req, "disconnect", res, isBusinessAlert, function (err, result) {
                global.log.info('successfully sent a disconnect request to Kafka');
            });
        }
        client.end();
    });
}

/**
 * To Send SSE request to Kafka
 * @param {*} req -- request
 * @param {*} type -- subscribe/heartbeat/disconnect
 * @param {*} res -- response
 * @param {*} callback
 */
function sendDataStreamRequest(req, type, res, isBusinessAlert, callback) {
    const model = isBusinessAlert ? "SSEBusinessAlertModel" : "SSEModel";
    let additionalObj = {};
    let sseKafkaTopicKey = 'sserequest'; // refer main.conf.js for the actual request topic name
    let params = {
        "requestid": req.request_id,
        "type": type,
        "model": model,
        "user": req.getCurrentUser().user.userid,
        "orgprops": {
            "orgid": req.params.cust
        },
        "siteprops": {
            "siteid": req.params.site
        }
    };

    if (isBusinessAlert) {
        sseKafkaTopicKey = 'businessalertsserequest';
        additionalObj = {
            "businessalertprops": {
                "application": req.params.application,
                "triggerid": req.params.triggerid
            }
        }
    } else {
        additionalObj = {
            "nodeprops": {
                "nodeid": req.params.node
            },
            "extprops": {
                "eventtype": req.params.event
            }
        }
    }
    params = Object.assign(params, additionalObj);
    try {
        //Using Kafka-msconnector to send message
        msck.Send(sseKafkaTopicKey, params, function (err, result) {
            global.log.info('MS response for SSE request', err, result);
            if (result.status.code == "200") {
                callback(null, result);
            } else {
                callback(err, null);
            }
        });
    } catch (e) {
        global.log.error(e.message);
        callback(err, null);
    }
}

/** Streaming End **/

/**
 * Status check
 */
app.get('/v3.0/status', function (req, res) {
    res.setHeader('Content-Type', 'application/json');
    res.status(200).json({"status": "ok"});
    return;
});

app.use(onTimedout);
if (true)
    SwaggerExpress.create(swagger_config, function (err, swaggerExpress) {
        if (err) {
            if (err.validationErrors) {
                for (var i in err.validationErrors) {
                    switch (err.validationErrors[i].code) {
                        case 'MISSING_PATH_PARAMETER_DEFINITION':
                            global.log.error('Swagger error', err.validationErrors[i].message, err.validationErrors[i].path.join(' '));
                            break;
                        default:
                            global.log.error('Swagger error', err.validationErrors[i].message);
                    }
                }
            } else {
                global.log.error(err.stack);
            }
            //res.status(400).send({error:true, message:err.message});
            return;
        }


        // install middleware
        swaggerExpress.register(app);

        // For Swagger UI
        if(enable_swaggerui == 'true'){
            swaggerTools.initializeMiddleware(swaggerDoc, function (middleware) {
                app.use(middleware.swaggerUi(uiOptions));
            });
        }

        // Error handler
        app.use(function (err, req, res, next) {
            global.log.error(err.message, err.stack);
            switch (err.code) {
                case 'SCHEMA_VALIDATION_FAILED':
                    global.log.error('Swagger validation error(s)', JSON.stringify(err));
                    res.status(400).send({error: true, message: err.message});
                    break;
                case 'ETIMEDOUT':
                    global.log.error('Response timeout error(s)', JSON.stringify(err));
                    res.status(408).send({error: true, message: err.message});
                    break;
                default:
                    if (err.message.indexOf('defined in Swagger') !== -1)
                        res.status(400).send({error: true, message: err.message});
                    else
                        res.status(500).send({error: true, message: err.message});
            }
        });
        var server;
        var port = process.env.interface_service_port || 10010;
        if (process.argv[2] === 'secure') {
            global.log.trace("starting secure server");
            server = https.createServer(server_options, app).listen(port);
        } else {
            global.log.trace("starting insecure server for local testing");
            server = app.listen(port);
        }
        if (server) {
            // listen for checkContinue events
            server.on('checkContinue', function (req, res) {
                req.checkContinue = true;
                app(req, res); // call express directly to route the request
            });
        }

        global.log.trace("%s started", global.instance_id)
    });

/**
 * Handle uncaughtException
 */
process.on('uncaughtException', function (err) {
    console.error((new Date).toUTCString() + ' uncaughtException:', err.message, err.stack);
    //console.error(err.stack);

    global.log.fatal((new Date).toUTCString() + ' uncaughtException:', err.message);
    global.log.trace(err.stack);
    process.exit(1);
})
