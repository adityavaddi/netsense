require('./globals');
//var looker = require('./looker');
var getLooker = require('./looker').sample;
var getEnergyLooker = require('./lookerEnergy').energyLooker;
var getSensorLooker = require('./lookerSensor').sensorLooker;
var configManager = require('kea-config');
configManager.init('./config/main.conf.js');

var fs = require('fs');
var path = require('path');
var express = require('express');
var compression = require('compression');
var cookieParser = require('cookie-parser');
var config = require('./config/environments');
var app = express();
var bodyParser = require('body-parser');
app.use(bodyParser.json()); // for parsing application/json
app.use(bodyParser.urlencoded({ extended: true })); // for parsing application/x-www-form-urlencoded

app.use(compression());
app.use(cookieParser());
app.use(express.static(path.join(process.cwd(), 'public')));

var rpackage = require('./package.json');
var Router = require('react-router').Router;
var createHistory = require('react-router').createMemoryHistory;
var ReactDOMServer = require('react-dom/server')
var defaultAppName = process.env.APP ? process.env.APP : 'app';
var routes = require('./public/js/' + defaultAppName + '/' + defaultAppName + '.node.js');

var refappport = config.referenceImplementation.port;
var webpack_host = config.referenceImplementation.host;
var webpack_dev_server_port = config.referenceImplementation.webpackServerPort;

// var webpack_host = configManager.get('referenceImplementation.host');
// var webpack_dev_server_port = configManager.get('referenceImplementation.webpackServerPort');
// var refappport = configManager.get('referenceImplementation.port') || 8090;

var html = fs.readFileSync(path.join(process.cwd(), 'src', 'jsx', defaultAppName, 'index.html'), {
  encoding: 'utf8'
});



var createStyleTag = function (file, media) {
  media = media || 'screen';
  return "    <link media='" + media + "' rel='stylesheet' type='text/css' href='" + file + "'>\n";
};

var stylesheets = '';
if (process.env.NODE_ENV === 'development') {
  stylesheets += createStyleTag('/css/' + defaultAppName + '/raw/{dir}/main.css', 'screen,print');
  stylesheets += createStyleTag('/css/' + defaultAppName + '/raw/{dir}/theme.css');
  stylesheets += createStyleTag('/css/' + defaultAppName + '/raw/{dir}/colors.css');
  stylesheets += createStyleTag('/css/' + defaultAppName + '/raw/{dir}/font-faces.css');
  html = html.replace(new RegExp('{appscript}', 'g'), 'http://' + webpack_host + ':' + webpack_dev_server_port + '/scripts/bundle.js');
} else {
  stylesheets += createStyleTag('/css/' + defaultAppName + '/min/{dir}/main.css', 'screen,print');
  stylesheets += createStyleTag('/css/' + defaultAppName + '/min/{dir}/theme.css');
  stylesheets += createStyleTag('/css/' + defaultAppName + '/min/{dir}/colors-blessed1.css');
  stylesheets += createStyleTag('/css/' + defaultAppName + '/min/{dir}/colors.css');
  stylesheets += createStyleTag('/css/' + defaultAppName + '/min/{dir}/font-faces.css');
  html = html.replace(new RegExp('{appscript}', 'g'), '/js/' + defaultAppName + '/' + defaultAppName + '.min.js');
}

html = html.replace(new RegExp('{app}', 'g'), defaultAppName);
html = html.replace(new RegExp('{stylesheets}', 'g'), stylesheets);
html = html.replace(new RegExp('{version}', 'g'), rpackage.version);

var ltr = html.replace(new RegExp('{dir}', 'g'), 'ltr');
var rtl = html.replace(new RegExp('{dir}', 'g'), 'rtl');

/** BEGIN X-EDITABLE ROUTES */

app.get('/xeditable/groups', function (req, res) {
  res.send([
    { value: 0, text: 'Guest' },
    { value: 1, text: 'Service' },
    { value: 2, text: 'Customer' },
    { value: 3, text: 'Operator' },
    { value: 4, text: 'Support' },
    { value: 5, text: 'Admin' }
  ]);
});

app.get('/xeditable/status', function (req, res) {
  res.status(500).end();
});

app.post('/xeditable/address', function (req, res) {
  res.status(200).end();
});

app.post('/dropzone/file-upload', function (req, res) {
  res.status(200).end();
});

/** END X-EDITABLE ROUTES */

app.get('/ltr', function (req, res, next) {
  res.redirect('/');
});

app.get('/rtl', function (req, res, next) {
  res.redirect('/');
});

app.get('/gitbuild', function (req, res) {
  res.status(200).send({ build: process.env.build });
});

app.post('/getLookerUrl',function(req,res){
 // getLooker();
  console.log(req.body.site);
  res.status(200).send(getLooker(req.body.site));
});

app.post('/getEnergyLookerUrl',function(req,res){
 // getEnergyLooker();
  res.status(200).send(getEnergyLooker(req.body.site,req.body.node,req.body.from, req.body.to, req.body.fixture, req.body.schedule));
});

app.post('/getSensorLookerUrl',function(req,res){
 // getSensorLooker();
  res.status(200).send(getSensorLooker(req.body.site,req.body.node,req.body.from, req.body.to, req.body.sensor1, req.body.sensor2, req.body.fromtime, req.body.totime, req.body.timeZone));
});

/** CATCH-ALL ROUTE **/
app.get('*', function (req, res, next) {
  if (req.url === '/favicon.ico'
    || (req.url.search('.l20n') !== -1)) return next();
  res.header('Access-Control-Allow-Origin', '*');
  res.header('Access-Control-Allow-Headers', 'X-Requested-With');
  res.header('X-Frame-Options', 'DENY');
  var isRTL = req.cookies.rubix_dir === 'rtl';
  var loggedIn = typeof req.cookies.sess != "undefined";
  var history = createHistory(req.url);
  //console.log("loggedIn: " + loggedIn + "; location: " + JSON.stringify(location));
  // console.log(history);
  //  console.log(history.getCurrentLocation());
  var str = ReactDOMServer.renderToString(routes(history));
  if (process.env.createRTL) {
    res.send(rtl.replace(new RegExp('{container}', 'g'), str));
  } else {
    res.send(ltr.replace(new RegExp('{container}', 'g'), str));
  }
});

// var server = app.listen(process.env.PORT, function() {

var server = app.listen(refappport, function () {
  if (typeof process.send != "undefined") {
    try {
      process.send('CONNECTED');
    } catch (e) {
      console.error(e);
    }
  } else {
    console.log('CONNECTED');
  }
});

process.on('uncaughtException', function (err) {
  console.log(arguments);
  process.exit(-1);
});