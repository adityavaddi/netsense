/**
 * Created by chiradip on 11/1/15.
 */
var express = require('express');
var mqtt = require('mqtt');
var app = express();

app.param('site', function (req, res, next, site) {
    console.log('app DOT param is called with site vale: ', site);
    // TODO: @Dejan - This is the simple way of authorizing - if the the user is not
    // authorized to listen to the streaming of the nodes of the site - do
    // not invoke next
    // In this sample - we are assuming user is authorized to listen to site 'ubersite'
    if(site === 'ubersite') {
        next();
    }
})

app.get('/streamv1/:cust/:site/:node/:event', function(req, res) {
    // set timeout as high as possible
    console.log('Complete URL: ' + req.originalUrl);
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
    var timer = setInterval(function() {
        res.write(':' + '\n');
    }, 20000);


    var client = mqtt.connect('mqtt://localhost')
    client.on('connect', function() {
        client.subscribe(req.originalUrl, function() {
            client.on('message', function(topic, msg, pkt) {
                res.write('data:' + msg + '\n\n');
            });
        });
    });

    // When the request is closed, e.g. the browser window
    // is closed. We search through the open connections
    // array and remove this connection.
    req.on("close", function() {
        clearTimeout(timer);
        client.end();
    });
});

var server = app.listen(3000, function () {
    var host = server.address().address;
    var port = server.address().port;

    console.log('Example app listening at http://%s:%s', host, port);
});