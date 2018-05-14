/**
 * Created by chiradip on 10/29/15.
 */

var zmq = require('zmq')
    , responder = zmq.socket('rep');

responder.connect('tcp://localhost:6542');
responder.on('message', function(msg) {
    console.log('received request:', msg.toString());
    setTimeout(function() {
        responder.send("World");
    }, 1000);
});
