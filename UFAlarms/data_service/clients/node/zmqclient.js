var zmq = require('zmq'),
  fs = require('fs'),
  requester = zmq.socket('req'),
  msgpack = require('msgpack5')(),
  enc = msgpack.encode,
  dec = msgpack.decode,
  minimist = require('minimist'),
  argv = minimist(process.argv.slice(2), {
    default: { enc: false }
  });

requester.connect('tcp://localhost:5559');
var numReplies = 0;
var numRequests = 1500;

requester.on('message', function(msg) {
  numReplies += 1;
  console.log('Reply #' + numReplies + ": '" + (argv.enc ? dec(msg) : msg) + "'");
  if (numReplies == numRequests) process.exit();
});

var payload = null;
if (fs.existsSync("./problematic.json")) {
  payload = fs.readFileSync("./problematic.json", "utf8");
  numRequests = 1;
}

for (var i = 1; i <= numRequests; ++i) {
  console.log('Sending msg: ' + i);
  var msg = JSON.stringify({
    message: payload != null ? payload :
      '{"token": "token","nested":{"random": "' + Math.random().toString(36).substring(2) + '"}}',
    timestamp: new Date().getTime(),
    level: "INFO"
  });
  requester.send(argv.enc ? enc(msg) : msg);
}

