var zmq = require('zmq'),
    mpkrequester = zmq.socket('req'),
    jsrequester = zmq.socket('req'),
    msgpack = require('msgpack5')(),
    Freezer = require('freezer-js'),
    Map = require('collections/map'),
    dateFormat = require('dateformat');

var now = new Date();
var dd = dateFormat(now, 'isoDateTime');
console.log("date => " + dd);

var map = new Map({}),
    enc = msgpack.encode,
    dec = msgpack.decode

mpkrequester.connect('tcp://localhost:6531');
jsrequester.connect('tcp://localhost:5521');
var mpReplyNbr = 0;
var jsReplyNbr = 0;

mpkrequester.on('message', function (msg) {
    console.log('got reply', mpReplyNbr, dec(msg));
    mpReplyNbr += 1;
});

jsrequester.on('message', function (msg) {
    //console.log('got JS reply', jsReplyNbr, new Buffer(msg).toString('ascii'));
    jsonObj = JSON.parse(new Buffer(msg).toString('ascii'))
    map.get(jsonObj.qid).set('result', jsonObj.resp)
    jsReplyNbr += 1;
});

var senddata = function (data, requester, callback) {
    var qid = Math.random().toString()
    var freezer = new Freezer({})
    var state = freezer.get()
    map.set(qid, state)
    freezer.on('update', function (newValue) {
        console.log("change happened")
        callback(freezer.get().result)
        map.delete(qid)
    })

    // Inject QID into the data object
    data.qid = qid
    requester.send(JSON.stringify(data))
}

var data = {
    name: 'LoginReq',
    nodeid: 'N01232ea3',
    protovsn: 0,
    clienttype: "unode-v4",
    swvsnid: "2b9f02c",
    ssid: "XeraL2",
    profilename: "x",
    assocchannel: 48,
    cfgtoken: "f1b5d481",
    ip: "10.20.109.34",
    ts: 1444778562623260
};

mpkrequester.send(enc(JSON.stringify(data)));




