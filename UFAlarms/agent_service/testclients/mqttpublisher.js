var mqtt    = require('mqtt');
var client  = mqtt.connect('mqtt://localhost');
var msgpack = require('msgpack5')();
var enc = msgpack.encode;
var dec = msgpack.decode;

client.on('connect', function () {
  client.subscribe('Topic2');
  encmsg = enc('Hello Mqtt');
  encmsgtemperature = enc('Hello Hot Summer ...!!!');
  encpresence = enc("Hello Presence!!!");
  // client.publish('TopicA/lul', 'ss');
  client.publish('Presence', encpresence);
  client.publish('Presence/Acme/BaseballGround/CrowdDetector', enc("Wow presence"));
  client.publish("TopicA/lul", encmsg);
  client.publish("TopicB/lul/lulu/hulu", encmsg);
  client.publish("TopicC", encmsg);
  client.publish('TopicB/sensor/temp', encmsgtemperature);

  //client.end();
});

client.on('message', function (topic, message) {
  // message is Buffer
  // console.log(message.toString());
  console.log(dec(message));
  //client.end();
});
