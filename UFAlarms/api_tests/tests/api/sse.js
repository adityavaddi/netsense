'use strict'
var server_url = process.env.stack_url ;
var should = require('should');
var request = require('supertest')(server_url);
var msgpack =  require('msgpack5')();
var superagent = require('superagent');
var agent = superagent.agent();
var version = '/v3.0';
var amqp = require('amqplib/callback_api');
var mqtt = require('mqtt');
var Kafka = require('node-rdkafka')
var node_id = "mochanode";
var corenode_id = "corenodemochanode";
var stream = require('stream');
var configManager = require('kea-config');
configManager.setup('./config/');

const helpers = require('./../../utils/helpers');
let csrfToken = null;

var mqttClient = mqtt.connect(configManager.get('sse').url);

var producer;

function sendMessageToKafka(topic, message, callback) {
    var data = JSON.stringify(message);
    console.log('kafka payload', data);

    var producer = new Kafka.Producer({
      'metadata.broker.list': configManager.get('kafkaurl'),
      'dr_cb': true
    });

    producer.connect();

    var topicName = topic;

    var counter =0, maxMessages=1;
    producer.on('delivery-report', function(err, report) {
      console.log('delivery-report: ' + JSON.stringify(report));
      callback(null);
    });

    //Wait for the ready event before producing
    producer.on('ready', function(arg) {
      producer.produce(topicName, -1, new Buffer(message), null);

      //need to keep polling for a while to ensure the delivery reports are received
      var pollLoop = setInterval(function() {
          producer.poll();
          if (counter === maxMessages) {
            clearInterval(pollLoop);
            producer.disconnect();
          }
        }, 10);

    });

}

/* Datadealer needs to be running for these tests to be successful
 % lein with-profile testjs run all
 Run suite of tests with
 % cd Farallones/api_service/ui_api
 % mocha --recursive
*/
// Sample data stream
describe('sse', function () {
    it('should sign in with correct credentials', function (done) {
        var data = { email: "uberuser@sensity.com", password: "ubeR$23" };
        request
            .post(version + '/login')
            .send(data)
            .set('Accept', 'application/json')
            .expect('Content-Type', /json/)
            .expect(200)
            .end(function (err, res) {
                should.not.exist(err);
                agent.saveCookies(res);
                csrfToken = helpers.getCsrfToken(res);
                done();
            });
    });
    it('should fail SSE without login credentials', function (done) {
        request
            .get('/streamv1/uberorg/ubersite/+/LoginReq')
            .set('Accept', 'application/json')
            .expect('Content-Type', /json/)
            .expect(403)
            .end(function (err, res) {
                should.not.exist(err);
                res.body.should.eql({ error: true, message: 'Please log in' });
                done();
            });
    });
    it('should fail SSE without valid orgid and siteid', function (done) {
        var reqsse = request.get('/streamv1/uberorgx/ubersitex/mochanode/LoginReq');
        agent.attachCookies(reqsse);
        reqsse
            .set('Accept', 'application/json')
            .expect('Content-Type', /json/)
            .expect(403)
            .end(function (err, res) {
                should.not.exist(err);
                res.body.should.eql({ error: true, message: 'Not allowed to access requested org' });
                request
                    .get('/streamv1/uberorg/ubersitex/+/LoginReq')
                    .set('Accept', 'application/json')
                    .expect('Content-Type', /json/)
                    .expect(403)
                    .end(function (err, res) {
                        should.not.exist(err);
                        res.body.should.eql({ error: true, message: 'Please log in' });
                        done();
                    });
            });
    });
    it('should create video node for sensor', function (done) {
        var createNodereq = request.put(version + '/nodes');
        agent.attachCookies(createNodereq);
        var payload = { csvNodeList: "nodeid,model,orgid,siteid,latitude,longitude\nmochanode,falcon-q,uberorg,ubersite,44.1,41.2" };
        createNodereq.send(payload)
            //.expect('Content-Type', /json/)
            .set('X-CSRF-Token', csrfToken)
            .expect(200)
            .end(function (err, res2) {
                should.not.exist(err);
                done();
            });
    });

    it('should create core node for sensor', function (done) {
        var createNodereq = request.put(version + '/nodes');
        agent.attachCookies(createNodereq);
        var payload = { csvNodeList: "nodeid,model,orgid,siteid,latitude,longitude\ncorenodemochanode,unode-v4,uberorg,ubersite,44.1,41.2" };
        createNodereq.send(payload)
            //.expect('Content-Type', /json/)
            .set('X-CSRF-Token', csrfToken)
            .expect(200)
            .end(function (err, res2) {
                should.not.exist(err);
                done();
            });
    });
    it('should assign a videonode using credentials', function (done) {
            var req = request.post(version + '/customers/uberorg/sites/ubersite/nodes/mochanode/assign');
            agent.attachCookies(req);
            req
                .set('Content-Type', 'application/json')
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    done();
                });
        });

        it('should assign a corenode using credentials', function (done) {
                    var req = request.post(version + '/customers/uberorg/sites/ubersite/nodes/corenodemochanode/assign');
                    agent.attachCookies(req);
                    req
                        .set('Content-Type', 'application/json')
                        .set('Accept', 'application/json')
                        .set('X-CSRF-Token', csrfToken)
                        .expect('Content-Type', /json/)
                        .expect(200)
                        .end(function (err, res) {
                            should.not.exist(err);
                            done();
                        });
                });

            /*
             * Sending CoreNode SensorSample message to RabbitMQ
             */
           it("should send SensorSample corenode data stream to RabbitMQ", function (done) {
                   var timer, total = 0;
                   var reqsse, skip_done = false;
                   var subscription = {
                       uri: configManager.get('sse').url,
                       topics: ['/streamv1/uberorg/ubersite/' + corenode_id + '/SensorSample']
                   };
                   // Create sse consumer
                   class SSEStream extends stream.Writable {
                       _write(chunk, enc, next) {
                           var str = chunk.toString();
                           if (str !== ("\n") && str !== (':' + '\n')) {
                               try {
                                   var response = JSON.parse(chunk.toString().substr(5));
                               } catch (e) {
                                   console.error(e)
                               }
                           }
                           next();
                       }
                   }
                   var writable = new SSEStream();
                   // Subscribe to sse
                   reqsse = request.get('/streamv1/uberorg/ubersite/+/SensorSample');
                   agent.attachCookies(reqsse);
                   reqsse.pipe(writable);
                   var mqttClient = mqtt.connect(subscription.uri);
                   mqttClient.on('connect', function () {
                       console.log('Connected to %s', subscription.uri);
                       for (var j in subscription.topics) {
                           mqttClient.subscribe(subscription.topics[j]);
                           console.log('Subscribed to %s', subscription.topics[j]);
                       }
                       timer = setTimeout(function () {
                           mqttClient.end();
                           done();
                       }, 500);
                       mqttClient.on('message', function (topic, message) {
                           try {
                               // message is Buffer
                               var response = JSON.parse(message.toString());
                               response.nodeid.should.eql("corenodemochanode");
                               console.log('New message', topic, response);
                               if (response.nodeid == corenode_id) {
                                   total++;
                               }
                           } catch (e) {
                               global.log.error('Error parsing received message: %s', e.message, e);
                           }
                       });
               var sensorSample = {
                                   name:'SensorSample',
                                   nodeid:'corenodemochanode',
                                   sensor:'p',
                                   units:'2',
                                   time:1515612128076938,
                                   value:1
                                 }
               var rabbitConfig = configManager.get('rabbit').url;
               amqp.connect(rabbitConfig, function(err, conn) {
                   if(err){
                       console.log("Rabbit MQ connection error: ", err);
                       return;
                   }
                       console.log('Connected to %s', rabbitConfig);
                       conn.createChannel(function(err, ch) {
                           if(err){
                               console.log("Rabbit MQ Channel creation error: ", err);
                                   return;
                           }
                            var exch = 'node.events';
                              ch.assertExchange(exch, 'topic', {
                                  durable: true
                              });
                              ch.publish(exch, '*.sensor.*', msgpack.encode(sensorSample), {
                                      persistent: true
                                  },
                           function(err){
                               if(err){
                                   console.log("Error posting sensor to RabbitMQ: ",err);
                                   return;
                               }
                               else{
                                   console.log(" Sensor posted to RabbitMQ exchange: ", sensorSample);
                               }
                           });
                       });
                       setTimeout(function() { conn.close(); }, 100);
                   });
               });
             });

       /*
        * Sending CoreNode DeviceAlarm message to RabbitMQ
        */
       it("should send DeviceAlarm core node data stream to RabbitMQ", function (done) {
          var timer, total = 0;
          var reqsse, skip_done = false;
          var subscription = {
              uri: configManager.get('sse').url,
              topics: ['/streamv1/uberorg/ubersite/' + corenode_id + '/DeviceAlarm']
          };
          // Create sse consumer
          class SSEStream extends stream.Writable {
              _write(chunk, enc, next) {
                  var str = chunk.toString();
                  if (str !== ("\n") && str !== (':' + '\n')) {
                      try {
                          var response = JSON.parse(chunk.toString().substr(5));
                      } catch (e) {
                          console.error(e)
                      }
                  }
                  next();
              }
          }
          var writable = new SSEStream();
          // Subscribe to sse
          reqsse = request.get('/streamv1/uberorg/ubersite/+/DeviceAlarm');
          agent.attachCookies(reqsse);
          reqsse.pipe(writable);
          var mqttClient = mqtt.connect(subscription.uri);
          mqttClient.on('connect', function () {
              console.log('Connected to %s', subscription.uri);
              for (var j in subscription.topics) {
                  mqttClient.subscribe(subscription.topics[j]);
                  console.log('Subscribed to %s', subscription.topics[j]);
              }
              timer = setTimeout(function () {
                  mqttClient.end();
                  done();
              }, 500);
              mqttClient.on('message', function (topic, message) {
                  try {
                      // message is Buffer
                      var response = JSON.parse(message.toString());
                      response.nodeid.should.eql("corenodemochanode");
                      console.log('New message', topic, response);
                      if (response.nodeid == corenode_id) {
                          total++;
                      }
                  } catch (e) {
                      global.log.error('Error parsing received message: %s', e.message, e);
                  }
              });
          var alarmSample = {
                       name:"DeviceAlarm",
                       nodeid:"corenodemochanode",
                       alarmType:"HWFail_RTC",
                       alarmSeverity:"Clear",
                       msg:"Hardware Failure"
                      }
          var rabbitConfig = configManager.get('rabbit').url;
          amqp.connect(rabbitConfig, function(err, conn) {
              if(err){
                  console.log("Rabbit MQ connection error: ", err);
                  return;
              }
              console.log('Connected to %s', rabbitConfig);
              conn.createChannel(function(err, ch) {
               if(err){
                   console.log("Rabbit MQ Channel creation error: ", err);
                       return;
               }
                var exch = 'node.events';
                 ch.assertExchange(exch, 'topic', {
                     durable: true
                 });
                 ch.publish(exch, '*.alarm.*', msgpack.encode(alarmSample), {
                         persistent: true
                     },
               function(err){
                   if(err){
                       console.log("Error posting alarm to RabbitMQ: ",err);
                       return;
                   }
                   else{
                       console.log(" Alarm posted to RabbitMQ exchange: ", alarmSample);
                   }
               });
             });
             setTimeout(function() { conn.close(); }, 100);
         });
      });
    });


       it("test send businessmessage to kafka ", function(done){
         var timer, total = 0;
                   var reqsse, skip_done = false;
                   var subscription = {
                       uri: configManager.get('sse').url,
                       topics: ['/streamv1/uberorg/ubersite/businessalert/parking/6fc6cc67-aaf7-4063-864f-c5467551ac45']
                   };
                   // Create sse consumer
                   class SSEStream extends stream.Writable {
                       _write(chunk, enc, next) {
                           var str = chunk.toString();
                           if (str !== ("\n") && str !== (':' + '\n')) {
                               try {
                                   var response = JSON.parse(chunk.toString().substr(5));
                               } catch (e) {
                                   console.error(e)
                               }
                           }
                           next();
                       }
                   }
                   var writable = new SSEStream();
                   // Subscribe to sse
                   reqsse = request.get('/streamv1/uberorg/ubersite/businessalert/parking/+');
                   agent.attachCookies(reqsse);
                   reqsse.pipe(writable);
                   var mqttClient = mqtt.connect(subscription.uri);
                   mqttClient.on('connect', function () {
                       console.log('Connected to %s', subscription.uri);
                       for (var j in subscription.topics) {
                           mqttClient.subscribe(subscription.topics[j]);
                           console.log('Subscribed to %s', subscription.topics[j]);
                       }
                       timer = setTimeout(function () {
                           mqttClient.end();
                           done();
                       }, 500);
                       mqttClient.on('message', function (topic, message) {
                           try {
                               // message is Buffer
                               var response = JSON.parse(message.toString());
                               response.application.should.eql("Parking");
                               console.log('New message', topic, response);
                               if (response.triggerId == triggerId ) {
                                   total++;
                               }
                           } catch (e) {
                               global.log.error('Error parsing received message: %s', e.message, e);
                           }
                       });

              var eventToKafka = { businessAlertId:'5fc6cc67-aaf7-4063-864f-c5467551ac46',
                                   triggerName:'average,occupancy,15,>,80',
                                   triggerId:'6fc6cc67-aaf7-4063-864f-c5467551ac45',
                                   triggerCategory:'parking',
                                   triggerSubcategory:'group',
                                   triggerUserId:'a8f6cc67-aaf7-4063-864f-c5467551ac45',
                                   siteid:'dbc377a2-7880-4087-92ab-966a53d4810a',
                                   orgid:'3881f299-2450-4f2c-a2d6-03635938b836',
                                   resourceId:'3881f299-2450-4f2c-a2d6-03635938b836',
                                   resourceName:'Marietta',
                                   active:true,
                                   createdOn:'2017-12-14T01:00:00.000Z',
                                   lastUpdated:'2018-01-12T10:00:00.000Z',
                                   lastClearedAt:'2018-01-16T11:00:00.000Z',
                                   lastClearedBy:'8991f299-2450-4f2c-a2d6-03635938b836',
                                   message:'The average occupancy percentage of the Marietta over 15 minutes is > 80%',
                                   severity:'Major'
                                 };
              sendMessageToKafka('businessalert',JSON.stringify(eventToKafka), function(err, res) {
                  setTimeout(function () {
                                             mqttClient.end();
                                             done();
                                         }, 500);
                                          mqttClient.on('message', function(mqttPath, message) {
                                          console.log(' message', message);
                                          eventToKafka.should.eqs.message
                                          done();
                   });
              });
          });
       });

    /*
     * LoginReq Data
     */
    it("should subscribe and receive login request data stream from mqtt broker", function (done) {
        var timer, total = 0;
        var reqsse, skip_done = false;
        var subscription = {
            uri: configManager.get('sse').url,
            topics: ['/streamv1/uberorg/ubersite/' + node_id + '/LoginReq']
        };
        // Create sse consumer
        class SSEStream extends stream.Writable {
            _write(chunk, enc, next) {
                var str = chunk.toString();
                if (str !== ("\n") && str !== (':' + '\n')) {
                    try {
                        var response = JSON.parse(chunk.toString().substr(5));
                    } catch (e) {
                        console.error(e)
                    }
                }
                next();
            }
        }
        var writable = new SSEStream();
        // Subscribe to sse
        reqsse = request.get('/streamv1/uberorg/ubersite/+/LoginReq');
        agent.attachCookies(reqsse);
        reqsse.pipe(writable);
        var mqttClient = mqtt.connect(subscription.uri);
        mqttClient.on('connect', function () {
            console.log('Connected to %s', subscription.uri);
            for (var j in subscription.topics) {
                mqttClient.subscribe(subscription.topics[j]);
                console.log('Subscribed to %s', subscription.topics[j]);
            }
            timer = setTimeout(function () {
                mqttClient.end();
                done();
            }, 500);
            mqttClient.on('message', function (topic, message) {
                try {
                    // message is Buffer
                    var response = JSON.parse(message.toString());
                    response.nodeid.should.eql("mochanode");
                    console.log('New message', topic, response);
                    if (response.nodeid == node_id) {
                        total++;
                    }
                } catch (e) {
                    global.log.error('Error parsing received message: %s', e.message, e);
                }
            });
            var loginReqPayload = {
                nid: 'mochanode',
                protoc: 1,
                dev: 'falcon-q',
                cid: '1',
                ssid: 'SensityDefault',
                profile: '',
                ch: 24,
                tok: 'ASFEF654213QWEQWEQWAFDS5632132465fq4w5e643241',
                ip: '127.0.0.1',
                t: 101010,
                bssid: 'AD:FF:BE:CB:AB:EF:BB:AA',
                mac: 'BA:FC:AF:BF:DA:FD:AE:CB',
                sec: 'WPA_PSK',
                subtype: '',
                modemRevEd: ''
            };
            var loginReq = {
                a: 'POST',
                p: 'v1/mochanode/out/POST/loginReq',
                sid: 'mochanode',
                d: '2017-10-10T20:34:01Z',
                uuid: '51e481b2-1f4e-4a6f-9b4e-b11c87b7d887',
                f: '',
                l: msgpack.encode(loginReqPayload)
            };
            setTimeout(function () { mqttClient.publish('v1/mochanode/out/POST/loginReq', msgpack.encode(loginReq)) }, 100);
        });
    });
           /*
            * SensorSample Data
            */
           it("should subscribe and receive sensor sample data stream from mqtt broker", function (done) {
               var timer, total = 0;
               var reqsse, skip_done = false;
               var subscription = {
                   uri: configManager.get('sse').url,
                   topics: ['/streamv1/uberorg/ubersite/' + node_id + '/SensorSample']
               };
               // Create sse consumer
               class SSEStream extends stream.Writable {
                   _write(chunk, enc, next) {
                       var str = chunk.toString();
                       if (str !== ("\n") && str !== (':' + '\n')) {
                           try {
                               var response = JSON.parse(chunk.toString().substr(5));
                           } catch (e) {
                               console.error(e)
                           }
                       }
                       next();
                   }
               }
               var writable = new SSEStream();
               // Subscribe to sse
               reqsse = request.get('/streamv1/uberorg/ubersite/+/SensorSample');
               agent.attachCookies(reqsse);
               reqsse.pipe(writable);
               var mqttClient = mqtt.connect(subscription.uri);


               mqttClient.on('connect', function () {

                   console.log('Connected to %s', subscription.uri);
                   for (var j in subscription.topics) {
                       mqttClient.subscribe(subscription.topics[j]);
                       console.log('Subscribed to %s', subscription.topics[j]);
                   }
                   timer = setTimeout(function () {
                       mqttClient.end();
                   done();
                   }, 500);
               mqttClient.on('message', function(topic, message) {
                       try{
                           // message is Buffer
                           var response = JSON.parse(message.toString());
                           response.nodeid.should.eql("mochanode");
                           console.log('New message', topic, response);
                           if( response.nodeid == node_id){
                               total++;
                           }
                       } catch(e){
                           global.log.error('Error parsing received message: %s', e.message, e);
                       }
                   });

                    var sensorSamplePayload = {
                        n: 'degC',
                        s: 'jt',
                        v: 123.11,
                        t: 1477674256293862
                    };
                    var sensorSample = {
                        a: 'POST',
                        p: 'v1/mochanode/out/UNSOL/sensors',
                        f: '',
                        sid: 'mochanode',
                        uuid: '51e481b2-1f4e-4a6f-9b4e-b11c87b7d887',
                        l: msgpack.encode(sensorSamplePayload)
                    };
                       setTimeout(function(){mqttClient.publish('v1/mochanode/out/UNSOL/sensors', msgpack.encode(sensorSample))},100);
               });
           });


        /*
         * DeviceAlarm Data
         */
        it("should subscribe and receive device alarm data stream from mqtt broker", function (done) {
            var timer, total = 0;
            var reqsse, skip_done = false;
            var subscription = {
                uri: configManager.get('sse').url,
                topics: ['/streamv1/uberorg/ubersite/' + node_id + '/DeviceAlarm']
            };
            // Create sse consumer
            class SSEStream extends stream.Writable {
                _write(chunk, enc, next) {
                    var str = chunk.toString();
                    if (str !== ("\n") && str !== (':' + '\n')) {
                        try {
                            var response = JSON.parse(chunk.toString().substr(5));
                        } catch (e) {
                            console.error(e)
                        }
                    }
                    next();
                }
            }
            var writable = new SSEStream();
            // Subscribe to sse
            reqsse = request.get('/streamv1/uberorg/ubersite/+/DeviceAlarm');
            agent.attachCookies(reqsse);
            reqsse.pipe(writable);
            var mqttClient = mqtt.connect(subscription.uri);
            mqttClient.on('connect', function () {
                console.log('Connected to %s', subscription.uri);
                for (var j in subscription.topics) {
                    mqttClient.subscribe(subscription.topics[j]);
                    console.log('Subscribed to %s', subscription.topics[j]);
                }
                timer = setTimeout(function () {
                    mqttClient.end();
                    done();
                }, 500);
                mqttClient.on('message', function (topic, message) {
                    try {
                        // message is Buffer
                        var response = JSON.parse(message.toString());
                        response.nodeid.should.eql("mochanode");
                        console.log('New message', topic, response);
                        if (response.nodeid == node_id) {
                            total++;
                        }
                    } catch (e) {
                        global.log.error('Error parsing received message: %s', e.message, e);
                    }
                });
                 var alarmPayload = {
                                     s: 0,
                                     a: 'SW/storage/volmissing/data',
                                     m: '/data volume missing',
                                     c: 3
                                 };
                 var alarm = {
                     uuid: '51e481b2-1f4e-4a6f-9b4e-111222333',
                     f: 'SW/storage/volmissing/data',
                     a: 'UNSOL',
                     l: msgpack.encode(alarmPayload),
                     p: 'v1/mochanode/out/UNSOL/alarm/{alarmid}',
                     sid: 'mochanode',
                     d: '2017-09-18T15:33:22Z'
                     };
                setTimeout(function () { mqttClient.publish('v1/mochanode/out/UNSOL/alarm', msgpack.encode(alarm)) }, 100);
            });
        });

    /*
     * ConnectionStatus Data
     */
    it("should subscribe and receive connection status data stream from mqtt broker", function (done) {
        var timer, total = 0;
        var reqsse, skip_done = false;
        var subscription = {
            uri: configManager.get('sse').url,
            topics: ['/streamv1/uberorg/ubersite/' + node_id + '/ConnectionStatus']
        };
        // Create sse consumer
        class SSEStream extends stream.Writable {
            _write(chunk, enc, next) {
                var str = chunk.toString();
                if (str !== ("\n") && str !== (':' + '\n')) {
                    try {
                        var response = JSON.parse(chunk.toString().substr(5));
                    } catch (e) {
                        console.error(e)
                    }
                }
                next();
            }
        }
        var writable = new SSEStream();
        // Subscribe to sse
        reqsse = request.get('/streamv1/uberorg/ubersite/+/ConnectionStatus');
        agent.attachCookies(reqsse);
        reqsse.pipe(writable);
        var mqttClient = mqtt.connect(subscription.uri);
        mqttClient.on('connect', function () {
            console.log('Connected to %s', subscription.uri);
            for (var j in subscription.topics) {
                mqttClient.subscribe(subscription.topics[j]);
                console.log('Subscribed to %s', subscription.topics[j]);
            }
            timer = setTimeout(function () {
                mqttClient.end();
                done();
            }, 500);
            mqttClient.on('message', function (topic, message) {
                try {
                    // message is Buffer
                    var response = JSON.parse(message.toString());
                    response.nodeid.should.eql("mochanode");
                    console.log('New message', topic, response);
                    if (response.nodeid == node_id) {
                        total++;
                    }
                } catch (e) {
                    global.log.error('Error parsing received message: %s', e.message, e);
                }
            });
            var connectionStatusPayload = {
            s: 0,
            a: "lwt",
            m: "mochanode_1_client_ext_disconnected",
            c: 1
            };
            var connectionStatus = {
            uuid: "f3328cb4-909c-4957-9b35-e481703ae3f6",
            f: "",
            a: "UNSOL",
            l: msgpack.encode(connectionStatusPayload),
            p: "v1/mochanode/out/UNSOL/lwt",
            sid: "mochanode",
            d: "2018-01-05T16:43:19Z"
            };
            setTimeout(function () { mqttClient.publish('v1/mochanode/out/UNSOL/lwt', msgpack.encode(connectionStatus)) }, 100);
        });
    });

        /*
         * GpsSample Data
         */
        it("should subscribe and receive gps sample data stream from mqtt broker", function (done) {
            var timer, total = 0;
            var reqsse, skip_done = false;
            var subscription = {
                uri: configManager.get('sse').url,
                topics: ['/streamv1/uberorg/ubersite/' + node_id + '/GpsSample']
            };
            // Create sse consumer
            class SSEStream extends stream.Writable {
                _write(chunk, enc, next) {
                    var str = chunk.toString();
                    if (str !== ("\n") && str !== (':' + '\n')) {
                        try {
                            var response = JSON.parse(chunk.toString().substr(5));
                        } catch (e) {
                            console.error(e)
                        }
                    }
                    next();
                }
            }
            var writable = new SSEStream();
            // Subscribe to sse
            reqsse = request.get('/streamv1/uberorg/ubersite/+/GpsSample');
            agent.attachCookies(reqsse);
            reqsse.pipe(writable);
            var mqttClient = mqtt.connect(subscription.uri);
            mqttClient.on('connect', function () {
                console.log('Connected to %s', subscription.uri);
                for (var j in subscription.topics) {
                    mqttClient.subscribe(subscription.topics[j]);
                    console.log('Subscribed to %s', subscription.topics[j]);
                }
                timer = setTimeout(function () {
                    mqttClient.end();
                done();
                }, 500);
            mqttClient.on('message', function(topic, message) {
                    try{
                        // message is Buffer
                        var response = JSON.parse(message.toString());
                        response.nodeid.should.eql("mochanode");
                        console.log('New message', topic, response);
                        if( response.nodeid == node_id){
                            total++;
                        }
                    } catch(e){
                        global.log.error('Error parsing received message: %s', e.message, e);
                    }
                 var gpsSample = {
                         nodeid:"falcon_q",
                         name:"GpsSample",
                         orgid:"uberorg",
                         siteid:"ubersite",
                         lat:42.614722,
                         lon:-71.324722345,
                         latUserAdded:42.23455876788,
                         lonUserAdded:-71.6888768812,
                         created:1520958011523,
                         updated:1520958601779
                 };
                    setTimeout(function(){mqttClient.publish('v1/mochanode/out/UNSOL/gps', msgpack.encode(gpsSample))},100);
            });
        });
     });

            /*
             * Sending CoreNode GpsSample message to RabbitMQ
             */
            it("should send GpsSample core node data stream to RabbitMQ", function (done) {
               var timer, total = 0;
               var reqsse, skip_done = false;
               var subscription = {
                   uri: configManager.get('sse').url,
                   topics: ['/streamv1/uberorg/ubersite/' + corenode_id + '/GpsSample']
               };
               // Create sse consumer
               class SSEStream extends stream.Writable {
                   _write(chunk, enc, next) {
                       var str = chunk.toString();
                       if (str !== ("\n") && str !== (':' + '\n')) {
                           try {
                               var response = JSON.parse(chunk.toString().substr(5));
                           } catch (e) {
                               console.error(e)
                           }
                       }
                       next();
                   }
               }
               var writable = new SSEStream();
               // Subscribe to sse
               reqsse = request.get('/streamv1/uberorg/ubersite/+/GpsSample');
               agent.attachCookies(reqsse);
               reqsse.pipe(writable);
               var mqttClient = mqtt.connect(subscription.uri);
               mqttClient.on('connect', function () {
                   console.log('Connected to %s', subscription.uri);
                   for (var j in subscription.topics) {
                       mqttClient.subscribe(subscription.topics[j]);
                       console.log('Subscribed to %s', subscription.topics[j]);
                   }
                   timer = setTimeout(function () {
                       mqttClient.end();
                       done();
                   }, 500);
                   mqttClient.on('message', function (topic, message) {
                       try {
                           // message is Buffer
                           var response = JSON.parse(message.toString());
                           response.nodeid.should.eql("corenodemochanode");
                           console.log('New message', topic, response);
                           if (response.nodeid == corenode_id) {
                               total++;
                           }
                       } catch (e) {
                           global.log.error('Error parsing received message: %s', e.message, e);
                       }
                   });
               var gpsSample = {
                            nodeid:"falcon_q",
                            name:"GpsSample",
                            orgid:"uberorg",
                            siteid:"ubersite",
                            lat:42.614722,
                            lon:-71.324722345,
                            latUserAdded:42.23455876788,
                            lonUserAdded:-71.6888768812,
                            created:1520958011523,
                            updated:1520958601779
                           }
               var rabbitConfig = configManager.get('rabbit').url;
               amqp.connect(rabbitConfig, function(err, conn) {
                   if(err){
                       console.log("Rabbit MQ connection error: ", err);
                       return;
                   }
                   console.log('Connected to %s', rabbitConfig);
                   conn.createChannel(function(err, ch) {
                    if(err){
                        console.log("Rabbit MQ Channel creation error: ", err);
                            return;
                    }
                     var exch = 'node.events';
                      ch.assertExchange(exch, 'topic', {
                          durable: true
                      });
                      ch.publish(exch, '*.gps.*', msgpack.encode(gpsSample), {
                              persistent: true
                          },
                    function(err){
                        if(err){
                            console.log("Error posting alarm to RabbitMQ: ",err);
                            return;
                        }
                        else{
                            console.log(" Alarm posted to RabbitMQ exchange: ", gpsSample);
                        }
                    });
                  });
                  setTimeout(function() { conn.close(); }, 100);
              });
           });
         });

    /*
     * wildcard (+) node and eventtype
     */
    it("should validate wildcard data stream for nodeid and eventtype", function (done) {
        var timer, total = 0;
        var reqsse, skip_done = false;
        var subscription = {
            uri: configManager.get('sse').url,
            topics: ['/streamv1/uberorg/ubersite/' + node_id + '/LoginReq']
        };
        // Create sse consumer
        class SSEStream extends stream.Writable {
            _write(chunk, enc, next) {
                var str = chunk.toString();
                if (str !== ("\n") && str !== (':' + '\n')) {
                    try {
                        var response = JSON.parse(chunk.toString().substr(5));
                    } catch (e) {
                        console.error(e)
                    }
                }
                next();
            }
        }
        var writable = new SSEStream();
        // Subscribe to sse
        reqsse = request.get('/streamv1/uberorg/ubersite/+/+');
        agent.attachCookies(reqsse);
        reqsse.pipe(writable);
        var mqttClient = mqtt.connect(subscription.uri);
        mqttClient.on('connect', function () {
            console.log('Connected to %s', subscription.uri);
            for (var j in subscription.topics) {
                mqttClient.subscribe(subscription.topics[j]);
                console.log('Subscribed to %s', subscription.topics[j]);
            }
            timer = setTimeout(function () {
                mqttClient.end();
                done();
            }, 500);
            mqttClient.on('message', function (topic, message) {
                try {
                    // message is Buffer
                    var response = JSON.parse(message.toString());
                    response.nodeid.should.eql("mochanode");
                    response.siteid.should.eql("ubersite");
                    response.orgid.should.eql("uberorg");
                    console.log('New message', topic, response);
                    if (response.nodeid == node_id) {
                        total++;
                    }
                } catch (e) {
                    global.log.error('Error parsing received message: %s', e.message, e);
                }
            });
            var loginReqPayload = {
                nid: 'mochanode',
                protoc: 1,
                dev: 'falcon-q',
                cid: '1',
                ssid: 'SensityDefault',
                profile: '',
                ch: 24,
                tok: 'ASFEF654213QWEQWEQWAFDS5632132465fq4w5e643241',
                ip: '127.0.0.1',
                t: 101010,
                bssid: 'AD:FF:BE:CB:AB:EF:BB:AA',
                mac: 'BA:FC:AF:BF:DA:FD:AE:CB',
                sec: 'WPA_PSK',
                subtype: '',
                modemRevEd: ''
            };
            var loginReq = {
                a: 'POST',
                p: 'v1/mochanode/out/POST/loginReq',
                sid: 'mochanode',
                d: '2017-10-10T20:34:01Z',
                uuid: '51e481b2-1f4e-4a6f-9b4e-b11c87b7d887',
                f: '',
                l: msgpack.encode(loginReqPayload)
            };
            setTimeout(function () { mqttClient.publish('v1/mochanode/out/POST/loginReq', msgpack.encode(loginReq)) }, 100);
        });
    });


    it("should not access login data for a invalid node and invalid eventtype", function (done) {
                  var req = request.get('/streamv1/uberorg/ubersite/' + node_id + '/LoginReq');
                  agent.attachCookies(req);
                  var filter = {
                  };
                  setTimeout(function () {
                      done();
                  }, 100);
                  req.send(filter)
                      .set('Accept', 'application/json')
                      .expect(200)
                      .end(function (err, res) {
                          should.not.exist(err);
                          var results = res.body;
                          results.length.should.eql(3);
                          results.nodeid.should.eql('mochanode');
                          results.siteid.should.eql('ubersite');
                          results.orgid.should.eql('uberorg');
                  // Try to filter by name
                  filter.type = "LoginReq";
                  var req = request.get('/streamv1/uberorg/ubersite/' + node_id + '/LoginReq');
                  agent.attachCookies(req);
                  req.send(filter)
                      .set('Accept', 'application/json')
                      .expect(200)
                      .end(function (err, res) {
                          should.not.exist(err);
                          var results = res.body;
                          results.length.should.eql(2);
                          results.type.should.eql('LoginReq');
                  // Try to filter by empty nodeid
                  filter.nodeid = "nnn";
                  delete filter.eventid;
                  var req = request.get('/streamv1/uberorg/ubersite/nnn/LoginReq');
                  agent.attachCookies(req);
                  req.send(filter)
                      .set('Accept', 'application/json')
                      .expect(404)
                      .end(function (err, res) {
                          should.not.exist(err);
                          var results = res.body;
                  // Try to filter by empty eventtype
                  filter.type = "aaa";
                  delete filter.nodeid;
                  var req = request.get('/streamv1/uberorg/ubersite/' + node_id + '/aaa');
                  agent.attachCookies(req);
                  req.send(filter)
                      .set('Accept', 'application/json')
                      .expect(404)
                      .end(function (err, res) {
                          should.not.exist(err);
                          var results = res.body;
                          done();
                                    });
                            });
                    });
            });
    });
    after(function (done) {
        // Delete Video node
        var deleteNodereq = request.delete(version + '/customers/uberorg/sites/ubersite/nodes/' + node_id);
        agent.attachCookies(deleteNodereq);
        deleteNodereq
            .set('Content-Type', 'application/json')
            .set('Accept', 'application/json')
            .set('X-CSRF-Token', csrfToken)
            .expect(204)
            .end(function (err, res) {
                console.log('Video Node Deleted');
                // Video node deleted successfully
                should.not.exist(err);
                done();
            });
    });
    after(function (done) {
            // Delete core node
            var deleteNodereq = request.delete(version + '/customers/uberorg/sites/ubersite/nodes/' + corenode_id);
            agent.attachCookies(deleteNodereq);
            deleteNodereq
                .set('Content-Type', 'application/json')
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect(204)
                .end(function (err, res) {
                    console.log('Core Node Deleted');
                    // Core node deleted successfully
                    should.not.exist(err);
                    done();
                });
        });
    });