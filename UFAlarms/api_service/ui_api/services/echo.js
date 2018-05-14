#!/usr/bin/env node
/**
 * Created by dejan on 3.4.17..
 */
var instance_id = 'Echo service - '+require('ip').address();
if(process.env.NODE_APP_INSTANCE){
    instance_id = 'Echo service ' + process.env.NODE_APP_INSTANCE + ' - '+require('ip').address();
}

var amqp = require('amqplib/callback_api');

amqp.connect('amqp://localhost', function(err, conn) {
    conn.createChannel(function(err, ch) {
        var q = 'ms.request.echo';

        ch.assertQueue(q, {durable: false});
        ch.prefetch(1);
        console.log('[%s] Awaiting RPC requests', instance_id);
        ch.consume(q, function reply(msg) {
            var payload = JSON.parse(msg.content.toString());
            if(!payload.data.trace)
                payload.data.trace = [];

            console.log("[%s] received message", instance_id, payload.data.message, payload.data.trace);

            payload.data.trace.push({
                id: instance_id,
                time: (new Date()).getTime(),
                log: ('['+instance_id+'] responded to request id = ' + msg.properties.correlationId)
            });

            ch.sendToQueue(msg.properties.replyTo,
                new Buffer(JSON.stringify(payload)),
                {correlationId: msg.properties.correlationId});

            ch.ack(msg);
        });
    });
});
