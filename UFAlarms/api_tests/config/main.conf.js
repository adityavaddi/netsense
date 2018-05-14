var config = {};

var ddservice = process.env.datadealer_service || 'localhost';
var ldapservice = process.env.ldap_service || 'localhost';
var ldapport = process.env.ldap_port || "1389";
var mqttservice = process.env.mosquitto_service || 'localhost';
var mqttport = process.env.mosquitto_port || "3002";
var amqpservice = process.env.rabbitmq_service || 'localhost';
var amqpport = process.env.rabbitmq_port || "5672";
var amqpurl = 'amqp://'+amqpservice;
var kafkaservice = process.env.kafka_service || 'localhost';
var kafkaport = process.env.kafka_port || "9092";
var kafkaurl = kafkaservice+':'+kafkaport;
var kafka_host = process.env.kafka_host || kafkaurl; // e.g 'localhost:9092,127.0.0.1:9093,192.168.1.0:9094'
//var amqpurl = 'amqp://farallones:sensity1@lb-aws-prod-01.sensity.com:5672';
var graphiteservice = process.env.graphite_service || 'localhost';
var cassandraservice = process.env.cassandra_service || '127.0.0.1';
var cassandra_keyspace = process.env.cassandra_keyspace || 'farallones';
var uiservice = process.env.reference_app_service || "localhost";
var instance_id = process.env.NODE_APP_INSTANCE;
/*if(!process.env.NODE_APP_INSTANCE){
    instance_id = 'IS-'+require('ip').address();
}*/

config.domain = {
    ui: uiservice+":8090" // ui root domain:port
};

config.datadealer = {};

config.datadealer.URL = {
    mpkrequester: 'tcp://'+ddservice+':5522',
    jsrequester: 'tcp://'+ddservice+':5521',
    jsrequester_act_log: 'tcp://'+ddservice+':5511'
};

config.mosca = {
    skip: true,
    host: mqttservice,
    port: mqttport
};

config.rabbit = {
    url:'amqp://farallones:sensity1@'+amqpservice+':'+amqpport,
 };

config.sse = {
    url:'mqtt://'+mqttservice+':'+mqttport,
 };

config.rabbit = {
      url:'amqp://farallones:sensity1@'+amqpservice+':'+amqpport,
   };

config.ldap = {
    URL: 'ldap://'+ldapservice+':'+ldapport, // OpenLDAP
    jsURL:'ldap://'+ldapservice+':'+ldapport,
    suffixUsers: 'ou=People,dc=sensity,dc=com',
    suffixApiKeys: 'ou=ApiKeys,dc=sensity,dc=com',
    adminDN: 'cn=admin,dc=sensity,dc=com',
    adminPass: 'admin123',
    //suffixUsers: 'o=users',
    //suffixApiKeys: 'o=apikeys',
    client: {
        url: 'ldap://'+ldapservice+':'+ldapport,
        //log: global.log,
        timeout: 3600000,
        idleTimeout: 600000,
        connectTimeout: 29000,
        reconnect : {
            initialDelay: 100,
            maxDelay: 10000,
            failAfter: Infinity
        }
    }
};

config.sendmail = {
    path: '/usr/sbin/sendmail',
    disabled: false
};

config.password_reset = {
    from: 'Sensity Admin <admin@sensity.com>', // sender address
    subject: 'Password reset', // Subject line
    text: 'Click here to reset password: [url]', // plaintext body
    html: 'Click <a href="[url]">here</a> to reset password' // html body
};

config.password_send = {
    from: 'Sensity Admin <admin@sensity.com>', // sender address
    subject: 'Your account is ready', // Subject line
    text: 'new-account.txt', // plaintext body
    html: 'new-account.html' // html body
};

config.api_key = {
    from: 'Sensity Admin <admin@sensity.com>', // sender address
    subject: 'Sensity API Key', // Subject line
    text: 'API Key: [api_key]', // plaintext body
    html: 'API Key: [api_key]' // html body

};

config.notifications = {
    subscriptions: [
        /*{   // Datadealer
            uri:'mqtt://127.0.0.1:3002',
            topics:['/streamv1/+/+/+/+']
        },*/
        {   // API service local
            uri:'mqtt://'+mqttservice+':'+mqttport,
            topics:['/test/alert', '/streamv1/+/+/+/DeviceAlarm']
        }
    ],
    sms:{
        disabled: false,
        accountSid: process.env.twilio_account_sid || 'AC56a71c802520fac74e8591a71d4a18cc',
        authToken: process.env.twilio_auth_token || '935a63397151b3eef26b7cc45120df2a',
        from: process.env.twilio_sender_phone_no ||'+14083421182', // sender address
        to: process.env.twilio_recipient_phone_no  || '+14083421182', // recipient address
        prefix: '', // Subject line
        suffix: '' // Subject line
    },
    email: {
        disabled: false,
        from: 'Sensity Admin <admin@sensity.com>', // sender address
        subject: 'Sensity notification', // Subject line
    }

};

config.acl = {
    disabled: false
};

config.graphite = {
    host: graphiteservice,
    port: 2003,
    reporting_interval: 555000,
    log_to_console: false
};

config.cassandra = {
    contactPoints: [cassandraservice],
    keyspace: cassandra_keyspace
}

config.rabbitmq = {
    apiservice: {
        uri: amqpurl,
        request_queue_prefix: 'ms.request.',
        reply_queue_prefix: 'api.reply.'
    },
    invalidation: {
        uri: amqpurl,
        queue: 'global-cache-invalidation'
    }
};

config.kafka = {
    kafkaurl: kafkaurl,
    instance_id: instance_id,
    topics: {
        'sensor': process.env.kafka_sensor_topic || 'ms.request.sensor',
        'config': process.env.kafka_config_topic || 'ms.request.config',
        'acl': process.env.kafka_acl_topic || 'acl',
        'reply': process.env.kafka_reply_topic || 'api.reply.interface',
        'sserequest': process.env.kafka_sse_topic || 'sseFilter'
    },
    apiservice: {
        request_queue_prefix: 'ms.request.',
        reply_queue_prefix: 'api.reply.'
    },
    client: {
        consumer: {
            clientId: instance_id + 'consumer',
            kafkaHost: kafka_host,
            connectTimeout: 10000,
            requestTimeout: 30000,
            autoConnect: true,
            idleConnection: 15 * 24 * 60 * 60 * 1000,
            connectRetryOptions: {
                retries: 5000000,
                factor: 2,
                minTimeout: 1 * 1000,
                maxTimeout: 5 * 1000,
                randomize: true
            }
        },
        producer: {
            clientId: instance_id + 'producer',
            kafkaHost: kafka_host,
            connectTimeout: 10000,
            requestTimeout: 30000,
            autoConnect: true,
            idleConnection: 15 * 24 * 60 * 60 * 1000,
            connectRetryOptions: {
                retries: 5000000,
                factor: 2,
                minTimeout: 1 * 1000,
                maxTimeout: 5 * 1000,
                randomize: true
            }
        },
        zkOptions: {
            sessionTimeout: 30000,
            spinDelay : 1000,
            retries : 0
        }
    },
    producer: {
        // Configuration for when to consider a message as acknowledged, default 1
        requireAcks: 1,
        // The amount of time in milliseconds to wait for all acks before considered, default 100ms
        ackTimeoutMs: 100,
        // Partitioner type (default = 0, random = 1, cyclic = 2, keyed = 3, custom = 4), default 2
        partitionerType: 1
    },
    consumer: {
        groupId: 'api.reply.'+instance_id,//consumer group id, default `kafka-node-group`
        // Auto commit config
        autoCommit: true,
        autoCommitIntervalMs: 5000,
        // The max wait time is the maximum amount of time in milliseconds to block waiting if insufficient data is available at the time the request is issued, default 100ms
        fetchMaxWaitMs: 100,
        // This is the minimum number of bytes of messages that must be available to give a response, default 1 byte
        fetchMinBytes: 1,
        // The maximum bytes to include in the message set for this partition. This helps bound the size of the response.
        fetchMaxBytes: 10 * 1024 * 1024,
        // If set true, consumer will fetch message from the given offset in the payloads
        fromOffset: 'earliest',
        // If set to 'buffer', values will be returned as raw buffer objects.
        encoding: 'utf8'
    }
};

module.exports = config;
