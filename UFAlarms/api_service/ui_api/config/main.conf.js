var config = {};

var ddservice = process.env.datadealer_service || 'localhost';
var ldapservice = process.env.ldap_service || 'localhost';
var ldapport = process.env.ldap_port || "1389";
var mqttservice = process.env.mosquitto_service || 'localhost';
var mqttport = process.env.mosquitto_port || "3002";
var kafkaservice = process.env.kafka_service || 'localhost';
var kafkaport = process.env.kafka_port || "9092";
var kafkaurl = kafkaservice+':'+kafkaport;
var kafka_host = process.env.kafka_host || kafkaurl; // e.g 'localhost:9092,127.0.0.1:9093,192.168.1.0:9094'
//var amqpurl = 'amqp://farallones:sensity1@lb-aws-prod-01.sensity.com:5672';
var graphiteservice = process.env.graphite_service || 'localhost';
var cassandraservice = process.env.cassandra_service || '127.0.0.1';
var uiservice = process.env.reference_app_service || "localhost";
var instance_id = process.env.NODE_APP_INSTANCE;
var uidomain = process.env.platform_public_url || 'nsn-local.sensity.com';

var isLogLevel = process.env.is_log_level || 'info';
var isLogThreshold = process.env.is_log_threshold || '50m'; //maximum size for a log file to reach before it's rotated. e.g, '100k', '1m', '2g' etc.
var isLogTotalFiles = process.env.is_log_total_files || 10; //maximum number of rotated files to keep
var isElkLogLevel = process.env.is_elk_log_level || 'debug';

if(!process.env.NODE_APP_INSTANCE){
    instance_id = 'IS-'+require('ip').address();
}

var env_suffix = process.env.env_suffix || instance_id;
var topic_suffix = process.env.env_suffix || "";

var reqTimeoutInSec = process.env.is_req_timeout_in_sec || '15'; // Should be less than HAProxy server timeout

config.log = {
    is_log_level: isLogLevel,
    is_log_threshold: isLogThreshold,
    is_log_total_files: isLogTotalFiles,
    elk_log_level: isElkLogLevel
}

config.request = {
    timeoutInSec: reqTimeoutInSec
}

config.domain = {
    ui: uidomain // ui root domain:port
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

config.sse = {
    url:'mqtt://'+mqttservice+':'+mqttport,
 };

config.ldap = {
    URL: 'ldap://'+ldapservice+':'+ldapport, // OpenLDAP
    jsURL:'ldap://'+ldapservice+':'+ldapport,
    suffixUsers: 'ou=People,dc=sensity,dc=com',
    suffixApiKeys: 'ou=ApiKeys,dc=sensity,dc=com',
    adminDN: 'cn=admin,dc=sensity,dc=com',
    adminPass: 'admin123',
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
    //suffixUsers: 'o=users',
    //suffixApiKeys: 'o=apikeys',
};

config.sendmail = {
    path: '/usr/sbin/sendmail',
    disabled: false
};

config.password_reset = {
    subject: 'Password reset', // Subject line
    text: 'Click here to reset password: [url]', // plaintext body
    html: 'Click <a href="[url]">here</a> to reset password' // html body
};

config.password_send = {
    subject: 'Your account is ready', // Subject line
    text: 'new-account.txt', // plaintext body
    html: 'new-account.html' // html body
};

config.api_key = {
    subject: 'NetSense API Key', // Subject line
    text: 'An API Key: [api_key] has been generated for user: [user_email]', // plaintext body
    html: 'An API Key: [api_key] has been generated for user: [user_email]' // html body
};

config.notifications = {
    subscriptions: [
        /*{   // Datadealer
            uri:'mqtt://127.0.0.1:3002',
            topics:['/streamv1/+/+/+/+']
        },*/
        {   // API service local
            uri:'mqtt://'+mqttservice+':'+mqttport,
            topics:['/test/alert', '/streamv1/+/+/+/DeviceAlarm', '/streamv1/+/+/businessalert/+/+']
        }
    ],
    awsKeys: {
        aws_access_key_id: process.env.aws_access_key_id || 'AKIAICTMTP7VMRZSHWZQ',
        aws_secret_access_key: process.env.aws_secret_access_key || '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',
        aws_region: process.env.aws_region || 'us-east-1'
    },
    sms: {
        disabled: false
    },
    email: {
        disabled: false,
        from: 'NetSense Admin <no-replay@sensity.com>', // sender address
        subject: 'NetSense notification' // Subject line
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

config.kafka = {
    instance_id: instance_id,
    topics: {
        'sensor': process.env.kafka_sensor_topic || ('ms.request.sensor' + topic_suffix),
        'config': process.env.kafka_config_topic || ('ms.request.config' + topic_suffix),
        'acl': process.env.kafka_acl_topic || ('ms.request.acl' + topic_suffix),
        'reply': process.env.kafka_reply_topic || ('api.reply.interface' + topic_suffix),
        'sserequest': process.env.kafka_sse_topic || ('ms.request.sse' + topic_suffix),
        'parkingmetadatarequest': process.env.kafka_parkingmetadata_topic || ('parking.policygroup.req' + topic_suffix),
        'parkingpolicytagrequest': process.env.kafka_parkingpolicytag_topic || ('parking.policy.req' + topic_suffix),
        'parkingtagrequest': process.env.kafka_parkingtag_topic || ('parking.tag.req' + topic_suffix),
        'alarmrequest': process.env.kafka_devicealaram_topic || ('ms.request.alert' + topic_suffix),
        'parkinguserdatarequest':  process.env.kafka_userdata_topic || ('parking.appuserdata.req' + topic_suffix),
        'media': process.env.kafka_media_topic || ('ms.request.media' + topic_suffix),
        'otarequest': process.env.kafka_ota_topic || ('ms.ota.api.req' + topic_suffix),
        'businessalertrequest': process.env.kafka_businessalert_topic || ('ms.business.alert.request' + topic_suffix),
        'lightcontrolrequest': process.env.kafka_lss_topic || ('ms.command.schedule' + topic_suffix),
        'businessalertsserequest': process.env.kafka_businessalertsse_topic || ('ms.businessrequest.sse' + topic_suffix),
        'triggerrequest': process.env.kafka_trigger_topic || ('ms.trigger.request' + topic_suffix),
        'gpsrequest': process.env.kafka_gps_topic || ('ms.request.gps' + topic_suffix),
        'whatifrequest': process.env.kafka_whatif_topic || ('ms.parking.whatif.request' + topic_suffix)
    },
    apiservice: {
        request_queue_prefix: 'ms.request.',
        reply_queue_prefix: 'api.reply.'
    },
    producer: {
        'client.id': 'interface-producer-' + env_suffix,
        'metadata.broker.list': kafka_host,
        'enable.auto.commit': true,
        // 'compression.codec': 'gzip',
        'retry.backoff.ms': 50,
        'message.send.max.retries': 100,
        'socket.keepalive.enable': true,
        'queue.buffering.max.messages': 100000,
        'queue.buffering.max.ms': 5,
        'batch.num.messages': 1000000,
        // 'debug': 'all',
        'topic.metadata.refresh.interval.ms': 10000,
        'auto.commit.interval.ms':5000,
        'heartbeat.interval.ms': 10000,
        // 'dr_cb': true
    },
    consumer: {
        'group.id': 'interface-consumer-' + env_suffix,
        'metadata.broker.list': kafka_host,
        'enable.auto.commit': true,
        'fetch.wait.max.ms': 5,
        'fetch.error.backoff.ms': 10,
        // 'debug': 'all',
        'socket.keepalive.enable': true,
        'auto.commit.interval.ms':5000,
        'heartbeat.interval.ms': 10000,
    }
};

config.isVideoNode = { "unode-v1": false, "unode-v2": false, "unode-v3": false, "unode-v4": false, "unode-v7": false, "ngcn": false, "cnext": false, "falcon-q": true, "merlin": true, "vdkmaster": true };
config.isNextGenCoreNode = { "cnext": true};

module.exports = config;
