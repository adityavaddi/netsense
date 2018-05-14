#!/usr/bin/env node
/**
 * Created by dejan on 3.4.17..
 */
var instance_id = 'Video mockup service - '+require('ip').address();
if(process.env.NODE_APP_INSTANCE){
    instance_id = 'Video mockup service ' + process.env.NODE_APP_INSTANCE + ' - '+require('ip').address();
}

var amqp = require('amqplib/callback_api');

var url = 'amqp://localhost';
//var url = 'amqp://farallones:sensity1@lb-aws-prod-01.sensity.com:5672';
var SampleConfigId = 'uberconfig';
var SampleNodeId = 'N2flcnqCONFIG';
var ExistingNodes = ['N03099480', SampleNodeId, 'ubernode']
var ExistingConfigs = {}
var NodeConfigs = {}

var counter = 0;
var i = setInterval(function(){
    // do your thing
    counter++;

    console.log(`Trying to connect to rabbitmq, take ${i}...`)
    connectedToRMQ();
    if(counter === 100) {
        clearInterval(i);
    }
}, 2000);

function connectedToRMQ(){

    amqp.connect(url, function(err, conn) {
        if(err)
            return console.log('Cant connect to rabbitmq', err);
        clearInterval(i);
        conn.createChannel(function(err, ch) {
            var q = 'ms.request.video';
            var exc_name = 'amq.topic';

            ch.assertQueue(q, {durable: false});

            ch.prefetch(1);
            console.log('[%s] Awaiting RPC requests', instance_id);
            ch.consume(q, function reply(msg) {
                var params = JSON.parse(msg.content.toString());

                console.log("[%s] received message", instance_id, params);

                var response ;
                switch(params.type){
                    case 'getConfig':
                        response = getConfig(params);
                        break;
                    case 'getAllConfigs':
                        response = getAllConfigs(params);
                        break;
                    case 'createConfig':
                        response = createConfig(params);
                        break;
                    case 'updateConfig':
                        response = updateConfig(params);
                        break;
                    case 'deleteConfig':
                        response = deleteConfig(params);
                        break;
                    case 'applyConfigToNode':
                    case 'applyConfigToNodes':
                        response = applyConfigToNodes(params);
                        break;
                    case 'getConfigFromNode':
                        response = getConfigFromNode(params);
                        break;
                    case 'getDefaultConfigsForSite':
                    case 'getDefaultConfigs':
                        response = getDefaultConfigs(params);
                        break;
                    case 'updateVPNInfo':
                    case 'connectToVPN':
                    case 'disconnectFromVPN':
                        response = handleVPNAPIReq(params);
                        break;
                    default:
                        response = {
                            error:true,
                            message: "Operation not implemented",
                            status: 501
                        };
                }

                ch.sendToQueue(msg.properties.replyTo,
                    new Buffer(JSON.stringify(response)),
                    {correlationId: msg.properties.correlationId});

                ch.ack(msg);
            });
        });
    });
}

/**
 * Mock createConfig
 * @param params
 * @returns {{model: string, data: [*]}}
 */
function createConfig(params) {
    //var nodeid = params.nodeprops.nodeid;
    var max = 1000, min=1
    var config = params.configprops.config;
    var configid = params.configprops.configid//'C-'+(Math.floor(Math.random() * (max - min)) + min)+'-'+Date.now();
    var name = params.configprops.name;
    var model = params.configprops.model;

    config.configid = configid;
    config.model = model;
    config.name = name;
    config.nodes= [ ];
    ExistingConfigs[configid] = config;
    return {
        model: 'ConfigModel',
        config:  config
    }
}

/**
 * Mock updateConfig
 * @param params
 * @returns {{model: string, data: [*]}}
 */
function updateConfig(params) {
    //var nodeid = params.nodeprops.nodeid;
    var config = params.configprops.config;
    var configid = params.configprops.configid;
    var name = params.configprops.name;
    var model = params.configprops.model;

    config.configid = configid;
    config.model = model;
    config.name = name;
    if(!ExistingConfigs[configid]){
        return {
            error:true,
            message: "Configuration not found",
            status: 404
        }
    }
    ExistingConfigs[configid] = config;
    return {
        model: 'ConfigModel',
        config:  config
    }
}

/**
 * Mock getConfigFromNode
 * @param params
 * @returns {{model: string, data: [*]}}
 */
function deleteConfig(params) {
    //var nodeid = params.nodeprops.nodeid;
    var configid = params.configprops.configid;
    //var config = params.configprops;

    if(!ExistingConfigs[configid]){
        return {
            error:true,
            message: "Configuration not found",
            status: 404
        }
    }
    delete ExistingConfigs[configid];
    return {status: 204, message: "Successfully deleted config", success: true}
}

/**
 * Mock getConfigFromNode
 * @param params
 * @returns {{model: string, data: [*]}}
 */
function applyConfigToNodes(params){
    var nodeid = params.nodeprops.nodeid;
    var configid = params.configprops.configid;
    
    if(!ExistingConfigs[configid]){
        return {
            error:true,
            message: "Configuration not found",
            status: 404
        }
    }

    if(ExistingNodes.indexOf(nodeid)===-1){
        return {
            error:true,
            message: "Node not found",
            status: 404
        }
    }

    //console.log(params);
    var cfg = JSON.parse(JSON.stringify(ExistingConfigs[configid]));
    if(!cfg.nodes[nodeid]){
        cfg.nodes.push({
            "name": nodeid,
            "model": "falcon-q",
            "nodeid": nodeid
        });
    }
    NodeConfigs[nodeid] = configid;
    return {
        "config": cfg,
        "nodes":cfg.nodes
    };
}
/**
 * Mock getConfigFromNode
 * @param params
 * @returns {{model: string, data: [*]}}
 */
function getConfigFromNode(params){
    var nodeid = params.nodeprops.nodeid;

    if(ExistingNodes.indexOf(nodeid)===-1){
        return {
            error:true,
            message: "Node not found",
            status: 404
        }
    }
    if(!NodeConfigs[nodeid]){
        return {
            error:true,
            message: "Node config not found",
            status: 404
        }
    }
    if(!ExistingConfigs[NodeConfigs[nodeid]]){
        return {
            error:true,
            message: "Config not found",
            status: 404
        }
    }

    return {
        model: 'ConfigModel',
        config: ExistingConfigs[NodeConfigs[nodeid]]
    }

}

/**
 * Mock getConfig
 * @param params
 * @returns {{model: string, data: [*]}}
 */
function getConfig(params){
    var configid = params.configprops.configid;
    //console.log(params);
    if(!ExistingConfigs[configid]){
        return {
            error:true,
            message: "Configuration not found",
            status: 404
        }
    }

    return {
        model:'ConfigModel',
        config:ExistingConfigs[configid]
    }

}

/**
 * Mock getConfig
 * @param params
 * @returns {{model: string, data: [*]}}
 */
function getDefaultConfigs(params){
    //console.log(params);
    var model = params.nodeprops.model;
    var siteid = params.siteprops?params.siteprops.siteid:null;
    var country_code = params.siteprops?params.siteprops.country_code:null;
    console.log(params, siteid, country_code, model);
    if(siteid && siteid!=='ubersite'){
        return {
            error:true,
            message: "Configuration not found",
            status: 404
        }
    }
    if(model!=='falcon-q'){
        return {
            error:true,
            message: "Model not found",
            status: 404
        }
    }
    var def = {
        "network.wlan-x.security.psk": "netsense",
        "vaevent.bucket_interval": "1",
        "network.wlan-x.security.bgscan-long-interval": "360",
        "server": "prod.sensity.com",
        "network.wlan-y.security.bgscan-signal-threshold": "-60",
        "sensor.p.pint": "3e8",
        "network.wlan-x.ipv6.method": "ignore",
        "network.server.mqtt": "mqtt.sensity.com",
        "network.wlan-x.security.bgscan-short-interval": "60",
        "application_server": "webrtc.sensity.com",
        "debugmode": "1",
        "medianode.commissioned": "false",
        "network.wlan-x.ssid": "SensityDefault",
        "network.server.vpn": "medianodevpn.sensity.com",
        "network.wlan-x.security": "wpa2p",
        "rtsp.service": "554",
        "network.firewall.protocols": "",
        "network.vpn_on_demand": "false",
        "vaevent.bucket_drop": "10",
        "network.wlan-x.security.bgscan-signal-threshold": "-60",
        "network.firewall.ports": "",
        "sensor.p.dint": "7530",
        "network.wlan-y.ipv6.method": "ignore",
        "network.wlan-y.security": "wpa2p",
        "sensor.p.mode": "2",
        "network.wlan-y.security.bgscan-long-interval": "360",
        "storage.maxsessions": "15",
        "application_server_port": "8443",
        "token": "0",
        "network.wlan-y.security.bgscan-short-interval": "60",
        "network.wlan-y.ssid": "XeraL2",
        "mediaserver.nomve": "false",
        "network.wlan-y.security.psk": "kentspeed",
        "network.region": "US",
        "mediaserver.wmm_qos": "true",
        "vaevent.bucket_depth": "100",
        "vaevent.qlen": "500",
        "aux_power": "true",
        "network.nowifi": "false"
    };
    if(country_code)
        def.country_code = country_code;

    console.log(def, siteid, country_code, model);

    return {
        model:'ConfigModel',
        config:def
    }

}

/**
 * Mock getAllConfigs
 * @param params
 * @returns {{model: string, data: [*]}}
 */
function getAllConfigs(params){
    var siteid = params.siteprops.siteid;
    //console.log(params);
    if(siteid!='ubersite'){
        return {
            error:true,
            message: "Configuration not found",
            status: 404
        }
    }
    const vals = Object.keys(ExistingConfigs).map(key => ExistingConfigs[key]);

    return {
        model:'ConfigModel',
        items: vals
    }

}

/**
 * Mock getConfigFromNode
 * @param params
 * @returns {{model: string, data: [*]}}
 */
function handleVPNAPIReq(params) {
    //var nodeid = params.nodeprops.nodeid;
    var nodeid = params.props.nodeid;
    //var config = params.configprops;

    if(ExistingNodes.indexOf(nodeid)===-1){
        console.log('Missing node', nodeid, ExistingNodes)
        return {
            error:true,
            message: "Node not found",
            status: 404
        }
    }
    //delete ExistingConfigs[configid];
    return {status: 204, message: params.type+" successfully completed", success: true}
}
