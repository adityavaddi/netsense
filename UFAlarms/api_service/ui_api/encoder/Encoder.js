"use strict";

//var encoder = require('./JSONEncoder.js').JSONEncoder;
var dsi = require('./../dsi/DataServiceInterface.js');

var mngr = {};

exports.JSONEncoder = mngr;
var async = require('async');
var _ = require('lodash');
var acl = require('./../auth/AclManager.js')
    .AclManager;

var configManager = require('kea-config');
configManager.init('./config/main.conf.js');

const alertsModel = require('./../api/models/alerts.js');
const notificationsModel = require('./../api/models/notifications.js');
const businessAlertsModel = require('./../api/models/businessalerts.js');
const gpsModel = require('./../api/models/gps.js');

const helpers = require('./../api/helpers/helpers.js')

// refer main.conf.js for the actual request topic name
var parkingMetadataReqTopicKey = "parkingmetadatarequest";
var parkingPolicyTagReqTopicKey = "parkingpolicytagrequest";
var parkingTagReqTopicKey = "parkingtagrequest";
var parkingUserDataKey = "parkinguserdatarequest";

const lightControlReqTopicKey = "lightcontrolrequest";
const schNodeModels = ["cnext"];
const nextGenCoreNode = 'cnext';

const parkPolicyUserIdKey = "userId",
  parkOptUserNameKey = "userName",
  userDataUserIdKey = "userid",
  triggerUserIdKey = "userId",
  businessAlertTriggerUserIdKey = "triggerUserId",
  businessAlertTriggerUserNameKey = "triggerUserName",
  lastClearedByUserIdKey = "lastClearedBy",
  lastClearedByUserNameKey = "lastClearedByUserName",
  whatIfCreatedByUserId = "createdByUserId",
  whatIfCreatedByUserName = "createdByUserName",
  whatIfLastUpdatedByUserId = "lastUpdatedByUserId",
  whatIfLastUpdatedByUserName = "lastUpdatedByUserName";

/**
 * Encode API Request
 * @param email
 * @param password
 * @param done
 * @returns {*}
 */
mngr.Encode = function(params, callback) {
    try {
        var userid = params.userid || (params['user'] && params['user']['user'] && params['user']['user']['userid']) || params.email;
        var result = JSON.parse(JSON.stringify(params));
        result.transactionID = new Date();
        result.requestid = params.requestid;
        // FIXME: Add the userid here as userprops in top level CASEL
        result['user'] = userid;
        global.log.info('mngr.Encode params', JSON.stringify(result));
        // TODO: Do some encoding here
        //switch(params.type){
        //    default:
        //        break;
        //};

        callback && callback(null, result);

    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

/**
 * Exec DS Request
 * @param email
 * @param password
 * @param done
 * @returns {*}
 */
mngr.Exec = function(user, params, done) {
    // TODO: Enable this when ACL is functional
    global.log.trace({
        user: user ? user.login.email : "not logged in",
        operation: params.type
    }, "in Encoder");
    if ( user ) { //Change this to !user to skip authentication
        acl.isAllowed(user.login.email || user, params, function(err, result){
            if(err){
                return done({
                    error: true,
                    message: err.message
                });
            }
            if(!result){
                return done({
                    error: true,
                    message: 'Access denied',
                    status: 403
                });
            }
            params.user = user;
            try {
                var result = {};
                // Encode API query
                mngr.Encode(params, function(err, result){
                    if(err){
                        try {
                            return done(err, null);
                        }
                        catch(e) {
                            global.log.error("Exception in callback: ", e.message, e.stack);
                            return done({ error: true, message: e.message, status: 500 });

                        }
                    }
                    //                global.log.info('mngr.ExecQuery', result);
                    //var user = cache.get('/user/' + email);
                    dsi.ExecQuery(result, function(err, data) {
                        // global.log.info('mngr.ExecQuery result', err, data);
                        if (err) {
                            try{
                                return done(err, null);
                            }
                            catch(e) {
                                global.log.error("Exception in callback: ", e.message, JSON.stringify(result), e.stack);
                                return done({ error: true, message: e.message, status: 500 });

                            }
                        } else if (!data) {
                            try {
                                return done(null, false);
                            }
                            catch(e) {
                                global.log.error("Exception in callback: ", e.message, JSON.stringify(result), e.stack);
                                return done({ error: true, message: e.message, status: 500 });
                            }
                        } else {
                          try {
                              return done(null, data);
                          }
                          catch(e) {
                              global.log.error("Exception in callback: ", e.message, JSON.stringify(result), e.stack);
                              return done({ error: true, message: e.message, status: 500 });
                          }
                        }
                    })
                });

            } catch (e) {
                try {
                    done({
                        error: true,
                        message: e.message
                    });
                }
                catch(e) {
                    global.log.error("Exception in callback: ", e.message, JSON.stringify(result), e.stack);
                    return done({ error: true, message: e.message, status: 500 });
                }
            }
        });
    } else {
        done({
            error: true,
            message: 'Access denied',
            status: 403
        });
    }
}

/**
 * Exec DS System Request. No access rights checked
 * @param email
 * @param password
 * @param done
 * @returns {*}
 */
mngr.SysExec = function(user, params, done) {
    try {
        var result = {};
        // Encode API query
        mngr.Encode(params, function(err, result){
            if(err){
                try {
                    return done(err, null);
                }
                catch(e) {
                    global.log.error("Exception in callback: ", e.message, JSON.stringify(result), e.stack);
                    return done({ error: true, message: e.message, status: 500 });
                }
            }
            dsi.SysExecQuery(result, function(err, data) {
                global.log.info('mngr.SysExecQuery result', err, data);
                try {
                    if (err) {
                        return done(err, null);
                    } else if (!data) {
                        return done(null, false);
                    } else {
                        return done(null, data);
                    }
                }
                catch(e) {
                    global.log.error("Exception in callback: ", e.message, JSON.stringify(result), e.stack);
                    return done({ error: true, message: e.message, status: 500 });
                }

            })
        });

    } catch (e) {
        global.log.error(e.message);
        done(null, false);
    }
};

/**
 * Demo
 * @param email
 * @param password
 * @param done
 * @returns {*}
 */
mngr.GetUser = function(email, password, done) {

    try {
        mngr.Exec(email, {
            'type': 'account',
            'where': {
                'email': 'eq ' + email
            }
        }, done);

    } catch (e) {
        done({
            error: true,
            message: e.message
        });
    }
}

/**
 * Demo
 * @param user
 * @param which
 * @param type
 * @param id
 * @param since
 * @param to
 * @param done
 * @returns {*}
 */
mngr.fetchHistoricalData = function(user, which, type, from, id, since, to, done) {
    var params = {
        'type': 'setLightLevel',
        'of': which,
        'from': from,
        'id': id,
        'since': since,
        'to': to
    };

    try {
        mngr.Exec(user, params, done);
    } catch (e) {
        done({
            error: true,
            message: e.message
        });
    }
}

/**
 * Demo
 * @param user
 * @param query
 * @param categories
 * @param done
 * @returns {*}
 */
mngr.autoComplete = function(user, query, categories, done) {
    var params = {
        type: 'autoComplete',
        user: user,
        autoprops: {
            query: query,
            categories: categories
        }
    };

    try {
        mngr.Exec(user, params, done);
    } catch (e) {
        done({
            error: true,
            message: e.message
        });
    }
}

mngr.assignFixture = function(userid, fixtureid, nodeid, siteid, groupid, callback) {

    var type = "assignFixtureToGroup";
    if (nodeid === null && groupid === null) {
        type = "assignFixtureToSite";
    } else if (groupid === null) {
        type = "assignFixtureToNode";
    }

    var params = {
        type: type,
        user: userid,
        model: 'NodeModel',
        action: 'CAN_ASSIGN_FIXTURE',
        fixtureprops: {
            fixtureid: fixtureid
        },
        siteprops: {
            siteid: siteid
        },
        groupprops: {
            groupids: groupid
        },
        nodeprops: {
            nodeid: nodeid
        }
    };

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                global.log.info("[assignFixture] " + JSON.stringify(response));
                callback(null, response.result);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.assignFixtureToNodes = function(userid, fixtureid, nodeids, siteid, groupid, callback) {

    var type = "assignFixtureToNodes";

    var params = {
        type: type,
        user: userid,
        model: 'NodeModel',
        action: 'CAN_ASSIGN_FIXTURE',
        fixtureprops: {
            fixtureid: fixtureid
        },
        siteprops: {
            siteid: siteid
        },
        nodeprops: {
            nodeids: nodeids
        }
    };

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                global.log.info("[assignFixtureToNodes] " + JSON.stringify(response));
                callback(null, response.result);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};


// Light Control
mngr.setLightLevelForDevice = function (params, callback) {
    const userid = params.user,
        nodeid = params.nodeprops.nodeid,
        reqid = params.requestid,
        orgid = params.orgprops.orgid,
        siteid = params.siteprops.siteid;
    // To find the node model
    // Send request to MS for SCH node and to DD for other core node
    mngr.getNode(reqid, userid, nodeid, orgid, siteid, function (err, resp) {
        
        if (err) {
            callback(err);
            return;
        } else if (resp && schNodeModels.indexOf(resp.model.toLowerCase()) !== -1) {
            global.log.info('SCH node found', nodeid);
            params.service = lightControlReqTopicKey;
            params.nodeprops.nodeids = Array.of(nodeid);
            delete params.nodeprops.nodeid;
        }

        try {
            mngr.Exec(userid, params, function (err, response) {
                if (!err) {
                    global.log.info("[lightLevelOverride] for device " + JSON.stringify(response));
                    callback(null, response.response);
                } else {
                    callback(err)
                }
            });
        } catch (e) {
            callback({
                error: true,
                message: e.message
            });
        }
    });
};

mngr.setLightLevelOnSite = function (params, callback) {
    const userid = params.user,
        siteid = params.siteprops.siteid;
    // Get node ids of all SCH node (with model 'cnext') for a site
    mngr.getAllNodeIdsForModelSite(userid, siteid, schNodeModels, function (err, response) {
        setLightLevelOnSiteOrGroup(err, response, params, callback);
    });

};

mngr.setLightLevelInGroup = function (params, callback) {
    const userid = params.user,
        siteid = params.siteprops.siteid,
        groupid = params.groupprops.groupid;
    // Get node ids of all SCH node (with model 'cnext') for a group
    mngr.getAllNodeIdsForModelGroup(userid, siteid, groupid, schNodeModels, function (err, response) {
        setLightLevelOnSiteOrGroup(err, response, params, callback);
    });
};

// Common function used by setLightLevelOnSite and setLightLevelInGroup
function setLightLevelOnSiteOrGroup(err, response, params, callback) {

    // Get SCH node ids
    let schNodeIds = [];
    if (err) {
        callback(err);
    } else if (response) {
        (typeof response === "string") ? schNodeIds.push(response) : schNodeIds = response;
        // Mix calls
        global.log.info('SCH node found', schNodeIds);
    }

    const userid = params.user;
    // Sends request to DD and MS
    let responses = 0,
        errors = 0,
        accepted = [],
        rejected = [];
    // Collect responses
    let onResponse = function (err, resp) {
        if (err) {
            errors++;
            rejected.push(err);
        }
        else {
            if (resp.response) {
                accepted = accepted.concat(resp.response);
            }
        }
        responses++;
        if (responses > 0 && accepted.length > 0) {
            // We should have at least accepted response
            callback(null, accepted);
        } else if (responses === 2 && errors === 2) {
            // Both returned error => return error
            const message = (rejected[0].status < rejected[1].status) ? rejected[1].message : rejected[0].message;
            callback({ error: true, message: message, status: Math.max(rejected[0].status, rejected[1].status) });
        }
    }
    try {
        // Send request to DD
        mngr.Exec(userid, JSON.parse(JSON.stringify(params)), onResponse);

        if (schNodeIds.length > 0) {
            // Send request to MS only when SCH nodes are available
            params.service = lightControlReqTopicKey;
            params.nodeprops.nodeids = schNodeIds;
            mngr.Exec(userid, params, onResponse);
        }
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.setScheduleForDevice = function(userid, sch, nodeid, siteid, orgid, callback) {
    var params = {
        type: 'lighting-control',
        model: 'NodeModel',
        action: 'CAN_OPERATE',
        user: userid,
        scheduleprops: sch,
        nodeprops: {
            type: 'LightingScheduledEvent',
            nodeid: nodeid,
            level: 100
        },
        siteprops: {
            siteid: siteid
        },
        orgprops: {
            orgid: orgid
        }
    };
    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                global.log.log("[setScheduleForDevice] " + JSON.stringify(response));
                callback(null, response.response);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    };
};

/**
 * Demo
 * @param done function Callback
 * @returns {*}
 */
mngr.fetchUser = function(user, email, done) {

    try {
        mngr.SysExec(user, {
            type: 'getUserPermissions',
            email: email,
        }, function(err, response) {
            if (!err) {
                done(null, response.data)
            } else {
                done(err)
            }
        });
    } catch (e) {
        done({
            error: true,
            message: e.message
        });
    }
};

/**
 * Demo
 * @param done function Callback
 * @returns {*}
 */
mngr.fetchAllCustomerSiteEdges = function(user, done) {
    var exp = 'true';

    try {
        mngr.SysExec(user, {
            'type': 'fetchAllCustomerSiteEdges',
            'where': exp,
            'emulate': true,
            //'user': user,
            //'level': level,
            //'cypher': cq
        }, done);
    } catch (e) {
        done({
            error: true,
            message: e.message
        });
    }
}

mngr.getActivityLogs = function(user, requestid, datemin, datemax, orgid, siteid, callback) {
    var params = {
        type: 'getActivityLogs',
        model: 'AuditModel',
        action: 'CAN_READ',
        user: user,
        requestid: requestid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        extprops: {
            datemin: datemin,
            datemax: datemax
        }
    }

    try {
        mngr.Exec(user, params, function(err, response) {
            if (!err) {
                callback(null, response.logs)
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}

mngr.logUserActivities = function(userid, when, targetid, targettype, activity, message, callback) {
    var params = {
        type: 'logActivity',
        userid: userid,
        logtype: 'user',
        logprops: {
            when: when,
            targetid: targetid,
            targettype: targettype,
            activity: activity,
            message: message
        }
    }

    try {
        mngr.SysExec(userid, params, callback);
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}

mngr.logLoginActivities = function(userid, ip, when, targetid, targettype, activity, message, callback) {
    var params = {
        type: 'logActivity',
        userid: userid,
        logtype: 'login',
        logprops: {
            ip: ip,
            when: when,
            targetid: targetid,
            targettype: targettype,
            activity: activity,
            message: message
        }
    }

    try {
        mngr.SysExec(userid, params, callback);
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}

// Nodes
mngr.getAllConfigs = function(reqid, userid, orgid, siteid, callback) {
    var params = {
        requestid: reqid,
        type: 'getAllConfigs',
        model: 'ConfigModel',
        action: 'CAN_READ',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        }
    };

    // Sends request to DD and MS
    // TODO: Try to find model info in cache
    var responses = 0,
        errors = 0,
        accepted = [],
        rejected = [];
    // Collect responses
    var onResponse = function(err, response){
        if(err){
            errors++;
            rejected.push(err);
        }
        else {
            if(response.items!=undefined)
            accepted = accepted.concat(response.items);
        }
        responses++;
        if(responses===2){
            if(errors===2){
                // Both returned error => return error
                var message = (rejected[0].status < rejected[1].status)?rejected[1].message:rejected[0].message;
                callback({error:true, message: message, status: Math.max(rejected[0].status, rejected[1].status)});
            } else {
                // We should have at least accepted response
                callback(null, accepted);
            }
        }
    }
    try {
        // Send request to DD
        mngr.Exec(userid, JSON.parse(JSON.stringify(params)), onResponse);
        // Send request to MS
        params.service = 'config';
        mngr.getAllNodeIdsForModelSite(userid, siteid, ['falcon-q', 'unode-v7', 'merlin', 'vdkmaster', nextGenCoreNode], function(err, response){
            params.nodeprops = {nodeids: response}
            //console.log('getAllNodeIdsForModelSite', JSON.stringify(err), JSON.stringify(response));
            mngr.Exec(userid, params, onResponse);
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }

};

mngr.getDefaultConfigs = function(reqid, userid, model, callback) {
    var params = {
        requestid: reqid,
        type: 'getDefaultConfigs',
        model: 'ConfigModel',
        action: 'CAN_READ',
        user: userid,
        nodeprops: {
            model: model
        }
    };

    if(configManager.get('isVideoNode')[model] || configManager.get('isNextGenCoreNode')[model]) {
        params.service = 'config';
    }

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.config);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.getDefaultConfigsForSite = function(reqid, userid, orgid, siteid, model, callback) {
    var params = {
        requestid: reqid,
        type: 'getDefaultConfigsForSite',
        model: 'ConfigModel',
        action: 'CAN_READ',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        nodeprops: {
            model: model
        }
    };

    var onResponse = function(err, response){
        if (!err) {
            callback(null, response.config);
        } else {
            callback(err)
        }
    }

    if(configManager.get('isVideoNode')[model] || configManager.get('isNextGenCoreNode')[model]) {

        params.service = 'config';
        try {
            // Get Site country code country_code
            mngr.getSite(userid, siteid, function(err, response){
                params.siteprops.country_code = response.country_code;
                mngr.Exec(userid, params, onResponse);
            });
        } catch (e) {
            callback({
                error: true,
                message: e.message
            });
        }
    } else {
        try {
            mngr.Exec(userid, params, onResponse);
        } catch (e) {
            callback({
                error: true,
                message: e.message
            });
        }

    }

};

mngr.getConfig = function(reqid, userid, orgid, siteid, configid, callback) {
    var params = {
        requestid: reqid,
        type: 'getConfig',
        model: 'ConfigModel',
        action: 'CAN_READ',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        configprops: {
            configid: configid
        }
    };

    // Sends request to DD and MS
    // TODO: Try to find model info in cache
    var responses = 0,
        errors = 0,
        accepted = null,
        rejected = [];
    // Collect responses
    var onResponse = function(err, response){
        if(err){
            errors++;
            rejected.push(err);
        }
        else
            accepted = response.config;
        if(accepted) {
          // We should have 1 accepted response
          callback(null, accepted);
        } else {
          responses++;
          if(responses===2){
              if(errors===2){
                  // Both returned error => return error
                  var message = (rejected[0].status < rejected[1].status)?rejected[1].message:rejected[0].message;
                  callback({error:true, message: message, status: Math.max(rejected[0].status, rejected[1].status)});
              } else if (!accepted) {
                  // TODO: Is it an error?
                  var message = (rejected[0])? rejected[0].message : 'Could not get config';
                  var status = (rejected[0])? rejected[0].status : 400;
                  callback({error:true, message: message, status: status});
              }
          }
        }
    }
    try {
        // Send request to DD
        mngr.Exec(userid, JSON.parse(JSON.stringify(params)), onResponse);
        // Send request to MS
        params.service = 'config';
        mngr.Exec(userid, params, onResponse);
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.getConfigFromNode = function(reqid, userid, orgid, siteid, nodeid, type, callback) {
    var params = {
        requestid: reqid,
        type: 'getConfigFromNode',
        model: 'ConfigModel',
        action: 'CAN_READ',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        nodeprops: {
            nodeid: nodeid
        },
        configprops: {
            type: type
        },
        service: 'config',
    };
    // Video node only
    mngr.getAllNodeIdsForModelSite(userid, siteid, ['falcon-q', 'unode-v7', 'merlin', 'vdkmaster', nextGenCoreNode], function(err, response){
        //params.nodeprops = {nodeids: response}
        //console.log('getAllNodeIdsForModelSite', JSON.stringify(err), JSON.stringify(response));
        //mngr.Exec(userid, params, onResponse);
        if( err ) {
            callback(err);
        } else if( !response || !response || !response.length){
            return  callback({error: true, message: 'Video node supported only', statuus: 400});
        } else if( response && response.indexOf(nodeid)===-1 ){
            return  callback({error: true, message: 'Video node supported only', statuus: 400});
        } else {
            try {
                mngr.Exec(userid, params, function(err, response) {
                    if (!err) {
                        if(response && response.config)
                            callback(null, response.config);
                        else
                            callback({error: true, message: "Config not found", status: 404}, null);
                    } else {
                        callback(err)
                    }
                });
            } catch (e) {
                callback({
                    error: true,
                    message: e.message
                });
            }
        }
    });
};

mngr.addConfig = function(reqid, userid, orgid, siteid, config, callback) {
    var params = {
        requestid: reqid,
        type: 'createConfig',
        model: 'ConfigModel',
        action: 'CAN_CREATE',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        configprops: config
    };
    if(configManager.get('isVideoNode')[config.model] || configManager.get('isNextGenCoreNode')[config.model]) {
        params.service = 'config';
        var name = config.name;
        var model = config.model;
        delete config.model;
        var configid = config.configid;
        delete config.configid;
        params.configprops = {
            configid: configid,
            name: name,
            model: model,
            config: config
        }
    }

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.config);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.updateConfig = function(reqid, userid, orgid, siteid, config, callback) {
    var params = {
        requestid: reqid,
        type: 'updateConfig',
        model: 'ConfigModel',
        action: 'CAN_UPDATE',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        configprops: config
    };
    if(configManager.get('isVideoNode')[config.model] || configManager.get('isNextGenCoreNode')[config.model]) {
        params.service = 'config';
        var name = config.name;
        var model = config.model;
        delete config.model;
        var configid = config.configid;
        delete config.configid;
        params.configprops = {
            configid: configid,
            name: name,
            model: model,
            config: config
        }
    }

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.config);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.deleteConfig = function(reqid, userid, orgid, siteid, configid, callback) {
  mngr.getConfig(reqid, userid, orgid, siteid, configid, function(err,resp) {
      if(err) {
          callback(err);
          return;
      }

      var params = {
          requestid: reqid,
          type: 'deleteConfig',
          model: 'ConfigModel',
          action: 'CAN_DEACTIVATE',
          user: userid,
          orgprops: {
              orgid: orgid
          },
          siteprops: {
              siteid: siteid
          },
          configprops: {
              configid: configid
          }
      };
      try {
        if(configManager.get('isVideoNode')[resp.model] || configManager.get('isNextGenCoreNode')[resp.model]) {
            params.service = 'config';
            mngr.Exec(userid, params, function(err,response) {callback(err,response);});
        } else {
            mngr.Exec(userid, JSON.parse(JSON.stringify(params)),function(err,response) {callback(err,response);});
        }
      } catch (e) {
          callback({
              error: true,
              message: e.message
          });
      }
    });
};

mngr.createEmptyNode = function(user, nodeprops, callback) {
    var params = {
        type: 'createEmptyNode',
        model: 'NodeModel',
        action: 'CAN_CREATE',
        user: user,
        nodeprops: nodeprops
    }
    try {
        mngr.Exec(user, params, function(err, response) {
            if (!err) {
                callback(null, response.node);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}

mngr.createBulkNode = function(user, nodes, extprops, callback) {
    var params = {
        type: 'createBulkNode',
        model: 'NodeModel',
        action: 'CAN_CREATE',
        user: user,
        nodeprops: {
            nodes: nodes
        },
        extprops: extprops
    };
    try {
        mngr.Exec(user, params, function(err, response) {
            if (!err) {
                callback(null, response.nodeids);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.createNode = function(user, nodeprops, siteid, orgid, callback) {
    var params = {
        type: 'createNode',
        model: 'NodeModel',
        action: 'CAN_CREATE',
        user: user,
        nodeprops: nodeprops,
        siteprops: {
            siteid: siteid
        },
        orgprops: {
            orgid: orgid
        }
    }
    try {
        mngr.Exec(user, params, function(err, response) {
            if (!err) {
                callback(null, response.node);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}

mngr.assignNodeToParkingGroup = function(user, parkinggroupid, siteid, nodeid, callback) {
    var params = {
        type: 'assignNodeToParkingGroup',
        model: 'ParkingZoneModel',
        action: 'CAN_CREATE',
        user: user,
        nodeprops: {
            nodeid: nodeid
        },
        siteprops: {
            siteid: siteid
        },
        parkinggroupprops: {
            parkinggroupid: parkinggroupid
        }
    }
    try {
        mngr.Exec(user, params, function(err, response) {
            if (!err) {
                callback(null, response.node);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}

mngr.assignNode = function(user, nodeid, siteid, orgid, callback) {
    var params = {
        type: 'assignNode',
        model: 'NodeModel',
        action: 'CAN_CREATE',
        user: user,
        nodeprops: {
            nodeid: nodeid
        },
        siteprops: {
            siteid: siteid
        },
        orgprops: {
            orgid: orgid
        }
    }
    try {
        mngr.Exec(user, params, function(err, response) {
            if (!err) {
                callback(null, response.node);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}

mngr.commandNode = function(reqid, user, orgid, siteid, nodeid, command, callback) {
    var params = {
        requestid: reqid,
        type: 'commandNode',
        model: 'NodeModel',
        action: 'CAN_ASSIGN_TO_SITES',
        user: user,
        nodeprops: {
            nodeid: nodeid,
            type: "DeviceActionReq",
            cmd: command
        },
        siteprops: {
            siteid: siteid
        },
        orgprops: {
            orgid: orgid
        }
    }

    mngr.getAllNodeIdsForModelSite(user, siteid, ['falcon-q', 'unode-v7', 'merlin', 'vdkmaster', nextGenCoreNode], function(err, response){
        if( err ) {
            console.log('Error:', err);
            callback(err);
        } else if( response ){
            var vnodeids = nodeid.filter(function(n) {
                return response.indexOf(n) !== -1;
            });

            if (vnodeids.length > 0) {
                console.log('Video node found', vnodeids);
                params.nodeprops.nodeid = vnodeids;
                params.service = 'config';
            }
            else {
                params.nodeprops.nodeid = nodeid;
            }
        }
        else {
            console.log('Response:', response);
        }
        try {
            mngr.Exec(user, params, function(err, response) {
                if (!err) {
                    callback(null, response.result);
                } else {
                    callback(err)
                }
            });
        } catch (e) {
            callback({
                error: true,
                message: e.message
            });
        }
    });
}

mngr.getNode = function (reqid, userid, nodeid, orgid, siteid, callback) {
    const params = {
        type: 'getNode',
        model: 'NodeModel',
        action: 'CAN_READ',
        user: userid,
        nodeprops: {
            nodeid: nodeid
        }
    }
    // Sends request to DD and MS
    let responses = 0,
        errors = 0,
        rejected = [],
        node = null,
        gps = null,
        timer = null;
    // Collect responses
    let onResponse = function (err, response) {
        if (err) {
            errors++;
            rejected.push(err);
        }
        else {
            if (response.node != undefined) {
                node = JSON.parse(JSON.stringify(response.node));
                // empty lat, lon from DD as to use only GPS return values
                if(node.latitude) node.latitude = "";
                if(node.longitude) node.longitude = "";
                // To send back only DD response if gps-service is not responding in 10sec
                timer = setInterval(function () {
                    clearTimeout(timer);
                    if (responses === 1 && node != null) {
                        global.log.info('Not received response from GPS-Service in like 10sec, so considering only DD response');
                        callback(null, node);
                    }
                }, 10000);
            } else if (response.result != undefined && response.result && Object.keys(response.result).length > 0) {
                gps = response.result;
            }
        }
        responses++;
        if (responses === 2) {
            clearTimeout(timer);
            if (errors === 2) {
                // Both returned error => return error
                var message = (rejected[0].status < rejected[1].status) ? rejected[1].message : rejected[0].message;
                callback({ error: true, message: message, status: Math.max(rejected[0].status, rejected[1].status) });
            } else if (gps == null && node != null) {
                // Send DD response (with empty lat, lon) if GPS response is empty
                callback(null, node);
            } else if(!node && errors === 1) {
                callback({ error: true, message: rejected[0].message, status: rejected[0].status});
            } else {
                if(gps){
                    // Get lat, long from GPS response and add it to DD response
                    if (gps.latuseradded) node.latitude = gps.latuseradded;
                    if (gps.lonuseradded) node.longitude = gps.lonuseradded;
                    if (gps.latitude) node.latitude_gps = gps.latitude;
                    if (gps.longitude) node.longitude_gps = gps.longitude;
                }
                callback(null, node);
            }
        }
    }
    try {
        // Send request to DD
        mngr.Exec(userid, JSON.parse(JSON.stringify(params)), onResponse);

        // Send request to MS
        const gpsParams = gpsModel.getGpsByNodeIdReq(reqid, userid, orgid, siteid, nodeid);
        mngr.Exec(userid, gpsParams, onResponse);
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}

mngr.getSensorHistoryFromTo = function(reqid, userid, orgid, siteid, nodeid, sensorid, date1, date2, limit, period, callback) {
    var params = {
        requestid: reqid,
        type: 'getSensorHistoryFromTo',
        model: 'NodeModel',
        action: 'CAN_READ',
        user: userid,
        nodeprops: {
            nodeid: nodeid
        },
        siteprops: {
            siteid: siteid
        },
        orgprops: {
            orgid: orgid
        },
        extprops: {
            sensorid: sensorid,
            date1: date1,
            date2: date2,
            limit: limit,
            period: period
        },
        service: 'sensor'
    }

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.items);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}

mngr.getSiteSensorHistoryFromTo = function(reqid, userid, orgid, siteid, sensorid, date1, date2, limit, period, callback) {
    var params = {
        requestid: reqid,
        type: 'getSensorHistoryFromTo',
        model: 'NodeModel',
        action: 'CAN_READ',
        user: userid,
        nodeprops: {
            nodeid: 'all'
        },
        siteprops: {
            siteid: siteid
        },
        orgprops: {
            orgid: orgid
        },
        extprops: {
            sensorid: sensorid,
            date1: date1,
            date2: date2,
            limit: limit,
            period: period
        },
        service: 'sensor'
    }

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.items);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}

mngr.getSensorHistory = function(reqid, userid, orgid, siteid, nodeid, sensorid, date, limit, callback) {
    var params = {
        requestid: reqid,
        type: 'getSensorHistory',
        model: 'NodeModel',
        action: 'CAN_READ',
        user: userid,
        nodeprops: {
            nodeid: nodeid
        },
        siteprops: {
            siteid: siteid
        },
        orgprops: {
            orgid: orgid
        },
        extprops: {
            sensorid: sensorid,
            date: date,
            limit: limit
        },
        service: 'sensor'
    }

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.items);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}

mngr.getNodeConnectionStatus = function(userid, siteid, nodeid, callback) {
    var params = {
        type: 'getNodeConnectionStatus',
        model: 'NodeModel',
        action: 'CAN_READ',
        user: userid,
        nodeprops: {
            nodeid: nodeid
        },
        siteprops: {
            siteid: siteid
        },
    }

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.status);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.getSiteNodesStatuses = function (reqid, userid, orgid, siteid, callback) {
    var type = siteid !== "_nosite_" ? "getSiteNodesStatuses" : "getLostAndFoundNodesStatuses";
    var action = siteid !== "_nosite_" ? "CAN_READ" : "CAN_CREATE";
    var params = {
        type: type,
        model: 'NodeModel',
        action: action,
        user: userid,
        siteprops: {
            siteid: siteid
        }
    }
    
    let getAlertsFromMS = false,
        nodeStatuses = null,
        responses = 0;
    // Collect DD & MS responses
    let onResponse = function(err, response) {
        if (err) {
            if (nodeStatuses) callback(null, nodeStatuses);
            else callback(err); // Send back error when DD returns error
        } else {
            responses++;
            if (response.status) nodeStatuses = JSON.parse(JSON.stringify(response.status));
            if (nodeStatuses && !getAlertsFromMS) {
                callback(null, response.status);
            }
            if (responses === 2 && nodeStatuses) {
                let alertsArray = JSON.parse(JSON.stringify(response.result));
                for (let status of nodeStatuses) {
                    // Iterate alertsArray in reverse to avoid element skip after splice
                    for(let i = alertsArray.length - 1; i >= 0; --i) {
                        const alert = alertsArray[i];
                        if (status.nodeid === alert.nodeid) {
                            // Add only required attributes of alert to node_status
                            const alertObj = {
                                alertid: alert.alertid,
                                type: alert.type,
                                severity: alert.severity
                            }
                            status.alerts.push(alertObj);
                            alertsArray.splice(i, 1); // remove item from alertsArray after nodeid match
                        }
                    }
                }
                callback(null, nodeStatuses);
            }
        }
    }

    try {
        // Send request to DD
        mngr.Exec(userid, JSON.parse(JSON.stringify(params)), onResponse);

        // Send request to MS (for alerts) only when type is getSiteNodesStatuses
        if (type === 'getSiteNodesStatuses') {
            getAlertsFromMS = true;
            const userType = helpers.getUserType(userid); // here userid is actually req.getCurrentUser()
            const getAlertsForSiteParams = alertsModel.getAllAlertsReq(reqid, userid, orgid, siteid, userType);
            mngr.Exec(userid, getAlertsForSiteParams, onResponse);
        }
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.getNodeLightStatus = function(userid, siteid, nodeid, callback) {
    var params = {
        type: 'getNodeLightStatus',
        model: 'NodeModel',
        action: 'CAN_READ',
        user: userid,
        nodeprops: {
            nodeid: nodeid
        },
        siteprops: {
            siteid: siteid
        }
    }

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.status);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.getParkingInfoSite = function(userid, siteid, filter, callback) {
    var params = {
        type: 'getParkingInfoSite',
        model: 'ParkingZoneModel',
        action: 'CAN_READ',
        user: userid,
        siteprops: {
            siteid: siteid
        },
        extprops: filter
    }

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.status);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.getTrafficConfig = function(userid, siteid, nodeid, filter, callback) {
    var params = {
        type: 'getTrafficConfig',
        model: 'TrafficObjectModel',
        action: 'CAN_READ',
        user: userid,
        siteprops: {
            siteid: siteid
        },
        extprops: filter
    }
    if(nodeid && nodeid!='all'){
        params['nodeprops'] = {
            nodeid: nodeid
        }
    }

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.data);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.getCurrentTrafficInfo = function(userid, siteid, filter, callback) {
    var params = {
        type: 'getCurrentTrafficInfo',
        model: 'TrafficObjectModel',
        action: 'CAN_READ',
        user: userid,
        siteprops: {
            siteid: siteid
        },
        extprops: filter
    }

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.data);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.getTrafficHistory = function(userid, siteid, filter, callback) {
    var params = {
        type: 'getTrafficHistory',
        model: 'TrafficObjectModel',
        action: 'CAN_READ',
        user: userid,
        siteprops: {
            siteid: siteid
        },
        extprops: filter
    }

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.data);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};


mngr.bulkConfigNodeWifi = function(userid, orgid, siteid, wifi, callback) {
    var nodeList = wifi.nodeList;
    delete wifi.nodeList;
    var params = {
        type: 'updateNodeWifi',
        model: 'NodeModel',
        action: 'CAN_CHANGE',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        nodeprops: {
            nodeList: nodeList
        },
        extprops: wifi
    };
    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.wifi);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.activateNode = function(userid, siteid, nodeid, act, callback) {
    var type = act ? 'activateNode' : 'deactivateNode';
    var action = 'CAN_DEACTIVATE';
    var params = {
        type: type,
        model: 'NodeModel',
        action: action,
        user: userid,
        nodeprops: {
            nodeid: nodeid
        },
        siteprops: {
            siteid: siteid
        }
    }

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.node);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.deleteNode = function(userid, nodeid, siteid, orgid, callback) {
    var params = {
        type: 'deleteNode',
        model: 'NodeModel',
        action: 'CAN_DEACTIVATE',
        user: userid,
        nodeprops: {
            nodeid: nodeid
        },
        siteprops: {
            siteid: siteid
        },
        orgprops: {
            orgid: orgid
        }
    }

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}

mngr.realDeleteNode = function(userid, nodeid, siteid, orgid, callback) {
    var params = {
        type: 'realDeleteNode',
        model: 'NodeModel',
        action: 'CAN_DELETE',
        user: userid,
        nodeprops: {
            nodeid: nodeid
        },
        siteprops: {
            siteid: siteid
        },
        orgprops: {
            orgid: orgid
        }
    }

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}

mngr.getAllNodes = function(reqid, userid, orgid, siteid, callback) {
    const type = siteid !== "_nosite_" ? "getAllNodesForSite" : "getAllLostAndFoundNodes";
    const action = siteid !== "_nosite_" ? "CAN_READ" : "CAN_CREATE";

    getGpsForAllNodesOrAllMinNodes(type, action, reqid, userid, orgid, siteid, callback);
}

mngr.getAllMinNodes = function(reqid, userid, orgid, siteid, callback) {
    const type = "getAllMinNodesForSite";
    const action = "CAN_READ";

    getGpsForAllNodesOrAllMinNodes(type, action, reqid, userid, orgid, siteid, callback);
}

// Common function used by getAllNodes and getAllMinNodes
function getGpsForAllNodesOrAllMinNodes(type, action, reqid, userid, orgid, siteid, callback) {
    const params = {
        type: type,
        model: 'NodeModel',
        action: action,
        user: userid,
        siteprops: {
            siteid: siteid
        }
    }
    // Sends request to DD and MS
    let responses = 0,
        errors = 0,
        rejected = [],
        nodes = [],
        gpsResponse = [],
        timer = null;
    // Collect responses
    let onResponse = function (err, response) {
        if (err) {
            errors++;
            rejected.push(err);
        }
        else {
            if (response.items != undefined) {
                nodes = JSON.parse(JSON.stringify(response.items));
                // empty lat, lon from DD as to use only GPS return values
                for(let node of nodes) {
                    if(node.latitude) node.latitude = "";
                    if(node.longitude) node.longitude = "";
                }
                // To send back only DD response if gps-service is not responding in 10sec
                timer = setInterval(function () {
                    clearTimeout(timer);
                    if (responses === 1 && nodes != null) {
                        global.log.info('Not received response from GPS-Service in like 10sec, so considering only DD response');
                        callback(null, nodes);
                    }
                }, 10000);
            } else if (response.result != undefined) {
                gpsResponse = response.result;
            }
        }
        responses++;
        if (responses === 2) {
            clearTimeout(timer);
            if (errors === 2) {
                // Both returned error => return error
                var message = (rejected[0].status < rejected[1].status) ? rejected[1].message : rejected[0].message;
                callback({ error: true, message: message, status: Math.max(rejected[0].status, rejected[1].status) });
            } else if (gpsResponse.length === 0) {
                // Send DD response (with empty lat, lon) if GPS response is empty
                callback(null, nodes);
            } else if(!nodes.length && errors === 1) {
                callback({ error: true, message: rejected[0].message, status: rejected[0].status});
            } else {
                const gpsArray = JSON.parse(JSON.stringify(gpsResponse));                
                // Get lat, long from GPS response and add it to DD response
                if(gpsArray) { // check if gpsResponse is null
                    for(let node of nodes){
                        // Iterate alertsArray in reverse to avoid element skip after splice
                        for(let i = gpsArray.length - 1; i >= 0; --i) {
                            const gps = gpsArray[i];
                            if (node.nodeid === gps.nodeid) {
                                if (gps.latuseradded) node.latitude = gps.latuseradded;
                                if (gps.lonuseradded) node.longitude = gps.lonuseradded;
                                if (gps.latitude) node.latitude_gps = gps.latitude;
                                if (gps.longitude) node.longitude_gps = gps.longitude;
                                gpsArray.splice(i, 1); // remove item from gpsArray after nodeid match 
                            }
                        }
                    }
                }
                callback(null, nodes);
            }
        }
    }
    try {
        // Send request to DD
        mngr.Exec(userid, JSON.parse(JSON.stringify(params)), onResponse);

        // Send request to MS
        const gpsParams = gpsModel.getGpsForOrgIdAndSiteIdReq(reqid, userid, orgid, siteid);
        mngr.Exec(userid, gpsParams, onResponse);
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}

mngr.getAllNodeIdsForModelSite = function(userid, siteid, model, callback) {
    var models = (model.constructor === Array)? model : [model];
    var type = "getAllNodeIdsForModelSite";
    var params = {
        type: type,
        model: 'NodeModel',
        action: 'CAN_READ',
        user: userid,
        siteprops: {
            siteid: siteid
        },
        nodeprops: {
            model: models
        }
    };

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.items);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}

mngr.getAllNodeIdsForModelGroup = function(userid, siteid, groupid, model, callback) {
    var type = "getAllNodeIdsForModelGroup";
    var params = {
        type: type,
        model: 'NodeModel',
        action: 'CAN_READ',
        user: userid,
        siteprops: {
            siteid: siteid
        },
        nodeprops: {
            groupid: groupid,
            model: model
        }
    }

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.items);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}

// Orgs
mngr.getOrg = function(userid, orgid, callback) {
    var params = {
        type: 'getOrg',
        model: 'OrgModel',
        action: 'CAN_READ',
        user: userid,
        orgprops: {
            orgid: orgid
        }
    };
    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.org);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.getAllOrgs = function(user, callback) {
    var params = {
        type: 'getAllOrgs',
        model: 'OrgModel',
        action: 'CAN_READ',
        user: user
    };
    try {
        mngr.Exec(user, params, function(err, response) {
            if (!err) {
                callback(null, response.items);
            } else {
                callback(err);
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.getAllSuspendedOrgs = function(user, callback) {
    var params = {
        type: 'getAllSuspendedOrgs',
        model: 'OrgModel',
        action: 'CAN_SUSPEND',
        user: user
    };
    try {
        mngr.Exec(user, params, function(err, response) {
            if (!err) {
                callback(null, response.items);
            } else {
                callback(err);
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.createOrg = function(user, orgprops, callback) {
    var model = (orgprops.type === 'partner') ? 'PartnerModel' : 'OrgModel';
    var params = {
        type: 'createOrg',
        model: model,
        action: 'CAN_CREATE',
        user: user,
        orgprops: orgprops
    };
    try {
        mngr.Exec(user, params, function(err, response) {
            if (!err) {
                callback(null, response.org);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.updateOrg = function(user, org, callback) {
    var model = (org.type === 'partner') ? 'PartnerModel' : 'OrgModel';
    var action = (org.type === 'partner') ? 'CAN_UPDATE' : 'CAN_CHANGE';
    var params = {
        type: 'updateOrg',
        model: model,
        action: action,
        user: user,
        orgprops: org
    };
    try {
        mngr.Exec(user, params, function(err, response) {
            if (!err) {
                callback(null, response.org);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.deleteOrg = function(user, orgid, callback) {
    var params = {
        type: 'deleteOrg',
        model: 'OrgModel',
        action: 'CAN_DELETE',
        user: user,
        orgprops: {
            orgid: orgid
        }
    };
    try {
        mngr.Exec(user, params, function(err, response) {
            if (!err) {
                callback(null, response.org);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.suspendOrg = function(user, orgid, callback) {
    var params = {
        type: 'suspendOrg',
        model: 'OrgModel',
        action: 'CAN_SUSPEND',
        user: user,
        orgprops: {
            orgid: orgid
        }
    };
    try {
        mngr.Exec(user, params, function(err, response) {
            if (!err) {
                callback(null, response.org);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.activateOrg = function(user, orgid, callback) {
    var params = {
        type: 'activateOrg',
        model: 'OrgModel',
        action: 'CAN_SUSPEND',
        user: user,
        orgprops: {
            orgid: orgid
        }
    };
    try {
        mngr.Exec(user, params, function(err, response) {
            if (!err) {
                callback(null, response.org);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

// Sites
mngr.createSite = function(user, siteprops, orgid, callback) {
    var params = {
        type: 'createSite',
        model: 'SiteModel',
        action: 'CAN_CREATE',
        user: user,
        siteprops: siteprops,
        orgprops: {
            orgid: orgid
        }
    }
    try {
        mngr.Exec(user, params, function(err, response) {
            if (!err) {
                callback(null, response.site);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}

mngr.getSite = function(userid, siteid, callback) {
    var params = {
        type: 'getSite',
        model: 'SiteModel',
        action: 'CAN_READ',
        user: userid,
        siteprops: {
            siteid: siteid
        }
    }

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.site);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}

mngr.deleteSite = function(user, siteid, orgid, callback) {
    var params = {
        type: 'deleteSite',
        model: 'SiteModel',
        action: 'CAN_DELETE',
        user: user,
        siteprops: {
            siteid: siteid
        },
        orgprops: {
            orgid: orgid
        }
    }
    try {
        mngr.Exec(user, params, function(err, response) {
            if (!err) {
                callback(null, response);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}

mngr.suspendSite = function(user, siteid, orgid, callback) {
    var params = {
        type: 'suspendSite',
        model: 'SiteModel',
        action: 'CAN_SUSPEND',
        user: user,
        siteprops: {
            siteid: siteid
        },
        orgprops: {
            orgid: orgid
        }
    }
    try {
        mngr.Exec(user, params, function(err, response) {
            if (!err) {
                callback(null, response);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}

mngr.activateSite = function(user, siteid, orgid, callback) {
    var params = {
        type: 'activateSite',
        model: 'SiteModel',
        action: 'CAN_SUSPEND',
        user: user,
        siteprops: {
            siteid: siteid
        },
        orgprops: {
            orgid: orgid
        }
    }
    try {
        mngr.Exec(user, params, function(err, response) {
            if (!err) {
                callback(null, response);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}

mngr.getAllSites = function(user, orgid, callback) {
    var params = {
        type: 'getAllSitesForOrg',
        model: 'SiteModel',
        action: 'CAN_READ',
        user: user,
        orgprops: {
            orgid: orgid
        }
    }

    try {
        mngr.Exec(user, params, function(err, response) {
            if (!err) {
                callback(null, response.items);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}

mngr.getAllSuspendedSites = function(user, orgid, callback) {
    var params = {
        type: 'getAllSuspendedSitesForOrg',
        model: 'SiteModel',
        action: 'CAN_READ',
        user: user,
        orgprops: {
            orgid: orgid
        }
    }

    try {
        mngr.Exec(user, params, function(err, response) {
            if (!err) {
                callback(null, response.items);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}

// User
//mngr.createUser = function(user, userprops, callback) {
//    var params = {
//        type: 'createUser',
//        model: 'UserModel',
//        action: 'CAN_CREATE_USER',
//        user: user,
//        userprops: userprops
//    }
//    try {
//        mngr.Exec(user, params, function(err, response) {
//             if(!err){
//                 callback(null, response.user);
//             } else {
//                 callback(err)
//             }
//        });
//    } catch (e) {
//        callback({error: true, message: e.message});
//    }
//}

mngr.updateSite = function(user, site, orgid, callback) {
    var params = {
        type: 'updateSite',
        model: 'SiteModel',
        action: 'CAN_CHANGE',
        user: user,
        siteprops: site,
        orgprops: {
            orgid: orgid
        }
    };
    try {
        mngr.Exec(user, params, function(err, response) {
            if (!err) {
                callback(null, response.site);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}

/**
 * Get all notifications for site by name
 * Used for SMS/email feature. Refer /notifications/manager.js
 * @param name
 * @param siteid
 * @param callback
 */
mngr.getAllNotificationsByName = function (userid, name, orgid, siteid, callback) {
    const params = notificationsModel.getAllNotificationsByNameReq(userid, name, orgid, siteid);
    try {
        mngr.SysExec('root', params, function (err, response) {
            if (!err) {
                callback(null, response.result);
            } else {
                callback(err);
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}

/**
 * Get notification by notification ID
 * Used for SMS/email feature. Refer /notifications/manager.js
 * @param {*} userid
 * @param {*} notificationid
 * @param {*} siteid
 * @param {*} callback
 */
mngr.getNotificationSys = function (userid, notificationid, orgid, siteid, callback) {
    const params = notificationsModel.getNotificationSysReq(userid, notificationid, orgid, siteid);
    try {
        mngr.SysExec('root', params, function (err, response) {
            if (!err) {
                callback(null, response.result);
            } else {
                callback(err);
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}

// Parking zones
mngr.getAllParkingZones = function(userid, siteid, callback) {
    var params = {
        type: 'getAllParkingZones',
        model: 'ParkingZoneModel',
        action: 'CAN_READ',
        user: userid,
        siteprops: {
            siteid: siteid
        }
    }

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, (response && response.items) || []);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}
mngr.getParkingZones = function(userid, siteid, callback) {
    var params = {
        type: 'getParkingZones',
        model: 'ParkingZoneModel',
        action: 'CAN_READ',
        user: userid,
        siteprops: {
            siteid: siteid
        }
    }

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.status);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}
mngr.getOneParkingZone = function(userid, siteid, parkingzoneid, callback) {
    var params = {
        type: 'getOneParkingZone',
        model: 'ParkingZoneModel',
        action: 'CAN_READ',
        user: userid,
        siteprops: {
            siteid: siteid
        },
        parkingzoneprops: {
            parkingzoneid: parkingzoneid
        }
    }

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.status);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}
mngr.deleteParkingZone = function(userid, parkingzoneid, siteid, callback) {
    var params = {
        type: 'deleteParkingZone',
        model: 'ParkingZoneModel',
        action: 'CAN_DELETE',
        user: userid,
        parkingzoneprops: {
            parkingzoneid: parkingzoneid
        },
        siteprops: {
            siteid: siteid
        }
    };

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};
mngr.createParkingZone = function(userid, parkingzone, siteid, callback) {
    var params = {
        type: 'createParkingZone',
        model: 'ParkingZoneModel',
        action: 'CAN_CREATE',
        user: userid,
        parkingzoneprops: parkingzone,
        siteprops: {
            siteid: siteid
        }
    }

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.parkingzone);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}
mngr.updateParkingZone = function(userid, parkingzone, parkingzoneid, siteid, callback) {
    parkingzone.parkingzoneid = parkingzoneid;
    var params = {
        type: 'updateParkingZone',
        model: 'ParkingZoneModel',
        action: 'CAN_UPDATE',
        user: userid,
        parkingzoneprops: parkingzone,
        siteprops: {
            siteid: siteid
        }
    }

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.parkingzone);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}

// Parking spots
mngr.getAllParkingSpots = function(userid, siteid, callback) {
    var params = {
        type: 'getAllParkingSpots',
        model: 'ParkingZoneModel',
        action: 'CAN_READ',
        user: userid,
        siteprops: {
            siteid: siteid
        }
    }

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, (response && response.items) || []);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}
mngr.getParkingSpot = function(userid, parkingspotid, callback) {
    var params = {
        type: 'getParkingSpot',
        model: 'ParkingZoneModel',
        action: 'CAN_READ',
        user: userid,
        parkingspotprops: {
            parkingspotid: parkingspotid
        }
    }

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.parkingspot);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}
mngr.deleteParkingSpot = function(userid, parkingzoneid, siteid, callback) {
    var params = {
        type: 'deleteParkingSpot',
        model: 'ParkingZoneModel',
        action: 'CAN_DELETE',
        user: userid,
        parkingspotprops: {
            parkingspotid: parkingspotid
        },
        siteprops: {
            siteid: siteid
        }
    };

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};
mngr.createParkingSpot = function(userid, siteid, parkingzoneid, parkingspot, callback) {
    var params = {
        type: 'createParkingSpot',
        model: 'ParkingZoneModel',
        action: 'CAN_CREATE',
        user: userid,
        parkingspotprops: parkingspot,
        parkingzoneprops: {
            parkingzoneid: parkingzoneid
        },
        siteprops: {
            siteid: siteid
        }
    }

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.parkingspot);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}
mngr.updateParkingSpot = function(userid, siteid, parkingzoneid, parkingspot, callback) {
    parkingspot.parkingspotid = parkingspotid;
    var params = {
        type: 'updateParkingSpot',
        model: 'ParkingZoneModel',
        action: 'CAN_UPDATE',
        user: userid,
        parkingspotprops: parkingspot,
        parkingzoneprops: {
            parkingzoneid: parkingzoneid
        },
        siteprops: {
            siteid: siteid
        }
    }

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.parkingspot);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}

// Parking groups
mngr.getAllParkingGroups = function(userid, siteid, callback) {
    var params = {
        type: 'getAllParkingGroups',
        model: 'ParkingZoneModel',
        action: 'CAN_READ',
        user: userid,
        siteprops: {
            siteid: siteid
        }
    }

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, (response && response.items) || []);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}
mngr.getParkingGroup = function(userid, parkinggroupid, siteid, callback) {
    var params = {
        type: 'getParkingGroup',
        model: 'ParkingZoneModel',
        action: 'CAN_READ',
        user: userid,
        parkinggroupprops: {
            parkinggroupid: parkinggroupid
        },
        siteprops: {
            siteid: siteid
        }
    }

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.parkinggroup);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}
mngr.deleteParkingGroup = function(userid, siteid, parkinggroupid, callback) {
    var params = {
        type: 'deleteParkingGroup',
        model: 'ParkingZoneModel',
        action: 'CAN_DELETE',
        user: userid,
        parkinggroupprops: {
            parkinggroupid: parkinggroupid
        },
        siteprops: {
            siteid: siteid
        }
    };

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};
mngr.createParkingGroup = function(userid, siteid, parkinggroup, callback) {
    var params = {
        type: 'createParkingGroup',
        model: 'ParkingZoneModel',
        action: 'CAN_CREATE',
        user: userid,
        parkinggroupprops: parkinggroup,
        siteprops: {
            siteid: siteid
        }
    }

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.parkinggroup);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}
mngr.updateParkingGroup = function(userid, siteid, parkinggroupid, parkinggroup, callback) {
    parkinggroup.parkinggroupid = parkinggroupid;
    var params = {
        type: 'updateParkingGroup',
        model: 'ParkingZoneModel',
        action: 'CAN_UPDATE',
        user: userid,
        parkinggroupprops: parkinggroup,
        siteprops: {
            siteid: siteid
        }
    }

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.parkinggroup);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}

// Metadata Parking Spot
/**
 * All public APIs request - encode and send request to MS (ParkingSpotMetadata)
 * @param {*} userid
 * @param {*} params
 * @param {*} callback
 */
mngr.encodeParkingSpotsRequest = function (userid, params, callback) {
    try {
        mngr.Exec(userid, params, function (err, response) {
            if (!err) {
                callback(null, response.result);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

// Parking Policy Tag
mngr.getPolicyCategory = function(reqid, userid, orgid, siteid, tagid, callback) {
    var params = {
        requestid: reqid,
        type: 'getPolicyCategory',
        model: 'PolicyCategoryModel',
        action: 'CAN_READ',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        configprops: {
            tagid: tagid
        }
    };
    try {
        //To Send the request to Kafka
        params.service = parkingTagReqTopicKey;

        mngr.Exec(userid, params, function(err, response) {
            global.log.info("kafka request params",params);
            if (!err) {
                callback(null, response.result);
            } else {
                global.log.error('Could not get a parking policy tag', err.message, err)
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.deletePolicyCategory = function(reqid, userid, orgid, siteid, tagid, callback) {
    var params = {
        requestid: reqid,
        type: 'deletePolicyCategory',
        model: 'PolicyCategoryModel',
        action: 'CAN_DELETE',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        configprops: {
            tagid: tagid
        }
    };
    try {
        //To Send the request to Kafka
        params.service = parkingTagReqTopicKey;

        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.result);
            } else {
                global.log.error('Could not delete a parking policy tag', err.message, err)
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.updatePolicyCategory = function(reqid, userid, orgid, siteid, tagid, tag, callback) {
    var params = {
        requestid: reqid,
        type: 'updatePolicyCategory',
        model: 'PolicyCategoryModel',
        action: 'CAN_UPDATE',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        configprops: {
            tagid: tagid,
            tag: tag
        }
    };
    try {
        //To Send the request to Kafka
        params.service = parkingTagReqTopicKey;

        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.result);
            } else {
                global.log.error('Could not update a parking policy tag', err.message, err)
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.createPolicyCategory = function(reqid, userid, orgid, siteid, tag, callback) {
    var params = {
        requestid: reqid,
        type: 'createPolicyCategory',
        model: 'PolicyCategoryModel',
        action: 'CAN_CREATE',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        configprops: {
            tag: tag
        },
    };
    try {
        //To Send the request to Kafka
        params.service = parkingTagReqTopicKey;

        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.result);
            } else {
                global.log.error('Could not create a parking policy tag', err.message, err)
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.getAllPolicyCategory = function(reqid, userid, orgid, siteid, callback) {
    var params = {
        requestid: reqid,
        type: 'getAllPolicyCategory',
        model: 'PolicyCategoryModel',
        action: 'CAN_READ',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        }
    };
    try {
        //To Send the request to Kafka
        params.service = parkingTagReqTopicKey;

        mngr.Exec(userid, params, function(err, response) {
            global.log.info("kafka request params",params);
            if (!err) {
                callback(null, response.result);
            } else {
                global.log.error('Could not get all parking poliy tags', err.message, err)
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

// Parking Policy
mngr.getParkingPolicy = function(reqid, userid, orgid, siteid, policyid, callback) {
    var params = {
        requestid: reqid,
        type: 'getParkingPolicy',
        model: 'ParkingPolicyModel',
        action: 'CAN_READ',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        configprops: {
            policyid: policyid
        }
    };
    try {
        //To Send the request to Kafka
        params.service = parkingPolicyTagReqTopicKey;

        mngr.Exec(userid, params, function(err, response) {
            global.log.info("kafka request params",params);
            if (!err) {               
                helpers.addUserNameForUserId(response.result, parkPolicyUserIdKey, parkOptUserNameKey, function(res) {
                    callback(null, res);
                });                               
            } else {
                global.log.error('Could not get a parking policy', err.message, err)
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.deleteParkingPolicy = function(reqid, userid, orgid, siteid, policyid, callback) {
    var params = {
        requestid: reqid,
        type: 'deleteParkingPolicy',
        model: 'ParkingPolicyModel',
        action: 'CAN_DELETE',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        configprops: {
            policyid: policyid
        }
    };
    try {
        //To Send the request to Kafka
        params.service = parkingPolicyTagReqTopicKey;

        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.result);
            } else {
                global.log.error('Could not delete a parking policy ', err.message, err)
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.updateParkingPolicy = function(reqid, userid, orgid, siteid, policyid, policy, callback) {
    var params = {
        requestid: reqid,
        type: 'updateParkingPolicy',
        model: 'ParkingPolicyModel',
        action: 'CAN_UPDATE',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        configprops: {
            policyid: policyid,
            policy: policy
        }
    };
    try {
        //To Send the request to Kafka
        params.service = parkingPolicyTagReqTopicKey;

        mngr.Exec(userid, params, function(err, response) {
            if (!err) {                
                helpers.addUserNameForUserId(response.result, parkPolicyUserIdKey, parkOptUserNameKey, function(res) {
                    callback(null, res);
                });
            } else {
                global.log.error('Could not update a parking policy', err.message, err)
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.createParkingPolicy = function(reqid, userid, orgid, siteid, policy, callback) {
    var params = {
        requestid: reqid,
        type: 'createParkingPolicy',
        model: 'ParkingPolicyModel',
        action: 'CAN_CREATE',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        configprops: {
            policy: policy
        },
    };
    try {
        //To Send the request to Kafka
        params.service = parkingPolicyTagReqTopicKey;

        mngr.Exec(userid, params, function(err, response) {
            if (!err) {                
                helpers.addUserNameForUserId(response.result, parkPolicyUserIdKey, parkOptUserNameKey, function(res) {
                    callback(null, res);
                });
            } else {
                global.log.error('Could not create a parking policy', err.message, err)
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.getAllParkingPolicy = function(reqid, userid, orgid, siteid, callback) {
    var params = {
        requestid: reqid,
        type: 'getAllParkingPolicy',
        model: 'ParkingPolicyModel',
        action: 'CAN_READ',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        }
    };
    try {
        //To Send the request to Kafka
        params.service = parkingPolicyTagReqTopicKey;

        mngr.Exec(userid, params, function(err, response) {
            global.log.info("kafka request params",params);
            if (!err) {                
                helpers.addUserNameForUserId(response.result, parkPolicyUserIdKey, parkOptUserNameKey, function(res) {
                    callback(null, res);
                });
            } else {
                global.log.error('Could not get all parking policys', err.message, err)
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.getParkingPolicyVersion = function(reqid, userid, orgid, siteid, parkingpolicyid, version, callback) {
    var params = {
        requestid: reqid,
        type: 'getParkingPolicyVersion',
        model: 'ParkingPolicyModel',
        action: 'CAN_READ',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        configprops: {
            policyid: parkingpolicyid,
            version: version
        }
    };
    try {
        //To Send the request to Kafka
        params.service = parkingPolicyTagReqTopicKey;

        mngr.Exec(userid, params, function(err, response) {
            global.log.info("kafka request params",params);
            if (!err) {                
                helpers.addUserNameForUserId(response.result, parkPolicyUserIdKey, parkOptUserNameKey, function(res) {
                    callback(null, res);
                });
            } else {
                global.log.error('Could not get parking policy based on version', err.message, err)
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.getAllVersionsOfParkingPolicy = function(reqid, userid, orgid, siteid, parkingpolicyid, callback) {
    var params = {
        requestid: reqid,
        type: 'getAllVersionsOfParkingPolicy',
        model: 'ParkingPolicyModel',
        action: 'CAN_READ',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        configprops: {
            policyid: parkingpolicyid,
        }
    };
    try {
        //To Send the request to Kafka
        params.service = parkingPolicyTagReqTopicKey;

        mngr.Exec(userid, params, function(err, response) {
            global.log.info("kafka request params",params);
            if (!err) {                
                helpers.addUserNameForUserId(response.result, parkPolicyUserIdKey, parkOptUserNameKey, function(res) {
                    callback(null, res);
                });
            } else {
                global.log.error('Could not get parking policy based on version history', err.message, err)
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.getAllActiveParkingPolicyForPeriod = function(reqid, userid, orgid, siteid, parkinggroupid, fromtime, totime,  callback) {
    var params = {
        requestid: reqid,
        type: 'getAllActiveParkingPolicyForPeriod',
        model: 'ParkingPolicyModel',
        action: 'CAN_READ',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        configprops: {
            parkinggroupid: parkinggroupid,
            fromtime: fromtime,
            totime: totime
        }
    };
    try {
        //To Send the request to Kafka
        params.service = parkingPolicyTagReqTopicKey;

        mngr.Exec(userid, params, function(err, response) {
            global.log.info("kafka request params",params);
            if (!err) {                
                helpers.addUserNameForUserId(response.result, parkPolicyUserIdKey, parkOptUserNameKey, function(res) {
                    callback(null, res);
                });
            } else {
                global.log.error('Could not get parking policy within timeline', err.message, err)
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.getActiveParkingPolicy = function(reqid, userid, orgid, siteid, parkinggroupid,  callback) {
    var params = {
        requestid: reqid,
        type: 'getActiveParkingPolicy',
        model: 'ParkingPolicyModel',
        action: 'CAN_READ',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        configprops: {
            parkinggroupid: parkinggroupid,
        }
    };
    try {
        //To Send the request to Kafka
        params.service = parkingPolicyTagReqTopicKey;

        mngr.Exec(userid, params, function(err, response) {
            global.log.info("kafka request params",params);
            if (!err) {                
                helpers.addUserNameForUserId(response.result, parkPolicyUserIdKey, parkOptUserNameKey, function(res) {
                    callback(null, res);
                });
            } else {
                global.log.error('Could not get parking policy within timeline', err.message, err)
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.searchParkingPolicy = function(reqid, userid, orgid, siteid, searchPayload,  callback) {
    var params = {
        requestid: reqid,
        type: 'searchParkingPolicy',
        model: 'ParkingPolicyModel',
        action: 'CAN_READ',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        configprops: {
            searchPayload: searchPayload,
        }
    };
    try {
        //To Send the request to Kafka
        params.service = parkingPolicyTagReqTopicKey;

        mngr.Exec(userid, params, function(err, response) {
            global.log.info("kafka request params",params);
            if (!err) {                
                helpers.addUserNameForUserId(response.result, parkPolicyUserIdKey, parkOptUserNameKey, function(res) {
                    callback(null, res);
                });
            } else {
                global.log.error('Could not get parking policy with search criteria', err.message, err)
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.policyTagsAssociation = function(reqid, userid, orgid, siteid, parkingpolicyid, tagspolicylink, callback) {
    var params = {
        requestid: reqid,
        type: 'policyTagsAssociation',
        model: 'ParkingPolicyModel',
        action: 'CAN_UPDATE',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        configprops: {
            policyid: parkingpolicyid,
            tagspolicylink: tagspolicylink
        }
    };
    try {
        //To Send the request to Kafka
        params.service = parkingPolicyTagReqTopicKey;

        mngr.Exec(userid, params, function(err, response) {
            global.log.info("kafka request params",params);
            if (!err) {
                callback(null, response.result);
            } else {
                global.log.error('Could not update a parking tags association', err.message, err)
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.policyTagsDisassociation = function(reqid, userid, orgid, siteid, parkingpolicyid, tagspolicylink, callback) {
    var params = {
        requestid: reqid,
        type: 'policyTagsDisassociation',
        model: 'ParkingPolicyModel',
        action: 'CAN_DELETE',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        configprops: {
            policyid: parkingpolicyid,
            tagspolicylink: tagspolicylink
        }
    };
    try {
        //To Send the request to Kafka
        params.service = parkingPolicyTagReqTopicKey;

        mngr.Exec(userid, params, function(err, response) {
            global.log.info("kafka request params",params);
            if (!err) {
                callback(null, response.result);
            } else {
                global.log.error('Could not delete a parking policy tags association', err.message, err)
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

// Parking Group Policy
mngr.associatedParkingGroups = function(reqid, userid, orgid, siteid, parkingpolicyid, callback) {
    var params = {
        requestid: reqid,
        type: 'associatedParkingGroups',
        model: 'ParkingGroupModel',
        action: 'CAN_READ',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        configprops: {
            policyid: parkingpolicyid
        }
    };
    try {
        //To Send the request to Kafka
        params.service = parkingMetadataReqTopicKey;

        mngr.Exec(userid, params, function(err, response) {
            global.log.info("kafka request params",params);
            if (!err) {
                callback(null, response.result);
            } else {
                global.log.error('Could not get a parking group policy', err.message, err)
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.policyAssociation = function(reqid, userid, orgid, siteid, parkingpolicyid, ParkingGroupPolicyLink, callback) {
    var params = {
        requestid: reqid,
        type: 'policyAssociation',
        model: 'ParkingGroupModel',
        action: 'CAN_UPDATE',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        configprops: {
            policyid: parkingpolicyid,
            ParkingGroupPolicyLink: ParkingGroupPolicyLink
        }
    };
    try {
        //To Send the request to Kafka
        params.service = parkingMetadataReqTopicKey;

        mngr.Exec(userid, params, function(err, response) {
            global.log.info("kafka request params",params);
            if (!err) {
                callback(null, response.result);
            } else {
                global.log.error('Could not update a parking group policy', err.message, err)
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.policyDisassociation = function(reqid, userid, orgid, siteid, parkingpolicyid, ParkingGroupPolicyLink, callback) {
    var params = {
        requestid: reqid,
        type: 'policyDisassociation',
        model: 'ParkingGroupModel',
        action: 'CAN_DELETE',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        configprops: {
            policyid: parkingpolicyid,
            ParkingGroupPolicyLink: ParkingGroupPolicyLink
        }
    };
    try {
        //To Send the request to Kafka
        params.service = parkingMetadataReqTopicKey;

        mngr.Exec(userid, params, function(err, response) {
            global.log.info("kafka request params",params);
            if (!err) {
                callback(null, response.result);
            } else {
                global.log.error('Could not delete a parking group policy', err.message, err)
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

/**
 * Used for SMS/email feature. Refer /notifications/manager.js
 * @param {*} isBusinessAlert - To know if it's business alert or not
 * @param {*} userid
 * @param {*} alertid
 * @param {*} orgid
 * @param {*} siteid
 * @param {*} callback
 */
mngr.getAlertSys = function (isBusinessAlert, userid, alertid, orgid, siteid, callback) {

    let params = alertsModel.getAlertSysReq(userid, alertid, orgid, siteid);
    if (isBusinessAlert) {
        params = businessAlertsModel.getBusinessAlertSysReq(userid, alertid, orgid, siteid);
    }
    try {
        mngr.SysExec('root', params, function (err, response) {
            if (!err) {
                callback(null, response.result);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}

mngr.getUserEmail = function(userid, callback) {
    var params = {
        type: 'getUserEmail',
        model: 'UserModel',
        action: 'CAN_READ',
        userid: 'root',
        useremailprops: {
            userid: userid
        }
    }

    try {
        mngr.SysExec('root', params, function(err, response) {
            if (!err) {
                callback(null, response.useremail);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}

mngr.getUserEmails = function(userids, callback) {
    var params = {
        type: 'getUserEmails',
        model: 'UserModel',
        action: 'CAN_READ',
        userid: 'root',
        userprops: {
            userids: userids
        }
    }

    try {
        mngr.SysExec('root', params, function(err, response) {
            if (!err) {
                callback(null, response.items);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}

// Overlays
mngr.getAllOverlays = function(userid, siteid, callback) {
    var params = {
        type: 'getAllOverlays',
        model: 'OverlayModel',
        action: 'CAN_READ',
        user: userid,
        siteprops: {
            siteid: siteid
        }
    }

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.items);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}
mngr.getOverlay = function(userid, oid, callback) {
    var params = {
        type: 'getOverlay',
        model: 'OverlayModel',
        action: 'CAN_READ',
        user: userid,
        overlayprops: {
            overlayid: oid
        }
    }

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.overlay);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}
mngr.deleteOverlay = function(userid, oid, siteid, callback) {
    var params = {
        type: 'deleteOverlay',
        model: 'OverlayModel',
        action: 'CAN_DELETE',
        user: userid,
        overlayprops: {
            overlayid: oid
        },
        siteprops: {
            siteid: siteid
        }
    };

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};
mngr.createOverlay = function(userid, ovl, siteid, callback) {
    var params = {
        type: 'createOverlay',
        model: 'OverlayModel',
        action: 'CAN_CREATE',
        user: userid,
        overlayprops: ovl,
        siteprops: {
            siteid: siteid
        }
    }

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.overlay);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}
mngr.updateOverlay = function(userid, ovl, siteid, callback) {
    var params = {
        type: 'updateOverlay',
        model: 'OverlayModel',
        action: 'CAN_UPDATE',
        user: userid,
        overlayprops: ovl,
        siteprops: {
            siteid: siteid
        }
    }

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.overlay);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}

// Groups
mngr.getAllGroups = function(userid, siteid, callback) {
    var params = {
        type: 'getAllGroups',
        model: 'GroupModel',
        action: 'CAN_READ',
        user: userid,
        siteprops: {
            siteid: siteid
        }
    }

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.items);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}
mngr.getGroup = function(userid, gid, callback) {
    var params = {
        type: 'getGroup',
        model: 'GroupModel',
        action: 'CAN_READ',
        user: userid,
        groupprops: {
            groupid: gid
        }
    }

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.group);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}
mngr.deleteGroup = function(userid, gid, siteid, callback) {
    var params = {
        type: 'deleteGroup',
        model: 'GroupModel',
        action: 'CAN_DELETE',
        user: userid,
        groupprops: {
            groupid: gid
        },
        siteprops: {
            siteid: siteid
        }
    }

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}
mngr.createGroup = function(userid, grp, orgid, siteid, callback) {
    var params = {
        type: 'createGroup',
        model: 'GroupModel',
        action: 'CAN_CREATE',
        user: userid,
        groupprops: grp,
        siteprops: {
            siteid: siteid
        },
        orgprops: {
            orgid: orgid
        }
    };

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.group)
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}
mngr.updateGroup = function(userid, grp, orgid, siteid, callback) {
    var params = {
        type: 'updateGroup',
        model: 'GroupModel',
        action: 'CAN_UPDATE',
        user: userid,
        groupprops: grp,
        siteprops: {
            siteid: siteid
        },
        orgprops: {
            orgid: orgid
        }
    }

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.group);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}

mngr.addNodeToGroup = function(userid, grp, orgid, siteid, callback) {
    var nodeid = grp.nodeid;
    delete grp.nodeid;
    var params = {
        type: 'addNodeToGroup',
        model: 'GroupModel',
        action: 'CAN_UPDATE',
        user: userid,
        groupprops: grp,
        nodeprops: {
            nodeid: nodeid
        },
        siteprops: {
            siteid: siteid
        },
        orgprops: {
            orgid: orgid
        }
    }

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.group);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}

mngr.resendScheduleToLG = function(userid, orgid, siteid, groupid, callback) {
    var params = {
        type: 'resendScheduleToLG',
        model: 'GroupModel',
        action: 'CAN_UPDATE',
        user: userid,
        groupprops: {
            groupid: groupid
        },
        siteprops: {
            siteid: siteid
        },
        orgprops: {
            orgid: orgid
        }
    };

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.group);
            } else {
                callback(err);
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}

mngr.resendScheduleToNode = function(userid, orgid, siteid, nodeid, callback) {
    var params = {
        type: 'resendScheduleToNode',
        model: 'ScheduleModel',
        action: 'CAN_APPLY',
        user: userid,
        nodeprops: {
            nodeid: nodeid
        },
        siteprops: {
            siteid: siteid
        },
        orgprops: {
            orgid: orgid
        }
    };

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.node);
            } else {
                callback(err);
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}

mngr.removeNodeFromGroup = function(userid, grp, orgid, siteid, callback) {
    var nodeid = grp.nodeid;
    delete grp.nodeid;
    var params = {
        type: 'removeNodeFromGroup',
        model: 'GroupModel',
        action: 'CAN_UPDATE',
        user: userid,
        groupprops: grp,
        nodeprops: {
            nodeid: nodeid
        },
        siteprops: {
            siteid: siteid
        },
        orgprops: {
            orgid: orgid
        }
    }

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.group);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}

mngr.getAllFixtures = function(userid, siteid, callback) {
    var params = {
        type: 'getAllFixtures',
        model: 'FixtureModel',
        action: 'CAN_READ',
        user: userid,
        siteprops: {
            siteid: siteid
        }
    }

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.items);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}
mngr.getFixture = function(userid, fixtureid, callback) {
    var params = {
        type: 'getFixture',
        model: 'FixtureModel',
        action: 'CAN_READ',
        user: userid,
        fixtureprops: {
            fixtureid: fixtureid
        }
    }

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.fixture);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}
mngr.deleteFixture = function(userid, fixtureid, siteid, callback) {
    var params = {
        type: 'deleteFixture',
        model: 'FixtureModel',
        action: 'CAN_DELETE',
        user: userid,
        fixtureprops: {
            fixtureid: fixtureid
        },
        siteprops: {
            siteid: siteid
        }
    };

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}
mngr.createFixture = function(userid, fixture, siteid, callback) {
    var params = {
        type: 'createFixture',
        model: 'FixtureModel',
        action: 'CAN_CREATE',
        user: userid,
        fixtureprops: fixture,
        siteprops: {
            siteid: siteid
        }
    }

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.fixture);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}
mngr.updateFixture = function(userid, fixture, siteid, callback) {
    var params = {
        type: 'updateFixture',
        model: 'FixtureModel',
        action: 'CAN_UPDATE',
        user: userid,
        fixtureprops: fixture,
        siteprops: {
            siteid: siteid
        }
    }

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.fixture);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}

mngr.getAllSchedules = function(userid, siteid, orgid, callback) {
    var params = {
        type: 'getAllSchedules',
        model: 'ScheduleModel',
        action: 'CAN_READ',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        }
    };

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.items);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.getSchedule = function(userid, scheduleid, siteid, orgid, callback) {
    var params = {
        type: 'getSchedule',
        model: 'ScheduleModel',
        action: 'CAN_READ',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        scheduleprops: {
            scheduleid: scheduleid
        }
    };

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.schedule);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.deleteSchedule = function(userid, scheduleid, siteid, orgid, callback) {
    var params = {
        type: 'deleteSchedule',
        model: 'ScheduleModel',
        action: 'CAN_DELETE',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        scheduleprops: {
            scheduleid: scheduleid
        }
    };

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.addSchedule = function(userid, sch, siteid, orgid, callback) {
    var params = {
        type: 'createSchedule',
        model: 'ScheduleModel',
        action: 'CAN_CREATE',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        scheduleprops: sch
    };

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.schedule);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};
mngr.updateSchedule = function(userid, sch, siteid, orgid, callback) {
    var params = {
        type: 'updateSchedule',
        model: 'ScheduleModel',
        action: 'CAN_UPDATE',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        scheduleprops: sch
    };

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.schedule);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.applyConfigToNode = function(reqid, userid, configid, siteid, orgid, nodeid, callback) {

    mngr.getConfig(reqid, userid, orgid, siteid, configid, function(err,resp) {
        if(err) {
            callback(err);
            return;
        }

        var params = {
            requestid: reqid,
            type: 'applyConfigToNode',
            model: 'ConfigModel',
            action: 'CAN_APPLY',
            user: userid,
            orgprops: {
                orgid: orgid
            },
            siteprops: {
                siteid: siteid
            },
            nodeprops: {
                nodeids: [nodeid],
                nodeid: nodeid
            },
            configprops: {
                configid: configid
            }
        };

        // Check is this video node or next gen core node
        mngr.getAllNodeIdsForModelSite(userid, siteid, resp.model, function(err, response){

            if( err ) {
                callback(err);
            } else if( response && response.indexOf(nodeid)!==-1
                && ((configManager.get('isVideoNode')[resp.model]) || (configManager.get('isNextGenCoreNode')[resp.model]))){
                console.log('Video node found', nodeid)
                params.service = 'config';
                params.type = 'applyConfigToNodes';
            }

            try {
                mngr.Exec(userid, params, function (err, resp) {
                    if (!err) {
                        callback(null, {
                            config: resp.config,
                            nodes: resp.nodes,
                            message: resp.message,
                            token: resp.token,
                        });
                    } else {
                        callback(err)
                    }
                });
            } catch (e) {
                callback({
                    error: true,
                    message: e.message
                });
            }
        });
    });
};

mngr.applyConfigToNodes = function(reqid, userid, configid, siteid, orgid, nodeids, callback) {
  mngr.getConfig(reqid, userid, orgid, siteid, configid, function(err,resp) {
    if(err) {
        callback(err);
        return;
    }
    var params = {
        requestid: reqid,
        type: 'applyConfigToNodes',
        model: 'ConfigModel',
        action: 'CAN_APPLY',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        nodeprops: {
            nodeids: nodeids
        },
        configprops: {
            configid: configid
        }
    };

    if(!(configManager.get('isVideoNode')[resp.model] || configManager.get('isNextGenCoreNode')[resp.model])) {
      // Posalji na dd
      try {
          mngr.Exec(userid, params, function(err, resp) {
              if (!err) {
                  callback(null, {
                      config: resp.config,
                      nodes: resp.nodes
                  });
              } else {
                  callback(err)
              }
          });
      } catch (e) {
          callback({
              error: true,
              message: e.message
          });
      }
    } else {

      // Check is this video node
      mngr.getAllNodeIdsForModelSite(userid, siteid, resp.model, function(err, response){

          if( err ) {
              callback(err);
          } else if( response  ){
              var vnodeids = nodeids.filter(function(n) {
                  return response.indexOf(n) !== -1;
              });
              // Mix calls
              console.log('Video node found', vnodeids);

              // Sends request to DD and MS
              // TODO: Try to find model info in cache
              var responses = 0,
                  errors = 0,
                  accepted = {
                      config: [],
                      nodes: []
                  },
                  rejected = [];
              try {
                  // Send request to MS
                  params.service = 'config';
                  params.nodeprops = {nodeids: vnodeids};
                  params.type = 'applyConfigToNodes';
                  mngr.Exec(userid, JSON.parse(JSON.stringify(params)), function(err, resp) {
                      if (!err) {
                          callback(null, {
                              config: resp.config,
                              nodes: resp.nodes
                          });
                      } else {
                          callback(err)
                      }
                  });
              } catch (e) {
                  callback({
                      error: true,
                      message: e.message
                  });
              }
          } else {
              // Nothing changed
              callback(null, {
                  config: [],
                  nodes: []
              });
          }
      });
    }

  });

};

mngr.applyConfigToGroup = function(reqid, userid, configid, siteid, orgid, groupid, callback) {
  mngr.getConfig(reqid, userid, orgid, siteid, configid, function(err,resp) {
    if(err) {
        callback(err);
        return;
    }
    var params = {
        requestid: reqid,
        type: 'applyConfigToGroup',
        model: 'ConfigModel',
        action: 'CAN_APPLY',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        groupprops: {
            groupid: groupid
        },
        configprops: {
            configid: configid
        }
    };

    if(!(configManager.get('isVideoNode')[resp.model] || configManager.get('isNextGenCoreNode')[resp.model])) {
      // Posalji na dd
      try {
          mngr.Exec(userid, params, function(err, resp) {
              if (!err) {
                  callback(null, {
                      config: resp.config,
                      nodes: resp.nodes
                  });
              } else {
                  callback(err)
              }
          });
      } catch (e) {
          callback({
              error: true,
              message: e.message
          });
      }
    } else {
      // Check is this video node
      mngr.getAllNodeIdsForModelGroup(userid, siteid, groupid, resp.model, function(err, response) {

          if (err) {
              callback(err);
          } else if (response) {
              var nodeids = response;
              if(typeof nodeids === "string") {
                  var tmp = nodeids;
                  nodeids = [];
                  nodeids.push(tmp);
              }
              var vnodeids = nodeids.filter(function (n) {
                  return response.indexOf(n) !== -1;
              });
              // Mix calls
              console.log('Video node found', vnodeids);

              // Sends request to DD and MS
              // TODO: Try to find model info in cache
              var responses = 0,
                  errors = 0,
                  accepted = {
                      config: [],
                      nodes: []
                  },
                  rejected = [];
              try {
                  // Send request to MS
                  params.service = 'config';
                  params.nodeprops = {nodeids: vnodeids};
                  params.type = 'applyConfigToNodes';
                  mngr.Exec(userid, JSON.parse(JSON.stringify(params)), function(err, resp) {
                      if (!err) {
                          callback(null, {
                              config: resp.config,
                              nodes: resp.nodes
                          });
                      } else {
                          callback(err)
                      }
                  });
              } catch (e) {
                  callback({
                      error: true,
                      message: e.message
                  });
              }
          } else {
              // Nothing changed
              callback(null, {
                  config: [],
                  nodes: []
              });
          }
      });
    }
  });
};

mngr.applyConfigToSite = function(reqid, userid, configid, siteid, orgid, callback) {
  mngr.getConfig(reqid, userid, orgid, siteid, configid, function(err,resp) {
    if(err) {
        callback(err);
        return;
    }
    var params = {
        requestid: reqid,
        type: 'applyConfigToSite',
        model: 'ConfigModel',
        action: 'CAN_APPLY',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        configprops: {
            configid: configid
        }
    };

    if(!(configManager.get('isVideoNode')[resp.model] || configManager.get('isNextGenCoreNode')[resp.model])) {
      // Posalji na dd
      try {
          mngr.Exec(userid, params, function(err, resp) {
              if (!err) {
                  callback(null, {
                      config: resp.config,
                      nodes: resp.nodes
                  });
              } else {
                  callback(err)
              }
          });
      } catch (e) {
          callback({
              error: true,
              message: e.message
          });
      }
    } else {
      // Check is this video node
      mngr.getAllNodeIdsForModelSite(userid, siteid, resp.model, function(err, response){

          if( err ) {
              callback(err);
          } else if( response  ){
              var vnodeids = response;
              if(typeof vnodeids === "string") {
                var tmp = vnodeids;
                vnodeids = [];
                vnodeids.push(tmp);
              }
              // Mix calls
              console.log('Video node found', vnodeids);

              // Sends request to DD and MS
              // TODO: Try to find model info in cache
              var responses = 0,
                  errors = 0,
                  accepted = {
                      config: null,
                      nodes: []
                  },
                  rejected = [];
              try {
                  // Send request to MS
                  params.service = 'config';
                  params.nodeprops = {nodeids: vnodeids};
                  mngr.Exec(userid, JSON.parse(JSON.stringify(params)), function(err, resp) {
                      if (!err) {
                          callback(null, {
                              config: resp.config,
                              nodes: resp.nodes
                          });
                      } else {
                          callback(err)
                      }
                  });
              } catch (e) {
                  callback({
                      error: true,
                      message: e.message
                  });
              }
          } else {
              // Nothing changed
              callback(null, {
                  config: [],
                  nodes: []
              });
          }
      });
    }
  });
};

mngr.applyScheduleToGroup = function(userid, scheduleid, siteid, orgid, groupid, callback) {
    var params = {
        type: 'applyScheduleToGroup',
        model: 'ScheduleModel',
        action: 'CAN_APPLY',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        scheduleprops: {
            scheduleid: scheduleid
        },
        groupprops: {
            groupids: groupid
        },
        nodeprops: {
            type: "LightingScheduledEvent"
        }
    };

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.schedule);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.applyScheduleToSite = function(userid, scheduleid, siteid, orgid, callback) {
    var params = {
        type: 'applyScheduleToSite',
        model: 'ScheduleModel',
        action: 'CAN_APPLY',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        scheduleprops: {
            scheduleid: scheduleid
        },
        nodeprops: {
            type: "LightingScheduledEvent"
        }
    };

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.schedule);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.getAllPDProfiles = function(userid, siteid, orgid, callback) {
    var params = {
        type: 'getAllPDProfiles',
        model: 'ScheduleModel',
        action: 'CAN_READ',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        }
    };

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.items);
            } else {
                callback(err);
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.getPDProfile = function(userid, pdprofileid, siteid, orgid, callback) {
    var params = {
        type: 'getPDProfile',
        model: 'ScheduleModel',
        action: 'CAN_READ',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        pdprofileprops: {
            pdprofileid: pdprofileid
        }
    };

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.pdprofile);
            } else {
                callback(err);
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.createPDProfile = function(user, pdprofile, siteid, orgid, callback) {
    var params = {
        type: 'createPDProfile',
        model: 'ScheduleModel',
        action: 'CAN_CREATE',
        user: user,
        pdprofileprops: pdprofile,
        siteprops: {
            siteid: siteid
        },
        orgprops: {
            orgid: orgid
        }
    };

    try {
        mngr.Exec(user, params, function(err, response) {
            if (!err) {
                callback(null, response.pdprofile);
            } else {
                callback(err);
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.updatePDProfile = function(userid, pdprofile, pdprofileid, siteid, orgid, callback) {
    pdprofile.pdprofileid = pdprofileid;
    var params = {
        type: 'updatePDProfile',
        model: 'ScheduleModel',
        action: 'CAN_UPDATE',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        pdprofileprops: pdprofile
    };

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.pdprofile);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.deletePDProfile = function(userid, pdprofileid, siteid, orgid, callback) {
    var params = {
        type: 'deletePDProfile',
        model: 'ScheduleModel',
        action: 'CAN_DELETE',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        pdprofileprops: {
            pdprofileid: pdprofileid
        }
    };

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, null);
            } else {
                callback(err);
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.applyPD = function(userid, pdprofileid, siteid, orgid, groupid, callback) {
    var type = groupid ? 'applyPDtoGroup' : 'applyPDtoSite';
    var params = {
        type: type,
        model: 'ScheduleModel',
        action: 'CAN_APPLY',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        groupprops: type === 'applyPDtoGroup' ? {
            groupids: groupid
        } : undefined,
        pdprofileprops: {
            pdprofileid: pdprofileid
        }
    };

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.result);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

function renameKeys(payload, keyMapping) {
    payload = JSON.parse(JSON.stringify(payload));

    Object.keys(payload)
        .forEach(function (oldKey){
            var newKey = keyMapping[oldKey] || oldKey;
            Object.defineProperty(payload, newKey,
                                  Object.getOwnPropertyDescriptor(payload, oldKey));
            if (newKey !== oldKey)
                delete payload[oldKey];
        });

    return payload;
}

function cleanupETDHProfile(etdhprofile) {
    delete etdhprofile.sites;
    delete etdhprofile.groups;
    delete etdhprofile.nodes;

    etdhprofile = renameKeys(etdhprofile,
                             {"highLux": "high-lux",
                              "highDriver": "high-driver",
                              "lowLux": "low-lux",
                              "lowDriver": "low-driver",
                              "minLux": "min-lux",
                              "minDriver": "min-driver",
                              "fastPoll": "fast-poll",
                              "slowPoll": "slow-poll"});

    return etdhprofile;
}

function prettyPrintETDH(etdhprofile) {
    if (!etdhprofile)
        return {};
    return renameKeys(etdhprofile,
                      {"high-lux": "highLux",
                       "high-driver": "highDriver",
                       "low-lux": "lowLux",
                       "low-driver": "lowDriver",
                       "min-lux": "minLux",
                       "min-driver": "minDriver",
                       "fast-poll": "fastPoll",
                       "slow-poll": "slowPoll"});
}

mngr.getAllETDHProfiles = function(userid, siteid, orgid, callback) {
    var params = {
        type: 'getAllETDHProfiles',
        model: 'ScheduleModel',
        action: 'CAN_READ',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        }
    };

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.items.map(prettyPrintETDH));
            } else {
                callback(err);
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.getAllDHProfiles = function(userid, siteid, orgid, callback) {
    var params = {
        type: 'getAllDHProfiles',
        model: 'ScheduleModel',
        action: 'CAN_READ',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        }
    };

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.items);
            } else {
                callback(err);
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.getETDHProfile = function(userid, etdhprofileid, siteid, orgid, callback) {
    var params = {
        type: 'getETDHProfile',
        model: 'ScheduleModel',
        action: 'CAN_READ',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        etdhprofileprops: {
            etdhprofileid: etdhprofileid
        }
    };

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, prettyPrintETDH(response.etdhprofile));
            } else {
                callback(err);
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.getDHProfile = function(userid, dhprofileid, siteid, orgid, callback) {
    var params = {
        type: 'getDHProfile',
        model: 'ScheduleModel',
        action: 'CAN_READ',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        dhprofileprops: {
            dhprofileid: dhprofileid
        }
    };

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.dhprofile);
            } else {
                callback(err);
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.applyServerToNodes = function(userid, server, siteid, orgid, nodeList, callback) {
    var params = {
        type: 'applyServerToNodes',
        model: 'NodeModel',
        action: 'CAN_CHANGE',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        nodeprops: {
            nodeList: nodeList,
            server: server
        }
    };

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.config);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.createETDHProfile = function(user, etdhprofile, siteid, orgid, callback) {
    var params = {
        type: 'createETDHProfile',
        model: 'ScheduleModel',
        action: 'CAN_CREATE',
        user: user,
        etdhprofileprops: cleanupETDHProfile(etdhprofile),
        siteprops: {
            siteid: siteid
        },
        orgprops: {
            orgid: orgid
        }
    };

    try {
        mngr.Exec(user, params, function(err, response) {
            if (!err) {
                callback(null, prettyPrintETDH(response.etdhprofile));
            } else {
                callback(err);
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.createDHProfile = function(user, dhprofile, siteid, orgid, callback) {
    var params = {
        type: 'createDHProfile',
        model: 'ScheduleModel',
        action: 'CAN_CREATE',
        user: user,
        dhprofileprops: dhprofile,
        siteprops: {
            siteid: siteid
        },
        orgprops: {
            orgid: orgid
        }
    };

    try {
        mngr.Exec(user, params, function(err, response) {
            if (!err) {
                callback(null, response.dhprofile);
            } else {
                callback(err);
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.updateETDHProfile = function(userid, etdhprofile, etdhprofileid, siteid, orgid, callback) {
    etdhprofile.etdhprofileid = etdhprofileid;
    var params = {
        type: 'updateETDHProfile',
        model: 'ScheduleModel',
        action: 'CAN_UPDATE',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        etdhprofileprops: cleanupETDHProfile(etdhprofile)
    };

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, prettyPrintETDH(response.etdhprofile));
            } else {
                callback(err);
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.updateDHProfile = function(userid, dhprofile, dhprofileid, siteid, orgid, callback) {
    dhprofile.dhprofileid = dhprofileid;
    var params = {
        type: 'updateDHProfile',
        model: 'ScheduleModel',
        action: 'CAN_UPDATE',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        dhprofileprops: dhprofile
    };

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.dhprofile);
            } else {
                callback(err);
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.deleteETDHProfile = function(userid, etdhprofileid, siteid, orgid, callback) {
    var params = {
        type: 'deleteETDHProfile',
        model: 'ScheduleModel',
        action: 'CAN_DELETE',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        etdhprofileprops: {
            etdhprofileid: etdhprofileid
        }
    };

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, null);
            } else {
                callback(err);
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.deleteDHProfile = function(userid, dhprofileid, siteid, orgid, callback) {
    var params = {
        type: 'deleteDHProfile',
        model: 'ScheduleModel',
        action: 'CAN_DELETE',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        dhprofileprops: {
            dhprofileid: dhprofileid
        }
    };

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, null);
            } else {
                callback(err);
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.calibrateDaylightHarvesting = function(userid, dhprofileid, siteid, orgid, callback) {
    var params = {
        type: 'calibrateDHProfile',
        model: 'ScheduleModel',
        action: 'CAN_UPDATE',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        dhprofileprops: {
            dhprofileid: dhprofileid
        }
    };

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response);
            } else {
                callback(err);
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.applyETDH = function(userid, etdhprofileid, siteid, orgid, groupid, nodeid, callback) {
    var type = nodeid ? 'applyETDHtoNode' : groupid ? 'applyETDHtoGroup' : 'applyETDHtoSite';
    var params = {
        type: type,
        model: 'ScheduleModel',
        action: 'CAN_APPLY',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        groupprops: type === 'applyETDHtoGroup' ? {
            groupids: groupid
        } : undefined,
        etdhprofileprops: {
            etdhprofileid: etdhprofileid
        },
        nodeprops: {
            nodeid: nodeid ? nodeid : undefined,
            type: "DaylightHarvesting"
        }
    };

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.result);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.applyDH = function(userid, dhprofileid, siteid, orgid, groupid, nodeid, callback) {
    var type = nodeid ? 'applyDHtoNode' : groupid ? 'applyDHtoGroup' : 'applyDHtoSite';
    var params = {
        type: type,
        model: 'ScheduleModel',
        action: 'CAN_APPLY',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        groupprops: type === 'applyDHtoGroup' ? {
            groupids: groupid
        } : undefined,
        dhprofileprops: {
            dhprofileid: dhprofileid
        },
        nodeprops: {
            nodeid: nodeid ? nodeid : undefined,
            type: "DaylightHarvesting"
        }
    };

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.result);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.getAllETDHProfileTriggers = function(userid, etdhprofileid, siteid, orgid, callback) {
    var params = {
        type: 'getAllETDHProfileTriggers',
        model: 'ScheduleModel',
        action: 'CAN_APPLY',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        etdhprofileprops: {
            etdhprofileid: etdhprofileid
        }
    };

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.items);
            } else {
                callback(err);
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.getAllDHProfileTriggers = function(userid, dhprofileid, siteid, orgid, callback) {
    var params = {
        type: 'getAllDHProfileTriggers',
        model: 'ScheduleModel',
        action: 'CAN_APPLY',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        dhprofileprops: {
            dhprofileid: dhprofileid
        }
    };

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.items);
            } else {
                callback(err);
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.removeETDHProfileTriggers = function(userid, etdhprofileid, siteid, orgid, nodeids, callback) {
    var params = {
        type: 'removeETDHProfileTriggers',
        model: 'ScheduleModel',
        action: 'CAN_APPLY',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        etdhprofileprops: {
            etdhprofileid: etdhprofileid
        },
        nodeprops: {
            nodeids: nodeids
        }
    };

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.success);
            } else {
                callback(err);
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.addETDHProfileTriggers = function(userid, etdhprofileid, siteid, orgid, nodeids, callback) {
    var params = {
        type: 'addETDHProfileTriggers',
        model: 'ScheduleModel',
        action: 'CAN_APPLY',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        etdhprofileprops: {
            etdhprofileid: etdhprofileid
        },
        nodeprops: {
            nodeids: nodeids
        }
    };

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.success);
            } else {
                callback(err);
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.removeDHProfileTrigger = function(userid, dhprofileid, siteid, orgid, nodeid, callback) {
    var params = {
        type: 'removeDHProfileTrigger',
        model: 'ScheduleModel',
        action: 'CAN_APPLY',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        dhprofileprops: {
            dhprofileid: dhprofileid
        },
        nodeprops: {
            nodeid: nodeid
        }
    };

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.success);
            } else {
                callback(err);
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.addDHProfileTrigger = function(userid, dhprofileid, siteid, orgid, nodeid, callback) {
    var params = {
        type: 'addDHProfileTrigger',
        model: 'ScheduleModel',
        action: 'CAN_APPLY',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        dhprofileprops: {
            dhprofileid: dhprofileid
        },
        nodeprops: {
            nodeid: nodeid
        }
    };

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.success);
            } else {
                callback(err);
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};


mngr.bulkAssignUsersToParkingGroup = function(userid, parkingGroupId, siteid, orgid, userIdsToLink, callback) {
    var params = {
        type: 'bulkAssignUsersToParkingGroup',
        model: 'ParkingGroupModel',
        action: 'CAN_CREATE',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        parkingGroupprops: {
            parkingGroupId: parkingGroupId
        },
        userIdToLinkprops: {
            userIdsToLink: userIdsToLink
        }
    };

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.success);
            } else {
                callback(err);
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};


mngr.bulkUnassignUsersFromParkingGroup = function(userid, parkingGroupId, siteid, orgid, userIdsToLink, callback) {
    var params = {
        type: 'bulkUnassignUsersFromParkingGroup',
        model: 'ParkingGroupModel',
        action: 'CAN_DELETE',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        parkingGroupprops: {
            parkingGroupId: parkingGroupId
        },
        userIdToLinkprops: {
            userIdsToLink: userIdsToLink
        }
    };

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.success);
            } else {
                callback(err);
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};





mngr.getLightingPolicy = function(userid, lpid, callback) {
    var params = {
        type: 'getLightingPolicy',
        model: 'LightingPolicyModel',
        action: 'CAN_READ',
        user: userid,
        lightingpolicyprops: {
            lightingpolicyid: lpid
        }
    }

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.result);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}
mngr.deleteLightingPolicy = function(userid, lpid, callback) {
    var params = {
        type: 'deleteLightingPolicy',
        model: 'LightingPolicyModel',
        action: 'CAN_DELETE',
        user: userid,
        lightingpolicyprops: {
            lightingpolicyid: lpid
        }
    }

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.result);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}
mngr.addLightingPolicy = function(userid, lp, callback) {
    var params = {
        type: 'updateLightingPolicy',
        model: 'LightingPolicyModel',
        action: 'CAN_CREATE',
        user: userid,
        lightingpolicyprops: lp
    }

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.result);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}
mngr.updateLightingPolicy = function(userid, lp, callback) {
    var params = {
        type: 'updateLightingPolicy',
        model: 'LightingPolicyModel',
        action: 'CAN_UPDATE',
        user: userid,
        lightingpolicyprops: lp
    }

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.result);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}

mngr.readAllFirmwares = function(userid, callback) {
    try {
        var params = {
            type: 'getAllFirmwares',
            model: 'FirmwareModel',
            action: 'CAN_READ',
            user: userid
        };

        async.parallel({
            dd: function(next) {
                mngr.Exec(userid, params, function(err, resp) {
                     if (!err) {
                        next(null, resp.items);
                    } else {
                        next(err)
                    }
                });
            },
            micro: function(next) {
                var params2 = _.clone(params);

                params2.service = "otarequest";

                mngr.Exec(userid, params2, function(err, resp) {
                    if(!err && resp.success == true) {
                        next(null, resp.data);
                    } else {
                        next(err);
                    }
                });
            }
        }, function(err, results) {
            callback(err, _.concat(results.dd, results.micro));
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.addFirmware = function(userid, firmware, callback) {
    var params = {
        type: 'createFirmware',
        model: 'FirmwareModel',
        action: 'CAN_CREATE',
        user: userid,
        firmwareprops: firmware
    };
    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.firmware);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.readFirmware = function(userid, firmwareid, callback) {
    var params = {
        type: 'getFirmware',
        model: 'FirmwareModel',
        action: 'CAN_READ',
        user: userid,
        firmwareprops: {
            firmwareid: firmwareid
        }
    };

    try {
        async.parallel({
            dd: function(next) {
                mngr.Exec(userid, params, function(err, resp) {
                     if (!err) {
                        next(null, resp.firmware);
                    } else {
                        if(err.status === 404) {
                            next();
                        } else {
                            next(err);
                        }
                    }
                });
            },
            micro: function(next) {
                var params2 = _.clone(params);

                params2.service = "otarequest";

                mngr.Exec(userid, params2, function(err, resp) {
                    if(!err && resp.success == true) {
                        next(null, resp.data);
                    } else {
                        next(err);
                    }
                });
            }
        }, function(err, results) {
            var data = (_.isEmpty(results.dd)) ? results.micro : results.dd;

            callback(err, data);
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.deleteFirmware = function(userid, firmwareid, callback) {
    var params = {
        type: 'deleteFirmware',
        model: 'FirmwareModel',
        action: 'CAN_DELETE',
        user: userid,
        firmwareprops: {
            firmwareid: firmwareid
        }
    };
    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.updateFirmware = function(userid, firmware, callback) {
    var params = {
        type: 'updateFirmware',
        model: 'FirmwareModel',
        action: 'CAN_UPDATE',
        user: userid,
        firmwareprops: firmware
    };
    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response.firmware);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.assignFirmwareToNode = function(userid, orgid, siteid, nodeid, firmwareid, description, callback) {
    var params = {
        type: 'assignFirmwareToNode',
        model: 'NodeModel',
        action: (siteid ? 'CAN_ASSIGN_TO_SITES' : 'CAN_ASSIGN_TO_ORGS'),
        user: userid,
        nodeprops: {
            nodeid: nodeid
        },
        firmwareprops: {
            firmwareid: firmwareid,
            description: description
        },
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        }
    };
    try {
        async.parallel({
            dd: async.reflect(function(next) {
                mngr.Exec(userid, params, function(err, resp) {
                     if (!err) {
                        next(null, resp);
                    } else {
                        next(err)
                    }
                });
            }),
            micro: async.reflect(function(next) {
                var params2 = _.clone(params);

                params2.service = "otarequest";
                params2.otaprops = {
                    description: description
                };

                mngr.Exec(userid, params2, function(err, resp) {
                    if(!err && resp.success == true) {
                        next(null, resp.data);
                    } else {
                        next(err);
                    }
                });
            })
        }, function(err, results) {
            var data = null;

            if(!results.dd.error) {
                data = results.dd.value;
            } else if(!results.micro.error) {
                data = results.micro.value;
            } else {
                data = {
                    status: 400,
                    dd: results.dd.error,
                    micro: results.micro.error
                }
            }

            callback(err, data);
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.assignFirmwareToGroup = function(userid, orgid, siteid, groupid, firmwareid, description, callback) {
    var params = {
        type: 'assignFirmwareToGroup',
        model: 'NodeModel',
        action: (siteid ? 'CAN_ASSIGN_TO_SITES' : 'CAN_ASSIGN_TO_ORGS'),
        user: userid,
        firmwareprops: {
            firmwareid: firmwareid,
            description: description
        },
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        groupprops: {
            groupids: groupid
        }
    };

    try {
        async.parallel({
            dd: async.reflect(function(next) {
                mngr.Exec(userid, params, function(err, resp) {
                     if (!err) {
                        next(null, resp);
                    } else {
                        next(err)
                    }
                });
            }),
            micro: async.reflect(function(next) {
                var params2 = _.clone(params);

                params2.groupprops.groupid = groupid[0];
                params2.otaprops = {
                    description: description
                };
                params2.service = "otarequest";

                mngr.Exec(userid, params2, function(err, resp) {
                    if(!err && resp.success == true) {
                        next(null, resp.data);
                    } else {
                        next(err);
                    }
                });
            })
        }, function(err, results) {
            var data = null;

            if(!results.dd.error) {
                data = results.dd.value;
            } else if(!results.micro.error) {
                data = results.micro.value;
            } else {
                data = {
                    status: 400,
                    dd: results.dd.error,
                    micro: results.micro.error
                }
            }

            callback(err, data);
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.assignFirmwareToSite = function(userid, orgid, siteid, firmwareid, description, callback) {
    var params = {
        type: 'assignFirmwareToSite',
        model: 'NodeModel',
        action: (siteid ? 'CAN_ASSIGN_TO_SITES' : 'CAN_ASSIGN_TO_ORGS'),
        user: userid,
        firmwareprops: {
            firmwareid: firmwareid,
            description: description
        },
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        }
    };
    try {
        async.parallel({
            dd: async.reflect(function(next) {
                mngr.Exec(userid, params, function(err, resp) {
                     if (!err) {
                        next(null, resp);
                    } else {
                        next(err)
                    }
                });
            }),
            micro: async.reflect(function(next) {
                var params2 = _.clone(params);

                params2.service = "otarequest";
                params2.otaprops = {
                    description: description
                };

                mngr.Exec(userid, params2, function(err, resp) {
                    if(!err && resp.success == true) {
                        next(null, resp.data);
                    } else {
                        next(err);
                    }
                });
            })
        }, function(err, results) {
            var data = null;

            if(!results.dd.error) {
                data = results.dd.value;
            } else if(!results.micro.error) {
                data = results.micro.value;
            } else {
                data = {
                    status: 400,
                    dd: results.dd.error,
                    micro: results.micro.error
                }
            }

            callback(err, data);
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.otaStatusForSite = function(userid, orgid, siteid, callback) {
    var params = {
        type: 'otaStatusForSite',
        model: 'FirmwareModel',
        action: 'CAN_READ',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        }
    };

    try {
        async.parallel({
            dd: function(next) {
                mngr.Exec(userid, params, function(err, resp) {
                     if (!err) {
                        next(null, resp.items);
                    } else {
                        next(err)
                    }
                });
            },
            micro: function(next) {
                var params2 = _.clone(params);

                params2.service = "otarequest";

                mngr.Exec(userid, params2, function(err, resp) {
                    if(!err && resp.success == true) {
                        next(null, resp.data || []);
                    } else {
                        next(err);
                    }
                });
            }
        }, function(err, results) {
            callback(err, _.concat(results.dd, results.micro));
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};


mngr.updateVPNInfo = function(reqid, userid, siteid, nodeid, command, callback) {

    var params = {
        requestid: reqid,
        type: 'updateVPNInfo',
        model: 'ConfigModel',
        action: 'CAN_READ',
        user: userid,
        nodeprops: {
            nodeid: nodeid,
            command: command
        }
    };

    params.service = 'config';

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err)
                callback(null, response.vpnip);
            else
                callback(err)
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}

mngr.connectToVPN = function(reqid, userid, siteid, nodeid, command, callback) {
    var params = {
        type: 'connectToVPN',
        model: 'ConfigModel',
        action: 'CAN_READ',
        requestid: reqid,
        user: userid,
        nodeprops: {
            nodeid: nodeid,
            command: command
        }
    };
    // Send request to MS
    params.service = 'config';

     try {
         mngr.Exec(userid, params, function(err, response) {
             if (!err) {
                 callback(null, response);
             } else {
                 callback(err)
             }
         });
     } catch (e) {
         callback({
             error: true,
             message: e.message
         });
     }
}

mngr.disconnectFromVPN = function(reqid, userid, siteid, nodeid, command, callback) {
    var params = {
        type: 'disconnectFromVPN',
        model: 'ConfigModel',
        action: 'CAN_READ',
        requestid: reqid,
        user: userid,
        nodeprops: {
            nodeid: nodeid,
            command: command
        }
    };
    // Send request to MS
    params.service = 'config';

     try {
         mngr.Exec(userid, params, function(err, response) {
             if (!err) {
                 callback(null, response);
             } else {
                 callback(err)
             }
         });
     } catch (e) {
         callback({
             error: true,
             message: e.message
         });
     }
}

mngr.otaStatusForJob = function(userid, orgid, siteid, jobid, callback) {
    var params = {
        type: 'otaStatusForJob',
        model: 'FirmwareModel',
        action: 'CAN_READ',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        otaprops: {
            jobid: jobid
        }
    };
    try {
        async.parallel({
            dd: async.reflect(function(next) {
                mngr.Exec(userid, params, function(err, resp) {
                     if (!err) {
                        next(null, resp.items);
                    } else {
                        next(err)
                    }
                });
            }),
            micro: async.reflect(function(next) {
                var params2 = _.clone(params);

                params2.service = "otarequest";

                mngr.Exec(userid, params2, function(err, resp) {
                    if(!err && resp.success == true) {
                        next(null, resp.data);
                    } else {
                        next(err);
                    }
                });
            })
        }, function(err, results) {
            let data = null;
            // Check if DD response is not error and empty ([])
            if (!results.dd.error && results.dd.value.length) data = results.dd.value;
            else if (!results.micro.error) data = results.micro.value;
            else {
                data = { 
                    status: 400,
                    dd: results.dd.error,
                    micro: results.micro.error
                };
            }
            callback(err, data);
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

mngr.otaStatusJobUpdate = function(userid, orgid, siteid, jobid, action, callback) {
    var params = {
        type: 'otaStatusJobUpdate',
        model: 'FirmwareModel',
        action: 'CAN_READ',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        otaprops: {
            jobid: jobid,
            action: action
        }
    };
    try {
        async.parallel({
            dd: async.reflect(function(next) {
                mngr.Exec(userid, params, function(err, resp) {
                     if (!err) {
                        next(null, resp);
                    } else {
                        next(err)
                    }
                });
            }),
            micro: async.reflect(function(next) {
                var params2 = _.clone(params);

                params2.service = "otarequest";

                mngr.Exec(userid, params2, function(err, resp) {
                    console.log(err, resp);
                    if(!err && resp.success == true) {
                        next(null, resp.data);
                    } else {
                        next(err);
                    }
                });
            })
        }, function(err, results) {
            var data = null;

            if(!results.dd.error) {
                data = results.dd.value;
            } else if(!results.micro.error) {
                data = results.micro.value;
            } else {
                data = {
                    status: 400,
                    dd: results.dd.error,
                    micro: results.micro.error
                }
            }

            callback(err, data);
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
};

/**
 * Demo
 * @param userid
 * @param operation - type of operation
 * @param email - principal ID of the user
 * @param name - name of the user
 * @param title - title of the user
 * @param org - organization the user belongs to
 * @param roles - roles associated with this user
 * @param sites - sites the user has access to
 * @param done function Callback
 * @returns {*}
 */
mngr.doSysCrud = function(userid, action, model, properties, callback) {
    try {
        var modeltype = model.replace('Model', '');
        var is_multi = action.indexOf('getAll') === 0; //!properties || properties.hasOwnProperty('ids') || !properties.hasOwnProperty('id');  // e.g. properties.ids = [id1, id2,...] or read without object id
        var model_name = is_multi ? plural(modeltype) : modeltype;
        var operation;
        if (is_multi) {
            var multiaction = 'getAll';
            if (action.indexOf('getAllSuspended') === 0)
                multiaction = 'getAllSuspended';
            else if (action.indexOf('getAllActive') === 0)
                multiaction = 'getAllActive';
            operation = multiaction + (model_name.charAt(0)
                    .toUpperCase() + (model_name)
                    .slice(1)) + action.replace(multiaction, '');
        } else {
            var parts = action.split('_');
            operation = parts[0] + (model_name.charAt(0)
                    .toUpperCase() + (model_name)
                    .slice(1));
            // get_Email->getUserEmail
            if (parts.length > 1 && parts[1]) {
                operation += parts[1];
                modeltype += parts[1];
            }
        }

        var params = properties;
        params['type'] = operation; // e.g getUser
        params['model'] = model; // e.g UserModel
        params['action'] = action; // e.g CAN_READ
        params['user'] = userid; // e.g {userid:'xxxyyy '}

        mngr.SysExec(userid, params, function(err, response) {
            //global.log.info("[Encoder] doUserCrud", err.message, response?response:'');
            ////if(!err)
            ////    acl.invalidateUserData(email);
            ////if(done)
            //    done(err, params.user);
            if (!err) {
                callback && callback(null, response[is_multi ? 'items' : modeltype.toLowerCase()]);
            } else {
                callback && callback(err)
            }

        });
    } catch (e) {
        callback && callback({
            error: true,
            message: e.message
        });
    }
}

var plural = function(str) {
    return str + 's'; // TODO: fix this function
}

mngr.doCrud = function(userid, action, model, properties, callback) {
    var modeltype = model.replace('Model', '');
    var is_multi = !properties || properties.hasOwnProperty('ids') || !properties.hasOwnProperty('id'); // e.g. properties.ids = [id1, id2,...] or read without object id
    var model_name = is_multi ? plural(modeltype) : modeltype;
    var operation = action + (properties && properties.hasOwnProperty('id') ? '' : 'All') + (modeltype.charAt(0)
            .toUpperCase() + (modeltype)
            .slice(1));
    var privilege = 'CAN_' + action.toUpperCase();

    var params = properties;
    params['type'] = operation; // e.g getSite
    params['model'] = model; // e.g SiteModel
    params['action'] = privilege; // e.g CAN_READ
    params['privilege'] = privilege; // e.g CAN_READ
    params['user'] = userid; // e.g {userid:'xxxyyy '}

    // TODO: Fix these privileges
    if ((modeltype == 'Node' || modeltype == 'Site' || modeltype == 'Org' || modeltype == 'User') && action == 'update') {
        params['action'] = 'CAN_CHANGE'; // e.g CAN_READ
        params['privilege'] = 'CAN_CHANGE'; // e.g CAN_READ
    }

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback && callback(null, response[modeltype.toLowerCase()]);
            } else {
                callback && callback(err)
            }
        });
    } catch (e) {
        callback && callback({
            error: true,
            message: e.message
        });
    }
}

mngr.imageCaptureFromNode = function(reqid, userid, orgid, siteid, nodeid, channel, resolution, imgTS, imgFormat, description, callback) {
    console.log('Encoder: imageCaptureFromNode');
    var params = {
        requestid: reqid,
        type: 'getNode',
        model: 'NodeModel',
        action: 'CAN_READ',
        user: userid,
        nodeprops: {
            nodeid: nodeid
        },
        siteprops: {
            siteid: siteid
        },
        orgprops: {
            orgid: orgid
        },
        sicProps: {
            channel: channel,
            res: resolution,
            imgTS: imgTS,
            imgFormat: imgFormat
        },
        service: 'media'
    }

    try {
        mngr.Exec(userid, params, function(err, response) {
            if (!err) {
                callback(null, response);
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}
/**
 * All public APIs request - encode and send request to MS
 * @param {*} userid
 * @param {*} params
 * @param {*} callback
 */
mngr.encodeMSRequest = function (userid, params, callback) {
    try {
        const addUserNameUserDataTypes = ['getAllAppUserData', 'getAppUserData', 'createAppUserData', 'updateAppUserData'];
        const addUserNameTriggersTypes = ['getAllTriggers', 'filterTrigger', 'getTrigger', 'createTrigger', 'updateTrigger'];
        const addUserNameBusinessAlertsTypes = ['getAllBusinessAlerts', 'filterBusinessAlert', 'getBusinessAlert', 'createBusinessAlert', 'updateBusinessAlert'];
        const addUserNameWhatIfJobTypes = ['getAllWhatIfJobs', 'searchWhatIfJob', 'getWhatIfJob', 'createWhatIfJob', 'updateWhatIfJob'];
        const addUserNameWhatIfPolicyTypes = ['getAllWhatIfPolicies', 'getWhatIfPolicy', 'createWhatIfPolicy', 'updateWhatIfPolicy'];
        mngr.Exec(userid, params, function (err, response) {
            if (!err) {
                if (addUserNameUserDataTypes.includes(params.type)) {
                    helpers.addUserNameForUserId(response.result, userDataUserIdKey, parkOptUserNameKey, function(res) {
                        callback(null, res);
                    });
                } else if (addUserNameTriggersTypes.includes(params.type)) {
                    helpers.addUserNameForUserId(response.result, triggerUserIdKey, parkOptUserNameKey, function(res) {
                        callback(null, res);
                    });
                } else if (addUserNameBusinessAlertsTypes.includes(params.type)) {
                    helpers.addUserNamesForUserIds(response.result, [businessAlertTriggerUserIdKey, lastClearedByUserIdKey], [businessAlertTriggerUserNameKey, lastClearedByUserNameKey], function(res) {
                        callback(null, res);
                    });
                } else if (addUserNameWhatIfJobTypes.includes(params.type)) {
                    helpers.addUserNamesForUserIds(response.result, [whatIfCreatedByUserId, whatIfLastUpdatedByUserId], [whatIfCreatedByUserName, whatIfLastUpdatedByUserName], function(res) {
                        callback(null, res);
                    });
                } else if (addUserNameWhatIfPolicyTypes.includes(params.type)) {
                    helpers.addUserNameForUserId(response.result, parkPolicyUserIdKey, parkOptUserNameKey, function(res) {
                        callback(null, res);
                    });
                } else {
                    callback(null, response.result);
                }
            } else {
                callback(err)
            }
        });
    } catch (e) {
        callback({
            error: true,
            message: e.message
        });
    }
}
