/**
 * Creates global ldap client
 */
"use strict";


var client = null;

var ldap = require('ldapjs');

var configManager = require('kea-config');
configManager.init('./config/main.conf.js');

var client_conf = configManager.get('ldap.client');
var client_conf_infinity = JSON.parse(JSON.stringify(client_conf));
client_conf_infinity.reconnect.failAfter = Infinity;

try {
    client = ldap.createClient(client_conf_infinity).on('connect', function(){
        try {
            global.log.info('Connected to LDAP [ldap-client]');
            if(!client) {
                global.log.error('Ldap client onConnect error: Client not connected');
                return done({error:true, message:'Ldap client onConnect error: Client not connected'});
            }
            if(client._socket && client._socket.setKeepAlive)
              client._socket.setKeepAlive(true,60000);
            client.bind(configManager.get('ldap.adminDN'), configManager.get('ldap.adminPass'), function(err) {
                    if(err){
                        global.log.error("LDAP Bind Error: ", err)
                    } else {
                        global.log.info('Bound to LDAP:', configManager.get('ldap.URL'))
                    }
            });

            /**
             * Get email by user id
             * @param userid string User id
             * @param done function Callback
             * @returns {*}
             */
            client.getEmailByUserid = function(userid, done){
                //global.log.info("[AclManager.getUserFromCache]");
                try {
                    var opts = {
                        //filter: 'uid='+userid,
                        //filter: '(uid='+userid+')',
                        filter: (configManager.get('ldap.URL')===configManager.get('ldap.jsURL'))?'(uid='+userid+')':'uid='+userid,
                        scope: 'sub',
                        //paging: true,
                        //sizeLimit: 200
                    };
                    client.search(/*'uid='+userid+','+*/configManager.get('ldap.suffixUsers'), opts, function(err, res) {
                        var ret = null;
                        try {
                            res.on('searchEntry', function(entry) {
                                ret = entry.object.cn;
                                //global.log.info('entry: ' + JSON.stringify(entry.object));
                            });
                            res.on('searchReference', function(referral) {
                                global.log.info('referral: ' + referral.uris.join());
                            });
                            res.on('error', function(err) {
                                global.log.error('error: ' + err.message);
                            });
                            res.on('data', function(data) {
                                global.log.info('data: ' , data);
                            });
                            res.on('end', function(result) {
                                //global.log.info('status: ', ret);
                                if(!ret){
                                    return done({error:true, message: 'User with userid='+ userid + ' not found'}, false);
                                } else {
                                    return done(null,ret)
                                }
                            });
                        } catch(e){
                            global.log.error(e.message);
                            return done(null, false);
                        }
                    });
                } catch (e) {
                    done({error:true, message:e.message});
                }
            };

            /**
             * Get email baram usery user id
             * @pid string User id
             * @param done function Callback
             * @returns {*}
             */
            client.deleteUserKeys = function(user, done){
                //global.log.info("[AclManager.getUserFromCache]");
                try {
                    var opts = {
                        //filter: 'uid='+userid,
                        //filter: '(uid='+userid+')',
                        filter: (configManager.get('ldap.URL')===configManager.get('ldap.jsURL'))?'(sn='+user+')':'sn='+user,
                        scope: 'sub',
                        //paging: true,
                        //sizeLimit: 200
                    };
                    // ToDo: deepak: usee how used
                    client.search(/*'uid='+userid+','+*/configManager.get('ldap.suffixApiKeys'), opts, function(err, res) {
                        var ret = null;
                        try {
                            res.on('searchEntry', function(entry) {
                                var dn = entry.object.dn;
                                client.del(dn, function(err) {
                                    if(err) {
                                        global.log.error("ldap operation del failed", err.message);
                                    }
                                });
                                //global.log.info('entry: ' + JSON.stringify(entry.object));
                            });
                            res.on('searchReference', function(referral) {
                                global.log.info('referral: ' + referral.uris.join());
                            });
                            res.on('error', function(err) {
                                global.log.error('error: ' + err.message);
                            });
                            res.on('data', function(data) {
                                global.log.info('data: ' , data);
                            });
                            res.on('end', function(result) {
                                //global.log.info('status: ', ret);
                                if(!ret){
                                    return done({error:true, message: 'User with email='+ user + ' not found'}, false);
                                } else {
                                    return done(null,ret)
                                }
                            });
                        } catch(e){
                            global.log.error(e.message);
                            return done(null, false);
                        }
                    });
                } catch (e) {
                    done({error:true, message:e.message});
                }
            };

            /**
             * Get email by user id
             * @param what string User id (e.g. 'cn=testuser')
             * @param done function Callback
             * @returns {*}
             */
            client.deleteByDN = function(dn, done){
                //global.log.info("[AclManager.getUserFromCache]");
                try {
                    client.del(dn, done);
                } catch (e) {
                    done({error:true, message:e.message});
                }
            };
        } catch (e) {
            global.log.error(e);
            return done(e, false);
        }

    }).on('error',function(err){
        //client = null;
        global.log.error("LDAP Connect Error: ", err)
        global.log.error(err, "connecting to ldap in ldap-client.js");
    });
}
catch(err) {
    global.log.info("LDAP Init Error: ",err);
    global.log.error(err, "initializing client in ldap-client.js");
}

exports.Client = client;
exports.get_client = function() {
  return client;
};
