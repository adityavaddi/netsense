"use strict";

var session = require('express-session');
var cache = require('./LocalCache.js').LocalCache;
var encoder = require('./../encoder/Encoder.js').JSONEncoder;
var acl = require('./AclManager.js').AclManager;
var crypto = require('crypto');


// Ldap client
var ldap = require('ldapjs');

var configManager = require('kea-config');
configManager.init('./config/main.conf.js');

var mngr = {
    ldapClient: null
};
var client = mngr.ldapClient;

var client_conf = configManager.get('ldap.client');
var client_conf_infinity = JSON.parse(JSON.stringify(client_conf));
client_conf_infinity.reconnect.failAfter = Infinity;

var connectToLdap = function(){
    client = ldap.createClient(client_conf_infinity)
        .on('connect', function(){
        global.log.info('Connected to ldap');
    }).on('error',function(err){
        client = null;
        global.log.error("Error %s in LoginManager connecting to LDAP", err.message, JSON.stringify(err));
        //global.log.info(err, "in LoginManager connecting as ", email, password);
        setTimeout(connectToLdap, 1000);
    });;
};

///connectToLdap();

/**
 * Encode user password
 * @param username
 * @param password
 * @returns {*}
 * TODO: Add real encryption function
 */
mngr.encPassword = function(email, password){
    return password;
};


/**
 * Passport local-strategy callback
 * @param email
 * @param password
 * @param done
 * @returns {*}
 */
mngr.ValidateLocalUser = function(req, email, password, done){

    try {
        var client;
        client = ldap.createClient(client_conf).on('connect', function(){
            try {
                if(!client) {
                    global.log.error('Ldap client onConnect error: Client not connected');
                    return done({error:true, status: 500, message: 'Ldap client onConnect error: Client not connected'}, false);
                }

                if(client._socket && client._socket.setKeepAlive)
                  client._socket.setKeepAlive(true,60000);

                client.bind('cn='+email+','+configManager.get('ldap.suffixUsers'), password, function(err) {
                    var ip = req.headers['x-forwarded-for'] || req.connection.remoteAddress;
                    if(ip.indexOf(',')!==-1){
                        var parts = ip.split(',');
                        ip = parts[0];
                        console.log('proxied request', ip, parts);
                    }

                    if(err){
                        //global.log.info('Connected to ldap error', err);
                        if(err.name && err.name!=='InvalidCredentialsError') {
                            global.log.error(err);
                            encoder.logLoginActivities('root', ip, new Date(), email, 'email', 'failed', 'User '+ email +' not found ', function(err, data){
                                if(err)
                                    global.log.error('logLoginActivities', JSON.stringify(err));
                            });
                        } else {
                            // Log to database
                            encoder.logLoginActivities('root', ip, new Date(), email, 'email', 'failed', 'Login failed for '+email, function(err, data){
                                if(err)
                                    global.log.error('logLoginActivities', JSON.stringify(err));
                            });
                        }

                        if(client) {
                            client.unbind();
                            client = null;
                        }

                        done(null, false);
                    } else {
                        var user = acl.getUserData(email, function(err, user){
                            if (!user || !user.id) {
                                // Log to database
                                encoder.logLoginActivities('root', ip, new Date(), email, 'email', 'failed', 'Login failed for '+email, function(err, data){
                                    if(err)
                                        global.log.error('logLoginActivities', JSON.stringify(err));
                                });

                                if(client) {
                                    client.unbind();
                                    client = null;
                                }
                                return done(null, false);
                            }

                            // Log to database
                            encoder.logLoginActivities(user.id, ip, new Date(), email, 'email', 'succeeded', 'Logged in as '+user.id, function(err, data){
                                if(err)
                                    global.log.error('logLoginActivities', JSON.stringify(err));
                            });

                            if(client) {
                                client.unbind();
                                client = null;
                            }
                            return done(null, user);

                        });

                    }
                });
            } catch (e) {
                global.log.error(e);
                return done(e, false);
            }
        }).on('error',function(err){
            //client = null;
            global.log.error("Error %s in LoginManager connecting as %s/%s", err.message, email, password, JSON.stringify(err));
            //global.log.info(err, "in LoginManager connecting as ", email, password);
            return done(err, false);
        });
        //var user = cache.get('user:' + email);
        //encoder.GetUser(email, password, function(){
        //    consle.log('ValidateLocalUser done')
        //})

    } catch (e) {
        done({error: true, message: e.message});
    }
};
/**
 * Passport local-strategy callback
 * @param email
 * @param password
 * @param done
 * @returns {*}
 */
mngr.ValidateUserByToken = function(req, api_key, done){

    try {
        var client;
        client = ldap.createClient(client_conf).on('connect', function(){
            try {
                if(!client) {
                    global.log.error('Ldap client onConnect error: Client not connected');
                    return done({error:true, status: 500, message: 'Ldap client onConnect error: Client not connected'}, false);
                }
                var keycn = crypto.createHash('sha256').update(api_key).digest('hex');
                if(client._socket && client._socket.setKeepAlive)
                  client._socket.setKeepAlive(true,60000);

                client.bind('cn='+keycn+','+configManager.get('ldap.suffixApiKeys'), api_key, function(err) {
                    try {
                        var ip = req.headers['x-forwarded-for'] || req.connection.remoteAddress;
                        if(ip && ip.indexOf(',')!==-1){
                            var parts = ip.split(',');
                            ip = parts[0];
                            console.log('proxied request', ip, parts);
                        }

                        if(err){
                            // Log to database
                            encoder.logLoginActivities('root', ip, new Date(), api_key, 'api-key', 'forbidden', 'Access denied for key '+api_key, function(err, data){
                                if(err)
                                    global.log.error('logLoginActivities', JSON.stringify(err));
                            });
                            if(err.name && err.name!=='InvalidCredentialsError')
                                global.log.error(err);
                            //client.unbind();
                            client.unbind();
                            client = null;
                            return done(null, false);
                        } else {
                            var opts = {
                                //filter: '(cn=*)',
                                //scope: 'base',
                                //attributes: ['dn', 'sn', 'cn']
                            };

                        client.search('cn='+keycn+','+configManager.get('ldap.suffixApiKeys'), opts, function(err, res) {

                                try {
                                    var found = null;
                                    var error = null;
                                    //client.unbind();
                                    res.on('searchEntry', function(entry) {
                                        //global.log.info('entry: ' + JSON.stringify(entry.object));
                                        var date = entry.object.shadowExpire || entry.object.shadowexpire;
                                        var user = null;
                                        if(date && parseInt(date)>new Date().getTime() / 1000 / 24 / 3600){
                                            found = entry.object;   // Should be only one result
                                        }
                                    });
                                    res.on('searchReference', function(referral) {
                                        global.log.info('LDAP referral: ' + referral.uris.join());
                                    });
                                    res.on('error', function(err) {
                                        global.log.error('LDAP client error: %s', err.message, err);
                                        client.unbind();
                                        client = null;
                                        return done(err, false);
                                    });
                                    res.on('end', function(result) {
                                        if(result.status!==0){
                                            global.log.info('LDAP status: %d', result.status);
                                            client.unbind();
                                            client = null;
                                            return done(null, false);
                                        } else if(found) {
                                            try {
                                                acl.invalidateUserData(found.sn, function (err, user) {
                                                    user = acl.getUserData(found.sn, function (err, user) {
                                                        if (!user) {
                                                            encoder.logLoginActivities('root', ip, new Date(), api_key, 'api-key', 'forbidden', 'Access denied for key '+api_key, function(err, data){
                                                                if(err)
                                                                    global.log.error('Error %s in logLoginActivities', err.message, JSON.stringify(err));
                                                            });
                                                            client.unbind();
                                                            client = null;
                                                            return done(null, false);
                                                        }
                                                        client.unbind();
                                                        client = null;
                                                        return done(null, user);

                                                    });
                                                });

                                            } catch(e){
                                                client.unbind();
                                                client = null;
                                                global.log.error(e.message);
                                                return done(e, false);
                                            }

                                        } else {
                                            client.unbind();
                                            client = null;
                                        }
                                    });
                                } catch(e){
                                    client.unbind();
                                    client = null;
                                    global.log.error(e.message);
                                    return done(null, false);
                                }
                            });

                        }
                    } catch(e){
                        client.unbind();
                        client = null;
                        global.log.error(e, "in LoginManager searching for  "+ api_key);
                        //global.log.info(err, "in LoginManager searching for  "+ api_key);
                        return done(null, false);
                    }
                });
            } catch (e) {
                global.log.error(e);
                return done(e, false);
            }
        }).on('error',function(err){
            client.unbind();
            //client = null;
            global.log.error("Error %s in LoginManager connecting as %s", err.message, api_key, JSON.stringify(err));
            //global.log.info(err, "in LoginManager connecting as ", api_key);
            return done(err, false);
        });

        //var user = cache.get('user:' + email);
        //encoder.GetUser(email, password, function(){
        //    consle.log('ValidateLocalUser done')
        //})

    } catch (e) {
        done({error: true, message: e.message});
    }
};

/**
 * Passport local-strategy callback
 * @param email
 * @param password
 * @param done
 * @returns {*}
 */
mngr.OnLogin = function(err, user){
    try {
        acl.init(err, user);
        acl.addUserRoles(user);
    } catch (e) {
        global.log.error({error: true, message: e.message});
    }
};

mngr.OnLogout = function(user, cb, params) {
    try {
        if(user){
            acl.logout(user.user.email, cb, params);
        } else {
            global.log.info("Already logged out");
            cb(null, params);
        }

    } catch (e) {
        var err = {error: true, message: e.message};
        global.log.error(err);
        cb(err, params);
    }
};

/**
 * Reload User data (org, site, privileges)
 * @param email
 */
mngr.ReloadUserData = function (email, callback) {
    try {
        acl.getUserData(email, function (err, user) {
            if (!user || !user.id) {
                // Log to database
                if (err)
                    global.log.error('Error reloading user data', JSON.stringify(err));
                callback(err, null)
            } else {
                var cuser = JSON.parse(JSON.stringify(user));
                // TODO: Remove these fixes
                if (cuser && !cuser.name)
                    cuser.name = cuser.email;
                    
                global.log.info('req.session.CurrentUser', cuser);
                acl.init(err, cuser);
                acl.addUserRoles(cuser);

                callback(null, cuser);
            }
        });
    } catch (e) {
        global.log.error({ error: true, message: e.message });
        callback({ error: true, message: e.message });
    }
};

module.exports = mngr;
