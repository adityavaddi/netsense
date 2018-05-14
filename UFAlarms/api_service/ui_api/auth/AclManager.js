"use strict";

var cache = require('./LocalCache.js').LocalCache;
var encoder = require('./../encoder/Encoder.js').JSONEncoder;
var acl = require('acl');
// Using the memory backend
acl = new acl(new acl.memoryBackend());

var mngr = {};

exports.AclManager = mngr;

var configManager = require('kea-config');
configManager.init('./config/main.conf.js');

/**
 * Create ACL Rules
 */
mngr.init = function(err, data) {
    global.log.info("[AclManager.init]", JSON.stringify(data));
    if(data && data.authorization){
        for(var i=0; i<data.authorization.length; i++){
            var authorization = data.authorization[i];
            var type = authorization.type;
            var model = authorization.model;
            var allowed = authorization.allowed;
            for(var j=0; j<allowed.length; j++) {
                var action = allowed[j];
                var role = type+':'+model+':'+action;
                //global.log.info("Role ", role);
                if (type && model && action) {
                    //global.log.info('acl rule adding', role, model, action);
                    acl.allow(role, model, action);
                }
            }
        }
    }
};

mngr.logout = function (email, cb, params) {
    try {
        cb(null, params);
        //    mngr.invalidateUserData(email, cb, params).wait();
    } catch (e) {
        global.log.error({error: true, message: e.message});
    }
};

/**
 * @param email string Email
 * @param params object
 * @param callbac function nodejs-style Callback function
 */
mngr.isAllowed = function (email, params, callback) {
    if(configManager.get('acl.disabled')){
        //global.log.info("[acl.disabled");
        callback(null, true);
    } else {
        //global.log.info("[AclManager.isAllowed] + email " + email + " params " + JSON.stringify(params));
        //return true
        try{
            if (!params.model || !params.action || !email)
                return callback(null, false);

            global.log.info("[acl.hasRole] has params.model " + JSON.stringify(params.model) + " params.action " + params.action);
            acl.isAllowed(email, params.model, params.action).then(function(has) {
                if(!has) {
                    global.log.info("[acl.isAllowed] acces denied " , email, params.model, params.action);
                }
                // TODO: check target visibility
                callback(null, has)
            });
        } catch(e){
            callback({error:true, message:e.message});
        }
    }
};

/**
 * Add user roles to acl
 * @param user
 */
mngr.addUserRoles = function (user) {
    try{
        global.log.info("[AclManager].addUserRoles user => " + JSON.stringify(user));
        if (!user)
            return;
        var uid = user.login.email;

        if (uid && user.authorization) {
            var roles = [];
            for (var i = 0; i < user.authorization.length; i++) {
                var role = user.authorization[i];
                for(var j=0;j<role.allowed.length; j++){
                    var crole = role.type+':'+role.model+':'+role.allowed[j];
                    if(roles.indexOf(crole)===-1)
                        roles.push(crole);
                }
            }
            //global.log.info("[acl.addUserRoles] uid => " + uid + " roles => " + JSON.stringify(roles));
            acl.addUserRoles(uid, roles);
        }
        //acl.userRoles(uid).then(function(roles) {
        //    global.log.info('userRoles', roles)
        //});

    } catch (e){
        global.log.error("Failed to add user roles", e);
    }
}

mngr.findVisibleCustomersAndSites = function (search, callback) {
    //global.log.info("[AclManager.findVisibleCustomersAndSites]");
    if (!search || !search.length) {
        callback(null, null);
    }
    var stack = search; // TODO: clone
    var obj, found = [];
    while (obj = stack.pop()) {
        if (['site', 'organization'].indexOf(obj.type) != -1) {
            cache.getAsync('/child/' + root, function (key, val) {
                if (val && !val.length) {
                    // If not found add to stack
                    for (var i = 0; i < val.length; i++) {
                        // Add child to the stack
                        if (['site', 'organization'].indexOf(val[i].type) != -1)
                            stack.push(val[i]);
                        if (val[i] && found.indexOf(val[i].id) === -1)
                            found.push(val[i].id);
                    }
                }
                if (stack.length == 0) {
                    callback(null, found);
                }
            })
        }
    }
}


/**
 * Encode API Request
 * @param email
 * @param done
 * @returns {*}
 */
mngr.invalidateUserData = function(email, callback, params){
    global.log.info("[AclManager.invalidateUserDate]", email);
    try {
        cache.removeAsync('user:' + email, function (key) {
            if ( key ) {
                callback(null, params);
            }
            else{
                callback(null, params);
            }
        },function(){
            callback(null, params);
        })

    } catch (e) {
        global.log.error("Failed to remove from cache");
        callback({error:true, message:e.message});
    }
};

/**
 * Encode API Request
 * @param email
 * @param done
 * @returns {*}
 */
mngr.getUserData = function(email, callback){
    //global.log.info("[AclManager.getUserData]");

       try {
            var user;
            //user = mngr.getUserFromCache(email).wait();
            mngr.getUserFromDb(email, function (err, user) {

                if(err){
                    callback(err, user);
                } else if(!user) {
                    callback({error: true, message: 'Not Found'});
                } else if ( !user.error ) {
                    //cache.put('user:' + user.login.email, user);
                    callback(null, user);
                }
                else {
                    // Some error
                    callback(user);
                }
            });

        } catch (e) {
            global.log.error(e.message);
            callback({error:true, message:e.message});
        }
};


/**
 * Encode API Request
 * @param email
 * @param done
 * @returns {*}
 */
mngr.getUserFromDb = function(email, callback){
    //global.log.info("[AclManager.getUserFromDb]");

    try {
        encoder.fetchUser('root', email,  function (err, val) {
            if (!err) {
                callback (null, val);
            }
            else {
                callback (err);
            }
        })

    } catch (e) {
        callback ({error:true, message:e.message});
    }
};

/**
 * Encode API Request
 * @param email
 * @param done
 * @returns {*}
 */
/*mngr.getUserFromCache = Future.wrap(function(email, callback){
    //global.log.info("[AclManager.getUserFromCache]");
    try {
        cache.getAsync('user:' + email, function (key, val) {
            if ( val ) {
                callback(null, val);
            }
            else{
                callback(null, null);
            }
        },function(){
            callback(null, null);
        })

    } catch (e) {
        callback({error:true, message:e.message});
    }
});*/