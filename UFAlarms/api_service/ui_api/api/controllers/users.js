'use strict';

var passwordValidator = require('password-validator');
const helpers = require('./../helpers/helpers.js');

// Create a schema
var schema = new passwordValidator();

// Add properties to it
schema
.isMin(8)           // Minimum length 8
.isMax(100)         // Maximum length 100
.has().uppercase()  // Must have uppercase letters
.has().lowercase()  // Must have lowercase letters
.has().digits()     // Must have digits
.has().symbols()    // Must have special characters
.not().spaces();    // Should not have spaces


var util = require('util');
var encoder = require('./../../encoder/Encoder.js').JSONEncoder;
var ldap = require('ldapjs'),
    uuid = require('uuid');

var configManager = require('kea-config');
configManager.init('./config/main.conf.js');

var acl = require('./../../auth/AclManager.js').AclManager;

var response = require('./../helpers/response.js');

var crypto = require('crypto');
var openssl = require('openssl-wrapper');

var ldapclient = global.ldapClient.get_client();
var fs = require('fs');
var Handlebars = require('handlebars');

/**
 * Perform ldap operation
 * @param type string Operation type
 * @param email string user email
 * @param entry object User data
 * @param cb function Node style callback
 * @param res object Response object
 */
function performLDAPOperation(type, params ,cb, res) {
    try{
        var email = params.email;
        var key = params.key;
        var entry = params.data;
        if(!ldapclient){
            if(cb)
                cb({error: true, message: 'No connection to LDAP server.', status:500}, null);
            return;
        }

        var run = function (email, key, entry) {

            if(key) {
                var keycn = crypto.createHash('sha256').update(key).digest('hex');
                var dn='cn='+keycn+','+configManager.get('ldap.suffixApiKeys');
                switch(type) {
                    case 'add':
                        delete entry.dn;
                        ldapclient.add(dn, entry, function(err) {
                            if(err) {
                                if(err.name == 'EntryAlreadyExistsError'){
                                    global.log.info("ldap operation warning", err.message);
                                    err.status = 400;
                                    if(cb)
                                        cb(err, null);
                                } else {
                                    global.log.error("ldap operation failed", err.message);
                                    err.status = 400;
                                    cb(err, null);
                                }
                            } else {
                                global.log.info('Success adding api key', dn, JSON.stringify(entry));
                                if(cb)
                                    cb()
                            }
                        });
                        break;
                    case 'del':
                        ldapclient.del(dn, function(err) {
                            if(err) {
                                global.log.error("ldap operation del failed", err);
                                err.status = 400;
                                if(cb)
                                    cb(err, null);
                            }
                            else if(cb)
                                cb();
                        });
                        break;
                    default:
                        if(cb)
                            cb({error:true, message: "LDAP Operation Not found", status:400}, null);
                }
            } else if(email) {
                var dn='cn='+email+','+configManager.get('ldap.suffixUsers');
                switch(type) {
                    case 'add':
                        delete entry.dn;
                        ldapclient.add(dn, entry, function(err) {
                            if(err) {
                                if(err.name == 'EntryAlreadyExistsError'){
                                    global.log.info("ldap operation error "+err.name, err.message);
                                    err.status = 400;
                                    if(cb)
                                        cb(err, null);
                                } else {
                                    global.log.error("ldap operation failed", err.message);
                                    err.status = 400;
                                    cb(err, null);
                                }
                            }
                            else if(cb)
                                cb();
                        });
                        break;
                    case 'del':
                        ldapclient.del(dn, function(err) {
                            if(err) {
                                global.log.error("ldap operation del failed", err.message);
                                err.status = 400;
                                if(cb)
                                    cb(err, null);
                            }
                            else{
                                if(email)
                                    ldapclient.deleteUserKeys(email, cb);
                                if(cb)
                                    cb();
                            }
                        });
                        break;
                    case 'mod':
                        if(entry) {
                            ldapclient.modify(dn, entry, function(err) {
                                if(err){
                                    global.log.error("ldap operation mod failed", err.message);
                                    err.status = 400;
                                    if(cb)
                                        cb(err, null);
                                }
                                else if(cb)
                                    cb();
                            });
                        } else {
                            if(cb)
                                cb();
                            //client.unbind();
                        }
                        break;
                    case 'get':
                        if(dn) {
                            ldapclient.search(dn, {}, function(err, res) {

                                try {
                                    res.on('searchEntry', function(entry) {
                                        global.log.info('entry: ' + JSON.stringify(entry.object));
                                        if(cb)
                                            cb(null, entry.object);
                                    });
                                    res.on('searchReference', function(referral) {
                                        global.log.info('referral: ' + referral.uris.join());
                                    });
                                    res.on('error', function(err) {
                                        global.log.error('error: ' + err.message);
                                        err.status = 400;
                                        if(cb)
                                            cb(err, null);
                                    });
                                    res.on('end', function(result) {
                                        global.log.info('status: ' + result.status);
                                    });
                                } catch(e){
                                    global.log.error(e.message);
                                    e.status = 500;
                                    if(cb)
                                        cb(e, null);
                                }
                            });

                        } else {
                            if(cb)
                                cb(null, false);
                            //client.unbind();
                        }
                        break;
                    default:
                        if(cb)
                            cb({error:true, message: "LDAP Operation Not found", status:400}, null);
                }
            } else {
                if(cb)
                    cb({error:true, message: "Email missing", status:400}, null);
            }
        }

        if(!email && !key){
            // Fetch userid from ldap
            ldapclient.getEmailByUserid(entry, function (err, email) {
                run(email, key, entry);
            });
        } else {
            run(email, key, entry);
        }


    } catch (e) {
        if(cb)
            cb({error: true, message: e.message, status:500, data:e}, null);
    }

}

module.exports = {
    addUser: addUser,
    updateUser: updateUser,
    updateUserPassword: updateUserPassword,
    setUserPassword: setUserPassword,
    requestUserPasswordReset: requestUserPasswordReset,
    resetUserPassword: resetUserPassword,
    deleteUser: deleteUser,
    deleteUserFromLDAP: deleteUserFromLDAP,
    suspendUser: suspendUser,
    activateUser: activateUser,
    getUser: getUser,
    requestUserApiKey: requestUserApiKey,
    revokeUserApiKey: revokeUserApiKey,
    getAllUsers: getAllUsers,
    getAllSuspendedUsers: getAllSuspendedUsers,
    forgotPassword: forgotPassword
};

/**
 * Common function for sending email for add and activate user apis
 * @param {*} email - to email
 * @param {*} err 
 * @param {*} data 
 * @param {*} req 
 * @param {*} res 
 */
function emailPwdForAddOrActivateUser(email, password, data, req, res) {
    const mailOptions = configManager.get('password_send');
    const domainOptions = configManager.get('domain');
    // Read templates
    fs.readFile('./config/emails/' + mailOptions.text, 'utf8', function(err, text_body) {
        try {
            if (err) {
                global.log.error("Error sending user password", err.message);
                return response.Done({
                    error: true,
                    message: err.message,
                    status: 500
                }, null, res, req);
            }
            fs.readFile('./config/emails/' + mailOptions.html, 'utf8', function(err, html_body) {
                try {
                    if (err) {
                        global.log.error("Error sending user password", err.message);
                        return response.Done({
                            error: true,
                            message: err.message,
                            status: 500
                        }, null, res, req);
                    }
                    const text_template = Handlebars.compile(text_body);
                    const html_template = Handlebars.compile(html_body);
                    const tdata = {
                        url: 'https://' + domainOptions.ui + "/",
                        password: password,
                        email: email
                    };
                    global.log.info('To email ID:', email);
                    // Send email using AWS SES
                    helpers.sendEmailViaAWSSES(Array.of(email), mailOptions.subject, html_template(tdata), text_template(tdata), function(error, info) {
                        if (error) return response.Done(error, null, res, req);
                        else return response.Done(null, data, res, req);
                    });
                } catch (err) {
                    global.log.error("Error sending user password", err.message);
                    return response.Done(err, null, res, req);
                }
            });
        } catch (err) {
            global.log.error("Error sending user password", err.message);
            return response.Done(err, null, res, req);
        }
    });
}

/**
 * Add user
 * @param req Request
 * @param res Response
 */
function addUser(req, res) {
    var user = req.getCurrentUser();
    if(!user || !user.login || !user.login.userid || user.apiUser)
    {
        global.log.error("Access not granted", JSON.stringify(user));
        return response.Done({error:true,message: "Access not granted", status:403}, null, res, req);
    }

    var userid = uuid.v1();
    var email = req.body.email.toLowerCase();
    var name = req.body.name;
    if(!name)
        name = req.body.email.toLowerCase();
    var phone = req.body.phone;

    var params = {
        userprops: {
            userid: userid,
            email: email,
            name: name,
            //org: req.body.organization,
            roles: req.body.roles,
            sites: req.body.sites,
            title: req.body.title,
            phone: phone,
            type: req.body.type
        },
        /*siteprops: {
         siteid: siteid
         },*/
        orgprops: {
            orgid: req.swagger.params.orgid.value
        }
    };

    const password = generatePassword();
    
    var doneUserCrud = function(err, data) {
        if(!err){
            var entry =
            {
                dn:'cn='+email+','+configManager.get('ldap.suffixUsers'),
                objectClass: "inetOrgPerson",
                uid: data.userid,
                sn: name||email,
                mail: email,
                cn: email,
                userPassword: password
            };

            global.log.info("adding user to ldap", JSON.stringify(entry), JSON.stringify(params));
            performLDAPOperation('add',{email:email,data:entry},function(err, datax){
                if(err){
                    global.log.error('Error in ldap add entry:', err.message);
                    var params = {
                        userprops: {
                            userid: data.userid
                        },
                        /*siteprops: {
                         siteid: siteid
                         },*/
                        orgprops: {
                            orgid: req.swagger.params.orgid.value
                        }
                    };
                    doUserCrud(req,res,"delete",params,function(){
                        var ldap_error = {
                            message:'LDAP error '+err.name+': '+err.message,
                            error: true,
                            status: 400
                        };
                        response.Done(ldap_error ,null,res, req);
                    });                   
                } else {
                    if (configManager.get('config.sendmail.disabled') || (data.roles && ['partner_api', 'end_user_api'].indexOf(data.roles)!==-1)){
                        global.log.info('AddUser: Sending email is disabled or the user role is partner_api/end_user_api');
                        response.Done(null, data, res, req);
                    } else {            
                        emailPwdForAddOrActivateUser(email, password, data, req, res);
                    }
                }

            },res);   // Should throw global event if fails
        } else {
            if(!err.status){
                err.status = 400;
            }
            response.Done(err ,null,res, req);
        }
    }

    global.log.info("Adding user as user", user?JSON.stringify(user):null);
    acl.isAllowed(user.login.email, {model:'UserModel',action:'CAN_CREATE_ORG_USER', type:'createUser'}, function(err, allowed){
        if(err){
            response.Done({error:true, message:err.message,status:500},null,res, req);
        } else if(allowed){
            global.log.info("adding user to db", JSON.stringify(params));
            doUserCrud(req,res,"create",params,doneUserCrud);
        } else {
            response.Done({error:true, message:'Access not granted',status:403},null,res, req);
        }
    })

    // This is where LDAP code goes
        // Let's only store email/password in LDAP for now
        // End LDAP calls

}

/**
 * Delete user
 * @param req Request
 * @param res Response
 */
function deleteUser(req, res) {
    var user = req.getCurrentUser();
    if(!user || !user.login || !user.login.userid || user.apiUser)
    {
        global.log.error("Access not granted", user);
        return response.Done({error:true,message: "Access not granted", status:403}, null, res, req);
    }

    var userid = req.swagger.params.userid.value;
    var params = {
        userprops: {
            userid: req.swagger.params.userid.value
        },
        /*siteprops: {
         siteid: siteid
         },*/
        orgprops: {
            orgid: req.swagger.params.orgid.value
        }
    };
    var doneUserCrud = function() {
        // This is where LDAP code goes
        // End LDAP calls
        global.log.info("deleting user from ldap", userid);
        performLDAPOperation('del',{email:null,data:userid},function(err, data){
            response.Done(err, data, res, req);
        },res);   // Should throw global event if fails
    };

    acl.isAllowed(user.login.email, {model:'UserModel',action:'CAN_SUSPEND_ORG_USER', type:'deleteUser'}, function(err, allowed){
        if(err){
            response.Done({error:true, message:err.message,status:500},null,res, req);
        } else if(allowed){
            global.log.info("deleting user from db", JSON.stringify(params));
            doUserCrud(req,res,"delete",params,doneUserCrud);
        } else {
            response.Done({error:true, message:'Access not granted',status:403},null,res, req);
        }
    });

}

/**
 * Delete user from LDAP if not exists in database
 * @param req Request
 * @param res Response
 */
function deleteUserFromLDAP(req, res) {
    var user = req.getCurrentUser();
    if(!user || !user.login || !user.login.userid || user.apiUser)
    {
        global.log.error("Access not granted", JSON.stringify(user));
        return response.Done({error:true,message: "Access not granted", status:403}, null, res, req);
    }
    var email = req.body.email.toLowerCase();

    acl.isAllowed(user.login.email, {model:'UserModel',action:'CAN_SUSPEND_ORG_USER', type:'deleteUser'}, function(err, allowed){
        if(err){
            response.Done({error:true, message:err.message,status:500},null,res, req);
        } else if(allowed){
            global.log.info("deleting user from ldap", email);
            // Get userid from ldap for email...
            performLDAPOperation('get',{email:email,data:null},function(err, data){
                if(err ){
                    response.Done(err, data, res, req);
                    return;
                }
                var params = {
                    userprops: {
                        userid: data.uid
                    },
                    orgprops: {
                        orgid: req.swagger.params.orgid.value
                    }
                };
                // ..current user must have access to db and userid must not exist in db..
                doUserCrud(req,res,"get",params, function(err, data){
                    if(!err || data || (err.message.indexOf('not found') === -1)){
                        // Report error if user is in db
                        if(data){
                            return response.Done({error:true, message:"Can't delete user that exist in database",status:403},null,res, req);
                        } else {
                            return response.Done(err, data, res, req);
                        }
                    }
                    // ... Delete from LDAP
                    performLDAPOperation('del',{email:email},function(err, data){
                        response.Done(err, data, res, req);
                    },res);
                });
            }, res);
        } else {
            response.Done({error:true, message:'Access not granted',status:403},null,res, req);
        }
    });

}

/**
 * Delete user
 * @param req Request
 * @param res Response
 */
function suspendUser(req, res) {
    var user = req.getCurrentUser();
    if(!user || !user.login || !user.login.userid || user.apiUser)
    {
        global.log.error("Access not granted", user);
        return response.Done({error:true,message: "Access not granted", status:403}, null, res, req);
    }

    var userid = req.swagger.params.userid.value;
    var params = {
        userprops: {
            userid: req.swagger.params.userid.value
        },
        /*siteprops: {
         siteid: siteid
         },*/
        orgprops: {
            orgid: req.swagger.params.orgid.value
        }
    };
    var doneUserCrud = function() {
        // This is where LDAP code goes
        // End LDAP calls
        global.log.info("deleting user from ldap", userid);
        performLDAPOperation('del',{email:null,data:userid},function(err, data){
            response.Done(err, data, res, req);
        },res);   // Should throw global event if fails
    };


    acl.isAllowed(user.login.email, {model:'UserModel',action:'CAN_SUSPEND_ORG_USER', type:'deleteUser'}, function(err, allowed){
        if(err){
            response.Done({error:true, message:err.message,status:500},null,res, req);
        } else if(allowed){
            global.log.info("suspending user from db", JSON.stringify(params));
            doUserCrud(req,res,"suspend",params,doneUserCrud);
        } else {
            response.Done({error:true, message:'Access not granted',status:403},null,res, req);
        }
    });
}

/**
 * Delete user
 * @param req Request
 * @param res Response
 */
function activateUser(req, res) {
    var user = req.getCurrentUser();
    if(!user || !user.login || !user.login.userid || user.apiUser)
    {
        global.log.error("Access not granted", user);
        return response.Done({error:true,message: "Access not granted", status:403}, null, res, req);
    }

    var userid = req.swagger.params.userid.value;
    var params = {
        userprops: {
            userid: req.swagger.params.userid.value
        },
        /*siteprops: {
         siteid: siteid
         },*/
        orgprops: {
            orgid: req.swagger.params.orgid.value
        }
    };

    const password = generatePassword();

    var doneUserCrud = function(err, data) {
        if(!err){
            doUserCrud(req,res,"get",params, function(err, data){
                var entry =
                {
                    dn:'cn='+data.email+','+configManager.get('ldap.suffixUsers'),
                    objectClass: "inetOrgPerson",
                    uid: data.userid,
                    sn: data.name || data.email,
                    mail: data.email,
                    cn: data.email,
                    userPassword: password
                };

                global.log.info("adding user to ldap", JSON.stringify(entry), JSON.stringify(params));
                performLDAPOperation('add',{email:data.email,data:entry},function(err, datax){
                    if(err) {
                        global.log.error('Error in ldap add entry:', e.message);
                        return response.Done({error:true, message:'Error adding user ',status:400},null,res, req);
                    } else {
                        if (configManager.get('config.sendmail.disabled')){
                            global.log.info('ActivateUser: Sending email is disabled');
                            response.Done(null, data, res, req);
                        } else {             
                            emailPwdForAddOrActivateUser(data.email, password, data, req, res);
                        }
                    }                    
                },res);   // Should throw global event if fails
            });
        } else {
            if(!err.status){
                err.status = 400;
            }
            response.Done(err ,null,res, req);
        }
    }

    acl.isAllowed(user.login.email, {model:'UserModel',action:'CAN_SUSPEND_ORG_USER', type:'deleteUser'}, function(err, allowed){
        if(err){
            response.Done({error:true, message:err.message,status:500},null,res, req);
        } else if(allowed){
            global.log.info("activating user ", JSON.stringify(params));
            doUserCrud(req,res,"activate",params,doneUserCrud);
        } else {
            response.Done({error:true, message:'Access not granted',status:403},null,res, req);
        }
    });
}

/**
 * Update user
 * @param req Request
 * @param res Response
 */
function updateUser(req, res) {
    var user = req.getCurrentUser();
    if(!user || !user.login || !user.login.userid || user.apiUser)
    {
        global.log.error("Access not granted", user);
        return response.Done({error:true,message: "Access not granted", status:403}, null, res, req);
    }
    var userid = req.swagger.params.userid.value;
    var params = {
        userprops: {
            userid: userid,
            email: req.body.email.toLowerCase(),
            name: req.body.name,
            roles: req.body.roles,
            sites: req.body.sites,
            title: req.body.title,
            phone: req.body.phone
        },
        orgprops: {
            orgid: req.swagger.params.orgid.value
        }
    };

    acl.isAllowed(user.login.email, {model:'UserModel',action:'CAN_CHANGE_ORG_USER', type:'updateUser'}, function(err, allowed){
        if(err){
            response.Done({error:true, message:err.message,status:500},null,res, req);
        } else if(allowed){
            global.log.info("updating user in db", JSON.stringify(params));
            doUserCrud(req,res,"update",params,function(err, data){
                response.Done(err, data, res, req);
            });
        } else {
            response.Done({error:true, message:'Access not granted',status:403},null,res, req);
        }
    });
}

/**
 * Modify user password
 * @param req
 * @param res
 */
function updateUserPassword(req, res) {
    var user = req.getCurrentUser();
    if(!user || !user.login || !user.login.userid || user.apiUser)
    {
        global.log.error("Access not granted", JSON.stringify(user));
        return response.Done({error:true,message: "Access not granted", status:403}, null, res, req);
    }

    var userid = user.login.userid;
    var oldpass = req.body.old_password;
    var password = req.body.password;

    if(!schema.validate(password)) {
      global.log.error("New password too low complexity for user: ", JSON.stringify(user));
      return response.Done({error:true,message: "New password of too low complexity", status:403}, null, res, req);
    }

    var canChangePassword = function(err, ldapuser) {
        // This is where LDAP code goes
        var change = null;

        // will cause regression and allow change to same password, until enforced by policy on slapd-server
        if(!err && user && password) {
            change = new ldap.Change({
                operation: 'replace',
                modification: {
                    userPassword: password
                }
            });
            global.log.info("updating user password in ldap", userid,user.login.email);
            performLDAPOperation('mod',{email:user.login.email,data:change},function(err, data){
                if(err)
                    global.log.error("Could not set user data to ldap", JSON.stringify(user));
                response.Done(err, null, res, req);

            },res);
        } else {
            if(!err){
                var message = 'Could not set user password';
                err = {error: true, massage: message, status: 400};
            }
            if(!err.status)
                err.status = 400;
            global.log.error("Could not get user data", JSON.stringify(user));
            response.Done(err, null, res, req);
        }
    };

    if(user /*&& (
            //acl.isAllowed(user.login.email, {model:'UserModel',action:'CAN_CHANGE_PARTNER_USER', type:'updateUser'}).wait() ||
            //acl.isAllowed(user.login.email, {model:'UserModel',action:'CAN_CHANGE_ORG_USER', type:'updateUser'}).wait()
        )*/){
        //global.log.info('Get user data from ldap', user.login.email)
        performLDAPOperation('get',{email:user.login.email,data:null},canChangePassword, res);
    } else {
        response.Done({error:true, message:'Access not granted',status:403},null,res, req);
    }

}

/**
 * Modify user password
 * @param req
 * @param res
 */
function setUserPassword(req, res) {
    var user = req.getCurrentUser();
    if(!user || !user.login || !user.login.userid)
    {
        global.log.error("Access not granted", JSON.stringify(user));
        return response.Done({error:true,message: "Access not granted", status:403}, null, res, req);
    }

    var userid = user.login.userid;
    var password = req.body.password;

    if(!schema.validate(password)) {
      global.log.error("New password too low complexity for user: ", JSON.stringify(user));
      return response.Done({error:true,message: "New password of too low complexity", status:403}, null, res, req);
    }

    var canChangePassword = function(err, ldapuser) {
        // This is where LDAP code goes
        var change = null;
        if(!err && user && password) {
            change = new ldap.Change({
                operation: 'replace',
                modification: {
                    userPassword: password
                }
            });
            global.log.info("updating user password in ldap", userid,user.login.email);
            performLDAPOperation('mod',{email:user.login.email,data:change},function(err, data){
                if(err)
                    global.log.error("Could not set user data to ldap", JSON.stringify(user));
                response.Done(err, null, res, req);

            },res);
        } else {
            if(!err.status)
                err.status = 400;
            global.log.error("Could not get user data", JSON.stringify(user));
            response.Done(err, null, res, req);
        }
    };

    if(user /*&& (
            //acl.isAllowed(user.login.email, {model:'UserModel',action:'CAN_CHANGE_PARTNER_USER', type:'updateUser'}).wait() ||
            //acl.isAllowed(user.login.email, {model:'UserModel',action:'CAN_CHANGE_ORG_USER', type:'updateUser'}).wait()
        )*/){
        //global.log.info('Get user data from ldap', user.login.email)
        performLDAPOperation('get',{email:user.login.email,data:null},canChangePassword, res);
    } else {
        response.Done({error:true, message:'Access not granted',status:403},null,res, req);
    }

}

/**
 * Request password reset
 * @param req
 * @param res
 */
function requestUserPasswordReset(req, res) {
    var user = req.getCurrentUser();
    if(!user || !user.login || !user.login.userid)
    {
        global.log.error("Access not granted", user);
        return response.Done({error:true,message: "Access not granted", status:403}, null, res, req);
    }

    var email = req.body.email.toLowerCase();
    if (req.headers['api_key'])
    {
        return response.Done({error:true,message: "Method not allowed without valid user session", status:405}, null, res, req);
    }
    if(!email)
    {
        return response.Done({error:true,message: "Email missing", status:400}, null, res, req);
    }

    var key = uuid.v1();

    if(email){
        updateLDAPAndEmailPasswordReset(email, key, req, res);
    } else {
        response.Done({error:true, message:'Access not granted',status:403},null,res, req);
    }
}

/**
 * Request password reset
 * @param req
 * @param res
 */
function resetUserPassword(req, res) {
    if (req.headers['api_key'])
    {
        return response.Done({error:true,message: "Method not allowed without valid user session", status:405}, null, res, req);
    }
    var user = req.getCurrentUser();
    if(!user || !user.login || !user.login.userid)
    {
        global.log.error("Access not granted", user);
        return response.Done({error:true,message: "Access not granted", status:403}, null, res, req);
    }

    //var email = req.body.email.toLowerCase();
    var orgid = req.swagger.params.orgid.value;
    var userid = req.swagger.params.userid.value;

    if(!userid)
    {
        return response.Done({error:true,message: "userid missing", status:400}, null, res, req);
    }

    var key = uuid.v1();

    var onCanDo = function(){
        // Get user id from ldap
        if(userid){
            // Get user data from db

            var params = {
                userprops: {
                    userid: userid
                },
                /*siteprops: {
                 siteid: siteid
                 },*/
                orgprops: {
                    orgid: orgid
                }
            };

            global.log.info("fetching user data", JSON.stringify(params));
            doUserCrud(req,res,"get",params, function(err, duser){
                if(duser && !duser.error){
                    // Reset password in db
                    var change = new ldap.Change({
                        operation: 'replace',
                        modification: {
                            userPassword: generatePassword()
                        }
                    });
                    global.log.info("updating user password in ldap", userid,duser.email);
                    performLDAPOperation('mod',{email:duser.email,data:change},function(err, data){
                        if(err)
                            global.log.error("Could not set user data to ldap", JSON.stringify(duser));
                        else {                            
                            updateLDAPAndEmailPasswordReset(duser.email, key, req, res);
                        }                        
                    },res);
                } else {
                    response.Done({error:true, message:duser.message,status:duser.status},null,res, req);
                }
            });

        }
    };

    acl.isAllowed(user.login.email, {model:'UserModel',action:'CAN_CHANGE_ORG_USER', type:'updateUser'}, function(err, allowed){
        if(err){
            response.Done({error:true, message:err.message,status:500},null,res, req);
        } else if(allowed){

            acl.isAllowed(user.login.email, {model:'UserModel',action:'CAN_READ_PARTNER_USER', type:'getUser'}, function(err, allowed){
                if(err){
                    response.Done({error:true, message:err.message,status:500},null,res, req);
                } else if(allowed){
                    onCanDo();
                } else {

                    acl.isAllowed(user.login.email, {model:'UserModel',action:'CAN_READ_ORG_USER', type:'getUser'}, function(err, allowed){
                        if(err){
                            response.Done({error:true, message:err.message,status:500},null,res, req);
                        } else if(allowed){
                            onCanDo();
                        } else {
                            response.Done({error:true, message:'Access not granted',status:403},null,res, req);
                        }
                    });
                }
            });
        } else {

            acl.isAllowed(user.login.email, {model:'UserModel',action:'CAN_CHANGE_PARTNER_USER', type:'updateUser'}, function(err, allowed){
                if(err){
                    response.Done({error:true, message:err.message,status:500},null,res, req);
                } else if(allowed){
                    acl.isAllowed(user.login.email, {model:'UserModel',action:'CAN_READ_PARTNER_USER', type:'getUser'}, function(err, allowed){
                        if(err){
                            response.Done({error:true, message:err.message,status:500},null,res, req);
                        } else if(allowed){
                            onCanDo();
                        } else {

                            acl.isAllowed(user.login.email, {model:'UserModel',action:'CAN_READ_ORG_USER', type:'getUser'}, function(err, allowed){
                                if(err){
                                    response.Done({error:true, message:err.message,status:500},null,res, req);
                                } else if(allowed){
                                    onCanDo();
                                } else {
                                    response.Done({error:true, message:'Access not granted',status:403},null,res, req);
                                }
                            });
                        }
                    });
                } else {
                    response.Done({error:true, message:'Access not granted',status:403},null,res, req);
                }
            });
        }
    });

}

// POST /v3.0/forgot-password
function forgotPassword(req, res) {
    const email = req.body.email.toLowerCase();
    if (!email) {
        response.Done({ error: true, message: "Email missing", status: 400 }, null, res, req);
    } else {
        // Check if user is available in ldap
        performLDAPOperation('get', { email: email, data: null }, function (err, data) {
            if (err) {
                global.log.error("Forgot password request: User %s is not available in ldap. Email not sent for password reset.", email);
                // Return 204 response even when user email does not exist in ldap
                response.Done(null, null, res, req);
            } else {
                // Send email to reset password only when user account is available in ldap
                var key = uuid.v1();
                updateLDAPAndEmailPasswordReset(email, key, req, res);
            }
        }, res);
    }

}

// Email API Key of the user for password reset
function emailAPIKeyForPasswordReset(email, key, req, res) {
    if (configManager.get('sendmail.disabled')) {
        response.Done(null, null, res, req);
    } else {
        const domainOptions = configManager.get('domain');
        const url = 'https://' + domainOptions.ui + "/reset-password-request?key=" + key;
        // setup e-mail data with unicode symbols
        const mailOptions = configManager.get('password_reset');
        const emailBodyText = mailOptions.text.replace('[url]', url);
        const emailBodyHtml = mailOptions.html.replace('[url]', url);

        // Send email using AWS SES
        helpers.sendEmailViaAWSSES(Array.of(email), mailOptions.subject, emailBodyHtml, emailBodyText, function(error, info) {
            if (error) global.log.error("Error sending API Key", error);
            else global.log.info("API Key sent");
            response.Done(error, null, res, req);
        });
    }
}

// Update API Key of the user in LDAP for password reset
function updateLDAPAndEmailPasswordReset(email, key, req, res) {
    var keycn = crypto.createHash('sha256').update(key).digest('hex');
    var entry =
        {
            dn: 'cn=' + keycn + ',' + configManager.get('ldap.suffixApiKeys'),
            objectClass: ['inetOrgPerson', 'shadowAccount'],
            cn: 'cn=' + keycn,
            uid: email,
            sn: email,
            userPassword: key,
            shadowExpire: 18000
            // TODO: Set max privileges
        };

    global.log.info("Updating temp API key in ldap", email);
    performLDAPOperation('add', { key: key, data: entry }, function (err, data) {
        if (err) {
            global.log.error("Could not add temp API key", JSON.stringify(entry));

            response.Done({ error: true, message: err.message, status: 400 }, null, res, req);
        } else {
            global.log.info("Emailing api key", email);
            emailAPIKeyForPasswordReset(email, key, req, res);
        }
    }, res);
}

/**
 * Request password reset
 * @param req
 * @param res
 */
function requestUserApiKey(req, res) {
    var user = req.getCurrentUser();
    if(!user || !user.login || !user.login.userid)
    {
        global.log.error("Access not granted", user);
        return response.Done({error:true,message: "Access not granted", status:403}, null, res, req);
    }

    var email = req.body.email.toLowerCase();
    if(!email)
    {
        return response.Done({error:true,message: "Email missing", status:400}, null, res, req);
    }

    acl.getUserFromDb(email, function(err, user_data){

        if(err){
            return response.Done({error:true, message: err.message, status:500}, null, res, req);
        } else if(!user_data || !user_data.id){
            return response.Done({error:true,message: "User not found", status:404}, null, res, req);
        } else {
            var key = uuid.v1();
            // openssl rand -base64 37
            var generateKey = function(cb) {

                openssl.exec('rand', {'hex': 20}, function( error, buffer ) {
                    //console.log("rand:", buffer.toString());
                    if ( error ) {
                        global.log.error('Error generating api key: %s', error)
                        cb( error );
                    } else {
                        cb( null, buffer.toString().trim() );
                    }
                });
            };

            generateKey(function (err, key) {
                if (err) {
                    return response.Done({
                        error: true,
                        message: 'Could not generate API key. ' + err.message,
                        status: 500
                    }, null, res, req);
                } else {
                    var emailAPIKey = function(err, data) {
                        if(!err) {
                            if (configManager.get('sendmail.disabled')) {
                                // Don't send email
                                response.Done(null, {email: email, api_key: key, status: 200}, res, req);
                            } else {                               
                                let toEmailIds = Array.of(email);
                                if(email != user.login.email){
                                    // Send the email to both users (logged in & user for which API key is generated)                                    
                                    toEmailIds.push(user.login.email);
                                }
                                // setup e-mail data with unicode symbols
                                const mailOptions = configManager.get('api_key');                               
                                const emailBodyText = mailOptions.text.replace('[api_key]', key).replace('[user_email]', email);
                                const emailBodyHtml = mailOptions.html.replace('[api_key]', key).replace('[user_email]', email);

                                // Send email using AWS SES
                                helpers.sendEmailViaAWSSES(toEmailIds, mailOptions.subject, emailBodyHtml, emailBodyText, function(error, info) {
                                    if (error) global.log.error("Error sending API Key", error);
                                    else global.log.info("API Key sent");
                                    response.Done(error, {email: email, api_key: key, status: 200}, res, req);
                                });
                            }
                        } else {
                            response.Done({error:true, message: err.message, status:400}, null, res, req);
                        }
                    };

                    acl.isAllowed(user.login.email, {model:'UserModel',action:'CAN_GENERATE_API_KEY', type:'generateAPIKey'}, function(err, allowed){
                        if(err){
                            response.Done({error:true, message:err.message,status:500},null,res, req);
                        } else if(allowed){
                            var keycn = crypto.createHash('sha256').update(key).digest('hex');
                            var entry =
                                {
                                    dn:'cn='+keycn+','+configManager.get('ldap.suffixApiKeys'),
                                    objectClass: ['inetOrgPerson', 'shadowAccount'],
                                    cn: 'cn='+keycn,
                                    uid: email,
                                    sn: email,
                                    userPassword: key,
                                    shadowExpire: 1800000
                                    // TODO: Set requested privileges
                                };

                            global.log.info("Adding api key", key);
                            performLDAPOperation('add',{key:key,data:entry},function(err, datax){
                                global.log.info("Emailing api key", email);
                                emailAPIKey(null,null);
                            },res);   // Should throw global event if fails
                        } else {
                            response.Done({error:true, message:'Access not granted',status:403},null,res, req);
                        }
                    });
                }
            });


        }
    });

}

/**
 * Revoke API key
 * @param req
 * @param res
 */
function revokeUserApiKey(req, res) {
    var user = req.getCurrentUser();
    if(!user || !user.login || !user.login.userid)
    {
        global.log.error("Access not granted", user);
        return response.Done({error:true,message: "Access not granted", status:403}, null, res, req);
    }

    var key = req.body.key;
    if(!key)
    {
        return response.Done({error:true,message: "Key missing", status:400}, null, res, req);
    }



    acl.isAllowed(user.login.email, {model:'UserModel',action:'CAN_GENERATE_API_KEY', type:'generateAPIKey'}, function(err, allowed){
        if(err){
            response.Done({error:true, message:err.message,status:500},null,res, req);
        } else if(allowed){
            global.log.info("deleting user key from ldap", key);
            performLDAPOperation('del',{key:key},function(err, data){
                response.Done(err, data, res, req);
            },res);   // Should throw global event if fails
        } else {
            response.Done({error:true, message:'Access not granted',status:403},null,res, req);
        }
    });
}

/**
 * Get user data
 * @param req Request
 * @param res Response
 */
function getUser(req, res) {
    var user = req.getCurrentUser();
    if(!user || !user.login || !user.login.userid)
    {
        global.log.error("Access not granted", user);
        return response.Done({error:true,message: "Access not granted", status:403}, null, res, req);
    }
    var params = {
        userprops: {
            userid: req.swagger.params.userid.value
        },
        /*siteprops: {
            siteid: siteid
        },*/
        orgprops: {
            orgid: req.swagger.params.orgid.value
        }
    };
    acl.isAllowed(user.login.email, {model:'UserModel',action:'CAN_READ_ORG_USER', type:'getUser'}, function(err, allowed){
        if(err){
            response.Done({error:true, message:err.message,status:500},null,res, req);
        } else if(allowed){
            global.log.info("fetching user data", JSON.stringify(params));
            doUserCrud(req,res,"get",params, function(err, data){
                response.Done(err, data, res, req);
            });
        } else {
            response.Done({error:true, message:'Access not granted',status:403},null,res, req);
        }
    });
}

/**
 * Get all user data
 * @param req Request
 * @param res Response
 */
function getAllUsers(req, res) {
    var user = req.getCurrentUser();
    if(!user || !user.login || !user.login.userid)
    {
        global.log.error("Access not granted", user);
        return response.Done({error:true,message: "Access not granted", status:403}, null, res, req);
    }
    //var groupid = req.swagger.params.groupid.value;
    var siteid = (req.swagger.params.siteid)?req.swagger.params.siteid.value:null;
    var orgid = (req.swagger.params.orgid)?req.swagger.params.orgid.value:null;
    var params = { };
    //if(groupid)
    //    params.groupprops = {groupid: groupid}
    if(siteid)
        params.siteprops = {siteid: siteid}
    if(orgid)
        params.orgprops = {orgid: orgid}
    acl.isAllowed(user.login.email, {model:'UserModel',action:'CAN_READ_ORG_USER', type:'getUser'}, function(err, allowed){
        if(err){
            response.Done({error:true, message:err.message,status:500},null,res, req);
        } else if(allowed){
            global.log.info("fetching org users", JSON.stringify(params));
            doUserCrud(req,res,"getAllForOrg", params, function(err, data){
                response.Done(err, data, res, req);
            });
        } else {
            response.Done({error:true, message:'Access not granted',status:403},null,res, req);
        }
    });
}

/**
 * Get all user data
 * @param req Request
 * @param res Response
 */
function getAllSuspendedUsers(req, res) {
    var user = req.getCurrentUser();
    if(!user || !user.login || !user.login.userid)
    {
        global.log.error("Access not granted", user);
        return response.Done({error:true,message: "Access not granted", status:403}, null, res, req);
    }
    //var groupid = req.swagger.params.groupid.value;
    var siteid = (req.swagger.params.siteid)?req.swagger.params.siteid.value:null;
    var orgid = (req.swagger.params.orgid)?req.swagger.params.orgid.value:null;
    var params = { };
    //if(groupid)
    //    params.groupprops = {groupid: groupid}
    if(siteid)
        params.siteprops = {siteid: siteid}
    if(orgid)
        params.orgprops = {orgid: orgid}

    acl.isAllowed(user.login.email, {model:'UserModel',action:'CAN_SUSPEND_ORG_USER', type:'getUser'}, function(err, allowed){
        if(err){
            response.Done({error:true, message:err.message,status:500},null,res, req);
        } else if(allowed){
            global.log.info("fetching org suspended users", JSON.stringify(params));
            doUserCrud(req,res,"getAllSuspendedForOrg", params, function(err, data){
                response.Done(err, data, res, req);
            });
        } else {
            response.Done({error:true, message:'Access not granted',status:403},null,res, req);
        }
    });
}


/**
 * call user CRUD operation
 * @param req Request
 * @param res Response
 * @param operation
 * @param userprops
 * @param orgprops
 * @param done Callback
 */
function doUserCrud(req,res,operation,properties, done) {
    var type = operation+'User';
    var user = req.getCurrentUser();
    var data = {
        type: type,
        user: user,
        model: 'UserModel',
        id: user.userid,
        userid: user.userid
    };
    if(properties.groupprops)
        data.groupprops = properties.groupprops;
    if(properties.siteprops)
        data.siteprops = properties.siteprops;
    if(properties.orgprops)
        data.orgprops = properties.orgprops;
    if(properties.userprops)
        data.userprops = properties.userprops;

    var cb = function(err,data) {
        try{
            if(done /*&& !err*/)
                done(err, data);
            //response.Done(err,data, res);

        } catch (e) {
            response.Done({error:true, message: e.message, status:500, data:e},null,res, req);
        };
    };
    // We drop password here intentionally since it's only meant to go to LDAP
    //encoder.doUserCrud(user,type,data,cb);
    encoder.doSysCrud(user, operation, 'User', data, cb);
}

function generatePassword(){

    var char_table = {
        lowercase : {
            min: 97,
            max  : 122
        },
        uppercase : {
            min: 65,
            max  : 90
        },
        number : {
            min: 48,
            max  : 57
        },
        symbol : {
            min: 33,
            max  : 47
        }
    };

    var generateNumber = function(min, max) {
        return Math.floor(Math.random() * (max - min)) + min;
    }


    // Generated password
    var output    = [];
    // Should be good enough for temp password
    output.push(String.fromCharCode(generateNumber(char_table.lowercase.min, char_table.lowercase.max)));
    output.push(String.fromCharCode(generateNumber(char_table.lowercase.min, char_table.lowercase.max)));
    output.push(String.fromCharCode(generateNumber(char_table.number.min, char_table.number.max)));
    output.push(String.fromCharCode(generateNumber(char_table.uppercase.min, char_table.uppercase.max)));
    output.push(String.fromCharCode(generateNumber(char_table.lowercase.min, char_table.lowercase.max)));
    output.push(String.fromCharCode(generateNumber(char_table.number.min, char_table.number.max)));
    output.push(String.fromCharCode(generateNumber(char_table.lowercase.min, char_table.lowercase.max)));
    output.push(String.fromCharCode(generateNumber(char_table.uppercase.min, char_table.uppercase.max)));
    output.push(String.fromCharCode(generateNumber(char_table.lowercase.min, char_table.lowercase.max)));
    output.push(String.fromCharCode(generateNumber(char_table.number.min, char_table.number.max)));
    output.push(String.fromCharCode(generateNumber(char_table.symbol.min, char_table.symbol.max)));
    output.push(String.fromCharCode(generateNumber(char_table.lowercase.min, char_table.lowercase.max)));
    output.push(String.fromCharCode(generateNumber(char_table.uppercase.min, char_table.uppercase.max)));
    output.push(String.fromCharCode(generateNumber(char_table.lowercase.min, char_table.lowercase.max)));
    output.push(String.fromCharCode(generateNumber(char_table.symbol.min, char_table.symbol.max)));
    output.push(String.fromCharCode(generateNumber(char_table.number.min, char_table.number.max)));
    var offset = generateNumber(0, output.length);
    var str = output.join('')

    return str.substr(offset)+str.substr(0, offset);

}
