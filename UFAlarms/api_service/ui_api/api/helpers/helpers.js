'use strict';

require("babel-polyfill"); // To use ES6 specific methods of array
const encoder = require('./../../encoder/Encoder.js').JSONEncoder;
const configManager = require('kea-config');
configManager.setup('./config/');

const awsKeys = configManager.get('notifications.awsKeys');
const emailOptions = configManager.get('notifications.email');
// AWS credentials
let AWS = require('aws-sdk');
AWS.config.region = awsKeys.aws_region;
AWS.config.accessKeyId = awsKeys.aws_access_key_id;
AWS.config.secretAccessKey = awsKeys.aws_secret_access_key;

// import { parse, format, AsYouType } from 'libphonenumber-js';
// https://github.com/catamphetamine/libphonenumber-js
const parse = require('libphonenumber-js').parse;
const format = require('libphonenumber-js').format;

module.exports = {
    sendEmailViaAWSSES,
    sendSmsViaAWSSNS,
    addUserNameForUserId,
    addUserNamesForUserIds,
    getUserType
}

/**
 * Send email using AWS SES service 
 * @param {*} toEmailIds - array of email Ids
 * @param {*} emailSubject 
 * @param {*} emailBodyHtml 
 * @param {*} emailBodyText 
 * @param {*} callback 
 */
function sendEmailViaAWSSES(toEmailIds, emailSubject, emailBodyHtml, emailBodyText, callback) {
    const SES = new AWS.SES();
    const params = {
        Destination: {
            ToAddresses: toEmailIds            
        },
        Source: emailOptions.from,
        Message: {
            Body: {
                Html: {
                    Charset: 'UTF-8',
                    Data: emailBodyHtml                     
                },
                Text: {
                    Charset: 'UTF-8',
                    Data: emailBodyText
                }
            },
            Subject: {
                Data: emailSubject
            }
        }
    };
    SES.sendEmail(params, function(err, data) {
        if (err) {
            global.log.error('Email Notification failed:', err.stack); // error
            callback(err);
        } else {
            global.log.info('Email Notification sent:', data); // Success
            callback(null, data);
        }
    });
}

/**
 * Send SMS using AWS SNS 
 * @param {*} phone - phone number
 * @param {*} message - SMS message
 * @param {*} callback 
 */
function sendSmsViaAWSSNS(phone, message, callback) {
    try {       
        const sns = new AWS.SNS();
        // Parse and format phone into `E.164` number format (+1XXXXXXXXXX) as per AWS recommendation
        const formattedPhoneNumber = format(parse(phone, 'US').phone, 'US', 'E.164');
        // Prepare data
        const smsData = {
            Message: message.msg, // Message to be sent in SMS
            MessageStructure: 'string', // Can be string/json
            PhoneNumber: formattedPhoneNumber // Phone number to which SMS message has to be delivered
        }
        // Send sms
        sns.publish(smsData, function (err, data) {
            global.log.info('SMS data:', smsData);
            if (err) {
                global.log.error('SMS Notification failed:', err.stack);
                callback(err);
            } else {
                global.log.info('SMS Notification sent:', data);
                callback(null, null);
            }
        });
    } catch (e) {
        callback({ error: true, message: e.message });
    }
}

/**
 * Get user name of user id and add it to response.
 * @param {*} res - response from MS
 * @param {*} userIdKey - user id key in the reponse from MS
 * @param {*} userNameKey - user name key to be added in the response
 * @param {*} callback
 */
function addUserNameForUserId(res, userIdKey, userNameKey, callback) {
    let response = JSON.parse(JSON.stringify(res));
    global.log.info('addUserNameForUserId response:', response);
    if (Array.isArray(response)) {
        const uniqueUserIds = [...(new Set(response.map(elem => elem[userIdKey])))];
        if (uniqueUserIds.length > 0 && typeof uniqueUserIds[0] !== 'undefined') {
            encoder.getUserEmails(uniqueUserIds, function (err, users) {
                global.log.info('addUserNameForUserId users:', users);
                if (!err && (users && users.length > 0)) {
                    for (let item of response) {
                        const user = users.find(u => u.userid === item[userIdKey]);
                        if (user) item[userNameKey] = user.name; // add user name
                    }
                }
                callback(response);
            });
        } else {
            callback(response);
        }
    } else {
        const userId = response[userIdKey];
        if (userId) {
            encoder.getUserEmail(userId, function (err, user) {
                global.log.info('addUserNameForUserId user:', user);
                if (!err) {
                    if (user.name) response[userNameKey] = user.name; // add user name
                }
                callback(response);
            });
        } else {
            callback(response);
        }
    }
}

/**
 * Get user names of the user ids and add it to response.
 * @param {*} res - response from MS
 * @param {*} userIdKeys [] - user id keys in the reponse from MS
 * @param {*} userNameKeys [] - user name keys to be added in the response
 * @param {*} callback
 */
function addUserNamesForUserIds(res, userIdKeys, userNameKeys, callback) {
    let response = JSON.parse(JSON.stringify(res));
    global.log.info('addUserNamesForUserIds response:', response);
    if (Array.isArray(response)) {
        addUserNamesToResArray(response, userIdKeys, userNameKeys, callback);
    } else {
        const uniqueUserIds = getUniqueUserIds(response, userIdKeys);
        if (uniqueUserIds.length > 0 && typeof uniqueUserIds[0] !== 'undefined') {
            encoder.getUserEmails(uniqueUserIds, function (err, users) {
                global.log.info('addUserNamesForUserIds users:', users);
                if (!err && (users && users.length > 0)) {
                    response = addUserNamesToObj(response, userIdKeys, userNameKeys, users);
                }
                callback(response);
            });
        } else {
            callback(response);
        }
    }
}

// Below functions are not exported as these are only used in helpers.js
/**
 * Use this only when userIdKeys is an array
 * @param {*} response
 * @param {*} userIdKeys - array ['triggerUserId', 'lastClearedBy',..]
 */
function getUniqueUserIds(response, userIdKeys) {
    let userIds = [];
    if (Array.isArray(response)) {
        for (let res of response) {
            for (let userid of userIdKeys) {
                if (res[userid]) userIds.push(res[userid]);
            }
        }
    } else {
        for (let userid of userIdKeys) {
            if (response[userid]) userIds.push(response[userid]);
        }
    }
    return [...new Set(userIds)] // To remove duplicate userids
}

// To iterate over user ids in an obj and add usernames to it
function addUserNamesToObj(resObj, userIdKeys, userNameKeys, users) {
    userIdKeys.forEach(function (userid, index) {
        const user = users.find(u => u.userid === resObj[userid]);
        if (user) resObj[userNameKeys[index]] = user.name; // add user name
    });
    return resObj;
}

// Use this only when resArray and userIdKeys are an array of objects
function addUserNamesToResArray(resArray, userIdKeys, userNameKeys, callback) {
    const uniqueUserIds = getUniqueUserIds(resArray, userIdKeys);
    if (uniqueUserIds.length > 0 && typeof uniqueUserIds[0] !== 'undefined') {
        encoder.getUserEmails(uniqueUserIds, function (err, users) {
            global.log.info('addUserNamesToResArray users:', users);
            if (!err && (users != undefined && users.length > 0)) {
                for (let item of resArray) {
                    item = addUserNamesToObj(item, userIdKeys, userNameKeys, users);
                }
            }
            callback(resArray);
        });
    } else {
        callback(resArray);
    }
}

/**
 * Below is the categorization for the user type:

 UserType == sensity
    sensity_admin
    sensity_read_only
    sensity_user

 UserType == partner
    partner_admin
    partner_deployment_user
    partner_lighting_user
    partner_sensor_user
    partner_networking_user
    partner_read_only
    partner_api

 UserType == customer
    end_user_admin
    end_user_lighting_user
    end_user_sensor_user
    end_user_networking_user
    end_user_read_only
    end_user_api
    parking_owner_admin
    parking_manager
    policy_authority
    installer

 * @param {*} user
 */
function getUserType(user) {
    try {
        let returnUserType = null;
        if(user && user.authorization) {
            const userType = user.authorization[0].type; //Get user type from first authorization object
            if (userType.includes('sensity_')) returnUserType = 'sensity';
            else if (userType.includes('partner_')) returnUserType = 'partner';
            else returnUserType = 'customer';
        }
        return returnUserType;
    } catch (e) {
        global.log.error('Exception in getUserType: ', e.message);
        return null;
    }
}