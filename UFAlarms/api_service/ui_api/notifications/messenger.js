//"use strict";
/**
 * @Example
 * message = {
  alertid: XYZ,
  name: 'My custom alert',
  active: true,
  rules: 'connected < 20%', // this is just an example we need to define which portion of query language we want to expose
  site: siteid,
  msg: 'This is my custom message to be delivered in case of this alert',
  email: 'someone@sensity.com',
  phone: '+381692144XXX'  // notify Dragan
}
 */
var configManager = require('kea-config');
configManager.setup('./config/');

var encoder = require('./../encoder/Encoder.js').JSONEncoder;

const helpers = require('./../api/helpers/helpers.js');


var mngr = {
};

/**
 * Send message asynchronously
 * @param message object
 * @param callback function Callback function
 * @returns {*}
 */
mngr.sendEmail = function(message, callback){
    try {
        if(!message){
            var err = new Error('Message missing');
            err.error = true; err.status = 400;
            if(callback)
                callback(err, null);
            return err;
        }
        const emailOptions = configManager.get('notifications.email')
        const emailSubject = emailOptions.subject + ' - ' + message.notification.name;

        const emailUsersList = message.notification.emailUsersList;
        getUserEmailsPhones(emailUsersList, function(err, users){
            const additionalEmails = message.notification.additionalEmails.filter(email => email.trim()); // remove empty or null values
            if((!additionalEmails || !additionalEmails.length) && ( !users||!users.length)) {
                if(callback)
                    callback(null, message);
                return;
            }
            if(!users) users = [];
            let toEmailIds = [];
            if(message.alert.name.toLowerCase() === 'devicealarm') {
                // Read user type and check for `displaytocustomer` or `displaytopartner` flag in alert
                for(let i = users.length - 1; i >= 0; --i) {
                    const user = users[i];
                    const userType = 'customer';
                    if (user.type.includes('sensity_')) userType = 'sensity';
                    else if(user.type.includes('partner_')) userType = 'partner';
                    global.log.info(`Device Alaram Email: User type is ${user.type} for userid: ${user.userid}`);
                    switch (userType) {
                        case 'sensity':
                            toEmailIds.push(user.email);
                            break;
                        case 'partner':
                            if(message.alert.displaytopartner) toEmailIds.push(user.email);
                            break;
                        default: // consider it as customer
                            if(message.alert.displaytocustomer) toEmailIds.push(user.email);
                    }                 
                }
            } else toEmailIds = users.map(item => item.email);

            if (additionalEmails && additionalEmails.length) toEmailIds = toEmailIds.concat(additionalEmails);
            global.log.info('To email IDs:', toEmailIds);
            if (toEmailIds.length) {
                // Send email using AWS SES
                helpers.sendEmailViaAWSSES(toEmailIds, emailSubject, message.msg, message.msg, function(err, data){
                    if(err) callback(err, null);
                    else callback(null, null);
                });
            } else {
                // No email ids to send
                callback(null, null);
            }
        });            
    } catch (e) {
        callback({error: true, message: e.message});
    }
};

/**
 * Send message asynchronously
 * @param message object
 * @param callback function Callback function
 * @returns {*}
 */
mngr.sendSms = function(message, callback){
    try {

        var smsOptions = configManager.get('notifications.sms');
        if(!smsOptions || smsOptions.disabled || !message){
            var err = new Error('Message missing');
            err.error = true; err.status = 400;

            if(callback)
                callback(err, null);
            return err;
        }
        var smsUsersList = message.notification.smsUsersList;
        getUserEmailsPhones(smsUsersList, function(err, users){

            // setup sms data
            if(!users)
                users = [];
            let smsNubmers = users.map( function(item){
                if(message.alert.name.toLowerCase() === 'devicealarm') {
                    global.log.info(`Device Alaram SMS: User type is ${item.type} for userid: ${item.userid}`);
                    if (item.type.includes('sensity_')) return item.phone;
                    else if(item.type.includes('partner_') && message.alert.displaytopartner) return item.phone;
                    else if(message.alert.displaytocustomer) return item.phone;;
                } else {
                    return item.phone;
                }
            });
            global.log.info('SMS Numbers:', smsNubmers);           
            
            let errors = [], success = [];
            smsNubmers.forEach(function (phone) {
                try {
                    if(phone.trim()) {
                        helpers.sendSmsViaAWSSNS(phone, message, function (err) {
                            (err) ? errors.push(err) : success.push(phone);
                            const found = success.length + errors.length;
                            if ((found >= smsNubmers.length) && callback) {
                                if (errors && errors.length) {
                                    callback({ error: true, message: "Error: could not send some messages", status: 500 }, errors);
                                } else {
                                    callback(null, null);
                                }
                            }
                        });
                    } else {
                        // Phone number is not available or empty
                        callback(null, null);
                    }
                } catch (snserr) {
                    errors.push(snserr);
                }
            });
        });
    } catch (e) {
        callback({error: true, message: e.message});
    }
};

/**
 * Send message asynchronously
 * @param message object
 * @param callback function Callback function
 * @returns {*}
 */
mngr.sendAsync = function(message, callback){
    try {

        var notificationOptions = configManager.get('notifications');

        if (!notificationOptions.email.disabled) {
            mngr.sendEmail(message, function(err, message){
                if(err){
                    global.log.error('Email notification error: ', err);
                }
            });
        }
        if (!notificationOptions.sms.disabled) {
            mngr.sendSms(message, function(err, message){
                if(err){
                    global.log.error('SMS Notification error: ', err);
                }
            })
        }
        callback && callback(null, message);

    } catch (e) {
        callback({error: true, message: e.message});
    }
};

/**
 * Send message asynchronously
 * @param message object
 * @param callback function Callback function
 * @returns {*}
 */
getUserEmailsPhones = function(userids, done){
    try {
        if( !userids || !userids.length)
        {
            if(done)
                return done(null, null);
            return;
        }

        var cb = function(err,data) {
            try{
                if(done )
                    done(err, data);

            } catch (e) {
                if(done /*&& !err*/)
                    done({error:true, message: e.message, status:500, data:e},null);
            };
        };
        // We drop password here intentionally since it's only meant to go to LDAP
        //encoder.doUserCrud(user,type,data,cb);
        encoder.getUserEmails(userids, cb);


    } catch (e) {
        done({error: true, message: e.message});
    }
};


module.exports = mngr;

