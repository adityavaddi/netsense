//"use strict";
/**
 *
 * @Example
 * message = {
  alertid: XYZ,
  name: 'My custom alert',
  active: true,
  rules: 'alert.connected < 0.2', // this is just an example we need to define which portion of query language we want to expose
  rules: 'alert.value >= 20132864 and (now-alert.time)>3000', // alert.name='Login'
  rules: 'alert.sensor='mPF' && alert.retries >= 5 && (now-alert.time)>3000', // alert.name='SensorSample'
  site: siteid,
  msg: 'This is my custom message to be delivered in case of {{alert.name}} alert ', // Handlear template (see https://github.com/wycats/handlebars.js  )
  email: 'someone@sensity.com',
  phone: '+381692144XXX'  // notify Dragan,
  schedule: "now.add(3, 'minutes')"
  schedule: "*\/3 * * * *"
  schedule: "new Date()"
}
 */

 /**
  * 
  *  Business alert:
  * {
        "businessAlertId": "5fc6cc67-aaf7-4063-864f-c5467551ac46",
        "triggerName": "average,occupancy,15,>,80",
        "triggerId": "6fc6cc67-aaf7-4063-864f-c5467551ac45",
        "triggerCategory": "parking",
        "triggerSubcategory": "group",
        "triggerUserId": "a8f6cc67-aaf7-4063-864f-c5467551ac45",
        "siteid": "dbc377a2-7880-4087-92ab-966a53d4810a",
        "orgid": "3881f299-2450-4f2c-a2d6-03635938b836",
        "resourceId": "3881f299-2450-4f2c-a2d6-03635938b836",
        "resourceName": "Mill Street Lot",
        "active": true,
        "createdOn": "2017-12-14T01:00:00.000Z",
        "lastUpdated": "2018-01-12T10:00:00.000Z",
        "lastClearedAt": "2018-01-16T11:00:00.000Z",
        "lastClearedBy": "8991f299-2450-4f2c-a2d6-03635938b836",
        "message": "The average occupancy percentage of the Marietta over 15 minutes is > 80%",
        "severity": "Major"
    }
  */

/**
 * businessalert.triggerCategory --> alert.name
 * businessalert.triggerName --> alert.type --> notification.notificationtype
 */

var configManager = require('kea-config');
configManager.setup('./config/');


var encoder = require('./../encoder/Encoder.js').JSONEncoder;
var messenger = require('./messenger.js');

var Schedule = require('node-schedule');
var Handlebars = require('handlebars');

var cronParser = require('cron-parser'),
    CronDate = require('cron-parser/lib/date');

var moment = require('moment-timezone');

var mngr = {
    send: send,
};
var ScheduledJobs = {},
    notificationJobInHoldOff = {};

/**
 * Send alert
 * @param alert objest Alert to send
 */
function send(valert, on_sent){
    // Check if it's a business alert
    if (valert.businessAlertId) {
        valert = assignBusinessAlertProperties(valert);
        global.log.info("Received business alert", valert);
    }
    // TODO: Get notifications for alertid on siteid
    encoder.getAllNotificationsByName('root', valert.name, valert.orgid, valert.siteid, function (err, notifications) {

        if(err){
            global.log.error("Could not send notification", err, notifications);
        } else {
            global.log.info("Found notifications", notifications, " for alert ", JSON.stringify(valert));
            for(var i=0;i< notifications.length; i++){

                global.log.info('holdoff', valert, notifications[i]);
                // Delay if needed
                if(notifications[i]){
                   holdOff(valert, notifications[i], on_sent, mainf);
                }
            }
        }

    });
}

/**
 * Assign business alert properties to alert
 * @param {*} alert 
 */
function assignBusinessAlertProperties(alert) {
    if (alert.businessAlertId) {
        alert.alertid = alert.businessAlertId;       
        alert.name = alert.triggerCategory; // alert name
        alert.type = alert.triggerName; // alert type        
    }
    return alert
}

/**
 * Notification processor
 * @param alert
 * @param notification
 * @param on_sent callback
 */
function mainf(alert, notification, on_sent) {
    // After hold_off time
    global.log.info('Processing', notification, alert);
    const isNotificationInHoldOff = notificationJobInHoldOff[alert.alertid + '-' + notification.notificationid];
    if (isNotificationInHoldOff)
        // Delete it from notificationInHoldOff object. So, it can be used when the same alert is active again
        delete notificationJobInHoldOff[alert.alertid + '-' + notification.notificationid];
    // Fetch alert from db
    encoder.getNotificationSys('root', notification.notificationid, alert.orgid, alert.siteid, function (err, dnotification) {
        if(dnotification ){
            // Should be scheduled?
            if(shouldSchedule(alert, dnotification)) {
                global.log.info('Scheduling', alert, dnotification);
                schedule(alert, dnotification, on_sent);
            }
            const isBusinessAlert = alert.businessAlertId ? true : false;
            // Don't need to get the latest alert status if the notification hold off is 0
            if(dnotification.hold_off === 0 && isMatchingRules(alert, dnotification) && shouldSend(alert, dnotification)) {                            
                prepareNotificationAndSend(alert, dnotification, on_sent);
            } else {                
                // Get the latest alert status
                encoder.getAlertSys(isBusinessAlert, 'root', alert.alertid, alert.orgid, alert.siteid, function (err, ralert) {
                    if (!err) {
                        ralert = isBusinessAlert ? assignBusinessAlertProperties(JSON.parse(JSON.stringify(ralert))) : ralert;                        
                        if (ralert !== null && ralert.alertid && isMatchingRules(ralert, dnotification) && shouldSend(ralert, dnotification)) {
                            prepareNotificationAndSend(ralert, dnotification, on_sent);                            
                        }
                    } else {
                        global.log.error('Error fetching alert: %s', err.message, err);
                    }
                });
            }
        } else {
            global.log.error("No notification %s found", notification.notificationid);
        }
    });
}

/**
 * Extracted it as separate function as it's used more than once
 * @param {*} alert 
 * @param {*} notification 
 * @param {*} on_sent 
 */
function prepareNotificationAndSend(alert, notification, on_sent) {
    prepareNotification(alert, notification, function sendFromScheduledTask(err, cnotification) {
        if (!err) {
            messenger.sendAsync(cnotification, on_sent);
        } else
            global.log.error('Error sending notification: %s', err.message, err);
    });
}

/** 
 * Called at regular interval based on resend interval of notification 
 * Refer setInterval in schedule() function
 * @param {*} palert 
 * @param {*} pnotification 
 * @param {*} on_sent 
 */
function scheduledNotification(palert, pnotification, on_sent) {
    const notyResendInterval = pnotification.resend_interval;
    global.log.info('Woke up  notification %s for alert %s', pnotification.notificationid, palert.alertid);
    encoder.getNotificationSys('root', pnotification.notificationid, palert.orgid, palert.siteid, function (err, rnotification) {
        if (rnotification) {
            const newNotyResendInterval = rnotification.resend_interval;
            const isBusinessAlert = palert.businessAlertId ? true : false;            
            encoder.getAlertSys(isBusinessAlert, 'root', palert.alertid, palert.orgid, palert.siteid, function (err, ralert) {
                if (!err) {
                    ralert = isBusinessAlert ? assignBusinessAlertProperties(JSON.parse(JSON.stringify(ralert))) : ralert;
                    // Cancel job if not matching rules and exit
                    if (ralert == null || !ralert || !ralert.alertid || !isMatchingRules(ralert, rnotification)) {
                        global.log.info('Canceled  notification %s for alert %s', rnotification.notificationid, (ralert) ? ralert.alertid : palert.alertid);
                        stopScheduledJob(palert, rnotification);
                    } else if (shouldSend(ralert, rnotification)) {
                        prepareNotificationAndSend(ralert, rnotification, on_sent);                       
                        // Check for resend interval change on notificaiton
                        if (newNotyResendInterval != notyResendInterval) {
                            stopScheduledJob(ralert, rnotification);
                            if (newNotyResendInterval > 0) {
                                global.log.info('Re-schedule notification %s with new resend interval %s seconds', rnotification.notificationid, rnotification.resend_interval);
                                // Schedule notification with new resend interval
                                schedule(ralert, rnotification, on_sent);
                            } else
                                global.log.info('Canceled notification %s for alert %s based on new resend interval (0 sec)', rnotification.notificationid, ralert.alertid);
                        }
                    }
                }
                else {
                    global.log.error('Error fetching alert: %s', err.message, err);
                    global.log.info('Canceled notification %s for alert %s', pnotification.notificationid, palert.alertid);
                    stopScheduledJob(palert, rnotification);
                }

            });
        } else {
            // If scheduled should be canceled
            global.log.info('Canceled inactive notification %s for alert %s', pnotification.notificationid, palert.alertid);
            stopScheduledJob(palert, pnotification);
        }
    });
}

/**
 * Stop scheduled job 
 * @param {*} salert 
 * @param {*} snotification 
 */
function stopScheduledJob(salert, snotification) {

    const job = ScheduledJobs[salert.alertid + '-' + snotification.notificationid];
    if (job)
        clearInterval(job);
    // Deleting the job to make sure it can  be scheduled when the same alert is active again.
    delete ScheduledJobs[salert.alertid + '-' + snotification.notificationid];
}
/**
 * Check alert against the holdoff rules
 * @param alert object Alert with data
 * @param notification object Notification with rules
 * @param callback function Callback
 * @returns {boolean}
 */
function holdOff(alert, notification, on_sent, callback) {
    var result = false;
    var delay = 0;
    const key = alert.alertid + '-' + notification.notificationid;
    if (alert && notification && notification.hold_off > 0) {
        delay = notification.hold_off * 1000;
        if (notificationJobInHoldOff[key]) {
            // Clear the previously scheduled job
            clearTimeout(notificationJobInHoldOff[key]);
            global.log.info('Reset hold off of notification %s for alert %s as this is the recent alert within hold off', notification.notificationid, alert.alertid);
        }
        global.log.info("Defer sending notification for " + notification.hold_off + " seconds for alert", JSON.stringify(alert));
    }
    notificationJobInHoldOff[key] = setTimeout(callback, delay, alert, notification, on_sent);
    return notificationJobInHoldOff[key];
}

/**
 * Check alert against the notification rules
 * @param alert object Alert with data
 * @param notification object Notification with rules
 * @returns {boolean}
 */
function shouldSchedule(alert, notification) {
    if(!alert){
        global.log.warn("Should not schedule empty alert for notification ", JSON.stringify(notification));
        return false;
    }
    if( !notification.active ){
        global.log.info("Don't schedule inactive notification : %s", notification.notificationid);
        return false;
    }
    var matches_types = false;
    if(!notification.notificationtype || notification.notificationtype.indexOf(alert.type)!=-1)
    {
        matches_types = true;
    }
    // Check is resend_interval valid, not exists, and matches against rules
    if (notification && notification.resend_interval>0 && !ScheduledJobs[alert.alertid+'-'+notification.notificationid] && matches_types){
        global.log.info("Should be scheduled ", JSON.stringify(alert), JSON.stringify(notification));
        return true;
    }
    global.log.info("Should not be scheduled ", JSON.stringify(alert), JSON.stringify(notification));
    return false;
}

/**
 * Create jobe if otification has schedule rule
 * @param alert Alert
 * @param notification Notification
 * @param callback Callback executet on error or
 */
function schedule(alert, notification, on_sent){
    // TODO: determine rule based on alert name and other params
    var schedule = notification.resend_interval>0?notification.resend_interval:0;
    if(
        schedule
    ) {
        try {

            schedule = '*/'+schedule+' * * * * *';
        }  catch(e){
            global.log.error('Error evaluating schedule \'%s\'', notification.resend_interval, e)

            callback({error:true, message: 'Error evaluating schedule \'%s\''+ notification.resend_interval}, alert, notification);
        }
        var n = notification;
        n.schedule = null;
        var a = alert;
        var delay = notification.resend_interval*1000;
        if(delay<1000)
            delay = 1000;   // min 1 sec
        if(delay>2147483647)
            delay = 2147483647; // about 24.8 days
        var key = alert.alertid+'-'+notification.notificationid;
        // Set if not scheduled already
        if(!ScheduledJobs[key]){
            ScheduledJobs[key] = setInterval(function(){
                scheduledNotification(a, n, on_sent);
            }, delay);

            global.log.info('Created new job \'%s\' with delay %s', key, delay)
        }
        /*ScheduledJobs[alert.alertid+'-'+notification.notificationid] = Schedule.scheduleJob(alert.alertid+'-'+notification.notificationid, schedule, function(){
            callback(null, a, n);
        })*/
        global.log.info('Scheduled \'%s\' for %s', alert.alertid, notification.notificationid, delay)

    }

}


/**
 * Check alert against the notification rules
 * @param alert object Alert with data
 * @param notification object Notification with rules
 * @returns {boolean}
 */
function shouldSend(alert, notification){
    if( !notification.active ){
        global.log.info("Skipped inactive notification : %s", notification.notificationid);
        return false;
    }
    var result = false;
    // TODO: Validate alert e.g. alert.name == notification.name
    if(!notification.window || notification.window=='* * * * * *' || notification.window.indexOf('* * ')===0)
        return true;
    // Check against notification.window
    var wparts = notification.window.split(' ');
    if(wparts.length!==3){
        var interval = cronParser.parseExpression(notification.window);
        var next = interval.next();
        var now = (new Date).getTime();
        if(Math.abs(now - next.getTime())<60*1000)
            return true;
    } else {
        // '@ hrs day tz'
        var win = notification.window.split(' '),
            hrs = win[0] || '*',
            day = win[1] || '*',
            tz  = win[2] || 'UTC',
            now = moment().tz(tz),
            matchedHours = true,
            matchedDays = true;
        if(hrs!='' && hrs!='*'){
            var parts = hrs.split('-'),
                from = parts[0],
                to = parts[1],
                match=now.format('HH:mm');
            if(from.length<3)
                from = from+':00';
            if(from.length===4)
                from = '0'+from;
            if(to.length<3)
                to = to+':00';
            if(to.length===4)
                to = '0'+to;
            matchedHours = from<=match && to>=match;
            global.log.info("matchedHours :", hrs,day,tz,now,from,to,match);

        }
        if(day!='' && day!='*'){
            var days = day.split(','),
                match = now.format('E');
            matchedDays = days.indexOf(match)!==-1;
            global.log.info("matchedDays :",days, match );
        }
        global.log.info(notification.window+" Should send matchedHours %d, matchedDays %d ",matchedHours, matchedDays);

        return matchedHours && matchedDays;
    }


    return false;
}

/**
 * Check alert against the notification rules
 * @param alert object Alert with data
 * @param notification object Notification with rules
 * @returns {boolean}
 */
function isMatchingRules(alert, notification){
    if( !notification.active ){
        global.log.info("isMatchingRules: Inactive notification found: %s", notification.notificationid);
        return false;
    }

    // Ignore type and severity if rules set
    if(!notification.rules ){
        // No
        global.log.info("Checking type / severity for ", alert, notification);

        var matches_types = false;
        if(!notification.notificationtype || notification.notificationtype.indexOf(alert.type)!=-1)
        {
            matches_types = true;
        }
        var matches_severity = false;
        if(!notification.severity || notification.severity.indexOf(alert.severity)!=-1)
        {
            matches_severity = true;
        }

        global.log.info("isMatchingRules: ", notification.notificationtype, alert.type, matches_types,  notification.severity, alert.severity, matches_severity);
        return matches_types && matches_severity;
    }

    var result = false;
    // Check against notification.rules
    var now = (new Date).getTime();
    // TODO: Do we need anything else, maybe alert specific data
    var notConnected = function(nodeid){
        // TODO: Fetch node connection status
        return false;
    }
    try{
        // alert.name=='DeviceAlarm' && alert.type=='HWFail_EEPROM' && alert.severity=='Major';
        // TODO: Check why do I need this
        var rules =  (notification.rules)?notification.rules.replace(/\&amp\;/g,'\&'):'false'
        // Evaluate
        var result = eval(' '+rules);
        global.log.trace('Rule "%s" evaluated as ', rules, result);
    } catch(e){
        global.log.error("Error evaluating notification rules: %s", e.message, e);
        return false;
    }

    return result;
}

/**
 * Prepare notification for messenger
 * @param alert object Alert with data
 * @param notification functon Node callback function
 */
function prepareNotification(alert, notification, callback){
    var err = null;
    var result = '';
    var data;
    try {
        // TODO: Prepare message e.g. using handlebars if possible
        var source = (notification && notification.msg) || '';
        var template = Handlebars.compile(source);

        //var node = {},
        //    site = {},
        //    org = {};
        // TODO: get node, site, org if needed. Do we need anything else?

        data = {
            alert: alert,
            notification: notification,
            time: moment().format(),
            //node: node,
            //site:site,
            //org: org
        };

        data.msg = template(data);
        global.log.info('Sending message', data);
    } catch (e){
        err = e;
        global.log.info("error %s", e.message, e);
    }
    callback && callback(err, data);
}



// --- alert specific functions ---------------
/**
 * Check if node is not connected
 * @param nodeid
 * @returns {boolean}
 */
function notConnected(nodeid){
    var node=null;
    // TODO: synchronously fetch node connection status
    if(!node /*|| !node.connected*/){
        return true;
    } else
        return false;
}

module.exports = mngr;
