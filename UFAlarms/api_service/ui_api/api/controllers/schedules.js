'use strict';

var util = require('util'),
    encoder = require('./../../encoder/Encoder.js').JSONEncoder,
    uuid = require('uuid');

var response = require('./../helpers/response.js');

module.exports = {
    getAllSchedules: getAllSchedules,
    getSchedule: getSchedule,
    addSchedule: addSchedule,
    deleteSchedule: deleteSchedule,
    updateSchedule: updateSchedule,
    applyScheduleToGroup: applyScheduleToGroup,
    applyScheduleToGroups: applyScheduleToGroups,
    applyScheduleToSite: applyScheduleToSite,
    getAllETDHProfiles: getAllETDHProfiles,
    getAllDHProfiles: getAllDHProfiles,
    getETDHProfile: getETDHProfile,
    getDHProfile: getDHProfile,
    createETDHProfile: createETDHProfile,
    createDHProfile: createDHProfile,
    updateETDHProfile: updateETDHProfile,
    updateDHProfile: updateDHProfile,
    deleteETDHProfile: deleteETDHProfile,
    deleteDHProfile: deleteDHProfile,
    setETDaylightHarvestingForGroup: setETDaylightHarvestingForGroup,
    setETDaylightHarvestingForGroups: setETDaylightHarvestingForGroups,
    setDaylightHarvestingForGroup: setDaylightHarvestingForGroup,
    setDaylightHarvestingForGroups: setDaylightHarvestingForGroups,
    setDaylightHarvestingForNode: setDaylightHarvestingForNode,
    setETDaylightHarvestingForSite: setETDaylightHarvestingForSite,
    setDaylightHarvestingForSite: setDaylightHarvestingForSite,
    getAllETDaylightHarvestingTriggers: getAllETDaylightHarvestingTriggers,
    removeETDHProfileTrigger: removeETDHProfileTrigger,
    addETDHProfileTrigger: addETDHProfileTrigger,
    removeETDHProfileTriggers: removeETDHProfileTriggers,
    addETDHProfileTriggers: addETDHProfileTriggers,
    getAllDaylightHarvestingTriggers: getAllDaylightHarvestingTriggers,
    removeDHProfileTrigger: removeDHProfileTrigger,
    addDHProfileTrigger: addDHProfileTrigger,
    removeDHProfileTriggers: removeDHProfileTriggers,
    addDHProfileTriggers: addDHProfileTriggers,
    getAllPDProfiles: getAllPDProfiles,
    getPDProfile: getPDProfile,
    createPDProfile: createPDProfile,
    updatePDProfile: updatePDProfile,
    deletePDProfile: deletePDProfile,
    applyProximityDimmingForGroup: applyProximityDimmingForGroup,
    applyProximityDimmingForGroups: applyProximityDimmingForGroups,
    applyProximityDimmingForSite: applyProximityDimmingForSite
};

function getAllSchedules(req, res) {
    var siteid = req.swagger.params.siteid.value;
    var orgid = req.swagger.params.orgid.value;
    encoder.getAllSchedules(req.getCurrentUser(), siteid, orgid, function (err, msg) {
        response.Done(err, msg, res, req);
    });
}

function getSchedule(req, res) {
    var siteid = req.swagger.params.siteid.value;
    var orgid = req.swagger.params.orgid.value;
    var scheduleid = req.swagger.params.scheduleid.value;
    encoder.getSchedule(req.getCurrentUser(), scheduleid, siteid, orgid, function (err, msg) {
        response.Done(err, msg, res, req);
    });
}

function deleteSchedule(req, res) {
    var siteid = req.swagger.params.siteid.value;
    var orgid = req.swagger.params.orgid.value;
    var scheduleid = req.swagger.params.scheduleid.value;
    encoder.deleteSchedule(req.getCurrentUser(), scheduleid, siteid, orgid, function (err, msg) {
        response.Done(err, msg, res, req);
    });
}

function validateClockTime(time) {
    if (time === undefined) {
        return {error: true,
                msg: "beginTime and endTime are necessary fields",
                status: 400};
    }
    var re = new RegExp(/^((([0-1]\d|2[0-3]):[0-5]\d:[0-5]\d)$)|((sunrise|sunset)((\+|\-)\d+)?)$/i);

    if(!re.test(time.toString())) {
        return {error: true,
                msg: "Time must be well formatted (HH:MM:SS or sunset-30 or sunrise) '"+time+"'",
                status: 400};
    } else {
        return false;
    }
}

function validateSchedule(schedule, isPhotocellEnabledForNetwork) {
    var daysOfWeek = {mon: 0, tue: 0, wed: 0, thu: 0, fri: 0, sat: 0, sun: 0};
    var dates = [];
    let hasPhotocellEvent = false;
    for (var ii = 0; ii < schedule.events.length; ii++) {
        var event = schedule.events[ii];        
        if(event.photocell_enabled) hasPhotocellEvent = true;
        if (!event.photocell_enabled && (event.actions == undefined || event.actions.length < 1)) {
            return {error: true, msg: "Schedule actions cannot be empty for non-photocell mode", status: 400};
        }
        else if (!event.photocell_enabled && !event.date && !event.days) {
            return {error: true, msg: "Schedule days or date must be set for non-photocell mode", status: 400};
        }
        else if (event.date && event.days) {
            return {error: true, msg: "Schedule days and date cannot both be set", status: 400};
        }       
        else if (!event.photocell_enabled && event.days && event.days.length < 1) {
            return {error: true, msg: "Schedule days must not be empty", status: 400};
        }

        var days = event.days || [];
        for (var kk = 0; kk < days.length; kk++) {
            daysOfWeek[(days[kk] + "").toLowerCase()] += 1;
        }

        if (event.date) {
            if (isNaN(Date.parse(event.date))) {
                return {error: true, msg: "Schedule date is invalid: " + event.date, status: 400};
            }
            if (dates.indexOf(event.date) != -1 )
                return {error: true, msg: "Schedule cannot contain duplicate date: " + event.date, status: 400};
            dates.push(event.date);
        }

        if (event.actions && event.actions.length > 0) {
            var times = [];            
            for (var jj = 0; jj < event.actions.length; jj++) {
                var action = event.actions[jj],
                    time = action.time,
                    errors = validateClockTime(time);

                if (errors) return errors;

                if (times.indexOf(time) != -1)
                    return { error: true, msg: "Schedule cannot contain duplicate time in an action: " + time, status: 400 };

                times.push(time);
            }
        }
    }

    if(!hasPhotocellEvent) {
        for (var key in daysOfWeek) {
            if (daysOfWeek[key] < 1) {
                return { error: true, msg: "All days of the week must have a schedule '" + key + "' is missing.", status: 400 };
            }

            if (daysOfWeek[key] > 1) {
                return { error: true, msg: "All days of the week must be used only once. '" + key + "' is used more than once.", status: 400 };
            }
        } 
    }
    if (!isPhotocellEnabledForNetwork && (!schedule.network ||
        !schedule.network.highTime ||
        !schedule.network.highLevel)) {
        return {
            error: true,
            msg: "Schedule requires a no-network block with, at minimum, a `highTime` and `highLevel` for non-photocell mode",
            status: 400
        };
    }
    if (schedule.network.highTime) {
        var errors = validateClockTime(schedule.network.highTime);
        if (errors) return errors;
    }

    if (schedule.network.lowTime) {
        var errors = validateClockTime(schedule.network.lowTime);
        if (errors) return errors;

        if (schedule.network.highTime === schedule.network.lowTime) {
            return { error: true, msg: "High time and low time cannot have the same value " + schedule.network.lowTime, status: 400 };
        }
    }    

    return false;
}

/**
 * Add photocell properties to network object in req payload for add and update schedule
 * @param {*} schNetwork 
 */
function addNetworkPhotocellProperties(schNetwork) {
    let photocellProperties = {
        "photocell_enabled": schNetwork.photocell_enabled !== undefined ? schNetwork.photocell_enabled : false,
        "photocell_highLevel": schNetwork.photocell_highLevel !== undefined ? schNetwork.photocell_highLevel : 100,
        "photocell_lowLevel": schNetwork.photocell_lowLevel !== undefined ? schNetwork.photocell_lowLevel : 0,
        "highTime": schNetwork.highTime !== undefined ? schNetwork.highTime : "00:00:00",
        "highLevel": schNetwork.highLevel !== undefined ? schNetwork.highLevel : 100
    }  

    if(!schNetwork.photocell_enabled){
        photocellProperties.photocell_enabled = false;
        delete photocellProperties.highTime;
        delete photocellProperties.highLevel;
        if(schNetwork.highTime !== undefined) photocellProperties.highTime = schNetwork.highTime;
        if(schNetwork.highLevel !== undefined) photocellProperties.highLevel = schNetwork.highLevel;
    }
    return photocellProperties;
}

/**
 * Add photocell properties to event object in req payload for add and update schedule
 * @param {*} schEvent 
 */
function addEventPhotocellProperties(schEvent) {
   return {
    "photocell_enabled": schEvent.photocell_enabled !== undefined ? schEvent.photocell_enabled : false,
    "photocell_highLevel": schEvent.photocell_highLevel !== undefined ? schEvent.photocell_highLevel : 100,
    "photocell_lowLevel": schEvent.photocell_lowLevel !== undefined ? schEvent.photocell_lowLevel : 0,
   }
}

function preserveScheduleIndices(sch) {
    // Preserve the event and action indexes
    for (var ii = 0; ii < sch.events.length; ii++) {
        var event = sch.events[ii];
        event["id"] = ii;
        if(event.actions && event.actions.length > 0){
            for(var jj = 0; jj < event.actions.length; jj++) {
                var action = event.actions[jj];
                action["id"] = jj;
            }
        }
    }
}

function addSchedule(req, res) {
    var scheduleid = uuid.v1();
    var orgid = req.swagger.params.orgid.value;
    var siteid = req.swagger.params.siteid.value;
    var sch = req.body;
    sch["scheduleid"] = scheduleid;
    global.log.info("Adding schedule: " + JSON.stringify(sch));
    if (!sch.network) {
        sch.network = {};
    }
    let photocellProperties = addNetworkPhotocellProperties(sch.network);
    sch.network = Object.assign(sch.network, photocellProperties);
    if (sch.events && sch.events.length > 0) {
        for (var i in sch.events) {
            let eventPhotocellProperties = addEventPhotocellProperties(sch.events[i]);
            sch.events[i] = Object.assign(sch.events[i], eventPhotocellProperties);
        }
    }
    // Validation
    const errors = validateSchedule(sch, sch.network.photocell_enabled);
    if (errors) return response.Done(errors, null, res, req);

    preserveScheduleIndices(sch);

    encoder.addSchedule(req.getCurrentUser(), sch, siteid, orgid, function (err, msg) {
        response.Done(err, msg, res, req);
    });
}

function updateSchedule(req, res) {
    var orgid = req.swagger.params.orgid.value;
    var siteid = req.swagger.params.siteid.value;
    var scheduleid = req.swagger.params.scheduleid.value;
    var sch = req.body;
    sch.scheduleid = scheduleid;
    global.log.info("Updating schedule: " + JSON.stringify(sch));
    if (!sch.network) {
        sch.network = {};
    }
    let photocellProperties = addNetworkPhotocellProperties(sch.network);
    sch.network = Object.assign(sch.network, photocellProperties);
    if (sch.events && sch.events.length > 0) {
        for (var i in sch.events) {
            let eventPhotocellProperties = addEventPhotocellProperties(sch.events[i]);
            sch.events[i] = Object.assign(sch.events[i], eventPhotocellProperties);
        }
    }
    // Validation
    const errors = validateSchedule(sch, sch.network.photocell_enabled);
    if (errors) return response.Done(errors, null, res, req);

    preserveScheduleIndices(sch);

    global.log.info("Updating schedule: " + JSON.stringify(sch));
    encoder.updateSchedule(req.getCurrentUser(), sch, siteid, orgid, function (err, msg) {
        response.Done(err, msg, res, req);
    });
}

function applyScheduleToGroup(req, res) {
    var orgid = req.swagger.params.orgid.value;
    var siteid = req.swagger.params.siteid.value;
    var scheduleid = req.swagger.params.scheduleid.value;
    var groupid = req.swagger.params.groupid.value;
    global.log.info("Applying schedule to group: " + groupid);
    encoder.applyScheduleToGroup(req.getCurrentUser(), scheduleid, siteid, orgid, [groupid], function (err, msg) {
        response.Done(err, msg, res, req);
    });
}

function applyScheduleToGroups(req, res) {
    var orgid = req.swagger.params.orgid.value;
    var siteid = req.swagger.params.siteid.value;
    var scheduleid = req.swagger.params.scheduleid.value;
    var groupids = req.body.groupList;
    global.log.info("Applying schedule to group: " + groupids);
    encoder.applyScheduleToGroup(req.getCurrentUser(), scheduleid, siteid, orgid, groupids, function (err, msg) {
        response.Done(err, msg, res, req);
    });
}

function applyScheduleToSite(req, res) {
    var orgid = req.swagger.params.orgid.value;
    var siteid = req.swagger.params.siteid.value;
    var scheduleid = req.swagger.params.scheduleid.value;
    var sch = req.body;
    global.log.info("Applying schedule to site: " + siteid);
    encoder.applyScheduleToSite(req.getCurrentUser(), scheduleid, siteid, orgid, function (err, msg) {
        response.Done(err, msg, res, req);
    });
}

function getAllPDProfiles(req, res) {
    var siteid = req.swagger.params.siteid.value;
    var orgid = req.swagger.params.orgid.value;
    encoder.getAllPDProfiles(req.getCurrentUser(), siteid, orgid, function (err, msg) {
        response.Done(err, msg, res, req);
    });
}

function getPDProfile(req, res) {
    var siteid = req.swagger.params.siteid.value;
    var orgid = req.swagger.params.orgid.value;
    var pdprofileid = req.swagger.params.pdprofileid.value;
    encoder.getPDProfile(req.getCurrentUser(), pdprofileid, siteid, orgid, function (err, msg) {
        response.Done(err, msg, res, req);
    });
}

function validatePDProfile(pdprofile) {
    var errors = validateClockTime(pdprofile.beginTime);
    if (errors) {
        return errors;
    }
    errors = validateClockTime(pdprofile.endTime);
    if (errors) {
        return errors;
    }
    if (! (pdprofile.minLevel < pdprofile.maxLevel)) {
        return {error: true,
                msg: "Max level must be greater than min level",
                status: 400};
    }
    if (pdprofile.detection_duration < 30 || pdprofile.detection_duration > 6*3600) {
        return {error: true,
                msg: "Detection duration must be between 30 seconds and 6 hours.",
                status: 400};
    }
    if (pdprofile.radius && pdprofile.radius > 1000) {
        return {error: true,
                msg: "Radius cannot be greater than one kilometer.",
                status: 400};
    }
}

function cleanupPDProfile(pdprofile) {
    delete pdprofile.sites;
    delete pdprofile.groups;
    delete pdprofile.nodes;
    return pdprofile;
}

function createPDProfile(req, res) {
    var pdprofileid = uuid.v1();
    var orgid = req.swagger.params.orgid.value;
    var siteid = req.swagger.params.siteid.value;
    var pdprofile = req.body;
    pdprofile["pdprofileid"] = pdprofileid;
    pdprofile = cleanupPDProfile(pdprofile);

    var errors = validatePDProfile(pdprofile);
    if (errors) {
        return response.Done(errors, null, res, req);
    }

    global.log.info("Adding pdprofile: " + JSON.stringify(pdprofile));
    encoder.createPDProfile(req.getCurrentUser(), pdprofile, siteid, orgid, function (err, msg) {
        response.Done(err, msg, res, req);
    });
}

function updatePDProfile(req, res) {
    var orgid = req.swagger.params.orgid.value;
    var siteid = req.swagger.params.siteid.value;
    var pdprofileid = req.swagger.params.pdprofileid.value;
    var pdprofile = req.body;
    pdprofile = cleanupPDProfile(pdprofile);

    var errors = validatePDProfile(pdprofile);
    if (errors) {
        return response.Done(errors, null, res, req);
    }
    global.log.info("Adding pdprofile: " + JSON.stringify(pdprofile));
    encoder.updatePDProfile(req.getCurrentUser(), pdprofile, pdprofileid, siteid, orgid, function (err, msg) {
        response.Done(err, msg, res, req);
    });
}

function deletePDProfile(req, res) {
    var siteid = req.swagger.params.siteid.value;
    var orgid = req.swagger.params.orgid.value;
    var pdprofileid = req.swagger.params.pdprofileid.value;
    encoder.deletePDProfile(req.getCurrentUser(), pdprofileid, siteid, orgid, function (err, msg) {
        response.Done(err, msg, res, req);
    });
}

function applyProximityDimmingForSite(req, res) {
    var orgid = req.swagger.params.orgid.value;
    var siteid = req.swagger.params.siteid.value;
    var pdprofileid = req.swagger.params.pdprofileid.value;
    global.log.info("Applying proximity dimming to site: " + siteid);
    encoder.applyPD(req.getCurrentUser(), pdprofileid, siteid, orgid, null, function (err, msg) {
        response.Done(err, msg, res, req);
    });
}

function applyProximityDimmingForGroup(req, res) {
    var orgid = req.swagger.params.orgid.value;
    var siteid = req.swagger.params.siteid.value;
    var groupid = req.swagger.params.groupid.value;
    var pdprofileid = req.swagger.params.pdprofileid.value;
    global.log.info("Applying proximity dimming to group: " + groupid);
    encoder.applyPD(req.getCurrentUser(), pdprofileid, siteid, orgid, [groupid], function (err, msg) {
        response.Done(err, msg, res, req);
    });
}

function applyProximityDimmingForGroups(req, res) {
    var orgid = req.swagger.params.orgid.value;
    var siteid = req.swagger.params.siteid.value;
    var groupids = req.body.groupList;
    var pdprofileid = req.swagger.params.pdprofileid.value;
    global.log.info("Applying proximity dimming to group: " + groupids);
    encoder.applyPD(req.getCurrentUser(), pdprofileid, siteid, orgid, groupids, function (err, msg) {
        response.Done(err, msg, res, req);
    });
}

function getAllETDHProfiles(req, res) {
    var siteid = req.swagger.params.siteid.value;
    var orgid = req.swagger.params.orgid.value;
    encoder.getAllETDHProfiles(req.getCurrentUser(), siteid, orgid, function (err, msg) {
        response.Done(err, msg, res, req);
    });
}

function getAllDHProfiles(req, res) {
    var siteid = req.swagger.params.siteid.value;
    var orgid = req.swagger.params.orgid.value;
    encoder.getAllDHProfiles(req.getCurrentUser(), siteid, orgid, function (err, msg) {
        response.Done(err, msg, res, req);
    });
}

function getETDHProfile(req, res) {
    var siteid = req.swagger.params.siteid.value;
    var orgid = req.swagger.params.orgid.value;
    var etdhprofileid = req.swagger.params.etdhprofileid.value;
    encoder.getETDHProfile(req.getCurrentUser(), etdhprofileid, siteid, orgid, function (err, msg) {
        response.Done(err, msg, res, req);
    });
}

function getDHProfile(req, res) {
    var siteid = req.swagger.params.siteid.value;
    var orgid = req.swagger.params.orgid.value;
    var dhprofileid = req.swagger.params.dhprofileid.value;
    encoder.getDHProfile(req.getCurrentUser(), dhprofileid, siteid, orgid, function (err, msg) {
        response.Done(err, msg, res, req);
    });
}

function cleanupDHProfile(dhprofile) {
    dhprofile.gain = dhprofile.gain || 50;
    dhprofile.slewRate = dhprofile.slewRate || 0.05;
    dhprofile.minDrive = dhprofile.minDrive || 1;
    dhprofile.resetTime = dhprofile.resetTime || 2;

    if (dhprofile.autocalibrateoptout === undefined)
        dhprofile.autocalibrateoptout = false;

    delete dhprofile.sites;
    delete dhprofile.groups;
    delete dhprofile.nodes;

    return dhprofile;
}

function validateDHProfile(dhprofile) {
    var errors = validateClockTime(dhprofile.beginTime);
    if (errors) {
        return errors;
    }
    errors = validateClockTime(dhprofile.endTime);
    if (errors) {
        return errors;
    }
    if (dhprofile.autocalibrateoptout && !dhprofile.setPoint) {
        return {error: true,
                msg: "Setpoint is mandatory if autocalibrateoptout is set",
                status: 400};
    }
}

function createETDHProfile(req, res) {
    var etdhprofileid = uuid.v1();
    var orgid = req.swagger.params.orgid.value;
    var siteid = req.swagger.params.siteid.value;
    var etdhprofile = req.body;
    etdhprofile["etdhprofileid"] = etdhprofileid;

    global.log.info("Adding etdhprofile: " + JSON.stringify(etdhprofile));
    encoder.createETDHProfile(req.getCurrentUser(), etdhprofile, siteid, orgid, function (err, msg) {
        response.Done(err, msg, res, req);
    });
}

function createDHProfile(req, res) {
    var dhprofileid = uuid.v1();
    var orgid = req.swagger.params.orgid.value;
    var siteid = req.swagger.params.siteid.value;
    var dhprofile = cleanupDHProfile(req.body);
    dhprofile["dhprofileid"] = dhprofileid;

    var errors = validateDHProfile(dhprofile);
    if (errors) {
        return response.Done(errors, null, res, req);
    }


    global.log.info("Adding dhprofile: " + JSON.stringify(dhprofile));
    encoder.createDHProfile(req.getCurrentUser(), dhprofile, siteid, orgid, function (err, msg) {
        response.Done(err, msg, res, req);
    });
}

function updateETDHProfile(req, res) {
    var orgid = req.swagger.params.orgid.value;
    var siteid = req.swagger.params.siteid.value;
    var etdhprofileid = req.swagger.params.etdhprofileid.value;
    var etdhprofile = req.body;

    global.log.info("Adding etdhprofile: " + JSON.stringify(etdhprofile));
    encoder.updateETDHProfile(req.getCurrentUser(), etdhprofile, etdhprofileid, siteid, orgid, function (err, msg) {
        response.Done(err, msg, res, req);
    });
}

function updateDHProfile(req, res) {
    var orgid = req.swagger.params.orgid.value;
    var siteid = req.swagger.params.siteid.value;
    var dhprofileid = req.swagger.params.dhprofileid.value;
    var dhprofile = cleanupDHProfile(req.body);

    var errors = validateDHProfile(dhprofile);
    if (errors) {
        return response.Done(errors, null, res, req);
    }
    global.log.info("Adding dhprofile: " + JSON.stringify(dhprofile));
    encoder.updateDHProfile(req.getCurrentUser(), dhprofile, dhprofileid, siteid, orgid, function (err, msg) {
        response.Done(err, msg, res, req);
    });
}

function deleteETDHProfile(req, res) {
    var siteid = req.swagger.params.siteid.value;
    var orgid = req.swagger.params.orgid.value;
    var etdhprofileid = req.swagger.params.etdhprofileid.value;
    encoder.deleteETDHProfile(req.getCurrentUser(), etdhprofileid, siteid, orgid, function (err, msg) {
        response.Done(err, msg, res, req);
    });
}

function deleteDHProfile(req, res) {
    var siteid = req.swagger.params.siteid.value;
    var orgid = req.swagger.params.orgid.value;
    var dhprofileid = req.swagger.params.dhprofileid.value;
    encoder.deleteDHProfile(req.getCurrentUser(), dhprofileid, siteid, orgid, function (err, msg) {
        response.Done(err, msg, res, req);
    });
}

function setETDaylightHarvestingForSite(req, res) {
    var orgid = req.swagger.params.orgid.value;
    var siteid = req.swagger.params.siteid.value;
    var etdhprofileid = req.swagger.params.etdhprofileid.value;
    global.log.info("Applying daylight harvesting to site: " + siteid);
    encoder.applyETDH(req.getCurrentUser(), etdhprofileid, siteid, orgid, null, null, function (err, msg) {
        response.Done(err, msg, res, req);
    });
}

function setDaylightHarvestingForSite(req, res) {
    var orgid = req.swagger.params.orgid.value;
    var siteid = req.swagger.params.siteid.value;
    var dhprofileid = req.swagger.params.dhprofileid.value;
    global.log.info("Applying daylight harvesting to site: " + siteid);
    encoder.applyDH(req.getCurrentUser(), dhprofileid, siteid, orgid, null, null, function (err, msg) {
        response.Done(err, msg, res, req);
    });
}

function setDaylightHarvestingForGroup(req, res) {
    var orgid = req.swagger.params.orgid.value;
    var siteid = req.swagger.params.siteid.value;
    var groupid = req.swagger.params.groupid.value;
    var dhprofileid = req.swagger.params.dhprofileid.value;
    global.log.info("Applying daylight harvesting to group: " + groupid);
    encoder.applyDH(req.getCurrentUser(), dhprofileid, siteid, orgid, [groupid], null, function (err, msg) {
        response.Done(err, msg, res, req);
    });
}

function setDaylightHarvestingForGroups(req, res) {
    var orgid = req.swagger.params.orgid.value;
    var siteid = req.swagger.params.siteid.value;
    var groupids = req.body.groupList;
    var dhprofileid = req.swagger.params.dhprofileid.value;
    global.log.info("Applying daylight harvesting to group: " + groupids);
    encoder.applyDH(req.getCurrentUser(), dhprofileid, siteid, orgid, groupids, null, function (err, msg) {
        response.Done(err, msg, res, req);
    });
}

function setETDaylightHarvestingForGroup(req, res) {
    var orgid = req.swagger.params.orgid.value;
    var siteid = req.swagger.params.siteid.value;
    var groupid = req.swagger.params.groupid.value;
    var etdhprofileid = req.swagger.params.etdhprofileid.value;
    global.log.info("Applying daylight harvesting to group: " + groupid);
    encoder.applyETDH(req.getCurrentUser(), etdhprofileid, siteid, orgid, [groupid], null, function (err, msg) {
        response.Done(err, msg, res, req);
    });
}

function setETDaylightHarvestingForGroups(req, res) {
    var orgid = req.swagger.params.orgid.value;
    var siteid = req.swagger.params.siteid.value;
    var groupids = req.body.groupList;
    var etdhprofileid = req.swagger.params.etdhprofileid.value;
    global.log.info("Applying daylight harvesting to group: " + groupids);
    encoder.applyETDH(req.getCurrentUser(), etdhprofileid, siteid, orgid, groupids, null, function (err, msg) {
        response.Done(err, msg, res, req);
    });
}

function setDaylightHarvestingForNode(req, res) {
    var orgid = req.swagger.params.orgid.value;
    var siteid = req.swagger.params.siteid.value;
    var nodeid = req.swagger.params.nodeid.value;
    var dhprofileid = req.swagger.params.dhprofileid.value;
    global.log.info("Applying daylight harvesting to node: " + nodeid);
    encoder.applyDH(req.getCurrentUser(), dhprofileid, siteid, orgid, null, nodeid, function (err, msg) {
        response.Done(err, msg, res, req);
    });
}

function getAllETDaylightHarvestingTriggers(req, res) {
    var orgid = req.swagger.params.orgid.value;
    var siteid = req.swagger.params.siteid.value;
    var etdhprofileid = req.swagger.params.etdhprofileid.value;
    encoder.getAllETDHProfileTriggers(req.getCurrentUser(), etdhprofileid, siteid, orgid, function (err, msg) {
        response.Done(err, msg, res, req);
    });
}

function removeETDHProfileTriggers(req, res) {
    var orgid = req.swagger.params.orgid.value;
    var siteid = req.swagger.params.siteid.value;
    var etdhprofileid = req.swagger.params.etdhprofileid.value;
    var nodeids = req.body.nodeList;
    encoder.removeETDHProfileTriggers(req.getCurrentUser(), etdhprofileid, siteid, orgid, nodeids, function (err, msg) {
        response.Done(err, null, res, req);
    });
}

function removeETDHProfileTrigger(req, res) {
    var orgid = req.swagger.params.orgid.value;
    var siteid = req.swagger.params.siteid.value;
    var etdhprofileid = req.swagger.params.etdhprofileid.value;
    var nodeid = req.swagger.params.nodeid.value;
    encoder.removeETDHProfileTriggers(req.getCurrentUser(), etdhprofileid, siteid, orgid, [nodeid], function (err, msg) {
        response.Done(err, null, res, req);
    });
}

function addETDHProfileTriggers(req, res) {
    var orgid = req.swagger.params.orgid.value;
    var siteid = req.swagger.params.siteid.value;
    var etdhprofileid = req.swagger.params.etdhprofileid.value;
    var nodeids = req.body.nodeList;
    encoder.addETDHProfileTriggers(req.getCurrentUser(), etdhprofileid, siteid, orgid, nodeids, function (err, msg) {
        response.Done(err, null, res, req);
    });
}

function addETDHProfileTrigger(req, res) {
    var orgid = req.swagger.params.orgid.value;
    var siteid = req.swagger.params.siteid.value;
    var etdhprofileid = req.swagger.params.etdhprofileid.value;
    var nodeid = req.swagger.params.nodeid.value;
    encoder.addETDHProfileTriggers(req.getCurrentUser(), etdhprofileid, siteid, orgid, [nodeid], function (err, msg) {
        response.Done(err, null, res, req);
    });
}

function getAllDaylightHarvestingTriggers(req, res) {
    var orgid = req.swagger.params.orgid.value;
    var siteid = req.swagger.params.siteid.value;
    var dhprofileid = req.swagger.params.dhprofileid.value;
    encoder.getAllDHProfileTriggers(req.getCurrentUser(), dhprofileid, siteid, orgid, function (err, msg) {
        response.Done(err, msg, res, req);
    });
}

function removeDHProfileTriggers(req, res) {
  var orgid = req.swagger.params.orgid.value;
  var siteid = req.swagger.params.siteid.value;
  var dhprofileid = req.swagger.params.dhprofileid.value;
  var nodeids = req.body.nodeList;
  for(var i=0;i<nodeids.length;i++) {
    encoder.removeDHProfileTrigger(req.getCurrentUser(), dhprofileid, siteid, orgid, nodeids[i], function (err, msg) {});
  }
  response.Done(null,"Ok",res, req);
}

function removeDHProfileTrigger(req, res) {
    var orgid = req.swagger.params.orgid.value;
    var siteid = req.swagger.params.siteid.value;
    var dhprofileid = req.swagger.params.dhprofileid.value;
    var nodeid = req.swagger.params.nodeid.value;
    encoder.removeDHProfileTrigger(req.getCurrentUser(), dhprofileid, siteid, orgid, nodeid, function (err, msg) {
        response.Done(err, null, res, req);
    });
}

function addDHProfileTriggers(req, res) {
  var orgid = req.swagger.params.orgid.value;
  var siteid = req.swagger.params.siteid.value;
  var dhprofileid = req.swagger.params.dhprofileid.value;
  var nodeids = req.body.nodeList;
  for(var i=0;i<nodeids.length;i++) {
    encoder.addDHProfileTrigger(req.getCurrentUser(), dhprofileid, siteid, orgid, nodeids[i], function (err, msg) {});
  }
  response.Done(null,"Ok",res, req);
}

function addDHProfileTrigger(req, res) {
    var orgid = req.swagger.params.orgid.value;
    var siteid = req.swagger.params.siteid.value;
    var dhprofileid = req.swagger.params.dhprofileid.value;
    var nodeid = req.swagger.params.nodeid.value;
    encoder.addDHProfileTrigger(req.getCurrentUser(), dhprofileid, siteid, orgid, nodeid, function (err, msg) {
        response.Done(err, null, res, req);
    });
}
