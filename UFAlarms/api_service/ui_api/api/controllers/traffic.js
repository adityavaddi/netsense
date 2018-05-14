'use strict';

var util = require('util'),
    encoder = require('./../../encoder/Encoder.js').JSONEncoder,
    uuid = require('uuid');

var response = require('./../helpers/response.js');
const default_car_length = 4.9;

module.exports = {
    getTrafficInfo: getTrafficInfo,
    getTrafficConfig: getTrafficConfig,
    getTrafficPoint: notImplemented, //getTrafficPoint,
    getAllTrafficPoints: notImplemented, //getAllTrafficPoints,
    addTrafficPoint: notImplemented, //addTrafficPoint,
    updateTrafficPoint: notImplemented, //updateTrafficPoint,
    deleteTrafficPoint: notImplemented, //deleteTrafficPoint,
};

function notImplemented(req, res){
    conole.log("Arguments:", arguments)
    response.Done({error:true, message: "Not implemented", status: 501}, null, res, req);
}


function getCurrentTrafficInfo(req, res) {
    var siteid = req.swagger.params.siteid.value;
    var orgid = req.swagger.params.orgid.value;
    var body = req.body;
    var nodeid = req.swagger.params.nodeid?req.swagger.params.nodeid.value:(body.nodeid?body.nodeid:null);
    var type = body.type?body.type:null;
    var trafficdetectioneventid = body.trafficdetectioneventid?body.trafficdetectioneventid:null;
    var active_filter = body.active_filter!=undefined?body.active_filter:'active';

    var filter = {
        orgid: orgid,
        siteid: siteid,
        nodeid: nodeid,
        type: type,
    }

    encoder.getCurrentTrafficInfo(req.getCurrentUser(), siteid, filter, function(err, msg) {
        if(err){
            return response.Done(err, null, res, req);
        }
        var resp = [];
        if(msg && msg.length) {
            for(var i=0;i<msg.length;i++) {
                if(active_filter==='active' && msg[i].active===false)
                    continue;
                if(active_filter==='inactive' && msg[i].active===true)
                    continue;
                if(trafficdetectioneventid && msg[i].trafficdetectioneventid!==trafficdetectioneventid)
                    continue;
                resp.push(msg[i]);
            }
        }
        response.Done(err, resp, res, req);
    });
}

function getTrafficHistory(req, res) {
    var resp = [];
    var siteid = req.swagger.params.siteid.value;
    var nodeid = req.swagger.params.nodeid?req.swagger.params.nodeid.value:null;
    var filter = req.body;
    var eventid = filter.eventid;
    if(nodeid && !filter.nodeid){
        filter.nodeid = nodeid;
    }
    if (filter.startTime) {
        filter.from = Date.parse(filter.startTime) * 1000;
        if(filter.endTime){
            filter.to = Date.parse(filter.endTime) * 1000;
        }
        else{
            global.log.error({error: true, message: "End time missing", status: 400});
            return response.Done({error: true, message: "End time missing", status: 400}, null, res, req);

            // filter.to = Date.now() * 1000;
        }
        if(isNaN(filter.from) || isNaN(filter.to))  {
            global.log.error({error: true, message: "Bad date format in traffic historic data API call", status: 400});
            return response.Done({error: true, message: "Bad date format in traffic historic data API call", status: 400}, null, res, req);
        }
        if(filter.to<=filter.from){
            global.log.error({error: true, message: "End time must be later than start time", status: 400});
            return response.Done({error: true, message: "End time must be later than start time", status: 400}, null, res, req);
        }
    }
    if(eventid && !nodeid){
        global.log.error({error: true, message: "Can't filter by eventid on a site level", status: 400});
        return response.Done({error: true, message: "Can't filter by eventid on a site level", status: 400}, null, res, req);
    }
    var resolution = filter.timeResolution ? filter.timeResolution : 60;
    var factor = resolution/15;

    if(filter.spatialResolution) {
        return response.Done({error: true, message: "Spatial aggregates not implemented", status: 501}, null, res, req);
    }
    filter.spatialResolution = 'site';

    if(!filter.timeResolution) {
        filter.timeResolution = '15min';
    }


    if(!filter.aggregation_type) {
        filter.aggregation_type = filter.timeResolution;
    }

    encoder.getTrafficHistory(req.getCurrentUser(), siteid, filter, function (err, msg) {
        if(msg && msg.length) {
             for(var i=0;i<msg.length;i++) {
                 if(filter.trafficdetectioneventid && msg[i].eventid!==filter.trafficdetectioneventid)
                     continue;
                 if(filter.eventid && msg[i].eventid!==filter.eventid)
                     continue;
                 if(filter.type && msg[i].type!==filter.type)
                     continue;
                 /*if(filter.name && msg[i].name!==filter.name)
                     continue;*/
                 if((!filter.object_class && msg[i].object_class.toLowerCase()!='na') || (msg[i].object_class && filter.object_class === msg[i].object_class.toLowerCase()))
                   resp.push(msg[i]);
             }
        }
        response.Done(err, resp, res, req);
    });
}

function getTrafficInfo(req, res) {
    try {
        var siteid = req.swagger.params.siteid.value;
        var filter = req.body;
        if(!filter.startTime) {
            return getCurrentTrafficInfo(req, res);
        } else {
            return getTrafficHistory(req, res);
        }
    } catch (exc) {
        global.log.error({error: true, message: exc.message, status: 500})
        response.Done({error: true, message: exc.message, status: 500}, null, res, req);
    }
}

function getTrafficConfig(req, res) {
    try {
        var resp = [];
        var siteid = req.swagger.params.siteid.value;
        var nodeid = req.swagger.params.nodeid?req.swagger.params.nodeid.value:null;

        var filter = req.body;
        var active_filter = filter.active_filter!=undefined?filter.active_filter:'active';

        if(nodeid && nodeid !==filter.nodeid){
            filter.nodeid = nodeid;
        }

        encoder.getTrafficConfig(req.getCurrentUser(), siteid, nodeid, filter, function (err, msg) {
            if(!err){

                if (msg && msg.length) {
                    for (var i = 0; i < msg.length; i++) {
                        if(filter.eventid === '' || (filter.eventid && msg[i].eventid!==filter.eventid))
                            continue;
                        if(filter.nodeid === '' || (filter.nodeid && filter.nodeid != 'all' && msg[i].nodeid!==filter.nodeid))
                            continue;
                        if(active_filter==='active' && msg[i].active===false)
                            continue;
                        if(active_filter==='inactive' && msg[i].active===true)
                            continue;
                        resp.push(msg[i]);
                    }
                }

            }
            response.Done(err, resp, res, req);
        });

    } catch (exc) {
        global.log.error({error: true, message: exc.message, status: 500})
        response.Done({error: true, message: exc.message, status: 500}, null, res, req);
    }
}