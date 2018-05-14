'use strict';

var util = require('util'),
    encoder = require('./../../encoder/Encoder.js').JSONEncoder,
    uuid = require('uuid');

var response = require('./../helpers/response.js');
var default_car_length = 4.9;

module.exports = {
    getParkingZones: getParkingZones,
    getOneParkingZone: getOneParkingZone,
    getAllParkingZones: getAllParkingZones,
    getParkingGroup: getParkingGroup,
    getAllParkingGroups: getAllParkingGroups,
    addParkingGroup: addParkingGroup,
    updateParkingGroup: updateParkingGroup,
    deleteParkingGroup: deleteParkingGroup,
    getParkingInfoSite: getParkingInfoSite,
    bulkAssignUsersToParkingGroup: bulkAssignUsersToParkingGroup,
    bulkUnassignUsersFromParkingGroup: bulkUnassignUsersFromParkingGroup
};

function parkingZoneLength(ps) {
    var box = ps.bounding_box_real_world;
    var d1 = distance(box[0].lat, box[1].lat, box[0].lng, box[1].lng);
    var d2 = distance(box[1].lat, box[2].lat, box[1].lng, box[2].lng);

    // Length is always the longer side of the rectangle
    return Math.max(d1, d2);
}

function parkingSpotCenter(ps) {
    var box = ps.bounding_box_real_world;
    // linear approximation is good enough for this purpose
    var lat = (box[0].lat + box[2].lat) / 2;
    var lng = (box(0).lat + box[3].lat) / 2;
    return {"lat": lat, "lng": lng};
}

/** Converts numeric degrees to radians */
if (typeof(Number.prototype.toRad) === "undefined") {
  Number.prototype.toRadians = function() {
    return this * Math.PI / 180;
  }
}

function distance(lat1, lat2, lon1, lon2) {
    if(!lat1 || !lat2 || !lon1 || !lon2) return 0;
    var f1 = lat1.toRadians(), f2 = lat2.toRadians(), dl = (lon2 - lon1).toRadians(), R = 6371e3;
    var x = dl * Math.cos((f1 + f2) / 2);
    var y = f2 - f1;
    var d = Math.sqrt(x * x + y * y) * R;
    return d.toFixed(2);
}

function estimateParkingSpotsLeft(zone, spots) {
    var spotsum = parseInt(distance(zone.world_bounding_box.lat[0], spots[0].world.lat[0], zone.world_bounding_box.lon[0], spots[0].world.lon[0]) / default_car_length);
    for (var i = 1; i < spots.length; i++) {
        var d = distance(spots[i - 1].world.lat[1], spots[i].world.lat[0], spots[i - 1].world.lon[1], spots[i].world.lon[0]);
        spotsum += parseInt(d / default_car_length);
    }
    spotsum += parseInt(distance(spots[i - 1].world.lat[1], zone.world_bounding_box.lat[2], spots[i-1].world.lon[1], zone.world_bounding_box.lon[2]) / default_car_length);

    return spotsum;
}

/* Parking Zones */

function getAllParkingZones(req, res) {

    var user = req.getCurrentUser();
    if(!user || !user.login || !user.login.userid)
    {
        global.log.error("Access not granted", JSON.stringify(user));
        return response.Done({error:true,message: "Access not granted", status:403}, null, res, req);
    }

    encoder.getAllParkingZones(req.getCurrentUser(), req.swagger.params.siteid.value, function (err, msg) {
        if (!err) {
            response.Done(err, msg, res, req);
        } else {
            if (err.message)
                response.Done(err, null, res, req);
            else
                response.Done({error: true, status: 404, message: 'Not found.'}, [], res, req);
        }
        //res.end();
    });
}

function getParkingZones(req, res) {
    var user = req.getCurrentUser();
    if(!user || !user.login || !user.login.userid)
    {
        global.log.error("Access not granted", JSON.stringify(user));
        return response.Done({error:true,message: "Access not granted", status:403}, null, res, req);
    }

    var pgs = {};
    // IS injects the mapping between parking groups (from Neo4j) to parkingzones (Cassandra)
    // This mapping is created by API calls to update Neo4j parking group objects
    encoder.getAllParkingGroups(req.getCurrentUser(), req.swagger.params.siteid.value, function(err, groups) {
        if(!err) {
            for(var j=0;j<groups.length;j++) {
                var g = groups[j];
                var zones = g.parkingzones.split(",");
                for(var k=0;k<zones.length;k++) {
                    pgs[zones[k].toLowerCase()] = g.parkinggroupid;
                }
            }
            encoder.getParkingZones(req.getCurrentUser(), req.swagger.params.siteid.value, function (err, msg) {
                var results = [];
                if(!err && msg) {
                    for(var i=0;i<msg.length;i++) {
                        try {
                            var it = msg[i];
                            var config = it.config && it.config !== null ? JSON.parse(it.config) : {};
                            var state = it.state ? JSON.parse(it.state) : {};
                            if (config && state && config.roi) {
                                var occupied_spaces = it.type === "NonDemarcated" ? ((state && state.spots) ? state.spots.length : 0)  : ((state && state.spots && state.spots.o) ? state.spots.o.length : 0);
                                var cdistance = config.roi.world_bounding_box && it.type === "NonDemarcated" ? distance(config.roi.world_bounding_box[0][0].latitude, config.roi.world_bounding_box[0][2].latitude, config.roi.world_bounding_box[0][0].longitude, config.roi.world_bounding_box[0][2].longitude) : undefined;
                                var max = it.type === "NonDemarcated" ? parseInt((cdistance != undefined ? cdistance : 0) / default_car_length) : (config.spots ? config.spots.length : 0);
                                var pgid = pgs[(it.parkingzoneid).toLowerCase()] || "Unknown";
                                var spots = state && state.spots ? state.spots : {};
                                var item = {
                                    'orgid': it.orgid,
                                    'siteid': it.siteid,
                                    'parkinggroupid': pgid,
                                    'parkingzoneid': it.parkingzoneid,
                                    'nodeid': config.nodeid,
                                    'channel': config.channel,
                                    'type': it.type,
                                    'length': cdistance,
                                    'description': config.description,
                                    'active': config.active,
                                    'max_spaces': max,
                                    'occupied_spaces': occupied_spaces,
                                    'available_spaces': max - occupied_spaces,
                                    'world_bounding_box': config.roi.world_bounding_box ? config.roi.world_bounding_box[0] : null,
                                    'image_bounding_box': config.roi.image_bounding_box ? config.roi.image_bounding_box[0] : null,
                                    'tags': config.tags,
                                    'spots': spots.o ? spots.o : spots,
                                    'config': config
                                }
                                //        TODO: Make sure this works with real coordinates, simulator sends junk
                                //          if(item.type==="NonDemarcated")
                                //            item.available_spaces = estimateParkingSpotsLeft(item, item.spots);
                                results.push(item);
                            }
                        } catch (e) {
                            global.log.error("Parking zone in old format found (siteid: %s, parkingzoneid %s)", it.siteid, it.parkingzoneid, JSON.stringify(it));
                        }
                    }
                    response.Done(err, results, res, req);
                } else {
                    response.Done(err, null, res, req);
                }
            });

        } else {
            response.Done(err, null, res, req);
        }
    });
}

function getOneParkingZone(req, res) {
    var user = req.getCurrentUser();
    if(!user || !user.login || !user.login.userid)
    {
        global.log.error("Access not granted", JSON.stringify(user));
        return response.Done({error:true,message: "Access not granted", status:403}, null, res, req);
    }

    var pgs = {};
    // IS injects the mapping between parking groups (from Neo4j) to parkingzones (Cassandra)
    // This mapping is created by API calls to update Neo4j parking group objects
    encoder.getAllParkingGroups(req.getCurrentUser(), req.swagger.params.siteid.value, function(err, groups) {
        if(!err) {
            for(var j=0;j<groups.length;j++) {
                var g = groups[j];
                var zones = g.parkingzones.split(",");
                for(var k=0;k<zones.length;k++) {
                    pgs[zones[k].toLowerCase()] = g.parkinggroupid;
                }
            }
            encoder.getOneParkingZone(req.getCurrentUser(), req.swagger.params.siteid.value, req.swagger.params.parkingzoneid.value, function (err, msg) {
                var results = [];
                if(!err && msg) {
                    for(var i=0;i<msg.length;i++) {
                        try {
                            var it = msg[i];
                            var config = it.config && it.config !== null ? JSON.parse(it.config) : {};
                            var state = it.state ? JSON.parse(it.state) : {};
                            if (config && state && config.roi) {
                                var occupied_spaces = it.type === "NonDemarcated" ? ((state && state.spots) ? state.spots.length : 0)  : ((state && state.spots && state.spots.o) ? state.spots.o.length : 0);
                                var cdistance = config.roi.world_bounding_box && it.type === "NonDemarcated" ? distance(config.roi.world_bounding_box[0][0].latitude, config.roi.world_bounding_box[0][2].latitude, config.roi.world_bounding_box[0][0].longitude, config.roi.world_bounding_box[0][2].longitude) : undefined;
                                var max = it.type === "NonDemarcated" ? parseInt((cdistance != undefined ? cdistance : 0) / default_car_length) : (config.spots ? config.spots.length : 0);
                                var pgid = pgs[(it.parkingzoneid).toLowerCase()] || "Unknown";
                                var spots = state && state.spots ? state.spots : {};
                                var item = {
                                    'orgid': it.orgid,
                                    'siteid': it.siteid,
                                    'parkinggroupid': pgid,
                                    'parkingzoneid': it.parkingzoneid,
                                    'nodeid': config.nodeid,
                                    'channel': config.channel,
                                    'type': it.type,
                                    'length': cdistance,
                                    'description': config.description,
                                    'active': config.active,
                                    'max_spaces': max,
                                    'occupied_spaces': occupied_spaces,
                                    'available_spaces': max - occupied_spaces,
                                    'world_bounding_box': config.roi.world_bounding_box ? config.roi.world_bounding_box[0] : null,
                                    'image_bounding_box': config.roi.image_bounding_box ? config.roi.image_bounding_box[0] : null,
                                    'tags': config.tags,
                                    'spots': spots.o ? spots.o : spots,
                                    'config': config
                                }
                                //        TODO: Make sure this works with real coordinates, simulator sends junk
                                //          if(item.type==="NonDemarcated")
                                //            item.available_spaces = estimateParkingSpotsLeft(item, item.spots);
                                results.push(item);
                            }
                        } catch (e) {
                            global.log.error("Parking zone in old format found (siteid: %s, parkingzoneid %s)", it.siteid, it.parkingzoneid, JSON.stringify(it));
                        }
                    }
                    response.Done(err, results, res, req);
                } else {
                    response.Done(err, null, res, req);
                }
            });

        } else {
            response.Done(err, null, res, req);
        }
    });
}

/* Parking groups */

function getAllParkingGroups(req, res) {
    var user = req.getCurrentUser();
    if(!user || !user.login || !user.login.userid)
    {
        global.log.error("Access not granted", JSON.stringify(user));
        return response.Done({error:true,message: "Access not granted", status:403}, null, res, req);
    }

    encoder.getAllParkingGroups(req.getCurrentUser(), req.swagger.params.siteid.value, function (err, msg) {
        if (!err) {
            response.Done(err, msg, res, req);
        } else {
            if (err.message)
                response.Done(err, null, res, req);
            else
                response.Done({error: true, status: 404, message: 'Not found.'}, [], res, req);
        }
    });
}

function getParkingGroup(req, res) {
    var user = req.getCurrentUser();
    if(!user || !user.login || !user.login.userid)
    {
        global.log.error("Access not granted", JSON.stringify(user));
        return response.Done({error:true,message: "Access not granted", status:403}, null, res, req);
    }

    encoder.getParkingGroup(req.getCurrentUser(), req.swagger.params.parkinggroupid.value, req.swagger.params.siteid.value, function (err, msg) {
        response.Done(err, msg, res, req);
    });
}

function deleteParkingGroup(req, res) {
    var user = req.getCurrentUser();
    if(!user || !user.login || !user.login.userid)
    {
        global.log.error("Access not granted", JSON.stringify(user));
        return response.Done({error:true,message: "Access not granted", status:403}, null, res, req);
    }

    encoder.deleteParkingGroup(req.getCurrentUser(), req.swagger.params.siteid.value, req.swagger.params.parkinggroupid.value, function (err, msg) {
        response.Done(err, msg, res, req);
    });
}

function addParkingGroup(req, res) {
    var user = req.getCurrentUser();
    if(!user || !user.login || !user.login.userid)
    {
        global.log.error("Access not granted", JSON.stringify(user));
        return response.Done({error:true,message: "Access not granted", status:403}, null, res, req);
    }

    var id = uuid.v1();
    var ParkingGroup = req.body;
    ParkingGroup.parkinggroupid = id;

    encoder.getAllParkingGroups(req.getCurrentUser(), req.swagger.params.siteid.value, function (err, msg) {
        if (err) {
            response.Done(err, msg, res, req);
        } else {
            var zones = ParkingGroup.parkingzones ? ParkingGroup.parkingzones.split(",") : [];
            var cgzones = [];
            for(var i=0;i<msg.length;i++) {
              cgzones = msg[i].parkingzones ? msg[i].parkingzones.split(",") : [];
              for(var j=0;j<zones.length;j++) {
                if(cgzones.indexOf(zones[j])!==-1) {
                  response.Done({error: true, status: 400, message: 'One of the parking zones is already assigned to a different parking group.'}, null, res, req);
                  return;
                }
              }
            }
            encoder.createParkingGroup(req.getCurrentUser(), req.swagger.params.siteid.value, ParkingGroup, function (err1, msg1) {
                response.Done(err1, msg1, res, req);
            });
        }
    });
}

function updateParkingGroup(req, res) {
    var user = req.getCurrentUser();
    if(!user || !user.login || !user.login.userid)
    {
        global.log.error("Access not granted", JSON.stringify(user));
        return response.Done({error:true,message: "Access not granted", status:403}, null, res, req);
    }

    var ParkingGroup = req.body;
    encoder.getAllParkingGroups(req.getCurrentUser(), req.swagger.params.siteid.value, function (err, msg) {
        if (err) {
            response.Done(err, msg, res, req);
        } else {
            var zones = ParkingGroup.parkingzones ? ParkingGroup.parkingzones.split(",") : [];
            var pg_parkinggroupid = req.swagger.params.parkinggroupid.value.toLowerCase();
            var cgzones = [];
            for(var i=0;i<msg.length;i++) {
              cgzones = msg[i].parkingzones ? msg[i].parkingzones.split(",") : [];
              var parkinggroupid = msg[i].parkinggroupid ? msg[i].parkinggroupid.toLowerCase() : "Unknown";
              for(var j=0;j<zones.length;j++) {
                if(cgzones.indexOf(zones[j])!==-1 && pg_parkinggroupid!==parkinggroupid) {
                  response.Done({error: true, status: 400, message: 'One of the parking zones is already assigned to a different parking group.'
                  }, null, res, req);
                  return;
                }
              }
            }
            encoder.updateParkingGroup(req.getCurrentUser(), req.swagger.params.siteid.value, req.swagger.params.parkinggroupid.value, ParkingGroup, function (err1, msg1) {
                response.Done(err1, msg1, res, req);
            });
        }
    });
}


function bulkAssignUsersToParkingGroup(req, res) {
    var user = req.getCurrentUser();
    if(!user || !user.login || !user.login.userid)
    {
        global.log.error("Access not granted", JSON.stringify(user));
        return response.Done({error:true,message: "Access not granted", status:403}, null, res, req);
    }

    var userId = req.getCurrentUser()
  var orgid = req.swagger.params.orgid.value;
  var siteid = req.swagger.params.siteid.value;
  var parkingGroupId = req.swagger.params.parkinggroupid.value
  var userids = req.body.userList;

    encoder.bulkAssignUsersToParkingGroup(userId, parkingGroupId, siteid, orgid, userids, function (err, msg) {
        response.Done(err,msg,res, req);
  });
}


function bulkUnassignUsersFromParkingGroup(req, res) {
    var user = req.getCurrentUser();
    if(!user || !user.login || !user.login.userid)
    {
        global.log.error("Access not granted", JSON.stringify(user));
        return response.Done({error:true,message: "Access not granted", status:403}, null, res, req);
    }

    var userId = req.getCurrentUser()
    var orgid = req.swagger.params.orgid.value;
    var siteid = req.swagger.params.siteid.value;
    var parkingGroupId = req.swagger.params.parkinggroupid.value
    var userids = req.body.userList;

    encoder.bulkUnassignUsersFromParkingGroup(userId, parkingGroupId, siteid, orgid, userids, function (err, msg) {
        response.Done(err,msg,res, req);
    });
}


function getParkingHistory(req, res) {
    var user = req.getCurrentUser();
    if(!user || !user.login || !user.login.userid)
    {
        global.log.error("Access not granted", JSON.stringify(user));
        return response.Done({error:true,message: "Access not granted", status:403}, null, res, req);
    }

    try {
    var resp = [];
    var siteid = req.swagger.params.siteid.value;
    var filter = req.body;
    if (filter.startTime) {
        filter.from = Date.parse(filter.startTime) * 1000;
        if(filter.endTime)
          filter.to = Date.parse(filter.endTime) * 1000;
        else
          filter.to = Date.now() * 1000;
        if(isNaN(filter.from) || isNaN(filter.to))  {
          global.log.error({error: true, message: "Bad date format in parking historic data API call", status: 400});
          response.Done({error: true, message: "Bad date format in parking historic data API call", status: 400}, null, res, req);
        }
    }
    var resolution = 15;
    if(filter.timeResolution) {
      switch (filter.timeResolution) {
        case "15min":
        break;
        case "1hr":
        resolution = 60;
        break;
        case "1day":
        resolution = 1440;
        break;
      }
    }
    var factor = resolution/15;
    if(!filter.type)
      filter.type = "Demarcated";
    if(filter.type==="Undemarcated")
      filter.type = "NonDemarcated";

    if(!filter.spatialResolution) {
      filter.spatialResolution = 'site';
    }

    encoder.getParkingInfoSite(req.getCurrentUser(), siteid, filter, function (err, msg) {
      try {
        var cnt = 0;
        if(!err && msg) {
          for(var i=0;i<msg.length;i++) {
            if(i%factor===0) {
              resp.push({ 'time': msg[i].startdt, 'occupancy': msg[i].occpercent, 'duration': msg[i].occduration/(1000*1000.0), 'turnover': msg[i].turnovers });
              cnt=1;
            } else {
              if(resp.length>0) {
                resp[resp.length-1].occupancy += msg[i].occpercent;
                resp[resp.length-1].duration += msg[i].occduration;
                resp[resp.length-1].turnover += msg[i].turnovers;
                cnt++;
                if(i%factor===factor-1 || i===msg.length-1) {
                  resp[resp.length-1].occupancy /= cnt;
                  resp[resp.length-1].until = msg[i].enddt;
                }
              }
            }
          }
        }
        response.Done(err, resp, res, req);
      } catch (exc) {
        global.log.error({error: true, message: exc.message, status: 500})
        response.Done({error: true, message: exc.message, status: 500}, null, res, req);
      }
    });
  } catch (exc) {
      global.log.error({error: true, message: exc.message, status: 500})
      response.Done({error: true, message: exc.message, status: 500}, null, res, req);
  }
}

function getParkingInfoSite(req, res) {
    var user = req.getCurrentUser();
    if(!user || !user.login || !user.login.userid)
    {
        global.log.error("Access not granted", JSON.stringify(user));
        return response.Done({error:true,message: "Access not granted", status:403}, null, res, req);
    }

    var pgs = {};
    try {
        var siteid = req.swagger.params.siteid.value;
        var filter = req.body;
        if(filter.startTime) {
          getParkingHistory(req, res);
          return;
        }

        encoder.getAllParkingGroups(req.getCurrentUser(), req.swagger.params.siteid.value, function(err, groups) {
          if(!err) {
            for(var j=0;j<groups.length;j++) {
                var g = groups[j];
                var zones = g.parkingzones.split(",");
                for(var k=0;k<zones.length;k++) {
                    pgs[zones[k].toLowerCase()] = g.parkinggroupid;
                }
            }
            encoder.getParkingInfoSite(req.getCurrentUser(), siteid, filter, function (err, msg) {
                try {
                    if(err){
                        return response.Done(err, null, res, req);
                    }
                    var i = 0;
                    if ( msg && filter && (Object.keys(filter).length !== 0)) {
                        var resp = [];
                        var zones = {};
                        var unassigned_zones = [];
                        var groups = {};
                        var sites = {};
                        var spots = [];
                        var active_filter = filter.active_filter!=undefined?filter.active_filter:'active';

                        // Simple ID based filter
                        if (filter.spatialResolution === "unassigned_zone" && !filter.from) {
                            for (i = 0; i < msg.length; i++) {
                                var parkinggroupid = pgs[msg[i].parkingzoneid.toLowerCase()] ? pgs[msg[i].parkingzoneid.toLowerCase()] : "Unknown";
                                if (parkinggroupid === "Unknown" && unassigned_zones.indexOf(msg[i].parkingzoneid) === -1) {
                                    unassigned_zones.push(msg[i].parkingzoneid);
                                    resp.push({
                                        "parkingzoneid": msg[i].parkingzoneid,
                                        "nodeid": msg[i].nodeid,
                                        "demarcated": msg[i].demarcated,
                                        "channel": msg[i].channel
                                    });
                                }
                            }
                            response.Done(err, resp, res, req);
                            return;
                        }

                        for (i = 0; i < msg.length; i++) {
                            var oldlength = resp.length;

                            if(filter.type && filter.type==="Undemarcated" && msg[i].demarcated!==false)
                                continue;

                            if(filter.type && filter.type==="Demarcated" && msg[i].demarcated!==true)
                                continue;

                            if(filter.occupancy!==undefined && filter.occupancy!==msg[i].occupancy)
                                continue;

                            if(active_filter==='active' && msg[i].active===false)
                                continue;

                            if(active_filter==='inactive' && msg[i].active===true)
                                continue;

                            var parkinggroupid = pgs[msg[i].parkingzoneid.toLowerCase()] ? pgs[msg[i].parkingzoneid.toLowerCase()] : "Unknown";

                            if (filter.parkingzoneid && filter.parkingzoneid === msg[i].parkingzoneid)
                                resp.push(JSON.parse(JSON.stringify(msg[i])));
                            else if (filter.parkinggroupid && filter.parkinggroupid === parkinggroupid)
                                resp.push(JSON.parse(JSON.stringify(msg[i])));
                            else if (filter.parkingspotid && filter.parkingspotid === msg[i].parkingspotid)
                                resp.push(JSON.parse(JSON.stringify(msg[i])));
                            else if (!filter.parkingspotid && !filter.parkinggroupid && !filter.parkingzoneid)
                                resp.push(JSON.parse(JSON.stringify(msg[i])));
                            else if (filter.nodeid && filter.nodeid.toLowerCase() === msg[i].nodeid.toLowerCase())
                                resp.push(JSON.parse(JSON.stringify(msg[i])));

                            if(resp.length!==oldlength){
                                if(resp[resp.length-1].activesince)
                                    resp[resp.length-1].activesince = new Date(parseInt(parseInt(resp[resp.length-1].activesince)/1000));
                                if(resp[resp.length-1].since)
                                    resp[resp.length-1].since = new Date(parseInt(parseInt(resp[resp.length-1].since)/1000));
                                resp[resp.length-1].parkinggroupid = parkinggroupid;
                            }
                        }
                        response.Done(err, resp, res, req);
                    } else if(msg){
                        var resp = [];
                        for (i = 0; i < msg.length; i++) {
                          var parkinggroupid = pgs[msg[i].parkingzoneid.toLowerCase()] ? pgs[msg[i].parkingzoneid.toLowerCase()] : "Unknown";
                          resp.push(JSON.parse(JSON.stringify(msg[i])));
                          resp[i].activesince = new Date(parseInt(parseInt(resp[i].activesince)/1000));
                          resp[i].since = new Date(parseInt(parseInt(resp[i].since)/1000));
                          resp[i].parkinggroupid = parkinggroupid;
                        }
                        response.Done(err, resp, res, req);
                    }
                    else {
                        response.Done(err, [], res, req);
                    }
                } catch (exc) {
                    global.log.error({error: true, message: exc.message, status: 500})
                    response.Done({error: true, message: exc.message, status: 500}, null, res, req);
                }
            });
          }
        });
    } catch (exc) {
        global.log.error({error: true, message: exc.message, status: 500})
        response.Done({error: true, message: exc.message, status: 500}, null, res, req);
    }
}

// Samples of aggregation_parking_zone
//siteid                               | zoneid               | starttime        | enddt            | endtime          | groupid | occduration | occpercent | parkingtype   | startday   | startdt          | starthr       | turnovers
//-------------------------------------+----------------------+------------------+------------------+------------------+---------+-------------+------------+---------------+------------+------------------+---------------+-----------
//000d2ef0-a16d-11e6-9b18-c3812f7d931e | 9d3LWSWQXYYsizmZz6ac | 1480212000000000 | 2016-11-27 02:15 | 1480212900000000 | Unknown |           0 |          0 | NonDemarcated | 2016-11-27 | 2016-11-27 02:00 | 2016-11-27 02 |         0
//000d2ef0-a16d-11e6-9b18-c3812f7d931e | 9d3LWSWQXYYsizmZz6ac | 1480212900000000 | 2016-11-27 02:30 | 1480213800000000 | Unknown |           0 |          0 | NonDemarcated | 2016-11-27 | 2016-11-27 02:15 | 2016-11-27 02 |         0

// Samples of aggregation_parking_spot
//siteid                               | starttime        | parkingspotid          | enddt            | endtime          | groupid | occduration | occpercent | parkingtype | startday   | startdt          | starthr       | turnovers | zoneid
//-------------------------------------+------------------+------------------------+------------------+------------------+---------+-------------+------------+-------------+------------+------------------+---------------+-----------+----------------------
//8d5c9260-81a5-11e6-8d0b-d3dfdd826d09 | 1480212000000000 | l:u&KzcGM.OHr=1kTzsV-0 | 2016-11-27 02:15 | 1480212900000000 | Unknown |           0 |          0 |  Demarcated | 2016-11-27 | 2016-11-27 02:00 | 2016-11-27 02 |         0 | l:u&KzcGM.OHr=1kTzsV
//8d5c9260-81a5-11e6-8d0b-d3dfdd826d09 | 1480212900000000 | l:u&KzcGM.OHr=1kTzsV-0 | 2016-11-27 02:30 | 1480213800000000 | Unknown |           0 |          0 |  Demarcated | 2016-11-27 | 2016-11-27 02:15 | 2016-11-27 02 |         0 | l:u&KzcGM.OHr=1kTzsV

// Samples of aggregation_parking_site
//siteid                               | starttime        | parkingtype   | enddt            | endtime          | occduration | occpercent | startday   | startdt          | starthr       | turnovers
//-------------------------------------+------------------+---------------+------------------+------------------+-------------+------------+------------+------------------+---------------+-----------
//5370b2a0-a1eb-11e6-8fcd-a11458e8a79d | 1480383900000000 | NonDemarcated | 2016-11-29 02:00 | 1480384800000000 |   900000000 |        100 | 2016-11-29 | 2016-11-29 01:45 | 2016-11-29 01 |         0
//5370b2a0-a1eb-11e6-8fcd-a11458e8a79d | 1480384800000000 | NonDemarcated | 2016-11-29 02:15 | 1480385700000000 |   900000000 |        100 | 2016-11-29 | 2016-11-29 02:00 | 2016-11-29 02 |         0

/* End parking groups */
