#!/usr/bin/env node
'use strict';
const program = require('commander');
const request = require('superagent');
const async = require('async');
const Encoder = require('node-html-encoder').Encoder;
// entity type encoder
var encoder = new Encoder('entity');

var eData = {'nodes': [], 'cfgs': [], 'fixtures': [], 'schedules': [], 'groups': []};
var iParams = {};
var defaultConfigs = {};
var cookie = "";

console.log('Syntax: exp1x 1xServer 1xSiteID 3xOrgID 3xSiteID username password 3xKey 3xServer 3xDCC');

function createConfigs() {
  var tdcc = iParams.tdcc;

  for(var i=0;i<eData.nodes.length;i++) {
    var cfg = {
      model: eData.nodes[i]['hw.model'],
      networkXPasskey: encoder.htmlDecode(eData.nodes[i]['n.network.x.security.psk']),
      networkXSSID: eData.nodes[i]['n.network.x.ssid'],
      networkXSecurity: 'wpa2p',
      networkYPasskey: encoder.htmlDecode(eData.nodes[i]['n.network.y.security.psk']),
      networkYSSID: eData.nodes[i]['n.network.y.ssid'],
      networkYSecurity: 'wpa2p',
      debugmode: eData.nodes[i]['n.debugmode'],
      network_region: eData.nodes[i]['n.network.region'],
      ota_disable: eData.nodes[i]['n.ota_disable'],
      telnet: eData.nodes[i]['n.telnet'],
      vpn_on_demand: eData.nodes[i]['n.vpn_on_demand'],
      server: tdcc + '.sensity.com',
    };
    var cpy = null;
    var nodes = [];
    if(cfg) {
      var index = -1;
      for(var k=0;k<eData.cfgs.length;k++) {
        cpy = JSON.parse(JSON.stringify(eData.cfgs[k]));
        nodes = cpy['nodes'];
        delete cpy['nodes'];
        if(JSON.stringify(cpy)===JSON.stringify(cfg)) {
          index = k;
          cpy['nodes'] = nodes;
          break;
        }
      }
      if(index===-1 && (Object.keys(cfg).length!==0)) {
        console.log("Pushing new config: ",cfg)
        eData.cfgs.push(cfg);
        index = eData.cfgs.length-1;
        eData.cfgs[index].nodes = [];
      }
      if(Object.keys(cfg).length!==0) {
        eData.cfgs[index].nodes.push(eData.nodes[i].id);
      }
    }
  }
}

function createGroups() {
  for(var i=0;i<eData.nodes.length;i++) {
    if(eData.nodes[i].tags) {
      for(var j=0;j<eData.nodes[i].tags.length;j++) {
        var gid = eData.nodes[i].tags[j];
        var index = -1;
        for(var k=0;k<eData.groups.length;k++) {
          if(eData.groups[k].id===gid) {
            index = k;
            break;
          }
        }
        if(index!==-1)
          eData.groups[index].nodes.push(eData.nodes[i].id);
      }
    }
  }
}

function createSchedules() {
  for(var k=0;k<eData.schedules.length;k++) {
    var s = eData.schedules[k];
    s.nodes = [];
    for(var i=0;i<eData.nodes.length;i++) {
      var node = eData.nodes[i];
      if(node['li.profile']) {
        var lgid = node['li.profile'];
        if(s.id===lgid) {
          s.nodes.push(eData.nodes[i].id);
        }
      }
    }
  }
}

function importGroupsTo3x(cb) {
  console.log("Starting to import groups");
  async.eachOfSeries(eData.groups, createOneGroup, function() { console.log ("Done importing groups"); cb(null);});
}

function createOneNode(n, i, cb) {
  var host = iParams.tsrv;
  var torg = iParams.torg;
  var tsite = iParams.tsite;
  var tkey = iParams.tkey;
  var siteid = iParams.siteid;

  var node = { nodeid: n.id, name: n['n.name'] ? n['n.name'] : "", latitude: ""+n['pos.latitude'], longitude: ""+n['pos.longitude'], model: n['hw.model'] };
  if(n['n.note'])
    node.note = n['n.note'];

  if(n['pos.level'])
    node.level = n['pos.level'];

  if(n['lpos.site']!==siteid) {
    cb(null);
    return;
  }

  console.log("Importing node: ", node);

  var url = 'https://'+host+'.sensity.com/v3.0/customers/'+torg+'/sites/'+tsite+'/nodes';
  request
    .post(url)
    .set('Accept', 'application/json')
    .set('api_key', tkey)
    .send(node)
    .end(function(err,res){
      if (err || !res.ok) {
        console.log('Server error in response to creating node %s: %s', node.nodeid, err);
        console.log('REASON: ',res.body);
        console.log('URL: ', url);
      } else {
        console.log('Node created: ', res.body);
      }
      setTimeout(function() {cb(null)}.bind(this),5000);
    });
}

function createOneGroup(grp, i, cb) {
  var host = iParams.tsrv;
  var torg = iParams.torg;
  var tsite = iParams.tsite;
  var tkey = iParams.tkey;
  var gid = grp.scope;
  var url = 'https://'+host+'.sensity.com/v3.0/customers/'+torg+'/sites/'+tsite+'/groups';

  var nodes = [];
  if(!grp.nodes) {
    cb(null);
    return;
  }
  for(var k=0;k<grp.nodes.length;k++)
    nodes.push(grp.nodes[k]);

  if(nodes.length===0) {
    cb(null);
    return;
  }

  var group = { nodeList: nodes, name: grp['tag.name'], type: "organizational", description: grp['tag.description']};

  console.log("Importing group: ",group);

  request
    .post(url)
    .set('Accept', 'application/json')
    .set('api_key', tkey)
    .send(group)
    .end(function(err,res){
      if (err || !res.ok) {
        console.log('Server error in response to creating groups %s: %s', gid, err);
        console.log('URL: ', url);
      } else {
        console.log('Group and node relationships created: ', res.text);
      }
      cb(null);
    });
}

function assignFixtureToOneNode(node, i, cb) {
  var host = iParams.tsrv;
  var torg = iParams.torg;
  var tsite = iParams.tsite;
  var tkey = iParams.tkey;
  var url = 'https://'+host+'.sensity.com/v3.0/customers/'+torg+'/sites/'+tsite+'/fixtures/' + node.fixid + '/assign/node/' + node.nodeid;

  request
    .post(url)
    .set('Content-Type', 'application/json')
    .set('Accept', 'application/json')
    .set('api_key', tkey)
    .send()
    .end(function(err,res){
      if (err || !res.ok) {
        console.log('Server error in response to assigning fixture %s to node %s', node.fixid, node.nodeid);
        console.log('URL: ', url);
      } else {
      }
      cb(null);
    });
}

function createOneFixture(fix, i, cb) {
  var host = iParams.tsrv;
  var torg = iParams.torg;
  var tsite = iParams.tsite;
  var tkey = iParams.tkey;
  var fid = fix.scope;
  var url = 'https://'+host+'.sensity.com/v3.0/customers/'+torg+'/sites/'+tsite+'/fixtures';

  if(fix.nodes.length===0) {
    cb(null);
    return;
  }

  var fixture = {     name: fix['f.name'],
                      description: fix['f.description'],
                      fixtureType: fix['f.type'],
                      manufacturer: fix['f.manufacturer'],
                      manufacturersku: fix['f.catalogNumber'],
                      MaxPower0: ""+fix['l.watts.max.pow0'],
                      MaxPower10: ""+fix['l.watts.max.pow10'],
                      MaxPower50: ""+fix['l.watts.max.pow50'],
                      MaxPower100: ""+fix['l.watts.max.pow100'],
                      MinPower100: ""+fix['l.watts.min.pow100'],
                      MinPower50: ""+fix['l.watts.min.pow50'],
                      MinPower10: ""+fix['l.watts.min.pow10'],
                      MinPower0: ""+fix['l.watts.min.pow0'],
                      PowerDraw: ""+fix['l.watts'],
                      MinimumLightLevelForFailureDetection: ""+fix['l.failure.light.level'] };

  if(fix['l.cost.ballast'])
    fixture.BallastCost = "" + fix['l.cost.ballast'];
  if(fix['l.cost.bulb'])
    fixture.BulbCost = "" + fix['l.cost.bulb'];
  if(fix['l.watts.legacy'])
    fixture.LegacyPowerDraw = "" + fix['l.watts.legacy'];
  if(fix['l.hours.daily'])
    fixture.DailyOperatingTime = "" + fix['l.hours.daily'];

  console.log("Importing fixture: ", fixture);

  request
    .post(url)
    .set('Accept', 'application/json')
    .set('api_key', tkey)
    .send(fixture)
    .end(function(err,res){
      if (err || !res.ok) {
        console.log('Server error in response to creating fixture %s: %s', fid, err);
        console.log('URL: ', url);
        cb(null);
      } else {
        console.log('Fixture created: ', res.body.fixtureid);
        fix.newid = res.body.fixtureid;
        var nodesArray = [];
        for(var k=0;k<fix.nodes.length;k++)
          nodesArray.push({nodeid: fix.nodes[k], fixid: fix.newid});
        async.eachOfSeries(nodesArray, assignFixtureToOneNode, function() { cb(null); });
      }
    });
}

function createOneLGroup(grp, i, cb) {
  var host = iParams.tsrv;
  var torg = iParams.torg;
  var tsite = iParams.tsite;
  var tkey = iParams.tkey;
  var gid = grp.scope;
  var url = 'https://'+host+'.sensity.com/v3.0/customers/'+torg+'/sites/'+tsite+'/groups';

  if(!grp.nodes || grp.nodes.length===0) {
    cb(null);
    return;
  }

  console.log("Importing one light group: ",grp);

  var group = { nodeList: grp.nodes, name: grp['profile.name'], type: "lighting", description: grp['profile.description']};
  request
    .post(url)
    .set('Accept', 'application/json')
    .set('api_key', tkey)
    .send(group)
    .end(function(err,res){
      if (err || !res.ok) {
        // Lighting group needs a schedule but we don't create it here
        //console.log('Server error in response to creating lighting groups %s: %s', gid, err);
        //console.log('URL: ', url);
      } else {
        console.log('Lighting group and node relationships created: ', res.text);
      }
      setTimeout(function() {cb(null)}.bind(this),5000);
    });
}

function getOneModelConfig(model, i, cb) {
  var host = iParams.tsrv;
  var tkey = iParams.tkey;
  var url = 'https://'+host+'.sensity.com/v3.0/configs/'+model;

  request
    .get(url)
    .set('Accept', 'application/json')
    .set('api_key', tkey)
    .send()
    .end(function(err,res){
      if (err || !res.ok) {
        console.log('Server error in response to fetching default config for model %s: %s', model, err);
        console.log('URL: ', url);
      } else {
        defaultConfigs[model] = res.body;
      }
      cb(null);
    });
}

function getDefaultConfigs(cb) {
  console.log("Starting to import config");
  var models = ["unode-v1","unode-v2","unode-v3","unode-v4","unode-v5"];
  async.eachOfSeries(models, getOneModelConfig, function() { console.log ("Done getting default configs: ", JSON.stringify(defaultConfigs,null,4)); cb(null);});
}

function createOneConfig(conf, i, cb) {
  var host = iParams.tsrv;
  var torg = iParams.torg;
  var tsite = iParams.tsite;
  var tkey = iParams.tkey;
  var tdcc = iParams.tdcc;

  var foundUnder = -1;
  var model = "";

  for(var k=0;k<eData.nodes.length;k++) {
    if(eData.nodes[k].id===conf.nodes[0]) {
      foundUnder = k;
      break;
    }
  }

  conf = JSON.parse(JSON.stringify(conf));

  if(foundUnder!==-1)
    model = eData.nodes[foundUnder]['hw.model'];

  if(model==="falcon-q" || model==="") {
    cb(null);
    return;
  }

  var cfg = JSON.parse(JSON.stringify(defaultConfigs[model]));

  cfg.model = model;
  if(cfg.debugmode)
    cfg.debugmode = conf['debugmode'] ? conf['debugmode'] : cfg.debugmode;
  cfg.name = 'Cfg_V'+i;
  if(cfg.aux_power)
    cfg.aux_power = conf['aux_power'] ? conf['aux_power'] : cfg.aux_power;
  if(cfg.networkXPasskey)
    cfg.networkXPasskey = conf['networkXPasskey'] ? conf['networkXPasskey'] : cfg.networkXPasskey;
  if(cfg.networkXSSID)
    cfg.networkXSSID = conf['networkXSSID'] ? conf['networkXSSID'] : cfg.networkXSSID;
  cfg.networkXSecurity = 'wpa2p';
  cfg.networkYPasskey = conf['networkYPasskey'] ? conf['networkYPasskey'] : cfg.networkYPasskey;
  cfg.networkYSSID = conf['networkYSSID'] ? conf['networkYSSID'] : cfg.networkYSSID;
  cfg.networkYSecurity = 'wpa2p';
  if(cfg.network_region)
    cfg.network_region = conf['network_region'] ? conf['network_region'] : cfg.network_region;
  if(cfg.ota_disable)
    cfg.ota_disable = conf['ota_disable'] ? conf['ota_disable'] : cfg.ota_disable;
  if(cfg.server)
    cfg.server = tdcc + '.sensity.com';
  if(cfg.telnet)
    cfg.telnet = conf['telnet'] ? conf['telnet'] : cfg.telnet;
  if(cfg.vpn_on_demand)
    cfg.vpn_on_demand = conf['vpn_on_demand'] ? conf['vpn_on_demand'] : cfg.vpn_on_demand;

  console.log("Import config: ", cfg);
  var url = 'https://'+host+'.sensity.com/v3.0/customers/'+torg+'/sites/'+tsite+'/configs';
  request
    .post(url)
    .set('Accept', 'application/json')
    .set('api_key', tkey)
    .send(cfg)
    .end(function(err,res){
      if (err || !res.ok) {
        console.log('Server error in response to creating config %s: %s', conf.scope, err);
        console.log('URL: ', url);
        setTimeout(function(){cb(null);}.bind(this),5000);
      } else {
        console.log('Config created: ', res.body.configid);
        conf.newid = res.body.configid;
        var url1 = 'https://'+host+'.sensity.com/v3.0/customers/'+torg+'/sites/'+tsite+'/configs/' + conf.newid + '/apply/nodes';

        request
          .post(url1)
          .set('Accept', 'application/json')
          .set('api_key', tkey)
          .send({nodeList: conf.nodes})
          .end(function(err,res){
            if (err || !res.ok) {
              console.log('Server error in response to assigning config %s to nodes', conf.newid );
              console.log(conf.nodes);
              console.log('URL: ', url1);
            } else {
            }
            setTimeout(function(){cb(null);}.bind(this),5000);
          });
      }
    });
}

function importConfigTo3x(cb) {
  console.log("Starting to import config");
  async.eachOfSeries(eData.cfgs, createOneConfig, function() { console.log ("Done importing configs"); cb(null);});
}

function importLightingGroupsTo3x(cb) {
  console.log("Starting to import lighting groups");
  async.eachOfSeries(eData.schedules, createOneLGroup, function() { console.log ("Done importing lighting groups"); cb(null);});
}

function importFixturesTo3x(cb) {
  console.log("Starting to import fixtures");
  async.eachOfSeries(eData.fixtures, createOneFixture, function() { console.log ("Done importing fixtures"); cb(null);});
}

function importNodesTo3x(cb) {
  console.log("Starting to import nodes");
  async.eachOfSeries(eData.nodes, createOneNode, function() { console.log ("Done importing nodes"); cb(null);});
}

function exportFixturesFrom1x(cb) {
  console.log("Starting to export fixtures");
  var host = iParams.server;
  var siteid = iParams.siteid;
  var url = 'https://'+host+'.sensity.com/api/fixture.json/query?api_key=fb62bfcc.79f435aae45c5e824ebcd984ada9f980&maxres=1000';
  request
    .get(url)
    .set('Accept', 'application/json')
    .set('Cookie', cookie)
    .send()
    .end(function(err, res){
      if (err || !res.ok) {
        console.log('Server error in response to export fixtures: ' + err);
        console.log('URL: ', url);
        exportFixturesFrom1x(cb);
      } else {
        eData.fixtures = res.body.records;
        for(var k=0;k<eData.fixtures.length;k++) {
          var f = eData.fixtures[k];
          f.nodes = [];
          for(var l=0;l<eData.nodes.length;l++) {
            var n = eData.nodes[l];
            if(f.id===n['li.fixture'])
              f.nodes.push(n.id);
          }
        }
        console.log("Fixtures: ", JSON.stringify(eData.fixtures,null,4));
        setTimeout(function() {cb(null)}.bind(this), 2000);
      }
    });
}

function exportSchedulesFrom1x(cb) {
    console.log("Starting to export Schedules");
    var host = iParams.server;
    var siteid = iParams.siteid;
    var url = 'https://'+host+'.sensity.com/api/profile.json/query?api_key=fb62bfcc.79f435aae45c5e824ebcd984ada9f980&maxres=1000';
    request
      .get(url)
      .set('Accept', 'application/json')
      .set('Cookie', cookie)
      .send()
      .end(function(err, res){
        if (err || !res.ok) {
          console.log('Server error in response to export schedules: ' + err);
          console.log('URL: ', url);
          exportSchedulesFrom1x(cb);
        } else {
          //console.log('Received schedules: ',res.body.records);
          eData.schedules = res.body.records;
          setTimeout(function() {cb(null)}.bind(this), 2000);
        }
      }.bind(this));
}

function exportGroupsFrom1x(cb) {
  console.log("Starting to export Groups");
  var host = iParams.server;
  var siteid = iParams.siteid;
  var url = 'https://'+host+'.sensity.com/api/tag.json/query?api_key=fb62bfcc.79f435aae45c5e824ebcd984ada9f980&maxres=1000';
  request
    .get(url)
    .set('Accept', 'application/json')
    .set('Cookie', cookie)
    .send()
    .end(function(err, res){
      if (err || !res.ok) {
        console.log('Server error in response to export groups: ' + err);
        console.log('URL: ', url);
        exportGroupsFrom1x(cb);
      } else {

        eData.groups = res.body.records;
        for(var i=0;i<eData.groups.length;i++)
          if(eData.groups[i]['tag.rule']!=='')
            eData.groups.splice(i,1);
        for(var k=0;k<eData.groups.length;k++) {
          var g = eData.groups[k];
          g.nodes = [];
          for(var l=0;l<eData.nodes.length;l++) {
            var n = eData.nodes[l];
            if(n['n.tags'].indexOf(g.id)!==-1)
              g.nodes.push(n.id);
          }
        }
        console.log('Received groups: ',eData.groups);
        setTimeout(function() {cb(null)}.bind(this), 2000);
      }
    }.bind(this));
}

function login1x(cb) {
  var host = iParams.server;
  var username = iParams.username;
  var password = iParams.password;
  var url = 'https://'+host+'.sensity.com/api/login.json/login';
  request
    .post(url)
    .set('Accept', 'application/json')
    .send({email:iParams.username, password:iParams.password})
    .end(function(err, res){
      if (err || !res.ok) {
        console.log('Server error in response to login: ' + res.status);
        console.log('URL: ', url);
        console.log('Response: ',res.body);
      } else {
        console.log('Successful login into 1x server: ', host);
        cookie = res.headers['set-cookie'];
      }
      cb(null);
    });
}

function exportNodesConfigFrom1x(cb) {
  console.log("Starting to export Nodes, Config & Schedules");
  var host = iParams.server;
  var siteid = iParams.siteid;
  var org = iParams.torg;
  var site = iParams.tsite;
  var url = 'https://'+host+'.sensity.com/api/node.json/query?filt=lpos.site%20eq%20"'+siteid+'"';
  request
    .get(url)
    .set('Accept', 'application/json')
    .set('Cookie', cookie)
    .send()
    .end(function(err, res){
      if (err || !res.ok) {
        console.log('Server error in response to export nodes: ' + err);
        console.log('URL: ', url);
        exportNodesConfigFrom1x(cb);
      } else {
        eData.nodes = res.body.records;
        createConfigs();
        createSchedules();
        console.log("Done with exporting Nodes, Config & Schedules");
        setTimeout(function() {cb(null)}.bind(this), 2000);
      }
    }.bind(this));

}

program
  .arguments('<server>')
  .arguments('<siteid>')
  .arguments('<torg>')
  .arguments('<tsite>')
  .arguments('<username>')
  .arguments('<password>')
  .arguments('<tkey>')
  .arguments('<tsrv>')
  .arguments('<tdcc>')
  .action(function(server,siteid,torg,tsite,username,password,tkey,tsrv,tdcc) {
        iParams.server = server;
        iParams.siteid = siteid;
        iParams.torg = torg;
        iParams.tsite = tsite;
        iParams.tkey = tkey;
        iParams.username = username;
        iParams.password = password;
        iParams.tsrv = tsrv;
        iParams.tdcc = tdcc;
        console.log('  Server to import from: ' + server + '.sensity.com');
        console.log('  Site ID: ' + siteid);
        console.log('  Target server: ' + tsrv + '.sensity.com');
        console.log('  Target org: ' + torg);
        console.log('  Target site: ' + tsite);
        console.log('  DCC host name: ' + tdcc + '.sensity.com');

        async.series([
            login1x,
            exportSchedulesFrom1x,
            exportNodesConfigFrom1x,
            exportFixturesFrom1x,
            exportGroupsFrom1x
        ], function(err, result) {
            //process.exit();
            console.log("\n\n\n\nEXPORT DONE\n");
            console.log("EXPORTED DATA: \n",JSON.stringify(eData, null, 4));

            async.series([
                getDefaultConfigs,
                importNodesTo3x,
                importConfigTo3x,
                importGroupsTo3x,
                importFixturesTo3x,
                importLightingGroupsTo3x
            ], function(err, result) {
                console.log("\n\n\n\nIMPORT DONE\n");
            });
        });
  })
  .parse(process.argv);
