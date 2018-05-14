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

console.log('Syntax: exp 2xServer 2xSiteID 3xOrgID 3xSiteID 2xKey 3xKey 3xServer 3xDCC');

function createConfigs() {
  for(var i=0;i<eData.nodes.length;i++) {
    if(eData.nodes[i].node && eData.nodes[i].node.config && eData.nodes[i].node.config.active) {
      var active = eData.nodes[i].node.config.active;
      var cfg = JSON.parse(JSON.stringify(active.micronode || active.roadrunner || active.carrierpigeon || active.hummingbird));
      for (var propName in cfg) {
        if (propName.indexOf('sensor')===0) {
          delete cfg[propName];
        }
      }
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
          console.log("Adding node %s to cfg %s", eData.nodes[i].path, index);
          eData.cfgs[index].nodes.push(eData.nodes[i].path);
        }
      }
    }
  }
}

function createSchedules() {
  for(var i=0;i<eData.nodes.length;i++) {
    if(eData.nodes[i].light && eData.nodes[i].light.policy && eData.nodes[i].hardware && eData.nodes[i].hardware.model!=='falcon-q') {
      var lg = eData.nodes[i].light.policy;
      var index = -1;
      for(var k=0;k<eData.schedules.length;k++) {
        if(eData.schedules[k].scope===lg.scope) {
          index = k;
          break;
        }
      }
      if(index===-1) {
        eData.schedules.push(lg);
        index = eData.schedules.length-1;
        eData.schedules[index].nodes = [];
      }
      eData.schedules[index].nodes.push(eData.nodes[i].path);
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

  var node = { nodeid: n.path, name: n.name ? n.name : "", latitude: ""+n.location.latitude, longitude: ""+n.location.longitude, model: n.hardware.model };

  if(node.model==="falcon-q" || (node.business && node.business.site && node.business.site.scope!==siteid)) {
    cb(null);
    console.log("Skip video nodes for now");
    return;
  }

  if(n.business && n.business.level && n.business.level.name)
    node.level = n.business.level.name;

  console.log("Importing node: ", node);

  var url = 'https://'+host+'.sensity.com/v3.0/customers/'+torg+'/sites/'+tsite+'/nodes';
  request
    .post(url)
    .set('Accept', 'application/json')
    .set('api_key', tkey)
    .send(node)
    .end(function(err,res){
      if (err || !res.ok) {
        console.log('Server error in response to creating node: %s, %s', node.nodeid, err);
        console.log('Reason: ', res.body);
        //console.log('URL: ', url);
        createOneNode(n,i,cb);
      } else {
        console.log('Node created: ', res.body);
        setTimeout(function() {cb(null)}.bind(this),5000);
      }
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
  for(var k=0;k<grp.nodes.length;k++)
    nodes.push(grp.nodes[k]);

  if(nodes.length===0) {
    cb(null);
    return;
  }

  var group = { nodeList: nodes, name: grp.name, type: "organizational", description: ""};

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

  if(!fix.fixture || !fix.fixture.type || fix.nodes.length===0) {
    cb(null);
    return;
  }

  var fixture = {     name: fix.name,
                      description: fix.fixture.description ? fix.fixture.description : "",
                      fixtureType: fix.fixture.type,
                      manufacturer: fix.fixture.manufacturer ? fix.fixture.manufacturer : "",
                      manufacturer_sku: fix.fixture.catalog_number ? fix.fixture.catalog_number : "",
                      MaxPower0: ""+fix.fixture.max_pow0,
                      MaxPower10: ""+fix.fixture.max_pow10,
                      MaxPower50: ""+fix.fixture.max_pow50,
                      MaxPower100: ""+fix.fixture.max_pow100,
                      MinPower100: ""+fix.fixture.min_pow100,
                      MinPower50: ""+fix.fixture.min_pow50,
                      MinPower10: ""+fix.fixture.min_pow10,
                      MinPower0: ""+fix.fixture.min_pow0,
                      PowerDraw: ""+fix.fixture.power_draw,
                      MinimumLightLevelForFailureDetection: "" + fix.legacy_fixture.failure_light_level };
  if(fix.legacy_fixture) {
    fixture.BallastCost = "" + fix.legacy_fixture.ballast_cost;
    fixture.BulbCost = "" + fix.legacy_fixture.bulb_cost;
    fixture.LegacyPowerDraw = "" + fix.legacy_fixture.legacy_power_draw;
    fixture.DailyOperatingTime = "" + fix.legacy_fixture.operating_time;
  }

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

  if(grp.nodes.length===0) {
    cb(null);
    return;
  }

  console.log("Importing one light group: ",grp);

  var group = { nodeList: grp.nodes, name: grp.name, type: "lighting", description: ""};
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
    if(eData.nodes[k].path===conf.nodes[0]) {
      foundUnder = k;
      break;
    }
  }

  conf = JSON.parse(JSON.stringify(conf));

  if(foundUnder!==-1)
    model = eData.nodes[foundUnder].hardware.model;

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
    cfg.networkXPasskey = encoder.htmlDecode(conf['network.x.security.psk'] ? conf['network.x.security.psk'] : cfg.networkXPasskey);
  if(cfg.networkXSSID)
    cfg.networkXSSID = conf['network.x.ssid'] ? conf['network.x.ssid'] : cfg.networkXSSID;
  if(cfg.networkXSecurity)
    cfg.networkXSecurity = 'wpa2p';
  if(cfg.networkYPasskey)
    cfg.networkYPasskey = encoder.htmlDecode(conf['network.y.security.psk'] ? conf['network.y.security.psk'] : cfg.networkYPasskey);
  if(cfg.networkYSSID)
    cfg.networkYSSID = conf['network.y.ssid'] ? conf['network.y.ssid'] : cfg.networkYSSID;
  if(cfg.networkYSecurity)
    cfg.networkYSecurity = 'wpa2p';
  if(cfg.network_region)
    cfg.network_region = conf['network.region'] ? conf['network.region'] : cfg.network_region;
  if(cfg.ota_disable)
    cfg.ota_disable = conf['ota.disable'] ? conf['ota.disable'] : cfg.ota_disable;
  if(cfg.server)
    cfg.server = tdcc + '.sensity.com';
  if(cfg.telnet)
    cfg.telnet = conf['network.telnet'] ? conf['network.telnet'] : cfg.telnet;
  if(cfg.vpn_on_demand)
    cfg.vpn_on_demand = conf['network.vpn_on_demand'] ? conf['network.vpn_on_demand'] : cfg.vpn_on_demand;

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

function oneFixture(fix, i, cb) {
  var host = iParams.server;
  var siteid = iParams.siteid;
  var key = iParams.key;
  var fid = fix.id
  var url1 = 'https://'+host+'.sensity.com/2.0/'+fid+'//devices.json?aspects=_&api_key='+key;

  fix.nodes = [];
  for(var j=0;j<eData.nodes.length;j++) {
    var node = eData.nodes[j];
    if(node && node.light && node.light.fixture && node.light.fixture.scope && node.light.fixture.scope===fid)
      fix.nodes.push(node.path);
  }
  console.log('Nodes that belong to fixture: ', fid);
  console.log(fix.nodes);
  cb(null);
}

function exportFixturesFrom2x(cb) {
  console.log("Starting to export fixtures");
  var host = iParams.server;
  var siteid = iParams.siteid;
  var key = iParams.key;
  var url = 'https://'+host+'.sensity.com/2.0/top//fixtures.json?api_key='+key;
  request
  .get(url)
  .set('Accept', 'application/json')
  .send()
  .end(function(err, res){
    if (err || !res.ok) {
      console.log('Server error in response to export fixtures: ' + err);
      console.log('URL: ', url);
      cb(null);
    } else {
      console.log('Received fixtures: ',res.body.scopes);
      eData.fixtures = res.body.scopes;
      async.eachOfSeries(eData.fixtures, oneFixture, function() { console.log ("Done exporting fixtures"); cb(null);});
    }
  }.bind(this));
}

function oneGroup(grp, i, cb) {
  var host = iParams.server;
  var siteid = iParams.siteid;
  var key = iParams.key;
  var gid = grp.id;
  var url1 = 'https://'+host+'.sensity.com/2.0/'+gid+'//devices.json?aspects=_&api_key='+key;

  request
    .get(url1)
    .set('Accept', 'application/json')
    .send()
    .end(function(err,res){
      if (err || !res.ok) {
        console.log('Server error in response to nodes in group %s: %s', gid, err);
        console.log('URL: ', url1);
      } else {
        grp.nodes = [];
        var nodes = res.body.scopes;
        for(var j=0;j<eData.nodes.length;j++) {
          var nodeid = eData.nodes[j].path;
          for(var k=0;k<nodes.length;k++)
            if(nodes[k].path===nodeid)
              grp.nodes.push(nodeid);
        }
      }
      cb(null);
    });
}

function exportGroupsFrom2x(cb) {
  console.log("Starting to export Groups");
  var host = iParams.server;
  var siteid = iParams.siteid;
  var key = iParams.key;
  var url = 'https://'+host+'.sensity.com/2.0/'+siteid+'//tags.json?api_key='+key;
  request
  .get(url)
  .set('Accept', 'application/json')
  .send()
  .end(function(err, res){
    if (err || !res.ok) {
      console.log('Server error in response to export groups: ' + err);
      console.log('URL: ', url);
      cb(null);
    } else {
      console.log('Received groups: ',res.body.scopes);
      eData.groups = res.body.scopes;
      async.eachOfSeries(eData.groups, oneGroup, function() { console.log ("Done exporting groups"); cb(null);});
    }
  }.bind(this));
}

function exportNodesConfigSchedulesFrom2x(cb) {
  console.log("Starting to export Nodes, Config & Schedules");
  var host = iParams.server;
  var siteid = iParams.siteid;
  var key = iParams.key;
  var org = iParams.torg;
  var site = iParams.tsite;
  var url = 'https://'+host+'.sensity.com/2.0/'+siteid+'//devices.json?aspects=hardware,business,location,name,node,light&api_key='+key;
  request
  .get(url)
  .set('Accept', 'application/json')
  .send()
  .end(function(err, res){
    if (err || !res.ok) {
      console.log('Server error in response to export nodes: ' + err);
      console.log('URL: ', url);
    } else {
      var nodes = res.body.scopes;
      for(var j=0;j<nodes.length;j++) {
        if(nodes[j].node) {
          if(nodes[j].node.firmware)
            delete nodes[j].node.firmware;
          if(nodes[j].node.certificate)
            delete nodes[j].node.certificate;
          if(nodes[j].node.config && nodes[j].node.config.active && nodes[j].node.config.active.vulture)
            delete nodes[j].node.config.active.vulture;
        }
        if(nodes[j].business && nodes[j].business.site && nodes[j].business.site.scope===siteid && nodes[j].hardware.model!=='falcon-q')
          eData.nodes.push(nodes[j]);
      }
      createConfigs();
      createSchedules();
      console.log("Done with exporting Nodes, Config & Schedules");
    }
    cb(null);
  }.bind(this));
}

program
  .arguments('<server>')
  .arguments('<siteid>')
  .arguments('<torg>')
  .arguments('<tsite>')
  .arguments('<key>')
  .arguments('<tkey>')
  .arguments('<tsrv>')
  .arguments('<tdcc>')
  .action(function(server,siteid,torg,tsite,key,tkey,tsrv,tdcc) {
        iParams.server = server;
        iParams.siteid = siteid;
        iParams.torg = torg;
        iParams.tsite = tsite;
        iParams.tkey = tkey;
        iParams.key = key;
        iParams.tsrv = tsrv;
        iParams.tdcc = tdcc;
        console.log('  Server to import from: ' + server + '.sensity.com');
        console.log('  Site ID: ' + siteid);
        console.log('  Target server: ' + tsrv + '.sensity.com');
        console.log('  Target org: ' + torg);
        console.log('  Target site: ' + tsite);
        console.log('  DCC host name: ' + tdcc + '.sensity.com');

        async.series([
            exportNodesConfigSchedulesFrom2x,
            exportFixturesFrom2x,
            exportGroupsFrom2x
        ], function(err, result) {
            console.log("\n\n\n\nEXPORT DONE\n");
            console.log("EXPORTED DATA: \n",JSON.stringify(eData, null, 4));
            //process.exit();
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
