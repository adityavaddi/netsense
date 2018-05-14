#!/usr/bin/env node
'use strict';
const program = require('commander');
const request = require('superagent');
const async = require('async');
const Encoder = require('node-html-encoder').Encoder;
const videoModels = ["falcon-q","merlin","vdkmaster"];

// entity type encoder
var encoder = new Encoder('entity');
var eData = {'nodes': [], 'cfgs': [], 'groups': []};
var iParams = {};
var defaultConfigs = {};

console.log('Syntax: exp 2xServer 2xSiteID 3xOrgID 3xSiteID 2xKey 3xKey 3xServer 3xDCC');

function createConfigs() {
  for(var i=0;i<eData.nodes.length;i++) {
    if(eData.nodes[i].node && eData.nodes[i].node.config && eData.nodes[i].node.config.active) {
      var active = eData.nodes[i].node.config.active;
      var cfg = JSON.parse(JSON.stringify(active["falcon-q"] || active["merlin"] || active["vdkmaster"]));
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

  if(videoModels.indexOf[node.model]===-1 || (node.business && node.business.site && node.business.site.scope!==siteid)) {
    cb(null);
    console.log("Skip non video nodes.");
    return;
  }

  if(n.business && n.business.level && n.business.level.name)
    node.level = n.business.level.name;

  console.log("Importing node: ", node);

  var geturl = 'https://'+host+'.sensity.com/v3.0/customers/'+torg+'/sites/'+tsite+'/nodes/'+node.nodeid;
  request
    .get(geturl)
    .set('Accept', 'application/json')
    .set('api_key', tkey)
    .end(function(err,res){
      if (err || !res.ok) {
        var posturl = 'https://'+host+'.sensity.com/v3.0/customers/'+torg+'/sites/'+tsite+'/nodes';
        request
          .post(posturl)
          .set('Accept', 'application/json')
          .set('api_key', tkey)
          .send(node)
          .end(function(err,res){
            if (err || !res.ok) {
              request // assign the node to the given site
                .post('https://'+host+'.sensity.com/v3.0/customers/'+torg+'/sites/'+tsite+'/nodes/'+node.nodeid+'/assign')
                .set('Accept', 'application/json')
                .set('Content-Type', 'application/json')
                .set('api_key', tkey)
                .end(function(err,res){
                    request
                      .post(geturl)
                      .set('Accept', 'application/json')
                      .set('api_key', tkey)
                      .send(node)
                      .end(function(err,res){
                        if (err || !res.ok) {
                          console.log('Server error in response to updating node: %s, %s', node.nodeid, err);
                          console.log('Reason: ', res.body);
                          //console.log('URL: ', url);
                          createOneNode(n,i,cb);
                        } else {
                          console.log('Node created: ', res.body);
                          setTimeout(function() {cb(null)}.bind(this),5000);
                        }
                      });
                    });
            } else {
              console.log('Node created: ', res.body);
              setTimeout(function() {cb(null)}.bind(this),5000);
            }
          });
      } else {
        request
          .post(geturl)
          .set('Accept', 'application/json')
          .set('api_key', tkey)
          .send(node)
          .end(function(err,res){
            if (err || !res.ok) {
              console.log('Server error in response to updating node: %s, %s', node.nodeid, err);
              console.log('Reason: ', res.body);
              //console.log('URL: ', url);
              createOneNode(n,i,cb);
            } else {
              console.log('Node created: ', res.body);
              setTimeout(function() {cb(null)}.bind(this),5000);
            }
          });
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
  async.eachOfSeries(videoModels, getOneModelConfig, function() { console.log ("Done getting default configs: ", JSON.stringify(defaultConfigs,null,4)); cb(null);});
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

  // Skip non video nodes
  if(videoModels.indexOf(model)===-1) {
    cb(null);
    return;
  }

  var cfg = JSON.parse(JSON.stringify(defaultConfigs[model]));

  cfg.model = model;
  if(cfg.debugmode)
    cfg.debugmode = conf['debugmode'] ? conf['debugmode'] : cfg.debugmode;
  cfg.name = 'Cfg_V'+i;
  if(cfg['network.x.security.psk'])
    cfg['network.x.security.psk'] = encoder.htmlDecode(conf['network.x.security.psk'] ? conf['network.x.security.psk'] : cfg['network.x.security.psk']);
  if(cfg['network.x.ssid'])
    cfg['network.x.ssid'] = conf['network.x.ssid'] ? conf['network.x.ssid'] : cfg['network.x.ssid'];
  if(cfg['network.x.security'])
    cfg['network.x.security'] = 'wpa2p';
  if(cfg['network.y.security.psk'])
    cfg['network.y.security.psk'] = encoder.htmlDecode(conf['network.y.security.psk'] ? conf['network.y.security.psk'] : cfg['network.y.security.psk']);
  if(cfg['network.y.ssid'])
    cfg['network.y.ssid'] = conf['network.y.ssid'] ? conf['network.y.ssid'] : cfg['network.y.ssid'];
  if(cfg['network.y.security'])
    cfg['network.y.security'] = 'wpa2p';
  if(cfg['network.region'])
    cfg['network.region'] = conf['network.region'] ? conf['network.region'] : cfg['network.region'];
  if(cfg['ota.disable'])
    cfg['ota.disable'] = conf['ota.disable'] ? conf['ota.disable'] : cfg['ota.disable'];
  if(cfg['network.telnet'])
    cfg['network.telnet'] = conf['network.telnet'] ? conf['network.telnet'] : cfg['network.telnet'];
  if(cfg['network.vpn_on_demand'])
    cfg['network.vpn_on_demand'] = conf['network.vpn_on_demand'] ? conf['network.vpn_on_demand'] : cfg['network.vpn_on_demand'];

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

function importNodesTo3x(cb) {
  console.log("Starting to import nodes");
  async.eachOfSeries(eData.nodes, createOneNode, function() { console.log ("Done importing nodes"); cb(null);});
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

function exportNodesConfigFrom2x(cb) {
  console.log("Starting to export Nodes, Config");
  var host = iParams.server;
  var siteid = iParams.siteid;
  var key = iParams.key;
  var org = iParams.torg;
  var site = iParams.tsite;
  var url = 'https://'+host+'.sensity.com/2.0/'+siteid+'//devices.json?aspects=hardware,business,location,name,node&api_key='+key;
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
        if(nodes[j].business && nodes[j].business.site && nodes[j].business.site.scope===siteid && videoModels.indexOf(nodes[j].hardware.model)!==-1)
          eData.nodes.push(nodes[j]);
      }
      createConfigs();
      console.log("Done with exporting Nodes, Config");
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
            exportNodesConfigFrom2x,
            exportGroupsFrom2x
        ], function(err, result) {
            console.log("\n\n\n\nEXPORT DONE\n");
            console.log("EXPORTED DATA: \n",JSON.stringify(eData, null, 4));
            //process.exit();
            async.series([
                getDefaultConfigs,
                importNodesTo3x,
                importConfigTo3x,
                importGroupsTo3x
            ], function(err, result) {
                console.log("\n\n\n\nIMPORT DONE\n");
            });
        });
  })
  .parse(process.argv);
