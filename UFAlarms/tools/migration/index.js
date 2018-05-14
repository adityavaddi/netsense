#!/usr/bin/env node
'use strict';
const prompt = require('prompt');
const sys = require('util')
const spawn = require('child_process').spawnSync;

prompt.message = "";
prompt.delimiter = ":";

const schema = {
  properties: {
    version: {
      description: 'Version of the server importing from [1 or 2]'
    },
    username: {
      description: 'Username for the account on 1x server',
      ask: function() {
        return prompt.history('version').value === '1';
      }
    },
    password: {
      description: 'Password for the account on 1x server',
      hidden: true,
      ask: function() {
        return prompt.history('version').value === '1';
      }
    },
    key: {
      description: 'API key for the account on 2x server',
      hidden: true,
      ask: function() {
        return prompt.history('version').value === '2';
      }
    },
    server: {
      description: 'Hostname of the server importing from',
    },
    siteID: {
      description: 'ID of the site importing from'
    },
    key_3x: {
      description: 'API key used to import data to 3x platform',
      hidden: true
    },
    server_3x: {
      description: 'Hostname of the IS instance of the 3x platform'
    },
    dcc_3x: {
      description: 'Hostname of the DCC instance of the 3x platform'
    },
    orgID: {
      description: 'ID of the 3x organization (customer) importing to'
    },
    siteID_3x: {
      description: 'ID of the 3x site importing to'
    }
  }
};

//
// Start the prompt
//
prompt.start();
console.log('Prompted data migration from 1x/2x system to 3x platform');

prompt.get(schema, function (err, result) {
  //
  // Log the results.
  //
  var cmd = '';
  var param = [];
  var execute = null;
  if(result.version==="1") {
    cmd = 'exp1x';
    param = [result.server,result.siteID,result.orgID,result.siteID_3x,result.username,result.password,result.key_3x,result.server_3x,result.dcc_3x];
    execute = spawn(cmd, param, { stdio: 'inherit' });
  } else if(result.version==="2"){
    cmd = 'exp';
    param = [result.server,result.siteID,result.orgID,result.siteID_3x,result.key,result.key_3x,result.server_3x,result.dcc_3x];
    execute = spawn(cmd, param, { stdio: 'inherit' });
  }
});
