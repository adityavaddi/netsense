import classNames from 'classnames';
import helpers from 'global/utils/helpers';
import Configlist from 'components/configs/configlist';
import Configdetail from 'components/configs/configdetail';
import DataUtil from '../service/datautil';

import Header from 'common/headernew';
import ConfigUpdateModal from 'components/configupdatemodal';
import ConfigDeleteWithNodesModal from 'components/configdeletewithnodesmodal';



var Body = React.createClass({
  getInitialState: function () {
    return {
      configs: null,
      defaultmodel: null,
      duplicate: false,
      showselectedconfigs: false,
      deleteconfigwithnodesalert: false,
      selectedconfig: null,
      configID: "-1",
      showConfigDetail: false
    }
  },

  calcHeight: function (pct, extra) {
    var h = (window && window.document) ? (($(window).height() - 75) * (pct / 100) + extra) : 500;
    return h;
  },

  recreateGroups: function (assigngroups, groups) {
    var g = [];
    if (assigngroups.length > 0) {
      for (var i = 0; i < groups.length; i++) {
        var j = assigngroups.indexOf(groups[i].groupid);
        if (j >= 0) {
          g.push({ name: groups[i].name, groupid: groups[i].groupid })
        }
      }
    }
    return g;
  },

  // modelName(model){
  //   var modelNames = {"unode-v2":"Core Node","unode-v3":"Internal Core Node","unode-v4":"Core Node EX Wifi","unode-v5":"Core Node EX Cellular","unode-v6":"Core Node EX LTE"};
  //   return (typeof modelNames[model] == "undefined")?"???":modelNames[model];
  // },

  init: function () {
    var that = this;

    if (NSN.customerID == "-1" && NSN.siteID == "-1") {
      $("#loadingmsg").html("Please select an Account and a Site first.")
      return;
    } else {
      if (NSN.customerID == "-1") {
        $("#loadingmsg").html("Please select an Account first.")
        return;
      } else {
        if (NSN.siteID == "-1") {
          $("#loadingmsg").html("Please select a Site first.")
          return;
        }
      }
    };

    that.getAllConfigs();

    // Groups:
    DataUtil.getAll('groups', that.processGroupsData)
    // $.ajax({
    //   url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/groups',
    //   data : '',
    //   method : 'GET',
    //   "xhrFields": {
    //      withCredentials: true
    //   },
    //   dataType : 'json',
    //   success : function(data){
    //       console.log("ajax success: " + JSON.stringify(data));
    //       that.setState({
    //         groups: data.map(function(group, index) {
    //           group.idx = index;
    //           return group;
    //         })
    //       })
    //   },
    //   error : function(jqXHR, status, error){
    //     $("#loadingmsg").html("Cannot retrieve Groups.  API reported error: " + error)
    //   }
    // });


  },
  /////////////Callback function to get Groups///////////////
  processGroupsData: function (data) {

    this.setState(DataUtil.assignState('groups', data, this, this.makeGroupsobj));

  },
  makeGroupsobj: function (group, index) {
    group.idx = index;
    return group;
  },

  getAllConfigs() {
    var that = this;

    DataUtil.getAll('configs', that.processAllConfigs);
    // $.ajax({
    //       url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/configs',
    //       data : '',
    //       xhrFields: {
    //          withCredentials: true
    //       },
    //       method : 'GET',
    //       dataType : 'json',
    //       success : function(data){
    //         if (data.errors) {
    //           console.log("/configs API returned error: " + JSON.stringify(data));
    //           $("#loadingmsg").html("Cannot retrieve config list. " + "/configs API returned error: " + JSON.stringify(data));
    //         } else {
    //           console.log("ajax success: " + JSON.stringify(data));
    //           NSN.configs = data;
    //           that.setState({
    //             configs: data.map(function(config, index) {

    //               if (typeof config.sites == "undefined") {config.sites = [];};
    //               if (typeof config.groups == "undefined") {config.groups = [];};
    //               if (typeof config.nodes == "undefined") {config.nodes = [];};
    //               if (config.sites.length == 1 && $.isEmptyObject(config.sites[0])) {
    //                 config.sites = [];
    //               };
    //               if (config.groups.length == 1 && $.isEmptyObject(config.groups[0])) {
    //                 config.groups = [];
    //               };
    //               if (config.nodes.length == 1 && $.isEmptyObject(config.nodes[0])) {
    //                 config.nodes = [];
    //               };
    //               config.assign = "unassigned";
    //               if (config.sites.length > 0) {
    //                 config.assign = "sitewide";
    //               } else {
    //                 if (config.groups.length > 0) {
    //                   config.assign = "groups";
    //                 }
    //               }

    //               config.idx = index;
    //               config.model = that.modelName(config.model)

    //               return config;
    //             })
    //           })
    //         };
    //       },
    //       error : function(jqXHR, status, error){
    //         console.log("ajax failure (configs): " + status + " - " + error);
    //         $("#loadingmsg").html("Cannot retrieve Configs.  API reported error: " + error);
    // //            noty({type:"error", text:"Could not retrieve configs."});
    //       }
    //     });

  },

  ////////// Callback function to get all configs//////////
  processAllConfigs: function (data) {
    if (data.errors) {
      console.log("/configs API returned error: " + JSON.stringify(data));
      $("#loadingmsg").html("Cannot retrieve config list. " + "/configs API returned error: " + JSON.stringify(data));
    } else {
      console.log("ajax success: " + JSON.stringify(data));
      NSN.configs = data;
      this.setState(DataUtil.assignState('configs', data, this, this.makeConfigsObj));
      // that.setState({
      //   configs: data.map()
      // })
    };
  },
  makeConfigsObj: function (config, index) {

    if (typeof config.sites == "undefined") { config.sites = []; };
    if (typeof config.groups == "undefined") { config.groups = []; };
    if (typeof config.nodes == "undefined") { config.nodes = []; };
    if (config.sites.length == 1 && $.isEmptyObject(config.sites[0])) {
      config.sites = [];
    };
    if (config.groups.length == 1 && $.isEmptyObject(config.groups[0])) {
      config.groups = [];
    };
    if (config.nodes.length == 1 && $.isEmptyObject(config.nodes[0])) {
      config.nodes = [];
    };
    config.assign = "unassigned";
    if (config.sites.length > 0) {
      config.assign = "sitewide";
    } else {
      if (config.groups.length > 0) {
        config.assign = "groups";
      }
    }

    config.idx = index;
    config.model = helpers.modelName(config.model);

    return config;
  },

  configAssignment(assignment, configid) {
    var payload = [];
    if (assignment.assign == "unassigned"
      || (assignment.assign == "groups" && assignment.assigngroups.length == 0)) {
      noty({ type: "information", text: "As requested, no assignment was made." });
      return;
    }
    if (assignment.assign == "sitewide") {
      var url = NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID
        + '/configs/' + configid + '/apply/site';
    }
    else {
      if (assignment.assign == "groups") {
        url = NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID
          + '/configs/' + configid + '/apply/groups/' + assignment.assigngroups[0];
      }
    }
    $.ajax({
      url: url,
      data: '',
      method: 'POST',
      "xhrFields": {
        withCredentials: true
      },
      dataType: "json",
      contentType: "application/json",
      processData: false,

      success: function (data) {
        console.log("ajax success: " + JSON.stringify(data));
        noty({
          type: "success", text: 'Config assigned '
          + ((assignment.assign == "sitewide") ? "sitewide" : "to group(s)")
          + ' as requested.'
        });
      },
      error: function (jqXHR, status, error) {
        if (jqXHR.status == 200) {
          noty({
            type: "success", text: 'Config assigned '
            + ((assignment.assign == "sitewide") ? "sitewide" : "to group(s)")
            + ' as requested.'
          });
        } else {
          noty({
            type: "error", text: 'Unable to assign config '
            + 'as requested.  Return status: ' + status
          });
        }
      }
    });
  },
  //////////////Callback function to add Configs/////////
  processAddConfigs: function (data) {
    NSN.configID = data.configid;
    sessionStorage.setItem("configID", NSN.configID);
    data.idx = this.state.configs.length;
    var newData = {
      "name": data.name,
      "server": data.server,
      "configid": data.configid
    };

    var newState = React.addons.update(this.state, { configs: { $push: [newData] }, currentconfig: { $set: [data] }, configID: { $set: data.configid } });
    that.getAllConfigs();
    noty({ type: "success", text: 'Config "' + data.name + '" added.' });
    ReactBootstrap.Dispatcher.emit("Configform.add.success", data);
    this.configAssignment(assignment, data.configid);
    this.setState(newState);
  },
  /////////////Callback function to delete configs/////////////
  processDeleteConfigs: function (entityData, idx, data) {

    var newState = React.addons.update(this.state, { configs: { $splice: [[idx, 1]] }, configID: { $set: "-1" } });
    NSN.configID = "-1";
    sessionStorage.setItem("configID", NSN.configID);
    noty({ type: "success", text: 'Config "' + entityData.name + '" deleted.' });
    ReactBootstrap.Dispatcher.emit("Configform.delete.success", entityData.configid);
    this.setState(newState);
  },

  checkDuplicate: function(config_info){
          var duplicate = false;
        for (var i = 0, len = this.state.configs.length; i < len; i++) {
          if ((this.state.configs[i].name === config_info.name)&& !(this.state.configs[i].configid === config_info.configid))
            duplicate = true;
        }
        if (duplicate) {
          noty({ type: "error", text: 'Config "' + config_info.name + '" already exists.' });
        }
        return duplicate;
  },
  componentDidMount: function () {
    this.init();
    var that = this;

    ReactBootstrap.Dispatcher.on("Configlist.select", function (configID, sortedFlag) {
      // if(configID != NSN.configID){
      if(configID != NSN.configID  || (configID == NSN.configID && !sortedFlag)) {
        NSN.configID = configID;
        sessionStorage.setItem("configID", NSN.configID);
        that.setState({
          "configID": configID,
          "showConfigDetail": true
        });
      $('.configForm').css("display","block");
      }
    });

    ReactBootstrap.Dispatcher.on("Configlist.add", function () {
      that.setState({
        "configID": "0",
        "defaultmodel": null,
        showConfigDetail: true
      });
      // $('.configForm').css("display","block");

    });

    ReactBootstrap.Dispatcher.on("Configform.duplicateconfig", function (config_info, configID) {

      console.log(JSON.stringify(config_info));
      var configObject = {};
      var configID = that.state.configID;

      delete config_info.idx;
      delete config_info.configid;
      config_info.name = config_info.name + "-dup";

      config_info.groups = [];
      config_info.assign = "unassigned";
      config_info.sites = [];
      config_info.nodes = [];
      config_info.assigngroups = [];

       if(config_info.model == "vdkmaster"){
            configObject = {  "name": config_info.name,
                              "model": config_info.model,
                              "siteid": config_info.siteid,
                              "al.left.setbrightness":parseInt(config_info["al.left.setbrightness"]),
                              "al.left.setmode":config_info["al.left.setmode"],
                              "al.right.setbrightness":parseInt(config_info["al.right.setbrightness"]),
                              "al.right.setmode":config_info["al.right.setmode"],
                              "alarm.redis.memory_threshold":parseFloat(config_info["alarm.redis.memory_threshold"]),
                              "application_server":config_info.application_server,
                              "application_server_port":parseInt(config_info.application_server_port),
                              "appos.timezone":config_info["appos.timezone"],
                              "glassl.display.blue.gain":parseInt(config_info["glassl.display.blue.gain"]),
                              "glassl.display.blue.offset":parseInt(config_info["glassl.display.blue.offset"]),
                              "glassl.display.brightness":parseInt(config_info["glassl.display.brightness"]),
                              "glassl.display.color-temp":parseInt(config_info["glassl.display.color-temp"]),
                              "glassl.display.contrast":parseInt(config_info["glassl.display.contrast"]),
                              "glassl.display.green.gain":parseInt(config_info["glassl.display.green.gain"]),
                              "glassl.display.green.offset":parseInt(config_info["glassl.display.green.offset"]),
                              "glassl.display.hue":parseInt(config_info["glassl.display.hue"]),
                              "glassl.display.mode.ccs":config_info["glassl.display.mode.ccs"],
                              "glassl.display.mode.dynamic-nr":config_info["glassl.display.mode.dynamic-nr"],
                              "glassl.display.mode.main-dcdi":config_info["glassl.display.mode.main-dcdi"],
                              "glassl.display.mode.main-madi":config_info["glassl.display.mode.main-madi"],
                              "glassl.display.mode.mpeg-nr":config_info["glassl.display.mode.mpeg-nr"],
                              "glassl.display.mode.selection-1080p":config_info["glassl.display.mode.selection-1080p"],
                              "glassl.display.mode.selection-720p":config_info["glassl.display.mode.selection-720p"],
                              "glassl.display.mpeg-nr":parseInt(config_info["glassl.display.mpeg-nr"]),
                              "glassl.display.power":config_info["glassl.display.power"],
                              "glassl.display.red.gain":parseInt(config_info["glassl.display.red.gain"]),
                              "glassl.display.red.offset":parseInt(config_info["glassl.display.red.offset"]),
                              "glassl.display.saturation":parseInt(config_info["glassl.display.saturation"]),
                              "glassl.display.sharpness":parseInt(config_info["glassl.display.sharpness"]),
                              "glassl.display.temp.cold":parseInt(config_info["glassl.display.temp.cold"]),
                              "glassl.display.temp.hot":parseInt(config_info["glassl.display.temp.hot"]),
                              "glassl.display.temp.warm":parseInt(config_info["glassl.display.temp.warm"]),
                              "glassr.display.blue.gain":parseInt(config_info["glassr.display.blue.gain"]),
                              "glassr.display.blue.offset":parseInt(config_info["glassr.display.blue.offset"]),
                              "glassr.display.brightness":parseInt(config_info["glassr.display.brightness"]),
                              "glassr.display.color-temp":parseInt(config_info["glassr.display.color-temp"]),
                              "glassr.display.contrast":parseInt(config_info["glassr.display.contrast"]),
                              "glassr.display.green.gain":parseInt(config_info["glassr.display.green.gain"]),
                              "glassr.display.green.offset":parseInt(config_info["glassr.display.green.offset"]),
                              "glassr.display.hue":parseInt(config_info["glassr.display.hue"]),
                              "glassr.display.mode.ccs":config_info["glassr.display.mode.ccs"],
                              "glassr.display.mode.dynamic-nr":config_info["glassr.display.mode.dynamic-nr"],
                              "glassr.display.mode.main-dcdi":config_info["glassr.display.mode.main-dcdi"],
                              "glassr.display.mode.main-madi":config_info["glassr.display.mode.main-madi"],
                              "glassr.display.mode.mpeg-nr":config_info["glassr.display.mode.mpeg-nr"],
                              "glassr.display.mode.selection-1080p":config_info["glassr.display.mode.selection-1080p"],
                              "glassr.display.mode.selection-720p":config_info["glassr.display.mode.selection-720p"],
                              "glassr.display.mpeg-nr":parseInt(config_info["glassr.display.mpeg-nr"]),
                              "glassr.display.power":config_info["glassr.display.power"],
                              "glassr.display.red.gain":parseInt(config_info["glassr.display.red.gain"]),
                              "glassr.display.red.offset":parseInt(config_info["glassr.display.red.offset"]),
                              "glassr.display.saturation":parseInt(config_info["glassr.display.saturation"]),
                              "glassr.display.sharpness":parseInt(config_info["glassr.display.sharpness"]),
                              "glassr.display.temp.cold":parseInt(config_info["glassr.display.temp.cold"]),
                              "glassr.display.temp.hot":parseInt(config_info["glassr.display.temp.hot"]),
                              "glassr.display.temp.warm":parseInt(config_info["glassr.display.temp.warm"]),
                              "lr.setcolor":config_info["lr.setcolor"],
                              "network.eth-x.ip":config_info["network.eth-x.ip"],
                              "network.eth-x.method":config_info["network.eth-x.method"],
                              "network.eth-x.netmask":parseInt(config_info["network.eth-x.netmask"]),
                              "network.eth-x.port-forwarding.genetec":config_info["network.eth-x.port-forwarding.genetec"],
                              "network.eth-x.port-forwarding.https":config_info["network.eth-x.port-forwarding.https"],
                              "network.eth-x.port-forwarding.rtsp":config_info["network.eth-x.port-forwarding.rtsp"],
                              "network.eth-x.port-forwarding.ssh":config_info["network.eth-x.port-forwarding.ssh"],
                              "network.eth-x.whitelist":config_info["network.eth-x.whitelist"],
                              "network.firewall.ports":config_info["network.firewall.ports"],
                              "network.firewall.protocols":config_info["network.firewall.protocols"],
                              "network.server.mqtt":config_info["network.server.mqtt"],
                              "network.server.vpn":config_info["network.server.vpn"],
                              "network.vpn_on_demand":config_info["network.vpn_on_demand"],
                              "pebble.display.blue.gain":parseInt(config_info["pebble.display.blue.gain"]),
                              "pebble.display.blue.offset":parseInt(config_info["pebble.display.blue.offset"]),
                              "pebble.display.brightness":parseInt(config_info["pebble.display.brightness"]),
                              "pebble.display.color-temp":parseInt(config_info["pebble.display.color-temp"]),
                              "pebble.display.contrast":parseInt(config_info["pebble.display.contrast"]),
                              "pebble.display.green.gain":parseInt(config_info["pebble.display.green.gain"]),
                              "pebble.display.green.offset":parseInt(config_info["pebble.display.green.offset"]),
                              "pebble.display.hue":parseInt(config_info["pebble.display.hue"]),
                              "pebble.display.mode.ccs":config_info["pebble.display.mode.ccs"],
                              "pebble.display.mode.dynamic-nr":config_info["pebble.display.mode.dynamic-nr"],
                              "pebble.display.mode.main-dcdi":config_info["pebble.display.mode.main-dcdi"],
                              "pebble.display.mode.main-madi":config_info["pebble.display.mode.main-madi"],
                              "pebble.display.mode.mpeg-nr":config_info["pebble.display.mode.mpeg-nr"],
                              "pebble.display.mode.selection-1080p":config_info["pebble.display.mode.selection-1080p"],
                              "pebble.display.mode.selection-720p":config_info["pebble.display.mode.selection-720p"],
                              "pebble.display.mpeg-nr":parseInt(config_info["pebble.display.mpeg-nr"]),
                              "pebble.display.power":config_info["pebble.display.power"],
                              "pebble.display.red.gain":config_info["pebble.display.red.gain"],
                              "pebble.display.red.offset":parseInt(config_info["pebble.display.red.offset"]),
                              "pebble.display.saturation":parseInt(config_info["pebble.display.saturation"]),
                              "pebble.display.sharpness":parseInt(config_info["pebble.display.sharpness"]),
                              "pebble.display.temp.cold":parseInt(config_info["pebble.display.temp.cold"]),
                              "pebble.display.temp.hot":parseInt(config_info["pebble.display.temp.hot"]),
                              "pebble.display.temp.warm":parseInt(config_info["pebble.display.temp.warm"]),
                              "server":config_info.server,
                            };
            

          }
          else if (config_info.model == "merlin"){
             configObject = {  "name": config_info.name,
                              "model": config_info.model,
                              "Genetec.custom_events":config_info["Genetec.custom_events"],
                              "alarm.redis.memory_threshold":parseFloat(config_info["alarm.redis.memory_threshold"]),
                              "application_server":config_info.application_server,
                              "application_server_port":parseInt(config_info.application_server_port),
                              "camera.0.binning":config_info["camera.0.binning"],
                              "camera.0.enabled":config_info["camera.0.enabled"],
                              "camera.0.exposure":parseInt(config_info["camera.0.exposure"]),
                              "camera.0.gain":parseInt(config_info["camera.0.gain"]),
                              "camera.0.master":parseInt(config_info["camera.0.master"]),
                              "camera.0.streamH.bitrate":parseInt(config_info["camera.0.streamH.bitrate"]),
                              "camera.0.streamH.format":config_info["camera.0.streamH.format"],
                              "camera.0.streamH.framerate":parseInt(config_info["camera.0.streamH.framerate"]),
                              "camera.0.streamH.gopsize":parseInt(config_info["camera.0.streamH.gopsize"]),
                              "camera.0.streamH.storage.diskquota":parseFloat(config_info["camera.0.streamH.storage.diskquota"]),
                              "camera.0.streamH.storage.recordevents":config_info["camera.0.streamH.storage.recordevents"],
                              "camera.0.streamH.storage.recordmode":parseInt(config_info["camera.0.streamH.storage.recordmode"]),
                              "camera.0.streamH.storage.recordpostamble":parseInt(config_info["camera.0.streamH.storage.recordpostamble"]),
                              "camera.0.streamH.storage.recordpreamble":parseInt(config_info["camera.0.streamH.storage.recordpreamble"]),
                              "camera.0.streamH.storage.vaqa.0.duration":config_info["camera.0.streamH.storage.vaqa.0.duration"],
                              "camera.0.streamH.storage.vaqa.0.interval":config_info["camera.0.streamH.storage.vaqa.0.interval"],
                              "camera.0.streamH.storage.vaqa.0.qty":parseInt(config_info["camera.0.streamH.storage.vaqa.0.qty"]),
                              "camera.0.streamH.storage.vaqa.1.duration":config_info["camera.0.streamH.storage.vaqa.1.duration"],
                              "camera.0.streamH.storage.vaqa.1.interval":config_info["camera.0.streamH.storage.vaqa.1.interval"],
                              "camera.0.streamH.storage.vaqa.1.qty":parseInt(config_info["camera.0.streamH.storage.vaqa.1.qty"]),
                              "camera.0.streamL.bitrate":parseInt(config_info["camera.0.streamL.bitrate"]),
                              "camera.0.streamL.format":config_info["camera.0.streamL.format"],
                              "camera.0.streamL.framerate":parseInt(config_info["camera.0.streamL.framerate"]),
                              "camera.0.streamL.gopsize":parseInt(config_info["camera.0.streamL.gopsize"]),
                              "camera.0.streamL.storage.diskquota":parseFloat(config_info["camera.0.streamL.storage.diskquota"]),
                              "camera.0.streamL.storage.recordevents":config_info["camera.0.streamL.storage.recordevents"],
                              "camera.0.streamL.storage.recordmode":parseInt(config_info["camera.0.streamL.storage.recordmode"]),
                              "camera.0.streamL.storage.recordpostamble":parseInt(config_info["camera.0.streamL.storage.recordpostamble"]),
                              "camera.0.streamL.storage.recordpreamble":parseInt(config_info["camera.0.streamL.storage.recordpreamble"]),
                              "camera.1.enabled":config_info["camera.1.enabled"],
                              "camera.1.streamH.bitrate":parseInt(config_info["camera.1.streamH.bitrate"]),
                              "camera.1.streamH.format":config_info["camera.1.streamH.format"],
                              "camera.1.streamH.framerate":parseInt(config_info["camera.1.streamH.framerate"]),
                              "camera.1.streamH.gopsize":parseInt(config_info["camera.1.streamH.gopsize"]),
                              "camera.1.streamH.storage.recordevents":config_info["camera.1.streamH.storage.recordevents"],
                              "camera.1.streamH.storage.recordpostamble":parseInt(config_info["camera.1.streamH.storage.recordpostamble"]),
                              "camera.1.streamH.storage.recordpreamble":parseInt(config_info["camera.1.streamH.storage.recordpreamble"]),
                              "camera.1.streamH.storage.vaqa.0.duration":config_info["camera.1.streamH.storage.vaqa.0.duration"],
                              "camera.1.streamH.storage.vaqa.0.interval":config_info["camera.1.streamH.storage.vaqa.0.interval"],
                              "camera.1.streamH.storage.vaqa.0.qty":parseInt(config_info["camera.1.streamH.storage.vaqa.0.qty"]),
                              "camera.1.streamH.storage.vaqa.1.duration":config_info["camera.1.streamH.storage.vaqa.1.duration"],
                              "camera.1.streamH.storage.vaqa.1.interval":config_info["camera.1.streamH.storage.vaqa.1.interval"],
                              "camera.1.streamH.storage.vaqa.1.qty":parseInt(config_info["camera.1.streamH.storage.vaqa.1.qty"]),
                              "camera.1.streamL.bitrate":parseInt(config_info["camera.1.streamL.bitrate"]),
                              "camera.1.streamL.format":config_info["camera.1.streamL.format"],
                              "camera.1.streamL.framerate":parseInt(config_info["camera.1.streamL.framerate"]),
                              "camera.1.streamL.gopsize":parseInt(config_info["camera.1.streamL.gopsize"]),
                              "camera.1.streamL.storage.recordevents":config_info["camera.1.streamL.storage.recordevents"],
                              "camera.1.streamL.storage.recordpostamble":parseInt(config_info["camera.1.streamL.storage.recordpostamble"]),
                              "camera.1.streamL.storage.recordpreamble":parseInt(config_info["camera.1.streamL.storage.recordpreamble"]),
                              "evtgrab.capint":parseInt(config_info["evtgrab.capint"]),
                              "evtgrab.dint":parseInt(config_info["evtgrab.dint"]),
                              "evtgrab.nfiles":parseInt(config_info["evtgrab.nfiles"]),
                              "evtgrab.periodic_only":config_info["evtgrab.periodic_only"],
                              "mediaserver.nomve":config_info["mediaserver.nomve"],
                              "mediaserver.rtspsrc":config_info["mediaserver.rtspsrc"],
                              "mediaserver.video_codec":config_info["mediaserver.video_codec"],
                              "mediaserver.wmm_qos":config_info["mediaserver.wmm_qos"],
                              "network.eth-x.ip":config_info["network.eth-x.ip"],
                              "network.eth-x.method":config_info["network.eth-x.method"],
                              "network.eth-x.netmask":parseInt(config_info["network.eth-x.netmask"]),
                              "network.firewall.ports":config_info["network.firewall.ports"],
                              "network.firewall.protocols":config_info["network.firewall.protocols"],
                              "network.server.mqtt":config_info["network.server.mqtt"],
                              "network.server.vpn":config_info["network.server.vpn"],
                              "network.vpn_on_demand":config_info["network.vpn_on_demand"],
                              "rtsp.service":parseInt(config_info["rtsp.service"]),
                              "server":config_info.server,
                              "storage.maxsessions":parseInt(config_info["storage.maxsessions"]),
                              "storageserver.cache_size":parseInt(config_info["storageserver.cache_size"]),
                              "storageserver.max_clip_size":parseInt(config_info["storageserver.max_clip_size"]),
                            };

          }
          else if (config_info.model == "falcon-q"){
            configObject = {  "name": config_info.name,
                              "model": config_info.model,
                              "Genetec.custom_events":config_info["Genetec.custom_events"],
                              "alarm.redis.memory_threshold":parseFloat(config_info["alarm.redis.memory_threshold"]),
                              "application_server":config_info.application_server,
                              "application_server_port":parseInt(config_info.application_server_port),
                              "camera.0.autoexposure":config_info["camera.0.autoexposure"],
                              "camera.0.autogain":config_info["camera.0.autogain"],
                              "camera.0.colortemp":parseInt(config_info["camera.0.colortemp"]),
                              "camera.0.enabled":config_info["camera.0.enabled"],
                              "camera.0.exposure":parseInt(config_info["camera.0.exposure"]),
                              "camera.0.flickermode":config_info["camera.0.flickermode"],
                              "camera.0.gain":parseInt(config_info["camera.0.gain"]),
                              "camera.0.indoormode":config_info["camera.0.indoormode"],
                              "camera.0.master":parseInt(config_info["camera.0.master"]),
                              "camera.0.scanmode":config_info["camera.0.scanmode"],
                              "camera.0.streamH.bitrate":parseInt(config_info["camera.0.streamH.bitrate"]),
                              "camera.0.streamH.format":config_info["camera.0.streamH.format"],
                              "camera.0.streamH.framerate":parseInt(config_info["camera.0.streamH.framerate"]),
                              "camera.0.streamH.gopsize":parseInt(config_info["camera.0.streamH.gopsize"]),
                              "camera.0.streamH.storage.diskquota":parseFloat(config_info["camera.0.streamH.storage.diskquota"]),
                              "camera.0.streamH.storage.recordevents":config_info["camera.0.streamH.storage.recordevents"],
                              "camera.0.streamH.storage.recordmode":parseInt(config_info["camera.0.streamH.storage.recordmode"]),
                              "camera.0.streamH.storage.recordpostamble":parseInt(config_info["camera.0.streamH.storage.recordpostamble"]),
                              "camera.0.streamH.storage.recordpreamble":parseInt(config_info["camera.0.streamH.storage.recordpreamble"]),
                              "camera.0.streamH.storage.vaqa.0.duration":config_info["camera.0.streamH.storage.vaqa.0.duration"],
                              "camera.0.streamH.storage.vaqa.0.interval":config_info["camera.0.streamH.storage.vaqa.0.interval"],
                              "camera.0.streamH.storage.vaqa.0.qty":parseInt(config_info["camera.0.streamH.storage.vaqa.0.qty"]),
                              "camera.0.streamH.storage.vaqa.1.duration":config_info["camera.0.streamH.storage.vaqa.1.duration"],
                              "camera.0.streamH.storage.vaqa.1.interval":config_info["camera.0.streamH.storage.vaqa.1.interval"],
                              "camera.0.streamH.storage.vaqa.1.qty":parseInt(config_info["camera.0.streamH.storage.vaqa.1.qty"]),
                              "camera.0.streamL.bitrate":parseInt(config_info["camera.0.streamL.bitrate"]),
                              "camera.0.streamL.format":config_info["camera.0.streamL.format"],
                              "camera.0.streamL.framerate":parseInt(config_info["camera.0.streamL.framerate"]),
                              "camera.0.streamL.gopsize":parseInt(config_info["camera.0.streamL.gopsize"]),
                              "camera.0.streamL.storage.diskquota":parseFloat(config_info["camera.0.streamL.storage.diskquota"]),
                              "camera.0.streamL.storage.recordevents":config_info["camera.0.streamL.storage.recordevents"],
                              "camera.0.streamL.storage.recordmode":parseInt(config_info["camera.0.streamL.storage.recordmode"]),
                              "camera.0.streamL.storage.recordpostamble":parseInt(config_info["camera.0.streamL.storage.recordpostamble"]),
                              "camera.0.streamL.storage.recordpreamble":parseInt(config_info["camera.0.streamL.storage.recordpreamble"]),
                              "camera.0.wdrmode":config_info["camera.0.wdrmode"],
                              "camera.1.autoexposure":config_info["camera.1.autoexposure"],
                              "camera.1.autogain":config_info["camera.1.autogain"],
                              "camera.1.colortemp":parseInt(config_info["camera.1.colortemp"]),
                              "camera.1.enabled":config_info["camera.1.enabled"],
                              "camera.1.exposure":parseInt(config_info["camera.1.exposure"]),
                              "camera.1.flickermode":config_info["camera.1.flickermode"],
                              "camera.1.gain":parseInt(config_info["camera.1.gain"]),
                              "camera.1.indoormode":config_info["camera.1.indoormode"],
                              "camera.1.scanmode":config_info["camera.1.scanmode"],
                              "camera.1.streamH.bitrate":parseInt(config_info["camera.1.streamH.bitrate"]),
                              "camera.1.streamH.format":config_info["camera.1.streamH.format"],
                              "camera.1.streamH.framerate":parseInt(config_info["camera.1.streamH.framerate"]),
                              "camera.1.streamH.gopsize":parseInt(config_info["camera.1.streamH.gopsize"]),
                              "camera.1.streamH.storage.diskquota":parseFloat(config_info["camera.1.streamH.storage.diskquota"]),
                              "camera.1.streamH.storage.recordevents":config_info["camera.1.streamH.storage.recordevents"],
                              "camera.1.streamH.storage.recordmode":parseInt(config_info["camera.1.streamH.storage.recordmode"]),
                              "camera.1.streamH.storage.recordpostamble":parseInt(config_info["camera.1.streamH.storage.recordpostamble"]),
                              "camera.1.streamH.storage.recordpreamble":parseInt(config_info["camera.1.streamH.storage.recordpreamble"]),
                              "camera.1.streamH.storage.vaqa.0.duration":config_info["camera.1.streamH.storage.vaqa.0.duration"],
                              "camera.1.streamH.storage.vaqa.0.interval":config_info["camera.1.streamH.storage.vaqa.0.interval"],
                              "camera.1.streamH.storage.vaqa.0.qty":parseInt(config_info["camera.1.streamH.storage.vaqa.0.qty"]),
                              "camera.1.streamH.storage.vaqa.1.duration":config_info["camera.1.streamH.storage.vaqa.1.duration"],
                              "camera.1.streamH.storage.vaqa.1.interval":config_info["camera.1.streamH.storage.vaqa.1.interval"],
                              "camera.1.streamH.storage.vaqa.1.qty":parseInt(config_info["camera.1.streamH.storage.vaqa.1.qty"]),
                              "camera.1.streamL.bitrate":parseInt(config_info["camera.1.streamL.bitrate"]),
                              "camera.1.streamL.format":config_info["camera.1.streamL.format"],
                              "camera.1.streamL.framerate":parseInt(config_info["camera.1.streamL.framerate"]),
                              "camera.1.streamL.gopsize":parseInt(config_info["camera.1.streamL.gopsize"]),
                              "camera.1.streamL.storage.diskquota":parseFloat(config_info["camera.1.streamL.storage.diskquota"]),
                              "camera.1.streamL.storage.recordevents":config_info["camera.1.streamL.storage.recordevents"],
                              "camera.1.streamL.storage.recordmode":parseInt(config_info["camera.1.streamL.storage.recordmode"]),
                              "camera.1.streamL.storage.recordpostamble":parseInt(config_info["camera.1.streamL.storage.recordpostamble"]),
                              "camera.1.streamL.storage.recordpreamble":parseInt(config_info["camera.1.streamL.storage.recordpreamble"]),
                              "camera.1.wdrmode":config_info["camera.1.wdrmode"],
                              "evtgrab.capint":parseInt(config_info["evtgrab.capint"]),
                              "evtgrab.dint":parseInt(config_info["evtgrab.dint"]),
                              "evtgrab.nfiles":parseInt(config_info["evtgrab.nfiles"]),
                              "evtgrab.periodic_only":config_info["evtgrab.periodic_only"],
                              "mediaserver.nomve":config_info["mediaserver.nomve"],
                              "mediaserver.video_codec":config_info["mediaserver.video_codec"],
                              "mediaserver.wmm_qos":config_info["mediaserver.wmm_qos"],
                              "network.eth-x.ip":config_info["network.eth-x.ip"],
                              "network.eth-x.leasetime":config_info["network.eth-x.leasetime"],
                              "network.eth-x.method":config_info["network.eth-x.method"],
                              "network.eth-x.netmask":parseInt(config_info["network.eth-x.netmask"]),
                              "network.eth-x.port-forwarding.genetec":config_info["network.eth-x.port-forwarding.genetec"],
                              "network.eth-x.port-forwarding.https":config_info["network.eth-x.port-forwarding.https"],
                              "network.eth-x.port-forwarding.rtsp":config_info["network.eth-x.port-forwarding.rtsp"],
                              "network.eth-x.port-forwarding.ssh":config_info["network.eth-x.port-forwarding.ssh"],
                              "network.eth-x.whitelist":config_info["network.eth-x.whitelist"],
                              "network.firewall.ports":config_info["network.firewall.ports"],
                              "network.firewall.protocols":config_info["network.firewall.protocols"],
                              "network.region":config_info["network.region"],
                              "network.server.mqtt":config_info["network.server.mqtt"],
                              "network.server.vpn":config_info["network.server.vpn"],
                              "network.vpn_on_demand":config_info["network.vpn_on_demand"],
                              "network.wlan-x.security":config_info["network.wlan-x.security"],
                              "network.wlan-x.security.bgscan-long-interval":parseInt(config_info["network.wlan-x.security.bgscan-long-interval"]),
                              "network.wlan-x.security.bgscan-short-interval":parseInt(config_info["network.wlan-x.security.bgscan-short-interval"]),
                              "network.wlan-x.security.bgscan-signal-threshold":parseInt(config_info["network.wlan-x.security.bgscan-signal-threshold"]),
                              "network.wlan-x.security.psk":config_info["network.wlan-x.security.psk"],
                              "network.wlan-x.ssid":config_info["network.wlan-x.ssid"],
                              "network.wlan-y.security":config_info["network.wlan-y.security"],
                              "network.wlan-y.security.bgscan-long-interval":parseInt(config_info["network.wlan-y.security.bgscan-long-interval"]),
                              "network.wlan-y.security.bgscan-short-interval":parseInt(config_info["network.wlan-y.security.bgscan-short-interval"]),
                              "network.wlan-y.security.bgscan-signal-threshold":parseInt(config_info["network.wlan-y.security.bgscan-signal-threshold"]),
                              "network.wlan-y.security.psk":config_info["network.wlan-y.security.psk"],
                              "network.wlan-y.ssid":config_info["network.wlan-y.ssid"],
                              "rtsp.service":parseInt(config_info["rtsp.service"]),
                              "server":config_info.server,
                              "storage.maxsessions":parseInt(config_info["storage.maxsessions"]),
                              "storageserver.cache_size":parseInt(config_info["storageserver.cache_size"]),
                              "storageserver.max_clip_size":parseInt(config_info["storageserver.max_clip_size"]),
                            };

          }
          else if (config_info.model == "cnext"){
            configObject = {  "name": config_info.name,
                              "model": config_info.model,
                              "alarm.redis.memory_threshold":parseFloat(config_info["alarm.redis.memory_threshold"]),
                              "application_server":config_info.application_server,
                              "application_server_port":parseInt(config_info.application_server_port),
                              "lctrl.dimmer.vmax":parseInt(config_info["lctrl.dimmer.vmax"]),
                              "lctrl.ltype":parseInt(config_info["lctrl.ltype"]),
                              "lctrl.lvlrate.fast":parseInt(config_info["lctrl.lvlrate.fast"]),
                              "lctrl.lvlrate.light":parseInt(config_info["lctrl.lvlrate.light"]),
                              "lctrl.lvlrate.motion":parseInt(config_info["lctrl.lvlrate.motion"]),
                              "lctrl.lvlrate.norm":parseInt(config_info["lctrl.lvlrate.norm"]),
                              "lctrl.trigger.nonett":parseInt(config_info["lctrl.trigger.nonett"]),
                              "lctrl.trigger.off.dint":parseInt(config_info["lctrl.trigger.off.dint"]),
                              "lctrl.trigger.offt":parseInt(config_info["lctrl.trigger.offt"]),
                              "lctrl.trigger.on.dint":parseInt(config_info["lctrl.trigger.on.dint"]),
                              "lctrl.trigger.ont":parseInt(config_info["lctrl.trigger.ont"]),
                              "network.eth-x.ip":config_info["network.eth-x.ip"],
                              "network.eth-x.leasetime":config_info["network.eth-x.leasetime"],
                              "network.eth-x.method":config_info["network.eth-x.method"],
                              "network.eth-x.netmask":parseInt(config_info["network.eth-x.netmask"]),
                              "network.firewall.ports":config_info["network.firewall.ports"],
                              "network.firewall.protocols":config_info["network.firewall.protocols"],
                              "network.ppp-x.apn":config_info["network.ppp-x.apn"],
                              "network.server.mqtt":config_info["network.server.mqtt"],
                              "network.server.vpn":config_info["network.server.vpn"],
                              "network.vpn_on_demand":config_info["network.vpn_on_demand"],
                              "network.wlan-x.security":config_info["network.wlan-x.security"],
                              "network.wlan-x.security.bgscan-long-interval":parseInt(config_info["network.wlan-x.security.bgscan-long-interval"]),
                              "network.wlan-x.security.bgscan-short-interval":parseInt(config_info["network.wlan-x.security.bgscan-short-interval"]),
                              "network.wlan-x.security.bgscan-signal-threshold":parseInt(config_info["network.wlan-x.security.bgscan-signal-threshold"]),
                              "network.wlan-x.security.psk":config_info["network.wlan-x.security.psk"],
                              "network.wlan-x.ssid":config_info["network.wlan-x.ssid"],
                              "network.wlan-y.security":config_info["network.wlan-y.security"],
                              "network.wlan-y.security.bgscan-long-interval":parseInt(config_info["network.wlan-y.security.bgscan-long-interval"]),
                              "network.wlan-y.security.bgscan-short-interval":parseInt(config_info["network.wlan-y.security.bgscan-short-interval"]),
                              "network.wlan-y.security.bgscan-signal-threshold":parseInt(config_info["network.wlan-y.security.bgscan-signal-threshold"]),
                              "network.wlan-y.security.psk":config_info["network.wlan-y.security.psk"],
                              "network.wlan-y.ssid":config_info["network.wlan-y.ssid"],
                              "podbus.disable":config_info["podbus.disable"],
                              "poe.enabled":config_info["poe.enabled"],
                              "pwr.noload.i":parseInt(config_info["pwr.noload.i"]),
                              "pwr.noload.mode":parseInt(config_info["pwr.noload.mode"]),
                              "server":config_info.server,
                            };

          }
          else if (config_info.model == "unode-v2"){
            configObject = {  "name": config_info.name,
                              "model": config_info.model,
                              "networkXPasskey":config_info.networkXPasskey,
                              "networkXSSID":config_info.networkXSSID,
                              "networkXSecurity":config_info.networkXSecurity,
                              "networkYPasskey":config_info.networkYPasskey,
                              "networkYSSID":config_info.networkYSSID,
                              "networkYSecurity":config_info.networkYSecurity,
                              "network_region":config_info.network_region,
                              "debugmode":config_info.debugmode,
                              "telnet":config_info.telnet,
                              "sensor_rf_pint":config_info.sensor_rf_pint * 1000,
                              "sensor_rf_dint":config_info.sensor_rf_dint * 1000,
                              "sensor_rf_mode":config_info.sensor_rf_mode,
                              "sensor_v_pint":config_info.sensor_v_pint * 1000,
                              "sensor_v_dint":config_info.sensor_v_dint * 1000,
                              "sensor_v_mode":config_info.sensor_v_mode,
                              "sensor_lIR_pint":config_info.sensor_lIR_pint * 1000,
                              "sensor_lIR_dint":config_info.sensor_lIR_dint  * 1000,
                              "sensor_lIR_mode":config_info.sensor_lIR_mode,
                              "sensor_l_pint":config_info.sensor_l_pint * 1000,
                              "sensor_l_dint":config_info.sensor_l_dint * 1000,
                              "sensor_l_mode":config_info.sensor_l_mode,
                              "sensor_p_pint":config_info.sensor_p_pint * 1000,
                              "sensor_p_dint":config_info.sensor_p_dint * 1000,
                              "sensor_p_mode":config_info.sensor_p_mode,
                              "sensor_t_pint":config_info.sensor_t_pint * 1000,
                              "sensor_t_dint":config_info.sensor_t_dint * 1000,
                              "sensor_t_mode":config_info.sensor_t_mode,
                              "sensor_T_pint":config_info.sensor_T_pint * 1000,
                              "sensor_T_dint":config_info.sensor_T_dint * 1000,
                              "sensor_T_mode":config_info.sensor_T_mode,
                              "sensor_pdc_pint":config_info.sensor_pdc_pint * 1000,
                              "sensor_pdc_dint":config_info.sensor_pdc_dint * 1000,
                              "sensor_pdc_mode":config_info.sensor_pdc_mode,
                              "sensor_ppr_pint":config_info.sensor_ppr_pint * 1000,
                              "sensor_ppr_dint":config_info.sensor_ppr_dint * 1000,
                              "sensor_ppr_mode":config_info.sensor_ppr_mode,
                              "sensor_pnd_pint":config_info.sensor_pnd_pint * 1000,
                              "sensor_pnd_dint":config_info.sensor_pnd_dint * 1000,
                              "sensor_pnd_mode":config_info.sensor_pnd_mode,
                              "sensor_pdt_pint":config_info.sensor_pdt_pint * 1000,
                              "sensor_pdt_dint":config_info.sensor_pdt_dint * 1000,
                              "sensor_pdt_mode":config_info.sensor_pdt_mode,
                              "lctrl_dimmer_vmax":config_info.lctrl_dimmer_vmax,
                          };
           
          }
          else if (config_info.model == "unode-v3"){
            configObject = {  "name": config_info.name,
                              "model": config_info.model,
                              "networkXPasskey":config_info.networkXPasskey,
                              "networkXSSID":config_info.networkXSSID,
                              "networkXSecurity":config_info.networkXSecurity,
                              "networkYPasskey":config_info.networkYPasskey,
                              "networkYSSID":config_info.networkYSSID,
                              "networkYSecurity":config_info.networkYSecurity,
                              "network_region":config_info.network_region,
                              "debugmode":config_info.debugmode,
                              "telnet":config_info.telnet,
                              "sensor_rf_pint":config_info.sensor_rf_pint * 1000,
                              "sensor_rf_dint":config_info.sensor_rf_dint * 1000,
                              "sensor_rf_mode":config_info.sensor_rf_mode,
                              "sensor_v_pint":config_info.sensor_v_pint * 1000,
                              "sensor_v_dint":config_info.sensor_v_dint * 1000,
                              "sensor_v_mode":config_info.sensor_v_mode,
                              "sensor_mi_pint":config_info.sensor_mi_pint * 1000,
                              "sensor_mi_dint":config_info.sensor_mi_dint * 1000,
                              "sensor_mi_mode":config_info.sensor_mi_mode,
                              "sensor_lIR_pint":config_info.sensor_lIR_pint * 1000,
                              "sensor_lIR_dint":config_info.sensor_lIR_dint  * 1000,
                              "sensor_lIR_mode":config_info.sensor_lIR_mode,
                              "sensor_l_pint":config_info.sensor_l_pint * 1000,
                              "sensor_l_dint":config_info.sensor_l_dint * 1000,
                              "sensor_l_mode":config_info.sensor_l_mode,
                              "sensor_p_pint":config_info.sensor_p_pint * 1000,
                              "sensor_p_dint":config_info.sensor_p_dint * 1000,
                              "sensor_p_mode":config_info.sensor_p_mode,
                              "sensor_t_pint":config_info.sensor_t_pint * 1000,
                              "sensor_t_dint":config_info.sensor_t_dint * 1000,
                              "sensor_t_mode":config_info.sensor_t_mode,
                              "sensor_T_pint":config_info.sensor_T_pint * 1000,
                              "sensor_T_dint":config_info.sensor_T_dint * 1000,
                              "sensor_T_mode":config_info.sensor_T_mode,
                              "sensor_pdc_pint":config_info.sensor_pdc_pint * 1000,
                              "sensor_pdc_dint":config_info.sensor_pdc_dint * 1000,
                              "sensor_pdc_mode":config_info.sensor_pdc_mode,
                              "sensor_ppr_pint":config_info.sensor_ppr_pint * 1000,
                              "sensor_ppr_dint":config_info.sensor_ppr_dint * 1000,
                              "sensor_ppr_mode":config_info.sensor_ppr_mode,
                              "sensor_pnd_pint":config_info.sensor_pnd_pint * 1000,
                              "sensor_pnd_dint":config_info.sensor_pnd_dint * 1000,
                              "sensor_pnd_mode":config_info.sensor_pnd_mode,
                              "sensor_pdt_pint":config_info.sensor_pdt_pint * 1000,
                              "sensor_pdt_dint":config_info.sensor_pdt_dint * 1000,
                              "sensor_pdt_mode":config_info.sensor_pdt_mode,
                              "sensor_a_jdelta":parseInt(config_info.sensor_a_jdelta),
                              "lctrl_dimmer_vmax":config_info.lctrl_dimmer_vmax,
                          };
           
          }
          else if (config_info.model == "unode-v4"){
            configObject = {  "name": config_info.name,
                              "model": config_info.model,
                              "networkXPasskey":config_info.networkXPasskey,
                              "networkXSSID":config_info.networkXSSID,
                              "networkXSecurity":config_info.networkXSecurity,
                              "networkYPasskey":config_info.networkYPasskey,
                              "networkYSSID":config_info.networkYSSID,
                              "networkYSecurity":config_info.networkYSecurity,
                              "network_region":config_info.network_region,
                              "debugmode":config_info.debugmode,
                              "telnet":config_info.telnet,
                              "aux_power":config_info.aux_power,
                              "podbus_disable":config_info.podbus_disable,
                              "sensor_rf_pint":config_info.sensor_rf_pint * 1000,
                              "sensor_rf_dint":config_info.sensor_rf_dint * 1000,
                              "sensor_rf_mode":config_info.sensor_rf_mode,
                              "sensor_v_pint":config_info.sensor_v_pint * 1000,
                              "sensor_v_dint":config_info.sensor_v_dint * 1000,
                              "sensor_v_mode":config_info.sensor_v_mode,
                              "sensor_aw_pint":config_info.sensor_aw_pint * 1000,
                              "sensor_aw_dint":config_info.sensor_aw_dint * 1000,
                              "sensor_aw_mode":config_info.sensor_aw_mode,
                              "sensor_aPF_pint":config_info.sensor_aPF_pint * 1000,
                              "sensor_aPF_dint":config_info.sensor_aPF_dint * 1000,
                              "sensor_aPF_mode":config_info.sensor_aPF_mode,
                              "sensor_aP_pint":config_info.sensor_aP_pint * 1000,
                              "sensor_aP_dint":config_info.sensor_aP_dint * 1000,
                              "sensor_aP_mode":config_info.sensor_aP_mode,
                              "sensor_mP_pint":config_info.sensor_mP_pint * 1000,
                              "sensor_mP_dint":config_info.sensor_mP_dint * 1000,
                              "sensor_mP_mode":config_info.sensor_mP_mode,
                              "sensor_mip_pint":config_info.sensor_mip_pint * 1000,
                              "sensor_mip_dint":config_info.sensor_mip_dint * 1000,
                              "sensor_mip_mode":config_info.sensor_mip_mode,
                              "sensor_ai_pint":config_info.sensor_ai_pint * 1000,
                              "sensor_ai_dint":config_info.sensor_ai_dint * 1000,
                              "sensor_ai_mode":config_info.sensor_ai_mode,
                              "sensor_aip_pint":config_info.sensor_aip_pint * 1000,
                              "sensor_aip_dint":config_info.sensor_aip_dint * 1000,
                              "sensor_aip_mode":config_info.sensor_aip_mode,
                              "sensor_mi_pint":config_info.sensor_mi_pint * 1000,
                              "sensor_mi_dint":config_info.sensor_mi_dint * 1000,
                              "sensor_mi_mode":config_info.sensor_mi_mode,
                              "sensor_mw_pint":config_info.sensor_mw_pint * 1000,
                              "sensor_mw_dint":config_info.sensor_mw_dint * 1000,
                              "sensor_mw_mode":config_info.sensor_mw_mode,
                              "sensor_mPF_pint":config_info.sensor_mPF_pint * 1000,
                              "sensor_mPF_dint":config_info.sensor_mPF_dint * 1000,
                              "sensor_mPF_mode":config_info.sensor_mPF_mode,
                              "sensor_lIR_pint":config_info.sensor_lIR_pint * 1000,
                              "sensor_lIR_dint":config_info.sensor_lIR_dint  * 1000,
                              "sensor_lIR_mode":config_info.sensor_lIR_mode,
                              "sensor_l_pint":config_info.sensor_l_pint * 1000,
                              "sensor_l_dint":config_info.sensor_l_dint * 1000,
                              "sensor_l_mode":config_info.sensor_l_mode,
                              "sensor_p_pint":config_info.sensor_p_pint * 1000,
                              "sensor_p_dint":config_info.sensor_p_dint * 1000,
                              "sensor_p_mode":config_info.sensor_p_mode,
                              "sensor_a_jdelta":parseInt(config_info.sensor_a_jdelta),
                              "sensor_pc_pint":config_info.sensor_pc_pint * 1000,
                              "sensor_pc_dint":config_info.sensor_pc_dint * 1000,
                              "sensor_pc_mode":config_info.sensor_pc_mode,
                              "sensor_t_pint":config_info.sensor_t_pint * 1000,
                              "sensor_t_dint":config_info.sensor_t_dint * 1000,
                              "sensor_t_mode":config_info.sensor_t_mode,
                              "sensor_T_pint":config_info.sensor_T_pint * 1000,
                              "sensor_T_dint":config_info.sensor_T_dint * 1000,
                              "sensor_T_mode":config_info.sensor_T_mode,
                              "sensor_mt_pint":config_info.sensor_mt_pint * 1000,
                              "sensor_mt_dint":config_info.sensor_mt_dint * 1000,
                              "sensor_mt_mode":config_info.sensor_mt_mode,
                              "sensor_podm_pint":config_info.sensor_podm_pint * 1000,
                              "sensor_podm_dint":config_info.sensor_podm_dint * 1000,
                              "sensor_podm_mode":config_info.sensor_podm_mode,
                              "sensor_podme_pint":config_info.sensor_podme_pint * 1000,
                              "sensor_podme_dint":config_info.sensor_podme_dint * 1000,
                              "sensor_podme_mode":config_info.sensor_podme_mode,
                              "sensor_rlym_pint":config_info.sensor_rlym_pint * 1000,
                              "lctrl_dimmer_vmax":config_info.lctrl_dimmer_vmax,
                              "lctrl_trigger_off_dint":config_info.lctrl_trigger_off_dint * 1000,
                              "lctrl_trigger_on_dint":config_info.lctrl_trigger_on_dint * 1000,
                              "lctrl_trigger_ont":config_info.lctrl_trigger_ont * 1000,
                              "lctrl_trigger_offt":config_info.lctrl_trigger_offt * 1000,
                              "lctrl_trigger_nonett":config_info.lctrl_trigger_nonett * 1000,
                          };

          }
          else if (config_info.model == "unode-v5"){
            configObject = {  "name": config_info.name,
                              "model": config_info.model,
                              "network_region":config_info.network_region,
                              "debugmode":config_info.debugmode,
                              "telnet":config_info.telnet,
                              "aux_power":config_info.aux_power,
                              "podbus_disable":config_info.podbus_disable,
                              "sensor_rf_pint":config_info.sensor_rf_pint * 1000,
                              "sensor_rf_dint":config_info.sensor_rf_dint * 1000,
                              "sensor_rf_mode":config_info.sensor_rf_mode,
                              "sensor_v_pint":config_info.sensor_v_pint * 1000,
                              "sensor_v_dint":config_info.sensor_v_dint * 1000,
                              "sensor_v_mode":config_info.sensor_v_mode,
                              "sensor_aw_pint":config_info.sensor_aw_pint * 1000,
                              "sensor_aw_dint":config_info.sensor_aw_dint * 1000,
                              "sensor_aw_mode":config_info.sensor_aw_mode,
                              "sensor_aPF_pint":config_info.sensor_aPF_pint * 1000,
                              "sensor_aPF_dint":config_info.sensor_aPF_dint * 1000,
                              "sensor_aPF_mode":config_info.sensor_aPF_mode,
                              "sensor_aP_pint":config_info.sensor_aP_pint * 1000,
                              "sensor_aP_dint":config_info.sensor_aP_dint * 1000,
                              "sensor_aP_mode":config_info.sensor_aP_mode,
                              "sensor_mP_pint":config_info.sensor_mP_pint * 1000,
                              "sensor_mP_dint":config_info.sensor_mP_dint * 1000,
                              "sensor_mP_mode":config_info.sensor_mP_mode,
                              "sensor_mip_pint":config_info.sensor_mip_pint * 1000,
                              "sensor_mip_dint":config_info.sensor_mip_dint * 1000,
                              "sensor_mip_mode":config_info.sensor_mip_mode,
                              "sensor_ai_pint":config_info.sensor_ai_pint * 1000,
                              "sensor_ai_dint":config_info.sensor_ai_dint * 1000,
                              "sensor_ai_mode":config_info.sensor_ai_mode,
                              "sensor_aip_pint":config_info.sensor_aip_pint * 1000,
                              "sensor_aip_dint":config_info.sensor_aip_dint * 1000,
                              "sensor_aip_mode":config_info.sensor_aip_mode,
                              "sensor_mi_pint":config_info.sensor_mi_pint * 1000,
                              "sensor_mi_dint":config_info.sensor_mi_dint * 1000,
                              "sensor_mi_mode":config_info.sensor_mi_mode,
                              "sensor_mw_pint":config_info.sensor_mw_pint * 1000,
                              "sensor_mw_dint":config_info.sensor_mw_dint * 1000,
                              "sensor_mw_mode":config_info.sensor_mw_mode,
                              "sensor_mPF_pint":config_info.sensor_mPF_pint * 1000,
                              "sensor_mPF_dint":config_info.sensor_mPF_dint * 1000,
                              "sensor_mPF_mode":config_info.sensor_mPF_mode,
                              "sensor_lIR_pint":config_info.sensor_lIR_pint * 1000,
                              "sensor_lIR_dint":config_info.sensor_lIR_dint  * 1000,
                              "sensor_lIR_mode":config_info.sensor_lIR_mode,
                              "sensor_l_pint":config_info.sensor_l_pint * 1000,
                              "sensor_l_dint":config_info.sensor_l_dint * 1000,
                              "sensor_l_mode":config_info.sensor_l_mode,
                              "sensor_l_i_pint":config_info.sensor_l_i_pint * 1000,
                              "sensor_l_i_dint":config_info.sensor_l_i_dint * 1000,
                              "sensor_l_i_mode":config_info.sensor_l_i_mode,
                              "sensor_lIR_i_pint":config_info.sensor_lIR_i_pint * 1000,
                              "sensor_lIR_i_dint":config_info.sensor_lIR_i_dint * 1000,
                              "sensor_lIR_i_mode":config_info.sensor_lIR_i_mode,
                              "sensor_p_pint":config_info.sensor_p_pint * 1000,
                              "sensor_p_dint":config_info.sensor_p_dint * 1000,
                              "sensor_p_mode":config_info.sensor_p_mode,
                              "sensor_pc_pint":config_info.sensor_pc_pint * 1000,
                              "sensor_pc_dint":config_info.sensor_pc_dint * 1000,
                              "sensor_pc_mode":config_info.sensor_pc_mode,
                              "sensor_t_pint":config_info.sensor_t_pint * 1000,
                              "sensor_t_dint":config_info.sensor_t_dint * 1000,
                              "sensor_t_mode":config_info.sensor_t_mode,
                              "sensor_T_pint":config_info.sensor_T_pint * 1000,
                              "sensor_T_dint":config_info.sensor_T_dint * 1000,
                              "sensor_T_mode":config_info.sensor_T_mode,
                              "sensor_mt_pint":config_info.sensor_mt_pint * 1000,
                              "sensor_mt_dint":config_info.sensor_mt_dint * 1000,
                              "sensor_mt_mode":config_info.sensor_mt_mode,
                              "sensor_podm_pint":config_info.sensor_podm_pint * 1000,
                              "sensor_podm_dint":config_info.sensor_podm_dint * 1000,
                              "sensor_podm_mode":config_info.sensor_podm_mode,
                              "sensor_podme_pint":config_info.sensor_podme_pint * 1000,
                              "sensor_podme_dint":config_info.sensor_podme_dint * 1000,
                              "sensor_podme_mode":config_info.sensor_podme_mode,
                              "sensor_rlym_pint":config_info.sensor_rlym_pint * 1000,
                              "lctrl_dimmer_vmax":config_info.lctrl_dimmer_vmax,
                              "lctrl_trigger_off_dint":config_info.lctrl_trigger_off_dint * 1000,
                              "lctrl_trigger_on_dint":config_info.lctrl_trigger_on_dint * 1000,
                              "lctrl_trigger_ont":config_info.lctrl_trigger_ont * 1000,
                              "lctrl_trigger_offt":config_info.lctrl_trigger_offt * 1000,
                              "lctrl_trigger_nonett":config_info.lctrl_trigger_nonett * 1000,
                          };

          }
          else {
            configObject = {  "name": config_info.name,
                              "model": config_info.model,
                              "network_region":config_info.network_region,
                              "debugmode":config_info.debugmode,
                              "telnet":config_info.telnet,
                              "aux_power":config_info.aux_power,
                              "podbus_disable":config_info.podbus_disable,
                              "sensor_rf_pint":config_info.sensor_rf_pint * 1000,
                              "sensor_rf_dint":config_info.sensor_rf_dint * 1000,
                              "sensor_rf_mode":config_info.sensor_rf_mode,
                              "sensor_v_pint":config_info.sensor_v_pint * 1000,
                              "sensor_v_dint":config_info.sensor_v_dint * 1000,
                              "sensor_v_mode":config_info.sensor_v_mode,
                              "sensor_aw_pint":config_info.sensor_aw_pint * 1000,
                              "sensor_aw_dint":config_info.sensor_aw_dint * 1000,
                              "sensor_aw_mode":config_info.sensor_aw_mode,
                              "sensor_aPF_pint":config_info.sensor_aPF_pint * 1000,
                              "sensor_aPF_dint":config_info.sensor_aPF_dint * 1000,
                              "sensor_aPF_mode":config_info.sensor_aPF_mode,
                              "sensor_aP_pint":config_info.sensor_aP_pint * 1000,
                              "sensor_aP_dint":config_info.sensor_aP_dint * 1000,
                              "sensor_aP_mode":config_info.sensor_aP_mode,
                              "sensor_mP_pint":config_info.sensor_mP_pint * 1000,
                              "sensor_mP_dint":config_info.sensor_mP_dint * 1000,
                              "sensor_mP_mode":config_info.sensor_mP_mode,
                              "sensor_mip_pint":config_info.sensor_mip_pint * 1000,
                              "sensor_mip_dint":config_info.sensor_mip_dint * 1000,
                              "sensor_mip_mode":config_info.sensor_mip_mode,
                              "sensor_ai_pint":config_info.sensor_ai_pint * 1000,
                              "sensor_ai_dint":config_info.sensor_ai_dint * 1000,
                              "sensor_ai_mode":config_info.sensor_ai_mode,
                              "sensor_aip_pint":config_info.sensor_aip_pint * 1000,
                              "sensor_aip_dint":config_info.sensor_aip_dint * 1000,
                              "sensor_aip_mode":config_info.sensor_aip_mode,
                              "sensor_mi_pint":config_info.sensor_mi_pint * 1000,
                              "sensor_mi_dint":config_info.sensor_mi_dint * 1000,
                              "sensor_mi_mode":config_info.sensor_mi_mode,
                              "sensor_mw_pint":config_info.sensor_mw_pint * 1000,
                              "sensor_mw_dint":config_info.sensor_mw_dint * 1000,
                              "sensor_mw_mode":config_info.sensor_mw_mode,
                              "sensor_mPF_pint":config_info.sensor_mPF_pint * 1000,
                              "sensor_mPF_dint":config_info.sensor_mPF_dint * 1000,
                              "sensor_mPF_mode":config_info.sensor_mPF_mode,
                              "sensor_lIR_pint":config_info.sensor_lIR_pint * 1000,
                              "sensor_lIR_dint":config_info.sensor_lIR_dint  * 1000,
                              "sensor_lIR_mode":config_info.sensor_lIR_mode,
                              "sensor_l_pint":config_info.sensor_l_pint * 1000,
                              "sensor_l_dint":config_info.sensor_l_dint * 1000,
                              "sensor_l_mode":config_info.sensor_l_mode,
                              "sensor_l_i_pint":config_info.sensor_l_i_pint * 1000,
                              "sensor_l_i_dint":config_info.sensor_l_i_dint * 1000,
                              "sensor_l_i_mode":config_info.sensor_l_i_mode,
                              "sensor_lIR_i_pint":config_info.sensor_lIR_i_pint * 1000,
                              "sensor_lIR_i_dint":config_info.sensor_lIR_i_dint * 1000,
                              "sensor_lIR_i_mode":config_info.sensor_lIR_i_mode,
                              "sensor_p_pint":config_info.sensor_p_pint * 1000,
                              "sensor_p_dint":config_info.sensor_p_dint * 1000,
                              "sensor_p_mode":config_info.sensor_p_mode,
                              "sensor_pc_pint":config_info.sensor_pc_pint * 1000,
                              "sensor_pc_dint":config_info.sensor_pc_dint * 1000,
                              "sensor_pc_mode":config_info.sensor_pc_mode,
                              "sensor_t_pint":config_info.sensor_t_pint * 1000,
                              "sensor_t_dint":config_info.sensor_t_dint * 1000,
                              "sensor_t_mode":config_info.sensor_t_mode,
                              "sensor_T_pint":config_info.sensor_T_pint * 1000,
                              "sensor_T_dint":config_info.sensor_T_dint * 1000,
                              "sensor_T_mode":config_info.sensor_T_mode,
                              "sensor_mt_pint":config_info.sensor_mt_pint * 1000,
                              "sensor_mt_dint":config_info.sensor_mt_dint * 1000,
                              "sensor_mt_mode":config_info.sensor_mt_mode,
                              "sensor_podm_pint":config_info.sensor_podm_pint * 1000,
                              "sensor_podm_dint":config_info.sensor_podm_dint * 1000,
                              "sensor_podm_mode":config_info.sensor_podm_mode,
                              "sensor_podme_pint":config_info.sensor_podme_pint * 1000,
                              "sensor_podme_dint":config_info.sensor_podme_dint * 1000,
                              "sensor_podme_mode":config_info.sensor_podme_mode,
                              "sensor_rlym_pint":config_info.sensor_rlym_pint * 1000,
                              "lctrl_dimmer_vmax":config_info.lctrl_dimmer_vmax,
                              "lctrl_trigger_off_dint":config_info.lctrl_trigger_off_dint * 1000,
                              "lctrl_trigger_on_dint":config_info.lctrl_trigger_on_dint * 1000,
                              "lctrl_trigger_ont":config_info.lctrl_trigger_ont * 1000,
                              "lctrl_trigger_offt":config_info.lctrl_trigger_offt * 1000,
                              "lctrl_trigger_nonett":config_info.lctrl_trigger_nonett * 1000,
                          };

          }
      

      //DataUtil.addEntity('add-configs', currentconfig_info, that.processAddConfigs, '');
      $.ajax({
        "url": NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/configs/',
        "type": "POST",
        "data": JSON.stringify(configObject),
        "xhrFields": {
          withCredentials: true
        },
        "dataType": "json",
        "contentType": "application/json",
        "processData": false,
        "success": function (data) {
          NSN.configID = data.configid;
          sessionStorage.setItem("configID", NSN.configID);
          data.idx = this.state.configs.length;
          data.model = helpers.modelName(data.model);
          data.nodes = 0;
          var newData = {
            "name": data.name,
            "server": data.server,
            "configid": data.configid,
            "model": data.model
          };

          var newState = React.addons.update(this.state, { configs: { $push: [newData] }, currentconfig: { $set: [data] }, configID: { $set: data.configid } });
          that.getAllConfigs();
          noty({ type: "success", text: 'Config "' + data.name + '" added.' });
          ReactBootstrap.Dispatcher.emit("Configform.add.success", data);
          this.configAssignment(assignment, data.configid);
          this.setState(newState);
        }.bind(that),
        "error": function () {
          noty({ type: "error", text: "Could not add config." });
        }
      })

    });

    ReactBootstrap.Dispatcher.on("Configform.modelSubmit", function (model_info) {
      console.log("Default config called", model_info);
      var url = (NSN.customerID && NSN.siteID) ?
        NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/config/' :
        NSN.apiURL + 'configs/';
      $.ajax({
        url: url + model_info,
        data: '',
        xhrFields: {
          withCredentials: true
        },
        method: 'GET',
        dataType: 'json',
        success: function (data) {
          if (data.errors) {
            console.log("/defaultmodel API returned error: " + JSON.stringify(data));
            $("#loadingmsg").html("Cannot retrieve default model list. " + " API returned error: " + JSON.stringify(data));
          } else {
            console.log("ajax success: " + JSON.stringify(data));
            data.model = model_info;
            data.name = "Default " + helpers.modelName(data.model);
            
            if(data.networkXPasskey){
              data.networkXPasskey = "";
            }
            if(data.networkYPasskey){
              data.networkYPasskey = "";
            }
            data.groups = [];
            data.assign = "unassigned";
            data.sites = [];
            data.nodes = [];
            data.assigngroups = [];

            if (data.sensor_a_jdelta) {
              data.sensor_a_jdelta = parseInt(data.sensor_a_jdelta);
            }
            if (data.lctrl_dimmer_vmax) {
              data.lctrl_dimmer_vmax = data.lctrl_dimmer_vmax;
            }
            if (data.sensor_rf_pint) {
              data.sensor_rf_pint = data.sensor_rf_pint / 1000;
            }
            if (data.sensor_rf_dint) {
              data.sensor_rf_dint = data.sensor_rf_dint / 1000;
            }
            if (data.sensor_v_pint) {
              data.sensor_v_pint = data.sensor_v_pint / 1000;
            }
            if (data.sensor_v_dint) {
              data.sensor_v_dint = data.sensor_v_dint / 1000;
            }
            if (data.sensor_ai_pint) {
              data.sensor_ai_pint = data.sensor_ai_pint / 1000;
            }
            if (data.sensor_ai_dint) {
              data.sensor_ai_dint = data.sensor_ai_dint / 1000;
            }
            if (data.sensor_aip_pint) {
              data.sensor_aip_pint = data.sensor_aip_pint / 1000;
            }
            if (data.sensor_aip_dint) {
              data.sensor_aip_dint = data.sensor_aip_dint / 1000;
            }
            if (data.sensor_mP_pint) {
              data.sensor_mP_pint = data.sensor_mP_pint / 1000;
            }
            if (data.sensor_mP_dint) {
              data.sensor_mP_dint = data.sensor_mP_dint / 1000;
            }
            if (data.sensor_mip_pint) {
              data.sensor_mip_pint = data.sensor_mip_pint / 1000;
            }
            if (data.sensor_mip_dint) {
              data.sensor_mip_dint = data.sensor_mip_dint / 1000;
            }
            if (data.sensor_aw_pint) {
              data.sensor_aw_pint = data.sensor_aw_pint / 1000;
            }
            if (data.sensor_aw_dint) {
              data.sensor_aw_dint = data.sensor_aw_dint / 1000;
            }
            if (data.sensor_aPF_pint) {
              data.sensor_aPF_pint = data.sensor_aPF_pint / 1000;
            }
            if (data.sensor_aPF_dint) {
              data.sensor_aPF_dint = data.sensor_aPF_dint / 1000;
            }
            if (data.sensor_aP_pint) {
              data.sensor_aP_pint = data.sensor_aP_pint / 1000;
            }
            if (data.sensor_aP_dint) {
              data.sensor_aP_dint = data.sensor_aP_dint / 1000;
            }
            if (data.sensor_mi_pint) {
              data.sensor_mi_pint = data.sensor_mi_pint / 1000;
            }
            if (data.sensor_mi_dint) {
              data.sensor_mi_dint = data.sensor_mi_dint / 1000;
            }
            if (data.sensor_mw_pint) {
              data.sensor_mw_pint = data.sensor_mw_pint / 1000;
            }
            if (data.sensor_mw_dint) {
              data.sensor_mw_dint = data.sensor_mw_dint / 1000;
            }
            if (data.sensor_mPF_pint) {
              data.sensor_mPF_pint = data.sensor_mPF_pint / 1000;
            }
            if (data.sensor_mPF_dint) {
              data.sensor_mPF_dint = data.sensor_mPF_dint / 1000;
            }
            if (data.sensor_lIR_pint) {
              data.sensor_lIR_pint = data.sensor_lIR_pint / 1000;
            }
            if (data.sensor_lIR_dint) {
              data.sensor_lIR_dint = data.sensor_lIR_dint / 1000;
            }
            if (data.sensor_l_pint) {
              data.sensor_l_pint = data.sensor_l_pint / 1000;
            }
            if (data.sensor_l_dint) {
              data.sensor_l_dint = data.sensor_l_dint / 1000;
            }
            if (data.sensor_l_i_pint) {
              data.sensor_l_i_pint = data.sensor_l_i_pint / 1000;
            }
            if (data.sensor_l_i_dint) {
              data.sensor_l_i_dint = data.sensor_l_i_dint / 1000;
            }
            if (data.sensor_lIR_i_pint) {
              data.sensor_lIR_i_pint = data.sensor_lIR_i_pint / 1000;
            }
            if (data.sensor_lIR_i_dint) {
              data.sensor_lIR_i_dint = data.sensor_lIR_i_dint / 1000;
            }
            if (data.sensor_p_pint) {
              data.sensor_p_pint = data.sensor_p_pint / 1000;
            }
            if (data.sensor_p_dint) {
              data.sensor_p_dint = data.sensor_p_dint / 1000;
            }
            if (data.sensor_pc_pint) {
              data.sensor_pc_pint = data.sensor_pc_pint / 1000;
            }
            if (data.sensor_pc_dint) {
              data.sensor_pc_dint = data.sensor_pc_dint / 1000;
            }
            if (data.sensor_t_pint) {
              data.sensor_t_pint = data.sensor_t_pint / 1000;
            }
            if (data.sensor_t_dint) {
              data.sensor_t_dint = data.sensor_t_dint / 1000;
            }
            if (data.sensor_T_pint) {
              data.sensor_T_pint = data.sensor_T_pint / 1000;
            }
            if (data.sensor_T_dint) {
              data.sensor_T_dint = data.sensor_T_dint / 1000;
            }
            if (data.sensor_mt_pint) {
              data.sensor_mt_pint = data.sensor_mt_pint / 1000;
            }
            if (data.sensor_mt_dint) {
              data.sensor_mt_dint = data.sensor_mt_dint / 1000;
            }
            if (data.sensor_pdc_pint) {
              data.sensor_pdc_pint = data.sensor_pdc_pint / 1000;
            }
            if (data.sensor_pdc_dint) {
              data.sensor_pdc_dint = data.sensor_pdc_dint / 1000;
            }
            if (data.sensor_ppr_pint) {
              data.sensor_ppr_pint = data.sensor_ppr_pint / 1000;
            }
            if (data.sensor_ppr_dint) {
              data.sensor_ppr_dint = data.sensor_ppr_dint / 1000;
            }
            if (data.sensor_pnd_pint) {
              data.sensor_pnd_pint = data.sensor_pnd_pint / 1000;
            }
            if (data.sensor_pnd_dint) {
              data.sensor_pnd_dint = data.sensor_pnd_dint / 1000;
            }
            if (data.sensor_pdt_pint) {
              data.sensor_pdt_pint = data.sensor_pdt_pint / 1000;
            }
            if (data.sensor_pdt_dint) {
              data.sensor_pdt_dint = data.sensor_pdt_dint / 1000;
            }
            if (data.sensor_podm_pint) {
              data.sensor_podm_pint = data.sensor_podm_pint / 1000;
            }
            if (data.sensor_podm_dint) {
              data.sensor_podm_dint = data.sensor_podm_dint / 1000;
            }
            if (data.sensor_podme_pint) {
              data.sensor_podme_pint = data.sensor_podme_pint / 1000;
            }
            if (data.sensor_podme_dint) {
              data.sensor_podme_dint = data.sensor_podme_dint / 1000;
            }
            if (data.sensor_rlym_pint) {
              data.sensor_rlym_pint = data.sensor_rlym_pint / 1000;
            }
            if (data.sensor_rlym_dint) {
              data.sensor_rlym_dint = data.sensor_rlym_dint / 1000;
            }
            if (data.lctrl_trigger_ont) {
              data.lctrl_trigger_ont = data.lctrl_trigger_ont / 1000;
            }
            if (data.lctrl_trigger_offt) {
              data.lctrl_trigger_offt = data.lctrl_trigger_offt / 1000;
            }
            if (data.lctrl_trigger_nonett) {
              data.lctrl_trigger_nonett = data.lctrl_trigger_nonett / 1000;
            }
            if (data.lctrl_trigger_off_dint) {
              data.lctrl_trigger_off_dint = data.lctrl_trigger_off_dint / 1000;
            }
            if (data.lctrl_trigger_on_dint) {
              data.lctrl_trigger_on_dint = data.lctrl_trigger_on_dint / 1000;
            }
          
            that.setState({
              defaultmodel: data
            })
          };
        },
        error: function (jqXHR, status, error) {
          console.log("ajax failure (defaultmodel): " + status + " - " + error);
          if (jqXHR.status == 400) {
            noty({ type: "error", text: JSON.parse(jqXHR.responseText).message });
          } else {
            noty({
              type: "error", text: 'Could not retrieve Default model.' + 'Return status: ' + status
            });
          }
          //$("#loadingmsg").html("Cannot retrieve Default model.  API reported error: " + error);
        }
      });

    });

    ReactBootstrap.Dispatcher.on("Configform.delete", function (config_info) {

      var idx = helpers.get_idx(that.state.configs, config_info, 'configid');
      console.log("Deleting config (idx:" + idx + "; id:" + config_info.configid + ")");
      delete config_info.idx;
      DataUtil.deleteEntity('delete-configs', config_info, that.processDeleteConfigs, idx);
      that.setState({
        showConfigDetail: false
      });

      // $.ajax({
      //   "url" : NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/configs/' + config_info.configid,
      //   "type" : "DELETE",
      //   "xhrFields": {
      //      withCredentials: true
      //   },
      //   "data" : JSON.stringify(config_info),
      //   "dataType" : "json",
      //   "contentType" : "application/json",
      //   "success" : function(data) {

      //     var newState = React.addons.update(this.state, { configs: { $splice: [[idx, 1]] }, configID: {$set: "-1" } });
      //     NSN.configID = "-1";
      //     sessionStorage.setItem("configID", NSN.configID);
      //     noty({type:"success", text:'Config "' + config_info.name + '" deleted.'});
      //     ReactBootstrap.Dispatcher.emit("Configform.delete.success", config_info.configid);
      //     this.setState(newState);
      //   }.bind(that),
      //   "error" : function(jqXHR, status, error) {
      //     console.log("ajax failure (configs): " + status + " - " + error);
      //     noty({type:"error", text:"Could not delete config."});
      //   }
      // });
    });

    ReactBootstrap.Dispatcher.on('Configform.update', function (switchto, config_info) {
      that.setState({
        showselectedconfigs: switchto == 'open',
        selectedconfig: config_info
      });
    });

    ReactBootstrap.Dispatcher.on('Configform.deleteconfigwithnodes', function (switchtoalert, config_info) {
      that.setState({
        deleteconfigwithnodesalert: switchtoalert == 'opendelete',
        deleteselectedconfig: config_info
      });
    });

    ReactBootstrap.Dispatcher.on('Configform.saveandapply', function (config_info) {
      console.log("save and apply emitting", config_info);
      that.setState({
        showConfigDetail: false
      });


      var assignmentToNodes = [];

      $.each(config_info.nodes, function (i, obj) {
        assignmentToNodes.push(obj.nodeid);
      });


      var newState = {};
      var configObject = {};
      var currentDateTime = new Date().toString();
      console.log("config_info.update", config_info);
      console.log(config_info.configid);
      delete config_info.assign;
      delete config_info.assigngroups;
      delete config_info.groups;
      delete config_info.sites;
      delete config_info.nodes;

      var idx = helpers.get_idx(that.state.configs, config_info, 'configid');
      console.log("Updating config (idx:" + idx + "; id:" + config_info.configid + ")");
      delete config_info.idx;

       if(config_info.model == "vdkmaster"){
            configObject = {  "name": config_info.name + "_" + currentDateTime,
                              "model": config_info.model,
                              "configid":config_info.configid,
                              "siteid": config_info.siteid,
                              "al.left.setbrightness":parseInt(config_info["al.left.setbrightness"]),
                              "al.left.setmode":config_info["al.left.setmode"],
                              "al.right.setbrightness":parseInt(config_info["al.right.setbrightness"]),
                              "al.right.setmode":config_info["al.right.setmode"],
                              "alarm.redis.memory_threshold":parseFloat(config_info["alarm.redis.memory_threshold"]),
                              "application_server":config_info.application_server,
                              "application_server_port":parseInt(config_info.application_server_port),
                              "appos.timezone":config_info["appos.timezone"],
                              "glassl.display.blue.gain":parseInt(config_info["glassl.display.blue.gain"]),
                              "glassl.display.blue.offset":parseInt(config_info["glassl.display.blue.offset"]),
                              "glassl.display.brightness":parseInt(config_info["glassl.display.brightness"]),
                              "glassl.display.color-temp":parseInt(config_info["glassl.display.color-temp"]),
                              "glassl.display.contrast":parseInt(config_info["glassl.display.contrast"]),
                              "glassl.display.green.gain":parseInt(config_info["glassl.display.green.gain"]),
                              "glassl.display.green.offset":parseInt(config_info["glassl.display.green.offset"]),
                              "glassl.display.hue":parseInt(config_info["glassl.display.hue"]),
                              "glassl.display.mode.ccs":config_info["glassl.display.mode.ccs"],
                              "glassl.display.mode.dynamic-nr":config_info["glassl.display.mode.dynamic-nr"],
                              "glassl.display.mode.main-dcdi":config_info["glassl.display.mode.main-dcdi"],
                              "glassl.display.mode.main-madi":config_info["glassl.display.mode.main-madi"],
                              "glassl.display.mode.mpeg-nr":config_info["glassl.display.mode.mpeg-nr"],
                              "glassl.display.mode.selection-1080p":config_info["glassl.display.mode.selection-1080p"],
                              "glassl.display.mode.selection-720p":config_info["glassl.display.mode.selection-720p"],
                              "glassl.display.mpeg-nr":parseInt(config_info["glassl.display.mpeg-nr"]),
                              "glassl.display.power":config_info["glassl.display.power"],
                              "glassl.display.red.gain":parseInt(config_info["glassl.display.red.gain"]),
                              "glassl.display.red.offset":parseInt(config_info["glassl.display.red.offset"]),
                              "glassl.display.saturation":parseInt(config_info["glassl.display.saturation"]),
                              "glassl.display.sharpness":parseInt(config_info["glassl.display.sharpness"]),
                              "glassl.display.temp.cold":parseInt(config_info["glassl.display.temp.cold"]),
                              "glassl.display.temp.hot":parseInt(config_info["glassl.display.temp.hot"]),
                              "glassl.display.temp.warm":parseInt(config_info["glassl.display.temp.warm"]),
                              "glassr.display.blue.gain":parseInt(config_info["glassr.display.blue.gain"]),
                              "glassr.display.blue.offset":parseInt(config_info["glassr.display.blue.offset"]),
                              "glassr.display.brightness":parseInt(config_info["glassr.display.brightness"]),
                              "glassr.display.color-temp":parseInt(config_info["glassr.display.color-temp"]),
                              "glassr.display.contrast":parseInt(config_info["glassr.display.contrast"]),
                              "glassr.display.green.gain":parseInt(config_info["glassr.display.green.gain"]),
                              "glassr.display.green.offset":parseInt(config_info["glassr.display.green.offset"]),
                              "glassr.display.hue":parseInt(config_info["glassr.display.hue"]),
                              "glassr.display.mode.ccs":config_info["glassr.display.mode.ccs"],
                              "glassr.display.mode.dynamic-nr":config_info["glassr.display.mode.dynamic-nr"],
                              "glassr.display.mode.main-dcdi":config_info["glassr.display.mode.main-dcdi"],
                              "glassr.display.mode.main-madi":config_info["glassr.display.mode.main-madi"],
                              "glassr.display.mode.mpeg-nr":config_info["glassr.display.mode.mpeg-nr"],
                              "glassr.display.mode.selection-1080p":config_info["glassr.display.mode.selection-1080p"],
                              "glassr.display.mode.selection-720p":config_info["glassr.display.mode.selection-720p"],
                              "glassr.display.mpeg-nr":parseInt(config_info["glassr.display.mpeg-nr"]),
                              "glassr.display.power":config_info["glassr.display.power"],
                              "glassr.display.red.gain":parseInt(config_info["glassr.display.red.gain"]),
                              "glassr.display.red.offset":parseInt(config_info["glassr.display.red.offset"]),
                              "glassr.display.saturation":parseInt(config_info["glassr.display.saturation"]),
                              "glassr.display.sharpness":parseInt(config_info["glassr.display.sharpness"]),
                              "glassr.display.temp.cold":parseInt(config_info["glassr.display.temp.cold"]),
                              "glassr.display.temp.hot":parseInt(config_info["glassr.display.temp.hot"]),
                              "glassr.display.temp.warm":parseInt(config_info["glassr.display.temp.warm"]),
                              "lr.setcolor":config_info["lr.setcolor"],
                              "network.eth-x.ip":config_info["network.eth-x.ip"],
                              "network.eth-x.method":config_info["network.eth-x.method"],
                              "network.eth-x.netmask":parseInt(config_info["network.eth-x.netmask"]),
                              "network.eth-x.port-forwarding.genetec":config_info["network.eth-x.port-forwarding.genetec"],
                              "network.eth-x.port-forwarding.https":config_info["network.eth-x.port-forwarding.https"],
                              "network.eth-x.port-forwarding.rtsp":config_info["network.eth-x.port-forwarding.rtsp"],
                              "network.eth-x.port-forwarding.ssh":config_info["network.eth-x.port-forwarding.ssh"],
                              "network.eth-x.whitelist":config_info["network.eth-x.whitelist"],
                              "network.firewall.ports":config_info["network.firewall.ports"],
                              "network.firewall.protocols":config_info["network.firewall.protocols"],
                              "network.server.mqtt":config_info["network.server.mqtt"],
                              "network.server.vpn":config_info["network.server.vpn"],
                              "network.vpn_on_demand":config_info["network.vpn_on_demand"],
                              "pebble.display.blue.gain":parseInt(config_info["pebble.display.blue.gain"]),
                              "pebble.display.blue.offset":parseInt(config_info["pebble.display.blue.offset"]),
                              "pebble.display.brightness":parseInt(config_info["pebble.display.brightness"]),
                              "pebble.display.color-temp":parseInt(config_info["pebble.display.color-temp"]),
                              "pebble.display.contrast":parseInt(config_info["pebble.display.contrast"]),
                              "pebble.display.green.gain":parseInt(config_info["pebble.display.green.gain"]),
                              "pebble.display.green.offset":parseInt(config_info["pebble.display.green.offset"]),
                              "pebble.display.hue":parseInt(config_info["pebble.display.hue"]),
                              "pebble.display.mode.ccs":config_info["pebble.display.mode.ccs"],
                              "pebble.display.mode.dynamic-nr":config_info["pebble.display.mode.dynamic-nr"],
                              "pebble.display.mode.main-dcdi":config_info["pebble.display.mode.main-dcdi"],
                              "pebble.display.mode.main-madi":config_info["pebble.display.mode.main-madi"],
                              "pebble.display.mode.mpeg-nr":config_info["pebble.display.mode.mpeg-nr"],
                              "pebble.display.mode.selection-1080p":config_info["pebble.display.mode.selection-1080p"],
                              "pebble.display.mode.selection-720p":config_info["pebble.display.mode.selection-720p"],
                              "pebble.display.mpeg-nr":parseInt(config_info["pebble.display.mpeg-nr"]),
                              "pebble.display.power":config_info["pebble.display.power"],
                              "pebble.display.red.gain":config_info["pebble.display.red.gain"],
                              "pebble.display.red.offset":parseInt(config_info["pebble.display.red.offset"]),
                              "pebble.display.saturation":parseInt(config_info["pebble.display.saturation"]),
                              "pebble.display.sharpness":parseInt(config_info["pebble.display.sharpness"]),
                              "pebble.display.temp.cold":parseInt(config_info["pebble.display.temp.cold"]),
                              "pebble.display.temp.hot":parseInt(config_info["pebble.display.temp.hot"]),
                              "pebble.display.temp.warm":parseInt(config_info["pebble.display.temp.warm"]),
                              "server":config_info.server,
                            };
            

          }
          else if (config_info.model == "merlin"){
             configObject = {  "name": config_info.name + "_" + currentDateTime,
                              "model": config_info.model,
                              "configid":config_info.configid,
                              "Genetec.custom_events":config_info["Genetec.custom_events"],
                              "alarm.redis.memory_threshold":parseFloat(config_info["alarm.redis.memory_threshold"]),
                              "application_server":config_info.application_server,
                              "application_server_port":parseInt(config_info.application_server_port),
                              "camera.0.binning":config_info["camera.0.binning"],
                              "camera.0.enabled":config_info["camera.0.enabled"],
                              "camera.0.exposure":parseInt(config_info["camera.0.exposure"]),
                              "camera.0.gain":parseInt(config_info["camera.0.gain"]),
                              "camera.0.master":parseInt(config_info["camera.0.master"]),
                              "camera.0.streamH.bitrate":parseInt(config_info["camera.0.streamH.bitrate"]),
                              "camera.0.streamH.format":config_info["camera.0.streamH.format"],
                              "camera.0.streamH.framerate":parseInt(config_info["camera.0.streamH.framerate"]),
                              "camera.0.streamH.gopsize":parseInt(config_info["camera.0.streamH.gopsize"]),
                              "camera.0.streamH.storage.diskquota":parseFloat(config_info["camera.0.streamH.storage.diskquota"]),
                              "camera.0.streamH.storage.recordevents":config_info["camera.0.streamH.storage.recordevents"],
                              "camera.0.streamH.storage.recordmode":parseInt(config_info["camera.0.streamH.storage.recordmode"]),
                              "camera.0.streamH.storage.recordpostamble":parseInt(config_info["camera.0.streamH.storage.recordpostamble"]),
                              "camera.0.streamH.storage.recordpreamble":parseInt(config_info["camera.0.streamH.storage.recordpreamble"]),
                              "camera.0.streamH.storage.vaqa.0.duration":config_info["camera.0.streamH.storage.vaqa.0.duration"],
                              "camera.0.streamH.storage.vaqa.0.interval":config_info["camera.0.streamH.storage.vaqa.0.interval"],
                              "camera.0.streamH.storage.vaqa.0.qty":parseInt(config_info["camera.0.streamH.storage.vaqa.0.qty"]),
                              "camera.0.streamH.storage.vaqa.1.duration":config_info["camera.0.streamH.storage.vaqa.1.duration"],
                              "camera.0.streamH.storage.vaqa.1.interval":config_info["camera.0.streamH.storage.vaqa.1.interval"],
                              "camera.0.streamH.storage.vaqa.1.qty":parseInt(config_info["camera.0.streamH.storage.vaqa.1.qty"]),
                              "camera.0.streamL.bitrate":parseInt(config_info["camera.0.streamL.bitrate"]),
                              "camera.0.streamL.format":config_info["camera.0.streamL.format"],
                              "camera.0.streamL.framerate":parseInt(config_info["camera.0.streamL.framerate"]),
                              "camera.0.streamL.gopsize":parseInt(config_info["camera.0.streamL.gopsize"]),
                              "camera.0.streamL.storage.diskquota":parseFloat(config_info["camera.0.streamL.storage.diskquota"]),
                              "camera.0.streamL.storage.recordevents":config_info["camera.0.streamL.storage.recordevents"],
                              "camera.0.streamL.storage.recordmode":parseInt(config_info["camera.0.streamL.storage.recordmode"]),
                              "camera.0.streamL.storage.recordpostamble":parseInt(config_info["camera.0.streamL.storage.recordpostamble"]),
                              "camera.0.streamL.storage.recordpreamble":parseInt(config_info["camera.0.streamL.storage.recordpreamble"]),
                              "camera.1.enabled":config_info["camera.1.enabled"],
                              "camera.1.streamH.bitrate":parseInt(config_info["camera.1.streamH.bitrate"]),
                              "camera.1.streamH.format":config_info["camera.1.streamH.format"],
                              "camera.1.streamH.framerate":parseInt(config_info["camera.1.streamH.framerate"]),
                              "camera.1.streamH.gopsize":parseInt(config_info["camera.1.streamH.gopsize"]),
                              "camera.1.streamH.storage.recordevents":config_info["camera.1.streamH.storage.recordevents"],
                              "camera.1.streamH.storage.recordpostamble":parseInt(config_info["camera.1.streamH.storage.recordpostamble"]),
                              "camera.1.streamH.storage.recordpreamble":parseInt(config_info["camera.1.streamH.storage.recordpreamble"]),
                              "camera.1.streamH.storage.vaqa.0.duration":config_info["camera.1.streamH.storage.vaqa.0.duration"],
                              "camera.1.streamH.storage.vaqa.0.interval":config_info["camera.1.streamH.storage.vaqa.0.interval"],
                              "camera.1.streamH.storage.vaqa.0.qty":parseInt(config_info["camera.1.streamH.storage.vaqa.0.qty"]),
                              "camera.1.streamH.storage.vaqa.1.duration":config_info["camera.1.streamH.storage.vaqa.1.duration"],
                              "camera.1.streamH.storage.vaqa.1.interval":config_info["camera.1.streamH.storage.vaqa.1.interval"],
                              "camera.1.streamH.storage.vaqa.1.qty":parseInt(config_info["camera.1.streamH.storage.vaqa.1.qty"]),
                              "camera.1.streamL.bitrate":parseInt(config_info["camera.1.streamL.bitrate"]),
                              "camera.1.streamL.format":config_info["camera.1.streamL.format"],
                              "camera.1.streamL.framerate":parseInt(config_info["camera.1.streamL.framerate"]),
                              "camera.1.streamL.gopsize":parseInt(config_info["camera.1.streamL.gopsize"]),
                              "camera.1.streamL.storage.recordevents":config_info["camera.1.streamL.storage.recordevents"],
                              "camera.1.streamL.storage.recordpostamble":parseInt(config_info["camera.1.streamL.storage.recordpostamble"]),
                              "camera.1.streamL.storage.recordpreamble":parseInt(config_info["camera.1.streamL.storage.recordpreamble"]),
                              "evtgrab.capint":parseInt(config_info["evtgrab.capint"]),
                              "evtgrab.dint":parseInt(config_info["evtgrab.dint"]),
                              "evtgrab.nfiles":parseInt(config_info["evtgrab.nfiles"]),
                              "evtgrab.periodic_only":config_info["evtgrab.periodic_only"],
                              "mediaserver.nomve":config_info["mediaserver.nomve"],
                              "mediaserver.rtspsrc":config_info["mediaserver.rtspsrc"],
                              "mediaserver.video_codec":config_info["mediaserver.video_codec"],
                              "mediaserver.wmm_qos":config_info["mediaserver.wmm_qos"],
                              "network.eth-x.ip":config_info["network.eth-x.ip"],
                              "network.eth-x.method":config_info["network.eth-x.method"],
                              "network.eth-x.netmask":parseInt(config_info["network.eth-x.netmask"]),
                              "network.firewall.ports":config_info["network.firewall.ports"],
                              "network.firewall.protocols":config_info["network.firewall.protocols"],
                              "network.server.mqtt":config_info["network.server.mqtt"],
                              "network.server.vpn":config_info["network.server.vpn"],
                              "network.vpn_on_demand":config_info["network.vpn_on_demand"],
                              "rtsp.service":parseInt(config_info["rtsp.service"]),
                              "server":config_info.server,
                              "storage.maxsessions":parseInt(config_info["storage.maxsessions"]),
                              "storageserver.cache_size":parseInt(config_info["storageserver.cache_size"]),
                              "storageserver.max_clip_size":parseInt(config_info["storageserver.max_clip_size"]),
                            };

          }
          else if (config_info.model == "falcon-q"){
            configObject = {  "name": config_info.name + "_" + currentDateTime,
                              "model": config_info.model,
                              "configid":config_info.configid,
                              "Genetec.custom_events":config_info["Genetec.custom_events"],
                              "alarm.redis.memory_threshold":parseFloat(config_info["alarm.redis.memory_threshold"]),
                              "application_server":config_info.application_server,
                              "application_server_port":parseInt(config_info.application_server_port),
                              "camera.0.autoexposure":config_info["camera.0.autoexposure"],
                              "camera.0.autogain":config_info["camera.0.autogain"],
                              "camera.0.colortemp":parseInt(config_info["camera.0.colortemp"]),
                              "camera.0.enabled":config_info["camera.0.enabled"],
                              "camera.0.exposure":parseInt(config_info["camera.0.exposure"]),
                              "camera.0.flickermode":config_info["camera.0.flickermode"],
                              "camera.0.gain":parseInt(config_info["camera.0.gain"]),
                              "camera.0.indoormode":config_info["camera.0.indoormode"],
                              "camera.0.master":parseInt(config_info["camera.0.master"]),
                              "camera.0.scanmode":config_info["camera.0.scanmode"],
                              "camera.0.streamH.bitrate":parseInt(config_info["camera.0.streamH.bitrate"]),
                              "camera.0.streamH.format":config_info["camera.0.streamH.format"],
                              "camera.0.streamH.framerate":parseInt(config_info["camera.0.streamH.framerate"]),
                              "camera.0.streamH.gopsize":parseInt(config_info["camera.0.streamH.gopsize"]),
                              "camera.0.streamH.storage.diskquota":parseFloat(config_info["camera.0.streamH.storage.diskquota"]),
                              "camera.0.streamH.storage.recordevents":config_info["camera.0.streamH.storage.recordevents"],
                              "camera.0.streamH.storage.recordmode":parseInt(config_info["camera.0.streamH.storage.recordmode"]),
                              "camera.0.streamH.storage.recordpostamble":parseInt(config_info["camera.0.streamH.storage.recordpostamble"]),
                              "camera.0.streamH.storage.recordpreamble":parseInt(config_info["camera.0.streamH.storage.recordpreamble"]),
                              "camera.0.streamH.storage.vaqa.0.duration":config_info["camera.0.streamH.storage.vaqa.0.duration"],
                              "camera.0.streamH.storage.vaqa.0.interval":config_info["camera.0.streamH.storage.vaqa.0.interval"],
                              "camera.0.streamH.storage.vaqa.0.qty":parseInt(config_info["camera.0.streamH.storage.vaqa.0.qty"]),
                              "camera.0.streamH.storage.vaqa.1.duration":config_info["camera.0.streamH.storage.vaqa.1.duration"],
                              "camera.0.streamH.storage.vaqa.1.interval":config_info["camera.0.streamH.storage.vaqa.1.interval"],
                              "camera.0.streamH.storage.vaqa.1.qty":parseInt(config_info["camera.0.streamH.storage.vaqa.1.qty"]),
                              "camera.0.streamL.bitrate":parseInt(config_info["camera.0.streamL.bitrate"]),
                              "camera.0.streamL.format":config_info["camera.0.streamL.format"],
                              "camera.0.streamL.framerate":parseInt(config_info["camera.0.streamL.framerate"]),
                              "camera.0.streamL.gopsize":parseInt(config_info["camera.0.streamL.gopsize"]),
                              "camera.0.streamL.storage.diskquota":parseFloat(config_info["camera.0.streamL.storage.diskquota"]),
                              "camera.0.streamL.storage.recordevents":config_info["camera.0.streamL.storage.recordevents"],
                              "camera.0.streamL.storage.recordmode":parseInt(config_info["camera.0.streamL.storage.recordmode"]),
                              "camera.0.streamL.storage.recordpostamble":parseInt(config_info["camera.0.streamL.storage.recordpostamble"]),
                              "camera.0.streamL.storage.recordpreamble":parseInt(config_info["camera.0.streamL.storage.recordpreamble"]),
                              "camera.0.wdrmode":config_info["camera.0.wdrmode"],
                              "camera.1.autoexposure":config_info["camera.1.autoexposure"],
                              "camera.1.autogain":config_info["camera.1.autogain"],
                              "camera.1.colortemp":parseInt(config_info["camera.1.colortemp"]),
                              "camera.1.enabled":config_info["camera.1.enabled"],
                              "camera.1.exposure":parseInt(config_info["camera.1.exposure"]),
                              "camera.1.flickermode":config_info["camera.1.flickermode"],
                              "camera.1.gain":parseInt(config_info["camera.1.gain"]),
                              "camera.1.indoormode":config_info["camera.1.indoormode"],
                              "camera.1.scanmode":config_info["camera.1.scanmode"],
                              "camera.1.streamH.bitrate":parseInt(config_info["camera.1.streamH.bitrate"]),
                              "camera.1.streamH.format":config_info["camera.1.streamH.format"],
                              "camera.1.streamH.framerate":parseInt(config_info["camera.1.streamH.framerate"]),
                              "camera.1.streamH.gopsize":parseInt(config_info["camera.1.streamH.gopsize"]),
                              "camera.1.streamH.storage.diskquota":parseFloat(config_info["camera.1.streamH.storage.diskquota"]),
                              "camera.1.streamH.storage.recordevents":config_info["camera.1.streamH.storage.recordevents"],
                              "camera.1.streamH.storage.recordmode":parseInt(config_info["camera.1.streamH.storage.recordmode"]),
                              "camera.1.streamH.storage.recordpostamble":parseInt(config_info["camera.1.streamH.storage.recordpostamble"]),
                              "camera.1.streamH.storage.recordpreamble":parseInt(config_info["camera.1.streamH.storage.recordpreamble"]),
                              "camera.1.streamH.storage.vaqa.0.duration":config_info["camera.1.streamH.storage.vaqa.0.duration"],
                              "camera.1.streamH.storage.vaqa.0.interval":config_info["camera.1.streamH.storage.vaqa.0.interval"],
                              "camera.1.streamH.storage.vaqa.0.qty":parseInt(config_info["camera.1.streamH.storage.vaqa.0.qty"]),
                              "camera.1.streamH.storage.vaqa.1.duration":config_info["camera.1.streamH.storage.vaqa.1.duration"],
                              "camera.1.streamH.storage.vaqa.1.interval":config_info["camera.1.streamH.storage.vaqa.1.interval"],
                              "camera.1.streamH.storage.vaqa.1.qty":parseInt(config_info["camera.1.streamH.storage.vaqa.1.qty"]),
                              "camera.1.streamL.bitrate":parseInt(config_info["camera.1.streamL.bitrate"]),
                              "camera.1.streamL.format":config_info["camera.1.streamL.format"],
                              "camera.1.streamL.framerate":parseInt(config_info["camera.1.streamL.framerate"]),
                              "camera.1.streamL.gopsize":parseInt(config_info["camera.1.streamL.gopsize"]),
                              "camera.1.streamL.storage.diskquota":parseFloat(config_info["camera.1.streamL.storage.diskquota"]),
                              "camera.1.streamL.storage.recordevents":config_info["camera.1.streamL.storage.recordevents"],
                              "camera.1.streamL.storage.recordmode":parseInt(config_info["camera.1.streamL.storage.recordmode"]),
                              "camera.1.streamL.storage.recordpostamble":parseInt(config_info["camera.1.streamL.storage.recordpostamble"]),
                              "camera.1.streamL.storage.recordpreamble":parseInt(config_info["camera.1.streamL.storage.recordpreamble"]),
                              "camera.1.wdrmode":config_info["camera.1.wdrmode"],
                              "evtgrab.capint":parseInt(config_info["evtgrab.capint"]),
                              "evtgrab.dint":parseInt(config_info["evtgrab.dint"]),
                              "evtgrab.nfiles":parseInt(config_info["evtgrab.nfiles"]),
                              "evtgrab.periodic_only":config_info["evtgrab.periodic_only"],
                              "mediaserver.nomve":config_info["mediaserver.nomve"],
                              "mediaserver.video_codec":config_info["mediaserver.video_codec"],
                              "mediaserver.wmm_qos":config_info["mediaserver.wmm_qos"],
                              "network.eth-x.ip":config_info["network.eth-x.ip"],
                              "network.eth-x.leasetime":config_info["network.eth-x.leasetime"],
                              "network.eth-x.method":config_info["network.eth-x.method"],
                              "network.eth-x.netmask":parseInt(config_info["network.eth-x.netmask"]),
                              "network.eth-x.port-forwarding.genetec":config_info["network.eth-x.port-forwarding.genetec"],
                              "network.eth-x.port-forwarding.https":config_info["network.eth-x.port-forwarding.https"],
                              "network.eth-x.port-forwarding.rtsp":config_info["network.eth-x.port-forwarding.rtsp"],
                              "network.eth-x.port-forwarding.ssh":config_info["network.eth-x.port-forwarding.ssh"],
                              "network.eth-x.whitelist":config_info["network.eth-x.whitelist"],
                              "network.firewall.ports":config_info["network.firewall.ports"],
                              "network.firewall.protocols":config_info["network.firewall.protocols"],
                              "network.region":config_info["network.region"],
                              "network.server.mqtt":config_info["network.server.mqtt"],
                              "network.server.vpn":config_info["network.server.vpn"],
                              "network.vpn_on_demand":config_info["network.vpn_on_demand"],
                              "network.wlan-x.security":config_info["network.wlan-x.security"],
                              "network.wlan-x.security.bgscan-long-interval":parseInt(config_info["network.wlan-x.security.bgscan-long-interval"]),
                              "network.wlan-x.security.bgscan-short-interval":parseInt(config_info["network.wlan-x.security.bgscan-short-interval"]),
                              "network.wlan-x.security.bgscan-signal-threshold":parseInt(config_info["network.wlan-x.security.bgscan-signal-threshold"]),
                              "network.wlan-x.security.psk":config_info["network.wlan-x.security.psk"],
                              "network.wlan-x.ssid":config_info["network.wlan-x.ssid"],
                              "network.wlan-y.security":config_info["network.wlan-y.security"],
                              "network.wlan-y.security.bgscan-long-interval":parseInt(config_info["network.wlan-y.security.bgscan-long-interval"]),
                              "network.wlan-y.security.bgscan-short-interval":parseInt(config_info["network.wlan-y.security.bgscan-short-interval"]),
                              "network.wlan-y.security.bgscan-signal-threshold":parseInt(config_info["network.wlan-y.security.bgscan-signal-threshold"]),
                              "network.wlan-y.security.psk":config_info["network.wlan-y.security.psk"],
                              "network.wlan-y.ssid":config_info["network.wlan-y.ssid"],
                              "rtsp.service":parseInt(config_info["rtsp.service"]),
                              "server":config_info.server,
                              "storage.maxsessions":parseInt(config_info["storage.maxsessions"]),
                              "storageserver.cache_size":parseInt(config_info["storageserver.cache_size"]),
                              "storageserver.max_clip_size":parseInt(config_info["storageserver.max_clip_size"]),
                            };

          }
          else if (config_info.model == "cnext"){
            configObject = {  "name": config_info.name + "_" + currentDateTime,
                              "model": config_info.model,
                              "alarm.redis.memory_threshold":parseFloat(config_info["alarm.redis.memory_threshold"]),
                              "application_server":config_info.application_server,
                              "application_server_port":parseInt(config_info.application_server_port),
                              "lctrl.dimmer.vmax":parseInt(config_info["lctrl.dimmer.vmax"]),
                              "lctrl.ltype":parseInt(config_info["lctrl.ltype"]),
                              "lctrl.lvlrate.fast":parseInt(config_info["lctrl.lvlrate.fast"]),
                              "lctrl.lvlrate.light":parseInt(config_info["lctrl.lvlrate.light"]),
                              "lctrl.lvlrate.motion":parseInt(config_info["lctrl.lvlrate.motion"]),
                              "lctrl.lvlrate.norm":parseInt(config_info["lctrl.lvlrate.norm"]),
                              "lctrl.trigger.nonett":parseInt(config_info["lctrl.trigger.nonett"]),
                              "lctrl.trigger.off.dint":parseInt(config_info["lctrl.trigger.off.dint"]),
                              "lctrl.trigger.offt":parseInt(config_info["lctrl.trigger.offt"]),
                              "lctrl.trigger.on.dint":parseInt(config_info["lctrl.trigger.on.dint"]),
                              "lctrl.trigger.ont":parseInt(config_info["lctrl.trigger.ont"]),
                              "network.eth-x.ip":config_info["network.eth-x.ip"],
                              "network.eth-x.leasetime":config_info["network.eth-x.leasetime"],
                              "network.eth-x.method":config_info["network.eth-x.method"],
                              "network.eth-x.netmask":parseInt(config_info["network.eth-x.netmask"]),
                              "network.firewall.ports":config_info["network.firewall.ports"],
                              "network.firewall.protocols":config_info["network.firewall.protocols"],
                              "network.ppp-x.apn":config_info["network.ppp-x.apn"],
                              "network.server.mqtt":config_info["network.server.mqtt"],
                              "network.server.vpn":config_info["network.server.vpn"],
                              "network.vpn_on_demand":config_info["network.vpn_on_demand"],
                              "network.wlan-x.security":config_info["network.wlan-x.security"],
                              "network.wlan-x.security.bgscan-long-interval":parseInt(config_info["network.wlan-x.security.bgscan-long-interval"]),
                              "network.wlan-x.security.bgscan-short-interval":parseInt(config_info["network.wlan-x.security.bgscan-short-interval"]),
                              "network.wlan-x.security.bgscan-signal-threshold":parseInt(config_info["network.wlan-x.security.bgscan-signal-threshold"]),
                              "network.wlan-x.security.psk":config_info["network.wlan-x.security.psk"],
                              "network.wlan-x.ssid":config_info["network.wlan-x.ssid"],
                              "network.wlan-y.security":config_info["network.wlan-y.security"],
                              "network.wlan-y.security.bgscan-long-interval":parseInt(config_info["network.wlan-y.security.bgscan-long-interval"]),
                              "network.wlan-y.security.bgscan-short-interval":parseInt(config_info["network.wlan-y.security.bgscan-short-interval"]),
                              "network.wlan-y.security.bgscan-signal-threshold":parseInt(config_info["network.wlan-y.security.bgscan-signal-threshold"]),
                              "network.wlan-y.security.psk":config_info["network.wlan-y.security.psk"],
                              "network.wlan-y.ssid":config_info["network.wlan-y.ssid"],
                              "podbus.disable":config_info["podbus.disable"],
                              "poe.enabled":config_info["poe.enabled"],
                              "pwr.noload.i":parseInt(config_info["pwr.noload.i"]),
                              "pwr.noload.mode":parseInt(config_info["pwr.noload.mode"]),
                              "server":config_info.server,
                            };

          }
          else if (config_info.model == "unode-v2"){
            configObject = {  "name": config_info.name + "_" + currentDateTime,
                              "model": config_info.model,
                              "configid":config_info.configid,
                              "networkXPasskey":config_info.networkXPasskey,
                              "networkXSSID":config_info.networkXSSID,
                              "networkXSecurity":config_info.networkXSecurity,
                              "networkYPasskey":config_info.networkYPasskey,
                              "networkYSSID":config_info.networkYSSID,
                              "networkYSecurity":config_info.networkYSecurity,
                              "network_region":config_info.network_region,
                              "debugmode":config_info.debugmode,
                              "telnet":config_info.telnet,
                              "sensor_rf_pint":config_info.sensor_rf_pint * 1000,
                              "sensor_rf_dint":config_info.sensor_rf_dint * 1000,
                              "sensor_rf_mode":config_info.sensor_rf_mode,
                              "sensor_v_pint":config_info.sensor_v_pint * 1000,
                              "sensor_v_dint":config_info.sensor_v_dint * 1000,
                              "sensor_v_mode":config_info.sensor_v_mode,
                              "sensor_lIR_pint":config_info.sensor_lIR_pint * 1000,
                              "sensor_lIR_dint":config_info.sensor_lIR_dint  * 1000,
                              "sensor_lIR_mode":config_info.sensor_lIR_mode,
                              "sensor_l_pint":config_info.sensor_l_pint * 1000,
                              "sensor_l_dint":config_info.sensor_l_dint * 1000,
                              "sensor_l_mode":config_info.sensor_l_mode,
                              "sensor_p_pint":config_info.sensor_p_pint * 1000,
                              "sensor_p_dint":config_info.sensor_p_dint * 1000,
                              "sensor_p_mode":config_info.sensor_p_mode,
                              "sensor_t_pint":config_info.sensor_t_pint * 1000,
                              "sensor_t_dint":config_info.sensor_t_dint * 1000,
                              "sensor_t_mode":config_info.sensor_t_mode,
                              "sensor_T_pint":config_info.sensor_T_pint * 1000,
                              "sensor_T_dint":config_info.sensor_T_dint * 1000,
                              "sensor_T_mode":config_info.sensor_T_mode,
                              "sensor_pdc_pint":config_info.sensor_pdc_pint * 1000,
                              "sensor_pdc_dint":config_info.sensor_pdc_dint * 1000,
                              "sensor_pdc_mode":config_info.sensor_pdc_mode,
                              "sensor_ppr_pint":config_info.sensor_ppr_pint * 1000,
                              "sensor_ppr_dint":config_info.sensor_ppr_dint * 1000,
                              "sensor_ppr_mode":config_info.sensor_ppr_mode,
                              "sensor_pnd_pint":config_info.sensor_pnd_pint * 1000,
                              "sensor_pnd_dint":config_info.sensor_pnd_dint * 1000,
                              "sensor_pnd_mode":config_info.sensor_pnd_mode,
                              "sensor_pdt_pint":config_info.sensor_pdt_pint * 1000,
                              "sensor_pdt_dint":config_info.sensor_pdt_dint * 1000,
                              "sensor_pdt_mode":config_info.sensor_pdt_mode,
                              "lctrl_dimmer_vmax":config_info.lctrl_dimmer_vmax,
                          };
           
          }
          else if (config_info.model == "unode-v3"){
            configObject = {  "name": config_info.name + "_" + currentDateTime,
                              "model": config_info.model,
                              "configid":config_info.configid,
                              "networkXPasskey":config_info.networkXPasskey,
                              "networkXSSID":config_info.networkXSSID,
                              "networkXSecurity":config_info.networkXSecurity,
                              "networkYPasskey":config_info.networkYPasskey,
                              "networkYSSID":config_info.networkYSSID,
                              "networkYSecurity":config_info.networkYSecurity,
                              "network_region":config_info.network_region,
                              "debugmode":config_info.debugmode,
                              "telnet":config_info.telnet,
                              "sensor_rf_pint":config_info.sensor_rf_pint * 1000,
                              "sensor_rf_dint":config_info.sensor_rf_dint * 1000,
                              "sensor_rf_mode":config_info.sensor_rf_mode,
                              "sensor_v_pint":config_info.sensor_v_pint * 1000,
                              "sensor_v_dint":config_info.sensor_v_dint * 1000,
                              "sensor_v_mode":config_info.sensor_v_mode,
                              "sensor_mi_pint":config_info.sensor_mi_pint * 1000,
                              "sensor_mi_dint":config_info.sensor_mi_dint * 1000,
                              "sensor_mi_mode":config_info.sensor_mi_mode,
                              "sensor_lIR_pint":config_info.sensor_lIR_pint * 1000,
                              "sensor_lIR_dint":config_info.sensor_lIR_dint  * 1000,
                              "sensor_lIR_mode":config_info.sensor_lIR_mode,
                              "sensor_l_pint":config_info.sensor_l_pint * 1000,
                              "sensor_l_dint":config_info.sensor_l_dint * 1000,
                              "sensor_l_mode":config_info.sensor_l_mode,
                              "sensor_p_pint":config_info.sensor_p_pint * 1000,
                              "sensor_p_dint":config_info.sensor_p_dint * 1000,
                              "sensor_p_mode":config_info.sensor_p_mode,
                              "sensor_t_pint":config_info.sensor_t_pint * 1000,
                              "sensor_t_dint":config_info.sensor_t_dint * 1000,
                              "sensor_t_mode":config_info.sensor_t_mode,
                              "sensor_T_pint":config_info.sensor_T_pint * 1000,
                              "sensor_T_dint":config_info.sensor_T_dint * 1000,
                              "sensor_T_mode":config_info.sensor_T_mode,
                              "sensor_pdc_pint":config_info.sensor_pdc_pint * 1000,
                              "sensor_pdc_dint":config_info.sensor_pdc_dint * 1000,
                              "sensor_pdc_mode":config_info.sensor_pdc_mode,
                              "sensor_ppr_pint":config_info.sensor_ppr_pint * 1000,
                              "sensor_ppr_dint":config_info.sensor_ppr_dint * 1000,
                              "sensor_ppr_mode":config_info.sensor_ppr_mode,
                              "sensor_pnd_pint":config_info.sensor_pnd_pint * 1000,
                              "sensor_pnd_dint":config_info.sensor_pnd_dint * 1000,
                              "sensor_pnd_mode":config_info.sensor_pnd_mode,
                              "sensor_pdt_pint":config_info.sensor_pdt_pint * 1000,
                              "sensor_pdt_dint":config_info.sensor_pdt_dint * 1000,
                              "sensor_pdt_mode":config_info.sensor_pdt_mode,
                              "sensor_a_jdelta":parseInt(config_info.sensor_a_jdelta),
                              "lctrl_dimmer_vmax":config_info.lctrl_dimmer_vmax,
                          };
           
          }
          else if (config_info.model == "unode-v4"){
            configObject = {  "name": config_info.name + "_" + currentDateTime,
                              "model": config_info.model,
                              "configid":config_info.configid,
                              "networkXPasskey":config_info.networkXPasskey,
                              "networkXSSID":config_info.networkXSSID,
                              "networkXSecurity":config_info.networkXSecurity,
                              "networkYPasskey":config_info.networkYPasskey,
                              "networkYSSID":config_info.networkYSSID,
                              "networkYSecurity":config_info.networkYSecurity,
                              "network_region":config_info.network_region,
                              "debugmode":config_info.debugmode,
                              "telnet":config_info.telnet,
                              "aux_power":config_info.aux_power,
                              "podbus_disable":config_info.podbus_disable,
                              "sensor_rf_pint":config_info.sensor_rf_pint * 1000,
                              "sensor_rf_dint":config_info.sensor_rf_dint * 1000,
                              "sensor_rf_mode":config_info.sensor_rf_mode,
                              "sensor_v_pint":config_info.sensor_v_pint * 1000,
                              "sensor_v_dint":config_info.sensor_v_dint * 1000,
                              "sensor_v_mode":config_info.sensor_v_mode,
                              "sensor_aw_pint":config_info.sensor_aw_pint * 1000,
                              "sensor_aw_dint":config_info.sensor_aw_dint * 1000,
                              "sensor_aw_mode":config_info.sensor_aw_mode,
                              "sensor_aPF_pint":config_info.sensor_aPF_pint * 1000,
                              "sensor_aPF_dint":config_info.sensor_aPF_dint * 1000,
                              "sensor_aPF_mode":config_info.sensor_aPF_mode,
                              "sensor_aP_pint":config_info.sensor_aP_pint * 1000,
                              "sensor_aP_dint":config_info.sensor_aP_dint * 1000,
                              "sensor_aP_mode":config_info.sensor_aP_mode,
                              "sensor_mP_pint":config_info.sensor_mP_pint * 1000,
                              "sensor_mP_dint":config_info.sensor_mP_dint * 1000,
                              "sensor_mP_mode":config_info.sensor_mP_mode,
                              "sensor_mip_pint":config_info.sensor_mip_pint * 1000,
                              "sensor_mip_dint":config_info.sensor_mip_dint * 1000,
                              "sensor_mip_mode":config_info.sensor_mip_mode,
                              "sensor_ai_pint":config_info.sensor_ai_pint * 1000,
                              "sensor_ai_dint":config_info.sensor_ai_dint * 1000,
                              "sensor_ai_mode":config_info.sensor_ai_mode,
                              "sensor_aip_pint":config_info.sensor_aip_pint * 1000,
                              "sensor_aip_dint":config_info.sensor_aip_dint * 1000,
                              "sensor_aip_mode":config_info.sensor_aip_mode,
                              "sensor_mi_pint":config_info.sensor_mi_pint * 1000,
                              "sensor_mi_dint":config_info.sensor_mi_dint * 1000,
                              "sensor_mi_mode":config_info.sensor_mi_mode,
                              "sensor_mw_pint":config_info.sensor_mw_pint * 1000,
                              "sensor_mw_dint":config_info.sensor_mw_dint * 1000,
                              "sensor_mw_mode":config_info.sensor_mw_mode,
                              "sensor_mPF_pint":config_info.sensor_mPF_pint * 1000,
                              "sensor_mPF_dint":config_info.sensor_mPF_dint * 1000,
                              "sensor_mPF_mode":config_info.sensor_mPF_mode,
                              "sensor_lIR_pint":config_info.sensor_lIR_pint * 1000,
                              "sensor_lIR_dint":config_info.sensor_lIR_dint  * 1000,
                              "sensor_lIR_mode":config_info.sensor_lIR_mode,
                              "sensor_l_pint":config_info.sensor_l_pint * 1000,
                              "sensor_l_dint":config_info.sensor_l_dint * 1000,
                              "sensor_l_mode":config_info.sensor_l_mode,
                              "sensor_p_pint":config_info.sensor_p_pint * 1000,
                              "sensor_p_dint":config_info.sensor_p_dint * 1000,
                              "sensor_p_mode":config_info.sensor_p_mode,
                              "sensor_a_jdelta":parseInt(config_info.sensor_a_jdelta),
                              "sensor_pc_pint":config_info.sensor_pc_pint * 1000,
                              "sensor_pc_dint":config_info.sensor_pc_dint * 1000,
                              "sensor_pc_mode":config_info.sensor_pc_mode,
                              "sensor_t_pint":config_info.sensor_t_pint * 1000,
                              "sensor_t_dint":config_info.sensor_t_dint * 1000,
                              "sensor_t_mode":config_info.sensor_t_mode,
                              "sensor_T_pint":config_info.sensor_T_pint * 1000,
                              "sensor_T_dint":config_info.sensor_T_dint * 1000,
                              "sensor_T_mode":config_info.sensor_T_mode,
                              "sensor_mt_pint":config_info.sensor_mt_pint * 1000,
                              "sensor_mt_dint":config_info.sensor_mt_dint * 1000,
                              "sensor_mt_mode":config_info.sensor_mt_mode,
                              "sensor_podm_pint":config_info.sensor_podm_pint * 1000,
                              "sensor_podm_dint":config_info.sensor_podm_dint * 1000,
                              "sensor_podm_mode":config_info.sensor_podm_mode,
                              "sensor_podme_pint":config_info.sensor_podme_pint * 1000,
                              "sensor_podme_dint":config_info.sensor_podme_dint * 1000,
                              "sensor_podme_mode":config_info.sensor_podme_mode,
                              "sensor_rlym_pint":config_info.sensor_rlym_pint * 1000,
                              "lctrl_dimmer_vmax":config_info.lctrl_dimmer_vmax,
                              "lctrl_trigger_off_dint":config_info.lctrl_trigger_off_dint * 1000,
                              "lctrl_trigger_on_dint":config_info.lctrl_trigger_on_dint * 1000,
                              "lctrl_trigger_ont":config_info.lctrl_trigger_ont * 1000,
                              "lctrl_trigger_offt":config_info.lctrl_trigger_offt * 1000,
                              "lctrl_trigger_nonett":config_info.lctrl_trigger_nonett * 1000,
                          };

          }
          else if (config_info.model == "unode-v5"){
            configObject = {  "name": config_info.name + "_" + currentDateTime,
                              "model": config_info.model,
                              "configid":config_info.configid,
                              "network_region":config_info.network_region,
                              "debugmode":config_info.debugmode,
                              "telnet":config_info.telnet,
                              "aux_power":config_info.aux_power,
                              "podbus_disable":config_info.podbus_disable,
                              "sensor_rf_pint":config_info.sensor_rf_pint * 1000,
                              "sensor_rf_dint":config_info.sensor_rf_dint * 1000,
                              "sensor_rf_mode":config_info.sensor_rf_mode,
                              "sensor_v_pint":config_info.sensor_v_pint * 1000,
                              "sensor_v_dint":config_info.sensor_v_dint * 1000,
                              "sensor_v_mode":config_info.sensor_v_mode,
                              "sensor_aw_pint":config_info.sensor_aw_pint * 1000,
                              "sensor_aw_dint":config_info.sensor_aw_dint * 1000,
                              "sensor_aw_mode":config_info.sensor_aw_mode,
                              "sensor_aPF_pint":config_info.sensor_aPF_pint * 1000,
                              "sensor_aPF_dint":config_info.sensor_aPF_dint * 1000,
                              "sensor_aPF_mode":config_info.sensor_aPF_mode,
                              "sensor_aP_pint":config_info.sensor_aP_pint * 1000,
                              "sensor_aP_dint":config_info.sensor_aP_dint * 1000,
                              "sensor_aP_mode":config_info.sensor_aP_mode,
                              "sensor_mP_pint":config_info.sensor_mP_pint * 1000,
                              "sensor_mP_dint":config_info.sensor_mP_dint * 1000,
                              "sensor_mP_mode":config_info.sensor_mP_mode,
                              "sensor_mip_pint":config_info.sensor_mip_pint * 1000,
                              "sensor_mip_dint":config_info.sensor_mip_dint * 1000,
                              "sensor_mip_mode":config_info.sensor_mip_mode,
                              "sensor_ai_pint":config_info.sensor_ai_pint * 1000,
                              "sensor_ai_dint":config_info.sensor_ai_dint * 1000,
                              "sensor_ai_mode":config_info.sensor_ai_mode,
                              "sensor_aip_pint":config_info.sensor_aip_pint * 1000,
                              "sensor_aip_dint":config_info.sensor_aip_dint * 1000,
                              "sensor_aip_mode":config_info.sensor_aip_mode,
                              "sensor_mi_pint":config_info.sensor_mi_pint * 1000,
                              "sensor_mi_dint":config_info.sensor_mi_dint * 1000,
                              "sensor_mi_mode":config_info.sensor_mi_mode,
                              "sensor_mw_pint":config_info.sensor_mw_pint * 1000,
                              "sensor_mw_dint":config_info.sensor_mw_dint * 1000,
                              "sensor_mw_mode":config_info.sensor_mw_mode,
                              "sensor_mPF_pint":config_info.sensor_mPF_pint * 1000,
                              "sensor_mPF_dint":config_info.sensor_mPF_dint * 1000,
                              "sensor_mPF_mode":config_info.sensor_mPF_mode,
                              "sensor_lIR_pint":config_info.sensor_lIR_pint * 1000,
                              "sensor_lIR_dint":config_info.sensor_lIR_dint  * 1000,
                              "sensor_lIR_mode":config_info.sensor_lIR_mode,
                              "sensor_l_pint":config_info.sensor_l_pint * 1000,
                              "sensor_l_dint":config_info.sensor_l_dint * 1000,
                              "sensor_l_mode":config_info.sensor_l_mode,
                              "sensor_l_i_pint":config_info.sensor_l_i_pint * 1000,
                              "sensor_l_i_dint":config_info.sensor_l_i_dint * 1000,
                              "sensor_l_i_mode":config_info.sensor_l_i_mode,
                              "sensor_lIR_i_pint":config_info.sensor_lIR_i_pint * 1000,
                              "sensor_lIR_i_dint":config_info.sensor_lIR_i_dint * 1000,
                              "sensor_lIR_i_mode":config_info.sensor_lIR_i_mode,
                              "sensor_p_pint":config_info.sensor_p_pint * 1000,
                              "sensor_p_dint":config_info.sensor_p_dint * 1000,
                              "sensor_p_mode":config_info.sensor_p_mode,
                              "sensor_pc_pint":config_info.sensor_pc_pint * 1000,
                              "sensor_pc_dint":config_info.sensor_pc_dint * 1000,
                              "sensor_pc_mode":config_info.sensor_pc_mode,
                              "sensor_t_pint":config_info.sensor_t_pint * 1000,
                              "sensor_t_dint":config_info.sensor_t_dint * 1000,
                              "sensor_t_mode":config_info.sensor_t_mode,
                              "sensor_T_pint":config_info.sensor_T_pint * 1000,
                              "sensor_T_dint":config_info.sensor_T_dint * 1000,
                              "sensor_T_mode":config_info.sensor_T_mode,
                              "sensor_mt_pint":config_info.sensor_mt_pint * 1000,
                              "sensor_mt_dint":config_info.sensor_mt_dint * 1000,
                              "sensor_mt_mode":config_info.sensor_mt_mode,
                              "sensor_podm_pint":config_info.sensor_podm_pint * 1000,
                              "sensor_podm_dint":config_info.sensor_podm_dint * 1000,
                              "sensor_podm_mode":config_info.sensor_podm_mode,
                              "sensor_podme_pint":config_info.sensor_podme_pint * 1000,
                              "sensor_podme_dint":config_info.sensor_podme_dint * 1000,
                              "sensor_podme_mode":config_info.sensor_podme_mode,
                              "sensor_rlym_pint":config_info.sensor_rlym_pint * 1000,
                              "lctrl_dimmer_vmax":config_info.lctrl_dimmer_vmax,
                              "lctrl_trigger_off_dint":config_info.lctrl_trigger_off_dint * 1000,
                              "lctrl_trigger_on_dint":config_info.lctrl_trigger_on_dint * 1000,
                              "lctrl_trigger_ont":config_info.lctrl_trigger_ont * 1000,
                              "lctrl_trigger_offt":config_info.lctrl_trigger_offt * 1000,
                              "lctrl_trigger_nonett":config_info.lctrl_trigger_nonett * 1000,
                          };

          }
          else {
            configObject = {  "name": config_info.name + "_" + currentDateTime,
                              "model": config_info.model,
                              "configid":config_info.configid,
                              "network_region":config_info.network_region,
                              "debugmode":config_info.debugmode,
                              "telnet":config_info.telnet,
                              "aux_power":config_info.aux_power,
                              "podbus_disable":config_info.podbus_disable,
                              "sensor_rf_pint":config_info.sensor_rf_pint * 1000,
                              "sensor_rf_dint":config_info.sensor_rf_dint * 1000,
                              "sensor_rf_mode":config_info.sensor_rf_mode,
                              "sensor_v_pint":config_info.sensor_v_pint * 1000,
                              "sensor_v_dint":config_info.sensor_v_dint * 1000,
                              "sensor_v_mode":config_info.sensor_v_mode,
                              "sensor_aw_pint":config_info.sensor_aw_pint * 1000,
                              "sensor_aw_dint":config_info.sensor_aw_dint * 1000,
                              "sensor_aw_mode":config_info.sensor_aw_mode,
                              "sensor_aPF_pint":config_info.sensor_aPF_pint * 1000,
                              "sensor_aPF_dint":config_info.sensor_aPF_dint * 1000,
                              "sensor_aPF_mode":config_info.sensor_aPF_mode,
                              "sensor_aP_pint":config_info.sensor_aP_pint * 1000,
                              "sensor_aP_dint":config_info.sensor_aP_dint * 1000,
                              "sensor_aP_mode":config_info.sensor_aP_mode,
                              "sensor_mP_pint":config_info.sensor_mP_pint * 1000,
                              "sensor_mP_dint":config_info.sensor_mP_dint * 1000,
                              "sensor_mP_mode":config_info.sensor_mP_mode,
                              "sensor_mip_pint":config_info.sensor_mip_pint * 1000,
                              "sensor_mip_dint":config_info.sensor_mip_dint * 1000,
                              "sensor_mip_mode":config_info.sensor_mip_mode,
                              "sensor_ai_pint":config_info.sensor_ai_pint * 1000,
                              "sensor_ai_dint":config_info.sensor_ai_dint * 1000,
                              "sensor_ai_mode":config_info.sensor_ai_mode,
                              "sensor_aip_pint":config_info.sensor_aip_pint * 1000,
                              "sensor_aip_dint":config_info.sensor_aip_dint * 1000,
                              "sensor_aip_mode":config_info.sensor_aip_mode,
                              "sensor_mi_pint":config_info.sensor_mi_pint * 1000,
                              "sensor_mi_dint":config_info.sensor_mi_dint * 1000,
                              "sensor_mi_mode":config_info.sensor_mi_mode,
                              "sensor_mw_pint":config_info.sensor_mw_pint * 1000,
                              "sensor_mw_dint":config_info.sensor_mw_dint * 1000,
                              "sensor_mw_mode":config_info.sensor_mw_mode,
                              "sensor_mPF_pint":config_info.sensor_mPF_pint * 1000,
                              "sensor_mPF_dint":config_info.sensor_mPF_dint * 1000,
                              "sensor_mPF_mode":config_info.sensor_mPF_mode,
                              "sensor_lIR_pint":config_info.sensor_lIR_pint * 1000,
                              "sensor_lIR_dint":config_info.sensor_lIR_dint  * 1000,
                              "sensor_lIR_mode":config_info.sensor_lIR_mode,
                              "sensor_l_pint":config_info.sensor_l_pint * 1000,
                              "sensor_l_dint":config_info.sensor_l_dint * 1000,
                              "sensor_l_mode":config_info.sensor_l_mode,
                              "sensor_l_i_pint":config_info.sensor_l_i_pint * 1000,
                              "sensor_l_i_dint":config_info.sensor_l_i_dint * 1000,
                              "sensor_l_i_mode":config_info.sensor_l_i_mode,
                              "sensor_lIR_i_pint":config_info.sensor_lIR_i_pint * 1000,
                              "sensor_lIR_i_dint":config_info.sensor_lIR_i_dint * 1000,
                              "sensor_lIR_i_mode":config_info.sensor_lIR_i_mode,
                              "sensor_p_pint":config_info.sensor_p_pint * 1000,
                              "sensor_p_dint":config_info.sensor_p_dint * 1000,
                              "sensor_p_mode":config_info.sensor_p_mode,
                              "sensor_pc_pint":config_info.sensor_pc_pint * 1000,
                              "sensor_pc_dint":config_info.sensor_pc_dint * 1000,
                              "sensor_pc_mode":config_info.sensor_pc_mode,
                              "sensor_t_pint":config_info.sensor_t_pint * 1000,
                              "sensor_t_dint":config_info.sensor_t_dint * 1000,
                              "sensor_t_mode":config_info.sensor_t_mode,
                              "sensor_T_pint":config_info.sensor_T_pint * 1000,
                              "sensor_T_dint":config_info.sensor_T_dint * 1000,
                              "sensor_T_mode":config_info.sensor_T_mode,
                              "sensor_mt_pint":config_info.sensor_mt_pint * 1000,
                              "sensor_mt_dint":config_info.sensor_mt_dint * 1000,
                              "sensor_mt_mode":config_info.sensor_mt_mode,
                              "sensor_podm_pint":config_info.sensor_podm_pint * 1000,
                              "sensor_podm_dint":config_info.sensor_podm_dint * 1000,
                              "sensor_podm_mode":config_info.sensor_podm_mode,
                              "sensor_podme_pint":config_info.sensor_podme_pint * 1000,
                              "sensor_podme_dint":config_info.sensor_podme_dint * 1000,
                              "sensor_podme_mode":config_info.sensor_podme_mode,
                              "sensor_rlym_pint":config_info.sensor_rlym_pint * 1000,
                              "lctrl_dimmer_vmax":config_info.lctrl_dimmer_vmax,
                              "lctrl_trigger_off_dint":config_info.lctrl_trigger_off_dint * 1000,
                              "lctrl_trigger_on_dint":config_info.lctrl_trigger_on_dint * 1000,
                              "lctrl_trigger_ont":config_info.lctrl_trigger_ont * 1000,
                              "lctrl_trigger_offt":config_info.lctrl_trigger_offt * 1000,
                              "lctrl_trigger_nonett":config_info.lctrl_trigger_nonett * 1000,
                          };

          }


      $.ajax({
        "url": NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/configs/' + config_info.configid,
        "type": "POST",
        "xhrFields": {
          withCredentials: true
        },
        "data": JSON.stringify(configObject),
        "dataType": "json",
        "contentType": "application/json",
        "processData": false,
        "success": function (data) {
          data.idx = idx;
          var newState = React.addons.update(this.state, { configs: { [idx]: { $set: data } } });
          noty({ type: "success", text: 'Config "' + data.name + '" updated.' })

          var payload = {
            nodeList: assignmentToNodes
          }

          $.ajax({
            url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/configs/' + data.configid + '/apply/nodes',
            type: 'POST',
            xhrFields: {
              withCredentials: true
            },
            data: JSON.stringify(payload),
            dataType: 'json',
            contentType: 'application/json',
            success: function (data) {
              console.log('Updating config for selected nodes: ' + JSON.stringify(data));

              if(data.lctrl_dimmer_vmax){
                data.lctrl_dimmer_vmax = selected_lctrl_dimmer_vmax;
              }
              if(data.networkXSecurity){
                data.networkXSecurity = selected_networkXSecurity;
              }
              if(data.networkYSecurity){
                data.networkYSecurity = selected_networkYSecurity;
              }

              if(data["camera.0.streamH.storage.recordmode"]){
                data["camera.0.streamH.storage.recordmode"] = selected_camera0streamHstoragerecordmode;
              }

              if(data["camera.0.streamL.storage.recordmode"]){
                data["camera.0.streamL.storage.recordmode"] = selected_camera0streamLstoragerecordmode;
              }

              if(data["camera.1.streamH.storage.recordmode"]){
                data["camera.1.streamH.storage.recordmode"] = selected_camera1streamHstoragerecordmode;
              }

              if(data["camera.1.streamL.storage.recordmode"]){
                data["camera.1.streamL.storage.recordmode"] = selected_camera1streamLstoragerecordmode;
              }

              ReactBootstrap.Dispatcher.emit('Configform.update', 'close');
              noty({
                type: 'success',
                text: 'Updated config assigned to Nodes "' + assignmentToNodes + '"'
              });


            },
            error: function () {
              noty({
                type: 'error',
                text: 'Could not assign updated config to Nodes "' + assignmentToNodes + '"'
              });
            }
          })
          this.setState(newState);
        }.bind(that),
        "error": function () {
          noty({ type: "error", text: 'Could not update config.' });
        }
      }); 

    });


    ReactBootstrap.Dispatcher.on("Configform.save", function (config_info) {
      var newState = {};
      var configObject = {};
      console.log("config_info.save", config_info);

      console.log(config_info.configid, that.state.duplicate);

      var assignment = {
        assign: config_info.assign,
        assigngroups: config_info.assigngroups.splice(0)
      };
      console.log(JSON.stringify(assignment));
      delete config_info.assign;
      delete config_info.assigngroups;
      var save_sites = config_info.sites.splice(0);
      delete config_info.sites;
      var save_groups = config_info.groups.splice(0);
      delete config_info.groups;
      var save_nodes = config_info.nodes.splice(0);
      delete config_info.nodes;
      
      if (config_info.lctrl_dimmer_vmax) {
        var selected_lctrl_dimmer_vmaxid = $("#lctrl_dimmer_vmax").val();
        var selected_lctrl_dimmer_vmax = $("#lctrl_dimmer_vmax").text();
        config_info.lctrl_dimmer_vmax = parseInt(selected_lctrl_dimmer_vmaxid);
      }

      if (config_info.networkXSecurity) {
        var selected_networkXsecurityid = $("#networkXSecurity").val();
        var selected_networkXSecurity = $("#networkXSecurity").text();
        config_info.networkXSecurity = selected_networkXsecurityid;
      }
      
      if (config_info.networkYSecurity) {
        var selected_networkYsecurityid = $("#networkYSecurity").val();
        var selected_networkYSecurity = $("#networkYSecurity").text();
        config_info.networkYSecurity = selected_networkYsecurityid;
      }

   
      if (typeof config_info["camera.0.streamH.storage.recordmode"] != "undefined") {
        if(config_info["camera.0.streamH.storage.recordmode"] == ""){
          config_info["camera.0.streamH.storage.recordmode"] = parseInt(0);
        }
        else{
          var selected_camera0streamHstoragerecordmodeid = $("#camera\\.0\\.streamH\\.storage\\.recordmode").val();
          var selected_camera0streamHstoragerecordmode = $("#camera\\.0\\.streamH\\.storage\\.recordmode").text();
          config_info["camera.0.streamH.storage.recordmode"] = parseInt(selected_camera0streamHstoragerecordmodeid);
        }
      } 

      if (typeof config_info["camera.0.streamL.storage.recordmode"] != "undefined") {
        if(config_info["camera.0.streamL.storage.recordmode"] == ""){
          config_info["camera.0.streamL.storage.recordmode"] = parseInt(0);
        }
        else{
          var selected_camera0streamLstoragerecordmodeid = $("#camera\\.0\\.streamL\\.storage\\.recordmode").val();
          var selected_camera0streamLstoragerecordmode = $("#camera\\.0\\.streamL\\.storage\\.recordmode").text();
          config_info["camera.0.streamL.storage.recordmode"] = parseInt(selected_camera0streamLstoragerecordmodeid);
        }
      } 

      if (typeof config_info["camera.1.streamH.storage.recordmode"] != "undefined") {
        if(config_info["camera.1.streamH.storage.recordmode"] == ""){
          config_info["camera.1.streamH.storage.recordmode"] = parseInt(0);
        }
        else{
          var selected_camera1streamHstoragerecordmodeid = $("#camera\\.1\\.streamH\\.storage\\.recordmode").val();
          var selected_camera1streamHstoragerecordmode = $("#camera\\.1\\.streamH\\.storage\\.recordmode").text();
          config_info["camera.1.streamH.storage.recordmode"] = parseInt(selected_camera1streamHstoragerecordmodeid);
        }
      } 

      if (typeof config_info["camera.1.streamL.storage.recordmode"] != "undefined") {
        if(config_info["camera.1.streamL.storage.recordmode"] == ""){
          config_info["camera.1.streamL.storage.recordmode"] = parseInt(0);
        }
        else{
          var selected_camera1streamLstoragerecordmodeid = $("#camera\\.1\\.streamL\\.storage\\.recordmode").val();
          var selected_camera1streamLstoragerecordmode = $("#camera\\.1\\.streamL\\.storage\\.recordmode").text();
          config_info["camera.1.streamL.storage.recordmode"] = parseInt(selected_camera1streamLstoragerecordmodeid);
        }
      } 

      if ((typeof config_info["camera.0.scanmode"] != "undefined") && (config_info["camera.0.scanmode"] == ""))  {
        config_info["camera.0.scanmode"] = "none";
      }

      if ((typeof config_info["camera.1.scanmode"] != "undefined") && (config_info["camera.1.scanmode"] == ""))  {
        config_info["camera.1.scanmode"] = "none";
      }
     

       if ((typeof config_info.configid == "undefined") || (config_info.configid == "") || (config_info.configid == "default") || (that.state.duplicate)) {
         if(!that.checkDuplicate(config_info)){
        console.log("Adding config: " + config_info.name);
        delete config_info.idx;
        delete config_info.configid;
          if(config_info.model == "vdkmaster"){

            configObject = {  "name": config_info.name,
                              "model": config_info.model,
                              "siteid": config_info.siteid,
                              "al.left.setbrightness":parseInt(config_info["al.left.setbrightness"]),
                              "al.left.setmode":config_info["al.left.setmode"],
                              "al.right.setbrightness":parseInt(config_info["al.right.setbrightness"]),
                              "al.right.setmode":config_info["al.right.setmode"],
                              "alarm.redis.memory_threshold":parseFloat(config_info["alarm.redis.memory_threshold"]),
                              "application_server":config_info.application_server,
                              "application_server_port":parseInt(config_info.application_server_port),
                              "appos.timezone":config_info["appos.timezone"],
                              "glassl.display.blue.gain":parseInt(config_info["glassl.display.blue.gain"]),
                              "glassl.display.blue.offset":parseInt(config_info["glassl.display.blue.offset"]),
                              "glassl.display.brightness":parseInt(config_info["glassl.display.brightness"]),
                              "glassl.display.color-temp":parseInt(config_info["glassl.display.color-temp"]),
                              "glassl.display.contrast":parseInt(config_info["glassl.display.contrast"]),
                              "glassl.display.green.gain":parseInt(config_info["glassl.display.green.gain"]),
                              "glassl.display.green.offset":parseInt(config_info["glassl.display.green.offset"]),
                              "glassl.display.hue":parseInt(config_info["glassl.display.hue"]),
                              "glassl.display.mode.ccs":config_info["glassl.display.mode.ccs"],
                              "glassl.display.mode.dynamic-nr":config_info["glassl.display.mode.dynamic-nr"],
                              "glassl.display.mode.main-dcdi":config_info["glassl.display.mode.main-dcdi"],
                              "glassl.display.mode.main-madi":config_info["glassl.display.mode.main-madi"],
                              "glassl.display.mode.mpeg-nr":config_info["glassl.display.mode.mpeg-nr"],
                              "glassl.display.mode.selection-1080p":config_info["glassl.display.mode.selection-1080p"],
                              "glassl.display.mode.selection-720p":config_info["glassl.display.mode.selection-720p"],
                              "glassl.display.mpeg-nr":parseInt(config_info["glassl.display.mpeg-nr"]),
                              "glassl.display.power":config_info["glassl.display.power"],
                              "glassl.display.red.gain":parseInt(config_info["glassl.display.red.gain"]),
                              "glassl.display.red.offset":parseInt(config_info["glassl.display.red.offset"]),
                              "glassl.display.saturation":parseInt(config_info["glassl.display.saturation"]),
                              "glassl.display.sharpness":parseInt(config_info["glassl.display.sharpness"]),
                              "glassl.display.temp.cold":parseInt(config_info["glassl.display.temp.cold"]),
                              "glassl.display.temp.hot":parseInt(config_info["glassl.display.temp.hot"]),
                              "glassl.display.temp.warm":parseInt(config_info["glassl.display.temp.warm"]),
                              "glassr.display.blue.gain":parseInt(config_info["glassr.display.blue.gain"]),
                              "glassr.display.blue.offset":parseInt(config_info["glassr.display.blue.offset"]),
                              "glassr.display.brightness":parseInt(config_info["glassr.display.brightness"]),
                              "glassr.display.color-temp":parseInt(config_info["glassr.display.color-temp"]),
                              "glassr.display.contrast":parseInt(config_info["glassr.display.contrast"]),
                              "glassr.display.green.gain":parseInt(config_info["glassr.display.green.gain"]),
                              "glassr.display.green.offset":parseInt(config_info["glassr.display.green.offset"]),
                              "glassr.display.hue":parseInt(config_info["glassr.display.hue"]),
                              "glassr.display.mode.ccs":config_info["glassr.display.mode.ccs"],
                              "glassr.display.mode.dynamic-nr":config_info["glassr.display.mode.dynamic-nr"],
                              "glassr.display.mode.main-dcdi":config_info["glassr.display.mode.main-dcdi"],
                              "glassr.display.mode.main-madi":config_info["glassr.display.mode.main-madi"],
                              "glassr.display.mode.mpeg-nr":config_info["glassr.display.mode.mpeg-nr"],
                              "glassr.display.mode.selection-1080p":config_info["glassr.display.mode.selection-1080p"],
                              "glassr.display.mode.selection-720p":config_info["glassr.display.mode.selection-720p"],
                              "glassr.display.mpeg-nr":parseInt(config_info["glassr.display.mpeg-nr"]),
                              "glassr.display.power":config_info["glassr.display.power"],
                              "glassr.display.red.gain":parseInt(config_info["glassr.display.red.gain"]),
                              "glassr.display.red.offset":parseInt(config_info["glassr.display.red.offset"]),
                              "glassr.display.saturation":parseInt(config_info["glassr.display.saturation"]),
                              "glassr.display.sharpness":parseInt(config_info["glassr.display.sharpness"]),
                              "glassr.display.temp.cold":parseInt(config_info["glassr.display.temp.cold"]),
                              "glassr.display.temp.hot":parseInt(config_info["glassr.display.temp.hot"]),
                              "glassr.display.temp.warm":parseInt(config_info["glassr.display.temp.warm"]),
                              "lr.setcolor":config_info["lr.setcolor"],
                              "network.eth-x.ip":config_info["network.eth-x.ip"],
                              "network.eth-x.method":config_info["network.eth-x.method"],
                              "network.eth-x.netmask":parseInt(config_info["network.eth-x.netmask"]),
                              "network.eth-x.port-forwarding.genetec":config_info["network.eth-x.port-forwarding.genetec"],
                              "network.eth-x.port-forwarding.https":config_info["network.eth-x.port-forwarding.https"],
                              "network.eth-x.port-forwarding.rtsp":config_info["network.eth-x.port-forwarding.rtsp"],
                              "network.eth-x.port-forwarding.ssh":config_info["network.eth-x.port-forwarding.ssh"],
                              "network.eth-x.whitelist":config_info["network.eth-x.whitelist"],
                              "network.firewall.ports":config_info["network.firewall.ports"],
                              "network.firewall.protocols":config_info["network.firewall.protocols"],
                              "network.server.mqtt":config_info["network.server.mqtt"],
                              "network.server.vpn":config_info["network.server.vpn"],
                              "network.vpn_on_demand":config_info["network.vpn_on_demand"],
                              "pebble.display.blue.gain":parseInt(config_info["pebble.display.blue.gain"]),
                              "pebble.display.blue.offset":parseInt(config_info["pebble.display.blue.offset"]),
                              "pebble.display.brightness":parseInt(config_info["pebble.display.brightness"]),
                              "pebble.display.color-temp":parseInt(config_info["pebble.display.color-temp"]),
                              "pebble.display.contrast":parseInt(config_info["pebble.display.contrast"]),
                              "pebble.display.green.gain":parseInt(config_info["pebble.display.green.gain"]),
                              "pebble.display.green.offset":parseInt(config_info["pebble.display.green.offset"]),
                              "pebble.display.hue":parseInt(config_info["pebble.display.hue"]),
                              "pebble.display.mode.ccs":config_info["pebble.display.mode.ccs"],
                              "pebble.display.mode.dynamic-nr":config_info["pebble.display.mode.dynamic-nr"],
                              "pebble.display.mode.main-dcdi":config_info["pebble.display.mode.main-dcdi"],
                              "pebble.display.mode.main-madi":config_info["pebble.display.mode.main-madi"],
                              "pebble.display.mode.mpeg-nr":config_info["pebble.display.mode.mpeg-nr"],
                              "pebble.display.mode.selection-1080p":config_info["pebble.display.mode.selection-1080p"],
                              "pebble.display.mode.selection-720p":config_info["pebble.display.mode.selection-720p"],
                              "pebble.display.mpeg-nr":parseInt(config_info["pebble.display.mpeg-nr"]),
                              "pebble.display.power":config_info["pebble.display.power"],
                              "pebble.display.red.gain":config_info["pebble.display.red.gain"],
                              "pebble.display.red.offset":parseInt(config_info["pebble.display.red.offset"]),
                              "pebble.display.saturation":parseInt(config_info["pebble.display.saturation"]),
                              "pebble.display.sharpness":parseInt(config_info["pebble.display.sharpness"]),
                              "pebble.display.temp.cold":parseInt(config_info["pebble.display.temp.cold"]),
                              "pebble.display.temp.hot":parseInt(config_info["pebble.display.temp.hot"]),
                              "pebble.display.temp.warm":parseInt(config_info["pebble.display.temp.warm"]),
                              "server":config_info.server,
                            };
          }

          else if (config_info.model == "merlin"){

             configObject = {  "name": config_info.name,
                              "model": config_info.model,
                              "Genetec.custom_events":config_info["Genetec.custom_events"],
                              "alarm.redis.memory_threshold":parseFloat(config_info["alarm.redis.memory_threshold"]),
                              "application_server":config_info.application_server,
                              "application_server_port":parseInt(config_info.application_server_port),
                              "camera.0.binning":config_info["camera.0.binning"],
                              "camera.0.enabled":config_info["camera.0.enabled"],
                              "camera.0.exposure":parseInt(config_info["camera.0.exposure"]),
                              "camera.0.gain":parseInt(config_info["camera.0.gain"]),
                              "camera.0.master":parseInt(config_info["camera.0.master"]),
                              "camera.0.streamH.bitrate":parseInt(config_info["camera.0.streamH.bitrate"]),
                              "camera.0.streamH.format":config_info["camera.0.streamH.format"],
                              "camera.0.streamH.framerate":parseInt(config_info["camera.0.streamH.framerate"]),
                              "camera.0.streamH.gopsize":parseInt(config_info["camera.0.streamH.gopsize"]),
                              "camera.0.streamH.storage.diskquota":parseFloat(config_info["camera.0.streamH.storage.diskquota"]),
                              "camera.0.streamH.storage.recordevents":config_info["camera.0.streamH.storage.recordevents"],
                              "camera.0.streamH.storage.recordmode":parseInt(config_info["camera.0.streamH.storage.recordmode"]),
                              "camera.0.streamH.storage.recordpostamble":parseInt(config_info["camera.0.streamH.storage.recordpostamble"]),
                              "camera.0.streamH.storage.recordpreamble":parseInt(config_info["camera.0.streamH.storage.recordpreamble"]),
                              "camera.0.streamH.storage.vaqa.0.duration":config_info["camera.0.streamH.storage.vaqa.0.duration"],
                              "camera.0.streamH.storage.vaqa.0.interval":config_info["camera.0.streamH.storage.vaqa.0.interval"],
                              "camera.0.streamH.storage.vaqa.0.qty":parseInt(config_info["camera.0.streamH.storage.vaqa.0.qty"]),
                              "camera.0.streamH.storage.vaqa.1.duration":config_info["camera.0.streamH.storage.vaqa.1.duration"],
                              "camera.0.streamH.storage.vaqa.1.interval":config_info["camera.0.streamH.storage.vaqa.1.interval"],
                              "camera.0.streamH.storage.vaqa.1.qty":parseInt(config_info["camera.0.streamH.storage.vaqa.1.qty"]),
                              "camera.0.streamL.bitrate":parseInt(config_info["camera.0.streamL.bitrate"]),
                              "camera.0.streamL.format":config_info["camera.0.streamL.format"],
                              "camera.0.streamL.framerate":parseInt(config_info["camera.0.streamL.framerate"]),
                              "camera.0.streamL.gopsize":parseInt(config_info["camera.0.streamL.gopsize"]),
                              "camera.0.streamL.storage.diskquota":parseFloat(config_info["camera.0.streamL.storage.diskquota"]),
                              "camera.0.streamL.storage.recordevents":config_info["camera.0.streamL.storage.recordevents"],
                              "camera.0.streamL.storage.recordmode":parseInt(config_info["camera.0.streamL.storage.recordmode"]),
                              "camera.0.streamL.storage.recordpostamble":parseInt(config_info["camera.0.streamL.storage.recordpostamble"]),
                              "camera.0.streamL.storage.recordpreamble":parseInt(config_info["camera.0.streamL.storage.recordpreamble"]),
                              "camera.1.enabled":config_info["camera.1.enabled"],
                              "camera.1.streamH.bitrate":parseInt(config_info["camera.1.streamH.bitrate"]),
                              "camera.1.streamH.format":config_info["camera.1.streamH.format"],
                              "camera.1.streamH.framerate":parseInt(config_info["camera.1.streamH.framerate"]),
                              "camera.1.streamH.gopsize":parseInt(config_info["camera.1.streamH.gopsize"]),
                              "camera.1.streamH.storage.recordevents":config_info["camera.1.streamH.storage.recordevents"],
                              "camera.1.streamH.storage.recordpostamble":parseInt(config_info["camera.1.streamH.storage.recordpostamble"]),
                              "camera.1.streamH.storage.recordpreamble":parseInt(config_info["camera.1.streamH.storage.recordpreamble"]),
                              "camera.1.streamH.storage.vaqa.0.duration":config_info["camera.1.streamH.storage.vaqa.0.duration"],
                              "camera.1.streamH.storage.vaqa.0.interval":config_info["camera.1.streamH.storage.vaqa.0.interval"],
                              "camera.1.streamH.storage.vaqa.0.qty":parseInt(config_info["camera.1.streamH.storage.vaqa.0.qty"]),
                              "camera.1.streamH.storage.vaqa.1.duration":config_info["camera.1.streamH.storage.vaqa.1.duration"],
                              "camera.1.streamH.storage.vaqa.1.interval":config_info["camera.1.streamH.storage.vaqa.1.interval"],
                              "camera.1.streamH.storage.vaqa.1.qty":parseInt(config_info["camera.1.streamH.storage.vaqa.1.qty"]),
                              "camera.1.streamL.bitrate":parseInt(config_info["camera.1.streamL.bitrate"]),
                              "camera.1.streamL.format":config_info["camera.1.streamL.format"],
                              "camera.1.streamL.framerate":parseInt(config_info["camera.1.streamL.framerate"]),
                              "camera.1.streamL.gopsize":parseInt(config_info["camera.1.streamL.gopsize"]),
                              "camera.1.streamL.storage.recordevents":config_info["camera.1.streamL.storage.recordevents"],
                              "camera.1.streamL.storage.recordpostamble":parseInt(config_info["camera.1.streamL.storage.recordpostamble"]),
                              "camera.1.streamL.storage.recordpreamble":parseInt(config_info["camera.1.streamL.storage.recordpreamble"]),
                              "evtgrab.capint":parseInt(config_info["evtgrab.capint"]),
                              "evtgrab.dint":parseInt(config_info["evtgrab.dint"]),
                              "evtgrab.nfiles":parseInt(config_info["evtgrab.nfiles"]),
                              "evtgrab.periodic_only":config_info["evtgrab.periodic_only"],
                              "mediaserver.nomve":config_info["mediaserver.nomve"],
                              "mediaserver.rtspsrc":config_info["mediaserver.rtspsrc"],
                              "mediaserver.video_codec":config_info["mediaserver.video_codec"],
                              "mediaserver.wmm_qos":config_info["mediaserver.wmm_qos"],
                              "network.eth-x.ip":config_info["network.eth-x.ip"],
                              "network.eth-x.method":config_info["network.eth-x.method"],
                              "network.eth-x.netmask":parseInt(config_info["network.eth-x.netmask"]),
                              "network.firewall.ports":config_info["network.firewall.ports"],
                              "network.firewall.protocols":config_info["network.firewall.protocols"],
                              "network.server.mqtt":config_info["network.server.mqtt"],
                              "network.server.vpn":config_info["network.server.vpn"],
                              "network.vpn_on_demand":config_info["network.vpn_on_demand"],
                              "rtsp.service":parseInt(config_info["rtsp.service"]),
                              "server":config_info.server,
                              "storage.maxsessions":parseInt(config_info["storage.maxsessions"]),
                              "storageserver.cache_size":parseInt(config_info["storageserver.cache_size"]),
                              "storageserver.max_clip_size":parseInt(config_info["storageserver.max_clip_size"]),
                            };

          }
          else if (config_info.model == "falcon-q"){

            configObject = {  "name": config_info.name,
                              "model": config_info.model,
                              "Genetec.custom_events":config_info["Genetec.custom_events"],
                              "alarm.redis.memory_threshold":parseFloat(config_info["alarm.redis.memory_threshold"]),
                              "application_server":config_info.application_server,
                              "application_server_port":parseInt(config_info.application_server_port),
                              "camera.0.autoexposure":config_info["camera.0.autoexposure"],
                              "camera.0.autogain":config_info["camera.0.autogain"],
                              "camera.0.colortemp":parseInt(config_info["camera.0.colortemp"]),
                              "camera.0.enabled":config_info["camera.0.enabled"],
                              "camera.0.exposure":parseInt(config_info["camera.0.exposure"]),
                              "camera.0.flickermode":config_info["camera.0.flickermode"],
                              "camera.0.gain":parseInt(config_info["camera.0.gain"]),
                              "camera.0.indoormode":config_info["camera.0.indoormode"],
                              "camera.0.master":parseInt(config_info["camera.0.master"]),
                              "camera.0.scanmode":config_info["camera.0.scanmode"],
                              "camera.0.streamH.bitrate":parseInt(config_info["camera.0.streamH.bitrate"]),
                              "camera.0.streamH.format":config_info["camera.0.streamH.format"],
                              "camera.0.streamH.framerate":parseInt(config_info["camera.0.streamH.framerate"]),
                              "camera.0.streamH.gopsize":parseInt(config_info["camera.0.streamH.gopsize"]),
                              "camera.0.streamH.storage.diskquota":parseFloat(config_info["camera.0.streamH.storage.diskquota"]),
                              "camera.0.streamH.storage.recordevents":config_info["camera.0.streamH.storage.recordevents"],
                              "camera.0.streamH.storage.recordmode":parseInt(config_info["camera.0.streamH.storage.recordmode"]),
                              "camera.0.streamH.storage.recordpostamble":parseInt(config_info["camera.0.streamH.storage.recordpostamble"]),
                              "camera.0.streamH.storage.recordpreamble":parseInt(config_info["camera.0.streamH.storage.recordpreamble"]),
                              "camera.0.streamH.storage.vaqa.0.duration":config_info["camera.0.streamH.storage.vaqa.0.duration"],
                              "camera.0.streamH.storage.vaqa.0.interval":config_info["camera.0.streamH.storage.vaqa.0.interval"],
                              "camera.0.streamH.storage.vaqa.0.qty":parseInt(config_info["camera.0.streamH.storage.vaqa.0.qty"]),
                              "camera.0.streamH.storage.vaqa.1.duration":config_info["camera.0.streamH.storage.vaqa.1.duration"],
                              "camera.0.streamH.storage.vaqa.1.interval":config_info["camera.0.streamH.storage.vaqa.1.interval"],
                              "camera.0.streamH.storage.vaqa.1.qty":parseInt(config_info["camera.0.streamH.storage.vaqa.1.qty"]),
                              "camera.0.streamL.bitrate":parseInt(config_info["camera.0.streamL.bitrate"]),
                              "camera.0.streamL.format":config_info["camera.0.streamL.format"],
                              "camera.0.streamL.framerate":parseInt(config_info["camera.0.streamL.framerate"]),
                              "camera.0.streamL.gopsize":parseInt(config_info["camera.0.streamL.gopsize"]),
                              "camera.0.streamL.storage.diskquota":parseFloat(config_info["camera.0.streamL.storage.diskquota"]),
                              "camera.0.streamL.storage.recordevents":config_info["camera.0.streamL.storage.recordevents"],
                              "camera.0.streamL.storage.recordmode":parseInt(config_info["camera.0.streamL.storage.recordmode"]),
                              "camera.0.streamL.storage.recordpostamble":parseInt(config_info["camera.0.streamL.storage.recordpostamble"]),
                              "camera.0.streamL.storage.recordpreamble":parseInt(config_info["camera.0.streamL.storage.recordpreamble"]),
                              "camera.0.wdrmode":config_info["camera.0.wdrmode"],
                              "camera.1.autoexposure":config_info["camera.1.autoexposure"],
                              "camera.1.autogain":config_info["camera.1.autogain"],
                              "camera.1.colortemp":parseInt(config_info["camera.1.colortemp"]),
                              "camera.1.enabled":config_info["camera.1.enabled"],
                              "camera.1.exposure":parseInt(config_info["camera.1.exposure"]),
                              "camera.1.flickermode":config_info["camera.1.flickermode"],
                              "camera.1.gain":parseInt(config_info["camera.1.gain"]),
                              "camera.1.indoormode":config_info["camera.1.indoormode"],
                              "camera.1.scanmode":config_info["camera.1.scanmode"],
                              "camera.1.streamH.bitrate":parseInt(config_info["camera.1.streamH.bitrate"]),
                              "camera.1.streamH.format":config_info["camera.1.streamH.format"],
                              "camera.1.streamH.framerate":parseInt(config_info["camera.1.streamH.framerate"]),
                              "camera.1.streamH.gopsize":parseInt(config_info["camera.1.streamH.gopsize"]),
                              "camera.1.streamH.storage.diskquota":parseFloat(config_info["camera.1.streamH.storage.diskquota"]),
                              "camera.1.streamH.storage.recordevents":config_info["camera.1.streamH.storage.recordevents"],
                              "camera.1.streamH.storage.recordmode":parseInt(config_info["camera.1.streamH.storage.recordmode"]),
                              "camera.1.streamH.storage.recordpostamble":parseInt(config_info["camera.1.streamH.storage.recordpostamble"]),
                              "camera.1.streamH.storage.recordpreamble":parseInt(config_info["camera.1.streamH.storage.recordpreamble"]),
                              "camera.1.streamH.storage.vaqa.0.duration":config_info["camera.1.streamH.storage.vaqa.0.duration"],
                              "camera.1.streamH.storage.vaqa.0.interval":config_info["camera.1.streamH.storage.vaqa.0.interval"],
                              "camera.1.streamH.storage.vaqa.0.qty":parseInt(config_info["camera.1.streamH.storage.vaqa.0.qty"]),
                              "camera.1.streamH.storage.vaqa.1.duration":config_info["camera.1.streamH.storage.vaqa.1.duration"],
                              "camera.1.streamH.storage.vaqa.1.interval":config_info["camera.1.streamH.storage.vaqa.1.interval"],
                              "camera.1.streamH.storage.vaqa.1.qty":parseInt(config_info["camera.1.streamH.storage.vaqa.1.qty"]),
                              "camera.1.streamL.bitrate":parseInt(config_info["camera.1.streamL.bitrate"]),
                              "camera.1.streamL.format":config_info["camera.1.streamL.format"],
                              "camera.1.streamL.framerate":parseInt(config_info["camera.1.streamL.framerate"]),
                              "camera.1.streamL.gopsize":parseInt(config_info["camera.1.streamL.gopsize"]),
                              "camera.1.streamL.storage.diskquota":parseFloat(config_info["camera.1.streamL.storage.diskquota"]),
                              "camera.1.streamL.storage.recordevents":config_info["camera.1.streamL.storage.recordevents"],
                              "camera.1.streamL.storage.recordmode":parseInt(config_info["camera.1.streamL.storage.recordmode"]),
                              "camera.1.streamL.storage.recordpostamble":parseInt(config_info["camera.1.streamL.storage.recordpostamble"]),
                              "camera.1.streamL.storage.recordpreamble":parseInt(config_info["camera.1.streamL.storage.recordpreamble"]),
                              "camera.1.wdrmode":config_info["camera.1.wdrmode"],
                              "evtgrab.capint":parseInt(config_info["evtgrab.capint"]),
                              "evtgrab.dint":parseInt(config_info["evtgrab.dint"]),
                              "evtgrab.nfiles":parseInt(config_info["evtgrab.nfiles"]),
                              "evtgrab.periodic_only":config_info["evtgrab.periodic_only"],
                              "mediaserver.nomve":config_info["mediaserver.nomve"],
                              "mediaserver.video_codec":config_info["mediaserver.video_codec"],
                              "mediaserver.wmm_qos":config_info["mediaserver.wmm_qos"],
                              "network.eth-x.ip":config_info["network.eth-x.ip"],
                              "network.eth-x.leasetime":config_info["network.eth-x.leasetime"],
                              "network.eth-x.method":config_info["network.eth-x.method"],
                              "network.eth-x.netmask":parseInt(config_info["network.eth-x.netmask"]),
                              "network.eth-x.port-forwarding.genetec":config_info["network.eth-x.port-forwarding.genetec"],
                              "network.eth-x.port-forwarding.https":config_info["network.eth-x.port-forwarding.https"],
                              "network.eth-x.port-forwarding.rtsp":config_info["network.eth-x.port-forwarding.rtsp"],
                              "network.eth-x.port-forwarding.ssh":config_info["network.eth-x.port-forwarding.ssh"],
                              "network.eth-x.whitelist":config_info["network.eth-x.whitelist"],
                              "network.firewall.ports":config_info["network.firewall.ports"],
                              "network.firewall.protocols":config_info["network.firewall.protocols"],
                              "network.region":config_info["network.region"],
                              "network.server.mqtt":config_info["network.server.mqtt"],
                              "network.server.vpn":config_info["network.server.vpn"],
                              "network.vpn_on_demand":config_info["network.vpn_on_demand"],
                              "network.wlan-x.security":config_info["network.wlan-x.security"],
                              "network.wlan-x.security.bgscan-long-interval":parseInt(config_info["network.wlan-x.security.bgscan-long-interval"]),
                              "network.wlan-x.security.bgscan-short-interval":parseInt(config_info["network.wlan-x.security.bgscan-short-interval"]),
                              "network.wlan-x.security.bgscan-signal-threshold":parseInt(config_info["network.wlan-x.security.bgscan-signal-threshold"]),
                              "network.wlan-x.security.psk":config_info["network.wlan-x.security.psk"],
                              "network.wlan-x.ssid":config_info["network.wlan-x.ssid"],
                              "network.wlan-y.security":config_info["network.wlan-y.security"],
                              "network.wlan-y.security.bgscan-long-interval":parseInt(config_info["network.wlan-y.security.bgscan-long-interval"]),
                              "network.wlan-y.security.bgscan-short-interval":parseInt(config_info["network.wlan-y.security.bgscan-short-interval"]),
                              "network.wlan-y.security.bgscan-signal-threshold":parseInt(config_info["network.wlan-y.security.bgscan-signal-threshold"]),
                              "network.wlan-y.security.psk":config_info["network.wlan-y.security.psk"],
                              "network.wlan-y.ssid":config_info["network.wlan-y.ssid"],
                              "rtsp.service":parseInt(config_info["rtsp.service"]),
                              "server":config_info.server,
                              "storage.maxsessions":parseInt(config_info["storage.maxsessions"]),
                              "storageserver.cache_size":parseInt(config_info["storageserver.cache_size"]),
                              "storageserver.max_clip_size":parseInt(config_info["storageserver.max_clip_size"]),
                            };

          }
          else if (config_info.model == "cnext"){
            configObject = {  "name": config_info.name,
                              "model": config_info.model,
                              "alarm.redis.memory_threshold":parseFloat(config_info["alarm.redis.memory_threshold"]),
                              "application_server":config_info.application_server,
                              "application_server_port":parseInt(config_info.application_server_port),
                              "lctrl.dimmer.vmax":parseInt(config_info["lctrl.dimmer.vmax"]),
                              "lctrl.ltype":parseInt(config_info["lctrl.ltype"]),
                              "lctrl.lvlrate.fast":parseInt(config_info["lctrl.lvlrate.fast"]),
                              "lctrl.lvlrate.light":parseInt(config_info["lctrl.lvlrate.light"]),
                              "lctrl.lvlrate.motion":parseInt(config_info["lctrl.lvlrate.motion"]),
                              "lctrl.lvlrate.norm":parseInt(config_info["lctrl.lvlrate.norm"]),
                              "lctrl.trigger.nonett":parseInt(config_info["lctrl.trigger.nonett"]),
                              "lctrl.trigger.off.dint":parseInt(config_info["lctrl.trigger.off.dint"]),
                              "lctrl.trigger.offt":parseInt(config_info["lctrl.trigger.offt"]),
                              "lctrl.trigger.on.dint":parseInt(config_info["lctrl.trigger.on.dint"]),
                              "lctrl.trigger.ont":parseInt(config_info["lctrl.trigger.ont"]),
                              "network.eth-x.ip":config_info["network.eth-x.ip"],
                              "network.eth-x.leasetime":config_info["network.eth-x.leasetime"],
                              "network.eth-x.method":config_info["network.eth-x.method"],
                              "network.eth-x.netmask":parseInt(config_info["network.eth-x.netmask"]),
                              "network.firewall.ports":config_info["network.firewall.ports"],
                              "network.firewall.protocols":config_info["network.firewall.protocols"],
                              "network.ppp-x.apn":config_info["network.ppp-x.apn"],
                              "network.server.mqtt":config_info["network.server.mqtt"],
                              "network.server.vpn":config_info["network.server.vpn"],
                              "network.vpn_on_demand":config_info["network.vpn_on_demand"],
                              "network.wlan-x.security":config_info["network.wlan-x.security"],
                              "network.wlan-x.security.bgscan-long-interval":parseInt(config_info["network.wlan-x.security.bgscan-long-interval"]),
                              "network.wlan-x.security.bgscan-short-interval":parseInt(config_info["network.wlan-x.security.bgscan-short-interval"]),
                              "network.wlan-x.security.bgscan-signal-threshold":parseInt(config_info["network.wlan-x.security.bgscan-signal-threshold"]),
                              "network.wlan-x.security.psk":config_info["network.wlan-x.security.psk"],
                              "network.wlan-x.ssid":config_info["network.wlan-x.ssid"],
                              "network.wlan-y.security":config_info["network.wlan-y.security"],
                              "network.wlan-y.security.bgscan-long-interval":parseInt(config_info["network.wlan-y.security.bgscan-long-interval"]),
                              "network.wlan-y.security.bgscan-short-interval":parseInt(config_info["network.wlan-y.security.bgscan-short-interval"]),
                              "network.wlan-y.security.bgscan-signal-threshold":parseInt(config_info["network.wlan-y.security.bgscan-signal-threshold"]),
                              "network.wlan-y.security.psk":config_info["network.wlan-y.security.psk"],
                              "network.wlan-y.ssid":config_info["network.wlan-y.ssid"],
                              "podbus.disable":config_info["podbus.disable"],
                              "poe.enabled":config_info["poe.enabled"],
                              "pwr.noload.i":parseInt(config_info["pwr.noload.i"]),
                              "pwr.noload.mode":parseInt(config_info["pwr.noload.mode"]),
                              "server":config_info.server,
                            };

          }
          else if (config_info.model == "unode-v2"){
            configObject = {  "name": config_info.name,
                              "model": config_info.model,
                              "networkXPasskey":config_info.networkXPasskey,
                              "networkXSSID":config_info.networkXSSID,
                              "networkXSecurity":config_info.networkXSecurity,
                              "networkYPasskey":config_info.networkYPasskey,
                              "networkYSSID":config_info.networkYSSID,
                              "networkYSecurity":config_info.networkYSecurity,
                              "network_region":config_info.network_region,
                              "debugmode":config_info.debugmode,
                              "telnet":config_info.telnet,
                              "sensor_rf_pint":config_info.sensor_rf_pint * 1000,
                              "sensor_rf_dint":config_info.sensor_rf_dint * 1000,
                              "sensor_rf_mode":config_info.sensor_rf_mode,
                              "sensor_v_pint":config_info.sensor_v_pint * 1000,
                              "sensor_v_dint":config_info.sensor_v_dint * 1000,
                              "sensor_v_mode":config_info.sensor_v_mode,
                              "sensor_lIR_pint":config_info.sensor_lIR_pint * 1000,
                              "sensor_lIR_dint":config_info.sensor_lIR_dint  * 1000,
                              "sensor_lIR_mode":config_info.sensor_lIR_mode,
                              "sensor_l_pint":config_info.sensor_l_pint * 1000,
                              "sensor_l_dint":config_info.sensor_l_dint * 1000,
                              "sensor_l_mode":config_info.sensor_l_mode,
                              "sensor_p_pint":config_info.sensor_p_pint * 1000,
                              "sensor_p_dint":config_info.sensor_p_dint * 1000,
                              "sensor_p_mode":config_info.sensor_p_mode,
                              "sensor_t_pint":config_info.sensor_t_pint * 1000,
                              "sensor_t_dint":config_info.sensor_t_dint * 1000,
                              "sensor_t_mode":config_info.sensor_t_mode,
                              "sensor_T_pint":config_info.sensor_T_pint * 1000,
                              "sensor_T_dint":config_info.sensor_T_dint * 1000,
                              "sensor_T_mode":config_info.sensor_T_mode,
                              "sensor_pdc_pint":config_info.sensor_pdc_pint * 1000,
                              "sensor_pdc_dint":config_info.sensor_pdc_dint * 1000,
                              "sensor_pdc_mode":config_info.sensor_pdc_mode,
                              "sensor_ppr_pint":config_info.sensor_ppr_pint * 1000,
                              "sensor_ppr_dint":config_info.sensor_ppr_dint * 1000,
                              "sensor_ppr_mode":config_info.sensor_ppr_mode,
                              "sensor_pnd_pint":config_info.sensor_pnd_pint * 1000,
                              "sensor_pnd_dint":config_info.sensor_pnd_dint * 1000,
                              "sensor_pnd_mode":config_info.sensor_pnd_mode,
                              "sensor_pdt_pint":config_info.sensor_pdt_pint * 1000,
                              "sensor_pdt_dint":config_info.sensor_pdt_dint * 1000,
                              "sensor_pdt_mode":config_info.sensor_pdt_mode,
                              "lctrl_dimmer_vmax":config_info.lctrl_dimmer_vmax,
                          };
           
          }
          else if (config_info.model == "unode-v3"){
            configObject = {  "name": config_info.name,
                              "model": config_info.model,
                              "networkXPasskey":config_info.networkXPasskey,
                              "networkXSSID":config_info.networkXSSID,
                              "networkXSecurity":config_info.networkXSecurity,
                              "networkYPasskey":config_info.networkYPasskey,
                              "networkYSSID":config_info.networkYSSID,
                              "networkYSecurity":config_info.networkYSecurity,
                              "network_region":config_info.network_region,
                              "debugmode":config_info.debugmode,
                              "telnet":config_info.telnet,
                              "sensor_rf_pint":config_info.sensor_rf_pint * 1000,
                              "sensor_rf_dint":config_info.sensor_rf_dint * 1000,
                              "sensor_rf_mode":config_info.sensor_rf_mode,
                              "sensor_v_pint":config_info.sensor_v_pint * 1000,
                              "sensor_v_dint":config_info.sensor_v_dint * 1000,
                              "sensor_v_mode":config_info.sensor_v_mode,
                              "sensor_mi_pint":config_info.sensor_mi_pint * 1000,
                              "sensor_mi_dint":config_info.sensor_mi_dint * 1000,
                              "sensor_mi_mode":config_info.sensor_mi_mode,
                              "sensor_lIR_pint":config_info.sensor_lIR_pint * 1000,
                              "sensor_lIR_dint":config_info.sensor_lIR_dint  * 1000,
                              "sensor_lIR_mode":config_info.sensor_lIR_mode,
                              "sensor_l_pint":config_info.sensor_l_pint * 1000,
                              "sensor_l_dint":config_info.sensor_l_dint * 1000,
                              "sensor_l_mode":config_info.sensor_l_mode,
                              "sensor_p_pint":config_info.sensor_p_pint * 1000,
                              "sensor_p_dint":config_info.sensor_p_dint * 1000,
                              "sensor_p_mode":config_info.sensor_p_mode,
                              "sensor_t_pint":config_info.sensor_t_pint * 1000,
                              "sensor_t_dint":config_info.sensor_t_dint * 1000,
                              "sensor_t_mode":config_info.sensor_t_mode,
                              "sensor_T_pint":config_info.sensor_T_pint * 1000,
                              "sensor_T_dint":config_info.sensor_T_dint * 1000,
                              "sensor_T_mode":config_info.sensor_T_mode,
                              "sensor_pdc_pint":config_info.sensor_pdc_pint * 1000,
                              "sensor_pdc_dint":config_info.sensor_pdc_dint * 1000,
                              "sensor_pdc_mode":config_info.sensor_pdc_mode,
                              "sensor_ppr_pint":config_info.sensor_ppr_pint * 1000,
                              "sensor_ppr_dint":config_info.sensor_ppr_dint * 1000,
                              "sensor_ppr_mode":config_info.sensor_ppr_mode,
                              "sensor_pnd_pint":config_info.sensor_pnd_pint * 1000,
                              "sensor_pnd_dint":config_info.sensor_pnd_dint * 1000,
                              "sensor_pnd_mode":config_info.sensor_pnd_mode,
                              "sensor_pdt_pint":config_info.sensor_pdt_pint * 1000,
                              "sensor_pdt_dint":config_info.sensor_pdt_dint * 1000,
                              "sensor_pdt_mode":config_info.sensor_pdt_mode,
                              "sensor_a_jdelta":parseInt(config_info.sensor_a_jdelta),
                              "lctrl_dimmer_vmax":config_info.lctrl_dimmer_vmax,
                          };
           
          }
          else if (config_info.model == "unode-v4"){
            configObject = {  "name": config_info.name,
                              "model": config_info.model,
                              "networkXPasskey":config_info.networkXPasskey,
                              "networkXSSID":config_info.networkXSSID,
                              "networkXSecurity":config_info.networkXSecurity,
                              "networkYPasskey":config_info.networkYPasskey,
                              "networkYSSID":config_info.networkYSSID,
                              "networkYSecurity":config_info.networkYSecurity,
                              "network_region":config_info.network_region,
                              "debugmode":config_info.debugmode,
                              "telnet":config_info.telnet,
                              "aux_power":config_info.aux_power,
                              "podbus_disable":config_info.podbus_disable,
                              "sensor_rf_pint":config_info.sensor_rf_pint * 1000,
                              "sensor_rf_dint":config_info.sensor_rf_dint * 1000,
                              "sensor_rf_mode":config_info.sensor_rf_mode,
                              "sensor_v_pint":config_info.sensor_v_pint * 1000,
                              "sensor_v_dint":config_info.sensor_v_dint * 1000,
                              "sensor_v_mode":config_info.sensor_v_mode,
                              "sensor_aw_pint":config_info.sensor_aw_pint * 1000,
                              "sensor_aw_dint":config_info.sensor_aw_dint * 1000,
                              "sensor_aw_mode":config_info.sensor_aw_mode,
                              "sensor_aPF_pint":config_info.sensor_aPF_pint * 1000,
                              "sensor_aPF_dint":config_info.sensor_aPF_dint * 1000,
                              "sensor_aPF_mode":config_info.sensor_aPF_mode,
                              "sensor_aP_pint":config_info.sensor_aP_pint * 1000,
                              "sensor_aP_dint":config_info.sensor_aP_dint * 1000,
                              "sensor_aP_mode":config_info.sensor_aP_mode,
                              "sensor_mP_pint":config_info.sensor_mP_pint * 1000,
                              "sensor_mP_dint":config_info.sensor_mP_dint * 1000,
                              "sensor_mP_mode":config_info.sensor_mP_mode,
                              "sensor_mip_pint":config_info.sensor_mip_pint * 1000,
                              "sensor_mip_dint":config_info.sensor_mip_dint * 1000,
                              "sensor_mip_mode":config_info.sensor_mip_mode,
                              "sensor_ai_pint":config_info.sensor_ai_pint * 1000,
                              "sensor_ai_dint":config_info.sensor_ai_dint * 1000,
                              "sensor_ai_mode":config_info.sensor_ai_mode,
                              "sensor_aip_pint":config_info.sensor_aip_pint * 1000,
                              "sensor_aip_dint":config_info.sensor_aip_dint * 1000,
                              "sensor_aip_mode":config_info.sensor_aip_mode,
                              "sensor_mi_pint":config_info.sensor_mi_pint * 1000,
                              "sensor_mi_dint":config_info.sensor_mi_dint * 1000,
                              "sensor_mi_mode":config_info.sensor_mi_mode,
                              "sensor_mw_pint":config_info.sensor_mw_pint * 1000,
                              "sensor_mw_dint":config_info.sensor_mw_dint * 1000,
                              "sensor_mw_mode":config_info.sensor_mw_mode,
                              "sensor_mPF_pint":config_info.sensor_mPF_pint * 1000,
                              "sensor_mPF_dint":config_info.sensor_mPF_dint * 1000,
                              "sensor_mPF_mode":config_info.sensor_mPF_mode,
                              "sensor_lIR_pint":config_info.sensor_lIR_pint * 1000,
                              "sensor_lIR_dint":config_info.sensor_lIR_dint  * 1000,
                              "sensor_lIR_mode":config_info.sensor_lIR_mode,
                              "sensor_l_pint":config_info.sensor_l_pint * 1000,
                              "sensor_l_dint":config_info.sensor_l_dint * 1000,
                              "sensor_l_mode":config_info.sensor_l_mode,
                              "sensor_p_pint":config_info.sensor_p_pint * 1000,
                              "sensor_p_dint":config_info.sensor_p_dint * 1000,
                              "sensor_p_mode":config_info.sensor_p_mode,
                              "sensor_a_jdelta":parseInt(config_info.sensor_a_jdelta),
                              "sensor_pc_pint":config_info.sensor_pc_pint * 1000,
                              "sensor_pc_dint":config_info.sensor_pc_dint * 1000,
                              "sensor_pc_mode":config_info.sensor_pc_mode,
                              "sensor_t_pint":config_info.sensor_t_pint * 1000,
                              "sensor_t_dint":config_info.sensor_t_dint * 1000,
                              "sensor_t_mode":config_info.sensor_t_mode,
                              "sensor_T_pint":config_info.sensor_T_pint * 1000,
                              "sensor_T_dint":config_info.sensor_T_dint * 1000,
                              "sensor_T_mode":config_info.sensor_T_mode,
                              "sensor_mt_pint":config_info.sensor_mt_pint * 1000,
                              "sensor_mt_dint":config_info.sensor_mt_dint * 1000,
                              "sensor_mt_mode":config_info.sensor_mt_mode,
                              "sensor_podm_pint":config_info.sensor_podm_pint * 1000,
                              "sensor_podm_dint":config_info.sensor_podm_dint * 1000,
                              "sensor_podm_mode":config_info.sensor_podm_mode,
                              "sensor_podme_pint":config_info.sensor_podme_pint * 1000,
                              "sensor_podme_dint":config_info.sensor_podme_dint * 1000,
                              "sensor_podme_mode":config_info.sensor_podme_mode,
                              "sensor_rlym_pint":config_info.sensor_rlym_pint * 1000,
                              "lctrl_dimmer_vmax":config_info.lctrl_dimmer_vmax,
                              "lctrl_trigger_off_dint":config_info.lctrl_trigger_off_dint * 1000,
                              "lctrl_trigger_on_dint":config_info.lctrl_trigger_on_dint * 1000,
                              "lctrl_trigger_ont":config_info.lctrl_trigger_ont * 1000,
                              "lctrl_trigger_offt":config_info.lctrl_trigger_offt * 1000,
                              "lctrl_trigger_nonett":config_info.lctrl_trigger_nonett * 1000,
                          };

          }
          else if (config_info.model == "unode-v5"){
            configObject = {  "name": config_info.name,
                              "model": config_info.model,
                              "network_region":config_info.network_region,
                              "debugmode":config_info.debugmode,
                              "telnet":config_info.telnet,
                              "aux_power":config_info.aux_power,
                              "podbus_disable":config_info.podbus_disable,
                              "sensor_rf_pint":config_info.sensor_rf_pint * 1000,
                              "sensor_rf_dint":config_info.sensor_rf_dint * 1000,
                              "sensor_rf_mode":config_info.sensor_rf_mode,
                              "sensor_v_pint":config_info.sensor_v_pint * 1000,
                              "sensor_v_dint":config_info.sensor_v_dint * 1000,
                              "sensor_v_mode":config_info.sensor_v_mode,
                              "sensor_aw_pint":config_info.sensor_aw_pint * 1000,
                              "sensor_aw_dint":config_info.sensor_aw_dint * 1000,
                              "sensor_aw_mode":config_info.sensor_aw_mode,
                              "sensor_aPF_pint":config_info.sensor_aPF_pint * 1000,
                              "sensor_aPF_dint":config_info.sensor_aPF_dint * 1000,
                              "sensor_aPF_mode":config_info.sensor_aPF_mode,
                              "sensor_aP_pint":config_info.sensor_aP_pint * 1000,
                              "sensor_aP_dint":config_info.sensor_aP_dint * 1000,
                              "sensor_aP_mode":config_info.sensor_aP_mode,
                              "sensor_mP_pint":config_info.sensor_mP_pint * 1000,
                              "sensor_mP_dint":config_info.sensor_mP_dint * 1000,
                              "sensor_mP_mode":config_info.sensor_mP_mode,
                              "sensor_mip_pint":config_info.sensor_mip_pint * 1000,
                              "sensor_mip_dint":config_info.sensor_mip_dint * 1000,
                              "sensor_mip_mode":config_info.sensor_mip_mode,
                              "sensor_ai_pint":config_info.sensor_ai_pint * 1000,
                              "sensor_ai_dint":config_info.sensor_ai_dint * 1000,
                              "sensor_ai_mode":config_info.sensor_ai_mode,
                              "sensor_aip_pint":config_info.sensor_aip_pint * 1000,
                              "sensor_aip_dint":config_info.sensor_aip_dint * 1000,
                              "sensor_aip_mode":config_info.sensor_aip_mode,
                              "sensor_mi_pint":config_info.sensor_mi_pint * 1000,
                              "sensor_mi_dint":config_info.sensor_mi_dint * 1000,
                              "sensor_mi_mode":config_info.sensor_mi_mode,
                              "sensor_mw_pint":config_info.sensor_mw_pint * 1000,
                              "sensor_mw_dint":config_info.sensor_mw_dint * 1000,
                              "sensor_mw_mode":config_info.sensor_mw_mode,
                              "sensor_mPF_pint":config_info.sensor_mPF_pint * 1000,
                              "sensor_mPF_dint":config_info.sensor_mPF_dint * 1000,
                              "sensor_mPF_mode":config_info.sensor_mPF_mode,
                              "sensor_lIR_pint":config_info.sensor_lIR_pint * 1000,
                              "sensor_lIR_dint":config_info.sensor_lIR_dint  * 1000,
                              "sensor_lIR_mode":config_info.sensor_lIR_mode,
                              "sensor_l_pint":config_info.sensor_l_pint * 1000,
                              "sensor_l_dint":config_info.sensor_l_dint * 1000,
                              "sensor_l_mode":config_info.sensor_l_mode,
                              "sensor_l_i_pint":config_info.sensor_l_i_pint * 1000,
                              "sensor_l_i_dint":config_info.sensor_l_i_dint * 1000,
                              "sensor_l_i_mode":config_info.sensor_l_i_mode,
                              "sensor_lIR_i_pint":config_info.sensor_lIR_i_pint * 1000,
                              "sensor_lIR_i_dint":config_info.sensor_lIR_i_dint * 1000,
                              "sensor_lIR_i_mode":config_info.sensor_lIR_i_mode,
                              "sensor_p_pint":config_info.sensor_p_pint * 1000,
                              "sensor_p_dint":config_info.sensor_p_dint * 1000,
                              "sensor_p_mode":config_info.sensor_p_mode,
                              "sensor_pc_pint":config_info.sensor_pc_pint * 1000,
                              "sensor_pc_dint":config_info.sensor_pc_dint * 1000,
                              "sensor_pc_mode":config_info.sensor_pc_mode,
                              "sensor_t_pint":config_info.sensor_t_pint * 1000,
                              "sensor_t_dint":config_info.sensor_t_dint * 1000,
                              "sensor_t_mode":config_info.sensor_t_mode,
                              "sensor_T_pint":config_info.sensor_T_pint * 1000,
                              "sensor_T_dint":config_info.sensor_T_dint * 1000,
                              "sensor_T_mode":config_info.sensor_T_mode,
                              "sensor_mt_pint":config_info.sensor_mt_pint * 1000,
                              "sensor_mt_dint":config_info.sensor_mt_dint * 1000,
                              "sensor_mt_mode":config_info.sensor_mt_mode,
                              "sensor_podm_pint":config_info.sensor_podm_pint * 1000,
                              "sensor_podm_dint":config_info.sensor_podm_dint * 1000,
                              "sensor_podm_mode":config_info.sensor_podm_mode,
                              "sensor_podme_pint":config_info.sensor_podme_pint * 1000,
                              "sensor_podme_dint":config_info.sensor_podme_dint * 1000,
                              "sensor_podme_mode":config_info.sensor_podme_mode,
                              "sensor_rlym_pint":config_info.sensor_rlym_pint * 1000,
                              "lctrl_dimmer_vmax":config_info.lctrl_dimmer_vmax,
                              "lctrl_trigger_off_dint":config_info.lctrl_trigger_off_dint * 1000,
                              "lctrl_trigger_on_dint":config_info.lctrl_trigger_on_dint * 1000,
                              "lctrl_trigger_ont":config_info.lctrl_trigger_ont * 1000,
                              "lctrl_trigger_offt":config_info.lctrl_trigger_offt * 1000,
                              "lctrl_trigger_nonett":config_info.lctrl_trigger_nonett * 1000,
                          };

          }
          else {
            configObject = {  "name": config_info.name,
                              "model": config_info.model,
                              "network_region":config_info.network_region,
                              "debugmode":config_info.debugmode,
                              "telnet":config_info.telnet,
                              "aux_power":config_info.aux_power,
                              "podbus_disable":config_info.podbus_disable,
                              "sensor_rf_pint":config_info.sensor_rf_pint * 1000,
                              "sensor_rf_dint":config_info.sensor_rf_dint * 1000,
                              "sensor_rf_mode":config_info.sensor_rf_mode,
                              "sensor_v_pint":config_info.sensor_v_pint * 1000,
                              "sensor_v_dint":config_info.sensor_v_dint * 1000,
                              "sensor_v_mode":config_info.sensor_v_mode,
                              "sensor_aw_pint":config_info.sensor_aw_pint * 1000,
                              "sensor_aw_dint":config_info.sensor_aw_dint * 1000,
                              "sensor_aw_mode":config_info.sensor_aw_mode,
                              "sensor_aPF_pint":config_info.sensor_aPF_pint * 1000,
                              "sensor_aPF_dint":config_info.sensor_aPF_dint * 1000,
                              "sensor_aPF_mode":config_info.sensor_aPF_mode,
                              "sensor_aP_pint":config_info.sensor_aP_pint * 1000,
                              "sensor_aP_dint":config_info.sensor_aP_dint * 1000,
                              "sensor_aP_mode":config_info.sensor_aP_mode,
                              "sensor_mP_pint":config_info.sensor_mP_pint * 1000,
                              "sensor_mP_dint":config_info.sensor_mP_dint * 1000,
                              "sensor_mP_mode":config_info.sensor_mP_mode,
                              "sensor_mip_pint":config_info.sensor_mip_pint * 1000,
                              "sensor_mip_dint":config_info.sensor_mip_dint * 1000,
                              "sensor_mip_mode":config_info.sensor_mip_mode,
                              "sensor_ai_pint":config_info.sensor_ai_pint * 1000,
                              "sensor_ai_dint":config_info.sensor_ai_dint * 1000,
                              "sensor_ai_mode":config_info.sensor_ai_mode,
                              "sensor_aip_pint":config_info.sensor_aip_pint * 1000,
                              "sensor_aip_dint":config_info.sensor_aip_dint * 1000,
                              "sensor_aip_mode":config_info.sensor_aip_mode,
                              "sensor_mi_pint":config_info.sensor_mi_pint * 1000,
                              "sensor_mi_dint":config_info.sensor_mi_dint * 1000,
                              "sensor_mi_mode":config_info.sensor_mi_mode,
                              "sensor_mw_pint":config_info.sensor_mw_pint * 1000,
                              "sensor_mw_dint":config_info.sensor_mw_dint * 1000,
                              "sensor_mw_mode":config_info.sensor_mw_mode,
                              "sensor_mPF_pint":config_info.sensor_mPF_pint * 1000,
                              "sensor_mPF_dint":config_info.sensor_mPF_dint * 1000,
                              "sensor_mPF_mode":config_info.sensor_mPF_mode,
                              "sensor_lIR_pint":config_info.sensor_lIR_pint * 1000,
                              "sensor_lIR_dint":config_info.sensor_lIR_dint  * 1000,
                              "sensor_lIR_mode":config_info.sensor_lIR_mode,
                              "sensor_l_pint":config_info.sensor_l_pint * 1000,
                              "sensor_l_dint":config_info.sensor_l_dint * 1000,
                              "sensor_l_mode":config_info.sensor_l_mode,
                              "sensor_l_i_pint":config_info.sensor_l_i_pint * 1000,
                              "sensor_l_i_dint":config_info.sensor_l_i_dint * 1000,
                              "sensor_l_i_mode":config_info.sensor_l_i_mode,
                              "sensor_lIR_i_pint":config_info.sensor_lIR_i_pint * 1000,
                              "sensor_lIR_i_dint":config_info.sensor_lIR_i_dint * 1000,
                              "sensor_lIR_i_mode":config_info.sensor_lIR_i_mode,
                              "sensor_p_pint":config_info.sensor_p_pint * 1000,
                              "sensor_p_dint":config_info.sensor_p_dint * 1000,
                              "sensor_p_mode":config_info.sensor_p_mode,
                              "sensor_pc_pint":config_info.sensor_pc_pint * 1000,
                              "sensor_pc_dint":config_info.sensor_pc_dint * 1000,
                              "sensor_pc_mode":config_info.sensor_pc_mode,
                              "sensor_t_pint":config_info.sensor_t_pint * 1000,
                              "sensor_t_dint":config_info.sensor_t_dint * 1000,
                              "sensor_t_mode":config_info.sensor_t_mode,
                              "sensor_T_pint":config_info.sensor_T_pint * 1000,
                              "sensor_T_dint":config_info.sensor_T_dint * 1000,
                              "sensor_T_mode":config_info.sensor_T_mode,
                              "sensor_mt_pint":config_info.sensor_mt_pint * 1000,
                              "sensor_mt_dint":config_info.sensor_mt_dint * 1000,
                              "sensor_mt_mode":config_info.sensor_mt_mode,
                              "sensor_podm_pint":config_info.sensor_podm_pint * 1000,
                              "sensor_podm_dint":config_info.sensor_podm_dint * 1000,
                              "sensor_podm_mode":config_info.sensor_podm_mode,
                              "sensor_podme_pint":config_info.sensor_podme_pint * 1000,
                              "sensor_podme_dint":config_info.sensor_podme_dint * 1000,
                              "sensor_podme_mode":config_info.sensor_podme_mode,
                              "sensor_rlym_pint":config_info.sensor_rlym_pint * 1000,
                              "lctrl_dimmer_vmax":config_info.lctrl_dimmer_vmax,
                              "lctrl_trigger_off_dint":config_info.lctrl_trigger_off_dint * 1000,
                              "lctrl_trigger_on_dint":config_info.lctrl_trigger_on_dint * 1000,
                              "lctrl_trigger_ont":config_info.lctrl_trigger_ont * 1000,
                              "lctrl_trigger_offt":config_info.lctrl_trigger_offt * 1000,
                              "lctrl_trigger_nonett":config_info.lctrl_trigger_nonett * 1000,
                          };
          }


          $.ajax({
            "url": NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/configs/',
            "type": "POST",
            "data": JSON.stringify(configObject),
            "xhrFields": {
              withCredentials: true
            },
            "dataType": "json",
            "contentType": "application/json",
            "processData": false,
            "success": function (data) {
              data.groups = that.recreateGroups(assignment.assigngroups, that.state.groups);
              data.assign = assignment.assign;
              data.sites = save_sites.splice(0);
              data.nodes = save_nodes.splice(0).length;
              data.model = helpers.modelName(data.model);
              if(data.lctrl_dimmer_vmax){
                data.lctrl_dimmer_vmax = selected_lctrl_dimmer_vmax;
              }
              if(data.networkXSecurity){
                data.networkXSecurity = selected_networkXSecurity;
              }
              if(data.networkYSecurity){
                data.networkYSecurity = selected_networkYSecurity;
              }

              if(data["camera.0.streamH.storage.recordmode"]) {
                data["camera.0.streamH.storage.recordmode"] = selected_camera0streamHstoragerecordmode;
              }

              if(data["camera.0.streamL.storage.recordmode"]){
                data["camera.0.streamL.storage.recordmode"] = selected_camera0streamLstoragerecordmode;
              }

              if(data["camera.1.streamH.storage.recordmode"]){
                data["camera.1.streamH.storage.recordmode"] = selected_camera1streamHstoragerecordmode;
              }

              if(data["camera.1.streamL.storage.recordmode"]){
                data["camera.1.streamL.storage.recordmode"] = selected_camera1streamLstoragerecordmode;
              }

              NSN.configID = data.configid;
              sessionStorage.setItem("configID", NSN.configID);
              data.idx = this.state.configs.length;
              var newData = {
                "name": data.name,
                "server": data.server,
                "configid": data.configid
              };

              var newState = React.addons.update(this.state, { configs: { $push: [newData] }, currentconfig: { $set: [data] }, configID: { $set: data.configid } });
              that.getAllConfigs();
              noty({ type: "success", text: 'Config "' + data.name + '" added.' });
              ReactBootstrap.Dispatcher.emit("Configform.add.success", data);
              this.configAssignment(assignment, data.configid);
              this.setState(newState);
            }.bind(that),
            "error": function () {
              noty({ type: "error", text: "Could not add config." });
            }
          })
        
        }
       }
      else if(!that.checkDuplicate(config_info)){

        var idx = helpers.get_idx(that.state.configs, config_info, 'configid');
        console.log("Updating config (idx:" + idx + "; id:" + config_info.configid + ")");
        delete config_info.idx;
        
          if(config_info.model == "vdkmaster"){
            configObject = {  "name": config_info.name,
                              "model": config_info.model,
                              "configid":config_info.configid,
                              "siteid": config_info.siteid,
                              "al.left.setbrightness":parseInt(config_info["al.left.setbrightness"]),
                              "al.left.setmode":config_info["al.left.setmode"],
                              "al.right.setbrightness":parseInt(config_info["al.right.setbrightness"]),
                              "al.right.setmode":config_info["al.right.setmode"],
                              "alarm.redis.memory_threshold":parseFloat(config_info["alarm.redis.memory_threshold"]),
                              "application_server":config_info.application_server,
                              "application_server_port":parseInt(config_info.application_server_port),
                              "appos.timezone":config_info["appos.timezone"],
                              "glassl.display.blue.gain":parseInt(config_info["glassl.display.blue.gain"]),
                              "glassl.display.blue.offset":parseInt(config_info["glassl.display.blue.offset"]),
                              "glassl.display.brightness":parseInt(config_info["glassl.display.brightness"]),
                              "glassl.display.color-temp":parseInt(config_info["glassl.display.color-temp"]),
                              "glassl.display.contrast":parseInt(config_info["glassl.display.contrast"]),
                              "glassl.display.green.gain":parseInt(config_info["glassl.display.green.gain"]),
                              "glassl.display.green.offset":parseInt(config_info["glassl.display.green.offset"]),
                              "glassl.display.hue":parseInt(config_info["glassl.display.hue"]),
                              "glassl.display.mode.ccs":config_info["glassl.display.mode.ccs"],
                              "glassl.display.mode.dynamic-nr":config_info["glassl.display.mode.dynamic-nr"],
                              "glassl.display.mode.main-dcdi":config_info["glassl.display.mode.main-dcdi"],
                              "glassl.display.mode.main-madi":config_info["glassl.display.mode.main-madi"],
                              "glassl.display.mode.mpeg-nr":config_info["glassl.display.mode.mpeg-nr"],
                              "glassl.display.mode.selection-1080p":config_info["glassl.display.mode.selection-1080p"],
                              "glassl.display.mode.selection-720p":config_info["glassl.display.mode.selection-720p"],
                              "glassl.display.mpeg-nr":parseInt(config_info["glassl.display.mpeg-nr"]),
                              "glassl.display.power":config_info["glassl.display.power"],
                              "glassl.display.red.gain":parseInt(config_info["glassl.display.red.gain"]),
                              "glassl.display.red.offset":parseInt(config_info["glassl.display.red.offset"]),
                              "glassl.display.saturation":parseInt(config_info["glassl.display.saturation"]),
                              "glassl.display.sharpness":parseInt(config_info["glassl.display.sharpness"]),
                              "glassl.display.temp.cold":parseInt(config_info["glassl.display.temp.cold"]),
                              "glassl.display.temp.hot":parseInt(config_info["glassl.display.temp.hot"]),
                              "glassl.display.temp.warm":parseInt(config_info["glassl.display.temp.warm"]),
                              "glassr.display.blue.gain":parseInt(config_info["glassr.display.blue.gain"]),
                              "glassr.display.blue.offset":parseInt(config_info["glassr.display.blue.offset"]),
                              "glassr.display.brightness":parseInt(config_info["glassr.display.brightness"]),
                              "glassr.display.color-temp":parseInt(config_info["glassr.display.color-temp"]),
                              "glassr.display.contrast":parseInt(config_info["glassr.display.contrast"]),
                              "glassr.display.green.gain":parseInt(config_info["glassr.display.green.gain"]),
                              "glassr.display.green.offset":parseInt(config_info["glassr.display.green.offset"]),
                              "glassr.display.hue":parseInt(config_info["glassr.display.hue"]),
                              "glassr.display.mode.ccs":config_info["glassr.display.mode.ccs"],
                              "glassr.display.mode.dynamic-nr":config_info["glassr.display.mode.dynamic-nr"],
                              "glassr.display.mode.main-dcdi":config_info["glassr.display.mode.main-dcdi"],
                              "glassr.display.mode.main-madi":config_info["glassr.display.mode.main-madi"],
                              "glassr.display.mode.mpeg-nr":config_info["glassr.display.mode.mpeg-nr"],
                              "glassr.display.mode.selection-1080p":config_info["glassr.display.mode.selection-1080p"],
                              "glassr.display.mode.selection-720p":config_info["glassr.display.mode.selection-720p"],
                              "glassr.display.mpeg-nr":parseInt(config_info["glassr.display.mpeg-nr"]),
                              "glassr.display.power":config_info["glassr.display.power"],
                              "glassr.display.red.gain":parseInt(config_info["glassr.display.red.gain"]),
                              "glassr.display.red.offset":parseInt(config_info["glassr.display.red.offset"]),
                              "glassr.display.saturation":parseInt(config_info["glassr.display.saturation"]),
                              "glassr.display.sharpness":parseInt(config_info["glassr.display.sharpness"]),
                              "glassr.display.temp.cold":parseInt(config_info["glassr.display.temp.cold"]),
                              "glassr.display.temp.hot":parseInt(config_info["glassr.display.temp.hot"]),
                              "glassr.display.temp.warm":parseInt(config_info["glassr.display.temp.warm"]),
                              "lr.setcolor":config_info["lr.setcolor"],
                              "network.eth-x.ip":config_info["network.eth-x.ip"],
                              "network.eth-x.method":config_info["network.eth-x.method"],
                              "network.eth-x.netmask":parseInt(config_info["network.eth-x.netmask"]),
                              "network.eth-x.port-forwarding.genetec":config_info["network.eth-x.port-forwarding.genetec"],
                              "network.eth-x.port-forwarding.https":config_info["network.eth-x.port-forwarding.https"],
                              "network.eth-x.port-forwarding.rtsp":config_info["network.eth-x.port-forwarding.rtsp"],
                              "network.eth-x.port-forwarding.ssh":config_info["network.eth-x.port-forwarding.ssh"],
                              "network.eth-x.whitelist":config_info["network.eth-x.whitelist"],
                              "network.firewall.ports":config_info["network.firewall.ports"],
                              "network.firewall.protocols":config_info["network.firewall.protocols"],
                              "network.server.mqtt":config_info["network.server.mqtt"],
                              "network.server.vpn":config_info["network.server.vpn"],
                              "network.vpn_on_demand":config_info["network.vpn_on_demand"],
                              "pebble.display.blue.gain":parseInt(config_info["pebble.display.blue.gain"]),
                              "pebble.display.blue.offset":parseInt(config_info["pebble.display.blue.offset"]),
                              "pebble.display.brightness":parseInt(config_info["pebble.display.brightness"]),
                              "pebble.display.color-temp":parseInt(config_info["pebble.display.color-temp"]),
                              "pebble.display.contrast":parseInt(config_info["pebble.display.contrast"]),
                              "pebble.display.green.gain":parseInt(config_info["pebble.display.green.gain"]),
                              "pebble.display.green.offset":parseInt(config_info["pebble.display.green.offset"]),
                              "pebble.display.hue":parseInt(config_info["pebble.display.hue"]),
                              "pebble.display.mode.ccs":config_info["pebble.display.mode.ccs"],
                              "pebble.display.mode.dynamic-nr":config_info["pebble.display.mode.dynamic-nr"],
                              "pebble.display.mode.main-dcdi":config_info["pebble.display.mode.main-dcdi"],
                              "pebble.display.mode.main-madi":config_info["pebble.display.mode.main-madi"],
                              "pebble.display.mode.mpeg-nr":config_info["pebble.display.mode.mpeg-nr"],
                              "pebble.display.mode.selection-1080p":config_info["pebble.display.mode.selection-1080p"],
                              "pebble.display.mode.selection-720p":config_info["pebble.display.mode.selection-720p"],
                              "pebble.display.mpeg-nr":parseInt(config_info["pebble.display.mpeg-nr"]),
                              "pebble.display.power":config_info["pebble.display.power"],
                              "pebble.display.red.gain":config_info["pebble.display.red.gain"],
                              "pebble.display.red.offset":parseInt(config_info["pebble.display.red.offset"]),
                              "pebble.display.saturation":parseInt(config_info["pebble.display.saturation"]),
                              "pebble.display.sharpness":parseInt(config_info["pebble.display.sharpness"]),
                              "pebble.display.temp.cold":parseInt(config_info["pebble.display.temp.cold"]),
                              "pebble.display.temp.hot":parseInt(config_info["pebble.display.temp.hot"]),
                              "pebble.display.temp.warm":parseInt(config_info["pebble.display.temp.warm"]),
                              "server":config_info.server,
                            };
            

          }
          else if (config_info.model == "merlin"){
             configObject = {  "name": config_info.name,
                              "model": config_info.model,
                              "configid":config_info.configid,
                              "Genetec.custom_events":config_info["Genetec.custom_events"],
                              "alarm.redis.memory_threshold":parseFloat(config_info["alarm.redis.memory_threshold"]),
                              "application_server":config_info.application_server,
                              "application_server_port":parseInt(config_info.application_server_port),
                              "camera.0.binning":config_info["camera.0.binning"],
                              "camera.0.enabled":config_info["camera.0.enabled"],
                              "camera.0.exposure":parseInt(config_info["camera.0.exposure"]),
                              "camera.0.gain":parseInt(config_info["camera.0.gain"]),
                              "camera.0.master":parseInt(config_info["camera.0.master"]),
                              "camera.0.streamH.bitrate":parseInt(config_info["camera.0.streamH.bitrate"]),
                              "camera.0.streamH.format":config_info["camera.0.streamH.format"],
                              "camera.0.streamH.framerate":parseInt(config_info["camera.0.streamH.framerate"]),
                              "camera.0.streamH.gopsize":parseInt(config_info["camera.0.streamH.gopsize"]),
                              "camera.0.streamH.storage.diskquota":parseFloat(config_info["camera.0.streamH.storage.diskquota"]),
                              "camera.0.streamH.storage.recordevents":config_info["camera.0.streamH.storage.recordevents"],
                              "camera.0.streamH.storage.recordmode":parseInt(config_info["camera.0.streamH.storage.recordmode"]),
                              "camera.0.streamH.storage.recordpostamble":parseInt(config_info["camera.0.streamH.storage.recordpostamble"]),
                              "camera.0.streamH.storage.recordpreamble":parseInt(config_info["camera.0.streamH.storage.recordpreamble"]),
                              "camera.0.streamH.storage.vaqa.0.duration":config_info["camera.0.streamH.storage.vaqa.0.duration"],
                              "camera.0.streamH.storage.vaqa.0.interval":config_info["camera.0.streamH.storage.vaqa.0.interval"],
                              "camera.0.streamH.storage.vaqa.0.qty":parseInt(config_info["camera.0.streamH.storage.vaqa.0.qty"]),
                              "camera.0.streamH.storage.vaqa.1.duration":config_info["camera.0.streamH.storage.vaqa.1.duration"],
                              "camera.0.streamH.storage.vaqa.1.interval":config_info["camera.0.streamH.storage.vaqa.1.interval"],
                              "camera.0.streamH.storage.vaqa.1.qty":parseInt(config_info["camera.0.streamH.storage.vaqa.1.qty"]),
                              "camera.0.streamL.bitrate":parseInt(config_info["camera.0.streamL.bitrate"]),
                              "camera.0.streamL.format":config_info["camera.0.streamL.format"],
                              "camera.0.streamL.framerate":parseInt(config_info["camera.0.streamL.framerate"]),
                              "camera.0.streamL.gopsize":parseInt(config_info["camera.0.streamL.gopsize"]),
                              "camera.0.streamL.storage.diskquota":parseFloat(config_info["camera.0.streamL.storage.diskquota"]),
                              "camera.0.streamL.storage.recordevents":config_info["camera.0.streamL.storage.recordevents"],
                              "camera.0.streamL.storage.recordmode":parseInt(config_info["camera.0.streamL.storage.recordmode"]),
                              "camera.0.streamL.storage.recordpostamble":parseInt(config_info["camera.0.streamL.storage.recordpostamble"]),
                              "camera.0.streamL.storage.recordpreamble":parseInt(config_info["camera.0.streamL.storage.recordpreamble"]),
                              "camera.1.enabled":config_info["camera.1.enabled"],
                              "camera.1.streamH.bitrate":parseInt(config_info["camera.1.streamH.bitrate"]),
                              "camera.1.streamH.format":config_info["camera.1.streamH.format"],
                              "camera.1.streamH.framerate":parseInt(config_info["camera.1.streamH.framerate"]),
                              "camera.1.streamH.gopsize":parseInt(config_info["camera.1.streamH.gopsize"]),
                              "camera.1.streamH.storage.recordevents":config_info["camera.1.streamH.storage.recordevents"],
                              "camera.1.streamH.storage.recordpostamble":parseInt(config_info["camera.1.streamH.storage.recordpostamble"]),
                              "camera.1.streamH.storage.recordpreamble":parseInt(config_info["camera.1.streamH.storage.recordpreamble"]),
                              "camera.1.streamH.storage.vaqa.0.duration":config_info["camera.1.streamH.storage.vaqa.0.duration"],
                              "camera.1.streamH.storage.vaqa.0.interval":config_info["camera.1.streamH.storage.vaqa.0.interval"],
                              "camera.1.streamH.storage.vaqa.0.qty":parseInt(config_info["camera.1.streamH.storage.vaqa.0.qty"]),
                              "camera.1.streamH.storage.vaqa.1.duration":config_info["camera.1.streamH.storage.vaqa.1.duration"],
                              "camera.1.streamH.storage.vaqa.1.interval":config_info["camera.1.streamH.storage.vaqa.1.interval"],
                              "camera.1.streamH.storage.vaqa.1.qty":parseInt(config_info["camera.1.streamH.storage.vaqa.1.qty"]),
                              "camera.1.streamL.bitrate":parseInt(config_info["camera.1.streamL.bitrate"]),
                              "camera.1.streamL.format":config_info["camera.1.streamL.format"],
                              "camera.1.streamL.framerate":parseInt(config_info["camera.1.streamL.framerate"]),
                              "camera.1.streamL.gopsize":parseInt(config_info["camera.1.streamL.gopsize"]),
                              "camera.1.streamL.storage.recordevents":config_info["camera.1.streamL.storage.recordevents"],
                              "camera.1.streamL.storage.recordpostamble":parseInt(config_info["camera.1.streamL.storage.recordpostamble"]),
                              "camera.1.streamL.storage.recordpreamble":parseInt(config_info["camera.1.streamL.storage.recordpreamble"]),
                              "evtgrab.capint":parseInt(config_info["evtgrab.capint"]),
                              "evtgrab.dint":parseInt(config_info["evtgrab.dint"]),
                              "evtgrab.nfiles":parseInt(config_info["evtgrab.nfiles"]),
                              "evtgrab.periodic_only":config_info["evtgrab.periodic_only"],
                              "mediaserver.nomve":config_info["mediaserver.nomve"],
                              "mediaserver.rtspsrc":config_info["mediaserver.rtspsrc"],
                              "mediaserver.video_codec":config_info["mediaserver.video_codec"],
                              "mediaserver.wmm_qos":config_info["mediaserver.wmm_qos"],
                              "network.eth-x.ip":config_info["network.eth-x.ip"],
                              "network.eth-x.method":config_info["network.eth-x.method"],
                              "network.eth-x.netmask":parseInt(config_info["network.eth-x.netmask"]),
                              "network.firewall.ports":config_info["network.firewall.ports"],
                              "network.firewall.protocols":config_info["network.firewall.protocols"],
                              "network.server.mqtt":config_info["network.server.mqtt"],
                              "network.server.vpn":config_info["network.server.vpn"],
                              "network.vpn_on_demand":config_info["network.vpn_on_demand"],
                              "rtsp.service":parseInt(config_info["rtsp.service"]),
                              "server":config_info.server,
                              "storage.maxsessions":parseInt(config_info["storage.maxsessions"]),
                              "storageserver.cache_size":parseInt(config_info["storageserver.cache_size"]),
                              "storageserver.max_clip_size":parseInt(config_info["storageserver.max_clip_size"]),
                            };

          }
          else if (config_info.model == "falcon-q"){
            configObject = {  "name": config_info.name,
                              "model": config_info.model,
                              "configid":config_info.configid,
                              "Genetec.custom_events":config_info["Genetec.custom_events"],
                              "alarm.redis.memory_threshold":parseFloat(config_info["alarm.redis.memory_threshold"]),
                              "application_server":config_info.application_server,
                              "application_server_port":parseInt(config_info.application_server_port),
                              "camera.0.autoexposure":config_info["camera.0.autoexposure"],
                              "camera.0.autogain":config_info["camera.0.autogain"],
                              "camera.0.colortemp":parseInt(config_info["camera.0.colortemp"]),
                              "camera.0.enabled":config_info["camera.0.enabled"],
                              "camera.0.exposure":parseInt(config_info["camera.0.exposure"]),
                              "camera.0.flickermode":config_info["camera.0.flickermode"],
                              "camera.0.gain":parseInt(config_info["camera.0.gain"]),
                              "camera.0.indoormode":config_info["camera.0.indoormode"],
                              "camera.0.master":parseInt(config_info["camera.0.master"]),
                              "camera.0.scanmode":config_info["camera.0.scanmode"],
                              "camera.0.streamH.bitrate":parseInt(config_info["camera.0.streamH.bitrate"]),
                              "camera.0.streamH.format":config_info["camera.0.streamH.format"],
                              "camera.0.streamH.framerate":parseInt(config_info["camera.0.streamH.framerate"]),
                              "camera.0.streamH.gopsize":parseInt(config_info["camera.0.streamH.gopsize"]),
                              "camera.0.streamH.storage.diskquota":parseFloat(config_info["camera.0.streamH.storage.diskquota"]),
                              "camera.0.streamH.storage.recordevents":config_info["camera.0.streamH.storage.recordevents"],
                              "camera.0.streamH.storage.recordmode":parseInt(config_info["camera.0.streamH.storage.recordmode"]),
                              "camera.0.streamH.storage.recordpostamble":parseInt(config_info["camera.0.streamH.storage.recordpostamble"]),
                              "camera.0.streamH.storage.recordpreamble":parseInt(config_info["camera.0.streamH.storage.recordpreamble"]),
                              "camera.0.streamH.storage.vaqa.0.duration":config_info["camera.0.streamH.storage.vaqa.0.duration"],
                              "camera.0.streamH.storage.vaqa.0.interval":config_info["camera.0.streamH.storage.vaqa.0.interval"],
                              "camera.0.streamH.storage.vaqa.0.qty":parseInt(config_info["camera.0.streamH.storage.vaqa.0.qty"]),
                              "camera.0.streamH.storage.vaqa.1.duration":config_info["camera.0.streamH.storage.vaqa.1.duration"],
                              "camera.0.streamH.storage.vaqa.1.interval":config_info["camera.0.streamH.storage.vaqa.1.interval"],
                              "camera.0.streamH.storage.vaqa.1.qty":parseInt(config_info["camera.0.streamH.storage.vaqa.1.qty"]),
                              "camera.0.streamL.bitrate":parseInt(config_info["camera.0.streamL.bitrate"]),
                              "camera.0.streamL.format":config_info["camera.0.streamL.format"],
                              "camera.0.streamL.framerate":parseInt(config_info["camera.0.streamL.framerate"]),
                              "camera.0.streamL.gopsize":parseInt(config_info["camera.0.streamL.gopsize"]),
                              "camera.0.streamL.storage.diskquota":parseFloat(config_info["camera.0.streamL.storage.diskquota"]),
                              "camera.0.streamL.storage.recordevents":config_info["camera.0.streamL.storage.recordevents"],
                              "camera.0.streamL.storage.recordmode":parseInt(config_info["camera.0.streamL.storage.recordmode"]),
                              "camera.0.streamL.storage.recordpostamble":parseInt(config_info["camera.0.streamL.storage.recordpostamble"]),
                              "camera.0.streamL.storage.recordpreamble":parseInt(config_info["camera.0.streamL.storage.recordpreamble"]),
                              "camera.0.wdrmode":config_info["camera.0.wdrmode"],
                              "camera.1.autoexposure":config_info["camera.1.autoexposure"],
                              "camera.1.autogain":config_info["camera.1.autogain"],
                              "camera.1.colortemp":parseInt(config_info["camera.1.colortemp"]),
                              "camera.1.enabled":config_info["camera.1.enabled"],
                              "camera.1.exposure":parseInt(config_info["camera.1.exposure"]),
                              "camera.1.flickermode":config_info["camera.1.flickermode"],
                              "camera.1.gain":parseInt(config_info["camera.1.gain"]),
                              "camera.1.indoormode":config_info["camera.1.indoormode"],
                              "camera.1.scanmode":config_info["camera.1.scanmode"],
                              "camera.1.streamH.bitrate":parseInt(config_info["camera.1.streamH.bitrate"]),
                              "camera.1.streamH.format":config_info["camera.1.streamH.format"],
                              "camera.1.streamH.framerate":parseInt(config_info["camera.1.streamH.framerate"]),
                              "camera.1.streamH.gopsize":parseInt(config_info["camera.1.streamH.gopsize"]),
                              "camera.1.streamH.storage.diskquota":parseFloat(config_info["camera.1.streamH.storage.diskquota"]),
                              "camera.1.streamH.storage.recordevents":config_info["camera.1.streamH.storage.recordevents"],
                              "camera.1.streamH.storage.recordmode":parseInt(config_info["camera.1.streamH.storage.recordmode"]),
                              "camera.1.streamH.storage.recordpostamble":parseInt(config_info["camera.1.streamH.storage.recordpostamble"]),
                              "camera.1.streamH.storage.recordpreamble":parseInt(config_info["camera.1.streamH.storage.recordpreamble"]),
                              "camera.1.streamH.storage.vaqa.0.duration":config_info["camera.1.streamH.storage.vaqa.0.duration"],
                              "camera.1.streamH.storage.vaqa.0.interval":config_info["camera.1.streamH.storage.vaqa.0.interval"],
                              "camera.1.streamH.storage.vaqa.0.qty":parseInt(config_info["camera.1.streamH.storage.vaqa.0.qty"]),
                              "camera.1.streamH.storage.vaqa.1.duration":config_info["camera.1.streamH.storage.vaqa.1.duration"],
                              "camera.1.streamH.storage.vaqa.1.interval":config_info["camera.1.streamH.storage.vaqa.1.interval"],
                              "camera.1.streamH.storage.vaqa.1.qty":parseInt(config_info["camera.1.streamH.storage.vaqa.1.qty"]),
                              "camera.1.streamL.bitrate":parseInt(config_info["camera.1.streamL.bitrate"]),
                              "camera.1.streamL.format":config_info["camera.1.streamL.format"],
                              "camera.1.streamL.framerate":parseInt(config_info["camera.1.streamL.framerate"]),
                              "camera.1.streamL.gopsize":parseInt(config_info["camera.1.streamL.gopsize"]),
                              "camera.1.streamL.storage.diskquota":parseFloat(config_info["camera.1.streamL.storage.diskquota"]),
                              "camera.1.streamL.storage.recordevents":config_info["camera.1.streamL.storage.recordevents"],
                              "camera.1.streamL.storage.recordmode":parseInt(config_info["camera.1.streamL.storage.recordmode"]),
                              "camera.1.streamL.storage.recordpostamble":parseInt(config_info["camera.1.streamL.storage.recordpostamble"]),
                              "camera.1.streamL.storage.recordpreamble":parseInt(config_info["camera.1.streamL.storage.recordpreamble"]),
                              "camera.1.wdrmode":config_info["camera.1.wdrmode"],
                              "evtgrab.capint":parseInt(config_info["evtgrab.capint"]),
                              "evtgrab.dint":parseInt(config_info["evtgrab.dint"]),
                              "evtgrab.nfiles":parseInt(config_info["evtgrab.nfiles"]),
                              "evtgrab.periodic_only":config_info["evtgrab.periodic_only"],
                              "mediaserver.nomve":config_info["mediaserver.nomve"],
                              "mediaserver.video_codec":config_info["mediaserver.video_codec"],
                              "mediaserver.wmm_qos":config_info["mediaserver.wmm_qos"],
                              "network.eth-x.ip":config_info["network.eth-x.ip"],
                              "network.eth-x.leasetime":config_info["network.eth-x.leasetime"],
                              "network.eth-x.method":config_info["network.eth-x.method"],
                              "network.eth-x.netmask":parseInt(config_info["network.eth-x.netmask"]),
                              "network.eth-x.port-forwarding.genetec":config_info["network.eth-x.port-forwarding.genetec"],
                              "network.eth-x.port-forwarding.https":config_info["network.eth-x.port-forwarding.https"],
                              "network.eth-x.port-forwarding.rtsp":config_info["network.eth-x.port-forwarding.rtsp"],
                              "network.eth-x.port-forwarding.ssh":config_info["network.eth-x.port-forwarding.ssh"],
                              "network.eth-x.whitelist":config_info["network.eth-x.whitelist"],
                              "network.firewall.ports":config_info["network.firewall.ports"],
                              "network.firewall.protocols":config_info["network.firewall.protocols"],
                              "network.region":config_info["network.region"],
                              "network.server.mqtt":config_info["network.server.mqtt"],
                              "network.server.vpn":config_info["network.server.vpn"],
                              "network.vpn_on_demand":config_info["network.vpn_on_demand"],
                              "network.wlan-x.security":config_info["network.wlan-x.security"],
                              "network.wlan-x.security.bgscan-long-interval":parseInt(config_info["network.wlan-x.security.bgscan-long-interval"]),
                              "network.wlan-x.security.bgscan-short-interval":parseInt(config_info["network.wlan-x.security.bgscan-short-interval"]),
                              "network.wlan-x.security.bgscan-signal-threshold":parseInt(config_info["network.wlan-x.security.bgscan-signal-threshold"]),
                              "network.wlan-x.security.psk":config_info["network.wlan-x.security.psk"],
                              "network.wlan-x.ssid":config_info["network.wlan-x.ssid"],
                              "network.wlan-y.security":config_info["network.wlan-y.security"],
                              "network.wlan-y.security.bgscan-long-interval":parseInt(config_info["network.wlan-y.security.bgscan-long-interval"]),
                              "network.wlan-y.security.bgscan-short-interval":parseInt(config_info["network.wlan-y.security.bgscan-short-interval"]),
                              "network.wlan-y.security.bgscan-signal-threshold":parseInt(config_info["network.wlan-y.security.bgscan-signal-threshold"]),
                              "network.wlan-y.security.psk":config_info["network.wlan-y.security.psk"],
                              "network.wlan-y.ssid":config_info["network.wlan-y.ssid"],
                              "rtsp.service":parseInt(config_info["rtsp.service"]),
                              "server":config_info.server,
                              "storage.maxsessions":parseInt(config_info["storage.maxsessions"]),
                              "storageserver.cache_size":parseInt(config_info["storageserver.cache_size"]),
                              "storageserver.max_clip_size":parseInt(config_info["storageserver.max_clip_size"]),
                            };

          }
          else if (config_info.model == "cnext"){
            configObject = {  "name": config_info.name,
                              "model": config_info.model,
                              "alarm.redis.memory_threshold":parseFloat(config_info["alarm.redis.memory_threshold"]),
                              "application_server":config_info.application_server,
                              "application_server_port":parseInt(config_info.application_server_port),
                              "lctrl.dimmer.vmax":parseInt(config_info["lctrl.dimmer.vmax"]),
                              "lctrl.ltype":parseInt(config_info["lctrl.ltype"]),
                              "lctrl.lvlrate.fast":parseInt(config_info["lctrl.lvlrate.fast"]),
                              "lctrl.lvlrate.light":parseInt(config_info["lctrl.lvlrate.light"]),
                              "lctrl.lvlrate.motion":parseInt(config_info["lctrl.lvlrate.motion"]),
                              "lctrl.lvlrate.norm":parseInt(config_info["lctrl.lvlrate.norm"]),
                              "lctrl.trigger.nonett":parseInt(config_info["lctrl.trigger.nonett"]),
                              "lctrl.trigger.off.dint":parseInt(config_info["lctrl.trigger.off.dint"]),
                              "lctrl.trigger.offt":parseInt(config_info["lctrl.trigger.offt"]),
                              "lctrl.trigger.on.dint":parseInt(config_info["lctrl.trigger.on.dint"]),
                              "lctrl.trigger.ont":parseInt(config_info["lctrl.trigger.ont"]),
                              "network.eth-x.ip":config_info["network.eth-x.ip"],
                              "network.eth-x.leasetime":config_info["network.eth-x.leasetime"],
                              "network.eth-x.method":config_info["network.eth-x.method"],
                              "network.eth-x.netmask":parseInt(config_info["network.eth-x.netmask"]),
                              "network.firewall.ports":config_info["network.firewall.ports"],
                              "network.firewall.protocols":config_info["network.firewall.protocols"],
                              "network.ppp-x.apn":config_info["network.ppp-x.apn"],
                              "network.server.mqtt":config_info["network.server.mqtt"],
                              "network.server.vpn":config_info["network.server.vpn"],
                              "network.vpn_on_demand":config_info["network.vpn_on_demand"],
                              "network.wlan-x.security":config_info["network.wlan-x.security"],
                              "network.wlan-x.security.bgscan-long-interval":parseInt(config_info["network.wlan-x.security.bgscan-long-interval"]),
                              "network.wlan-x.security.bgscan-short-interval":parseInt(config_info["network.wlan-x.security.bgscan-short-interval"]),
                              "network.wlan-x.security.bgscan-signal-threshold":parseInt(config_info["network.wlan-x.security.bgscan-signal-threshold"]),
                              "network.wlan-x.security.psk":config_info["network.wlan-x.security.psk"],
                              "network.wlan-x.ssid":config_info["network.wlan-x.ssid"],
                              "network.wlan-y.security":config_info["network.wlan-y.security"],
                              "network.wlan-y.security.bgscan-long-interval":parseInt(config_info["network.wlan-y.security.bgscan-long-interval"]),
                              "network.wlan-y.security.bgscan-short-interval":parseInt(config_info["network.wlan-y.security.bgscan-short-interval"]),
                              "network.wlan-y.security.bgscan-signal-threshold":parseInt(config_info["network.wlan-y.security.bgscan-signal-threshold"]),
                              "network.wlan-y.security.psk":config_info["network.wlan-y.security.psk"],
                              "network.wlan-y.ssid":config_info["network.wlan-y.ssid"],
                              "podbus.disable":config_info["podbus.disable"],
                              "poe.enabled":config_info["poe.enabled"],
                              "pwr.noload.i":parseInt(config_info["pwr.noload.i"]),
                              "pwr.noload.mode":parseInt(config_info["pwr.noload.mode"]),
                              "server":config_info.server,
                            };

          }
          else if (config_info.model == "unode-v2"){
            configObject = {  "name": config_info.name,
                              "model": config_info.model,
                              "configid":config_info.configid,
                              "networkXPasskey":config_info.networkXPasskey,
                              "networkXSSID":config_info.networkXSSID,
                              "networkXSecurity":config_info.networkXSecurity,
                              "networkYPasskey":config_info.networkYPasskey,
                              "networkYSSID":config_info.networkYSSID,
                              "networkYSecurity":config_info.networkYSecurity,
                              "network_region":config_info.network_region,
                              "debugmode":config_info.debugmode,
                              "telnet":config_info.telnet,
                              "sensor_rf_pint":config_info.sensor_rf_pint * 1000,
                              "sensor_rf_dint":config_info.sensor_rf_dint * 1000,
                              "sensor_rf_mode":config_info.sensor_rf_mode,
                              "sensor_v_pint":config_info.sensor_v_pint * 1000,
                              "sensor_v_dint":config_info.sensor_v_dint * 1000,
                              "sensor_v_mode":config_info.sensor_v_mode,
                              "sensor_lIR_pint":config_info.sensor_lIR_pint * 1000,
                              "sensor_lIR_dint":config_info.sensor_lIR_dint  * 1000,
                              "sensor_lIR_mode":config_info.sensor_lIR_mode,
                              "sensor_l_pint":config_info.sensor_l_pint * 1000,
                              "sensor_l_dint":config_info.sensor_l_dint * 1000,
                              "sensor_l_mode":config_info.sensor_l_mode,
                              "sensor_p_pint":config_info.sensor_p_pint * 1000,
                              "sensor_p_dint":config_info.sensor_p_dint * 1000,
                              "sensor_p_mode":config_info.sensor_p_mode,
                              "sensor_t_pint":config_info.sensor_t_pint * 1000,
                              "sensor_t_dint":config_info.sensor_t_dint * 1000,
                              "sensor_t_mode":config_info.sensor_t_mode,
                              "sensor_T_pint":config_info.sensor_T_pint * 1000,
                              "sensor_T_dint":config_info.sensor_T_dint * 1000,
                              "sensor_T_mode":config_info.sensor_T_mode,
                              "sensor_pdc_pint":config_info.sensor_pdc_pint * 1000,
                              "sensor_pdc_dint":config_info.sensor_pdc_dint * 1000,
                              "sensor_pdc_mode":config_info.sensor_pdc_mode,
                              "sensor_ppr_pint":config_info.sensor_ppr_pint * 1000,
                              "sensor_ppr_dint":config_info.sensor_ppr_dint * 1000,
                              "sensor_ppr_mode":config_info.sensor_ppr_mode,
                              "sensor_pnd_pint":config_info.sensor_pnd_pint * 1000,
                              "sensor_pnd_dint":config_info.sensor_pnd_dint * 1000,
                              "sensor_pnd_mode":config_info.sensor_pnd_mode,
                              "sensor_pdt_pint":config_info.sensor_pdt_pint * 1000,
                              "sensor_pdt_dint":config_info.sensor_pdt_dint * 1000,
                              "sensor_pdt_mode":config_info.sensor_pdt_mode,
                              "lctrl_dimmer_vmax":config_info.lctrl_dimmer_vmax,
                          };
           
          }
          else if (config_info.model == "unode-v3"){
            configObject = {  "name": config_info.name,
                              "model": config_info.model,
                              "configid":config_info.configid,
                              "networkXPasskey":config_info.networkXPasskey,
                              "networkXSSID":config_info.networkXSSID,
                              "networkXSecurity":config_info.networkXSecurity,
                              "networkYPasskey":config_info.networkYPasskey,
                              "networkYSSID":config_info.networkYSSID,
                              "networkYSecurity":config_info.networkYSecurity,
                              "network_region":config_info.network_region,
                              "debugmode":config_info.debugmode,
                              "telnet":config_info.telnet,
                              "sensor_rf_pint":config_info.sensor_rf_pint * 1000,
                              "sensor_rf_dint":config_info.sensor_rf_dint * 1000,
                              "sensor_rf_mode":config_info.sensor_rf_mode,
                              "sensor_v_pint":config_info.sensor_v_pint * 1000,
                              "sensor_v_dint":config_info.sensor_v_dint * 1000,
                              "sensor_v_mode":config_info.sensor_v_mode,
                              "sensor_mi_pint":config_info.sensor_mi_pint * 1000,
                              "sensor_mi_dint":config_info.sensor_mi_dint * 1000,
                              "sensor_mi_mode":config_info.sensor_mi_mode,
                              "sensor_lIR_pint":config_info.sensor_lIR_pint * 1000,
                              "sensor_lIR_dint":config_info.sensor_lIR_dint  * 1000,
                              "sensor_lIR_mode":config_info.sensor_lIR_mode,
                              "sensor_l_pint":config_info.sensor_l_pint * 1000,
                              "sensor_l_dint":config_info.sensor_l_dint * 1000,
                              "sensor_l_mode":config_info.sensor_l_mode,
                              "sensor_p_pint":config_info.sensor_p_pint * 1000,
                              "sensor_p_dint":config_info.sensor_p_dint * 1000,
                              "sensor_p_mode":config_info.sensor_p_mode,
                              "sensor_t_pint":config_info.sensor_t_pint * 1000,
                              "sensor_t_dint":config_info.sensor_t_dint * 1000,
                              "sensor_t_mode":config_info.sensor_t_mode,
                              "sensor_T_pint":config_info.sensor_T_pint * 1000,
                              "sensor_T_dint":config_info.sensor_T_dint * 1000,
                              "sensor_T_mode":config_info.sensor_T_mode,
                              "sensor_pdc_pint":config_info.sensor_pdc_pint * 1000,
                              "sensor_pdc_dint":config_info.sensor_pdc_dint * 1000,
                              "sensor_pdc_mode":config_info.sensor_pdc_mode,
                              "sensor_ppr_pint":config_info.sensor_ppr_pint * 1000,
                              "sensor_ppr_dint":config_info.sensor_ppr_dint * 1000,
                              "sensor_ppr_mode":config_info.sensor_ppr_mode,
                              "sensor_pnd_pint":config_info.sensor_pnd_pint * 1000,
                              "sensor_pnd_dint":config_info.sensor_pnd_dint * 1000,
                              "sensor_pnd_mode":config_info.sensor_pnd_mode,
                              "sensor_pdt_pint":config_info.sensor_pdt_pint * 1000,
                              "sensor_pdt_dint":config_info.sensor_pdt_dint * 1000,
                              "sensor_pdt_mode":config_info.sensor_pdt_mode,
                              "sensor_a_jdelta":parseInt(config_info.sensor_a_jdelta),
                              "lctrl_dimmer_vmax":config_info.lctrl_dimmer_vmax,
                          };
           
          }
          else if (config_info.model == "unode-v4"){
            configObject = {  "name": config_info.name,
                              "model": config_info.model,
                              "configid":config_info.configid,
                              "networkXPasskey":config_info.networkXPasskey,
                              "networkXSSID":config_info.networkXSSID,
                              "networkXSecurity":config_info.networkXSecurity,
                              "networkYPasskey":config_info.networkYPasskey,
                              "networkYSSID":config_info.networkYSSID,
                              "networkYSecurity":config_info.networkYSecurity,
                              "network_region":config_info.network_region,
                              "debugmode":config_info.debugmode,
                              "telnet":config_info.telnet,
                              "aux_power":config_info.aux_power,
                              "podbus_disable":config_info.podbus_disable,
                              "sensor_rf_pint":config_info.sensor_rf_pint * 1000,
                              "sensor_rf_dint":config_info.sensor_rf_dint * 1000,
                              "sensor_rf_mode":config_info.sensor_rf_mode,
                              "sensor_v_pint":config_info.sensor_v_pint * 1000,
                              "sensor_v_dint":config_info.sensor_v_dint * 1000,
                              "sensor_v_mode":config_info.sensor_v_mode,
                              "sensor_aw_pint":config_info.sensor_aw_pint * 1000,
                              "sensor_aw_dint":config_info.sensor_aw_dint * 1000,
                              "sensor_aw_mode":config_info.sensor_aw_mode,
                              "sensor_aPF_pint":config_info.sensor_aPF_pint * 1000,
                              "sensor_aPF_dint":config_info.sensor_aPF_dint * 1000,
                              "sensor_aPF_mode":config_info.sensor_aPF_mode,
                              "sensor_aP_pint":config_info.sensor_aP_pint * 1000,
                              "sensor_aP_dint":config_info.sensor_aP_dint * 1000,
                              "sensor_aP_mode":config_info.sensor_aP_mode,
                              "sensor_mP_pint":config_info.sensor_mP_pint * 1000,
                              "sensor_mP_dint":config_info.sensor_mP_dint * 1000,
                              "sensor_mP_mode":config_info.sensor_mP_mode,
                              "sensor_mip_pint":config_info.sensor_mip_pint * 1000,
                              "sensor_mip_dint":config_info.sensor_mip_dint * 1000,
                              "sensor_mip_mode":config_info.sensor_mip_mode,
                              "sensor_ai_pint":config_info.sensor_ai_pint * 1000,
                              "sensor_ai_dint":config_info.sensor_ai_dint * 1000,
                              "sensor_ai_mode":config_info.sensor_ai_mode,
                              "sensor_aip_pint":config_info.sensor_aip_pint * 1000,
                              "sensor_aip_dint":config_info.sensor_aip_dint * 1000,
                              "sensor_aip_mode":config_info.sensor_aip_mode,
                              "sensor_mi_pint":config_info.sensor_mi_pint * 1000,
                              "sensor_mi_dint":config_info.sensor_mi_dint * 1000,
                              "sensor_mi_mode":config_info.sensor_mi_mode,
                              "sensor_mw_pint":config_info.sensor_mw_pint * 1000,
                              "sensor_mw_dint":config_info.sensor_mw_dint * 1000,
                              "sensor_mw_mode":config_info.sensor_mw_mode,
                              "sensor_mPF_pint":config_info.sensor_mPF_pint * 1000,
                              "sensor_mPF_dint":config_info.sensor_mPF_dint * 1000,
                              "sensor_mPF_mode":config_info.sensor_mPF_mode,
                              "sensor_lIR_pint":config_info.sensor_lIR_pint * 1000,
                              "sensor_lIR_dint":config_info.sensor_lIR_dint  * 1000,
                              "sensor_lIR_mode":config_info.sensor_lIR_mode,
                              "sensor_l_pint":config_info.sensor_l_pint * 1000,
                              "sensor_l_dint":config_info.sensor_l_dint * 1000,
                              "sensor_l_mode":config_info.sensor_l_mode,
                              "sensor_p_pint":config_info.sensor_p_pint * 1000,
                              "sensor_p_dint":config_info.sensor_p_dint * 1000,
                              "sensor_p_mode":config_info.sensor_p_mode,
                              "sensor_a_jdelta":parseInt(config_info.sensor_a_jdelta),
                              "sensor_pc_pint":config_info.sensor_pc_pint * 1000,
                              "sensor_pc_dint":config_info.sensor_pc_dint * 1000,
                              "sensor_pc_mode":config_info.sensor_pc_mode,
                              "sensor_t_pint":config_info.sensor_t_pint * 1000,
                              "sensor_t_dint":config_info.sensor_t_dint * 1000,
                              "sensor_t_mode":config_info.sensor_t_mode,
                              "sensor_T_pint":config_info.sensor_T_pint * 1000,
                              "sensor_T_dint":config_info.sensor_T_dint * 1000,
                              "sensor_T_mode":config_info.sensor_T_mode,
                              "sensor_mt_pint":config_info.sensor_mt_pint * 1000,
                              "sensor_mt_dint":config_info.sensor_mt_dint * 1000,
                              "sensor_mt_mode":config_info.sensor_mt_mode,
                              "sensor_podm_pint":config_info.sensor_podm_pint * 1000,
                              "sensor_podm_dint":config_info.sensor_podm_dint * 1000,
                              "sensor_podm_mode":config_info.sensor_podm_mode,
                              "sensor_podme_pint":config_info.sensor_podme_pint * 1000,
                              "sensor_podme_dint":config_info.sensor_podme_dint * 1000,
                              "sensor_podme_mode":config_info.sensor_podme_mode,
                              "sensor_rlym_pint":config_info.sensor_rlym_pint * 1000,
                              "lctrl_dimmer_vmax":config_info.lctrl_dimmer_vmax,
                              "lctrl_trigger_off_dint":config_info.lctrl_trigger_off_dint * 1000,
                              "lctrl_trigger_on_dint":config_info.lctrl_trigger_on_dint * 1000,
                              "lctrl_trigger_ont":config_info.lctrl_trigger_ont * 1000,
                              "lctrl_trigger_offt":config_info.lctrl_trigger_offt * 1000,
                              "lctrl_trigger_nonett":config_info.lctrl_trigger_nonett * 1000,
                          };

          }
          else if (config_info.model == "unode-v5"){
            configObject = {  "name": config_info.name,
                              "model": config_info.model,
                              "configid":config_info.configid,
                              "network_region":config_info.network_region,
                              "debugmode":config_info.debugmode,
                              "telnet":config_info.telnet,
                              "aux_power":config_info.aux_power,
                              "podbus_disable":config_info.podbus_disable,
                              "sensor_rf_pint":config_info.sensor_rf_pint * 1000,
                              "sensor_rf_dint":config_info.sensor_rf_dint * 1000,
                              "sensor_rf_mode":config_info.sensor_rf_mode,
                              "sensor_v_pint":config_info.sensor_v_pint * 1000,
                              "sensor_v_dint":config_info.sensor_v_dint * 1000,
                              "sensor_v_mode":config_info.sensor_v_mode,
                              "sensor_aw_pint":config_info.sensor_aw_pint * 1000,
                              "sensor_aw_dint":config_info.sensor_aw_dint * 1000,
                              "sensor_aw_mode":config_info.sensor_aw_mode,
                              "sensor_aPF_pint":config_info.sensor_aPF_pint * 1000,
                              "sensor_aPF_dint":config_info.sensor_aPF_dint * 1000,
                              "sensor_aPF_mode":config_info.sensor_aPF_mode,
                              "sensor_aP_pint":config_info.sensor_aP_pint * 1000,
                              "sensor_aP_dint":config_info.sensor_aP_dint * 1000,
                              "sensor_aP_mode":config_info.sensor_aP_mode,
                              "sensor_mP_pint":config_info.sensor_mP_pint * 1000,
                              "sensor_mP_dint":config_info.sensor_mP_dint * 1000,
                              "sensor_mP_mode":config_info.sensor_mP_mode,
                              "sensor_mip_pint":config_info.sensor_mip_pint * 1000,
                              "sensor_mip_dint":config_info.sensor_mip_dint * 1000,
                              "sensor_mip_mode":config_info.sensor_mip_mode,
                              "sensor_ai_pint":config_info.sensor_ai_pint * 1000,
                              "sensor_ai_dint":config_info.sensor_ai_dint * 1000,
                              "sensor_ai_mode":config_info.sensor_ai_mode,
                              "sensor_aip_pint":config_info.sensor_aip_pint * 1000,
                              "sensor_aip_dint":config_info.sensor_aip_dint * 1000,
                              "sensor_aip_mode":config_info.sensor_aip_mode,
                              "sensor_mi_pint":config_info.sensor_mi_pint * 1000,
                              "sensor_mi_dint":config_info.sensor_mi_dint * 1000,
                              "sensor_mi_mode":config_info.sensor_mi_mode,
                              "sensor_mw_pint":config_info.sensor_mw_pint * 1000,
                              "sensor_mw_dint":config_info.sensor_mw_dint * 1000,
                              "sensor_mw_mode":config_info.sensor_mw_mode,
                              "sensor_mPF_pint":config_info.sensor_mPF_pint * 1000,
                              "sensor_mPF_dint":config_info.sensor_mPF_dint * 1000,
                              "sensor_mPF_mode":config_info.sensor_mPF_mode,
                              "sensor_lIR_pint":config_info.sensor_lIR_pint * 1000,
                              "sensor_lIR_dint":config_info.sensor_lIR_dint  * 1000,
                              "sensor_lIR_mode":config_info.sensor_lIR_mode,
                              "sensor_l_pint":config_info.sensor_l_pint * 1000,
                              "sensor_l_dint":config_info.sensor_l_dint * 1000,
                              "sensor_l_mode":config_info.sensor_l_mode,
                              "sensor_l_i_pint":config_info.sensor_l_i_pint * 1000,
                              "sensor_l_i_dint":config_info.sensor_l_i_dint * 1000,
                              "sensor_l_i_mode":config_info.sensor_l_i_mode,
                              "sensor_lIR_i_pint":config_info.sensor_lIR_i_pint * 1000,
                              "sensor_lIR_i_dint":config_info.sensor_lIR_i_dint * 1000,
                              "sensor_lIR_i_mode":config_info.sensor_lIR_i_mode,
                              "sensor_p_pint":config_info.sensor_p_pint * 1000,
                              "sensor_p_dint":config_info.sensor_p_dint * 1000,
                              "sensor_p_mode":config_info.sensor_p_mode,
                              "sensor_pc_pint":config_info.sensor_pc_pint * 1000,
                              "sensor_pc_dint":config_info.sensor_pc_dint * 1000,
                              "sensor_pc_mode":config_info.sensor_pc_mode,
                              "sensor_t_pint":config_info.sensor_t_pint * 1000,
                              "sensor_t_dint":config_info.sensor_t_dint * 1000,
                              "sensor_t_mode":config_info.sensor_t_mode,
                              "sensor_T_pint":config_info.sensor_T_pint * 1000,
                              "sensor_T_dint":config_info.sensor_T_dint * 1000,
                              "sensor_T_mode":config_info.sensor_T_mode,
                              "sensor_mt_pint":config_info.sensor_mt_pint * 1000,
                              "sensor_mt_dint":config_info.sensor_mt_dint * 1000,
                              "sensor_mt_mode":config_info.sensor_mt_mode,
                              "sensor_podm_pint":config_info.sensor_podm_pint * 1000,
                              "sensor_podm_dint":config_info.sensor_podm_dint * 1000,
                              "sensor_podm_mode":config_info.sensor_podm_mode,
                              "sensor_podme_pint":config_info.sensor_podme_pint * 1000,
                              "sensor_podme_dint":config_info.sensor_podme_dint * 1000,
                              "sensor_podme_mode":config_info.sensor_podme_mode,
                              "sensor_rlym_pint":config_info.sensor_rlym_pint * 1000,
                              "lctrl_dimmer_vmax":config_info.lctrl_dimmer_vmax,
                              "lctrl_trigger_off_dint":config_info.lctrl_trigger_off_dint * 1000,
                              "lctrl_trigger_on_dint":config_info.lctrl_trigger_on_dint * 1000,
                              "lctrl_trigger_ont":config_info.lctrl_trigger_ont * 1000,
                              "lctrl_trigger_offt":config_info.lctrl_trigger_offt * 1000,
                              "lctrl_trigger_nonett":config_info.lctrl_trigger_nonett * 1000,
                          };

          }
          else {
            configObject = {  "name": config_info.name,
                              "model": config_info.model,
                              "configid":config_info.configid,
                              "network_region":config_info.network_region,
                              "debugmode":config_info.debugmode,
                              "telnet":config_info.telnet,
                              "aux_power":config_info.aux_power,
                              "podbus_disable":config_info.podbus_disable,
                              "sensor_rf_pint":config_info.sensor_rf_pint * 1000,
                              "sensor_rf_dint":config_info.sensor_rf_dint * 1000,
                              "sensor_rf_mode":config_info.sensor_rf_mode,
                              "sensor_v_pint":config_info.sensor_v_pint * 1000,
                              "sensor_v_dint":config_info.sensor_v_dint * 1000,
                              "sensor_v_mode":config_info.sensor_v_mode,
                              "sensor_aw_pint":config_info.sensor_aw_pint * 1000,
                              "sensor_aw_dint":config_info.sensor_aw_dint * 1000,
                              "sensor_aw_mode":config_info.sensor_aw_mode,
                              "sensor_aPF_pint":config_info.sensor_aPF_pint * 1000,
                              "sensor_aPF_dint":config_info.sensor_aPF_dint * 1000,
                              "sensor_aPF_mode":config_info.sensor_aPF_mode,
                              "sensor_aP_pint":config_info.sensor_aP_pint * 1000,
                              "sensor_aP_dint":config_info.sensor_aP_dint * 1000,
                              "sensor_aP_mode":config_info.sensor_aP_mode,
                              "sensor_mP_pint":config_info.sensor_mP_pint * 1000,
                              "sensor_mP_dint":config_info.sensor_mP_dint * 1000,
                              "sensor_mP_mode":config_info.sensor_mP_mode,
                              "sensor_mip_pint":config_info.sensor_mip_pint * 1000,
                              "sensor_mip_dint":config_info.sensor_mip_dint * 1000,
                              "sensor_mip_mode":config_info.sensor_mip_mode,
                              "sensor_ai_pint":config_info.sensor_ai_pint * 1000,
                              "sensor_ai_dint":config_info.sensor_ai_dint * 1000,
                              "sensor_ai_mode":config_info.sensor_ai_mode,
                              "sensor_aip_pint":config_info.sensor_aip_pint * 1000,
                              "sensor_aip_dint":config_info.sensor_aip_dint * 1000,
                              "sensor_aip_mode":config_info.sensor_aip_mode,
                              "sensor_mi_pint":config_info.sensor_mi_pint * 1000,
                              "sensor_mi_dint":config_info.sensor_mi_dint * 1000,
                              "sensor_mi_mode":config_info.sensor_mi_mode,
                              "sensor_mw_pint":config_info.sensor_mw_pint * 1000,
                              "sensor_mw_dint":config_info.sensor_mw_dint * 1000,
                              "sensor_mw_mode":config_info.sensor_mw_mode,
                              "sensor_mPF_pint":config_info.sensor_mPF_pint * 1000,
                              "sensor_mPF_dint":config_info.sensor_mPF_dint * 1000,
                              "sensor_mPF_mode":config_info.sensor_mPF_mode,
                              "sensor_lIR_pint":config_info.sensor_lIR_pint * 1000,
                              "sensor_lIR_dint":config_info.sensor_lIR_dint  * 1000,
                              "sensor_lIR_mode":config_info.sensor_lIR_mode,
                              "sensor_l_pint":config_info.sensor_l_pint * 1000,
                              "sensor_l_dint":config_info.sensor_l_dint * 1000,
                              "sensor_l_mode":config_info.sensor_l_mode,
                              "sensor_l_i_pint":config_info.sensor_l_i_pint * 1000,
                              "sensor_l_i_dint":config_info.sensor_l_i_dint * 1000,
                              "sensor_l_i_mode":config_info.sensor_l_i_mode,
                              "sensor_lIR_i_pint":config_info.sensor_lIR_i_pint * 1000,
                              "sensor_lIR_i_dint":config_info.sensor_lIR_i_dint * 1000,
                              "sensor_lIR_i_mode":config_info.sensor_lIR_i_mode,
                              "sensor_p_pint":config_info.sensor_p_pint * 1000,
                              "sensor_p_dint":config_info.sensor_p_dint * 1000,
                              "sensor_p_mode":config_info.sensor_p_mode,
                              "sensor_pc_pint":config_info.sensor_pc_pint * 1000,
                              "sensor_pc_dint":config_info.sensor_pc_dint * 1000,
                              "sensor_pc_mode":config_info.sensor_pc_mode,
                              "sensor_t_pint":config_info.sensor_t_pint * 1000,
                              "sensor_t_dint":config_info.sensor_t_dint * 1000,
                              "sensor_t_mode":config_info.sensor_t_mode,
                              "sensor_T_pint":config_info.sensor_T_pint * 1000,
                              "sensor_T_dint":config_info.sensor_T_dint * 1000,
                              "sensor_T_mode":config_info.sensor_T_mode,
                              "sensor_mt_pint":config_info.sensor_mt_pint * 1000,
                              "sensor_mt_dint":config_info.sensor_mt_dint * 1000,
                              "sensor_mt_mode":config_info.sensor_mt_mode,
                              "sensor_podm_pint":config_info.sensor_podm_pint * 1000,
                              "sensor_podm_dint":config_info.sensor_podm_dint * 1000,
                              "sensor_podm_mode":config_info.sensor_podm_mode,
                              "sensor_podme_pint":config_info.sensor_podme_pint * 1000,
                              "sensor_podme_dint":config_info.sensor_podme_dint * 1000,
                              "sensor_podme_mode":config_info.sensor_podme_mode,
                              "sensor_rlym_pint":config_info.sensor_rlym_pint * 1000,
                              "lctrl_dimmer_vmax":config_info.lctrl_dimmer_vmax,
                              "lctrl_trigger_off_dint":config_info.lctrl_trigger_off_dint * 1000,
                              "lctrl_trigger_on_dint":config_info.lctrl_trigger_on_dint * 1000,
                              "lctrl_trigger_ont":config_info.lctrl_trigger_ont * 1000,
                              "lctrl_trigger_offt":config_info.lctrl_trigger_offt * 1000,
                              "lctrl_trigger_nonett":config_info.lctrl_trigger_nonett * 1000,
                          };

          }


        $.ajax({
          "url": NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/configs/' + config_info.configid,
          "type": "POST",
          "xhrFields": {
            withCredentials: true
          },
          "data": JSON.stringify(configObject),
          "dataType": "json",
          "contentType": "application/json",
          "processData": false,
          "success": function (data) {
            data.idx = idx;
            data.groups = that.recreateGroups(assignment.assigngroups, that.state.groups);
            data.assign = assignment.assign;
            data.sites = save_sites.splice(0);
            data.nodes = save_nodes.splice(0).length;
            data.model = helpers.modelName(data.model);

            if(data.lctrl_dimmer_vmax){
              data.lctrl_dimmer_vmax = selected_lctrl_dimmer_vmax;
            }
            if(data.networkXSecurity){
              data.networkXSecurity = selected_networkXSecurity;
            }
            if(data.networkYSecurity){
              data.networkYSecurity = selected_networkYSecurity;
            }

            if(data["camera.0.streamH.storage.recordmode"]){
              data["camera.0.streamH.storage.recordmode"] = selected_camera0streamHstoragerecordmode;
            }

            if(data["camera.0.streamL.storage.recordmode"]){
              data["camera.0.streamL.storage.recordmode"] = selected_camera0streamLstoragerecordmode;
            }

            if(data["camera.1.streamH.storage.recordmode"]){
              data["camera.1.streamH.storage.recordmode"] = selected_camera1streamHstoragerecordmode;
            }

            if(data["camera.1.streamL.storage.recordmode"]){
              data["camera.1.streamL.storage.recordmode"] = selected_camera1streamLstoragerecordmode;
            }

            var newState = React.addons.update(this.state, { configs: { [idx]: { $set: data } } });
            that.getAllConfigs();
            noty({ type: "success", text: 'Config "' + data.name + '" updated.' });
            ReactBootstrap.Dispatcher.emit("Configform.update.success", data);
            this.configAssignment(assignment, data.configid);
            this.setState(newState);
          }.bind(that),
          "error": function () {
            noty({ type: "error", text: 'Could not update config.' });
          }
        });
      }

    });
  },

  componentWillUnmount: function () {
    ReactBootstrap.Dispatcher.removeAllListeners("Configlist.select");
    ReactBootstrap.Dispatcher.removeAllListeners("Configlist.add");
    ReactBootstrap.Dispatcher.removeAllListeners("Configform.save");
    ReactBootstrap.Dispatcher.removeAllListeners("Configform.update");
    ReactBootstrap.Dispatcher.removeAllListeners("Configform.saveandapply");
    ReactBootstrap.Dispatcher.removeAllListeners("Configform.delete");
    ReactBootstrap.Dispatcher.removeAllListeners("Configform.modelSubmit");
    ReactBootstrap.Dispatcher.removeAllListeners("Configform.duplicateconfig");
  },

  hideConfigDetail() {
    this.setState({
      showConfigDetail: false
    })



  },

  render() {
    var hstyle = { height: helpers.calcHeight(90, 0) + "px !important" };
    if (this.state.configs && this.state.groups) {
      var Subpanels = (
        <div className="netsense-center-panel">
          <Col md={12} lg={12}>
            <PanelContainer>
              <Panel>
                <PanelBody style={hstyle}>
                  {/*<SearchOverlay overlayType='config' configs={this.state.configs} defaultmodel={this.state.defaultmodel} configID={this.state.configID} groups={this.state.groups} /> */}
                  <Configdetail show={this.state.showConfigDetail} hide={this.hideConfigDetail} configs={this.state.configs} groups={this.state.groups} defaultmodel={this.state.defaultmodel} configID={this.state.configID} />
                  <Configlist configs={this.state.configs} defaultmodel={this.state.defaultmodel} configID={this.state.configID} />
                </PanelBody>
              </Panel>
            </PanelContainer>
          </Col>
          {/*<Col md={12} lg={7}>*/}
          {/*<PanelContainer>*/}
          {/*<Panel>*/}
          {/*<PanelBody style={hstyle}>*/}
          {/*<ConfigDetail configs={this.state.configs} groups={this.state.groups} defaultmodel={this.state.defaultmodel} configID={this.state.configID} />*/}
          {/*</PanelBody>*/}
          {/*</Panel>*/}
          {/*</PanelContainer>*/}
          {/*</Col>*/}
        </div>
      );
      return (
        <Container id='body'>
          <Grid>
            <Row>
              <Col sm={12}>
                <Row>
                  {Subpanels}
                </Row>
              </Col>
            </Row>
          </Grid>
          <ConfigUpdateModal context="config" show={this.state.showselectedconfigs} entity={this.state.selectedconfig} />
          <ConfigDeleteWithNodesModal context="config" showdeletealert={this.state.deleteconfigwithnodesalert} entity={this.state.deleteselectedconfig} />

        </Container>
      );
    };
    return (
      <Container id='body'>
        <Grid>
          <Row>
            <Col sm={12}>
              <PanelContainer>
                <Panel>
                  <PanelBody style={hstyle}>
                    <h2 id="loadingmsg" style={{ paddingTop: "16%", textAlign: "center" }}>Loading...</h2>
                  </PanelBody>
                </Panel>
              </PanelContainer>
            </Col>
          </Row>
        </Grid>
      </Container>
    );
  }
});

export default class extends React.Component {
  render() {
    var classes = classNames({
      'container-open': this.props.open
    });

    return (
      <Container id='container' className={classes}>
        <Header />
        <Body />
      </Container>
    );
  }
}
