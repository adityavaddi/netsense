import classNames from 'classnames';
import {
    State,
    Navigation
} from 'react-router';

import helpers from 'global/utils/helpers';
import auth from 'global/utils/auth';
import Nodelist from 'components/nodes/nodelist';
import Nodemap from 'components/nodes/nodemap';
import Nodedetail from 'components/nodes/nodedetail';
import Nodemultidetail from 'components/nodes/nodemultidetail';
import ScheduleView from 'components/scheduleview';
import ImageView from 'components/imageview';
import ConfigView from 'components/configview';
import LocalConfigView from 'components/localconfigview';
import FactoryConfigView from 'components/factoryconfigview';
import ProvConfigView from 'components/provconfigview';
import FirmwareView from 'components/firmwareview';

import Header from 'common/headernew';

var Body = React.createClass({
    mixins: [State, Navigation],
    getInitialState: function () {
        return {
            nodes: null,
            site: null,
            alerts: null,
            otas:null,
            overlays: null,
            fixtures: null,
            firmwares: null,
            configs: null,
            groups: null,
            level: null,
            panels: 'split',
            savepanels: 'split',
            nodeID: '-1', // (typeof NSN == 'undefined' || typeof NSN.nodeID == 'undefined') ? '-1' : NSN.nodeID,
            overlayID: '-1',
            showschedule: false,
            showimage: false,
            showassignedconfig: false,
            showlocalsettings: false,
            showfactorysettings:false,
            showprovsettings:false,
            showsetfirmware:false,
            selectednode: null,
            selectedvideonode: null,
            selectedfirmware: null,
            selected_nodes: [],
            selected_nodesmodel: [],
            center: null,
            zoom: -1,
            multi_alerts : [],
            ui: {detail: "hidden",  // one of "unpinned", "pinned", "hidden"
                 prevDetail: "pinned",   // one of "unpinned", "pinned", "hidden"
                 list: "normal",  // "normal" or "expanded"
                 map: "normal"  // "normal" or "expanded"
             }
        }
    },

    detailUnpinnedPosition: {
        left: "34%",
        top:  "6%",
        height: "80%",
        width: "25%"
    },

    hideDetail: function() {
        if (this.state.ui.detail == "unpinned") {
            this.detailUnpinnedPosition = {
                left: $("#node-detail-panel").position().left + "px",
                top: $("#node-detail-panel").position().top + "px",
                width: $("#node-detail-panel").width() + "px",
                height: $("#node-detail-panel").height() + "px"
                };
        };
        this.setState({ui: $.extend({}, this.state.ui, {detail: "hidden", prevDetail: this.state.ui.detail})});
    },

    showDetail: function() {
        this.setState({ui: $.extend({}, this.state.ui, {detail: this.state.ui.prevDetail, prevDetail: "hidden"})});
    },

    pinDetail: function() {
        this.detailUnpinnedPosition = {
            left: $("#node-detail-panel").position().left + "px",
            top: $("#node-detail-panel").position().top + "px",
            width: $("#node-detail-panel").width() + "px",
            height: $("#node-detail-panel").height() + "px"
        };
        this.setState({ui: $.extend({}, this.state.ui, {detail: "pinned", prevDetail: "unpinned"})});
    },

    unpinDetail: function() {
        this.setState({ui: $.extend({}, this.state.ui, {detail: "unpinned", prevDetail: "pinned"})});
    },

    init: function () {
        var that = this;
        NSN.nodeID = '-1';
        if (NSN.customerID == '-1') {
            $('#loadingmsg').html('Please select an Account first.')
            return;
        };
        if (NSN.siteID == '-1') {
            $('#loadingmsg').html('Please select a Site first.')
            return;
        };

        if (NSN.siteName && NSN.siteName.match("PerfTest-")) {
            NSN.perfA = new Date();
            var list = helpers.genFakeSiteNodeList(parseInt(NSN.siteName.substring(9)));
            NSN.perfB = new Date();
            this.setState({
                nodes: list.map(function (node, index) {
                    node.name = "";
                    node.lig_stat = "on";
                    node.net_stat = Math.random() > 0.05;
                    node.sen_stat = "10";
                    node.level = "1";
                    node.idx = index;
                    return node;
                })
            });
        } else {

            $.ajax({
                url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/nodes',
                data: '',
                method: 'GET',
                xhrFields: {
                    withCredentials: true
                },
                dataType: 'json',
                success: function (data) {
                    if (data.errors) {
                        console.log('/minnodes API returned error:' + JSON.stringify(data));
                        $('#loadingmsg').html('Cannot retrieve node list. ' + '/nodes API returned error: ' + JSON.stringify(data));
                    } else {
                        // console.log('ajax success: ' + JSON.stringify(data));
                        $('#loadingmsg').html('Generating display.');
                        if (data.length > 0 && typeof data[0].lig_stat == "undefined") {
                            $.ajax({
                                url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/node_status' +
                                '?t=' + (new Date()).getTime(),
                                data: '',
                                method: 'GET',
                                xhrFields: {
                                    withCredentials: true
                                },
                                dataType: 'json',
                                success: function (statusdata) {
                                    // statusdata = helpers.getNodeStatus();
                                    for (var i = 0; i < data.length; i++) {
                                        var nodeid = data[i].nodeid;
                                        for (var j = 0; j < statusdata.length; j++) {
                                            if (nodeid == statusdata[j].nodeid) {
                                                data[i].lig_stat = statusdata[j].lig_stat;
                                                data[i].net_stat = statusdata[j].net_stat;
                                                data[i].sen_stat = statusdata[j].sen_stat;
                                                data[i].alerts = $.extend(true, [], statusdata[j].alerts);
                                                statusdata.splice(j, 1);
                                            }
                                        }
                                    }
                                    that.setState({
                                        nodes: data.map(function (node, index) {
                                            if (typeof node.level == 'undefined') {
                                                node.level = '1';
                                            };

                                            node.nodeid = node.nodeid || "";
                                            node.name = node.name || "";
                                            node.model = helpers.modelName(node.model);
                                            node.type = helpers.modelType(node.model);
                                            node.bssid = node.bssid || "";
                                            node.apn = node.apn || "";
                                            node.level = node.level || "1";
                                            node.building = node.building || "";
                                            node.ip = node.ip || "";
                                            node.softwareVersion = node.softwareVersion || "";
                                            node.fixturename = node.fixturename || "";
                                            node.schedulename = node.schedulename || "";
                                            node.groupnamelist = node.groupnamelist || "";
                                            node.remoteNetwork = node.remoteNetwork || "";
                                            // apply business logic
                                            node = helpers.setNodeStatus(node);
                                            node.idx = index;
                                            return node;
                                        })
                                    })
                                },
                                error: function (jqXHR, status, error) {
                                    console.log("node_status API failed");
                                    $('#loadingmsg').html('Cannot retrieve Node Status.  API reported error: ' + error);
                                }
                            });

                        } else {
                            that.setState({ nodes: [] });
                        };

                    };
                },
                error: function (jqXHR, status, error) {
                    console.log('ajax failure (nodes): ' + status + ' - ' + error);
                    $('#loadingmsg').html('Cannot retrieve Nodes.  API reported error: ' + error);
                }
            });
        };

        $.ajax({
            url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID,
            data: '',
            method: 'GET',
            xhrFields: {
                withCredentials: true
            },
            dataType: 'json',
            success: function (data) {
                if (data.errors) {
                    console.log('/nodes API returned error:' + JSON.stringify(data));
                    $('#loadingmsg').html('Cannot retrieve Site info.  API returned error: ' + JSON.stringify(data));
                } else {
                    console.log('ajax success: ' + JSON.stringify(data));
                    $('#loadingmsg').html('Generating display.');
                    that.setState({
                        site: data
                    })
                };
            },
            error: function (jqXHR, status, error) {
                console.log('ajax failure (site): ' + status + ' - ' + error);
                $('#loadingmsg').html('Cannot retrieve Site information.  API reported error: ' + error);
            }
        });
/*
        if (auth.allowed('CAN_READ', 'ConfigModel')) {
            $.ajax({
                url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/configs',
                data: '',
                method: 'GET',
                "xhrFields": {
                    withCredentials: true
                },
                dataType: 'json',
                success: function (data) {
                    console.log("ajax success: " + JSON.stringify(data));
                    that.setState({
                        configs: data.map(function (config, index) {
                            config.idx = index;
                            return config;
                        })
                    })
                },
                error : function(jqXHR, status, error){
                    noty({'type':'warning',text:'Could not access Configurations ('+error+').   Proceeding without them.'});
                    that.setState({configs:[]});
                }
            });
        } else {
            this.setState({configs:[]});
        };
*/

        /*if (auth.allowed('CAN_READ', 'FirmwareModel')) {
            $.ajax({
              url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/ota_status',
              data : '',
              xhrFields: {
                 withCredentials: true
              },
              method : 'GET',
              dataType : 'json',
              success : function(data){
                console.log("ajax success: " + JSON.stringify(data));
                if (data == "") {
                  that.setState({otas:[]});
                } else {
                  that.setState({
                    otas: data.map(function(ota, index) {
                      ota.otaid = ota.jobid;
                      ota.idx = index;
                      return ota;
                    })
                  })
                }
              },
              error : function(jqXHR, status, error){
                console.log("ajax failure (otas): " + status + " - " + error);
//                $("#loadingmsg").html("Cannot retrieve Ota status.  API reported error: " + error);
                  noty({'type':'warning',text:'Could not access OTA information ('+error+').   Proceeding without it.'});
                  that.setState({otas:[]});
              }
            });
        } else {
            this.setState({otas: []});
        }; */
/*
          $.ajax({
            url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/groups',
            data: '',
            method: 'GET',
            "xhrFields": {
                withCredentials: true
            },
            dataType: 'json',
            success: function (data) {
                console.log("ajax success: " + JSON.stringify(data));
                that.setState({
                    groups: data
                })
            },
            error: function (jqXHR, status, error) {
                $("#loadingmsg").html("Cannot retrieve Groups.  API reported error: " + error)
            }
        });
*/
        /*if (auth.allowed('CAN_READ', 'FirmwareModel')) {
            $.ajax({
               url: NSN.apiURL + 'firmwares',
               data : '',
               xhrFields: {
                  withCredentials: true
               },
               method : 'GET',
               dataType : 'json',
               success : function(data){
                 console.log("ajax success: " + JSON.stringify(data));
                 if (data == "") {
                   that.setState({firmwares:[]});
                 } else {
                   that.setState({
                     firmwares: data.map(function(firmware, index) {
                       firmware.idx = index;
                       return firmware;
                     })
                   })
                 }
               },
               error : function(jqXHR, status, error){
                 console.log("ajax failure (firmwares): " + status + " - " + error);
                  noty({'type':'warning',text:'Could not access Firmware Version information ('+error+').   Proceeding without it.'});
                  that.setState({firmwares:[]});
               }
             });
        } else {
            this.setState({firmwares:[]});
        }; */
    },


    componentDidMount: function () {
        this.init();
        var that = this;

        ReactBootstrap.Dispatcher.on('Nodeform.getConfigs', function () {
            if (auth.allowed('CAN_READ', 'ConfigModel')) {
                $.ajax({
                    url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/configs',
                    data: '',
                    method: 'GET',
                    "xhrFields": {
                        withCredentials: true
                    },
                    dataType: 'json',
                    success: function (data) {
                        console.log("ajax success: " + JSON.stringify(data));
                        that.setState({
                            configs: data.map(function (config, index) {
                                config.idx = index;
                                return config;
                            })
                        })
                    },
                    error : function(jqXHR, status, error){
                        noty({'type':'warning',text:'Could not access Configurations ('+error+').   Proceeding without them.'});
                        that.setState({configs:[]});
                    }
                });
            } else {
                that.setState({configs:[]});
            };
        });

        ReactBootstrap.Dispatcher.on('Nodeform.getGroups', function () {
          $.ajax({
            url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/groups',
            data: '',
            method: 'GET',
            "xhrFields": {
                withCredentials: true
            },
            dataType: 'json',
            success: function (data) {
                console.log("ajax success: " + JSON.stringify(data));
                that.setState({
                    groups: data
                })
            },
            error: function (jqXHR, status, error) {
                noty({'type':'warning',text:'Could not access Groups ('+error+').   Proceeding without them.'});
                that.setState({groups:[]});
                }
            });
        });

        ReactBootstrap.Dispatcher.on('Nodeform.getFirmwaresOtas', function () {
            if (auth.allowed('CAN_READ', 'FirmwareModel')) {
                var requests = [];
                var responses = {};
                requests.push($.ajax({
                   url: NSN.apiURL + 'firmwares',
                   data : '',
                   xhrFields: {
                      withCredentials: true
                   },
                   method : 'GET',
                   dataType : 'json',
                   success : function(data){
                     console.log("ajax success: " + JSON.stringify(data));
                     if (data == "") {
                       responses.firmwares =[];
                     } else {
                       responses.firmwares = data.map(function(firmware, index) {
                           firmware.idx = index;
                           return firmware;
                       })
                     }
                   },
                   error : function(jqXHR, status, error){
                     console.log("ajax failure (firmwares): " + status + " - " + error);
                     noty({'type':'warning',text:'Could not access Firmware Version information ('+error+').   Proceeding without it.'});
                     responses.firmwares =[];
                   }
                 }));

                requests.push($.ajax({
                  url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/ota_status',
                  data : '',
                  xhrFields: {
                     withCredentials: true
                  },
                  method : 'GET',
                  dataType : 'json',
                  success : function(data){
                    console.log("ajax success: " + JSON.stringify(data));
                    if (data == "") {
                      responses.otas = [];
                    } else {
                      responses.otas = data.map(function(ota, index) {
                          ota.otaid = ota.jobid;
                          ota.idx = index;
                          return ota;
                      })
                    }
                  },
                  error : function(jqXHR, status, error){
                    console.log("ajax failure (otas): " + status + " - " + error);
                      noty({'type':'warning',text:'Could not access OTA information ('+error+').   Proceeding without it.'});
                      responses.otas = [];
                  }
                }));

                $.when.apply($, requests).then(function(results) {
                    that.setState(responses);
                }, function(results) {
                    that.setState(responses);
                });

            } else {
                that.setState({firmwares:[], otas:[]});
            };
        });

        ReactBootstrap.Dispatcher.on('Nodelist.select', function (nodeID) {
            if( nodeID != NSN.nodeID){
                NSN.nodeID = nodeID;
                sessionStorage.setItem('nodeID', NSN.nodeID);
                that.setState({
                    nodeID: nodeID,
                    selected_nodes: []
                });
            }
        });
        ReactBootstrap.Dispatcher.on('Nodelist.multiSelect', function (nodes, modelrows) {
            that.setState({
                selected_nodes: nodes,
                selected_nodesmodel: that.state.nodes.filter(function (node) {
                    return nodes.indexOf(node.nodeid) >= 0;
                })
                    .map(function (node) { return node.model })
            });

            //fetching alerts of selected nodes
            var multi_alerts = []
            that.setState({
                multi_alerts// Clearing last fetched alerts
            })
            for (var i in nodes) {
                var k = 0
                $.ajax({
                    url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/alerts' + '/node/' + nodes[i],
                    data: '',
                    method: 'GET',
                    xhrFields: {
                        withCredentials: true
                    },
                    dataType: 'json',
                    success: function (data) {
                        multi_alerts = multi_alerts.concat(data)
                        k++
                        if (k == nodes.length) { //Update State only after all Async Calls Complete
                            that.setState({
                                multi_alerts
                            })
                        }
                    },
                    error: function (jqXHR, status, error) {
                        console.log('ajax failure (nodes): ' + status + ' - ' + error);
                        noty({ type: "error", text: "Unable to get Alerts" });
                    }
                });
            }
        });
        /*
                ReactBootstrap.Dispatcher.on('Nodemap.maximize', function(center, zoom) {
                    $("#node-map-panel").css({left:"0px",width:"100%",zIndex:"400"});
                });

                ReactBootstrap.Dispatcher.on('Nodemap.minimize', function(center, zoom) {
                    $("#node-map-panel").css({left:"33%",width:"67%",zIndex:"100"});
                });
        */
        ReactBootstrap.Dispatcher.on('Nodemap.selectNode', function (nodeid) {
            NSN.nodeID = nodeid;
            ReactBootstrap.Dispatcher.emit('Nodelist.selectrow', nodeid)
            that.setState({
                selected_nodes: [],
                nodeID: nodeid
            });
        });

        ReactBootstrap.Dispatcher.on("Nodemultigroupform.save",function(group_info) {
            var newState = {};
            if (group_info.groupid == "") {
                delete group_info.idx;
                delete group_info.groupid;
                // Check is user is Creating a new Group or updating a existing group
                var duplicate = false;
                for(var i = 0, len = that.state.groups.length; i < len; i++) {
                console.log(JSON.stringify(that.state.groups[i].name) + JSON.stringify(group_info.name));
                if( (that.state.groups[ i ].name === group_info.name))
                    duplicate = true;
                }
                if(duplicate){
                noty({type:"error", text:'Group "' + group_info.name + '" already exists.'});
                }

                else{
                $.ajax({
                    "url" : NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/groups',
                    "type" : "POST",
                    "data" : JSON.stringify(group_info),
                    "xhrFields": {
                        withCredentials: true
                        },
                    "dataType" : "json",
                    "contentType": "application/json",
                    "processData": false,
                    "success" : function(data) {                        
                        console.log("Response from Add Group: " + JSON.stringify(data));
                        NSN.groupID = data.groupid;
                        data.idx = this.state.groups.length;
                        var newState = React.addons.update(this.state, { groups: { $push : [data] }, groupID: { $set : data.groupid }});
                        noty({type:"success", text:'Group "' + data.name + '" added.'});
                        ReactBootstrap.Dispatcher.emit("Groupform.add.success", data);
                        this.setState(newState);
                        }.bind(that),
                    "error" : function(jqXHR, textStatus, errorThrown) {
                    if (jqXHR.status==400) {
                        noty({type:"error", text:JSON.parse(jqXHR.responseText).message});
                    } else {
                        noty({type:"error", text:"Could not add group."});
                    }
                    }
                });
                }
            } else {
                var group_idx = group_info.idx;
                delete group_info.idx;

                // Check if new Group name is already taken
                var duplicate = false;
                for(var i = 0, len = that.state.groups.length; i < len; i++) {
                if( (that.state.groups[ i ].name === group_info.name) && (that.state.groups[i].groupid != group_info.groupid))
                    duplicate = true;
                }
                if(duplicate){
                noty({type:"error", text:'Group "' + group_info.name + '" name is already used.'});
                }

                else{

                $.ajax({
                    "url" : NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/groups/' + group_info.groupid,
                    "type" : "POST",
                    "xhrFields": {
                    withCredentials: true
                    },
                    "data" : JSON.stringify(group_info),
                    "dataType" : "json",
                    "contentType" : "application/json",
                    "processData" : false,
                    "success" : function(data) {
                    data.idx = group_idx;
                    var newState = React.addons.update(this.state, { groups: { [group_idx]: { $set: data } }});
                    noty({type:"success", text:'Group "' + data.name + '" updated.'});
                    ReactBootstrap.Dispatcher.emit("Groupform.update.success", data);
                    this.setState(newState);
                    }.bind(that),
                    "error" : function(jqXHR, textStatus, errorThrown) {
                    if (jqXHR.status==400) {
                        noty({type:"error", text:JSON.parse(jqXHR.responseText).message});
                    } else {
                        noty({type:"error", text:"Could not update group."});
                    }
                    }
                });
                }
            }
        });


        ReactBootstrap.Dispatcher.on('Nodeform.setfirmware', function (switchto, nodeinfo) {
            var selectedFirmware = $("select#newfirmwareid option").filter(":selected").val();
            var selectedNode = nodeinfo;
            that.setState({
                showsetfirmware: switchto == 'open',
                selectednode: selectedNode,
                selectedfirmware:selectedFirmware
            });
        });

        ReactBootstrap.Dispatcher.on('Nodeform.assignedconfig', function (switchto, selectednode) {
            that.setState({
                showassignedconfig: switchto == 'open',
                selectednode: selectednode
            });
        });

        ReactBootstrap.Dispatcher.on('Nodeform.viewlocalsettings', function (switchto, selectednode) {
            that.setState({
                showlocalsettings: switchto == 'open',
                selectednode: selectednode
            });
        });

        ReactBootstrap.Dispatcher.on('Nodeform.viewfactorysettings', function (switchto, selectednode) {
            that.setState({
                showfactorysettings: switchto == 'open',
                selectednode: selectednode
            });
        });

        ReactBootstrap.Dispatcher.on('Nodeform.viewprovsettings', function (switchto, selectednode) {
            that.setState({
                showprovsettings: switchto == 'open',
                selectednode: selectednode
            });
        });

        ReactBootstrap.Dispatcher.on('Nodeform.schedule', function (switchto, selectednode) {
            if(!that.state.showschedule && switchto == 'open'){
            that.setState({
                showschedule: switchto == 'open',
                selectednode: selectednode
            });
        }
        else if (switchto == 'close'){
            that.setState({
                showschedule: switchto == 'open',
                selectednode: selectednode
            });
        }
        });
        ReactBootstrap.Dispatcher.on('Nodeform.image', function (switchto, selectednode) {
            that.setState({
                showimage: switchto == 'open',
                selectedvideonode: typeof selectednode == "undefined" ? null : selectednode
            });
        });

        ReactBootstrap.Dispatcher.on('Nodeform.startVPN', function(nodeid) {
            $.ajax({
                url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/nodes/' + nodeid + '/vpn/start',
                data: '',
                method: 'PUT',
                processData : false,
                contentType: 'application/json',
                xhrFields: {
                    withCredentials: true
                },
                dataType: 'json',
                success: function (data) {
                    noty({
                        type: "success", text: 'VPN Started for Node ' + nodeid
                    });
                },
                error: function () {
                    noty({
                        type: "error", text: 'VPN Start Failed for Node ' + nodeid
                    });
                }                    
            });
        });

        ReactBootstrap.Dispatcher.on('Nodeform.stopVPN', function(nodeid) {
            $.ajax({
                url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/nodes/' + nodeid + '/vpn/stop',
                data: '',
                method: 'PUT',
                processData : false,
                contentType: 'application/json',
                xhrFields: {
                    withCredentials: true
                },
                dataType: 'json',
                success: function (data) {
                    noty({
                        type: "success", text: 'VPN Stopped for Node ' + nodeid
                    });
                },
                error: function () {
                    noty({
                        type: "error", text: 'VPN Stop Failed for Node ' + nodeid
                    });
                }                    
            });
        });

        ReactBootstrap.Dispatcher.on('Nodeform.lightLevel', function (nodeid, level, timeout) {
            var idx = helpers.get_idx(that.state.nodes, { nodeid: nodeid }, 'nodeid');
            $.ajax({
                url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/lightcontrol/node/' + NSN.nodeID,
                data: JSON.stringify({
                    level: parseInt(level),
                    timeout: parseInt(timeout)
                }),
                contentType: 'application/json',
                method: 'POST',
                xhrFields: {
                    withCredentials: true
                },
                dataType: 'json',
                context: {
                    level: parseInt(level),
                    timeout: parseInt(timeout)
                },
                success: function (data) {
                    noty({
                        type: "success", text: 'Driver Level for node "' + NSN.nodeID + '" set to '
                            + this.level + '% for ' + Math.round(this.timeout / 60) + ' minutes.'
                    });
                    /*                        var newState = React.addons.update(this.state, {
                                                nodes: {
                                                    [idx]: {
                                                        levelInfo: {
                                                            driver: {$set: this.level},
                                                            isscheduled: {$set: true}
                                                        }
                                                    }
                                                }
                                            });
                    */
                    $("#nodelevel").html("(" + this.level + "%*)");
                },
                error: function () {
                    noty({ type: "error", text: 'Could not override node light level.' });
                }
            })
        });

        ReactBootstrap.Dispatcher.on('Nodeform.fixtureUpdate', function (state) {

            var fixtureSelected = $("select#changefixture option").filter(":selected").val();
            var fixtureSelectedName = $("select#changefixture option").filter(":selected").text();
            console.log(fixtureSelected, fixtureSelectedName);

            $.ajax({
                url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/fixtures/' + fixtureSelected + '/assign/node/' + NSN.nodeID,
                contentType: 'application/json',
                method: 'POST',
                xhrFields: {
                    withCredentials: true
                },
                dataType: 'json',
                success: function (data) {
                    noty({ type: "success", text: 'Node "' + NSN.nodeID + '" set to Fixture ' + fixtureSelectedName });
                },
                error: function () {
                    noty({ type: "error", text: 'Could not set Fixture.' });
                }
            })
        });

        ReactBootstrap.Dispatcher.on("Nodeform.getFixtures", function () {
            $.ajax({
                url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/fixtures',
                type: 'GET',
                xhrFields: {
                    withCredentials: true
                },
                dataType: 'json',
                contentType: 'application/json',
                success: function (data) {

                    data.sort(function (a, b) {
                        if (a.name < b.name) return -1;
                        if (a.name > b.name) return 1;
                        return 0;
                    });
                    that.setState({
                        fixtures: data.map(function (fixture, index) {
                            fixture.idx = index;
                            return fixture;
                        })
                    })
                }.bind(that),
                error: function () {
                    noty({
                        type: 'information',
                        text: 'Could not get fixtures for this site.'
                    });
                }.bind(that)
            })
        });

        ReactBootstrap.Dispatcher.on("Nodeform.getLightLevel", function (state) {
            $.ajax({
                url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/nodes/' + state.nodeid + '/light_status',
                type: 'GET',
                xhrFields: {
                    withCredentials: true
                },
                dataType: 'json',
                contentType: 'application/json',
                success: function (data) {
                    state.levelInfo.driver = data.driver;
                    this.setState(state);
                }.bind(that),
                error: function () {
                    noty({
                        type: 'information',
                        text: 'Could not determine current light level for this node.'
                    });
                    state.levelInfo.driver = "--";
                    this.setState(state);
                }.bind(that)
            })
        });

        ReactBootstrap.Dispatcher.on("Nodeform.pushSchedule", function (nodeid) {
            var url = NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/nodes/' + nodeid + '/schedule_push';
            $.ajax({
                url: url,
                method: 'POST',
                "xhrFields": {
                    withCredentials: true
                },
                contentType: 'application/json',
                dataType: 'json',
                data: {},
                success: function (data) {
                    console.log("ajax success: " + JSON.stringify(data));
                    noty({
                        type: "success",
                        text: 'Schedule pushed to Node ' + nodeid + ' as requested.'
                    });
                },
                error: function (jqXHR, status, error) {
                    if (jqXHR.status == 200) {
                        noty({
                            type: "success",
                            text: 'Schedule pushed to Node ' + nodeid + ' as requested.'
                        });
                    } else {
                        noty({
                            type: "error",
                            text: 'Unable to push Schedule to this Node.  Return status: ' + jqXHR.status
                        });
                    }
                }
            });
        });

        ReactBootstrap.Dispatcher.on('Nodelist.add', function () {
            that.setState({
                nodeID: '0'
            });
        });

        ReactBootstrap.Dispatcher.on("Nodeform.reset", function () {
            that.forceUpdate();
        });

        ReactBootstrap.Dispatcher.on('Nodeform.save', function (node_info) {
            var newState = {};
// there's no Add capability for nodes
/*            if (node_info.idx == -1) {
                delete node_info.idx;

                $.ajax({
                    url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/nodes',
                    type: 'POST',
                    'xhrFields': {
                        withCredentials: true
                    },
                    'data': JSON.stringify(node_info),
                    dataType: 'json',
                    contentType: 'application/json',
                    success: function (data) {
                        console.log('Response from Add Node: ' + JSON.stringify(data));
                        //            NSN.siteID = data.siteid;
                        data.idx = this.state.nodes.length;
                        NSN.nodeID = data.nodeid;
                        sessionStorage.setItem('nodeID', NSN.nodeID);
                        newState = React.addons.update(this.state, {
                            nodes: {
                                $push: [data]
                            },
                            nodeID: {
                                $set: data.nodeid
                            },
                            panels: {
                                $set: this.state.savepanels
                            }
                        });
                        noty({
                            type: 'success',
                            text: 'Node "' + data.nodeid + '" added.'
                        })
                        ReactBootstrap.Dispatcher.emit("Nodeform.add.success", data);
                        this.setState(newState);
                    }.bind(that),
                    error: function () {
                        noty({
                            type: 'error',
                            text: 'Could not add node.'
                        });
                    }
                })

            } else {
*/
                var idx = helpers.get_idx(that.state.nodes, node_info, 'nodeid');

                delete node_info.idx;
                delete node_info.levelInfo;
                delete node_info.time_zone;
                delete node_info.country_code;

                node_info.model = helpers.modelInternalName(node_info.model);

                if ((node_info.model != "unode-v5")
                    && (node_info.model != "unode-v6")) {
                    delete node_info.apn;
                    delete node_info.iccid;
                    delete node_info.imei;
                    delete node_info.imsi;
                }
                else {
                    delete node_info.bssid;
                }

                var node = that.state.nodes[idx];
                var save_status = {
                    net_stat: node.net_stat,
                    lig_stat: node.lig_stat,
                    sen_stat: node.sen_stat,
                    net_stats: node.net_stats,
                    lig_stats: node.lig_stats,
                    sen_stats: node.sen_stats,
                    alerts: $.extend(true, [], node.alerts),
                    vpnip: node.vpnip || ""
                };
                delete node_info.alerts;
                delete node_info.net_stat;
                delete node_info.net_stats;
                delete node_info.lig_stat;
                delete node_info.lig_stats;
                delete node_info.sen_stat;
                delete node_info.sen_stats;
                delete node_info.vpnip;
                
                $.ajax({
                    url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/nodes/' + node_info.nodeid,
                    type: 'POST',
                    xhrFields: {
                        withCredentials: true
                    },
                    data: JSON.stringify(node_info),
                    dataType: 'json',
                    contentType: 'application/json',
                    success: function (data) {
                        data.idx = idx;
                        data.model = helpers.modelName(data.model);
                        data.type = helpers.modelType(data.model);
                        Object.assign(data, save_status);
                        var newState = React.addons.update(this.state, {
                            nodes: {
                                [idx]: {
                                    $set: data
                                }
                            }
                        });
                        noty({
                            type: 'success',
                            text: 'Node "' + data.nodeid + '" updated.'
                        })
                        ReactBootstrap.Dispatcher.emit("Nodeform.update.success", data);
                        this.setState(newState);
                    }.bind(that),
                    error: function () {
                        noty({
                            type: 'error',
                            text: 'Could not update node.'
                        });
                    }
                })
//            }
        });

        ReactBootstrap.Dispatcher.on('Nodeform.firmwareassign', function (node_info,selectedfirmwareid,description) {
            console.log("Assignment:",node_info.nodeid, selectedfirmwareid, description);
            var payload = {
              description: description
            }
            $.ajax({
              url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/firmwares/' + selectedfirmwareid + '/assign/' + 'node/'+ node_info.nodeid,
              data: JSON.stringify(payload),
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
                  type: "success", text: 'Updates sent to ' + node_info.nodeid + ' as requested.'
                });
              },
              error: function (jqXHR, status, error) {
                if (jqXHR.status == 200) {
                  noty({
                    type: "success", text: 'Updates sent to ' + node_info.nodeid + ' as requested.'
                  });
                } else {
                  noty({
                    type: "error", text: 'Unable to send updates '
                      + 'as requested.  Return status: ' + status
                  });
                }
              }
            });
        });

        ReactBootstrap.Dispatcher.on('Nodeformconfig.save', function (config, selected_nodes) {

            var configSelected = config.configid;
            var configSelectedName = config.name;

            var payload = {
                nodeList: selected_nodes,
            }

            $.ajax({
                url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/configs/' + configSelected + '/apply/nodes',
                type: 'POST',
                xhrFields: {
                    withCredentials: true
                },
                data: JSON.stringify(payload),
                dataType: 'json',
                contentType: 'application/json',
                success: function (data) {
                    console.log('Updating node(s) with selected Configuration: ' + JSON.stringify(data));
                    noty({
                        type: 'success',
                        text: 'Updated node(s) "' + selected_nodes + '"  with Configuration "' + configSelectedName + '"'
                    })

                },
                error: function () {
                    noty({
                        type: 'error',
                        text: 'Could not update node(s) "' + selected_nodes + '"  with Configuration "' + configSelectedName + '"'
                    });
                }
            })

        });

        ReactBootstrap.Dispatcher.on('Nodemultiformserver.save', function (node_info, selected_nodes) {
            //alert("API Payload for Multi-Node Config:\n" + JSON.stringify({data:node_info,nodes:selected_nodes},null,3));
            var payload = {
                nodeList: selected_nodes,
                server: node_info.server
            }

            $.ajax({
                url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/nodes' + '/redirect',
                type: 'POST',
                xhrFields: {
                    withCredentials: true
                },
                data: JSON.stringify(payload),
                dataType: 'json',
                contentType: 'application/json',
                success: function (data) {
                    console.log('Response from Edit Server config: ' + JSON.stringify(data));
                    noty({
                        type: 'success',
                        text: 'Nodes "' + selected_nodes + '" config server data successfully updated.'
                    })

                },
                error: function () {
                    noty({
                        type: 'error',
                        text: 'Could not update "' + selected_nodes + '" server data.'
                    });
                }
            })
        });

        ReactBootstrap.Dispatcher.on("Nodemultiformfixture.save", function (node_info, selected_nodes) {
            var payload = {
                nodeList: selected_nodes,
            }

            $.ajax({
                url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/fixtures/' + node_info.multifixtureid + '/assign' + '/nodes',
                type: 'POST',
                xhrFields: {
                    withCredentials: true
                },
                data: JSON.stringify(payload),
                dataType: 'json',
                contentType: 'application/json',
                success: function (data) {
                    console.log('Response from assigning fixture to selected nodes: ' + JSON.stringify(data));
                    noty({
                        type: 'success',
                        text: 'Nodes "' + selected_nodes + '" successfully assigned to selected fixture.'
                    })

                },
                error: function () {
                    noty({
                        type: 'error',
                        text: 'Could not update "' + selected_nodes + '" to selected fixture.'
                    });
                }
            })

        });

        function getSensorValue(nodeID, sensorid, callback) {
            var now = new Date(),
                yesterday = new Date(now.getTime() - (24 * 60 * 60 * 1000));

            $.ajax({
                url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/nodes/' + nodeID + '/sensors/' + sensorid + '/date/' + yesterday.toISOString() + '/limit/1',
                data: '',
                method: 'GET',
                xhrFields: {
                    withCredentials: true
                },
                dataType: 'json',
                success: function (data) {
                    if (data.datapoints[0]) {
                        callback(null, data.datapoints[0].value);
                    } else {
                        callback();
                    }
                },
                error: function () {
                    callback(true);
                }
            });
        }

        function exist(data) {
            if (data !== null && data !== undefined) {
                return true;
            } else {
                return false;
            }
        }

        function boot_reasons_low(code) {
            var r = '';

            switch (code) {
                case 1:
                    r = 'Brownout on Vbat power source';
                    break;
                case 2:
                    r = 'Brownout on V12 power source';
                    break;
                case 4:
                    r = 'Brownout on V18 power source';
                    break;
                case 8:
                    r = 'Software requested system reset';
                    break;
                case 16:
                    r = 'CPU locked up';
                    break;
                case 32:
                    r = 'Watchdog timeout';
                    break;
                case 64:
                    r = 'Brownout on Vfl power source';
                    break;
                default:
                    r = 'Unknown Reason'
            }

            return r;
        }

        function boot_reasons_app(code) {
            var r = '';

            switch (code) {
                case 1:
                    r = 'COMMAND_REBOOT';
                    break;
                case 2:
                    r = 'COMMAND_WDTTOUT';
                    break;
                case 3:
                    r = 'SW_WDTTOUT';
                    break;
                case 4:
                    r = 'BOOT_FAILURE';
                    break;
                case 16:
                    r = 'NETWORK_WD_TIMEOUT';
                    break;
                case 17:
                    r = 'OTA_COMMAND_RESET';
                    break;
                case 18:
                    r = 'OTA_COMMAND_PARTITION';
                    break;
                case 19:
                    r = 'OTA_FW_UPDATE';
                    break;
                case 20:
                    r = 'OTACFG_OUT_OF_MEM';
                    break;
                case 21:
                    r = 'X509_UPDATE';
                    break;
                case 32:
                    r = 'CS5480_POLL';
                    break;
                case 33:
                    r = 'CS5480_POLL_REG_RD_1';
                    break;
                case 34:
                    r = 'CS5480_POLL_REG_RD_2';
                    break;
                case 35:
                    r = 'CS5480_POLL_REG_RD_3';
                    break;
                case 36:
                    r = 'CS5480_POLL_REG_RD_4';
                    break;
                case 37:
                    r = 'CS5480_POLL_REG_RD_5';
                    break;
                case 41:
                    r = 'CS5480_POLL_REG_WR';
                    break;
                default:
                    r = 'Unknown Reason'
            }

            return r;
        }

        ReactBootstrap.Dispatcher.on("Nodeform.getDiagnostic", function (state) {
            console.log("handling Nodeform.getDiagnostic");
            var nodeID = state.nodeid;

            if (state.model == "unode-v2") {
                async.parallel({

                    T: function (callback) {
                        getSensorValue(nodeID, 'T', callback);
                    },
                    bc: function (callback) {
                        getSensorValue(nodeID, 'bc', callback);
                    },
                    bRA: function (callback) {
                        getSensorValue(nodeID, 'bRA', callback);
                    },
                    bRL: function (callback) {
                        getSensorValue(nodeID, 'bRL', callback);
                    },
                    WDT: function (callback) {
                        getSensorValue(nodeID, 'WDT', callback);
                    }
                },
                    function (err, results) {
                        if (!err) {
                            state.internaltemp = exist(results.T) ? results.T + ' C' : 'undefined';
                            state.rebootcount = exist(results.bc) ? results.bc : 'undefined';
                            state.rebootreasonapp = boot_reasons_app(results.bRA);
                            state.rebootreasonlow = boot_reasons_low(results.bRL);
                            state.watchdogfailurecount = exist(results.WDT) ? results.WDT : 'undefined';

                            that.setState(state);
                        } else {
                            console.log(err);
                        }

                    }
                );
            }
            else {
                async.parallel({
                    aip: function (callback) {
                        getSensorValue(nodeID, 'aip', callback);
                    },
                    T: function (callback) {
                        getSensorValue(nodeID, 'T', callback);
                    },
                    jtx: function (callback) {
                        getSensorValue(nodeID, 'jtx', callback);
                    },
                    jty: function (callback) {
                        getSensorValue(nodeID, 'jty', callback);
                    },
                    jtz: function (callback) {
                        getSensorValue(nodeID, 'jtz', callback);
                    },
                    jtm: function (callback) {
                        getSensorValue(nodeID, 'jtm', callback);
                    },
                    mip: function (callback) {
                        getSensorValue(nodeID, 'mip', callback);
                    },
                    mt: function (callback) {
                        getSensorValue(nodeID, 'mt', callback);
                    },
                    bc: function (callback) {
                        getSensorValue(nodeID, 'bc', callback);
                    },
                    bRA: function (callback) {
                        getSensorValue(nodeID, 'bRA', callback);
                    },
                    bRL: function (callback) {
                        getSensorValue(nodeID, 'bRL', callback);
                    },
                    vp: function (callback) {
                        getSensorValue(nodeID, 'vp', callback);
                    },
                    WDT: function (callback) {
                        getSensorValue(nodeID, 'WDT', callback);
                    }
                },
                    function (err, results) {
                        console.log('827', results);
                        if (!err) {
                            state.auxcurrentspike = exist(results.aip) ? results.aip + ' A' : 'undefined';
                            state.internaltemp = exist(results.T) ? results.T + ' C' : 'undefined';
                            state.joltx = exist(results.jtx) ? results.jtx : 'undefined';
                            state.jolty = exist(results.jty) ? results.jty : 'undefined';
                            state.joltz = exist(results.jtz) ? results.jtz : 'undefined';
                            state.joltmagnitude = exist(results.jtm) ? results.jtm : 'undefined';
                            state.maincurrentspike = exist(results.mip) ? results.mip + ' A' : 'undefined';
                            state.mcutemp = exist(results.mt) ? results.mt + ' C' : 'undefined';
                            state.rebootcount = exist(results.bc) ? results.bc : 'undefined';
                            state.rebootreasonapp = boot_reasons_app(results.bRA);
                            state.rebootreasonlow = boot_reasons_low(results.bRL);
                            state.voltagespike = exist(results.vp) ? results.vp + ' V' : 'undefined';
                            state.watchdogfailurecount = exist(results.WDT) ? results.WDT : 'undefined';

                            that.setState(state);
                        } else {
                            console.log(err);
                        }

                    }
                );
            }

        });

        ReactBootstrap.Dispatcher.on('Nodeform.getSensorsNetwork', function (state) {
        // ReactBootstrap.Dispatcher.on('Nodedetail.getSensorsNetwork', function (state) {
            console.log("handling Nodeform.getSensorsNetwork");
            var nodeID = state.nodeid;

            if (state.model == "unode-v2") {
                async.parallel({
                    RF: function (callback) {
                        getSensorValue(nodeID, 'RF', callback);
                    },
                    sn: function (callback) {
                        getSensorValue(nodeID, 'sn', callback);
                    },
                    connection: getConnStatus
                },
                    function (err, results) {
                        if (!err) {
                            state.signalstrength = results.RF + ' dBm' || 'undefined';
                            state.noisefigure = results.sn + ' dB' || 'undefined';
                            state.connected = results.connection.isconnected;
                            state.connectedsince = moment(results.connection.since).format('MM/DD/YY hh:mm:ss Z');

                            if (results.connection.isconnected) {
                                state.connectedsincelabel = 'Connected Since';
                            } else {
                                state.connectedsincelabel = 'Disconnected Since';
                            }

                            that.setState(state);
                        } else {
                            console.log(err);
                        }
                    }
                );
            }
            else {
                async.parallel({
                    RF: function (callback) {
                        getSensorValue(nodeID, 'RF', callback);
                    },
                    sn: function (callback) {
                        getSensorValue(nodeID, 'sn', callback);
                    },
                    mip: function (callback) {
                        getSensorValue(nodeID, 'mip', callback);
                    },
                    aip: function (callback) {
                        getSensorValue(nodeID, 'aip', callback);
                    },
                    connection: getConnStatus
                },
                    function (err, results) {
                        if (!err) {
                            state.signalstrength = results.RF + ' dBm' || 'undefined';
                            state.noisefigure = results.sn + ' dB' || 'undefined';
                            state.maincurrentpeak = exist(results.mip) ? results.mip + ' A' : 'undefined';
                            state.auxcurrentpeak = exist(results.aip) ? results.aip + ' A' : 'undefined';
                            state.connected = results.connection.isconnected;
                            state.connectedsince = moment(results.connection.since).format('MM/DD/YY hh:mm:ss Z');

                            if (results.connection.isconnected) {
                                state.connectedsincelabel = 'Connected Since';
                            } else {
                                state.connectedsincelabel = 'Disconnected Since';
                            }

                            that.setState(state);
                        } else {
                            console.log(err);
                        }
                    }
                );
            }


            function getConnStatus(callback) {
                $.ajax({
                    url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/nodes/' + nodeID + '/connection_status',
                    data: '',
                    method: 'GET',
                    xhrFields: {
                        withCredentials: true
                    },
                    dataType: 'json',
                    success: function (data) {
                        callback(null, data);
                    },
                    error: function () {
                        callback(true);
                    }
                });
            }
        });

        ReactBootstrap.Dispatcher.on('Nodeform.getSensorsAmbient', function (state) {
        //ReactBootstrap.Dispatcher.on('Nodedetail.getSensorsAmbient', function (state) {
            console.log("handling Nodeform.getSensorsAmbient");
            var nodeID = state.nodeid;

            async.parallel({
                lt: function (callback) {
                    getSensorValue(nodeID, 'lt', callback);
                },
                p: function (callback) {
                    getSensorValue(nodeID, 'p', callback);
                },
                pc: function (callback) {
                    getSensorValue(nodeID, 'pc', callback);
                },
                t: function (callback) {
                    getSensorValue(nodeID, 't', callback);
                },
                lIR: function (callback) {
                    getSensorValue(nodeID, 'lIR', callback);
                }
            },

                function (err, results) {
                    if (!err) {
                        state.light = exist(results.lt) ? results.lt + '%' : 'undefined';
                        state.presence = (results.p == "1") ? 'true' : 'false';
                        state.presencecount = exist(results.pc) ? results.pc : 'undefined';
                        state.externaltemp = exist(results.t) ? results.t + ' C' : 'undefined';
                        state.infrared = exist(results.lIR) ? results.lIR + ' l' : 'undefined';

                        that.setState(state);
                    } else {
                        console.log(err);
                    }

                }
            );
        });

        ReactBootstrap.Dispatcher.on('Nodeform.getSensorsEnergy', function (state) {
        // ReactBootstrap.Dispatcher.on('Nodedetail.getSensorsEnergy', function (state) {
            console.log("handling Nodeform.getSensorsEnergy");
            var nodeID = state.nodeid;

            if (state.model == "unode-v2") {
                async.parallel({
                    i: function (callback) {
                        getSensorValue(nodeID, 'i', callback);
                    },
                    w: function (callback) {
                        getSensorValue(nodeID, 'w', callback);
                    },
                    v: function (callback) {
                        getSensorValue(nodeID, 'v', callback);
                    }
                },
                    function (err, results) {
                        if (!err) {

                            state.maincurrent = exist(results.i) ? results.i + ' A' : 'undefined';
                            state.mainenergyuse = exist(results.w) ? results.w + ' WH' : 'undefined';
                            state.voltage = exist(results.v) ? results.v + ' V' : 'undefined';

                            that.setState(state);
                        } else {
                            console.log(err);
                        }

                    }
                );
            }
            else {

                async.parallel({
                    ai: function (callback) {
                        getSensorValue(nodeID, 'ai', callback);
                    },
                    aw: function (callback) {
                        getSensorValue(nodeID, 'aw', callback);
                    },
                    ap: function (callback) {
                        getSensorValue(nodeID, 'aP', callback);
                    },
                    aPF: function (callback) {
                        getSensorValue(nodeID, 'aPF', callback);
                    },
                    mi: function (callback) {
                        getSensorValue(nodeID, 'mi', callback);
                    },
                    mw: function (callback) {
                        getSensorValue(nodeID, 'mw', callback);
                    },
                    mP: function (callback) {
                        getSensorValue(nodeID, 'mP', callback);
                    },
                    mPF: function (callback) {
                        getSensorValue(nodeID, 'mPF', callback);
                    },
                    v: function (callback) {
                        getSensorValue(nodeID, 'v', callback);
                    }
                },
                    function (err, results) {
                        if (!err) {
                            state.auxcurrent = exist(results.ai) ? results.ai + ' A' : 'undefined';
                            state.auxenergyuse = exist(results.aw) ? results.aw + ' WH' : 'undefined';
                            state.auxpower = exist(results.ap) ? results.ap + ' W' : 'undefined';
                            state.auxpowerfactor = exist(results.aPF) ? results.aPF : 'undefined';
                            state.maincurrent = exist(results.mi) ? results.mi + ' A' : 'undefined';
                            state.mainenergyuse = exist(results.mw) ? results.mw + ' WH' : 'undefined';
                            state.mainpower = exist(results.mP) ? results.mP + ' W' : 'undefined';
                            state.mainpowerfactor = exist(results.mPF) ? results.mPF : 'undefined';
                            state.voltage = exist(results.v) ? results.v + ' V' : 'undefined';

                            that.setState(state);
                        } else {
                            console.log(err);
                        }

                    }
                );
            }
        });

        ReactBootstrap.Dispatcher.on('Nodeform.getConnectionSince', function (state) {
            var nodeID = state.nodeid;

            async.parallel({
                connection: getConnSince
            },
                function (err, results) {
                    if (!err) {

                        state.since = results.connection.since ? moment(results.connection.since).format('MM/DD/YY HH:MM:ss Z') : ""

                        that.setState(state);
                    } else {
                        console.log(err);
                    }

                }
            );

            function getConnSince(callback) {
                $.ajax({
                    url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/nodes/' + nodeID + '/connection_status',
                    data: '',
                    method: 'GET',
                    xhrFields: {
                        withCredentials: true
                    },
                    dataType: 'json',
                    success: function (data) {
                        callback(null, data);
                    },
                    error: function () {
                        callback(true);
                    }
                });
            }
        });

        ReactBootstrap.Dispatcher.on('Nodeform.setconfig', function (node_info) {
            console.log(JSON.stringify(node_info.nodeid), $("select#configid option").filter(":selected").val());
            var configSelected = $("select#configid option").filter(":selected").val();
            var payload = {
                nodeList: [node_info.nodeid],
            }

            $.ajax({
                url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/configs/' + configSelected + '/apply/nodes',
                type: 'POST',
                xhrFields: {
                    withCredentials: true
                },
                data: JSON.stringify(payload),
                dataType: 'json',
                contentType: 'application/json',
                success: function (data) {
                    console.log('Updating node with selected config: ' + JSON.stringify(data));
                    noty({
                        type: 'success',
                        text: 'Updated node "' + node_info.nodeid + '"  with selected config "' + data.config.name + '"'
                    })

                },
                error: function () {
                    noty({
                        type: 'error',
                        text: 'Could not update node "' + node_info.nodeid + '" with selected config "' + data.config.name + '"'
                    });
                }
            })
        });

        ReactBootstrap.Dispatcher.on('Nodeform.configWifiUpdate', function (node_info) {
            console.log(JSON.stringify(node_info));
            var payload = {
                nodeList: [node_info.nodeid],
                networkXSSID: node_info.networkXSSID,
                networkXSecurity: $("select#networkXSecurity option").filter(":selected").val(),
                networkXPasskey: node_info.networkXPasskey,
                networkYSSID: node_info.networkYSSID,
                networkYSecurity: $("select#networkYSecurity option").filter(":selected").val(),
                networkYPasskey: node_info.networkYPasskey
            }

            $.ajax({
                url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/wifi',
                type: 'POST',
                xhrFields: {
                    withCredentials: true
                },
                data: JSON.stringify(payload),
                dataType: 'json',
                contentType: 'application/json',
                success: function (data) {
                    console.log('Response from Edit Wifi config: ' + JSON.stringify(data));
                    noty({
                        type: 'success',
                        text: 'Node "' + node_info.nodeid + '" config wifi data successfully updated.'
                    })

                },
                error: function () {
                    noty({
                        type: 'error',
                        text: 'Could not update "' + node_info.nodeid + '" config data.'
                    });
                }
            })

        });

        ReactBootstrap.Dispatcher.on('Nodeform.configServerUpdate', function (node_info) {
            var payload = {
                nodeList: [node_info.nodeid],
                server: node_info.server
            }

            $.ajax({
                url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/nodes' + '/redirect',
                type: 'POST',
                xhrFields: {
                    withCredentials: true
                },
                data: JSON.stringify(payload),
                dataType: 'json',
                contentType: 'application/json',
                success: function (data) {
                    console.log('Response from Update Server config: ' + JSON.stringify(data));
                    noty({
                        type: 'success',
                        text: 'Node "' + node_info.nodeid + '" server data successfully updated.'
                    })

                },
                error: function () {
                    noty({
                        type: 'error',
                        text: 'Could not update "' + node_info.nodeid + '" server data.'
                    });
                }
            })

        });

        ReactBootstrap.Dispatcher.on('Nodeform.delete', function (node_info) {
            var idx = helpers.get_idx(that.state.nodes, node_info, 'nodeid');
            $.ajax({
                url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/nodes' + '/' + node_info.nodeid,
                type: 'DELETE',
                'xhrFields': {
                    withCredentials: true
                },
                //'data': JSON.stringify({}),
                dataType: 'json',
                contentType: 'application/json',
                success: function (data) {
                    var newState = React.addons.update(this.state, { nodes: { $splice: [[idx, 1]] }, nodeID: { $set: "-1" } });
                    /* Read more about React.addons https://facebook.github.io/react/docs/update.html: */
                    ReactBootstrap.Dispatcher.emit("Nodeform.delete.success", node_info.nodeid); // Event to Remove from the List
                    this.setState(newState);
                    noty({
                        type: 'success',
                        text: 'Node Deleted'
                    });

                }.bind(that),
                error: function () {
                    noty({
                        type: 'error',
                        text: 'Could not Delete node'
                    });
                }
            })
        });

        ReactBootstrap.Dispatcher.on('Nodeform.nodereset', function (node_info) {

            var row = $('#node-table tr[data-nodeid="' + node_info.nodeid + '"]');
            var item = row.find('img')[1];

            var nodegrid = $("#Node-grid").data("gridInstance");
            var rowData = nodegrid.getData().getItemById(node_info.nodeid)
            console.log(rowData.net_stat);

            if (rowData.net_stat) {

                var cmd = $('#cmd').val();
                var payload = {
                    nodeList: [node_info.nodeid],
                    cmd: cmd,
                }
                console.log(JSON.stringify(payload));

                $.ajax({
                    url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/deviceaction/' + cmd,
                    data: JSON.stringify(payload),
                    method: 'POST',
                    xhrFields: {
                        withCredentials: true
                    },
                    contentType: 'application/json',
                    success: function () {
                        console.log('ajax success of reset successful');

                        if (cmd == 'ColdReset') {
                            noty({
                                type: 'success',
                                text: 'Node reset successfully to Cold Reset',
                                timeout: 3000
                            });
                        } else if (cmd == 'ResetProvisioning') {
                            noty({
                                type: 'success',
                                text: 'Node reset successfully to Reset Provisioning',
                                timeout: 3000
                            });
                        } else if (cmd == 'ResetFactory') {
                            noty({
                                type: 'success',
                                text: 'Node reset successfully to Factory Reset',
                                timeout: 3000
                            });
                        } else {
                            noty({
                                type: 'success',
                                text: 'Node reset successfully to Change FW Partition',
                                timeout: 3000
                            });
                        };

                    },
                    error: function (jqXHR, status, error) {
                        console.log('ajax failure (nodes): ' + status + ' - ' + error);
                        $('#loadingmsg').html('Cannot reset node.  API reported error: ' + error);
                    }
                });
            } else {

                noty({
                    type: 'error',
                    text: 'Node is not reachable.Please make sure if the node is connected',
                    timeout: 3000
                });

            }

        });

        ReactBootstrap.Dispatcher.on('Nodedetail.copyAlertsToPanel', function(alerts) {
            that.setState({alerts:alerts});
        });

        //ReactBootstrap.Dispatcher.on('Nodeform.getAlerts', function (nodeid) {
        ReactBootstrap.Dispatcher.on('Nodedetail.getAlerts', function (nodeid) {

            $.ajax({
                url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/alerts' + '/node/' + nodeid,
                data: '',
                method: 'GET',
                xhrFields: {
                    withCredentials: true
                },
                dataType: 'json',
                success: function (data) {
                    if (data.errors) {
                        console.log('/nodes API returned error:' + JSON.stringify(data));
                        $('#loadingmsg').html('Cannot retrieve node alerts list. ' + '/nodes API returned error: ' + JSON.stringify(data));
                    } else {
                        console.log('ajax success: ' + JSON.stringify(data));
                        $('#loadingmsg').html('Generating display.');
                        that.setState({
                            alerts: data.map(function (alert, index) {
                                alert.ufname = alert.ufname || '';
                                alert.idx = index;
                                alert.alertid = alert.alertid || '';
                                alert.type = alert.type || '';
                                alert.description = alert.description || '';
                                alert.severity = alert.severity || '';
                                return alert;
                            })
                        })
                    };
                },
                error: function (jqXHR, status, error) {
                    console.log('ajax failure (nodes): ' + status + ' - ' + error);
                    $('#loadingmsg').html('Cannot retrieve Nodes Alerts List.  API reported error: ' + error);
                }
            });

        });


        ReactBootstrap.Dispatcher.on('Nodeform.dismissAlert', function (alertid, selectedidx) {
            $.ajax({
                url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/alerts' + '/dismiss/' + alertid,
                data: '',
                contentType: 'application/json',
                method: 'POST',
                xhrFields: {
                    withCredentials: true
                },
                dataType: 'json',
                success: function (data) {
//                    var newState = React.addons.update(this.state, { alerts: { $splice: [[selectedidx, 1]] } });
                    noty({ type: "success", text: 'Alert "' + alertid + '" successfully dismissed.' });
//                    this.setState(newState);
this.setState(this.state);
                }.bind(that),

                error: function (jqXHR, status, error) {
                    console.log('ajax failure (dismiss alert): ' + status + ' - ' + error);
                    $('#loadingmsg').html('Cannot dismiss the selected alert.  API reported error: ' + error);
                }
            });
        });

        ReactBootstrap.Dispatcher.on('Nodeform.multiDismissAlert', function (alertid_info, selectedidx) {
            for (var i in alertid_info) {
                var k = 0
                $.ajax({
                    url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/alerts' + '/dismiss/' + alertid_info[i].alertid,
                    data: '',
                    contentType: 'application/json',
                    method: 'POST',
                    xhrFields: {
                        withCredentials: true
                    },
                    dataType: 'json',
                    success: function (data) {
                        k++
                        if (k == alertid_info.length) {
                            ReactBootstrap.Dispatcher.emit('Nodelist.multiSelect', that.state.selected_nodes)
                            noty({ type: "success", text: "Alerts successfully dismissed." });
                        }

                    }.bind(that),
                    error: function (jqXHR, status, error) {
                        console.log('ajax failure (dismiss alert): ' + status + ' - ' + error);
                        noty({ type: "error", text: "Unable to dismiss Alerts" });
                    }
                });
            }

        });

        ReactBootstrap.Dispatcher.on('Nodelist.toggleDetail', function () {
            if (that.state.ui.detail == "hidden") {
                that.showDetail();
            } else {
                that.hideDetail();
            }
        });

        ReactBootstrap.Dispatcher.on('Nodedetail.togglePin', function () {
            if (that.state.ui.detail == "pinned") {
                that.unpinDetail();
            } else {
                that.pinDetail();
            }
        });

    },

    componentDidUpdate: function(prevProps, prevState) {
        if (this.state.ui.detail != prevState.ui.detail) {
            $(window).trigger('resize');
        };
    },

    componentWillUnmount: function () {
        NSN.nodeID = "-1";
        ReactBootstrap.Dispatcher.removeAllListeners('Nodelist.select');
        ReactBootstrap.Dispatcher.removeAllListeners('Nodelist.multiSelect');
        ReactBootstrap.Dispatcher.removeAllListeners('Nodelist.add');
        ReactBootstrap.Dispatcher.removeAllListeners('Nodeform.reset');
        ReactBootstrap.Dispatcher.removeAllListeners('Nodeform.delete');
        ReactBootstrap.Dispatcher.removeAllListeners('Nodeform.save');
        ReactBootstrap.Dispatcher.removeAllListeners('Nodeform.getAlerts');
        ReactBootstrap.Dispatcher.removeAllListeners('Nodeform.getFixtures');
        ReactBootstrap.Dispatcher.removeAllListeners('Nodeform.schedule');
        ReactBootstrap.Dispatcher.removeAllListeners('Nodeform.image');
        ReactBootstrap.Dispatcher.removeAllListeners('Nodeform.pushSchedule');
        ReactBootstrap.Dispatcher.removeAllListeners('Nodeform.assignedconfig');
        ReactBootstrap.Dispatcher.removeAllListeners('Nodeform.viewlocalsettings');
        ReactBootstrap.Dispatcher.removeAllListeners('Nodeform.viewfactorysettings');
        ReactBootstrap.Dispatcher.removeAllListeners('Nodeform.viewprovsettings');
        ReactBootstrap.Dispatcher.removeAllListeners('Nodeform.fixtureUpdate');
        ReactBootstrap.Dispatcher.removeAllListeners('Nodeform.nodereset');
        ReactBootstrap.Dispatcher.removeAllListeners('Nodemultigroupform.save');
        ReactBootstrap.Dispatcher.removeAllListeners('Nodeform.dismissAlert');
        ReactBootstrap.Dispatcher.removeAllListeners('Nodeform.multiDismissAlert');
        ReactBootstrap.Dispatcher.removeAllListeners('Nodemultiformwifi.save');
        ReactBootstrap.Dispatcher.removeAllListeners('Nodemultformserver.save');
        ReactBootstrap.Dispatcher.removeAllListeners('Nodeform.setconfig');
        ReactBootstrap.Dispatcher.removeAllListeners('Nodeform.firmwareassign');
        ReactBootstrap.Dispatcher.removeAllListeners('Nodeform.setfirmware');
        ReactBootstrap.Dispatcher.removeAllListeners('Nodeform.configWifiUpdate');
        ReactBootstrap.Dispatcher.removeAllListeners('Nodeform.configServerUpdate');
        ReactBootstrap.Dispatcher.removeAllListeners('Nodemultiformfixture.save');
        ReactBootstrap.Dispatcher.removeAllListeners('Nodeformconfig.save');
        ReactBootstrap.Dispatcher.removeAllListeners('Nodemultiformserver.save');
        ReactBootstrap.Dispatcher.removeAllListeners('Nodeform.lightLevel');
        ReactBootstrap.Dispatcher.removeAllListeners('Nodeform.getLightLevel');
        ReactBootstrap.Dispatcher.removeAllListeners('Nodeform.getConnectionSince');
        ReactBootstrap.Dispatcher.removeAllListeners('Nodemap.maximize');
        ReactBootstrap.Dispatcher.removeAllListeners('Nodemap.minimize');
        ReactBootstrap.Dispatcher.removeAllListeners('Nodemap.selectNode');
        ReactBootstrap.Dispatcher.removeAllListeners('Nodeform.getDiagnostic');
        ReactBootstrap.Dispatcher.removeAllListeners('Nodeform.getSensorsNetwork');
        ReactBootstrap.Dispatcher.removeAllListeners('Nodeform.getSensorsAmbient');
        ReactBootstrap.Dispatcher.removeAllListeners('Nodeform.getSensorsEnergy');
        ReactBootstrap.Dispatcher.removeAllListeners('Nodelist.toggleDetail');
        ReactBootstrap.Dispatcher.removeAllListeners('Nodedetail.togglePin');
        ReactBootstrap.Dispatcher.removeAllListeners('Nodedetail.getAlerts');
        ReactBootstrap.Dispatcher.removeAllListeners('Nodedetail.copyAlertsToPanel');
        ReactBootstrap.Dispatcher.removeAllListeners('Nodeform.getConfigs');
        ReactBootstrap.Dispatcher.removeAllListeners('Nodeform.getGroups');
        ReactBootstrap.Dispatcher.removeAllListeners('Nodeform.getFirmwaresOtas');
        ReactBootstrap.Dispatcher.removeAllListeners('Nodeform.startVPN');
        ReactBootstrap.Dispatcher.removeAllListeners('Nodeform.stopVPN');

    },

    render() {
        var hstyle = {
            height: helpers.calcHeight(90, 0) + 'px !important'
        };
        if (this.state.nodes && this.state.site) {
            var Detail;
            if (this.state.selected_nodes.length > 0) {
                Detail = (
                    <Nodemultidetail fixtures={this.state.fixtures} configs={this.state.configs} selected_nodes={this.state.selected_nodes} nodes={this.state.nodes} selected_nodesmodel={this.state.selected_nodesmodel} multi_alerts={this.state.multi_alerts} groups={this.state.groups} detail_state={this.state.ui.detail} />
                );
            } else {
                Detail = (
                    <Nodedetail fixtures={this.state.fixtures} firmwares={this.state.firmwares} otas={this.state.otas} configs={this.state.configs} allNodes={this.state.nodes} alerts={this.state.alerts} nodeID={this.state.nodeID} detail_state={this.state.ui.detail} />
                );
            };

            var listStyle = {width:"33%",height:"100%",overflow:"hidden",backgroundColor:"#FFF",position:"absolute",zIndex:"300",left:"0px",top:"0px",boxShadow:"2px 0px 2px 0px #CCC"};
            switch (this.state.ui.detail) {
                case "unpinned": 
                    var detailStyle = {opacity:"0.9",position:"absolute",zIndex:"500",backgroundColor:"#FFF",overflowY:"hidden",overflowX:"hidden",border:"1px solid #EEE"};
                    $.extend(detailStyle, this.detailUnpinnedPosition);
                    var mapStyle = {width:"67%",height:"100%",position:"absolute",top:"0px",left:"33%",zIndex:"100"};
                    $("#node-detail-panel").draggable("enable").resizable("enable");
                    break;
                case "pinned":
                    detailStyle = {position:"absolute", zIndex:"200", backgroundColor:"#FFF", left:"33%", width:"25%", height:"100%"};
                    mapStyle = {width:"42%",height:"100%",position:"absolute",top:"0px",left:"58%",zIndex:"100"};
                    $("#node-detail-panel").draggable("disable").resizable("disable");
                    break;
                case "hidden":
                    detailStyle = {position:"absolute", left:"-3000px"};
                    mapStyle = {width:"67%",height:"100%",position:"absolute",top:"0px",left:"33%",zIndex:"100"}; 
                    break;
            };

            return (
                <Container id='body' className="node-body" style={{ backgroundColor: "#FFF", marginTop: "0px !important" }}>
                  <div id="node-list-panel" data-state="closed" style={listStyle}>
                    <Nodelist minmax="max" nodes={this.state.nodes} selected_nodes={this.state.selected_nodes} alerts={this.state.alerts} detail_state={this.state.ui.detail} ui_state={this.state.ui} />
                  </div>
                  <div id="node-detail-panel" style={detailStyle}>
                    {Detail}  {/*}  either single-node or multi-node detail {*/}
                  </div>
                  <div id="node-map-panel" data-state="closed" style={mapStyle}>
                    <Nodemap minmax="max" nodes={this.state.nodes} site={this.state.site} center={this.state.center} zoom={this.state.zoom} detail_state={this.state.ui.detail} />
                  </div>
                  <ScheduleView context="node" show={this.state.showschedule} entity={this.state.selectednode} />
                  <ImageView context="node" show={this.state.showimage} node={this.state.selectedvideonode} />
                  <ConfigView context="node" show={this.state.showassignedconfig} entity={this.state.selectednode} />
                  <LocalConfigView context="node" show={this.state.showlocalsettings} entity={this.state.selectednode} />
                  <FactoryConfigView context="node" show={this.state.showfactorysettings} entity={this.state.selectednode} />
                  <ProvConfigView context="node" show={this.state.showprovsettings} entity={this.state.selectednode} />
                 <FirmwareView context="node" show={this.state.showsetfirmware} entity={this.state.selectednode} selectedfirmware={this.state.selectedfirmware}/>
                </Container>
            );
        }
        var forSite = (NSN && NSN.siteName && NSN.siteName != '') ? (' for ' + NSN.siteName) : '';
        return (
            <Container id='body' className="fade-out">
                <Grid>
                    <Row>
                        <Col sm={12}>
                            <PanelContainer>
                                <Panel>
                                    <PanelBody style={hstyle} ref="loading">
                                        <h2 id="loadingmsg" style={{ paddingTop: "16%", textAlign: "center" }}>Loading node information{forSite}.</h2>
                                        <div style={{ textAlign: "center" }}><img src="/imgs/loading.gif" /></div>
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
