import classNames from 'classnames';
import helpers from 'global/utils/helpers';
import {
    State,
    Navigation
} from 'react-router';
import Nodeform from 'components/nodes/nodeform';

var Nodedetail = React.createClass({

    getInitialState: function () {
        return null;
    },

    propTypes: {
        nodeID: React.PropTypes.string.isRequired,
        fixtures: React.PropTypes.array.isRequired,
        firmwares: React.PropTypes.array.isRequired,
        configs: React.PropTypes.array.isRequired,
        otas: React.PropTypes.array.isRequired,
        alerts: React.PropTypes.array.isRequired,
        detail_state: React.PropTypes.string,
        allNodes: React.PropTypes.array.isRequired
    },

    getNode: function (nodeID) {
        var that = this;

        if (nodeID == "0" || nodeID == "-1") {  // (this should never be true?)
            return {
                nodeid: "",
                name: "",
                model: "",
                level: "1",
                ip: "",
                country_code: "",
                latitude: "",
                longitude: "",
                time_zone: "",
                meshId: "",
                note: "",
                baseStation: "",
                configStatus: "",
                publicKey: "",
                signature: "",
                configToken: "",
                softwareVersion: "",
                mfgDate: "",
                circuit: "",
                building: "",
                levelInfo: null,
                idx: -1
            };
        };

        var idx = helpers.get_idx(that.props.allNodes, {nodeid: nodeID} , 'nodeid');
        var node = that.props.allNodes[idx];

        // we need to save and restore the various status indicators and alerts
        var save_status = {
            net_stat: node.net_stat,
            lig_stat: node.lig_stat,
            sen_stat: node.sen_stat,
            net_stats: node.net_stats,
            lig_stats: node.lig_stats,
            sen_stats: node.sen_stats,
            alerts: $.extend(true, [], node.alerts)
        };                

        $.ajax({
            url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/nodes/' + nodeID,
            data: '',
            method: 'GET',
            async: false,
            "xhrFields": {
                withCredentials: true
            },
            dataType: 'json',
            success: function (node) {
                $("#loadingmsg").html("Generating display.");
                console.log("Retrieving node info: " + JSON.stringify(node));
                that.props.nodeID = node.nodeid;
                node.nodeid = node.nodeid || "";
                node.name = node.name || "";
                node.model = node.model ? node.model : "";
                node.type = node.model ? helpers.modelType(node.model) : "";
                node.subType = node.subType || "";
                node.voltageType = node.voltageType || "";
                node.ip = node.ip || "";
                node.meshId = node.name || "";
                node.note = node.name || "";
                node.baseStation = node.baseStation || "";
                node.publickKey = node.publicKey || "";
                node.signature = node.signature || "";
                node.remoteNetwork = node.remoteNetwork || "";

                node.bssid = node.bssid || "";
                node.imei = node.imei || "";
                node.imsi = node.imsi || "";
                node.apn = node.apn || "";
                node.iccid = node.iccid || "";
                node.channel = node.channel || "";
                node.mac = node.mac || "";
                node.auth = node.auth || "";

                node.configToken = node.configToken || "";
                node.mfgDate = node.mfgDate || "";
                node.circuit = node.circuit || "";
                node.latitude = node.latitude || "";
                node.longitude = node.longitude || "";
                node.time_zone = node.time_zone || "";
                node.country_code = node.country_code || "";
                node.building = node.building || "";
                node.scheduleid = node.scheduleid || "";
                node.schedulename = node.schedulename || "";
                node.etdhprofileid = node.etdhprofileid || "";
                node.dhprofilename = node.dhprofilename || "";
                node.pdprofileid = node.pdprofileid || "";
                node.pdprofilename = node.pdprofilename || "";

                node.fixtureid= node.fixtureid|| "";
                node.fixturename = node.fixturename || "";
                node.fixtureType = node.fixtureType || "";
                node.modemRevEd = node.modemRevEd || "";
                node.level = node.level || "1";
                node.configname = node.configname || "default";

                if(typeof node.firmwareLastUpdated != "undefined"){
                    node.firmwareLastUpdated = moment(node.firmwareLastUpdated).format("MM/DD/YY HH:MM:ss Z");
                } else {
                    node.firmwareLastUpdated = "";
                }
                node.softwareVersion = node.softwareVersion || "";

                // restore saved status
                Object.assign(node, save_status);

                var requests = [];
                var responses = {};

                if((node.model == "falcon-q") || (node.model == "merlin") || (node.model == "vdkmaster") || (node.model == "cnext")){
                    // Get firmwareid:
                    if(node.softwareVersion != ""){
                        var build_firmwareid = node.softwareVersion.substring(0,7) + "-" + helpers.modelInternalName(node.model);
                        requests.push($.ajax({
                            url: NSN.apiURL + 'firmwares/'+ build_firmwareid,
                            data: '',
                            method: 'GET',
                            xhrFields: {
                                withCredentials: true
                            },
                            dataType: 'json',
                            success: function (data1) {
                                responses["softwareVersion"] = data1.name + '_' + (data1.when.substring(0,10));
                            },
                            error: function () {
                                responses["softwareVersion"] = "";
                                noty({
                                    type: 'information',
                                    text: 'Could not retrieve Firmware Version for ' + nodeID + '.'
                                });
                            }
                        }));
                    }
                };

                if (node.type == "Lighting") {
                    requests.push($.ajax({
                        url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/nodes/' + nodeID + '/light_status',
                        type: 'GET',
                        xhrFields: {
                            withCredentials: true
                        },
                        dataType: 'json',
                        contentType: 'application/json',
                        success: function (data) {
                            responses["levelInfo"] = data;
                            },
                        error: function () {
                            responses["levelInfo"] = {};
                            noty({
                                type: 'information',
                                text: 'Could not determine current Light Level for ' + nodeID + '.'
                            });
                        }
                    }));
                } else {
                    node.levelInfo = {};
                }; 

                var now = new Date(),
                    yesterday = new Date(now.getTime() - (24 * 60 * 60 * 1000));

                requests.push($.ajax({
                    url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/nodes/' + nodeID + '/sensors/cc/date/' + yesterday.toISOString() + '/limit/1',
                    data: '',
                    method: 'GET',
                    xhrFields: {
                        withCredentials: true
                    },
                    dataType: 'json',
                    success: function (data) {
                        responses["connectioncount"] = data.datapoints.length ? data.datapoints[0].value : 0;
                    },
                    error: function() {
                        responses["connectioncount"] = 0;
                        noty({
                            type: 'information',
                            text: 'Could not determine Connection Count for ' + nodeID + '.'
                        });
                    }
                }));
/*
                requests.push($.ajax({
                    url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/alerts' + '/node/' + nodeID,
                    data: '',
                    method: 'GET',
                    xhrFields: {
                        withCredentials: true
                    },
                    dataType: 'json',
                    success: function (data) {
                        responses["alerts"] = data.map(function (alert, index) {
                                alert.idx = index;
                                alert.alertid = alert.alertid || '';
                                alert.type = alert.type || '';
                                alert.msg = alert.msg || '';
                                alert.severity = alert.severity || '';
                                return alert;
                            })
                        ReactBootstrap.Dispatcher.emit('Nodedetail.copyAlertsToPanel', responses["alerts"]);
                        },
                    error: function() {
                        responses["alerts"] = [];
                        ReactBootstrap.Dispatcher.emit('Nodedetail.copyAlertsToPanel', []);
                        noty({
                            type: 'information',
                            text: 'Could not retrieve Alerts for ' + nodeID + '.'
                        });
                    }
                }));
*/

                if (helpers.isInternalUser() 
                    && ((node.model == "falcon-q") || (node.model == "merlin") || (node.model == "vdkmaster") || (node.model == "cnext"))){
                    if (node.net_stat) {
                        // Get VPN IP:
                        requests.push($.ajax({
                            url: NSN.apiURL + "customers/" + NSN.customerID + "/sites/" + NSN.siteID + "/nodes/" + node.nodeid + "/vpn/query",
                            data: '',
                            method: 'PUT',
                            processData : false,
                            contentType: 'application/json',
                            xhrFields: {
                                withCredentials: true
                            },
                            dataType: 'json',
                            success: function (data) {
                                responses["vpnip"] = data;
                            },
                            error: function () {
                                responses["vpnip"] = "";
                                noty({
                                    type: 'information',
                                    text: 'Could not retrieve VPN IP for ' + nodeID + '.'
                                });
                            }
                        }));
                    } else {
                        node.vpnip = "";
                    }
                };

//                that.setState(node);

                $.when.apply($, requests).then(function(results) {
                    Object.assign(node, responses);
                    that.setState(node);
                }, function(results) {
                    Object.assign(node, responses);
                    that.setState(node);
                });
            },
        error: function(){
            noty({
                type: 'error',
                text: 'Could not retrieve basic information for ' + nodeID + '.'
            });
            }
        });

    },

    componentDidMount: function () {
        this.getNode(this.props.nodeID);
        $("#node-detail-panel").draggable({handle:"h2",cursor:"move"})
            .resizable({handles:"all",
              stop: function( event, ui ) {
                $("#innerAccordion").css({height: ($("#node-detail-panel").height() - 160) + "px"});
              }
        });
    },

    componentDidUpdate: function() {
        $("#innerAccordion").css({height: ($("#node-detail-panel").height() - 160) + "px"});
    },

    componentWillReceiveProps: function (nextProps) {
        if (this.props.nodeID != nextProps.nodeID) {
            this.getNode(nextProps.nodeID);
            ReactBootstrap.Dispatcher.emit("Nodedetail.getAlerts", nextProps.nodeID);
        };
    },

    togglePin: function () {
        ReactBootstrap.Dispatcher.emit('Nodedetail.togglePin');
    },

    render: function () {
        var pinButton = (
            <div style={{ position: "absolute", top: "15px", right: "4px" }}>
              <img src={this.props.detail_state == "pinned"? "/imgs/new-icons/detach-dock.svg":"/imgs/new-icons/attach-dock.svg"} 
                    title={this.props.detail_state == "pinned"? "Undock (allow drag)":"Dock"}
                    height="24" onClick={this.togglePin} />
            </div>
        );
        if (this.state && typeof this.state.levelInfo != "undefined") {
            if (this.props.nodeID == "-1") {
                return (
                    <div>
                        {pinButton}
                        <h2 id="nodeDetailHeading" style={{textAlign: "center", padding: "100px 0px", fontSize: "24px"}} >
                            <i>Select a Node from the table<br />or<br />from the map.</i>
                        </h2>
                    </div>
                );
            }
            return (
                <div>
                   {pinButton}
                   <Nodeform fixtures={this.props.fixtures} otas={this.props.otas} firmwares={this.props.firmwares}
                             configs={this.props.configs} alerts={this.props.alerts} allNodes={this.props.allNodes}
                             node={this.state} detail_state={this.props.detail_state} />
                </div>
            )
        } else {
            return (
                <div>
                    {pinButton}
                    <h2 id="nodeDetailHeading" style={{textAlign: "center",padding: "100px 0px",fontSize: "24px"}} >
                        <i>Select a Node from the table<br />or<br />from the map.</i>
                    </h2>
                </div>
            )
        };

    }

});

module.exports = Nodedetail;