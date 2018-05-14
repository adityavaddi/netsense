import classNames from 'classnames';
import auth from 'global/utils/auth';
import helpers from 'global/utils/helpers';
import LightControl from 'components/lightcontrol';
import DisabledTextField from 'components/disabledtextfield';
import NodeSensors from 'components/nodes/nodesensors';
import Nodeconfigform from 'components/nodes/nodeconfigform';
import NodeFirmware from 'components/nodes/nodefirmware';
import { State, Navigation } from 'react-router';
import { Link, withRouter } from 'react-router';

var Nodeform = React.createClass({

  getInitialState: function () {
    return this.props.node
  },

  propTypes: {
    node: React.PropTypes.object.isRequired,
    otas: React.PropTypes.array.isRequired,
    detail_state: React.PropTypes.string,
    alerts:React.PropTypes.array.isRequired,
    allNodes: React.PropTypes.array.isRequired
  },

  handleChange: function (key) {
    return function (e) {
      var state = {};
      state[key] = e.target.value;
      this.setState(state);
    }.bind(this);
  },

  handleSubmit: function (e) {
    e.stopPropagation();
    e.preventDefault();
    ReactBootstrap.Dispatcher.emit("Nodeform.save", this.state);
    return false;
  },

  handleConfigServerUpdate: function (e) {
    e.stopPropagation();
    e.preventDefault();

    if (confirm("Are you sure you want to update server information for the selected node?")) {
      ReactBootstrap.Dispatcher.emit("Nodeform.configServerUpdate", this.state);
    };
  },

  handleFixtureUpdate: function (e) {
    e.stopPropagation();
    e.preventDefault();
    ReactBootstrap.Dispatcher.emit("Nodeform.fixtureUpdate", this.state);
  },

  handleConnectionStatus: function (e) {
    e.stopPropagation();
    e.preventDefault();

    if (!$(e.target).hasClass('collapsed')) {
      ReactBootstrap.Dispatcher.emit("Nodeform.getConnectionSince", this.state);
    }
  },


  handleDiagnostic: function (e) {
    if (typeof e != "undefined") {
      e.stopPropagation();
      e.preventDefault();
    };

    if (!$("#diagnostic-heading a").hasClass('collapsed')) {
      ReactBootstrap.Dispatcher.emit("Nodeform.getDiagnostic", this.state);
    }
  },

  handleSensorsNetwork: function (e) {
    if (typeof e != "undefined") {
      e.stopPropagation();
      e.preventDefault();
    };

//    if (!$("#sensors-network-heading a").hasClass('collapsed')) {
      ReactBootstrap.Dispatcher.emit("Nodeform.getSensorsNetwork", this.state);
//    }
  },

  handleSensorsAmbient: function (e) {
    if (typeof e != "undefined") {
      e.stopPropagation();
      e.preventDefault();
    };

    if (!$("#sensors-ambient-heading a").hasClass('collapsed')) {
      ReactBootstrap.Dispatcher.emit("Nodeform.getSensorsAmbient", this.state);
    }
  },

  handleSensorsEnergy: function (e) {
    if (typeof e != "undefined") {
      e.stopPropagation();
      e.preventDefault();
    };

    if (!$("#sensors-energy-heading a").hasClass('collapsed')) {
      ReactBootstrap.Dispatcher.emit("Nodeform.getSensorsEnergy", this.state);
    }
  },

  handleAlerts: function (e) {
    e.stopPropagation();
    e.preventDefault();

    if (!$(e.target).hasClass('collapsed')) {
      ReactBootstrap.Dispatcher.emit("Nodeform.getAlerts", this.state);
    }
  },

  handleDelete: function (e) {
    e.stopPropagation();
    e.preventDefault();
    ReactBootstrap.Dispatcher.emit("Nodeform.delete", this.state);
    return false;
  },

  handleReset: function (e) {
    e.stopPropagation();
    e.preventDefault();
    ReactBootstrap.Dispatcher.emit("Nodeform.reset", this.state);
  },

  handleFixture: function (e) {
    e.stopPropagation();
    e.preventDefault();
    if (!$(e.target).hasClass('collapsed')) {
      ReactBootstrap.Dispatcher.emit("Nodeform.getFixtures");
    };
  },

  handleSchedule: function (e) {
    e.stopPropagation();
    e.preventDefault();
    ReactBootstrap.Dispatcher.emit("Nodeform.schedule", "open", Object.assign({}, this.state));
  },

  handleImage: function (e) {
    e.stopPropagation();
    e.preventDefault();
    ReactBootstrap.Dispatcher.emit("Nodeform.image", "open", Object.assign({}, this.state));
  },

  handleAssignedConfig: function (e) {
    e.stopPropagation();
    e.preventDefault();
    ReactBootstrap.Dispatcher.emit("Nodeform.assignedconfig", "open", Object.assign({}, this.state));
  },

  handleLocalConfigurations: function(e) {
    e.stopPropagation();
    e.preventDefault();
    ReactBootstrap.Dispatcher.emit("Nodeform.viewlocalsettings", "open", Object.assign({}, this.state));
  },

  handleFactoryConfigurations: function(e) {
    e.stopPropagation();
    e.preventDefault();
    ReactBootstrap.Dispatcher.emit("Nodeform.viewfactorysettings", "open", Object.assign({}, this.state));
  },

  handleProvConfigurations: function(e){
    e.stopPropagation();
    e.preventDefault();
    ReactBootstrap.Dispatcher.emit("Nodeform.viewprovsettings", "open", Object.assign({}, this.state));
  },

  handleSetFirmware: function (e) {
    e.stopPropagation();
    e.preventDefault();
    ReactBootstrap.Dispatcher.emit("Nodeform.setfirmware", "open", Object.assign({}, this.state));

   /* if (confirm("Are you sure you want to update firmware for the selected node?")) {
      ReactBootstrap.Dispatcher.emit("Nodeform.setfirmware", Object.assign({}, this.state));
    }; */
  },

  handleViewJob: function(e){
    e.stopPropagation();
    e.preventDefault();
    this.props.router.push("/app/firmwareupdatepanel");
  },

  handleSetConfig: function (e) {
    e.stopPropagation();
    e.preventDefault();

    if (confirm("Are you sure you want to update config for the selected node?")) {
      ReactBootstrap.Dispatcher.emit("Nodeform.setconfig", Object.assign({}, this.state));
    };
  },

  handleNodeReset: function (e) {
    e.stopPropagation();
    e.preventDefault();

    if (confirm("Are you sure you want to reset the selected node?")) {
      ReactBootstrap.Dispatcher.emit("Nodeform.nodereset", this.state);
    };

  },

  calcHeight: function (pct, extra) {
    var h = (window && window.document) ? (($(window).height() - 75) * (pct / 100) + extra) : 500;
    return h;
  },

  handleVoltageType: function(voltageType){
    var r = '';

    switch (voltageType) {
      case 1:
          r = '277 V';
          break;
      case 2:
          r = '480 V';
          break;
      default:
          r = 'Undetermined'
    }
    return r;
  },

  handleSubType: function(subType,model) {
      var r = '';

      if(model == "unode-v3"){
        switch (subType) {
          case 1:
              r = 'ZMotion';
              break;
          case 2:
              r = 'SiHawk';
              break;
          default:
              r = 'Undetermined'
        }
      }
      else if(model == "unode-v4"){
        switch (subType) {
          case 1:
              r = 'Standard';
              break;
          case 2:
              r = 'Acuity';
              break;
          default:
              r = 'Undetermined'
        }
      }
      else if((model == "unode-v5") || (model == "unode-v6")){
        switch (subType) {
          case 1:
              r = 'Pop';
              break;
          case 2:
              r = 'Depop';
              break;
          default:
              r = 'Undetermined'
        }
      }
      else{
        switch (subType) {
          default:
              r = 'Undetermined'
        }
      }
      return r;
  },

/*
  handleLightOn: function () {
    ReactBootstrap.Dispatcher.emit("Nodeform.lightLevel", this.state.nodeid, "100", "30");
  },

  handleLightOff: function () {
    ReactBootstrap.Dispatcher.emit("Nodeform.lightLevel", this.state.nodeid, "0", "30");
  },

  handleLightLevel: function (e) {
    e.stopPropagation();
    e.preventDefault();

    if (!$(e.target).hasClass('collapsed')) {
      ReactBootstrap.Dispatcher.emit("Nodeform.getLightLevel", this.state);
    };
  },
*/

  handlePushSchedule: function (e) {
    e.stopPropagation();
    e.preventDefault();
    ReactBootstrap.Dispatcher.emit("Nodeform.pushSchedule", this.state.nodeid);
  },

  handleStartVPN: function (e) {
    e.stopPropagation();
    e.preventDefault();
    ReactBootstrap.Dispatcher.emit("Nodeform.startVPN", this.state.nodeid);
  },

  handleStopVPN: function (e) {
    e.stopPropagation();
    e.preventDefault();
    ReactBootstrap.Dispatcher.emit("Nodeform.stopVPN", this.state.nodeid);
  },

  _setAlerts: function(node, alerts){

    // creates a new alerts array for the node
    node.alerts = [];

    for (var i = 0; i < alerts.length; i++) {
        if (alerts[i].nodeid === node.nodeid) {
          node.alerts.push(alerts[i]);
        };
    };

    return node;

  },

  handleDismissAlert: function (alertid) {
    //e.stopPropagation();
    //e.preventDefault();

    if (confirm("Are you sure you want to dismiss the selected alert?")) {
      for (var i = 0, found = false; !found && i < this.props.alerts.length; i++) {
        if (this.props.alerts[i].alertid == alertid) {

          var selectedidx = this.props.alerts[i].idx;
          this.props.alerts[i].severity = "Clear";

          ReactBootstrap.Dispatcher.emit("Nodeform.dismissAlert", alertid, selectedidx);

          // As the node here did not have net_stat, so adding it in to checkif the condition does clear in helpers.jsx
          for (var k in this.props.allNodes) {
            if (this.props.allNodes[k].nodeid === this.props.node.nodeid) {
              this.props.node.net_stat = this.props.allNodes[k].net_stat;
            };
          };
          //  alerts was not a part of the node so finding the alerts from the props and adding them in
          var node = this._setAlerts(this.props.node, this.props.alerts);
          for (var j = 0; j < node.alerts.length; j++) {
            if (node.alerts[j].alertid == alertid) {
              node.alerts[j].severity = "Clear";
            };
          };
          this.props.node = helpers.setNodeStatus(node);
          ReactBootstrap.Dispatcher.emit("Nodeform.update.success", [this.props.node]);
          ReactBootstrap.Dispatcher.emit("Nodelist.nodeStatusUpdated", [this.props.node]);
          found = true;
        }
      }
    };

  },

  handleFirmwareSection: function(e){

    if (this.props.firmwares === null) {
      ReactBootstrap.Dispatcher.emit('Nodeform.getFirmwaresOtas');
    }

  },

  handleNodeConfigSection: function(e){

    if (this.props.configs === null) {
      ReactBootstrap.Dispatcher.emit('Nodeform.getConfigs');
    }

  },

  componentWillReceiveProps: function (nextProps) {
    if (this.props.node.nodeid != nextProps.node.nodeid || this.props.fixtures != nextProps.fixtures) {
      this.setState(nextProps.node);
    };
  },

  componentDidUpdate: function () {
    if (helpers.modelType(this.state.model) == "Lighting") {
      if (typeof this.state.levelInfo == "undefined"
        || typeof this.state.levelInfo.driver == "undefined"
        || typeof this.state.levelInfo.isscheduled == "undefined") {
        $("#nodelevel").html("(n/a)");
      } else {
        $("#nodelevel").html("("
          + this.state.levelInfo.driver
          + "%"
          + (this.state.levelInfo.isscheduled ? "" : "*")
          + ")");
      }
    };

    // the following will cause multiple sensor API calls only if the corresponding
    //  accordion panels are open
    if (NSN.userInfo.authorization[0].type == "sensity_user") {
//      this.handleDiagnostic();
    };
//    this.handleSensorsAmbient();
//    this.handleSensorsNetwork();
//    this.handleSensorsEnergy();
  },

  componentDidMount: function () {
    var that = this;
    // If user is a sensity user:

    if (NSN.userInfo.authorization[0].type == "sensity_user") {
      $("#diagnosticwrapper").show();
    }
    else {
      $("#diagnosticwrapper").hide();
    }

    ReactBootstrap.Dispatcher.on('Nodemap.moveNode', function (nodeid, latlng) {
      that.setState({ latitude: latlng.lat().toString(), longitude: latlng.lng().toString() });
    })

    $("#innerAccordion").css({height: ($("#node-detail-panel").height() - 160) + "px"});

  },

  componentWillUnmount: function () {
    ReactBootstrap.Dispatcher.removeAllListeners("Nodeform.moveNode");
  },


  render: function () {
    var disabled = !auth.allowed('CAN_CHANGE', 'NodeModel');
    var resetVisibility = ((NSN.userInfo.authorization[0].type == "sensity_user") || (NSN.userInfo.authorization[0].type == "partner_admin"));
    var resetOptions = (resetVisibility) ? (
      <select className="form-control" id="cmd" ref="cmd" value={this.state.cmd} onChange={this.handleChange('cmd')}>
        <option value="ColdReset">Cold Reset</option>
        <option value="ResetFactory">Factory Reset</option>
        <option value="ResetProvisioning">Provisioning Reset</option>
        {/*<option value="ChangeFWPartition">Change FW Partition</option> */} //Will be added once API support is available.
      </select>
    ) : (
        <select className="form-control" id="cmd" ref="cmd" value={this.state.cmd} onChange={this.handleChange('cmd')}>
          <option value="ResetProvisioning">Provisioning Reset</option>
          {/*<option value="ChangeFWPartition">Change FW Partition</option> */} //Will be added once API support is available.
        </select>
      );

    var setConfigVisibility = ((NSN.userInfo.authorization[0].type == "end_user_lighting_user") || (NSN.userInfo.authorization[0].type == "end_user_sensor_user"));
    var nodeSaveDeleteVisibility = ((NSN.userInfo.authorization[0].type == "partner_read_only") || (NSN.userInfo.authorization[0].type == "end_user_read_only"));
    var resetContainerVisibility = ((NSN.userInfo.authorization[0].type == "partner_read_only") || (NSN.userInfo.authorization[0].type == "end_user_read_only"));
    var setFixtureVisibility = ((NSN.userInfo.authorization[0].type !== "end_user_networking_user")  ? {} : { display: "none" });
    var setConfig = (setConfigVisibility)? (
      <button disabled='disabled' className="ns-form-btn btn-set-config-disable" id="setNodeConfig"> <b>Set Config </b></button>
    ) : (
      <button className="ns-form-btn btn-set-config-enable" id="setNodeConfig" onClick={this.handleSetConfig}> <b>Set Config</b></button>
    );
    var visibility = auth.allowed('CAN_CHANGE', 'NodeModel') ? {} : { display: "none" };

    var nodeSaveDeleteHtml =(nodeSaveDeleteVisibility)?(
        <div></div>
    ) : (
        <div style={{ position:"absolute",height:"58px",width:"100%",backgroundColor:"#FFF",bottom:"0px" }}>
          <div className="col-sm-6">
            <button type="button" className="ns-delete-btn" id="deleteNodeDetails" onClick={this.handleDelete}>
              <b>Delete</b></button>
          </div>

          <div className="col-sm-6 text-right">
            <button type="button" className="ns-save-btn" id="saveNodeDetails"  onClick={this.handleSubmit}>
              <b>Save</b> </button>
          </div>
        </div>
    ); 

    var node = this.props.node;

    var resetContainerHtml = (resetContainerVisibility) ? (
      <div> </div>
    ) : (
        <div >
          <div className="panel-heading">
            <h5 className="panel-title">
              <a className="collapsed" data-toggle="collapse" id="resetLabel" data-parent="#collapseTwonested" href="#collapseTwonested-collapseEleven">
                Reset
                        </a>
            </h5>
          </div>

          <div id="collapseTwonested-collapseEleven" className="panel-collapse collapse">
            <div className="panel-body">
              <form role="form" className="form-horizontal" data-nodeid={node.nodeid} >
                <div className="form-group">
                  <label htmlFor="cmd" className="control-label col-sm-3"> Select reset type:</label>
                  <div className="col-sm-6">
                    {resetOptions}
                  </div>
                </div>
              </form>

              <div className="text-right">
                <button type="button" className="ns-form-btn" id="applyReset" onClick={this.handleNodeReset}>
                  <Icon glyph="icon-fontello-ok" /> <b>Apply </b></button>
              </div>
            </div>
          </div>
        </div>
      );
    var subTypeStatus = this.handleSubType(node.subType,node.model);

    var voltageTypeStatus = this.handleVoltageType(node.voltageType);

    var subandVoltagetypeVisibility = (node.model == "unode-v2") ? { display: "none" } : {};

    // Check groups length:

    if (node.groupnamelist) {
      var groupslength = (node.groupnamelist.length);

      var groupsName;
      if (node.groupnamelist.length > 0) {
        groupsName = node.groupnamelist.join(" , ");
      }
      else {
        groupsName = "This node is not assigned to any groups."
      }
    }

    var visibilityAssignedConfig = (node.configname != "default") ? {} : { display: "none" };
    var backgroundConfigName = (node.configStatus == "pending") ? { backgroundColor: "#f0ad4e", float: "left", width: "auto" } : { backgroundColor: "#5cb85c", float: "left", width: "auto" };

    var hStyle = {
      overflow: 'auto',
      height: helpers.calcHeight(80,-170)
    };
    var acknowledgeVisibility = (NSN.userInfo.authorization[0].type == "partner_read_only") || (NSN.userInfo.authorization[0].type == "end_user_read_only");
    if (this.props.alerts) {
      console.log("props in node form &&&&",NSN.userInfo);
      var that = this;
      var NodeAlerttablerows = this.props.alerts.map(function (alert, index) {
      
        var alertname = (alert.ufname.length>0)?(alert.ufname):("Unknown");
        var alerttype = (alert.type.length>0)?(alert.type):("Unknown");
        var alertdescription = (alert.description.length>0)?(alert.description):("None");
        var alertseverity = (alert.severity.length>0)?(alert.severity):("Unknown");
        var alertupdated = (alert.updated.length>0)?(new Date(alert.updated).toString()):("Unknown");
       
        if (alert.type === "disconnected" && node.state.connectionstatus === "good") return (

          <dl key={index} style={{ display: "none", marginBottom: "5px" }} className="nodeAlertsWrapper">
            <dt>Name </dt>
            <dd> {alertname} </dd>
            <dt>Alarm Type </dt>
            <dd> {alerttype} </dd>
            <dt>Description </dt>
            <dd> {alertdescription} </dd>
            <dt>Severity</dt>
            <dd> {alertseverity} </dd>
            <dt>Date & Time</dt>
            <dd> {alertupdated} </dd>
          </dl>

        )
        else if (alert.severity != "Clear" && !acknowledgeVisibility) return (
          <div>
            <div className="nodeAlertsWrapper" data-idx={index}>
              <dl key={index} style={{ cursor: "pointer", marginBottom: "5px" }}>
                <dt>Name </dt>
                <dd> {alertname} </dd>
                <dt>Alarm Type </dt>
                <dd> {alerttype} </dd>
                <dt>Description </dt>
                <dd> {alertdescription} </dd>
                <dt>Severity</dt>
                <dd> {alertseverity} </dd>
                <dt>Date & Time</dt>
                <dd> {alertupdated} </dd>
              </dl>
              <button style={{ float: "right" }} className="ns-form-btn" onClick={that.handleDismissAlert.bind(this, alert.alertid)}><b>Acknowledge</b></button>
              <div style={{clear:"both"}}></div>
            </div>
            <br />
          </div>

        )
        else if (alert.severity != "Clear" && acknowledgeVisibility) return (
           <div>
            <div className="nodeAlertsWrapper" data-idx={index}>
              <dl key={index} style={{ cursor: "pointer", marginBottom: "5px" }}>
                <dt>Name </dt>
                <dd> {alertname} </dd>
                <dt>Alarm Type </dt>
                <dd> {alerttype} </dd>
                <dt>Description </dt>
                <dd> {alertdescription} </dd>
                <dt>Severity</dt>
                <dd> {alertseverity} </dd>
                <dt>Date & Time</dt>
                <dd> {alertupdated} </dd>
              </dl>
              <br />
            </div>
            <br />
          </div>
        );
      });

      var NodeDismissedAlerttablerows = this.props.alerts.map(function (alert, index) {
        var alertname = (alert.ufname.length>0)?(alert.ufname):("Unknown");
        var alerttype = (alert.type.length>0)?(alert.type):("Unknown");
        var alertdescription = (alert.description.length>0)?(alert.description):("None");
        var alertseverity = (alert.severity.length>0)?(alert.severity):("Unknown");
        var alertupdated = (alert.updated.length>0)?(new Date(alert.updated).toString()):("Unknown");
       

        if (alert.severity == "Clear") return (

          <div>
            <div className="nodeDismissedAlertsWrapper" data-idx={index}>
              <dl key={index} style={{ cursor: "pointer", marginBottom: "5px" }}>
                <dt>Name </dt>
                <dd> {alertname} </dd>
                <dt>Alarm Type </dt>
                <dd> {alerttype} </dd>
                <dt>Description </dt>
                <dd> {alertdescription} </dd>
                <dt>Severity</dt>
                <dd> {alertseverity} </dd>
                <dt>Date & Time</dt>
                <dd> {alertupdated} </dd>
              </dl>
            </div>
            <div style={{clear:"both"}}></div>
          </div>
        );

      });
    };

    if (this.props.fixtures) {
      var fOptions = this.props.fixtures.map(function (fixture, index) {
        return (
          <option key={index} selected={node.fixtureid === fixture.fixtureid} value={fixture.fixtureid}>{fixture.name}</option>
        );
      });
    };


    /*if (this.props.firmwares){
      var firmwareOptions = this.props.firmwares.filter(function(firmware, index){

        if (["falcon-q","merlin","vdkmaster","cnext"].indexOf(node.model) >=0){
          if(typeof firmware.type == "undefined") {
            return false;
          }
          else{
            return node.model == firmware.type;
          } 
        }
 
        if ((node.model == "unode-v2") || (node.model== "unode-v3") || (node.model =="unode-v4") || (node.model == "unode-v5") || (node.model == "unode-v6")){
          if(typeof firmware.type == "undefined") {
            return firmware;
          }
        }

      }).map(function(firmware, index) {
        if(typeof firmware.type != "undefined"){
          var firmwareList = firmware.name + "_" + firmware.when.substring(0,10);
          return (
            <option key={index} value={firmware.firmwareid}>{firmwareList}</option>
          );
        }
        else{
          return (
            <option key={index} value={firmware.firmwareid}>{firmware.name}</option>
          );
        }
        
      });
    };

    var firmwareDisabled = false;
    if (firmwareOptions.length == 0) {
      firmwareOptions = ["<option>Model has no defined Firmwares</option>"];
      firmwareDisabled = true;
    }; */

    if (this.props.configs){
      var configOptions = this.props.configs.filter(function(config, index) {
        return (node.model == config.model)
      }).map(function(config, index){
        return (
          <option value={config.configid}>{config.name}</option>
        );
      });
    };


    var visibilityMac = ["unode-v5","unode-v6"].indexOf(node.model)>=0 ? { display: "none" } : {};

    var visibilityRebootReasonApp = ["unode-v2"].indexOf(node.model)>=0 ? { display: "none" } : {};
    var visibilityGPS = ["unode-v4","unode-v5","unode-v6","cnext"].indexOf(node.model)>=0 ? {} : { display: "none" };
    //var visibilityModemFirmware = ["unode-v6"].indexOf(node.model)>=0 ? {} : { display: "none" };

    if (helpers.modelType(node.model) == "Lighting") {
      var level = (node.nodeid == "" || typeof this.state.levelInfo == "undefined")
        ? ""
        : ((typeof this.state.levelInfo.driver != "undefined")
          ? (" (" + this.state.levelInfo.driver + "%"
            + (this.state.levelInfo.isscheduled ? "" : "*") + ")")
          : "");
      var heading = (<span><span><Icon glyph="icon-fontello-right-dir" /> {node.nodeid}</span>
          <span id="nodelevel" style={{ fontSize: "20px" }}>{level}</span></span>
        );
    } else {
      heading = (<span><Icon glyph="icon-fontello-right-dir" /> {node.nodeid}</span>);
    };

    /*var count = 0;
    for(var i=0;i<this.props.otas.length;i++){
      if(this.props.otas[i].firmwareid === node.firmwareid){
        count ++;
      }
    }

    var viewBtnStyle = ((node.firmwareid=="")||(count==0))?{display:"none"}:{}; */
    var modelType = helpers.modelType(node.model);
    return (
      <div>
      <div className="accordionWrapper" style={{height:"calc(100% - 160px)"}}>
        <h2 className="noselect" style={{position:"relative",top:"0px",left:"9px",width:"90%",fontSize:"20px",cursor:"move"}}>{heading}</h2>
        {modelType == "Lighting" &&
           <button id="viewSchedule" style={{ position: "absolute", top: "20px", right: "34px" }} className="ns-form-btn" onClick={this.handleSchedule}><b>View Schedule</b></button>
        }
        {modelType == "Video" &&
           <button id="viewImage" style={{ position: "absolute", top: "20px", right: "34px" }} className="ns-form-btn" onClick={this.handleImage}><b>View Image</b></button>
        }
        <div id="innerAccordion" style={{overflow:"auto",height:"calc(100% - 160px)"}}>
        <div className="panel-group" id="accordion" >
          <div className="panel panel-default">
            <div className="panel-heading">
              <h4 className="panel-title">
                <a data-toggle="collapse" id="basicIdentifiers" data-parent="#accordion" href="#collapseOne">
                  Basic Identifiers
                </a>
              </h4>
            </div>
            <div id="collapseOne" className="panel-collapse collapse in">
              <div className="panel-body">
                <form role="form" className="form-horizontal" data-nodeid={node.nodeid} >
                  <div className="form-group">
                    <label htmlFor="nodeid" className="control-label col-sm-3">Node ID:</label>
                    <div className="col-sm-6">
                      <input type="text" disabled={this.props.node.nodeid !== ""} className="form-control" id="nodeid" ref="nodeid" value={this.state.nodeid} onChange={this.handleChange('nodeid')} />
                    </div>
                  </div>
                  <div className="form-group">
                    <label htmlFor="name" className="control-label col-sm-3">Name:</label>
                    <div className="col-sm-6">
                      <input type="text" className="form-control" id="name" ref="name" value={this.state.name} onChange={this.handleChange('name')} />
                    </div>
                  </div>
                  <div className="form-group">
                    <label htmlFor="serialnumber" className="control-label col-sm-3" style={{lineHeight:"18px",paddingTop:"4px"}}>Serial Number:</label>
                    <div className="col-sm-6">
                      <input type="text" className="form-control" id="serialnumber" ref="serialnumber" value={this.state.serialnumber} onChange={this.handleChange('serialnumber')} />
                    </div>
                  </div>
                  <div className="form-group" style={visibilityMac}>
                    <label htmlFor="mac" className="control-label col-sm-3" style={{lineHeight:"18px",paddingTop:"4px"}}>MAC Address:</label>
                    <div className="col-sm-6">
                      <input type="text" className="form-control" id="mac" ref="mac" disabled="disabled" value={this.state.mac} />
                    </div>
                  </div>
                </form>
              </div>
            </div>
          </div>

          <div className="panel panel-default">
            <div className="panel-heading">
              <h4 className="panel-title">
                <a className="collapsed" data-toggle="collapse" id="configlabel" data-parent="#accordion" href="#collapseTwo" >
                  Config
                </a>
              </h4>
            </div>
            <div id="collapseTwo" className="panel-collapse collapse">
              <div className="panel-body">
                <div id="collapseTwonested">
                  <div className="panel panel-default">
                    <div className="panel-heading">
                      <h5 className="panel-title">
                        <a data-toggle="collapse" id="groupsAndProfilesLabel" data-parent="#collapseTwonested" href="#collapseTwonested-collapseOne">
                          Groups ({groupslength}) {helpers.modelType(node.model) == "Lighting" ? "and Profiles":""}
                        </a>
                      </h5>
                    </div>
                    <div id="collapseTwonested-collapseOne" className="panel-collapse collapse in">
                      <div className="panel-body">
                        <form role="form" className="form-horizontal">
                          <div className="form-group">
                            <label htmlFor="grouplist" className="control-label col-sm-3" style={{ lineHeight: "18px", paddingTop: "4px" }}>Groups:</label>
                            <div className="col-sm-8" style={{ marginTop: "10px"}}>
                              <span id="groupnamelist" style={{padding:"0px"}}>{groupsName}</span>
                            </div>
                          </div>
                          {helpers.modelType(node.model) == "Lighting" ? (
                            <div>
                            {helpers.isInternalUser() &&
                              <div className="form-group">
                                <label htmlFor="dhprofilename" className="control-label col-sm-3" style={{ lineHeight: "18px", paddingTop: "4px" }}>Daylight Harvesting Profile:</label>
                                <div className="col-sm-8" style={{ marginTop: "10px"}}>
                                  {this.props.node.etdhprofileid ? this.props.node.etdhprofilename : this.props.node.etdhprofilename="None"}
                                </div>
                              </div>
                            }
                            <div className="form-group">
                              <label htmlFor="pdprofilename" className="control-label col-sm-3" style={{ lineHeight: "18px", paddingTop: "4px" }}>Proximity Dimming Profile:</label>
                              <div className="col-sm-8" style={{ marginTop: "10px"}}>
                                {this.state.pdprofileid ? this.state.pdprofilename : this.state.pdprofilename="None"}
                              </div>
                            </div>
                            </div>
                            ):""}
                        </form>
                      </div>
                    </div>
                  </div>

{["unode-v5", "unode-v6", "vdkmaster", "merlin"].indexOf(this.state.model) < 0 &&
                  (
                  <div className="panel panel-default" id="wifi">
                    <div className="panel-heading">
                      <h5 className="panel-title">
                        <a className="collapsed" data-toggle="collapse" id="wifiLabel" data-parent="#collapseTwonested" href="#collapseTwonested-collapseTwo" onClick={this.handleConnectionStatus}>
                          Wi-Fi
                        </a>
                      </h5>
                    </div>
                    <div id="collapseTwonested-collapseTwo" className="panel-collapse collapse">
                      <div className="panel-body">
                        <form role="form" className="form-horizontal" data-nodeid={node.nodeid} >
                          <DisabledTextField label="BSSID" fieldid="bssid" value={this.state.bssid} />
                          <DisabledTextField label="Channel" fieldid="channel" value={this.state.channel} />
                          <DisabledTextField label="IP" fieldid="ip" value={this.state.ip} />
                          <DisabledTextField label="MAC" fieldid="mac" value={this.state.mac} />
                          <DisabledTextField label="Security Method" fieldid="auth" value={this.state.auth} />
                          <DisabledTextField label="SSID" fieldid="remoteNetwork" value={this.state.remoteNetwork} />
                          <DisabledTextField label="Connection Count" fieldid="connectioncount" value={this.state.connectioncount} />
                          <DisabledTextField label="Connection Since" fieldid="since" value={this.state.since} />
                       </form>
                      </div>
                    </div>
                  </div>
                  )
                  }

{["vdkmaster", "falcon-q", "merlin", "cnext"].indexOf(this.state.model) >= 0 &&
                  (
                  <div className="panel panel-default" id="ethernet">
                    <div className="panel-heading">
                      <h5 className="panel-title">
                        <a className="collapsed" data-toggle="collapse" id="ethernetLabel" data-parent="#collapseTwonested" href="#collapseTwonested-collapseThree" onClick={this.handleConnectionStatus}>
                          Ethernet
                        </a>
                      </h5>
                    </div>
                    <div id="collapseTwonested-collapseThree" className="panel-collapse collapse">
                      <div className="panel-body">
                        <form role="form" className="form-horizontal" data-nodeid={node.nodeid} >
                          <DisabledTextField label="IP" fieldid="ip" value={this.state.ip} />
                          <DisabledTextField label="MAC" fieldid="mac" value={this.state.mac} />
                          <DisabledTextField label="Security Method" fieldid="auth" value={this.state.auth} />
                          <DisabledTextField label="SSID" fieldid="remoteNetwork" value={this.state.remoteNetwork} />
                          <DisabledTextField label="Connection Count" fieldid="connectioncount" value={this.state.connectioncount} />
                          <DisabledTextField label="Connection Since" fieldid="since" value={this.state.since} />
                       </form>
                      </div>
                    </div>
                  </div>
                  )
                  }

{["unode-v5", "unode-v6", "cnext"].indexOf(this.state.model) >= 0 &&
                  (
                  <div className="panel panel-default" id="cellular">
                    <div className="panel-heading">
                      <h5 className="panel-title">
                        <a className="collapsed" data-toggle="collapse"  data-parent="#collapseTwonested" href="#collapseTwonested-collapseFour" onClick={this.handleConnectionStatus}>
                          Cellular
                        </a>
                      </h5>
                    </div>
                    <div id="collapseTwonested-collapseFour" className="panel-collapse collapse">
                      <div className="panel-body">
                        <form role="form" className="form-horizontal" data-nodeid={node.nodeid} >
                          <DisabledTextField label="IMEI" fieldid="imei" value={this.state.imei} />
                          <DisabledTextField label="IMSI" fieldid="imsi" value={this.state.imsi} />
                          <DisabledTextField label="APN" fieldid="apn" value={this.state.apn} />
                          <DisabledTextField label="IP" fieldid="ip" value={this.state.ip} />
                          <DisabledTextField label="ICCID" fieldid="iccid" value={this.state.iccid} />
                          <DisabledTextField label="Connection Count" fieldid="connectioncount" value={this.state.connectioncount} />
                          <DisabledTextField label="Connection Since" fieldid="since" value={this.state.since} />
                        </form>
                      </div>
                    </div>
                  </div>
                  )
                  }

{helpers.isInternalUser() && ["falcon-q", "merlin", "vdkmaster", "cnext"].indexOf(this.state.model) >= 0 &&
                  (
                  <div className="panel panel-default" id="vpn">
                    <div className="panel-heading">
                      <h5 className="panel-title">
                        <a className="collapsed" data-toggle="collapse"  data-parent="#collapseTwonested" href="#collapseTwonested-collapseFive" onClick={this.handleConnectionStatus}>
                          VPN
                        </a>
                      </h5>
                    </div>
                    <div id="collapseTwonested-collapseFive" className="panel-collapse collapse">
                      <div className="panel-body">
                        <form role="form" className="form-horizontal" data-nodeid={node.nodeid} >
                          <DisabledTextField label="VPN IP" fieldid="vpnip" value={this.state.vpnip} />
                          {this.state.vpnip != "" &&
                            (
                              <div>
                                <div className="form-group row">
                                  <div className="col-sm-6 text-center">
                                    <button type="button" className="ns-form-btn" id="startVPN" onClick={this.handleStartVPN}><b> Start VPN </b></button>
                                  </div>
                                  <div className="col-sm-6 text-center">
                                    <button type="button" className="ns-form-btn" id="stopVPN" onClick={this.handleStopVPN}><b> Stop VPN </b></button>
                                  </div>
                                </div>
                                <div style={{textAlign:"center"}}>
                                <a href={"https://" + this.state.vpnip + "/signin"} target="_blank">VPN Signin <img height="18" src="/imgs/navigation/NewWindow.png" /></a>
                                </div>
                              </div>
                            )
                          }
                        </form>
                      </div>
                    </div>
                  </div>
                  )
                  }

{helpers.modelType(this.state.model) == "Lighting" &&
                  (
                  <div className="panel panel-default">
                    <div className="panel-heading">
                      <h5 className="panel-title">
                        <a className="collapsed" id="fixtureLabel" data-toggle="collapse" data-parent="#collapseTwonested" href="#collapseTwonested-collapseSix" onClick={this.handleFixture}>
                          Fixture
                        </a>
                      </h5>
                    </div>
                    <div id="collapseTwonested-collapseSix" className="panel-collapse collapse">
                      <div className="panel-body">
                        <form role="form" className="form-horizontal" data-nodeid={node.nodeid} >
                          <div className="form-group">
                            <label htmlFor="" className="control-label col-sm-3" style={{ lineHeight: "18px", paddingTop: "4px" }}>Current:</label>
                            <div className="col-sm-6" id="currentFixture" style={{marginTop:"10px"}}>
                              {this.props.node.fixtureid ? this.props.node.fixturename : this.props.node.fixturename="Not Set"}
                            </div>
                          </div>
                          <div className="form-group">
                            <label htmlFor="changefixture" className="control-label col-sm-3">Change:</label>
                            <div className="col-sm-6">
                              <select className="form-control" id="changefixture" ref="changefixture" value={this.state.changefixture} onChange={this.handleChange('changefixture')} >
                                {fOptions}
                              </select>
                            </div>
                          </div>
                          <div className="text-right" style={setFixtureVisibility}>
                            <button type="button" className="ns-save-btn" id="setFixture" onClick={this.handleFixtureUpdate}>
                              <Icon glyph="icon-fontello-ok" /> <b> Set Fixture </b></button>
                          </div>
                        </form>
                      </div>
                    </div>
                  </div>
                  )
                  }

                  <NodeSensors type="diagnostic" nodeid={this.state.nodeid} model={helpers.modelInternalName(this.state.model)} />

                  <div className="panel panel-default">
                    <div className="panel-heading">
                      <h5 className="panel-title">
                        <a className="collapsed" data-toggle="collapse" id="locationLabel" data-parent="#collapseTwonested" href="#collapseTwonested-collapseSeven">
                          Location
                        </a>
                      </h5>
                    </div>
                    <div id="collapseTwonested-collapseSeven" className="panel-collapse collapse">
                      <div className="panel-body">
                        <form role="form" className="form-horizontal" data-nodeid={node.nodeid} >
                          <div className="form-group">
                            <label htmlFor="altitude" className="control-label col-sm-3">Altitude:</label>
                            <div className="col-sm-6">
                              <input type="text" className="form-control" id="altitude" ref="altitude" value={this.state.altitude} onChange={this.handleChange('altitude')} />
                            </div>
                          </div>
                          <div className="form-group">
                            <label htmlFor="level" className="control-label col-sm-3">Level:</label>
                            <div className="col-sm-6">
                              <input type="text" className="form-control" id="level" ref="level" value={this.state.level} onChange={this.handleChange('level')} />
                            </div>
                          </div>
                          <div className="form-group">
                            <label htmlFor="latitude" className="control-label col-sm-3">Latitude:</label>
                            <div className="col-sm-6">
                              <input type="text" className="form-control" id="latitude" ref="latitude" value={this.state.latitude} onChange={this.handleChange('latitude')} />
                            </div>
                          </div>
                          <div className="form-group">
                            <label htmlFor="longitude" className="control-label col-sm-3">Longitude:</label>
                            <div className="col-sm-6">
                              <input type="text" className="form-control" id="longitude" ref="longitude" value={this.state.longitude} onChange={this.handleChange('longitude')} />
                            </div>
                          </div>
                          <div style={visibilityGPS}>
                            <DisabledTextField label="GPS Latitude" cols={[3, 6]} fieldid="latitude_gps" value={this.state.latitude_gps} />
                            <DisabledTextField label="GPS Longitude" cols={[3, 6]} fieldid="longitude_gps" value={this.state.longitude_gps} />
                          </div>
                          <DisabledTextField label="Country Code" cols={[3, 6]} fieldid="country_code" value={this.state.country_code} />
                          <DisabledTextField label="Timezone" cols={[3, 6]} fieldid="time_zone" value={this.state.time_zone} />
                        </form>
                      </div>
                    </div>
                  </div>

                  <div className="panel panel-default">
                    <div className="panel-heading">
                      <h5 className="panel-title">
                        <a className="collapsed" data-toggle="collapse" id="hardwareLabel" data-parent="#collapseTwonested" href="#collapseTwonested-collapseEight">
                          Hardware
                        </a>
                      </h5>
                    </div>
                    <div id="collapseTwonested-collapseEight" className="panel-collapse collapse">
                      <div className="panel-body">
                        <form role="form" className="form-horizontal" data-nodeid={node.nodeid} >
                          <DisabledTextField cols={[3, 6]} label="Model" fieldid="model" style={{width:"190px"}} value={helpers.modelName(this.state.model)} />
                          <div style={subandVoltagetypeVisibility}>
                            <DisabledTextField cols={[3, 6]} label="Sub Type" fieldid="subType" value={subTypeStatus} />
                            <DisabledTextField cols={[3, 6]} label="Voltage Type" fieldid="voltageType" value={voltageTypeStatus} />
                          </div>
                          <DisabledTextField cols={[3, 6]} label="Version" fieldid="version" value={this.state.version} />
                        </form>
                      </div>
                    </div>
                  </div>

{helpers.modelType(this.state.model) == "Lighting"
                && (
                  <div className="panel panel-default">
                    <div className="panel-heading">
                      <h5 className="panel-title">
                        <a className="collapsed" data-toggle="collapse" id="lightingControlLabel" data-parent="#collapseTwonested" href="#collapseTwonested-collapseNine">
                          Lighting Control
                        </a>
                      </h5>
                    </div>
                    <div id="collapseTwonested-collapseNine" className="panel-collapse collapse">
                      <div className="panel-body">
                        <div className="form-group">
                          <div className="col-sm-7" style={{padding:"0px"}}>
                            <button style={{padding:"3px"}} className="ns-form-btn" id="pushSchedule" onClick={this.handlePushSchedule}> <b> Push Schedule to Node </b></button>
                          </div>
                          <div className="col-sm-5" style={{padding:"0px"}}>
                            <button style={{padding:"4px"}} className="ns-form-btn" id="viewNodeSchedule" onClick={this.handleSchedule}><b>View Schedule </b></button>
                          </div>
                        </div>
                        <div className="text-center">
                          <h4 style={{ marginTop: "48px" }}>Manual Override</h4>
                          <LightControl event="Nodeform.lightLevel" isDisabled={disabled} />
                        </div>
                      </div>
                    </div>
                  </div>
                )
}

                {auth.allowed('CAN_READ', 'FirmwareModel') 
                  && (
                    <div className="panel panel-default">
                      <div className="panel-heading">
                        <h5 className="panel-title">
                          <a className="collapsed" data-toggle="collapse" id="firmwareLabel" data-parent="#collapseTwonested" href="#firmwarePanel" onClick={this.handleFirmwareSection}>
                            Firmware
                          </a>
                        </h5>
                      </div>
                      <div id="firmwarePanel" className="panel-collapse collapse">
                        <div className="panel-body">
                          <NodeFirmware firmwares={this.props.firmwares} otas={this.props.otas} nodeinfo={this.state} />
                        </div>
                      </div>
                    </div>
                    )
                }


                  <div className="panel panel-default">
                    <div className="panel-heading">
                      <h5 className="panel-title">
                        <a className="collapsed" data-toggle="collapse" id="configManagementLabel" data-parent="#collapseTwonested" href="#collapseTwonested-collapseTen">
                          Configuration Management
                        </a>
                      </h5>
                    </div>
                    <div id="collapseTwonested-collapseTen" className="panel-collapse collapse">
                      <div className="panel-body">
                        <div id="collapseThreenested">
                          <div className="panel panel-default" id="assignedConfiguration">
                            <div className="panel-heading">
                              <h5 className="panel-title">
                                <a data-toggle="collapse" className="collapsed" data-parent="#collapseThreenested" href="#collapseTwonested-collapseTenNestedOne" onClick={this.handleNodeConfigSection} >
                                  Node Configuration
                                </a>
                              </h5>
                            </div>

                            <div id="collapseTwonested-collapseTenNestedOne" className="panel-collapse collapse">
                              <div className="panel-body" style={{padding:"0px"}}>
                                <Nodeconfigform singlenode={true} node={this.state} selected_nodes={[this.state.nodeid]} selected_nodesmodel={[this.state.model]} configs={this.props.configs} />
                              </div>
                            </div>
                          </div>

                          <div className="panel panel-default">
                            <div className="panel-heading">
                              <h5 className="panel-title">
                                <a data-toggle="collapse" className="collapsed" data-parent="#collapseThreenested" href="#collapseTwonested-collapseTenNestedTwo">
                                  Server
                              </a>
                              </h5>
                            </div>
                            <div id="collapseTwonested-collapseTenNestedTwo" className="panel-collapse collapse">
                              <div className="panel-body">
                                <form role="form" className="form-horizontal" data-nodeid={node.nodeid} >

                                  <div className="form-group">
                                    <label htmlFor="server" className="control-label col-sm-4">Server:</label>
                                    <div className="col-sm-6">
                                      <input type="text" disabled={disabled} className="form-control" id="server" ref="server" value={this.state.server} onChange={this.handleChange('server')} />
                                    </div>
                                  </div>
                                  <div style={visibility} className="text-right">
                                    <button type="button" className="ns-form-btn" id="applyServer" onClick={this.handleConfigServerUpdate}>
                                      <Icon glyph="icon-fontello-ok" /> <b>Apply </b></button>
                                  </div>
                                </form>
                              </div>
                            </div>
                          </div>
                          {((helpers.modelType(this.state.model) != "Lighting")||(this.state.model =="cnext"))
                          && (
                            <div className="panel panel-default">
                              <div className="panel-heading">
                                <h5 className="panel-title">
                                  <a data-toggle="collapse" style={{height:"51px"}} className="collapsed" data-parent="#collapseThreenested" href="#collapseTwonested-collapseTenNestedThree">
                                    Local, Factory & Provisional Configurations
                                </a>
                                <div style={{clear:"both"}}></div>
                                </h5>
                              </div>
                              <div id="collapseTwonested-collapseTenNestedThree" className="panel-collapse collapse">
                                <div className="panel-body">
                                  <form role="form" className="form-horizontal" data-nodeid={node.nodeid} >

                                    <div className="form-group">
                                      <label htmlFor="local" className="control-label col-sm-7" style={{lineHeight:"18px",margin:"0",textOverflow:"ellipsis",overflow:"hidden"}}>Local Configurations:</label>
                                      <div className="col-sm-3">
                                          <button className="ns-form-btn" id="viewLocalConfiguration" onClick={this.handleLocalConfigurations}><b>View</b></button>
                                      </div>
                                    </div>

                                    <div className="form-group">
                                      <label htmlFor="factory" className="control-label col-sm-7" style={{lineHeight:"18px",margin:"0",textOverflow:"ellipsis",overflow:"hidden"}}>Factory Configurations:</label>
                                      <div className="col-sm-3">
                                          <button className="ns-form-btn" id="viewFactoryConfiguration" onClick={this.handleFactoryConfigurations}><b>View</b></button>
                                      </div>
                                    </div>

                                    <div className="form-group">
                                      <label htmlFor="prov" className="control-label col-sm-7" style={{lineHeight:"18px",margin:"0",textOverflow:"ellipsis",overflow:"hidden"}}>Provisional Configurations:</label>
                                      <div className="col-sm-3">
                                          <button className="ns-form-btn" id="viewProvisionalConfig" onClick={this.handleProvConfigurations}><b>View</b></button>
                                      </div>
                                    </div>

                                  </form>
                                </div>
                              </div>
                            </div>
                            )
                          }

                        </div>
                      </div>
                    </div>
                  </div>


                      <div className="panel panel-default">
                        {resetContainerHtml}
                      </div>

                </div>
              </div>
            </div>
          </div>

          <div className="panel panel-default">
            <div className="panel-heading">
              <h4 className="panel-title">
                <a className="collapsed" data-toggle="collapse" id="reports" data-parent="#accordion" href="#collapseThree">
                  Reports
                </a>
              </h4>
            </div>
            <NodeSensors nodeid={this.state.nodeid} model={helpers.modelInternalName(this.state.model)} />
          </div>

          <div className="panel panel-default">
            <div className="panel-heading">
              <h4 className="panel-title">
                <a className="collapsed" data-toggle="collapse" id="alertsAndAlarms" data-parent="#accordion" href="#collapseFour">
                  Alerts & Alarms
                </a>
              </h4>
            </div>
            <div id="collapseFour" className="panel-collapse collapse">
              <div className="panel-body">
                <div id="collapseFournested">
                  <div className="panel panel-default">
                    <div className="panel-heading">
                      <h5 className="panel-title">
                        <a className="collapsed" data-toggle="collapse" data-parent="#collapseFournested" href="#collapseFournested-collapseOne">
                          Active Alerts
                        </a>
                      </h5>
                    </div>
                    <div id="collapseFournested-collapseOne" className="panel-collapse collapse">
                      <div className="panel-body">
                        {NodeAlerttablerows}
                      </div>
                    </div>
                  </div>
                  <div className="panel panel-default">
                    <div className="panel-heading">
                      <h5 className="panel-title">
                        <a className="collapsed" data-toggle="collapse" data-parent="#collapseFournested" href="#collapseFournested-collapseTwo">
                          Acknowledged Alerts
                        </a>
                      </h5>
                    </div>
                    <div id="collapseFournested-collapseTwo" className="panel-collapse collapse">
                      <div className="panel-body">
                        {NodeDismissedAlerttablerows}
                      </div>
                    </div>
                  </div>
                </div>
              </div>
            </div>
          </div>
        </div>
        </div>
        </div>
          {nodeSaveDeleteHtml}
      </div>
    );

  }
});

module.exports = withRouter(Nodeform);
