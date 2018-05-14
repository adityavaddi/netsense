import classNames from 'classnames';
import DisabledTextField from 'components/disabledtextfield'
import sensors from 'global/utils/sensors';

import { State, Navigation } from 'react-router';

var NodeSensors = React.createClass({
  getInitialState: function(){
    return {diagnostic:{},
            network:{},
            ambient:{},
            energy:{}
          };
  },

  propTypes: {
    nodeid: React.PropTypes.string.isRequired,
    model: React.PropTypes.string.isRequired,
    type: React.PropTypes.string,
    cols: React.PropTypes.array
  },

  getDefaultProps: function() {
    return {cols: [3, 6],
            type: 'reports'
            }
  },

  exist: function (data) {
    if (data !== null && data !== undefined) {
        return true;
    } else {
        return false;
    }
  },

  getConnStatus: function(callback) {
    $.ajax({
        url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/nodes/' + this.props.nodeid + '/connection_status',
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
  },

  init: function(nodeid){
    if (this.props.nodeid != nodeid) {
      this.props.nodeid = nodeid;
    }
    if (this.props.type == "diagnostic") {
      this.handleSensorsDiagnostic();
    } else {
      this.handleSensorsNetwork();
      this.handleSensorsAmbient();
      this.handleSensorsEnergy();
    };
  },
  
  boot_reasons_low: function(code) {
    return {
      '1': 'Brownout on Vbat power source',
      '2': 'Brownout on V12 power source',
      '4': 'Brownout on V18 power source',
      '8': 'Software requested system reset',
      '16': 'CPU locked up',
      '32': 'Watchdog timeout',
      '64': 'Brownout on Vfl power source'
    }[code] || 'Unknown Reason';
  },

  boot_reasons_app: function(code) {
    return {
      '1': 'COMMAND_REBOOT',
      '2': 'COMMAND_WDTTOUT',
      '3': 'SW_WDTTOUT',
      '4': 'BOOT_FAILURE',
      '16': 'NETWORK_WD_TIMEOUT',
      '17': 'OTA_COMMAND_RESET',
      '18': 'OTA_COMMAND_PARTITION',
      '19': 'OTA_FW_UPDATE',
      '20': 'OTACFG_OUT_OF_MEM',
      '21': 'X509_UPDATE',
      '32': 'CS5480_POLL',
      '33': 'CS5480_POLL_REG_RD_1',
      '34': 'CS5480_POLL_REG_RD_2',
      '35': 'CS5480_POLL_REG_RD_3',
      '36': 'CS5480_POLL_REG_RD_4',
      '37': 'CS5480_POLL_REG_RD_5',
      '41': 'CS5480_POLL_REG_WR'
    }[code] || 'Unknown Reason';
  },

  handleSensorsDiagnostic: function (e) {
    if (typeof e != "undefined") {
      e.stopPropagation();
      e.preventDefault();
    };
    if ($("#sensors-diagnostic-heading a").length > 0 
        && !$("#sensors-diagnostic-heading a").hasClass('collapsed')) {
      var now = new Date(),
        yesterday = new Date(now.getTime() - (24 * 60 * 60 * 1000));

      var that = this;
      var nodeID = this.props.nodeid;

      var requests = [];
      var responses = {};
      for (var i = 0; i<this.sensorsList.length; i++) {
        if (this.sensorsList[i].category == 'Diagnostic' 
            && this.sensorsList[i].model.indexOf(this.props.model) >= 0) {
          var sensorid = this.sensorsList[i].id;
          requests.push($.ajax({
              url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/nodes/' + nodeID + '/sensors/' + sensorid + '/date/' + yesterday.toISOString() + '/limit/1',
              data: '',
              method: 'GET',
              xhrFields: {
                withCredentials: true
              },
              dataType: 'json',
              success: function (data) {
                if (data.datapoints[0]) {
                  responses[data.sensorid] = data.datapoints[0].value + that.formats[data.sensorid].units;
                } else {
                  responses[data.sensorid] = "no value";
                }
              },
              error: function () {
                //responses[sensorid] = "undefined";
              }
            })
          );
          }
        };
      $.when.apply(undefined, requests).then(function(results) {
        that.setState({diagnostic:responses});
      });
    }
  },

  handleSensorsNetwork: function (e) {
    if (typeof e != "undefined") {
      e.stopPropagation();
      e.preventDefault();
    };
    if ($("#sensors-network-heading a").length > 0 
        && !$("#sensors-network-heading a").hasClass('collapsed')) {
      var now = new Date(),
        yesterday = new Date(now.getTime() - (24 * 60 * 60 * 1000));

      var that = this;
      var nodeID = this.props.nodeid;

      var requests = [];
      var responses = {};
      for (var i = 0; i<this.sensorsList.length; i++) {
        if (this.sensorsList[i].category == 'Network' 
            && this.sensorsList[i].model.indexOf(this.props.model) >= 0) {
          var sensorid = this.sensorsList[i].id;
          requests.push($.ajax({
              url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/nodes/' + nodeID + '/sensors/' + sensorid + '/date/' + yesterday.toISOString() + '/limit/1',
              data: '',
              method: 'GET',
              xhrFields: {
                withCredentials: true
              },
              dataType: 'json',
              success: function (data) {
                if (data.datapoints[0]) {
                  responses[data.sensorid] = data.datapoints[0].value + that.formats[data.sensorid].units;
                } else {
                  responses[data.sensorid] = "no value";
                }
              },
              error: function () {
                //responses[sensorid] = "undefined";
              }
            })
          );
        };
      };
      requests.push($.ajax({
          url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/nodes/' + this.props.nodeid + '/connection_status',
          data: '',
          method: 'GET',
          xhrFields: {
              withCredentials: true
          },
          dataType: 'json',
          success: function (data) {
            responses.connected = data.isconnected;
            responses.connectedsince = moment(data.since).format('MM/DD/YY hh:mm:ss Z');
            responses.connectedsincelabel = responses.connected?'Connected Since':"Disconnected Since";
          },
          error: function () {
            responses.connected = "no value";
            responses.connectedsince = "no value";
            responses.connectedsincelabel = "Disconnected Since";
          }
        })
      );
      $.when.apply(undefined, requests).then(function(results) {
        that.setState({network:responses});
      });
    }
  },

  handleSensorsAmbient: function (e) {
    if (typeof e != "undefined") {
      e.stopPropagation();
      e.preventDefault();
    };
    if ($("#sensors-ambient-heading a").length > 0 
        && !$("#sensors-ambient-heading a").hasClass('collapsed')) {
      var now = new Date(),
        yesterday = new Date(now.getTime() - (24 * 60 * 60 * 1000));

      var that = this;
      var nodeID = this.props.nodeid;

      var requests = [];
      var responses = {};
      for (var i = 0; i<this.sensorsList.length; i++) {
        if (this.sensorsList[i].category == 'Ambient' 
            && this.sensorsList[i].model.indexOf(this.props.model) >= 0) {
          var sensorid = this.sensorsList[i].id;
          requests.push($.ajax({
              url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/nodes/' + nodeID + '/sensors/' + sensorid + '/date/' + yesterday.toISOString() + '/limit/1',
              data: '',
              method: 'GET',
              xhrFields: {
                withCredentials: true
              },
              dataType: 'json',
              success: function (data) {
                if (data.datapoints[0]) {
                  responses[data.sensorid] = data.datapoints[0].value + that.formats[data.sensorid].units;
                } else {
                  responses[data.sensorid] = "no value";
                }
              },
              error: function () {
                //responses[sensorid] = "undefined";
              }
            })
          );
          }
        };
      $.when.apply(undefined, requests).then(function(results) {
        that.setState({ambient:responses});
      });
    }
  },

  handleSensorsEnergy: function (e) {
    if (typeof e != "undefined") {
      e.stopPropagation();
      e.preventDefault();
    };
    if ($("#sensors-energy-heading a").length > 0 
        && !$("#sensors-energy-heading a").hasClass('collapsed')) {
      var now = new Date(),
        yesterday = new Date(now.getTime() - (24 * 60 * 60 * 1000));

      var that = this;
      var nodeID = this.props.nodeid;

      var requests = [];
      var responses = {};
      for (var i = 0; i<this.sensorsList.length; i++) {
        if (this.sensorsList[i].category == 'Energy' 
            && this.sensorsList[i].model.indexOf(this.props.model) >= 0) {
          var sensorid = this.sensorsList[i].id;
          requests.push($.ajax({
              url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/nodes/' + nodeID + '/sensors/' + sensorid + '/date/' + yesterday.toISOString() + '/limit/1',
              data: '',
              method: 'GET',
              xhrFields: {
                withCredentials: true
              },
              dataType: 'json',
              success: function (data) {
                if (data.datapoints[0]) {
                  responses[data.sensorid] = data.datapoints[0].value + that.formats[data.sensorid].units;
                } else {
                  responses[data.sensorid] = "no value";
                }
              },
              error: function () {
                //responses[sensorid] = "undefined";
              }
            })
          );
          }
        };
      $.when.apply(undefined, requests).then(function(results) {
        that.setState({energy:responses});
      });
    }
  },

  componentWillReceiveProps: function(nextprops) {
    if (this.props.nodeid != nextprops.nodeid) {
      this.init(nextprops.nodeid);
    }
  },

  componentDidMount() {
    this.sensorsList = sensors.getSensorsList();
    this.formats = {};
    for (var i=0; i<this.sensorsList.length; i++) {
      this.formats[this.sensorsList[i].id] = {
        name: this.sensorsList[i].name,
        units: this.sensorsList[i].units,
        order: this.sensorsList[i].order
      };
    };
  },

  render() {   
    var that = this;
    if (this.props.type == "diagnostic") {
      if (NSN && NSN.userInfo && NSN.userInfo.authorization[0].type == "sensity_user") {
      return (
        <div className="panel panel-default" id="diagnosticwrapper">
          <div className="panel-heading" id="sensors-diagnostic-heading">
            <h5 className="panel-title">
              <a className="collapsed" data-toggle="collapse" data-parent="#collapseTwonested" href="#collapseTwonested-diagnostic" onClick={this.handleSensorsDiagnostic}>
                Diagnostic
              </a>
            </h5>
          </div>
          <div id="collapseTwonested-diagnostic" className="panel-collapse collapse">
            <div className="panel-body">
              <form role="form" className="form-horizontal" data-nodeid={this.props.nodeid} >
                {Object.keys(this.state.diagnostic)
                  .sort(function(a,b){return that.formats[a].order - that.formats[b].order})
                  .map(function(id, index) {
                  return (<DisabledTextField key={index} label={that.formats[id].name} fieldid={id} value={that.state.diagnostic[id]} />)
                })}
              </form>
            </div>
          </div>
        </div>
        )
      } else {
        return (<div style={{height:"4px"}} />);
      }
    };

    return (
      <div id="collapseThree" className="panel-collapse collapse">
        <div className="panel-body">
          <div id="collapseThreenested">
            <div className="panel panel-default">
              <div className="panel-heading" id="sensors-network-heading">
                <h5 className="panel-title">
                  <a className="collapsed" data-toggle="collapse" data-parent="#collapseThreenested" href="#collapseThreenested-collapseOne" onClick={this.handleSensorsNetwork} >
                    Network & Connection
                  </a>
                </h5>
              </div>
              <div id="collapseThreenested-collapseOne" className="panel-collapse collapse">
                <div className="panel-body">
                  <form role="form" className="form-horizontal" data-nodeid={this.props.nodeid} >
                  <DisabledTextField label="Connected" fieldid="connected" value={that.state.network["connected"]} />
                  <DisabledTextField label={that.state.network["connectedsincelabel"]} fieldid="connectedsince" value={that.state.network["connectedsince"]} />
                    {Object.keys(this.state.network).filter(function(id) {
                      return (id != "connected" && id !="connectedsince" && id != "connectedsincelabel");
                    }).sort(function(a,b){return that.formats[a].order - that.formats[b].order})
                      .map(function(id, index) {
                      return (<DisabledTextField key={index} label={that.formats[id].name} fieldid={id} value={that.state.network[id]} />)
                    })}
                  </form>
                </div>
              </div>
            </div>

            <div className="panel panel-default">
              <div className="panel-heading" id="sensors-ambient-heading">
                <h5 className="panel-title">
                  <a className="collapsed" data-toggle="collapse" data-parent="#collapseThreenested" href="#collapseThreenested-collapseTwo" onClick={this.handleSensorsAmbient} >
                    Ambient Sensors
                  </a>
                </h5>
              </div>
              <div id="collapseThreenested-collapseTwo" className="panel-collapse collapse">
                <div className="panel-body">
                  <form role="form" className="form-horizontal" data-nodeid={this.props.nodeid} >
                    {Object.keys(this.state.ambient)
                      .sort(function(a,b){return that.formats[a].order - that.formats[b].order})
                      .map(function(id, index) {
                      return (<DisabledTextField key={index} label={that.formats[id].name} fieldid={id} value={that.state.ambient[id]} />)
                    })}
                  </form>
                </div>
              </div>
            </div>

            <div className="panel panel-default">
              <div className="panel-heading" id="sensors-energy-heading">
                <h5 className="panel-title">
                  <a className="collapsed" data-toggle="collapse" data-parent="#collapseThreenested" href="#collapseThreenested-collapseThree" onClick={this.handleSensorsEnergy} >
                    Energy Sensors
                  </a>
                </h5>
              </div>
              <div id="collapseThreenested-collapseThree" className="panel-collapse collapse">
                <div className="panel-body">
                  <form role="form" className="form-horizontal" data-nodeid={this.props.nodeid} >
                    {Object.keys(this.state.energy)
                      .sort(function(a,b){return that.formats[a].order - that.formats[b].order})
                      .map(function(id, index) {
                      return (<DisabledTextField key={index} label={that.formats[id].name} fieldid={id} value={that.state.energy[id]} />)
                    })}
                  </form>
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>
    );     
  }
});

module.exports = NodeSensors;

