import classNames from 'classnames';
import auth from 'global/utils/auth';
import helpers from 'global/utils/helpers';
import Listeditor  from 'components/listeditor';

import { State, Navigation } from 'react-router';
import DataGrid from 'components/datagrid';

var Nodelist = React.createClass({

  getInitialState: function(){
    var userRole = NSN.userInfo.name;
    var componentColumns = "NodeColumnWidths_"+ userRole;
    var nodeStoredWidths = [];
    var height = "430px";
    var overflowY = "scroll";

// temporarily ignore column defs in local storage until we can migrate old values 
    if (localStorage.getItem(componentColumns)){  // (localStorage.getItem(componentColumns)){
      nodeStoredWidths = JSON.parse(localStorage.getItem(componentColumns));
    }else{
      // default widths
      nodeStoredWidths = [
        {name: "L", field: "lig_stats", id: "lig_stats", sortable: true, maxWidth: 36, headerCssClass: "text-center gridHeader"
          ,checked:true,required:true},
        {name: "N", field: "net_stats", id: "net_stats",sortable: true, maxWidth: 36, headerCssClass: "text-center gridHeader"
          ,checked:true,required:true},
        {name: "S", field: "sen_stats", id: "sen_stats",sortable: true, maxWidth: 36, headerCssClass: "text-center gridHeader"
          ,checked:true,required:true},
        { name: "ID", field: "nodeid", id: "nodeid", sortable: true, cssClass: "text-center", headerCssClass: "gridHeader",
          minWidth: 120, width: 120,checked:true,required:true},
        {name: "Model", field: "model", id: "model",sortable: true, cssClass: "text-center", headerCssClass: "gridHeader",
          minwidth: 120, width: 120,checked:true,required:false},
        {name: "Type", field: "type", id: "type",sortable:true, cssClass: "text-center", headerCssClass: "gridHeader",
          minwidth: 120, width: 120, checked:true,required:false},
        { name: "Level", field: "level", id: "level",checked:true,required:true,
          sortable: true, cssClass: "text-center", headerCssClass: "gridHeader", width: 60},
        { name: "Name", field: "name", id: "name", headerCssClass: "gridHeader",
          sortable: true, width: 120,checked:true,required:false},
        { name: "Remote Network", field: "remoteNetwork", id: "remoteNetwork", headerCssClass: "gridHeader",
          sortable: true, width: 100,checked:true,required:true},
        { name: "APN", field: "apn", id: "apn", headerCssClass: "gridHeader",
          sortable: true, width: 100,checked:true,required:false},
        { name: "BSSID", field: "bssid", id: "bssid", headerCssClass: "gridHeader",
          sortable: true, width: 100,checked:true,required:false},
        { name: "IP Address", field: "ip", id: "ip", headerCssClass: "gridHeader",
          sortable: true, width: 100, maxWidth: 140,checked:true,required:false},
        { name: "Schedule", field: "schedulename", id: "schedulename", headerCssClass: "gridHeader",
          sortable: true, width: 120,checked:true,required:false},
        { name: "Fixture", field: "fixturename", id: "fixturename", headerCssClass: "gridHeader",
          sortable: true, width: 100,checked:true,required:false},
        {name: "Firmware", field: "softwareVersion", id: "softwareVersion", headerCssClass: "gridHeader",
          sortable: true, width: 100,checked:true,required:true},
        {name: "Latitude", field: "latitude", id: "latitude", headerCssClass: "gridHeader",
          sortable: true, width: 100,checked:true,required:true},
        {name: "Longitude", field: "longitude", id: "longitude", headerCssClass: "gridHeader",
          sortable: true, width: 100,checked:true,required:true},
        { name: "Groups", field: "groupnamelist", id: "groupnamelist", headerCssClass: "gridHeader",
          sortable: true, width: 200,checked:true,required:true}
      ]
    }
    return {
      showListEditor:false,
      nodeStoredWidths:nodeStoredWidths,
      componentColumns:componentColumns,
      component:"Node",
      height: height,
      overflowY:overflowY
    }
  },

  propTypes: {
    nodes: React.PropTypes.array.isRequired,
    alerts: React.PropTypes.array.isRequired,
    selected_nodes: React.PropTypes.array.isRequired,
    minmax: React.PropTypes.string,
    detail_state: React.PropTypes.string
  },

  maximize: function () {
    if (this.props.ui_state.detail == "pinned") {
        ReactBootstrap.Dispatcher.emit('Nodedetail.togglePin');
        }
    $("#node-list-panel").data("state", "open").css({ width: "100%", zIndex: "401" });
    $("#node-table-wide").css({ width: "98%" });
    $(window).trigger('resize');
  },

  minimize: function () {
    $("#node-list-panel").data("state", "closed").css({ width: "33%",zIndex: "300" });
    $("#node-table-wide").css({ width: "1600px" });
  },

  togglegrid: function () {
    if ($("#node-list-panel").data("state") == "closed") {
      this.maximize();
    } else {
      this.minimize();
    }
  },

      toggleDetail: function () {
        if ($("#node-list-panel").data("state") == "open") {
            this.minimize();
            ReactBootstrap.Dispatcher.emit("Nodelist.toggleDetail");
        } else {
            ReactBootstrap.Dispatcher.emit("Nodelist.toggleDetail");
        }
    },

  handleAdd: function () {
    ReactBootstrap.Dispatcher.emit("Nodelist.add");
  },

  updateSummary: function (type) {
    var res = [0, 0, 0, 0];
    switch (type) {
      case "network":
        for (var i = 0; i < this.props.nodes.length; i++) {
          res[this.props.nodes[i].net_stats]++;
        };
        $("#nodesummary tr:nth-child(2) td:nth-child(2) span:first").html(res[0]);
        $("#nodesummary tr:nth-child(2) td:nth-child(3) span:first").html(res[1]);
        $("#nodesummary tr:nth-child(2) td:nth-child(4) span:first").html(res[2]);
        $("#nodesummary tr:nth-child(2) td:nth-child(5) span:first").html(res[3]);
      // no break statement here because we need to update lighting also
      case "light":
        for (var i = 0; i < this.props.nodes.length; i++) {
          res[this.props.nodes[i].lig_stats]++;
        };
        $("#nodesummary tr:nth-child(1) td:nth-child(2) span:first").html(res[0]);
        $("#nodesummary tr:nth-child(1) td:nth-child(3) span:first").html(res[1]);
        $("#nodesummary tr:nth-child(1) td:nth-child(4) span:first").html(res[2]);
        $("#nodesummary tr:nth-child(1) td:nth-child(5) span:first").html(res[3]);
        break;
      case "sensor":
        for (var i = 0; i < this.props.nodes.length; i++) {
          res[this.props.nodes[i].sen_stats]++;
        };
        $("#nodesummary tr:nth-child(3) td:nth-child(2) span:first").html(res[0]);
        $("#nodesummary tr:nth-child(3) td:nth-child(3) span:first").html(res[1]);
        $("#nodesummary tr:nth-child(3) td:nth-child(4) span:first").html(res[2]);
        $("#nodesummary tr:nth-child(3) td:nth-child(5) span:first").html(res[3]);
        break;
    }
  },

  toggleListEditor : function(){
    this.setState({showListEditor:!this.state.showListEditor});
  },

  shouldComponentUpdate: function (nextProps, nextState) {
    if(this.state.showListEditor === false || this.state.showListEditor === true){
      return true;
    }
    // This component does not need to be rerendered unless nodes have been added
    return (this.props.nodes !== nextProps.nodes)
        || (this.props.selected_nodes !== nextProps.selected_nodes)
        || (this.props.alerts !== nextProps.alerts);
  },

  componentDidMount: function () {
    var that = this;

    ReactBootstrap.Dispatcher.on('Nodemap.Alert', function (nodeid, alert) {
      for (var i = 0; i < that.props.nodes.length; i++) {
        if (nodeid === that.props.nodes[i].nodeid) {
          node = that.props.nodes[i];
        };
      };
      if (alert.severity === "Clear") {
        if (node.alerts) {
          for (var j = 0; j < node.alerts.length; j++) {
            if (alert.type === node.alerts[j].type)
              node.alerts.splice(j, 1);
          }
        }
      }
      if (alert.category === "Connection") {
        node.alerts.push(alert);
        that.updateSummary("network");
      }
      if (alert.category === "Sensor") {
        node.alerts.push(alert);
        that.updateSummary("sensor");
      }
      if (alert.category === "Light") {
        node.alerts.push(alert);
        that.updateSummary("light");
      }
      node = helpers.setNodeStatus(node);
      ReactBootstrap.Dispatcher.emit("Nodeform.update.success", node);
    });

    ReactBootstrap.Dispatcher.on('Nodemap.Status', function (nodeid, status) {
      for (var i = 0; i < that.props.nodes.length; i++) {
        if (nodeid === that.props.nodes[i].nodeid) {
          var node = that.props.nodes[i];
          if (typeof status.Connection != "undefined") {
            that.props.nodes[i].net_stats = status.Connection?0:2;
            that.updateSummary("network");
          };
          if (typeof status.Light != "undefined") {
            that.props.nodes[i].lig_stats = (status.Light !== null && status.Light != 0)?0:2;
          };
          if (helpers.modelType(that.props.nodes[i].model) != "Lighting") {
            that.props.nodes[i].lig_stats = that.props.nodes[i].net_stats;
          };
          that.updateSummary("light");
          if (typeof status.Sensor != "undefined") {
            that.props.nodes[i].sen_stats = (status.Sensor !== null && status.Light != 0)?0:2;
            that.updateSummary("sensor");
          };
          node = that.props.nodes[i];
        };
      };
      ReactBootstrap.Dispatcher.emit("Nodeform.update.success", node);
    });

    ReactBootstrap.Dispatcher.on('Nodemap.UpdateNodeStatus', function () {
      console.log("in UpdateNodeStatus");
      $.ajax({
        url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/node_status'
        + '?t=' + (new Date()).getTime(),
        data: '',
        method: 'GET',
        xhrFields: {
          withCredentials: true
        },
        dataType: 'json',
        success: function (data) {
          var updates = [];
          for (var i = 0; i < data.length; i++) {
            var nodeid = data[i].nodeid;
            for (var j = 0; j < that.props.nodes.length; j++) {
              if (nodeid == that.props.nodes[j].nodeid) {
                if (data[i].lig_stat != that.props.nodes[j].lig_stat
                  || data[i].net_stat != that.props.nodes[j].net_stat
                  || data[i].sen_stat != that.props.nodes[j].sen_stat
                  || !helpers.compareArrays(data[i].alerts, that.props.nodes[j].alerts)) {
                  that.props.nodes[j].lig_stat = data[i].lig_stat;
                  that.props.nodes[j].net_stat = data[i].net_stat;
                  that.props.nodes[j].sen_stat = data[i].sen_stat;
                  that.props.nodes[j].alerts = $.extend(true, [], data[i].alerts);

                  var node = that.props.nodes[j];


                  // apply business logic
                  node = helpers.setNodeStatus(node);

                  that.props.nodes[j] = node;
                  updates.push(that.props.nodes[j]);
                }
              }
            }
          }
          if (updates.length > 0) {
            ReactBootstrap.Dispatcher.emit("Nodeform.update.success", updates);
            ReactBootstrap.Dispatcher.emit("Nodelist.nodeStatusUpdated", updates);
          }
        },
        error: function () {
          console.log("node_status API failed");
        }
      });

    })

  },

  componentWillUnmount: function () {
    ReactBootstrap.Dispatcher.removeAllListeners("Nodemap.Status");
    ReactBootstrap.Dispatcher.removeAllListeners("Nodemap.Alert");
    ReactBootstrap.Dispatcher.removeAllListeners("Nodemap.UpdateNodeStatus");
  },

  render() {

    var Addbutton = (<span></span>);
    if (auth.allowed('CAN_CREATE', 'NodeModel')) {
      Addbutton = (
        <button id="add-node" onClick={this.handleAdd} className="ns-med-btn" title="Add Node">
          <b>Add node</b></button>);
    };
    var lig_sum = [0, 0, 0, 0], net_sum = [0, 0, 0, 0], sen_sum = [0, 0, 0, 0];
    for (var i = 0; i < this.props.nodes.length; i++) {
      var node = this.props.nodes[i];
      lig_sum[node.lig_stats]++;
      net_sum[node.net_stats]++;
      sen_sum[node.sen_stats]++;
    };

    var Minmaxlist = (<div></div>);
    if (this.props.minmax) {
      var glyph = (this.props.minmax === "expand") ? "icon-fontello-step-backward" : "icon-fontello-resize-horizontal";
      var handler = this.togglegrid;
      Minmaxlist = (
        <div style={{ position: "absolute", cursor: "pointer", top: "-5px", right: "33px", height: "20px", width: "20px", fontSize: "28px" }}
          onClick={handler} title="Toggle Full-Screen Table">
          <Icon bundle={this.props.bundle} glyph={glyph} />
        </div>);
    };

    return (
      <div style={{ height: "100%" }}>
        {Minmaxlist}
        <h2 className="netsense__map__table__title">Nodes
            <span onClick={()=>this.toggleListEditor()}
                  className="ns-map-filter-icon" style={{top:"8px !important"}}></span>
        </h2>
        <div id="node-table-container">
          <table ref="nodesummary" id="nodesummary" cellpadding="12" style={{ cellPadding: "10px", width: "92%", maxWidth: "580px", margin: "16px 0px 0px 30px" }}>
            <tr>
              <td style={{ fontWeight: "bold", width: "24%", position: "relative", right: "2px" }}><img src="/imgs/new-icons/lighting-icon.svg" height="20" width="22" /> Lighting</td>
              <td style={{ textAlign: "right" }}><span>{lig_sum[0]}</span> Working <span style={{ color: "#ccc" }}> | </span></td>
              <td style={{ textAlign: "right" }}><span>{lig_sum[1]}</span> Warn <span style={{ color: "#ccc" }}> | </span></td>
              <td style={{ textAlign: "right" }}><span>{lig_sum[2]}</span> Error <span style={{ color: "#ccc" }}> | </span></td>
              <td title="Disconnected Nodes" style={{ textAlign: "right" }}><span>{lig_sum[3]}</span> Disc.</td>
            </tr>
            <tr>
              <td style={{ fontWeight: "bold", paddingTop: "2px" }}><img src="/imgs/new-icons/network-icon.svg" height="19" width="19" /> Network</td>
              <td style={{ textAlign: "right" }}><span>{net_sum[0]}</span> Working <span style={{ color: "#ccc" }}> | </span></td>
              <td style={{ textAlign: "right" }}><span>{net_sum[1]}</span> Warn <span style={{ color: "#ccc" }}> | </span></td>
              <td style={{ textAlign: "right" }}><span>{net_sum[2]}</span> Error <span style={{ color: "#ccc" }}> | </span></td>
              <td title="Disconnected Nodes" style={{ textAlign: "right" }}><span>{net_sum[3]}</span> Disc.</td>
            </tr>
            <tr>
              <td style={{ fontWeight: "bold" }}><img src="/imgs/new-icons/sensor-icon.svg" height="19" width="19" /> Sensors</td>
              <td style={{ textAlign: "right" }}><span>{sen_sum[0]}</span> Working <span style={{ color: "#ccc" }}> | </span></td>
              <td style={{ textAlign: "right" }}><span>{sen_sum[1]}</span> Warn <span style={{ color: "#ccc" }}> | </span></td>
              <td style={{ textAlign: "right" }}><span>{sen_sum[2]}</span> Error <span style={{ color: "#ccc" }}> | </span></td>
              <td title="Disconnected Nodes" style={{ textAlign: "right" }}><span>{sen_sum[3]}</span> Disc.</td>
            </tr>
          </table>


          {
            this.state.showListEditor ?
                <div className="ns-list-editor" style={{top:"68px !important"}}>
                  <Listeditor show={this.state.showListEditor}
                              component={this.state.component}
                              componentColumns ={this.state.componentColumns}
                              handleToggle = {this.toggleListEditor}
                              height={this.state.height}
                              overflowY={this.state.overflowY}
                              columns={
                                this.state.nodeStoredWidths.map(function(column, index){
                                  if(column.name === "L"){
                                    return(
                                    {name:column.name,field:column.field,id: column.id, checked: column.checked, required: column.required,width: column.width,
                                      sortable:column.sortable, disableSearch: true, headerCssClass: "text-center gridHeader",maxWidth: 36,
                                      formatter: function (row, cell, value, columnDef, dataContext) {
                                        var status = ["Working", "Warn", "Error", "NA"];
                                        value = status[value];
                                        return '<img height="20" width="20" title="" alt="'
                                            + value
                                            + '" src="/imgs/Lighting_'
                                            + value
                                            + '.svg" />'
                                      }
                                    }
                                    )
                                  }else if(column.name === "N"){
                                    return(
                                    {
                                      name:column.name,field:column.field, id: column.id,checked: column.checked,required: column.required,maxWidth: 36,
                                      width: column.width,disableSearch: true, sortable:column.sortable,headerCssClass: "text-center gridHeader",
                                      formatter: function (row, cell, value) {
                                        var status = ["Working", "Warn", "Error", "NA"];
                                        value = status[value];
                                        return '<img height="20" width="20" title="" alt="'
                                            + value
                                            + '" src="/imgs/Network_'
                                            + value
                                            + '.svg" />'
                                      }
                                    }
                                    )
                                  }else if(column.name === "S"){
                                    return(
                                    {
                                      name:column.name,field:column.field, id: column.id,checked: column.checked,required: column.required,maxWidth: 36,
                                      width: column.width,disableSearch: true, sortable:column.sortable,headerCssClass: "text-center gridHeader",
                                      formatter: function (row, cell, value) {
                                        var status = ["Working", "Warn", "Error", "NA"];
                                        value = status[value];
                                        return '<img height="20" width="20" title="" alt="'
                                            + value
                                            + '" src="/imgs/Sensor_'
                                            + value
                                            + '.svg" />'
                                      }
                                    }
                                    )
                                  }else if(column.name === "Latitude"){
                                    return(
                                    {
                                      name:column.name,field:column.field, id: column.id,checked: column.checked,required: column.required,
                                      width: column.width, sortable:column.sortable,headerCssClass: "gridHeader",
                                      formatter: function (row, cell, value) {
                                        if (typeof value == "undefined"
                                            || value == ""
                                            || !$.isNumeric(value)
                                            || value < -90
                                            || value > 90) {
                                          return '<span style="color:red">' + value + '</span>'
                                        } else {
                                          return value;
                                        }
                                      }
                                    }
                                    )
                                  }else if(column.name === "Longitude"){
                                    return(
                                    {
                                      name:column.name,field:column.field, id: column.id,checked: column.checked,required: column.required,
                                      width: column.width, sortable:column.sortable,headerCssClass: "gridHeader",
                                      formatter: function (row, cell, value) {
                                        if (typeof value == "undefined"
                                            || value == ""
                                            || !$.isNumeric(value)
                                            || value < -180
                                            || value > 180) {
                                          return '<span style="color:red">' + value + '</span>'
                                        } else {
                                          return value;
                                        }
                                      }
                                    }
                                    )
                                  }else if(column.name === "Groups"){
                                    return(
                                    {
                                      name:column.name,field:column.field, id: column.id,checked: column.checked,required: column.required,
                                      width: column.width, sortable:column.sortable,headerCssClass: "gridHeader",
                                      formatter: function (row, cell, value) {
                                        return JSON.stringify(value).replace(/[\[\]\"]/g, '');
                                      }
                                    }
                                    )
                                  }else{
                                    return({
                                      name:column.name,field:column.field, id: column.id,checked: column.checked,required: column.required,
                                      width: column.width, sortable:column.sortable,headerCssClass: "gridHeader"
                                    })
                                  }
                                })
                              }/>
                </div>
                :null
          }



          <div id="node-table-wide" style={{ width: "1600px", overflow: "hidden", marginTop: "24px" }}>
            <DataGrid component="Node"
                      dataArray={this.props.nodes}
                      dataID={this.props.nodeID}
                      dataIdField="nodeid"
                      match="contains"
                      componentColumns ={this.state.componentColumns}
                      options={{gridHeight: {windowPct: 100, offset: -285}, multiSelect: true}}
                      columns={this.state.nodeStoredWidths.map(function(column, index){
                        if(column.name === "L"){
                          return(
                          {name:column.name, field:column.field, id: column.id, checked: column.checked, required: column.required,width: column.width,
                            sortable:column.sortable, disableSearch: true, cssClass: "text-center", headerCssClass: "text-center gridHeader",maxWidth: 36,
                            formatter: function (row, cell, value, columnDef, dataContext) {
                              switch (helpers.modelType(dataContext.model)) {
                                case "Lighting":
                                  var status = ["Working", "Warn", "Error", "NA"];
                                  value = status[value];
                                  return '<img height="20" width="20" title="" alt="'
                                      + value
                                      + '" src="/imgs/Lighting_'
                                      + value
                                      + '.svg" />';
                                  break;
                                case "Video":
                                  status = ["good", "warn", "error", "none"];
                                  value = status[value];
                                  return '<img height="16" width="20" title="" alt="'
                                      + value
                                      + '" src="/imgs/camera-'
                                      + value
                                      + '.png" />';
                                  break;
                                case "Kiosk":
                                  status = ["good", "warn", "error", "none"];
                                  value = status[value];
                                  return '<img height="18" width="18" title="" alt="'
                                      + value
                                      + '" src="/imgs/Kiosk_'
                                      + value
                                      + '.svg" />';
                                }
                              }
                            }
                          )
                        }else if(column.name === "N"){
                          return(
                          {
                            name:column.name,field:column.field, id: column.id,checked: column.checked,required: column.required,
                            width: column.width, disableSearch: true,sortable:column.sortable,headerCssClass: "text-center gridHeader",maxWidth: 36,
                            formatter: function (row, cell, value) {
                              var status = ["Working", "Warn", "Error", "NA"];
                              value = status[value];
                              return '<img height="20" width="20" title="" alt="'
                                  + value
                                  + '" src="/imgs/Network_'
                                  + value
                                  + '.svg" />'
                            }
                          }
                          )
                        }else if(column.name === "S"){
                          return(
                          {
                            name:column.name,field:column.field, id: column.id,checked: column.checked,required: column.required,
                            width: column.width, disableSearch: true,sortable:column.sortable,headerCssClass: "text-center gridHeader",maxWidth: 36,
                            formatter: function (row, cell, value) {
                              var status = ["Working", "Warn", "Error", "NA"];
                              value = status[value];
                              return '<img height="20" width="20" title="" alt="'
                                  + value
                                  + '" src="/imgs/Sensor_'
                                  + value
                                  + '.svg" />'
                            }
                          }
                          )
                        }else if(column.name === "Latitude"){
                          return(
                          {
                            name:column.name,field:column.field, id: column.id,checked: column.checked,required: column.required,
                            width: column.width, sortable:column.sortable,headerCssClass: "gridHeader",
                            formatter: function (row, cell, value) {
                              if (typeof value == "undefined"
                                  || value == ""
                                  || !$.isNumeric(value)
                                  || value < -90
                                  || value > 90) {
                                return '<span style="color:red">' + value + '</span>'
                              } else {
                                return value;
                              }
                            }
                          }
                          )
                        }else if(column.name === "Longitude"){
                          return(
                          {
                            name:column.name,field:column.field, id: column.id,checked: column.checked,required: column.required,
                            width: column.width, sortable:column.sortable,headerCssClass: "gridHeader",
                            formatter: function (row, cell, value) {
                              if (typeof value == "undefined"
                                  || value == ""
                                  || !$.isNumeric(value)
                                  || value < -180
                                  || value > 180) {
                                return '<span style="color:red">' + value + '</span>'
                              } else {
                                return value;
                              }
                            }
                          }
                          )
                        }else if(column.name === "Groups"){
                          return(
                          {
                            name:column.name,field:column.field, id: column.id,checked: column.checked,required: column.required,
                            width: column.width, sortable:column.sortable,headerCssClass: "gridHeader",
                            formatter: function (row, cell, value) {
                              return JSON.stringify(value).replace(/[\[\]\"]/g, '');
                            }
                          }
                          )
                        }else{
                          return({
                            name:column.name,field:column.field, id: column.id,checked: column.checked,required: column.required,
                            width: column.width, sortable:column.sortable,cssClass: column.cssClass, headerCssClass: "gridHeader"
                          })
                        }
                      })}
            />

          </div>
        </div>
        <div style={{textAlign:"center"}}>
            <button id="showHideDetails" className="ns-big-btn" style={{float:"none",width:"300px !important"}} onClick={this.toggleDetail}>{
                this.props.detail_state=="hidden" 
                ?<b>Show/Edit Details</b>
                :<b>Hide Details</b>
              }</button>
          </div>
      </div>
    );
  }
});

module.exports = Nodelist;