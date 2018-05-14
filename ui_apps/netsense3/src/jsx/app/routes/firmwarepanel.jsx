import classNames from 'classnames';
import { Link, withRouter } from 'react-router';

import helpers from 'global/utils/helpers';
import Firmwarelist from 'components/firmware/firmwarelist';
import FirmwareDetail from 'components/firmware/firmwaredetail';
import FirmwareUpdateModal from 'components/firmwareupdatemodal';
import DataUtil from '../service/datautil';

import Header from 'common/headernew';


var Body = React.createClass({
  getInitialState: function () {
    return {
      firmwares: null,
      otas:null,
      groups: null,
      showupdatedfirmware: false,
      showFirmwareDetail: false,
      updatedfirmware:null,
      firmwareID: "-1"
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
    DataUtil.getAll('firmwares', that.processFirmwareData);
    // $.ajax({
    //       url: NSN.apiURL + 'firmwares', 
    //       data : '',
    //       xhrFields: {
    //          withCredentials: true
    //       },
    //       method : 'GET',
    //       dataType : 'json',
    //       success : function(data){
    //         console.log("ajax success: " + JSON.stringify(data));
    //         if (data == "") {
    //           that.setState({firmwares:[]});
    //         } else {
    //           that.setState({
    //             firmwares: data.map(function(firmware, index) {
    //               if (typeof firmware.sites == "undefined") {firmware.sites = [];};
    //               if (typeof firmware.groups == "undefined") {firmware.groups = [];};
    //               if (typeof firmware.nodes == "undefined") {firmware.nodes = [];};
    //               if (firmware.sites.length == 1 && $.isEmptyObject(firmware.sites[0])) {
    //                 firmware.sites = [];
    //               };
    //               if (firmware.groups.length == 1 && $.isEmptyObject(firmware.groups[0])) {
    //                 firmware.groups = [];
    //               };
    //               if (firmware.nodes.length == 1 && $.isEmptyObject(firmware.nodes[0])) {
    //                 firmware.nodes = [];
    //               };
    //               firmware.assign = "unassigned";
    //               if (firmware.sites.length > 0) {
    //                 firmware.assign = "sitewide";
    //               } else {
    //                 if (firmware.groups.length > 0) {
    //                   firmware.assign = "groups";
    //                 }
    //               }
    //               firmware.idx = index;
    //               return firmware;
    //             })
    //           })
    //         }
    //       },
    //       error : function(jqXHR, status, error){
    //         console.log("ajax failure (firmwares): " + status + " - " + error);
    //         $("#loadingmsg").html("Cannot retrieve Firmware versions.  API reported error: " + error);
    //       }
    //     });

    DataUtil.getAll('groups', that.processGroupData);
    DataUtil.getAll('otas', that.processOtaData);

    // $.ajax({
    //     url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/groups', 
    //     data : '',
    //     method : 'GET',
    //     "xhrFields": {
    //        withCredentials: true
    //     },
    //     dataType : 'json',
    //     success : function(data){
    //         console.log("ajax success: " + JSON.stringify(data));
    //         that.setState({
    //           groups: data.map(function(group, index) {
    //             group.idx = index;
    //             return group;
    //           })
    //         })
    //     },
    //     error : function(jqXHR, status, error){
    //       $("#loadingmsg").html("Cannot retrieve Groups.  API reported error: " + error)
    //     }
    //   });
  },

  makeOtaObj: function (ota, index) {
    ota.idx = index;
    return ota;
  },
  processOtaData: function (data) {
    console.log("ajax success otas: " + JSON.stringify(data));
    this.setState(DataUtil.assignState('otas', data, this, this.makeOtaObj))
  },

  makeGroupObj: function (group, index) {
    group.idx = index;
    return group;
  },
  processGroupData: function (data) {
    console.log("ajax success: " + JSON.stringify(data));
    this.setState(DataUtil.assignState('groups', data, this, this.makeGroupObj))
  },

  makeFirmwareobj: function (firmware, index) {
    if (typeof firmware.sites == "undefined") { firmware.sites = []; };
    if (typeof firmware.groups == "undefined") { firmware.groups = []; };
    if (typeof firmware.nodes == "undefined") { firmware.nodes = []; };
    if (firmware.sites.length == 1 && $.isEmptyObject(firmware.sites[0])) {
      firmware.sites = [];
    };
    if (firmware.groups.length == 1 && $.isEmptyObject(firmware.groups[0])) {
      firmware.groups = [];
    };
    if (firmware.nodes.length == 1 && $.isEmptyObject(firmware.nodes[0])) {
      firmware.nodes = [];
    };
    firmware.assign = "unassigned";
    if (firmware.sites.length > 0) {
      firmware.assign = "sitewide";
    } else {
      if (firmware.groups.length > 0) {
        firmware.assign = "groups";
      }
    }
    firmware.idx = index;
    firmware.type = helpers.modelName(firmware.type) || "Core Node";
    return firmware;
  },
  processFirmwareData: function (data) {
    console.log("ajax success: " + JSON.stringify(data));
    if (data == "") {
      this.setState({ firmwares: [] });
    } else {
      this.setState(DataUtil.assignState('firmwares', data, this, this.makeFirmwareobj))
    }
  },
  firmwareAssignment(assignment, firmwareid,description) {
    var payload = {};
    if(typeof description != "undefined"){
      payload = {
        description: description
      }
    }
    else{
      payload = {
        description: ""
      }
    }
    
    if (assignment.assign == "unassigned"
      || (assignment.assign == "groups" && assignment.assigngroups.length == 0)) {
      noty({ type: "information", text: "As requested, no updates were sent." });
      return;
    }
    if (assignment.assign == "sitewide") {
      var url = NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID
        + '/firmwares/' + firmwareid + '/assign/site';
    } else {
      if (assignment.assign == "groups") {
        url = NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID
          + '/firmwares/' + firmwareid + '/assign/groups/' + assignment.assigngroups[0];
      }
    }
    $.ajax({
      url: url,
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
          type: "success", text: 'Updates sent '
            + ((assignment.assign == "sitewide") ? "sitewide" : "to group")
            + ' as requested.'
        });
      },
      error: function (jqXHR, status, error) {
        if (jqXHR.status == 200) {
          noty({
            type: "success", text: 'Updates sent '
              + ((assignment.assign == "sitewide") ? "sitewide" : "to group")
              + ' as requested.'
          });
        } else {

          console.log("ajax failure (firmware assignment): " + status + " - " + error);

          if (jqXHR.status == 400) {
            noty({ type: "error", text: JSON.parse(jqXHR.responseText).message });
          } else {
            noty({
              type: "error", text: 'Unable to send updates '
                + 'as requested.  Return status: ' + status
            });
          }

        }
      }
    });
  },

  /////////////////Callback funtion to Delete Firmware////////
  processDeleteFirmware: function (entityData, idx, data) {
    var newState = React.addons.update(this.state, { firmwares: { $splice: [[idx, 1]] }, firmwareID: { $set: "-1" } });
    noty({ type: "success", text: 'Firmware "' + entityData.name + '" deleted.' })
    NSN.firmwareID = "-1";
    sessionStorage.setItem("firmwareID", NSN.firmwareID);
    ReactBootstrap.Dispatcher.emit("Firmwareform.delete.success", entityData.firmwareid);
    this.setState(newState);
  },
  //////////////Callback to add Firmware/////////
  processAddFirmware: function (data) {
    var assignment = {
      assign: data.inputdata.assign,
      assigngroups: data.inputdata.assigngroups.splice(0)
    };
    var save_sites = data.inputdata.sites.splice(0);
    var save_groups = data.inputdata.groups.splice(0);
    var save_nodes = data.inputdata.nodes.splice(0);
    var that = this;
    data.groups = that.recreateGroups(assignment.assigngroups, that.state.groups);
    data.assign = assignment.assign;
    data.sites = save_sites.splice(0);
    data.nodes = save_nodes.splice(0);
    NSN.firmwareID = data.firmwareid;
    sessionStorage.setItem("firmwareID", NSN.firmwareID);
    data.idx = this.state.firmwares.length;
    var newState = React.addons.update(this.state, { firmwares: { $push: [data] }, firmwareID: { $set: data.firmwareid } });
    noty({ type: "success", text: 'Firmware version "' + data.name + '" added.' })
    this.firmwareAssignment(assignment, data.firmwareid);
    ReactBootstrap.Dispatcher.emit("Firmwareform.add.success", data);
    this.setState(newState);
  },
  ///////////////Callback to update firmware//////////
  processUpdateFirmware: function (data, inputdata) {
    var assignment = {
      assign: inputdata.assign,
      assigngroups: inputdata.assigngroups.splice(0)
    };
    var save_sites = inputdata.sites.splice(0);
    var save_groups = inputdata.groups.splice(0);
    var save_nodes = inputdata.nodes.splice(0);
    data.groups = that.recreateGroups(assignment.assigngroups, that.state.groups);
    data.assign = assignment.assign;
    data.sites = save_sites.splice(0);
    data.nodes = save_nodes.splice(0);
    var newState = React.addons.update(this.state, { firmwares: { [idx]: { $set: data } } });
    noty({ type: "success", text: 'Firmware version "' + data.name + '" updated.' })
    this.firmwareAssignment(assignment, data.firmwareid);
    ReactBootstrap.Dispatcher.emit("Firmwareform.update.success", data);
    this.setState(newState);
  },

  componentDidMount: function () {
    this.init();
    var that = this;

    ReactBootstrap.Dispatcher.on("Firmwarelist.select", function (firmwareID) {
      NSN.firmwareID = firmwareID;
      sessionStorage.setItem("firmwareID", NSN.firmwareID);
      that.setState({ 
        "firmwareID": firmwareID, 
        showFirmwareDetail: true
      });
    });

    /*ReactBootstrap.Dispatcher.on("Firmwarelist.add", function () {
      NSN.firmwareID = "0";
      sessionStorage.setItem("firmwareID", NSN.firmwareID);
      that.setState({ 
        "firmwareID": "0" ,
        showFirmwareDetail: true
      });
    }); */

    ReactBootstrap.Dispatcher.on("Firmwarelist.viewOTA", function () {
      that.props.router.push("/app/firmwareupdatepanel");
    });

    /*ReactBootstrap.Dispatcher.on("Firmwareform.reset", function () {
      that.forceUpdate();
    }); */

    ReactBootstrap.Dispatcher.on('Firmwareform.assign', function(switchto, firmware_info) {
      that.setState({
          showupdatedfirmware: switchto == 'open',
          updatedfirmware: firmware_info
      });
    });

    /*ReactBootstrap.Dispatcher.on("Firmwareform.delete", function (firmware_info) {
      var idx = helpers.get_idx(that.state.firmwares, firmware_info, 'firmwareid');
      console.log("Deleting firmware (idx:" + idx + "; id:" + firmware_info.firmwareid + ")");
      delete firmware_info.idx;
      DataUtil.deleteEntity('firmwares/', firmware_info, that.processDeleteFirmware, idx);
      that.setState({
        showFirmwareDetail: false
      });
      // $.ajax({
      //   url: NSN.apiURL + 'firmwares/' + firmware_info.firmwareid,
      //   "type" : "DELETE",
      //   "xhrFields": {
      //      withCredentials: true
      //   },
      //   "data" : JSON.stringify(firmware_info),
      //   "dataType" : "json",
      //   "contentType" : "application/json",
      //   "processData" : false,
      //   "success" : function(data) {
      //     var newState = React.addons.update(this.state, { firmwares: { $splice: [[idx, 1]] }, firmwareID: { $set : "-1"}});
      //     noty({type:"success", text:'Firmware "' + firmware_info.name + '" deleted.'})
      //     NSN.firmwareID = "-1";
      //     sessionStorage.setItem("firmwareID", NSN.firmwareID);
      //     ReactBootstrap.Dispatcher.emit("Firmwareform.delete.success", firmware_info.firmwareid);
      //     this.setState(newState);
      //   }.bind(that),
      //   "error" : function() {
      //     noty({type:"error", text:'Could not delete Firmware.'});
      //   }
      // })
    });*/

    ReactBootstrap.Dispatcher.on("Firmwareform.save", function (firmware_info) {
      var newState = {};
      var assignment = {assign: firmware_info.assign, 
                        assigngroups: firmware_info.assigngroups.splice(0)};
    
      var description = firmware_info.description;
      delete firmware_info.assign;
      delete firmware_info.assigngroups;
      var save_sites = firmware_info.sites.splice(0);
      delete firmware_info.sites;
      var save_groups = firmware_info.groups.splice(0);
      delete firmware_info.groups;
      var save_nodes = firmware_info.nodes.splice(0);
      delete firmware_info.nodes;
      //delete firmware_info.description;
      /*if (typeof firmware_info.firmwareid == "undefined" || firmware_info.firmwareid == "") {
        console.log("Adding firmware: " + firmware_info.name);
        delete firmware_info.idx;
        delete firmware_info.firmwareid;
        var duplicate = false;
        for(var i = 0, len = that.state.firmwares.length; i < len; i++) {
          console.log(JSON.stringify(that.state.firmwares[i].name) + JSON.stringify(firmware_info.name));
          if( (that.state.firmwares[ i ].name === firmware_info.name))
            duplicate = true;
        }
        if(duplicate){
          noty({type:"error", text:'Firmware Version "' + firmware_info.name + '" already exists.'});
        }  
        else{
          $.ajax({
            url: NSN.apiURL + 'firmwares', 
            "type" : "POST",
            "data" : JSON.stringify(firmware_info),
            "xhrFields": {
               withCredentials: true
            },
            "dataType" : "json",
            "contentType": "application/json",
            "processData": false,
            "success" : function(data) {
              data.groups = that.recreateGroups(assignment.assigngroups, that.state.groups);
              data.assign = assignment.assign;
              data.sites = save_sites.splice(0);
              data.nodes = save_nodes.splice(0);
              NSN.firmwareID = data.firmwareid;
              sessionStorage.setItem("firmwareID", NSN.firmwareID);
              data.idx = this.state.firmwares.length;
              var newState = React.addons.update(this.state, { firmwares: { $push : [data] }, firmwareID: { $set : data.firmwareid }});
              noty({type:"success", text:'Firmware version "' + data.name + '" added.'})
              this.firmwareAssignment(assignment, data.firmwareid,description);
              ReactBootstrap.Dispatcher.emit("Firmwareform.add.success", data);
              this.setState(newState);
            }.bind(that),
            "error" : function() {
              noty({type:"error", text:"Could not add Firmware version."});
            }
          });
          that.setState({
            showFirmwareDetail: false
          });
        }
      } else { */
        /*var idx = helpers.get_idx(that.state.firmwares, firmware_info, 'firmwareid');
        console.log("Updating firmware (idx:" + idx + "; id:" + firmware_info.firmwareid + ")");
        delete firmware_info.idx;
        $.ajax({
          url: NSN.apiURL + 'firmwares/' + firmware_info.firmwareid, 
          "type" : "POST",
          "xhrFields": {
             withCredentials: true
          },
          "data" : JSON.stringify(firmware_info),
          "dataType" : "json",
          "contentType" : "application/json",
          "processData" : false,
          "success" : function(data) {
            data.groups = that.recreateGroups(assignment.assigngroups, that.state.groups);
            data.assign = assignment.assign;
            data.sites = save_sites.splice(0);
            data.nodes = save_nodes.splice(0);
            console.log(data);
            var newState = React.addons.update(this.state, { firmwares: { [idx]: { $set: data } }});
            noty({type:"success", text:'Firmware version "' + data.name + '" updated.'})
            this.firmwareAssignment(assignment, data.firmwareid,description);
            ReactBootstrap.Dispatcher.emit("Firmwareform.update.success", data);
            this.setState(newState);
          }.bind(that),
          "error" : function() {
            noty({type:"error", text:'Could not update Firmware version.'});
          }
        }); */

        that.firmwareAssignment(assignment, firmware_info.firmwareid,description);
        that.setState({
          showFirmwareDetail: false
        });
      //}
    })
  },

  componentWillUnmount: function () {
    ReactBootstrap.Dispatcher.removeAllListeners("Firmwarelist.select");
   // ReactBootstrap.Dispatcher.removeAllListeners("Firmwarelist.add");
    ReactBootstrap.Dispatcher.removeAllListeners("Firmwareform.save");
    //ReactBootstrap.Dispatcher.removeAllListeners("Firmwareform.reset");
    //ReactBootstrap.Dispatcher.removeAllListeners("Firmwareform.delete");
  },

  hideFirmwareDetail() {
    this.setState({
      showFirmwareDetail: false
    });
  },

  render() {
    var hstyle = { height: helpers.calcHeight(90, 0) + "px !important" };
    if (this.state.firmwares && this.state.groups && this.state.otas) {
      var Subpanels = (
        <div className="netsense-center-panel">
          <Col md={12} lg={12} >
            <PanelContainer>
              <Panel>
                <PanelBody style={hstyle}>
                  <Firmwarelist firmwares={this.state.firmwares} firmwareID={this.state.firmwareID} otas={this.state.otas} />
                  <FirmwareDetail show={this.state.showFirmwareDetail}  hide={this.hideFirmwareDetail} groups={this.state.groups} otas={this.state.otas} firmwares={this.state.firmwares} firmwareID={this.state.firmwareID} allFirmwares={this.state.firmwares} />
                </PanelBody>
              </Panel>
            </PanelContainer>
          </Col>
         
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
          <FirmwareUpdateModal context="firmware" show={this.state.showupdatedfirmware} entity={this.state.updatedfirmware}/>
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
                    <h2 id="loadingmsg" style={{ paddingTop: "16%", textAlign: "center" }}>Loading Firmware Versions.</h2>
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

var BodyComponent = withRouter(Body);

export default class extends React.Component {
  render() {
    var classes = classNames({
      'container-open': this.props.open
    });

    return (
      <Container id='container' className={classes}>
        <Header />
        <BodyComponent />
      </Container>
    );
  }
}