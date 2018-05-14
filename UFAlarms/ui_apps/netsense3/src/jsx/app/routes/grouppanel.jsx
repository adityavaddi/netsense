import classNames from 'classnames';
import helpers from 'global/utils/helpers';
import Grouplist from 'components/groups/grouplist';
import Groupmap from 'components/groups/groupmap';
import Groupdetail from 'components/groups/groupdetail';
import ScheduleView from 'components/scheduleview';
import DataUtil from '../service/datautil';

import Header from 'common/headernew';


var Body = React.createClass({
  getInitialState: function () {
    return {
      groups: null,
      nodes: null,
      schedules: null,
      pdprofiles: null,
      etdhprofiles: null,
      site: null,
      showschedule: false,
      buildinglevels: [],
      groupID: NSN.groupID || "-1",
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
              left: $("#group-detail-panel").position().left + "px",
              top: $("#group-detail-panel").position().top + "px",
              width: $("#group-detail-panel").width() + "px",
              height: $("#group-detail-panel").height() + "px"
              };
      };
      this.setState({ui: $.extend({}, this.state.ui, {detail: "hidden", prevDetail: this.state.ui.detail})});
  },

  showDetail: function() {
      this.setState({ui: $.extend({}, this.state.ui, {detail: this.state.ui.prevDetail, prevDetail: "hidden"})});
  },

  pinDetail: function() {
      this.detailUnpinnedPosition = {
          left: $("#group-detail-panel").position().left + "px",
          top: $("#group-detail-panel").position().top + "px",
          width: $("#group-detail-panel").width() + "px",
          height: $("#group-detail-panel").height() + "px"
      };
      this.setState({ui: $.extend({}, this.state.ui, {detail: "pinned", prevDetail: "unpinned"})});
  },

  unpinDetail: function() {
      this.setState({ui: $.extend({}, this.state.ui, {detail: "unpinned", prevDetail: "pinned"})});
  },

  getGroup: function () {

    for (var i = 0; i < this.state.groups.length; i++) {
      if (this.state.groups[i].groupid == this.state.groupID) {
        return (this.state.groups[i]);
      }
    };
    return null;
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

   $.ajax({
      url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/minnodes',
      data: '',
      method: 'GET',
      "xhrFields": {
        withCredentials: true
      },
      dataType: 'json',
      success: function (data) {
        console.log("ajax success: " + JSON.stringify(data));
        $("#loadingmsg").html("Generating display.");

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
                for (var i = 0; i < data.length; i++) {
                    var nodeid = data[i].nodeid;
                    for (var j = 0; j < statusdata.length; j++) {
                        if (nodeid == statusdata[j].nodeid) {
                            data[i].net_stat = statusdata[j].net_stat;
                            statusdata.splice(j, 1);
                        }
                    }
                }

                that.setState({
                  nodes: data.map(function (node, index) {
                    if (typeof node.level == "undefined") {
                      node.level = "1";
                    };
                    if (typeof node.net_stat == "undefined") {
                      node.net_stat = false;
                    };
                    node.model = helpers.modelName(node.model);
                    node.idx = index;
                    return node;
                  })
                });

            },
            error: function () {
                console.log("node_status API failed");
            }
        });
      },
      error: function () {
        console.log("ajax failure");
        that.setState({
          nodes: helpers.getNodeList().map(function (node, index) {
            node.idx = index;
            return node;
          })
        });
      }
    });

    DataUtil.getAll('groups', that.processGroupData);
    // $.ajax({
    //   url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/groups',
    //   data : '',
    //   method : 'GET',
    //   "xhrFields": {
    //      withCredentials: true
    //   },
    //   dataType : 'json',
    //   success : function(data){
    //     if (data.errors) {
    //       $("#loadingmsg").html("Cannot get groups.  API returned error.")
    //     } else {
    //       console.log("ajax success: " + JSON.stringify(data));
    //       that.setState({
    //         groups: data.map(function(group, index) {
    //           group.idx = index;
    //           group.name = group.name || "";
    //           group.description = group.description || "";
    //           group.nodeList = group.nodeList || [];
    //           if (group.schedules.length == 1 && $.isEmptyObject(group.schedules[0])) {
    //             group.schedules = [];
    //           };
    //           if (group.dhprofiles.length == 1 && $.isEmptyObject(group.dhprofiles[0])) {
    //             group.dhprofiles = [];
    //           };
    //           if (group.pdprofiles.length == 1 && $.isEmptyObject(group.pdprofiles[0])) {
    //             group.pdprofiles = [];
    //           };
    //           group.type = group.type || "organizational";
    //           return group;
    //         })
    //       })
    //     };
    //   },
    //   error : function(){
    //     $("#loadingmsg").html("Cannot get groups.  API call failed.")
    //   }
    // });

    DataUtil.getAll('schedules', that.processScheduleData);
    // $.ajax({
    //   url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/schedules',
    //   data : '',
    //   method : 'GET',
    //   "xhrFields": {
    //      withCredentials: true
    //   },
    //   dataType : 'json',
    //   success : function(data){
    //     if (data.errors) {
    //       $("#loadingmsg").html("Cannot get schedules.  API returned error.")
    //     } else {
    //       console.log("ajax success: " + JSON.stringify(data));
    //       that.setState({
    //         schedules: data.map(function(schedule, index) {
    //           schedule.idx = index;
    //           return schedule;
    //         })
    //       })
    //     };
    //   },
    //   error : function(){
    //     $("#loadingmsg").html("Cannot get schedules.  API call failed.")
    //   }
    // });

    DataUtil.getAll('etdhprofiles', that.processDaylightHarvestData);
    // $.ajax({
    //   url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/daylightharvesting',
    //   data : '',
    //   method : 'GET',
    //   "xhrFields": {
    //      withCredentials: true
    //   },
    //   dataType : 'json',
    //   success : function(data){
    //     if (data.errors) {
    //       $("#loadingmsg").html("Cannot get Daylight Harvesting Profiles.  API returned error.")
    //     } else {
    //       console.log("ajax success: " + JSON.stringify(data));
    //       that.setState({
    //         dhprofiles: data.map(function(dhprofile, index) {
    //           dhprofile.idx = index;
    //           return dhprofile;
    //         })
    //       })
    //     };
    //   },
    //   error : function(){
    //     $("#loadingmsg").html("Cannot get schedules.  API call failed.")
    //   }
    // });

    DataUtil.getAll('pdprofiles', that.processPDprofileData);
    // $.ajax({
    //   url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/proximitydimming',
    //   data : '',
    //   method : 'GET',
    //   "xhrFields": {
    //      withCredentials: true
    //   },
    //   dataType : 'json',
    //   success : function(data){
    //     if (data.errors) {
    //       $("#loadingmsg").html("Cannot get Proximity Dimming Profiles.  API returned error.")
    //     } else {
    //       console.log("ajax success: " + JSON.stringify(data));
    //       that.setState({
    //           pdprofiles: data.map(function(pdprofile, index) {
    //           pdprofile.idx = index;
    //           return pdprofile;
    //         })
    //       })
    //     };
    //   },
    //   error : function(){
    //     $("#loadingmsg").html("Cannot get schedules.  API call failed.")
    //   }
    // });

    $.ajax({
      url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID,
      data: '',
      method: 'GET',
      "xhrFields": {
        withCredentials: true
      },
      dataType: 'json',
      success: function (data) {
        if (data.errors) {
          console.log("/nodes API returned error:" + JSON.stringify(data));
          $("#loadingmsg").html("Cannot retrieve site info. " + "/nodes API returned error: " + JSON.stringify(data));
        } else {
          console.log("ajax success: " + JSON.stringify(data));
          $("#loadingmsg").html("Generating display.");
          that.setState({
            site: data
          })
        };
      },
      error: function (jqXHR, status, error) {
        console.log("ajax failure (site): " + status + " - " + error);
        $("#loadingmsg").html("Cannot retrieve Site information.  API reported error: " + error);
      }
    });

  },

  /////////////////////Callback function to get groups//////////////
  processGroupData: function (data) {
    if (data.errors) {
      $("#loadingmsg").html("Cannot get groups.  API returned error.")
    } else {
      console.log("ajax success: " + JSON.stringify(data));
      this.setState(DataUtil.assignState('groups', data, this, this.makeGroupObj))
    };
  },
  makeGroupObj: function (group, index) {
    group.idx = index;
    group.name = group.name || "";
    group.description = group.description || "";
    group.nodeList = group.nodeList || [];
    if (group.schedules.length == 1 && $.isEmptyObject(group.schedules[0])) {
      group.schedules = [];
    };
    if (typeof group.etdhprofiles != "undefined") {
      if (group.etdhprofiles.length == 1 && $.isEmptyObject(group.etdhprofiles[0])) {
        group.etdhprofiles = [];
      };
    } else {
      if (group.dhprofiles.length == 1 && $.isEmptyObject(group.dhprofiles[0])) {
        group.dhprofiles = [];
      };
    };
    if (group.pdprofiles.length == 1 && $.isEmptyObject(group.pdprofiles[0])) {
      group.pdprofiles = [];
    };
    group.type = group.type || "organizational";
    return group;
  },
  ///////////////////Callback function to get schedules///////////
  processScheduleData: function (data) {
    if (data.errors) {
      $("#loadingmsg").html("Cannot get schedules.  API returned error.")
    } else {
      console.log("ajax success: " + JSON.stringify(data));
      this.setState(DataUtil.assignState('schedules', data, this, this.makeScheduleObj))
    };
  },
  makeScheduleObj: function (schedule, index) {
    schedule.idx = index;
    return schedule;
  },
  ///////////Callback function to get daylightharvesting//////////
  processDaylightHarvestData: function (data) {
    if (data.errors) {
      $("#loadingmsg").html("Cannot get Daylight Harvesting Profiles.  API returned error.")
    } else {
      console.log("ajax success: " + JSON.stringify(data));
      this.setState(DataUtil.assignState('etdhprofiles', data, this, this.makeDHprofileObj))
    };
  },
  makeDHprofileObj: function (etdhprofile, index) {
    etdhprofile.idx = index;
    return etdhprofile;
  },

  /////////////Callback function to get proximity dimming profile///////////
  processPDprofileData: function (data) {
    if (data.errors) {
      $("#loadingmsg").html("Cannot get Proximity Dimming Profiles.  API returned error.")
    } else {
      console.log("ajax success: " + JSON.stringify(data));
      this.setState(DataUtil.assignState('pdprofiles', data, this, this.makePDObj))
    };
  },
  makePDObj: function (pdprofile, index) {
    pdprofile.idx = index;
    return pdprofile;
  },
  checkDuplicate: function(group_info) {
        var duplicate = false;
        for (var i = 0, len = this.state.groups.length; i < len; i++) {
          console.log(JSON.stringify(this.state.groups[i].name) + JSON.stringify(group_info.name));
          if ((this.state.groups[i].name === group_info.name)&&!(this.state.groups[i].groupid === group_info.groupid))
            duplicate = true;
        }
            if (duplicate) {
              noty({ type: "error", text: 'Group "' + group_info.name + '" already exists.' });
            } 
          return duplicate;  
  },
  componentDidMount: function () {
    this.init();
    var that = this;

    ReactBootstrap.Dispatcher.on("Grouplist.select", function (groupID) {
      if(groupID != NSN.groupID){
        NSN.groupID = groupID;
        that.setState({ "groupID": groupID });
      }
    });

    ReactBootstrap.Dispatcher.on("Grouplist.add", function () {
      if (that.state.ui.detail == "hidden") {
        that.setState({groupID: "0", ui: $.extend({}, that.state.ui, {detail: that.state.ui.prevDetail, prevDetail: "hidden"})});
      } else {
        that.setState({groupID: "0"});
      }
    });

    ReactBootstrap.Dispatcher.on("Groupform.schedule", function (switchto) {
      that.setState({ showschedule: switchto == "open" });
    });

    ReactBootstrap.Dispatcher.on("Groupform.applySchedule", function (groupid, scheduleid) {
      var url = NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/schedules/'
        + scheduleid + '/apply/groups/' + groupid;
      $.ajax({
        url: url,
        method: 'GET',
        "xhrFields": {
          withCredentials: true
        },
        dataType: 'json',
        success: function (data) {
          console.log("ajax success: " + JSON.stringify(data));
          noty({ type: "success", text: 'Schedule applied as requested.' });
        },
        error: function (jqXHR, status, error) {
          if (jqXHR.status == 200) {
            noty({ type: "success", text: 'Schedule applied as requested.' });
          } else {
            noty({ type: "error", text: 'Unable to apply Schedule to this Group.  Return status: ' + status });
          }
        }
      });
    });

    ReactBootstrap.Dispatcher.on("Groupform.pushSchedule", function (groupid) {
      var url = NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/groups/' + groupid + '/schedule_push';
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
            text: 'Schedule pushed as requested.'
          });
        },
        error: function (jqXHR, status, error) {
          if (jqXHR.status == 200) {
            noty({
              type: "success",
              text: 'Schedule pushed as requested.'
            });
          } else {
            noty({
              type: "error",
              text: 'Unable to push Schedule to this Group.  Return status: ' + status
            });
          }
        }
      });
    });

    ReactBootstrap.Dispatcher.on("Groupform.lightLevel", function (nodeid, level, timeout) {
      $.ajax({
        url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/lightcontrol/groups/' + NSN.groupID,
        data: JSON.stringify({ level: parseInt(level), timeout: parseInt(timeout) }),
        contentType: 'application/json',
        method: 'POST',
        "xhrFields": {
          withCredentials: true
        },
        dataType: 'json',
        success: function (data) {
          if (data && data.errors) {
            console.log('lightcontrol error:' + JSON.stringify(data.errors))
          }
          noty({
            type: "success",
            text: 'Light Control pushed as requested.'
          });
        },
        error: function () {
          console.log("lightcontrol ajax failure");
        }
      })
    });

    ReactBootstrap.Dispatcher.on("Groupform.delete", function (group_info) {
      console.log("Groupform.delete", group_info);
      var group_idx = group_info.idx;
      delete group_info.idx;

      $.ajax({
        "url": NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/groups/' + group_info.groupid,
        "type": "DELETE",
        "xhrFields": {
          withCredentials: true
        },
        "data": JSON.stringify(group_info),
        "dataType": "json",
        "contentType": "application/json",
        "success": function (data) {
          var newState = React.addons.update(this.state, { groups: { $splice: [[group_idx, 1]] }, groupID: { $set: "-1" } });
          NSN.groupID = "-1";
          sessionStorage.setItem("groupID", NSN.groupID);
          noty({ type: "success", text: 'Group deleted.' });
          ReactBootstrap.Dispatcher.emit("Groupform.delete.success", group_info.groupid);
          this.setState(newState);
        }.bind(that),
        "error": function (jqXHR, status, error) {
          console.log("ajax failure (groups): " + status + " - " + error);
          if (jqXHR.status == 400) {
            noty({ type: "error", text: JSON.parse(jqXHR.responseText).message });
          } else {
            noty({ type: "error", text: "Could not delete group." });
          }
        }
      });
    });

    ReactBootstrap.Dispatcher.on("Groupform.save", function (group_info) {
      var newState = {};

       if (group_info.groupid == "") {
         if(!that.checkDuplicate(group_info)){
        delete group_info.idx;
        delete group_info.groupid;

              $.ajax({
                "url": NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/groups',
                "type": "POST",
                "data": JSON.stringify(group_info),
                "xhrFields": {
                  withCredentials: true
                },
                "dataType": "json",
                "contentType": "application/json",
                "processData": false,
                "success": function (data) {
                  console.log("Response from Add Group: " + JSON.stringify(data));
                  NSN.groupID = data.groupid;
                  data.idx = this.state.groups.length;
                  var newState = React.addons.update(this.state, { groups: { $push: [data] }, groupID: { $set: data.groupid } });
                  noty({ type: "success", text: 'Group "' + data.name + '" added.' });
                  ReactBootstrap.Dispatcher.emit("Groupform.add.success", data);
                  this.setState(newState);
                  $("#Group-grid").data("gridInstance").setSelectedRows([data.idx])
                }.bind(that),
                "error": function (jqXHR, textStatus, errorThrown) {
                  if (jqXHR.status == 400) {
                    noty({ type: "error", text: JSON.parse(jqXHR.responseText).message });
                  } else {
                    noty({ type: "error", text: "Could not add group." });
                  }
                }
              });
       }
      } else if(!that.checkDuplicate(group_info)){
        var group_idx = group_info.idx;
        delete group_info.idx;

        // Check for no two group names to be same:
        var duplicate = false;
        for (var i = 0, len = that.state.groups.length; i < len; i++) {
          if ((that.state.groups[i].name === group_info.name) && (that.state.groups[i].groupid != group_info.groupid))
            duplicate = true;
        }
        if (duplicate) {
          noty({ type: "error", text: 'Group "' + group_info.name + '" name is already used.' });
        }

        else {

          $.ajax({
            "url": NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/groups/' + group_info.groupid,
            "type": "POST",
            "xhrFields": {
              withCredentials: true
            },
            "data": JSON.stringify(group_info),
            "dataType": "json",
            "contentType": "application/json",
            "processData": false,
            "success": function (data) {
              data.idx = group_idx;
              var newState = React.addons.update(this.state, { groups: { [group_idx]: { $set: data } } });
              noty({ type: "success", text: 'Group "' + data.name + '" updated.' });
              ReactBootstrap.Dispatcher.emit("Groupform.update.success", data);
              this.setState(newState);
            }.bind(that),
            "error": function (jqXHR, textStatus, errorThrown) {
              if (jqXHR.status == 400) {
                noty({ type: "error", text: JSON.parse(jqXHR.responseText).message });
              } else {
                noty({ type: "error", text: "Could not update group." });
              }
            }
          });
        }
      }
    })

    ReactBootstrap.Dispatcher.on('Grouplist.toggleDetail', function () {
        if (that.state.ui.detail == "hidden") {
            that.showDetail();
        } else {
            that.hideDetail();
        }
    });

    ReactBootstrap.Dispatcher.on('Groupdetail.togglePin', function () {
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
    ReactBootstrap.Dispatcher.removeAllListeners("Grouplist.select");
    ReactBootstrap.Dispatcher.removeAllListeners("Grouplist.add");
    ReactBootstrap.Dispatcher.removeAllListeners("Groupform.save");
    ReactBootstrap.Dispatcher.removeAllListeners("Groupform.delete");
    ReactBootstrap.Dispatcher.removeAllListeners("Groupform.lightLevel");
    ReactBootstrap.Dispatcher.removeAllListeners("Groupform.schedule");
    ReactBootstrap.Dispatcher.removeAllListeners("Groupform.applySchedule");
    ReactBootstrap.Dispatcher.removeAllListeners("Groupform.pushSchedule");
    ReactBootstrap.Dispatcher.removeAllListeners("Grouplist.toggleDetail");
    ReactBootstrap.Dispatcher.removeAllListeners("Groupdetail.togglePin");

  },

  render() {
    var hstyle = { height: helpers.calcHeight(90, 0) + "px !important" };
    if (this.state.groups && this.state.nodes && this.state.schedules && this.state.site && this.state.etdhprofiles && this.state.pdprofiles) {
     var listStyle = {width:"33%",height:"100%",overflow:"hidden",backgroundColor:"#FFF",position:"absolute",zIndex:"300",left:"0px",top:"0px",boxShadow:"2px 0px 2px 0px #CCC"};
      switch (this.state.ui.detail) {
          case "unpinned":
              var detailStyle = {opacity:"0.9",position:"absolute",zIndex:"500",backgroundColor:"#FFF",overflowY:"hidden",overflowX:"hidden",border:"1px solid #EEE"};
              $.extend(detailStyle, this.detailUnpinnedPosition);
              var mapStyle = {width:"67%",height:"100%",position:"absolute",top:"0px",left:"33%",zIndex:"100"};
              $("#group-detail-panel").draggable("enable").resizable("enable");
              break;
          case "pinned":
              detailStyle = {position:"absolute", zIndex:"200", backgroundColor:"#FFF", left:"33%", width:"25%", height:"100%"};
              mapStyle = {width:"42%",height:"100%",position:"absolute",top:"0px",left:"58%",zIndex:"100"};
              $("#group-detail-panel").draggable("disable").resizable("disable");
              break;
          case "hidden":
              detailStyle = {position:"absolute", left:"-3000px"};
              mapStyle = {width:"67%",height:"100%",position:"absolute",top:"0px",left:"33%",zIndex:"100"};
              break;
       };
       return (
        <Container id='body' className="group-body" style={{backgroundColor:"#FFF",marginTop:"0px !important"}}>
            <div id="group-list-panel" data-state="closed" style={listStyle}>
                  <Grouplist minmax="max" groups={this.state.groups} groupID={this.state.groupID} detail_state={this.state.ui.detail} ui_state={this.state.ui} />
            </div>
            <div id="group-detail-panel" style={detailStyle}>
                  <Groupdetail groups={this.state.groups} groupID={this.state.groupID} schedules={this.state.schedules} etdhprofiles={this.state.etdhprofiles} pdprofiles={this.state.pdprofiles} nodes={this.state.nodes}  detail_state={this.state.ui.detail}/>
            </div>
            <div id="group-map-panel" data-state="closed" style={mapStyle}>
                  <Groupmap readonly={this.getGroup() != null && this.getGroup().type == "site-lighting"} nodes={this.state.nodes} groupID={this.state.groupID} site={this.state.site} />
            </div>
            <ScheduleView context="group" show={this.state.showschedule} entity={this.getGroup()} />
        </Container>
      );
    };
    var forSite = (NSN && NSN.siteName && NSN.siteName != "") ? (" for " + NSN.siteName) : "";
    return (
      <Container id='body'>
        <Grid>
          <Row>
            <Col sm={12}>
              <PanelContainer>
                <Panel>
                  <PanelBody style={hstyle} ref="loading">
                    <h2 id="loadingmsg" style={{ paddingTop: "16%", textAlign: "center" }}>Loading group information{forSite}.</h2>
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
