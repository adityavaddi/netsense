import classNames from 'classnames';
import helpers from 'global/utils/helpers';
import Schedulelist from 'components/schedules/schedulelist';
import Scheduledetail from 'components/schedules/scheduledetail';
import DataUtil from '../service/datautil';

import Header from 'common/headernew';


var Body = React.createClass({
  getInitialState: function () {
    return {
      groups: null,
      schedules: null,
      groupID: "-1",
      scheduleID: "-1",
      showScheduleDetail : false,
      submitStatus: false
    }
  },

  calcHeight: function (pct, extra) {
    var h = (window && window.document) ? (($(window).height() - 75) * (pct / 100) + extra) : 500;
    return h;
  },

  init() {
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
    if (typeof NSN.site == "undefined") {
      NSN.site = JSON.parse(sessionStorage.getItem("site"));
    };
    // if (typeof NSN.site.time_zone == "undefined" || NSN.site.time_zone == "") {
    //   $("#loadingmsg").html("<p>This site does not have time zone information.</p>"
    //     + "<p>As a result, schedules cannot be created or modified.</p>"
    //     + "<p>Go to the Sites page, select this site and Save it.</p>"
    //     + "<p>The system will determine the time zone and you can carry on.</p>")
    //     .css({ paddingTop: '8%' });
    //   return;
    // }
    $.ajax({
      url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/schedules',
      data: '',
      method: 'GET',
      "xhrFields": {
        withCredentials: true
      },
      dataType: 'json',
      success: function (data) {
        console.log("ajax success: " + JSON.stringify(data));
        $("#loadingmsg").html("Generating display.");
        if (data == "") {
          that.setState({ schedules: [] });
        } else {
          that.setState({
            schedules: data.map(function (schedule, index) {
              // populate old schedules with default photocell data
              if (typeof schedule.network.photocell_enabled == "undefined"
                || schedule.network.photocell_enabled === null) {
                schedule.network.photocell_enabled = false;
              };
              if (typeof schedule.network.photocell_highLevel == "undefined"
                || schedule.network.photocell_highLevel === null) {
                schedule.network.photocell_highLevel = 100;
              };
              if (typeof schedule.network.photocell_lowLevel == "undefined"
                || schedule.network.photocell_lowLevel === null) {
                schedule.network.photocell_lowLevel = 0;
              };
              for (var i=0; i<schedule.events.length; i++) {
                if (typeof schedule.events[i].photocell_enabled == "undefined"
                  || schedule.events[i].photocell_enabled === null) {
                  schedule.events[i].photocell_enabled = false;
                };
                if (typeof schedule.events[i].photocell_highLevel == "undefined"
                  || schedule.events[i].photocell_highLevel === null) {
                  schedule.events[i].photocell_highLevel = 100;
                };
                if (typeof schedule.events[i].photocell_lowLevel == "undefined"
                  || schedule.events[i].photocell_lowLevel === null) {
                  schedule.events[i].photocell_lowLevel = 0;
                };
              };
              if (schedule.sites.length == 1 && $.isEmptyObject(schedule.sites[0])) {
                schedule.sites = [];
              };
              schedule.idx = index;
              return schedule;
            })
          });
        };
      },
      error: function () {
        console.log("ajax failure");
        that.setState({
          schedules: helpers.getScheduleList().map(function (schedule, index) {
            schedule.idx = index;
            return schedule;
          })
        });

      }
    });
    DataUtil.getAll('groups', that.processGetGroups);
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
    //         groups: data.filter(function(group, index) {
    //           return group.type == "lighting";
    //         }).map(function(group, index) {
    //           group.idx = index;
    //           return group;
    //         })
    //       })
    //   },
    //   error : function(){
    //     $("#loadingmsg").html("Cannot get groups.  API call failed.")
    //   }
    // });
  },

  //callback function to get all groups

  processGetGroups: function (data) {
    console.log("ajax success: " + JSON.stringify(data));
    this.setState({
      groups: data.filter(function (group, index) {
        return group.type == "lighting";
      }).map(function (group, index) {
        group.idx = index;
        return group;
      })
    })
  },

  //callback function to delete schedules

  processDeleteSchedules: function (schedule_info, idx, data) {
    var newState = React.addons.update(this.state, { schedules: { $splice: [[idx, 1]] }, scheduleID: { $set: "-1" } });
    noty({ type: "success", text: 'Schedule "' + schedule_info.name + '" deleted.' })
    NSN.scheduleID = "-1";
    sessionStorage.setItem("scheduleID", NSN.scheduleID);
    ReactBootstrap.Dispatcher.emit("Scheduleform.delete.success", schedule_info.scheduleid);
    this.setState(newState);
  },

  //callback fucntion for add schedules

  // processAddSchedule: function (data) {
  //   console.log("Response from Add Schedule: " + JSON.stringify(data));
  //   NSN.scheduleID = data.scheduleid;
  //   sessionStorage.setItem("scheduleID", NSN.scheduleID);
  //   noty({ type: "success", text: 'Schedule "' + data.name + '" added.' });
  //   ReactBootstrap.Dispatcher.emit('Scheduleform.add.success', data);
  //   var newState = React.addons.update(this.state, { schedules: { $push: [data] }, scheduleID: { $set: data.scheduleid } });
  //   this.setState(newState);
  //   //              this.scheduleAssignment(schedule_info);
  // },

  // callback for update entity

  // processUpdateSchedule: function (data, idx) {
  //           noty({ type: "success", text: 'Schedule "' + data.name + '" updated.' });
  //           ReactBootstrap.Dispatcher.emit('Scheduleform.update.success', data);
  //           var newState = React.addons.update(this.state, { schedules: { [idx]: { $set: data } } });
  //           this.setState(newState);
  //           //            this.scheduleAssignment(schedule_info);
  //         },

  checkDuplicate: function(schedule_info){
          var duplicate = false;
        for (var i = 0, len = this.state.schedules.length; i < len; i++) {
          if ((this.state.schedules[i].name === schedule_info.name)&&!(this.state.schedules[i].scheduleid === schedule_info.scheduleid))
            duplicate = true;
        }
        if (duplicate) {
          noty({ type: "error", text: 'Schedule "' + schedule_info.name + '" already exists.' });
          this.setState({
            submitStatus: true
       });
        }
      return duplicate;
  },
  componentDidMount() {
    this.init();
    var that = this;

    ReactBootstrap.Dispatcher.on("Schedulelist.select", function (scheduleID, sortedFlag) {
      // if(scheduleID != NSN.scheduleID){
        if(scheduleID != NSN.scheduleID  || (scheduleID == NSN.scheduleID && !sortedFlag)) {
        NSN.scheduleID = scheduleID;
        sessionStorage.setItem('scheduleID', NSN.scheduleID);
        that.setState({ "scheduleID": scheduleID, showScheduleDetail: true, submitStatus: true });
      }
    });

    ReactBootstrap.Dispatcher.on("Schedulelist.add", function () {
      that.setState({ "scheduleID": "0", showScheduleDetail: true, submitStatus: true });
    });

    ReactBootstrap.Dispatcher.on("Scheduleform.reset", function () {
      that.forceUpdate();
    });

    ReactBootstrap.Dispatcher.on("Scheduleform.delete", function (schedule_info) {
      var idx = helpers.get_idx(that.state.schedules, schedule_info, 'scheduleid');
      console.log("Deleting schedule (idx:" + idx + "; id:" + schedule_info.scheduleid + ")");
      delete schedule_info.idx;
      DataUtil.deleteEntity('delete-schedules', schedule_info, that.processDeleteSchedules, idx);
      that.hideScheduleDetail();
      // $.ajax({
      //   url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/schedules/' + schedule_info.scheduleid, 
      //   "type" : "DELETE",
      //   "xhrFields": {
      //      withCredentials: true
      //   },
      //   "data" : JSON.stringify(schedule_info),
      //   "dataType" : "json",
      //   "contentType" : "application/json",
      //   "processData" : false,
      //   "success" : function(data) {
      //     var newState = React.addons.update(this.state, { schedules: { $splice: [[idx, 1]] }, scheduleID: { $set : "-1"}});
      //     noty({type:"success", text:'Schedule "' + schedule_info.name + '" deleted.'})
      //     NSN.scheduleID = "-1";
      //     sessionStorage.setItem("scheduleID", NSN.scheduleID);
      //     ReactBootstrap.Dispatcher.emit("Scheduleform.delete.success", schedule_info.scheduleid);
      //     this.setState(newState);
      //   }.bind(that),
      //   "error" : function() {
      //     noty({type:"error", text:'Could not delete schedule.'});
      //   }
      // }) 
    });

    ReactBootstrap.Dispatcher.on("Scheduleform.save",function(schedule_info) {
      that.setState({
          submitStatus: false
       });
      var missingDays = ['mon','tue','wed','thu','fri','sat','sun']
      var schledDays = []
      for(var i in schedule_info.events){
        //get all the Scheduled Days from all events
        schledDays = schledDays.concat(schedule_info.events[i].days)
      }

      var index;
      for (var i in schledDays) {
          index = missingDays.indexOf(schledDays[i]);
          if (index > -1) {
              //Filter That does not have Schedule
              missingDays.splice(index, 1);
          }
      }

      // alert(JSON.stringify(schedule_info, null, 5));
      var newState = {};
      var assignment = Object.assign({}, schedule_info.assignment);
      delete schedule_info.assignment;

         if (schedule_info.scheduleid == "-1" || schedule_info.scheduleid == "0") {
           if(!that.checkDuplicate(schedule_info)){
        delete schedule_info.idx;
        delete schedule_info.scheduleid;
          // DataUtil.addEntity('schedules', schedule_info, that.processAddSchedule, '');
          $.ajax({
            "url": NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/schedules',
            "type": "POST",
            "data": JSON.stringify(schedule_info),
            "xhrFields": {
              withCredentials: true
            },
            "dataType": "json",
            "contentType": "application/json",
            "processData": false,
            "success": function (data) {
              console.log("Response from Add Schedule: " + JSON.stringify(data));
              NSN.scheduleID = data.scheduleid;
              sessionStorage.setItem("scheduleID", NSN.scheduleID);
              noty({ type: "success", text: 'Schedule "' + data.name + '" added.' });
              ReactBootstrap.Dispatcher.emit('Scheduleform.add.success', data);
              let schedulesList = that.state.schedules;
              this.setState({
                schedules: this.state.schedules.concat(data),
                scheduleID: data.scheduleid,
                submitStatus: true,
                showScheduleDetail: false
              });
              //newState = React.addons.update(that.state, { schedules: { $push: [data] }, scheduleID: { $set: data.scheduleid }, submitStatus: { $set: true } });
              //that.setState({newState});
              //that.hideScheduleDetail();              
            }.bind(that),
            "error": function (jqXHR, textStatus, errorThrown) {
              noty({ type: "error", text: "Could not add schedule: " + textStatus });
            }
          })
         }
      } else if(!that.checkDuplicate(schedule_info)){
        var idx = helpers.get_idx(that.state.schedules, schedule_info, 'scheduleid');
        delete schedule_info.idx;
        // DataUtil.updateEntity('update-schedules',schedule_info, that.processUpdateSchedule, idx  );
        $.ajax({
          "url": NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/schedules/' + NSN.scheduleID,
          "type": "POST",
          "xhrFields": {
            withCredentials: true
          },
          "data": JSON.stringify(schedule_info),
          "dataType": "json",
          "contentType": "application/json",
          "processData": false,
          "success": function (data) {
            noty({ type: "success", text: 'Schedule "' + data.name + '" updated.' });
            ReactBootstrap.Dispatcher.emit('Scheduleform.update.success', data);
            newState = React.addons.update(this.state, { schedules: { [idx]: { $set: data } }, submitStatus: { $set: true } });
            this.setState(newState);
            this.hideScheduleDetail();
            //            this.scheduleAssignment(schedule_info);
          }.bind(that),
          "error": function (jqXHR, textStatus, errorThrown) {
            noty({ type: "error", text: "Could not update schedule: " + textStatus });
          }
        })
      }
    })
  },

  componentWillUnmount: function () {
    ReactBootstrap.Dispatcher.removeAllListeners("Schedulelist.select");
    ReactBootstrap.Dispatcher.removeAllListeners("Schedulelist.add");
    ReactBootstrap.Dispatcher.removeAllListeners("Scheduleform.save");
    ReactBootstrap.Dispatcher.removeAllListeners("Scheduleform.reset");
    ReactBootstrap.Dispatcher.removeAllListeners("Scheduleform.delete");
  },

    hideScheduleDetail() {
    this.setState({
      showScheduleDetail: false
    })
  },
  render() {
    var hstyle = { height: helpers.calcHeight(90, 0) + "px !important" };
    if (this.state.schedules && this.state.groups) {
      var Subpanels = (
        <div className="netsense-center-panel">
          {/*<Col md={12} lg={12}>*/}
            <PanelContainer>
              <Panel>
                <PanelBody style={hstyle}>
                  <Scheduledetail show={this.state.showScheduleDetail} hide={this.hideScheduleDetail} schedules={this.state.schedules} scheduleID={this.state.scheduleID} groups={this.state.groups} submitStatus={this.state.submitStatus} />
                  <Schedulelist show={this.state.showScheduleDetail} schedules={this.state.schedules} scheduleID={this.state.scheduleID} />
                </PanelBody>
              </Panel>
            </PanelContainer>
          {/*</Col>*/}
          {/*<Col md={12} lg={8}>
            <PanelContainer>
              <Panel>
                <PanelBody style={hstyle}>
                  <Scheduledetail schedules={this.state.schedules} scheduleID={this.state.scheduleID} groups={this.state.groups} />
                </PanelBody>
              </Panel>
            </PanelContainer>
          </Col>*/}
        </div>
      );
      return (
        <Container id='body'>
          <Grid>
            <Row>
              <Col sm={12}>
                {Subpanels}
              </Col>
            </Row>
          </Grid>
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
                    <h2 id="loadingmsg" style={{ paddingTop: "16%", textAlign: "center" }}>Loading schedule information{forSite}.</h2>
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