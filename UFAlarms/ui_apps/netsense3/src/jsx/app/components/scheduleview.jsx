import classNames from 'classnames';
import helpers from 'global/utils/helpers';
import scheduler from 'global/utils/scheduler';

import { State, Navigation } from 'react-router';

var ScheduleView = React.createClass({

  propTypes: {
    show: React.PropTypes.bool.isRequired,
    context: React.PropTypes.string.isRequired,
    entity: React.PropTypes.object.isRequired
  },

  handleClose: function() {
    $("#sched-heading").html("");
    ReactBootstrap.Dispatcher.emit(helpers.ucfirst(this.props.context) + 'form.schedule', 'close');
  },

  launch: function(schedule) {
    var that = this;
    if (schedule == "default") {
      $("#sched-heading").html('This ' + this.props.context + ' is operating under the default (always on) schedule.');
      scheduler.destroy($("#schedule"));      
    } else {
      //   Old dhprofileId
      // var hasDH = typeof this.props.entity.dhprofiles != "undefined"
      //             && this.props.entity.dhprofiles.length > 0
      //             && !$.isEmptyObject(this.props.entity.dhprofiles[0]);
      // hasDH = hasDH
      //         || (this.props.entity.etdhprofileid != "undefined"
      //             && this.props.entity.dhprofileid != "");


      // New - etdhprofileid and etdhprofiles
      var hasDH = typeof this.props.entity.etdhprofiles != "undefined"
          && this.props.entity.etdhprofiles.length > 0
          && !$.isEmptyObject(this.props.entity.etdhprofiles[0]);
      hasDH = hasDH
          || (typeof this.props.entity.etdhprofileid != "undefined"
            && this.props.entity.etdhprofileid != "");
      var hasPD = typeof this.props.entity.pdprofiles != "undefined"
          && this.props.entity.pdprofiles.length > 0
          && !$.isEmptyObject(this.props.entity.pdprofiles[0]);
      hasPD = hasPD 
          || (typeof this.props.entity.pdprofileid != "undefined" 
             && this.props.entity.pdprofileid != "");
      console.log ("hasDH: " + hasDH + ";  hasPD: " + hasPD);

      if (schedule === null) {
        $.ajax({
          url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/schedules', 
          data : '',
          method : 'GET',
          "xhrFields": {
             withCredentials: true
          },
          dataType : 'json',
          success : function(data){
            for (var i=0; i<data.length; i++) {
              if (data[i].sites.length == 1 && !$.isEmptyObject(data[i].sites[0])) {
                schedule = data[i];
              }
            }
            if (typeof schedule.network == "undefined") {
              schedule.network = {highTime:"18:00:00",
                            highLevel:60,
                            lowTime:"06:00:00",
                            lowLevel:0
                          };
            }
            $("#sched-heading").html(schedule.name 
              + ' <span style="font-size:20px" title="Site Default Schedule" '
                                + ' class="rubix-icon icon-fontello-commerical-building"></span>'
                                + ' &nbsp; (Default Site Schedule)');
              scheduler.init($("#schedule"), schedule, [], true, hasDH,  hasPD);
          },
          error : function(){
            $("#sched-heading").html("No Schedule defined for this " + this.props.context + ".");  
            scheduler.destroy($("#schedule"));             
          }
        });
      } else {

          // read only section as in ony display the Schedule
        if (typeof schedule.network == "undefined") {
          schedule.network = {highTime:"18:00:00",
                        highLevel:60,
                        lowTime:"06:00:00",
                        lowLevel:0
                      };
        }
        $("#sched-heading").html(schedule.name);
          scheduler.init($("#schedule"), schedule, [], true, hasDH, hasPD);
      };
    };
  },

  getSchedule: function() {
    var that = this, scheduleID = "";

      if(this.props.entity.etdhprofiles){
          for(var a=0; a < this.props.entity.etdhprofiles.length ; a++){
              this.props.entity.etdhprofileid = this.props.entity.etdhprofiles[a].etdhprofileid;
          }
      }
      if(this.props.entity.pdprofiles){
          for(var j=0; j < this.props.entity.pdprofiles.length ; j++){
              this.props.entity.pdprofileid = this.props.entity.pdprofiles[j].pdprofileid;
          }
      }

      if (this.props.context == "node") {
      if (typeof this.props.entity.scheduleid == "undefined" || this.props.entity.scheduleid == "") {
        this.launch(null);
      } else {
        scheduleID = this.props.entity.scheduleid;
      };
    } else {

        if ((this.props.entity.schedules.length == 0)
          || (this.props.entity.schedules.length == 1 && $.isEmptyObject(this.props.entity.schedules[0]))) {
          this.launch(null);
       } else {
         scheduleID = this.props.entity.schedules[0].scheduleid;
       }
    };
    if (scheduleID == "default") {
      this.launch("default");
    } else {
      if (scheduleID!== "") {
       $.ajax({
          "url" : NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/schedules/' + scheduleID,
          "type" : "GET",
          "xhrFields": {
             withCredentials: true
          },
          "dataType" : "json",
          "contentType" : "application/json",
          "processData" : false,
          "success" : function(data) {
              if ($.isArray(data)) {
                data = data[0];
              };
              // if (typeof data.pdprofileid != "undefined") {  - not sure why we are checking the pd Profile id in the schedules API
              if (typeof this.props.entity.pdprofileid != "undefined" ) {
               $.ajax({
                  "url" : NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/proximitydimming/' + this.props.entity.pdprofileid,
                  "type" : "GET",
                  "xhrFields": {
                     withCredentials: true
                  },
                  "dataType" : "json",
                  "contentType" : "application/json",
                  "processData" : false,
                  "success" : function(pddata) {
                      data.pdprofile = pddata;

                      if(this.props.entity.etdhprofileid !== undefined ||this.props.entity.etdhprofileid !== ""  ){
                          $.ajax({
                              url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/daylightharvesting/'+ this.props.entity.etdhprofileid,
                              data : '',
                              method : 'GET',
                              "xhrFields": {
                                  withCredentials: true
                              },
                              dataType : 'json',
                              success : function(etDhProfileData){
                                  console.log(" data from the ajax call for ETDHprofileid", data)
                                  // var dhProfileData = data;
                                  data.etDhProfile = etDhProfileData;
                                  this.launch(data);

                              }.bind(that),
                              error : function(){
                              }
                          });
                      }else {
                          this.launch(data);
                      }
                  }.bind(that),
                  "error" : function(jqXHR, textStatus, errorThrown) {
                    noty({type:"error", text: "Could not retrieve PD profile: " + textStatus});
                  }
                }); 
              } else {
                this.launch(data);
              }
          }.bind(that),
          "error" : function(jqXHR, textStatus, errorThrown) {
            noty({type:"error", text: "Could not retrieve schedule: " + textStatus});
          }
        });
     };
    };
  },

  componentWillReceiveProps: function(nextProps){
//    if (this.props.entity != nextProps.entity){
      scheduler.destroy($("#schedule"));
//    };
  },

  componentDidUpdate: function(){
    if (this.props.show) {
        $(".sched-display").fadeIn('fast');
//        if ($("#schedule").children().length == 0) {
          this.getSchedule();
//        }
    } else {
        $(".sched-display").fadeOut('fast');
    }
  },

  componentWillUnmount: function(){
    scheduler.destroy($("#schedule"));
  },

  render() {
    return (
      <div className="sched-display">
        <div id="sched-heading"></div>
        <div id="schedule" style={{marginBottom:"60px"}}></div>
        <div style={{position:"absolute",bottom:"10px",right:"35px"}}>
          <button className="ns-cancel-btn" onClick={this.handleClose}><b>Close</b></button>
        </div>
      </div>
    );
  }
});



module.exports = ScheduleView;
