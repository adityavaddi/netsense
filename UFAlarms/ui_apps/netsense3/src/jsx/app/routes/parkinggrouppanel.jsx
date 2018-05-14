import classNames from 'classnames';
import {
    State,
    Navigation
} from 'react-router';
import helpers from 'global/utils/helpers';
import Parkinggrouplist from 'components/parkinggroups/parkinggrouplist';
import ParkinggroupDetail from 'components/parkinggroups/parkinggroupdetail';
import Parkinggroupmap from 'components/parkinggroups/parkinggroupmap';

import Header from 'common/headernew';

var Body = React.createClass({
  getInitialState: function(){
    return {
      parkinggroups: null,
      parkingzones: null,
      nodes: null,
      site: null,
      parkinggroupID: "-1",
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
              left: $("#parkinggroup-detail-panel").position().left + "px",
              top: $("#parkinggroup-detail-panel").position().top + "px",
              width: $("#parkinggroup-detail-panel").width() + "px",
              height: $("#parkinggroup-detail-panel").height() + "px"
              };
      };
      this.setState({ui: $.extend({}, this.state.ui, {detail: "hidden", prevDetail: this.state.ui.detail})});
  },

  showDetail: function() {
      this.setState({ui: $.extend({}, this.state.ui, {detail: this.state.ui.prevDetail, prevDetail: "hidden"})});
  },

  pinDetail: function() {
      this.detailUnpinnedPosition = {
          left: $("#parkinggroup-detail-panel").position().left + "px",
          top: $("#parkinggroup-detail-panel").position().top + "px",
          width: $("#parkinggroup-detail-panel").width() + "px",
          height: $("#parkinggroup-detail-panel").height() + "px"
      };
      this.setState({ui: $.extend({}, this.state.ui, {detail: "pinned", prevDetail: "unpinned"})});
  },

  unpinDetail: function() {
      this.setState({ui: $.extend({}, this.state.ui, {detail: "unpinned", prevDetail: "pinned"})});
  },

  init: function() {
    var that = this;
    if (NSN.customerID=="-1" && NSN.siteID=="-1") {
      $("#loadingmsg").html("Please select an Account and a Site first.")
      return;
    } else {
      if (NSN.customerID=="-1") {
        $("#loadingmsg").html("Please select an Account first.")
        return;
      } else {
        if (NSN.siteID=="-1") {
          $("#loadingmsg").html("Please select a Site first.")
          return;
        }
      }
    };
    $.ajax({
          url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/parkinggroups', 
          data : '',
          xhrFields: {
             withCredentials: true
          },
          method : 'GET',
          dataType : 'json',
          success : function(data){
              if (data == "") {
                that.setState({parkinggroups:[]});
              } else {
                that.setState({
                  parkinggroups: data.map(function(parkinggroup, index) {
                    if (typeof parkinggroup.parkingzones == "undefined" || parkinggroup.parkingzones == "") {
                      parkinggroup.zoneList = [];
                    } else {
                      parkinggroup.zoneList = parkinggroup.parkingzones.split(",");
                    }
                    return parkinggroup;
                  })
                })
              }
          },
          error : function(jqXHR, status, error){
            console.log("ajax failure (parkinggroups): " + status + " - " + error);
            $("#loadingmsg").html("Cannot retrieve Parking Groups.  API reported error: " + error);
          }
        });

    $.ajax({
          url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/parkingzones', 
          data : '',
          xhrFields: {
             withCredentials: true
          },
          method : 'GET',
          dataType : 'json',
          success : function(data){
              if (data == "") {
                that.setState({parkingzones:[]});
              } else {
                that.setState({
                  parkingzones: data.filter(function(parkingzone, index) {
                    return true; // !parkingzone.parkingzoneid.match(/\<|\>|\$|\&/);
                  })
                })
              }
          },
          error : function(jqXHR, status, error){
            console.log("ajax failure (parkingzones): " + status + " - " + error);
            $("#loadingmsg").html("Cannot retrieve Parking Zones.  API reported error: " + error);
          }
        });

    $.ajax({
      url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/minnodes',
      data : '',
      method : 'GET',
      "xhrFields": {
         withCredentials: true
      },
      dataType : 'json',
      success : function(data){
        that.setState({
          nodes: data.filter(function(node, index) {
            return (typeof node.model != "undefined") && helpers.modelType(node.model) == "Video";
            })
        });
      },
      error : function(){
        console.log("ajax failure");
      }
    });

    $.ajax({
      url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID,
      data : '',
      method : 'GET',
      "xhrFields": {
         withCredentials: true
      },
      dataType : 'json',
      success : function(data){
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
      error : function(jqXHR, status, error){
        console.log("ajax failure (site): " + status + " - " + error);
        $("#loadingmsg").html("Cannot retrieve Site information.  API reported error: " + error);
      }
    });

  },

  componentDidMount: function() {
    this.init();
    var that = this;

    ReactBootstrap.Dispatcher.on("Parkinggrouplist.select",function(parkinggroupID){
      NSN.parkinggroupID = parkinggroupID;
      sessionStorage.setItem("parkinggroupID", NSN.parkinggroupID);
      that.setState({"parkinggroupID":parkinggroupID});
    });

    ReactBootstrap.Dispatcher.on("Parkinggrouplist.add", function(){
      NSN.parkinggroupID = "0";
      sessionStorage.setItem("parkinggroupID", NSN.parkinggroupID); 
      if (that.state.ui.detail == "hidden") {
        that.setState({parkinggroupID: "0", 
                       ui: $.extend({}, that.state.ui, {detail: that.state.ui.prevDetail, prevDetail: "hidden"})});
      } else {  
        that.setState({parkinggroupID: "0"});
      };
    });

    ReactBootstrap.Dispatcher.on("Parkinggroupform.reset", function(){
      that.forceUpdate();
    });

    ReactBootstrap.Dispatcher.on("Parkinggroupform.delete", function(parkinggroup_info){
      var idx = helpers.get_idx(that.state.parkinggroups, parkinggroup_info, 'parkinggroupid');
      console.log("Deleting parkinggroup (idx:" + idx + "; id:" + parkinggroup_info.parkinggroupid + ")");
      $.ajax({
        url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/parkinggroups/' + parkinggroup_info.parkinggroupid, 
        "type" : "DELETE",
        "xhrFields": {
           withCredentials: true
        },
        "data" : JSON.stringify(parkinggroup_info),
        "dataType" : "json",
        "contentType" : "application/json",
        "processData" : false,
        "success" : function(data) {
          var newState = React.addons.update(this.state, { parkinggroups: { $splice: [[idx, 1]] }, parkinggroupID: { $set : "-1"}});
          noty({type:"success", text:'Parking Group "' + parkinggroup_info.name + '" deleted.'})
          NSN.parkinggroupID = "-1";
          sessionStorage.setItem("parkinggroupID", NSN.parkinggroupID);
          ReactBootstrap.Dispatcher.emit("Parkinggroupform.delete.success", parkinggroup_info.parkinggroupid);
          this.setState(newState);
        }.bind(that),
        "error" : function() {
          noty({type:"error", text:'Could not delete Parking Group.'});
        }
      }) 
    });

    ReactBootstrap.Dispatcher.on("Parkinggroupform.save",function(parkinggroup_info) {
      var newState = {};
      if (parkinggroup_info.parkinggroupid == "") {
        console.log("Adding parkinggroup: " + parkinggroup_info.name);
        var duplicate = false;
        for(var i = 0, len = that.state.parkinggroups.length; i < len; i++) {
          console.log(JSON.stringify(that.state.parkinggroups[i].name) + JSON.stringify(parkinggroup_info.name));
          if( (that.state.parkinggroups[ i ].name === parkinggroup_info.name))
            duplicate = true;
        }
        if(duplicate){
          noty({type:"error", text:'Parking Group "' + parkinggroup_info.name + '" already exists.'});
        }  
        else{
          delete parkinggroup_info.parkinggroupid;
          parkinggroup_info.parkingzones = parkinggroup_info.zoneList.join(",");
          $.ajax({
            url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/parkinggroups', 
            "type" : "POST",
            "data" : JSON.stringify(parkinggroup_info),
            "xhrFields": {
               withCredentials: true
            },
            "dataType" : "json",
            "contentType": "application/json",
            "processData": false,
            "success" : function(data) {
              console.log("Response from Add Parking Group: " + JSON.stringify(data));
              if (typeof data.parkingzones == "undefined" || data.parkingzones == "") {
                data.zoneList = [];
              } else {
                data.zoneList = data.parkingzones.split(",");
              }
              NSN.parkinggroupID = data.parkinggroupid;
              sessionStorage.setItem("parkinggroupID", NSN.parkinggroupID);
              noty({type:"success", text:'Parking Group "' + data.name + '" added.'});
              ReactBootstrap.Dispatcher.emit('Parkinggroupform.add.success', data);
              var newState = React.addons.update(this.state, { parkinggroups: { $push: [data] }, parkinggroupID: {$set: data.parkinggroupid } });
              this.setState(newState);
            }.bind(that),
            "error" : function() {
              noty({type:"error", text:"Could not add Parking Group."});
            }
          })
        }
      } else {
        var idx = helpers.get_idx(that.state.parkinggroups, parkinggroup_info, 'parkinggroupid');
        delete parkinggroup_info.site;
        parkinggroup_info.parkingzones = parkinggroup_info.zoneList.join(",");
        $.ajax({
          url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/parkinggroups/' + parkinggroup_info.parkinggroupid, 
          "type" : "POST",
          "xhrFields": {
             withCredentials: true
          },
          "data" : JSON.stringify(parkinggroup_info),
          "dataType" : "json",
          "contentType" : "application/json",
          "processData" : false,
          "success" : function(data) {
            if (typeof data.parkingzones == "undefined" || data.parkingzones == "") {
              data.zoneList = [];
            } else {
              data.zoneList = data.parkingzones.split(",");
            } 
            noty({type:"success", text:'Parking Group "' + data.name + '" updated.'})
            ReactBootstrap.Dispatcher.emit('Parkinggroupform.update.success', data);
            newState = React.addons.update(this.state, { parkinggroups: { [idx]: { $set: data } }});
            this.setState(newState);
          }.bind(that),
          "error" : function() {
            noty({type:"error", text:'Could not update Parking Group.'});
          }
        })
      }
    })

    ReactBootstrap.Dispatcher.on('Parkinggrouplist.toggleDetail', function () {
        if (that.state.ui.detail == "hidden") {
            that.showDetail();
        } else {
            that.hideDetail();
        }
    });

    ReactBootstrap.Dispatcher.on('Parkinggroupdetail.togglePin', function () {
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

  componentWillUnmount: function() {
    ReactBootstrap.Dispatcher.removeAllListeners("Parkinggrouplist.select");
    ReactBootstrap.Dispatcher.removeAllListeners("Parkinggrouplist.add");
    ReactBootstrap.Dispatcher.removeAllListeners("Parkinggroupform.save");
    ReactBootstrap.Dispatcher.removeAllListeners("Parkinggroupform.reset");
    ReactBootstrap.Dispatcher.removeAllListeners("Parkinggroupform.delete");
    ReactBootstrap.Dispatcher.removeAllListeners("Parkinggrouplist.toggleDetail");
    ReactBootstrap.Dispatcher.removeAllListeners("Parkinggroupdetail.togglePin");
  },

  render() {
    var hstyle = {height:helpers.calcHeight(90, 0)+"px !important"};
    if (this.state.parkinggroups && this.state.parkingzones && this.state.nodes && this.state.site) {
      var listStyle = {width:"33%",height:"100%",overflow:"hidden",backgroundColor:"#FFF",position:"absolute",zIndex:"300",left:"0px",top:"0px",boxShadow:"2px 0px 2px 0px #CCC"};
      switch (this.state.ui.detail) {
          case "unpinned": 
              var detailStyle = {opacity:"0.9",position:"absolute",zIndex:"500",backgroundColor:"#FFF",overflowY:"hidden",overflowX:"hidden",border:"1px solid #EEE"};
              $.extend(detailStyle, this.detailUnpinnedPosition);
              var mapStyle = {width:"67%",height:"100%",position:"absolute",top:"0px",left:"33%",zIndex:"100"};
              $("#parkinggroup-detail-panel").draggable("enable").resizable("enable");
              break;
          case "pinned":
              detailStyle = {position:"absolute", zIndex:"200", backgroundColor:"#FFF", left:"33%", width:"25%", height:"100%"};
              mapStyle = {width:"42%",height:"100%",position:"absolute",top:"0px",left:"58%",zIndex:"100"};
              $("#parkinggroup-detail-panel").draggable("disable").resizable("disable");
              break;
          case "hidden":
              detailStyle = {position:"absolute", left:"-3000px"};
              mapStyle = {width:"67%",height:"100%",position:"absolute",top:"0px",left:"33%",zIndex:"100"}; 
              break;
      };
      return (
        <Container id='body' className="parkinggroup-body" style={{backgroundColor:"#FFF",marginTop:"0px !important"}}>
          <div id="parkinggroup-list-panel" data-state="closed" style={listStyle}>
               <Parkinggrouplist minmax="max" parkinggroups={this.state.parkinggroups} parkingzones={this.state.parkingzones} parkinggroupID={this.state.parkinggroupID} detail_state={this.state.ui.detail} ui_state={this.state.ui}/>
          </div>
          <div id="parkinggroup-detail-panel" style={detailStyle}>
                <ParkinggroupDetail parkinggroups={this.state.parkinggroups} parkingzones={this.state.parkingzones} parkinggroupID={this.state.parkinggroupID} detail_state={this.state.ui.detail}/>
          </div>
          <div id="parkinggroup-map-panel" data-state="closed" style={mapStyle}>
                <Parkinggroupmap minmax="max" parkinggroups={this.state.parkinggroups} parkingzones={this.state.parkingzones}  nodes={this.state.nodes} site={this.state.site} />
          </div>
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
                    <h2 id="loadingmsg" style={{paddingTop:"16%",textAlign:"center"}}>Loading...</h2>
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