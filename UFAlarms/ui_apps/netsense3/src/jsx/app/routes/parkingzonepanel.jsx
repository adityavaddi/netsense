import classNames from 'classnames';
import {
    State,
    Navigation
} from 'react-router';
import helpers from 'global/utils/helpers';
import ImageView from 'components/imageview';
import Parkingzonelist from 'components/parkingzones/parkingzonelist';
import ParkingzoneDetail from 'components/parkingzones/parkingzonedetail';
import Parkingzonemap from 'components/parkingzones/parkingzonemap';

import Header from 'common/headernew';

var Body = React.createClass({
  getInitialState: function(){
    return {
      parkingroups: null,
      parkingzones: null,
      activeFilter: 'active',
      showimage: false,
      selectedvideonode: null,
      site: null,
      parkingzoneID: "-1",
      ui: {detail: "hidden",  // one of "unpinned", "pinned", "hidden"
           prevDetail: "pinned",   // one of "unpinned", "pinned", "hidden"
           list: "normal",  // "normal" or "expanded"
           map: "normal"  // "normal" or "expanded"
       },
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
              left: $("#parkingzone-detail-panel").position().left + "px",
              top: $("#parkingzone-detail-panel").position().top + "px",
              width: $("#parkingzone-detail-panel").width() + "px",
              height: $("#parkingzone-detail-panel").height() + "px"
              };
      };
      this.setState({ui: $.extend({}, this.state.ui, {detail: "hidden", prevDetail: this.state.ui.detail})});
  },

  showDetail: function() {
      this.setState({ui: $.extend({}, this.state.ui, {detail: this.state.ui.prevDetail, prevDetail: "hidden"})});
  },

  pinDetail: function() {
      this.detailUnpinnedPosition = {
          left: $("#parkingzone-detail-panel").position().left + "px",
          top: $("#parkingzone-detail-panel").position().top + "px",
          width: $("#parkingzone-detail-panel").width() + "px",
          height: $("#parkingzone-detail-panel").height() + "px"
      };
      this.setState({ui: $.extend({}, this.state.ui, {detail: "pinned", prevDetail: "unpinned"})});
  },

  unpinDetail: function() {
      this.setState({ui: $.extend({}, this.state.ui, {detail: "unpinned", prevDetail: "pinned"})});
  },

  applyFilter: function(zone) {
    switch (this.state.activeFilter) {
      case "active":
        return typeof zone.active == "undefined" || zone.active;
      case "inactive":
        return typeof zone.active != "undefined" && !zone.active;
      case "all":
        return true;
    }
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
          url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/parkinggroups'
                  + '?t=' + (new Date()).getTime(), 
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
                      parkinggroup.numzones = 0;
                    } else {
                      parkinggroup.numzones = parkinggroup.parkingzones.split(" ").length;
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
          url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/parkingzones'
                + '?t=' + (new Date()).getTime(), 
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
                for (var i=0; i<data.length; i++) {
                  data[i].active = data[i].active?"true":"false";
                  data[i].available_spaces = Math.max(0, data[i].available_spaces);
                }
                that.setState({parkingzones: data})
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

    ReactBootstrap.Dispatcher.on('Parkingzonemap.selectZone', function(parkingzoneid) {
      NSN.parkingzoneID = parkingzoneid;
      ReactBootstrap.Dispatcher.emit('Parkingzonelist.selectrow', parkingzoneid)
      that.setState({
        selected_nodes: [],
        parkingzoneID: parkingzoneid
      });
    });

    ReactBootstrap.Dispatcher.on("Parkingzonelist.select",function(parkingzoneID){
      NSN.parkingzoneID = parkingzoneID;
      sessionStorage.setItem("parkingzoneID", NSN.parkingzoneID);
      that.setState({
        parkingzoneID:parkingzoneID, 
        selected_nodes: []
      });
    });

    ReactBootstrap.Dispatcher.on("Parkingzonelist.add", function(){
      NSN.parkingzoneID = "0";
      sessionStorage.setItem("parkingzoneID", NSN.parkingzoneID);      
      that.setState({"parkingzoneID":"0"});
    });

    ReactBootstrap.Dispatcher.on("Parkingzonelist.changeFilter", function(value){     
      that.setState({activeFilter:value});
    });

    ReactBootstrap.Dispatcher.on("Parkingzoneform.reset", function(){
      that.forceUpdate();
    });

    ReactBootstrap.Dispatcher.on('Parkingzoneform.image', function (switchto, selectednode) {
        that.setState({
            showimage: switchto == 'open',
            selectedvideonode: typeof selectednode == "undefined" ? null : selectednode
        });
    });

    ReactBootstrap.Dispatcher.on("Parkingzoneform.delete", function(parkingzone_info){
      var idx = helpers.get_idx(that.state.parkingzones, parkingzone_info, 'parkingzoneid');
      console.log("Deleting parkingzone (idx:" + idx + "; id:" + parkingzone_info.parkingzoneid + ")");
      $.ajax({
        url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/parkingzones/' + parkingzone_info.parkingzoneid, 
        "type" : "DELETE",
        "xhrFields": {
           withCredentials: true
        },
        "data" : JSON.stringify(parkingzone_info),
        "dataType" : "json",
        "contentType" : "application/json",
        "processData" : false,
        "success" : function(data) {
          var newState = React.addons.update(this.state, { parkingzones: { $splice: [[idx, 1]] }, parkingzoneID: { $set : "-1"}});
          noty({type:"success", text:'Parking Zone "' + parkingzone_info.name + '" deleted.'})
          NSN.parkingzoneID = "-1";
          sessionStorage.setItem("parkingzoneID", NSN.parkingzoneID);
          ReactBootstrap.Dispatcher.emit("Parkingzoneform.delete.success", parkingzone_info.parkingzoneid);
          this.setState(newState);
        }.bind(that),
        "error" : function() {
          noty({type:"error", text:'Could not delete Parking Zone.'});
        }
      }) 
    });

    ReactBootstrap.Dispatcher.on("Parkingzoneform.save",function(parkingzone_info) {
      var newState = {};
      if (parkingzone_info.parkingzoneid == "-1" || parkingzone_info.parkingzoneid == "0") {
        console.log("Adding parkingzone: " + parkingzone_info.name);
        var duplicate = false;
        for(var i = 0, len = that.state.parkingzones.length; i < len; i++) {
          console.log(JSON.stringify(that.state.parkingzones[i].name) + JSON.stringify(parkingzone_info.name));
          if( (that.state.parkingzones[ i ].name === parkingzone_info.name))
            duplicate = true;
        }
        if(duplicate){
          noty({type:"error", text:'Parking Zone "' + parkingzone_info.name + '" already exists.'});
        }  
        else{
          delete parkingzone_info.parkingzoneid;
          $.ajax({
            url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/parkingzones', 
            "type" : "POST",
            "data" : JSON.stringify(parkingzone_info),
            "xhrFields": {
               withCredentials: true
            },
            "dataType" : "json",
            "contentType": "application/json",
            "processData": false,
            "success" : function(data) {
              console.log("Response from Add Parking Zone: " + JSON.stringify(data));    
              NSN.parkingzoneID = data.parkingzoneid;
              sessionStorage.setItem("parkingzoneID", NSN.parkingzoneID);
              noty({type:"success", text:'Parking Zone "' + data.name + '" added.'});
              ReactBootstrap.Dispatcher.emit('Parkingzoneform.add.success', data);
              var newState = React.addons.update(this.state, { parkingzones: { $push: [data] }, parkingzoneID: {$set: data.parkingzoneid } });
              this.setState(newState);
            }.bind(that),
            "error" : function() {
              noty({type:"error", text:"Could not add Parking Zone."});
            }
          })
        }
      } else {
        var idx = helpers.get_idx(that.state.parkingzones, parkingzone_info, 'parkingzoneid');
        delete parkingzone_info.site;
        $.ajax({
          url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/parkingzones/' + parkingzone_info.parkingzoneid, 
          "type" : "POST",
          "xhrFields": {
             withCredentials: true
          },
          "data" : JSON.stringify(parkingzone_info),
          "dataType" : "json",
          "contentType" : "application/json",
          "processData" : false,
          "success" : function(data) {
            noty({type:"success", text:'Parking Zone "' + data.name + '" updated.'})
            ReactBootstrap.Dispatcher.emit('Parkingzoneform.update.success', data);
            newState = React.addons.update(this.state, { parkingzones: { [idx]: { $set: data } }});
            this.setState(newState);
          }.bind(that),
          "error" : function() {
            noty({type:"error", text:'Could not update Parking Zone.'});
          }
        })
      }
    });

    ReactBootstrap.Dispatcher.on('Parkingzonelist.toggleDetail', function () {
        if (that.state.ui.detail == "hidden") {
            that.showDetail();
        } else {
            that.hideDetail();
        }
    });

    ReactBootstrap.Dispatcher.on('Parkingzonedetail.togglePin', function () {
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
    ReactBootstrap.Dispatcher.removeAllListeners("Parkingzonemap.selectZone");
    ReactBootstrap.Dispatcher.removeAllListeners("Parkingzonelist.select");
    ReactBootstrap.Dispatcher.removeAllListeners("Parkingzonelist.add");
    ReactBootstrap.Dispatcher.removeAllListeners("Parkingzonelist.changeFilter");
    ReactBootstrap.Dispatcher.removeAllListeners("Parkingzoneform.save");
    ReactBootstrap.Dispatcher.removeAllListeners("Parkingzoneform.reset");
    ReactBootstrap.Dispatcher.removeAllListeners("Parkingzoneform.delete");
    ReactBootstrap.Dispatcher.removeAllListeners('Parkingzoneform.image');
    ReactBootstrap.Dispatcher.removeAllListeners("Parkingzonelist.toggleDetail");
    ReactBootstrap.Dispatcher.removeAllListeners("Parkingzonedetail.togglePin");
  },

  render() {
    var that = this;
    var hstyle = {height: helpers.calcHeight(90, 0)+"px !important"};
    if (this.state.parkingzones && this.state.parkinggroups && this.state.nodes && this.state.site) {
      var listStyle = {width:"33%",height:"100%",overflow:"hidden",backgroundColor:"#FFF",position:"absolute",zIndex:"300",left:"0px",top:"0px",boxShadow:"2px 0px 2px 0px #CCC"};
      switch (this.state.ui.detail) {
          case "unpinned": 
              var detailStyle = {opacity:"0.9",position:"absolute",zIndex:"500",backgroundColor:"#FFF",overflowY:"hidden",overflowX:"hidden",border:"1px solid #EEE"};
              $.extend(detailStyle, this.detailUnpinnedPosition);
              var mapStyle = {width:"67%",height:"100%",position:"absolute",top:"0px",left:"33%",zIndex:"100"};
              $("#parkingzone-detail-panel").draggable("enable").resizable("enable");
              break;
          case "pinned":
              detailStyle = {position:"absolute", zIndex:"200", backgroundColor:"#FFF", left:"33%", width:"25%", height:"100%"};
              mapStyle = {width:"42%",height:"100%",position:"absolute",top:"0px",left:"58%",zIndex:"100"};
              $("#parkingzone-detail-panel").draggable("disable").resizable("disable");
              break;
          case "hidden":
              detailStyle = {position:"absolute", left:"-3000px"};
              mapStyle = {width:"67%",height:"100%",position:"absolute",top:"0px",left:"33%",zIndex:"100"}; 
              break;
      };
      var parkingzones = this.state.parkingzones.filter(function(zone) {
          switch (that.state.activeFilter) {
            case "active":
              return typeof zone.active == "undefined" || zone.active;
            case "inactive":
              return typeof zone.active != "undefined" && !zone.active;
            case "all":
              return true;
          }
      });

      return (
        <Container id='body' className="parkingZone-body" style={{backgroundColor:"#FFF",marginTop:"0px !important"}}>
          <div id="parkingzone-list-panel" data-state="closed" style={listStyle}>
               <Parkingzonelist minmax="max" parkinggroups={this.state.parkinggroups} parkingzones={parkingzones} parkingzoneID={this.state.parkingzoneID} detail_state={this.state.ui.detail} activeFilter={this.state.activeFilter} ui_state={this.state.ui} />
          </div>
          <div id="parkingzone-detail-panel" style={detailStyle}>
                <ParkingzoneDetail parkinggroups={this.state.parkinggroups} parkingzones={parkingzones} parkingzoneID={this.state.parkingzoneID} detail_state={this.state.ui.detail} activeFilter={this.state.activeFilter} />
          </div>
          <div id="parkingzone-map-panel" data-state="closed" style={mapStyle}>
                <Parkingzonemap minmax="max" parkinggroups={this.state.parkinggroups} parkingzones={parkingzones}  nodes={this.state.nodes} site={this.state.site} activeFilter={this.state.activeFilter} />
          </div>
          <ImageView context="parkingzone" show={this.state.showimage} node={this.state.selectedvideonode} />
        </Container>
      );
    };
    var forSite = (NSN && NSN.siteName && NSN.siteName != '') ? (' for ' + NSN.siteName) : '';
    return (
      <Container id='body'>
        <Grid>
          <Row>
            <Col sm={12}>
              <PanelContainer>
                <Panel>
                  <PanelBody style={hstyle}>
                    <h2 id="loadingmsg" style={{ paddingTop: "16%", textAlign: "center" }}>Loading Parking Zone information{forSite}.</h2>
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
