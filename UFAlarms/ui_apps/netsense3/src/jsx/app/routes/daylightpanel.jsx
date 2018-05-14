import classNames from 'classnames';
import helpers from 'global/utils/helpers';
import Daylightlist from 'components/daylight/daylightlist';
import Daylightmap from 'components/daylight/daylightmap';
import DaylightDetail from 'components/daylight/daylightdetail';

import Header from 'common/headernew';


var Body = React.createClass({
  getInitialState: function(){
    return {
      daylights: null,
      groups: null,
      nodes: null,
      site: null,
      showDaylightDetail: false,
      daylightID: "-1"
    }
  },

  calcHeight: function(pct, extra) {
    var h = (window&&window.document)?(($(window).height() - 75) * (pct/100) + extra):500;
    return h;
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
      url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/daylightharvesting',
      data : '',
      xhrFields: {
         withCredentials: true
      },
      method : 'GET',
      dataType : 'json',
      success : function(data){
        console.log("ajax success: " + JSON.stringify(data));
        if (data == "") {
          that.setState({daylights:[]});
        } else {
          that.setState({
            daylights: data.map(function(daylight, index) {
                if (daylight.sites.length == 1 && $.isEmptyObject(daylight.sites[0])) {
                  daylight.sites = [];
                };
                if (daylight.groups.length == 1 && $.isEmptyObject(daylight.groups[0])) {
                  daylight.groups = [];
                };
                if (daylight.nodes.length == 1 && $.isEmptyObject(daylight.nodes[0])) {
                  daylight.nodes = [];
                };
                daylight.idx = index;
                return daylight;
              })
          })
        }

        },
      error : function(jqXHR, status, error){
        console.log("ajax failure (daylights): " + status + " - " + error);
        $("#loadingmsg").html("Cannot retrieve Daylight Harvesting profiles.  API reported error: " + error);
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
        console.log("ajax success: " + JSON.stringify(data));
        $("#loadingmsg").html("Generating display.");
        that.setState({
          nodes: data.map(function(node, index) {
            if (typeof node.level == "undefined") {
              node.level = "1";
            };
            node.idx = index;
            return node;
            })
        });
      },
      error : function(){
        console.log("ajax failure");
        that.setState({
          nodes: helpers.getNodeList().map(function(node, index) {
            node.idx = index;
            return node;
         })});

      }
    });


    $.ajax({
        url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/groups',
        data : '',
        method : 'GET',
        "xhrFields": {
           withCredentials: true
        },
        dataType : 'json',
        success : function(data){
            console.log("ajax success: " + JSON.stringify(data));
          that.setState({
            groups: data.filter(function(group, index) {
              return group.type == "lighting";
            }).map(function(group, index) {
              group.idx = index;
              return group;
            })
          })
        },
        error : function(jqXHR, status, error){
          $("#loadingmsg").html("Cannot retrieve Groups.  API reported error: " + error)
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

  checkDuplicate: function(daylight_info){
        var duplicate = false;
        for(var i = 0, len = this.state.daylights.length; i < len; i++) {
          console.log(JSON.stringify(this.state.daylights[i].name) + JSON.stringify(daylight_info.name));
          if( (this.state.daylights[ i ].name === daylight_info.name) && !(this.state.daylights[ i ].etdhprofileid === daylight_info.etdhprofileid))
            duplicate = true;
        }
        if(duplicate){
          noty({type:"error", text:'Daylight Harvesting profile "' + daylight_info.name + '" already exists.'});
        }
        return duplicate;
  },

  componentDidMount: function() {
    this.init();
    var that = this;

    ReactBootstrap.Dispatcher.on("Daylightlist.select",function(daylightID){
      NSN.daylightID = daylightID;
      sessionStorage.setItem("daylightID", NSN.daylightID);
      that.setState({ 
        "daylightID": daylightID, 
        showDaylightDetail: true
      });
    });

    ReactBootstrap.Dispatcher.on("Daylightlist.add", function(){
      NSN.daylightID = "0";
      sessionStorage.setItem("daylightID", NSN.daylightID);
      that.setState({ 
        "daylightID": "0" ,
        showDaylightDetail: true
      });
    });

    ReactBootstrap.Dispatcher.on("Daylightform.reset", function(){
      that.forceUpdate();
    });
    
    ReactBootstrap.Dispatcher.on("Daylightform.delete", function(daylight_info){
      var idx = helpers.get_idx(that.state.daylights, daylight_info, 'etdhprofileid');
      console.log("Deleting daylight (idx:" + idx + "; id:" + daylight_info.etdhprofileid + ")");
      delete daylight_info.idx;
      $.ajax({
        url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/daylightharvesting/' + daylight_info.etdhprofileid,
        "type" : "DELETE",
        "xhrFields": {
           withCredentials: true
        },
        "data" : JSON.stringify(daylight_info),
        "dataType" : "json",
        "contentType" : "application/json",
        "processData" : false,
        "success" : function(data) {
          var newState = React.addons.update(this.state, { daylights: { $splice: [[idx, 1]] }, daylightID: { $set : "-1"}});
          noty({type:"success", text:'Daylight Harvesting profile "' + daylight_info.name + '" deleted.'})
          NSN.daylightID = "-1";
          sessionStorage.setItem("daylightID", NSN.daylightID);
          ReactBootstrap.Dispatcher.emit("Daylightform.delete.success", daylight_info.etdhprofileid);
          this.setState(newState);
        }.bind(that),
        "error" : function() {
          noty({type:"error", text:'Could not delete Daylight Harvesting profile.'});
        }
      })
      that.setState({
        showDaylightDetail: false
      });
    });

    ReactBootstrap.Dispatcher.on("Daylightform.save",function(daylight_info) {
      var newState = {};

       if (daylight_info.etdhprofileid == "-1" || daylight_info.etdhprofileid == "0") {
         if(!that.checkDuplicate(daylight_info)){
        console.log("Adding daylight: " + daylight_info.name);
        delete daylight_info.idx;
        delete daylight_info.etdhprofileid;

          var daylight_info_copy = Object.assign({},daylight_info);
          delete daylight_info_copy.triggers;
          delete daylight_info_copy.sites;
          delete daylight_info_copy.groups;
          delete daylight_info_copy.nodes;
          $.ajax({
            url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/daylightharvesting',
            "type" : "POST",
            "data" : JSON.stringify(daylight_info_copy),
            "xhrFields": {
               withCredentials: true
            },
            "dataType" : "json",
            "contentType": "application/json",
            "processData": false,
            "success" : function(data) {
              console.log("Response from Add Profile: " + JSON.stringify(data));
              NSN.daylightID = data.etdhprofileid;
              daylight_info.etdhprofileid = data.etdhprofileid;
              sessionStorage.setItem("daylightID", NSN.daylightID);
              noty({type:"success", text:'Daylight Harvesting profile "' + data.name + '" added.'});
              ReactBootstrap.Dispatcher.emit('Daylightform.add.success', data);
              var newState = React.addons.update(this.state, { daylights: { $push: [data] }, daylightID: {$set: data.etdhprofileid } });
              this.setState(newState);
            }.bind(that),
            "error" : function(jqXHR, status, error) {

              console.log("ajax failure (adding daylightharvesting): " + status + " - " + error);

              if (jqXHR.status == 400) {
                noty({ type: "error", text: JSON.parse(jqXHR.responseText).message });
              } else {
                noty({
                  type: "error", text: 'Could not add Daylight Harvesting profile.' + 'Return status: ' + status
                });
              }

            }
          });
          that.setState({
            showDaylightDetail: false
          });
       }
      } else if(!that.checkDuplicate(daylight_info)){
        var idx = helpers.get_idx(that.state.daylights, daylight_info, 'etdhprofileid');
        delete daylight_info.idx;
        var triggers = daylight_info.triggers;
        var actualNodesList = daylight_info.nodes;
        delete daylight_info.triggers;
        delete daylight_info.nodes;
        delete daylight_info.sites;
        delete daylight_info.groups;
        $.ajax({
          url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/daylightharvesting/' + daylight_info.etdhprofileid,
          "type" : "PUT",
          "xhrFields": {
             withCredentials: true
          },
          "data" : JSON.stringify(daylight_info),
          "dataType" : "json",
          "contentType" : "application/json",
          "processData" : false,
          "success" : function(data) {
            noty({type:"success", text:'Daylight Harvesting profile "' + data.name + '" updated.'});
            ReactBootstrap.Dispatcher.emit('Daylightform.update.success', data);
            
            var addSelectedTriggers = [];
            for(var i=0;i<triggers.length;i++){
              addSelectedTriggers.push(triggers[i].nodeid);
            }
            var addPayload = {
                nodeList: addSelectedTriggers,
            }

            var removeSelectedTriggers = [];
            for(var i=0;i<actualNodesList.length;i++){
              removeSelectedTriggers.push(actualNodesList[i].nodeid);
            }
            var removePayload = {
                nodeList: removeSelectedTriggers,
            }
    
            newState = React.addons.update(this.state, { daylights: { [idx]: { $set: data } }});
            this.setState(newState);

            if((removePayload['nodeList'].length > 0) || (addPayload['nodeList'].length > 0)){
              $.ajax({
                url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/daylightharvesting/' + daylight_info.etdhprofileid + '/triggers',
                type: "POST",
                "xhrFields": {
                   withCredentials: true
                },
                "data" : JSON.stringify(removePayload),
                "dataType" : "json",
                "contentType" : "application/json",
                "processData" : false,
                "success" : function(data1) {

                  $.ajax({
                    url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/daylightharvesting/' + daylight_info.etdhprofileid + '/triggers',
                    type: "PUT",
                    "xhrFields": {
                       withCredentials: true
                    },
                    "data" : JSON.stringify(addPayload),
                    "dataType" : "json",
                    "contentType" : "application/json",
                    "processData" : false,
                    "success" : function(data2) {
                      var triggerNodes = [];
                      for(var i=0;i<data1.length;i++){
                        triggerNodes.push({"nodeid":data1[i]});
                      }
                      noty({type:"success", text:'Daylight Harvesting profile trigger nodes"' + data.name + '" updated.'});
                      ReactBootstrap.Dispatcher.emit('Daylightform.update.success', data);
                      data.triggers = triggerNodes;
                      newState = React.addons.update(this.state, { daylights: { [idx]: { $set: data } }});
                      this.setState(newState);
                      },
                    "error" : function() {
                      noty({type:"error", text:'Could not add trigger nodes.'});
                      }
                  }) 

                  },
                "error" : function() {
                  noty({type:"error", text:'Could not remove trigger nodes.'});
                  }
              });
            } 
          }.bind(that),
          "error" : function(jqXHR, status, error) {
            console.log("ajax failure (updating daylightharvesting): " + status + " - " + error);

            if (jqXHR.status == 400) {
              noty({ type: "error", text: JSON.parse(jqXHR.responseText).message });
            } else {
              noty({
                type: "error", text: 'Could not update Daylight Harvesting profile.' + 'Return status: ' + status
              });
            }
          }
        });
        that.setState({
          showDaylightDetail: false
        });
      }
    })

  },

  componentWillUnmount: function() {
    ReactBootstrap.Dispatcher.removeAllListeners("Daylightlist.select");
    ReactBootstrap.Dispatcher.removeAllListeners("Daylightlist.add");
    ReactBootstrap.Dispatcher.removeAllListeners("Daylightform.save");
    ReactBootstrap.Dispatcher.removeAllListeners("Daylightform.reset");
    ReactBootstrap.Dispatcher.removeAllListeners("Daylightform.delete");
  },


  hideDaylightDetail() {
    this.setState({
      showDaylightDetail: false
    });
  },

  render() {
    var hstyle = {height:helpers.calcHeight(90, 0)+"px !important"};
    if (this.state.daylights && this.state.nodes && this.state.site) {
      var Subpanels = (
            <div className="netsense-center-panel">
              <Col md={12} lg={12} >
                <PanelContainer>
                  <Panel>
                    <PanelBody style={hstyle}>
                      <Daylightlist daylights={this.state.daylights} daylightID={this.state.daylightID} />
                      <DaylightDetail show={this.state.showDaylightDetail}  hide={this.hideDaylightDetail} daylights={this.state.daylights} daylightID={this.state.daylightID} />
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