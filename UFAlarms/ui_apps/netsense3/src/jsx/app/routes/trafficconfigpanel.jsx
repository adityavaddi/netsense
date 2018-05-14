import classNames from 'classnames';

import helpers from 'global/utils/helpers';
import ImageView from 'components/imageview';
import Trafficconfiglist from 'components/trafficconfig/trafficconfiglist';
import Trafficconfigmap from 'components/trafficconfig/trafficconfigmap';
import Trafficconfigdetail from 'components/trafficconfig/trafficconfigdetail';

import Header from 'common/headernew';

import {
  State,
  Navigation
} from 'react-router';

var Body = React.createClass({
  getInitialState: function () {
    return {
      trafficconfigs: null,
      nodes: null,
      showimage: false,
      selectedvideonode: null,
      site: null,
      filterevent: "All",
      trafficconfigID: NSN.trafficconfigID || "-1",
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
              left: $("#trafficconfig-detail-panel").position().left + "px",
              top: $("#trafficconfig-detail-panel").position().top + "px",
              width: $("#trafficconfig-detail-panel").width() + "px",
              height: $("#trafficconfig-detail-panel").height() + "px"
              };
      };
      this.setState({ui: $.extend({}, this.state.ui, {detail: "hidden", prevDetail: this.state.ui.detail})});
  },

  showDetail: function() {
      this.setState({ui: $.extend({}, this.state.ui, {detail: this.state.ui.prevDetail, prevDetail: "hidden"})});
  },

  pinDetail: function() {
      this.detailUnpinnedPosition = {
          left: $("#trafficconfig-detail-panel").position().left + "px",
          top: $("#trafficconfig-detail-panel").position().top + "px",
          width: $("#trafficconfig-detail-panel").width() + "px",
          height: $("#trafficconfig-detail-panel").height() + "px"
      };
      this.setState({ui: $.extend({}, this.state.ui, {detail: "pinned", prevDetail: "unpinned"})});
  },

  unpinDetail: function() {
      this.setState({ui: $.extend({}, this.state.ui, {detail: "unpinned", prevDetail: "pinned"})});
  },


  calcHeight: function (pct, extra) {
    var h = (window && window.document) ? (($(window).height() - 75) * (pct / 100) + extra) : 500;
    return h;
  },

  getTrafficconfig: function () {
    for (var i = 0; i < this.state.trafficconfigs.length; i++) {
      if (this.state.trafficconfigs[i].trafficconfigid == this.state.trafficconfigID) {
        return (this.state.trafficconfigs[i]);
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

    var filterObject = {};

    var payload = filterObject;

    $.ajax({
      url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/traffic/' + 'config',
      data: JSON.stringify(payload),
      method: 'POST',
      "xhrFields": {
        withCredentials: true
      },
      "contentType": "application/json",
      dataType: 'json',
      success: function (data1) {
        if (data1.errors) {
          $("#loadingmsg").html("Cannot get trafficconfigs.  API returned error.")
        } else {
          console.log("ajax success: " + JSON.stringify(data1));

          $.ajax({
            url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/traffic',
            data: JSON.stringify(payload),
            method: 'POST',
            "xhrFields": {
              withCredentials: true
            },
            "contentType": "application/json",
            dataType: 'json',
            success: function (data2) {
              that.setState({
                trafficconfigs: data1.map(function (trafficconfig, index) {
                  trafficconfig.idx = index;
                  trafficconfig.trafficconfigid = trafficconfig.eventid;
                  trafficconfig.roiname = trafficconfig.roi.name || "";
                  trafficconfig.nodeid = trafficconfig.nodeid || "";
                  trafficconfig.roiid = trafficconfig.roi.roiid || "";
                  trafficconfig.type = trafficconfig.type || "";
                  var count = "";
                  data2.map(function (traffic) {
                    if ((traffic.trafficdetectioneventid == trafficconfig.eventid)) {
                      count = traffic.count;
                    }
                  });

                  trafficconfig.count = count || "";
                  return trafficconfig;
                })
              })

            },
            error: function () {
              console.log("ajax failure");
            }
          });



        };
      },
      error: function () {
        $("#loadingmsg").html("Cannot get Traffic Configurations.  API call failed.")
      }
    });

    $.ajax({
      url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/minnodes',
      data: '',
      method: 'GET',
      "xhrFields": {
        withCredentials: true
      },
      dataType: 'json',
      success: function (data) {
        that.setState({
          nodes: data.filter(function (node, index) {
            return (typeof node.model != "undefined") && helpers.modelType(node.model) == "Video";
          })
        });
      },
      error: function () {
        console.log("ajax failure");
      }
    });


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

  componentDidMount: function () {
    this.init();
    var that = this;

    ReactBootstrap.Dispatcher.on("Trafficconfiglist.filter", function (filter) {
      var filterRequest = {
        "isActive": filter.isActiveFilter,
        "type": filter.filterevent
      }

      getTrafficConfigData(filterRequest);

      var filterByisActive = (data1, isActive, callback) => {
        let e;
        if (isActive == true) {
          e = _.filter(data1, (o) => {
            return o.active;
          })
        }
        if (isActive == false) {
          e = _.filter(data1, (o) => {
            return !o.active;
          })
        }
        if (isActive == null) {
          e = _.filter(data1, (o) => {
            return o;
          })
        }
        callback(e);
      }

      var filterByType = (data, type, callback) => {
        if (filterRequest.type != "All") {
          let b = _.filter(data, (o) => {
            return o.type == type;
          });
          callback(b);
        }
      }

      function getTrafficConfigData(filterRequest) {
        var payload = {};
        if ((filterRequest.isActive != "All") && (filterRequest.type != "All")) {
          payload = { "type": filterRequest.type };
        }

        $.ajax({
          url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/traffic/' + 'config',
          data: JSON.stringify(payload),
          method: 'POST',
          "xhrFields": {
            withCredentials: true
          },
          "contentType": "application/json",
          dataType: 'json',
          success: function (data1) {
            if (data1.errors) {
              $("#loadingmsg").html("Cannot get trafficconfigs.  API returned error.")
            } else {
              // console.log("ajax success: " + JSON.stringify(data1));

              if (filterRequest.isActive == "Yes") {
                filterByisActive(data1, true, (d) => {
                  data1 = d;
                });
              } else if (filterRequest.isActive == "No") {
                filterByisActive(data1, false, (d) => {
                  data1 = d;
                });
              } else {
                filterByisActive(data1, null, (d) => {
                  data1 = d;
                });
              }

              filterByType(data1, filterRequest.type, (p) => {
                data1 = p;
              })

              $.ajax({
                url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/traffic',
                data: JSON.stringify(payload),
                method: 'POST',
                "xhrFields": {
                  withCredentials: true
                },
                "contentType": "application/json",
                dataType: 'json',
                success: function (data2) {
                  var config = data1.map(function (trafficconfig, index) {
                    trafficconfig.idx = index;
                    trafficconfig.trafficconfigid = trafficconfig.eventid;
                    trafficconfig.roiname = trafficconfig.roi.name || "";
                    trafficconfig.nodeid = trafficconfig.nodeid || "";
                    trafficconfig.roiid = trafficconfig.roi.roiid || "";
                    trafficconfig.type = trafficconfig.type || "";
                    var count = "";
                    data2.map(function (traffic) {
                      if ((traffic.trafficdetectioneventid == trafficconfig.eventid)) {
                        count = traffic.count;
                      }
                    });

                    trafficconfig.count = count || "";
                    return trafficconfig;
                  });

                  ReactBootstrap.Dispatcher.emit("Trafficconfiglist.update.success", config);
                  that.setState({ trafficconfigs: config, filterevent: filterRequest.type, isActiveFilter: filterRequest.isActive });
                },
                error: function () {
                  console.log("ajax failure");
                }
              });

            };
          },
          error: function () {
            $("#loadingmsg").html("Cannot get trafficconfigs.  API call failed.")
          }
        });
      }
    });

    ReactBootstrap.Dispatcher.on('Trafficconfigmap.selectTrafficconfig', function (trafficconfigid) {
      NSN.trafficconfigID = trafficconfigid;
      ReactBootstrap.Dispatcher.emit('Trafficconfiglist.selectrow', trafficconfigid)
      that.setState({
        selected_nodes: [],
        trafficconfigID: trafficconfigid
      });
    });

    ReactBootstrap.Dispatcher.on("Trafficconfiglist.select", function (trafficconfigID) {
      NSN.trafficconfigID = trafficconfigID;
      sessionStorage.setItem("trafficconfigID", NSN.trafficconfigID);
      that.setState({
        trafficconfigID: trafficconfigID,
        selected_nodes: []
      });
    });

    ReactBootstrap.Dispatcher.on('Trafficconfigform.image', function (switchto, selectednode) {
        that.setState({
            showimage: switchto == 'open',
            selectedvideonode: typeof selectednode == "undefined" ? null : selectednode
        });
    });

    ReactBootstrap.Dispatcher.on('Trafficconfiglist.toggleDetail', function () {
        if (that.state.ui.detail == "hidden") {
            that.showDetail();
        } else {
            that.hideDetail();
        }
    });

    ReactBootstrap.Dispatcher.on('Trafficconfigdetail.togglePin', function () {
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
    ReactBootstrap.Dispatcher.removeAllListeners("Trafficconfiglist.select");
    ReactBootstrap.Dispatcher.removeAllListeners("Trafficconfiglist.filter");
    ReactBootstrap.Dispatcher.removeAllListeners("Trafficconfigform.delete");
    ReactBootstrap.Dispatcher.removeAllListeners('Trafficconfigform.image');
    ReactBootstrap.Dispatcher.removeAllListeners("Trafficconfiglist.toggleDetail");
    ReactBootstrap.Dispatcher.removeAllListeners("Trafficconfigdetail.togglePin");
  },

  render() {
    var hstyle = { height: helpers.calcHeight(90, 0) + "px !important" };
    if (this.state.trafficconfigs && this.state.nodes && this.state.site && this.state.filterevent) {
      var listStyle = {width:"33%",height:"100%",overflow:"hidden",backgroundColor:"#FFF",position:"absolute",zIndex:"300",left:"0px",top:"0px",boxShadow:"2px 0px 2px 0px #CCC"};
      switch (this.state.ui.detail) {
          case "unpinned": 
              var detailStyle = {opacity:"0.9",position:"absolute",zIndex:"500",backgroundColor:"#FFF",overflowY:"hidden",overflowX:"hidden",border:"1px solid #EEE"};
              $.extend(detailStyle, this.detailUnpinnedPosition);
              var mapStyle = {width:"67%",height:"100%",position:"absolute",top:"0px",left:"33%",zIndex:"100"};
              $("#trafficconfig-detail-panel").draggable("enable").resizable("enable");
              break;
          case "pinned":
              detailStyle = {position:"absolute", zIndex:"200", backgroundColor:"#FFF", left:"33%", width:"25%", height:"100%"};
              mapStyle = {width:"42%",height:"100%",position:"absolute",top:"0px",left:"58%",zIndex:"100"};
              $("#trafficconfig-detail-panel").draggable("disable").resizable("disable");
              break;
          case "hidden":
              detailStyle = {position:"absolute", left:"-3000px"};
              mapStyle = {width:"67%",height:"100%",position:"absolute",top:"0px",left:"33%",zIndex:"100"}; 
              break;
      };
      return (
        <Container id='body' className="parkingZone-body" style={{backgroundColor:"#FFF",marginTop:"0px !important"}}>
          <div id="trafficconfig-list-panel" data-state="closed" style={listStyle}>
               <Trafficconfiglist minmax="max" trafficconfigs={this.state.trafficconfigs} trafficconfigID={this.state.trafficconfigID} detail_state={this.state.ui.detail} ui_state={this.state.ui}/>
          </div>
          <div id="trafficconfig-detail-panel" style={detailStyle}>
                <Trafficconfigdetail trafficconfigs={this.state.trafficconfigs} trafficconfigID={this.state.trafficconfigID} detail_state={this.state.ui.detail}/>
          </div>
          <div id="trafficconfig-map-panel" data-state="closed" style={mapStyle}>
                <Trafficconfigmap readonly={this.getTrafficconfig() != null} nodes={this.state.nodes} filterevent={this.state.filterevent} site={this.state.site} trafficconfigs={this.state.trafficconfigs} trafficconfigID={this.state.trafficconfigID} minmax="max" />
          </div>
          <ImageView context="trafficconfig" show={this.state.showimage} node={this.state.selectedvideonode} />
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
                    <h2 id="loadingmsg" style={{ paddingTop: "16%", textAlign: "center" }}>Loading trafficconfig information{forSite}.</h2>
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