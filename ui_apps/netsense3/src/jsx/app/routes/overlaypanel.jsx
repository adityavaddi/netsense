import classNames from 'classnames';
import Overlaylist from 'components/overlays/overlaylist';
import OverlayDetail from 'components/overlays/overlaydetail';
import DataUtil from '../service/datautil';
import helpers from 'global/utils/helpers';

import Header from 'common/headernew';


var Body = React.createClass({
  getInitialState: function () {
    return {
      overlays: null,
      overlayID: NSN.overlayID
    }
  },

  calcHeight: function (pct, extra) {
    var h = (window && window.document) ? (($(window).height() - 75) * (pct / 100) + extra) : 500;
    return h;
  },

  init() {
    if (NSN.customerID == "-1") {
      alert("Cannot call nodes API:  customerID is not set.")
      return;
    };
    if (NSN.siteID == "-1") {
      alert("Cannot call nodes API:  siteID is not set.")
      return;
    };

    var that = this;
    $.ajax({
      url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/overlays',
      xhrFields: {
        withCredentials: true
      },
      data: '',
      method: 'GET',
      dataType: 'json',
      success: function (data) {
        if (data.errors) {
          console.log("ajax error");
          that.setState({
            overlays: helpers.getOverlayList().map(function (overlay, index) {
              overlay.idx = index;
              return overlay;
            })
          });
        } else {
          console.log("ajax success: " + JSON.stringify(data));

          /*var overlay = [];
          $.each(data, function(i, obj) {
            overlay.push({fileName:obj.fileName,description:obj.description,users:obj.users,overlayID:obj.id,imageBounds:obj.imageBounds,imageData:obj.imageData,imageType:obj.imageType,buildingLevel:obj.buildingLevel});
          }); */
          that.setState({
            overlays: data.map(function (overlay, index) {
              overlay.idx = index;
              return overlay;
            })
          })
        };
      },
      error: function () {
        console.log("ajax failure");
        that.setState({
          overlays: helpers.getOverlayList().map(function (overlay, index) {
            overlay.idx = index;
            return overlay;
          })
        });
      }
    });
  },
  //////////////callback function to add overlay//////////

  processAddOverlay: function (data) {
    var newState = {};
    //console.log("Response from Add Overlays: " + JSON.stringify(data));
    NSN.overlayID = data.overlayid;
    newState = React.addons.update(that.state, { overlays: { $push: [data] } });
  },

  ////////////////callback function to delete overlay////////

  processDeleteOverlay: function (overlay_info, overlay_idx, data) {
    //console.log("DELETE that.state", that.state);
    newState = React.addons.update(that.state, { overlays: { $splice: [[overlay_idx, 1]] } });

    // After deleting elements of the array in the middle.  Reindex the overlays
    /*newState.overlays.forEach(function(val, ii) {
      val.idx = ii;
    }); */

    //console.log("DELETE newState", newState);
    //that.setState(newState);
  },

  /////////////callback function to update overlay//////////

  processUpdateOverlay: function (data, overlay_idx) {
    newState = React.addons.update(that.state, { overlays: { [overlay_idx]: { $set: overlay_info } } });
    //that.setState(newState);
    //alert("yes i am in success");
  },

  componentDidMount() {
    this.init();
    var that = this;

    ReactBootstrap.Dispatcher.on("Overlaylist.select", function (overlayID) {
      NSN.overlayID = overlayID;
      that.setState({ "overlayID": overlayID });
    });

    ReactBootstrap.Dispatcher.on("Overlayform.delete", function (overlay_info) {
      var newState = {};
      var overlay_idx = overlay_info.idx;
      delete overlay_info.idx;

      DataUtil.deleteEntity('delete-overlay', overlay_info, that.processDeleteOverlay, overlay_idx);
      // $.ajax({
      //   "url" : NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/overlays/' + overlay_info.overlayid,
      //   "type" : "DELETE",
      //   "xhrFields": {
      //      withCredentials: true
      //   },
      //   "success" : function(data) {
      //     //console.log("DELETE that.state", that.state);
      //     newState = React.addons.update(that.state, { overlays: { $splice: [[overlay_idx, 1]] }});

      //     // After deleting elements of the array in the middle.  Reindex the overlays
      //     /*newState.overlays.forEach(function(val, ii) {
      //       val.idx = ii;
      //     }); */

      //     //console.log("DELETE newState", newState);
      //     //that.setState(newState);
      //   },
      //   "error" : function() {
      //     console.log(JSON.stringify(overlay_info));
      //     //console.log(JSON.stringify(overlay_info.id));
      //     console.log(overlay_idx);
      //   }
      // })
      that.setState(newState);
    });

    ReactBootstrap.Dispatcher.on("Overlaylist.add", function () {
      that.setState({ "overlayID": "0" });
      /*that.setState({
        overlayID:"0",
        overlay: {
          overlayid: "",
          fileName: "",
          description: "",
          users: "",
          imageBounds: "",
          imageData: "",
          imageType: "",
          buildingLevel: "",
          idx: -1
        }
      }); */
    });

    ReactBootstrap.Dispatcher.on("Overlayform.save", function (overlay_info) {
      var newState = {};
      if (overlay_info.idx == -1) {
        delete overlay_info.idx;

        DataUtil.addEntity('add-overlay', overlay_info, that.processAddOverlay, '');
        // $.ajax({
        //   "url" : NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/overlays/',
        //   "type" : "POST",
        //   "xhrFields": {
        //      withCredentials: true
        //   },
        //   "data" : JSON.stringify(overlay_info),
        //   "dataType" : "json",
        //   "contentType": "application/json",
        //   "success" : function(data) {
        //     //console.log("Response from Add Overlays: " + JSON.stringify(data));
        //     NSN.overlayID = data.overlayid;
        //     newState = React.addons.update(that.state, { overlays: { $push : [data] }});
        //     //that.setState(newState);
        //     //console.log("data in newState: ", newState);
        //   },
        //   "error" : function() {
        //     //alert("Could not add overlays.");
        //   }
        // })
      } else {
        var overlay_idx = overlay_info.idx;
        delete overlay_info.idx;

        DataUtil.updateEntity('update-overlay', overlay_info, that.processUpdateOverlay, overlay_idx);
        // $.ajax({
        //   "url" : NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/overlays/' + overlay_info.overlayid,
        //   "type" : "POST",
        //   "xhrFields": {
        //      withCredentials: true
        //   },
        //   "data" : JSON.stringify(overlay_info),
        //   "dataType" : "json",
        //   "contentType" : "application/json",
        //   "success" : function(data) {
        //     newState = React.addons.update(that.state, { overlays: { [overlay_idx]: { $set: overlay_info } }});
        //     //that.setState(newState);
        //     //alert("yes i am in success");
        //   },
        //   "error" : function() {
        //     //alert("Could not update Overlays.")
        //     //alert("yes i am in failure");
        //     console.log(JSON.stringify(overlay_info));
        //     console.log(JSON.stringify(overlay_info.overlayid));
        //     console.log(overlay_idx);
        //   }
        // })
      }
      that.setState(newState);
    })
  },

  render() {
    var hstyle = { height: helpers.calcHeight(90, 0) + "px !important", 'overflowY': 'auto', 'overflowX': 'hidden' }
    if (this.state.overlays) {
      var Subpanels = (
        <div>
          <Col sm={5}>
            <PanelContainer>
              <Panel>
                <PanelBody>
                  <Overlaylist overlays={this.state.overlays} overlayID={this.state.overlayID} />
                </PanelBody>
              </Panel>
            </PanelContainer>
          </Col>
          <Col sm={7}>
            <PanelContainer>
              <Panel>
                <PanelBody style={hstyle}>
                  <OverlayDetail overlays={this.state.overlays} overlayID={this.state.overlayID} />
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
                    <h2 id="loadingmsg" style={{ paddingTop: "16%", textAlign: "center" }}>Loading...</h2>
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