import classNames from 'classnames';
import helpers from 'global/utils/helpers';

import Proximitylist from 'components/proximity/proximitylist';
import ProximityDetail from 'components/proximity/proximitydetail';
import DataUtil from '../service/datautil';

import Header from 'common/headernew';


var Body = React.createClass({
  getInitialState: function () {
    return {
      proximitys: null,
      groups: null,
      proximityID: NSN.proximityID,
      showProximityDetail: false,
      selectedProximity: null

    }
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

    DataUtil.getAll('proximitys', that.processProximityData);
    // $.ajax({
    //       url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/proximitydimming',
    //       data : '',
    //       xhrFields: {
    //          withCredentials: true
    //       },
    //       method : 'GET',
    //       dataType : 'json',
    //       success : function(data){
    //           console.log("ajax success: " + JSON.stringify(data));
    //           if (data == "") {
    //             that.setState({proximitys:[]});
    //           } else {
    //             that.setState({
    //               proximitys: data.map(function(proximity, index) {
    //                 proximity.enableRadius = (typeof proximity.radius != "undefined");
    //                 proximity.enableRadiusDisabled = proximity.enableRadius;
    //                 proximity.idx = index;
    //                 return proximity;
    //               })
    //           })
    //           }
    //       },
    //       error : function(jqXHR, status, error){
    //         console.log("ajax failure (proximitys): " + status + " - " + error);
    //         $("#loadingmsg").html("Cannot retrieve Proximity Dimming profiles.  API reported error: " + error);
    //       }
    //     });
  },
  ////////Callback function to get proximities//////////////
  processProximityData: function (data) {
    console.log("ajax success: " + JSON.stringify(data));
    if (data == "") {
      this.setState({ proximitys: [] });
    } else {
      this.setState(DataUtil.assignState('proximitys', data, this, this.makeProximityObj));
    }
  },
  makeProximityObj: function (proximity, index) {
    proximity.enableRadius = (typeof proximity.radius != "undefined");
    proximity.enableRadiusDisabled = proximity.enableRadius;
    proximity.idx = index;
    return proximity;
  },
  ///////////////Callback function to delete proximity//////
  processDeleteProximity: function (proximity_info, idx, data) {
    var newState = React.addons.update(this.state, { proximitys: { $splice: [[idx, 1]] }, proximityID: { $set: "-1" } });
    noty({ type: "success", text: 'Proximity Dimming profile "' + proximity_info.name + '" deleted.' })
    NSN.proximityID = "-1";
    sessionStorage.setItem("proximityID", NSN.proximityID);
    ReactBootstrap.Dispatcher.emit("Proximityform.delete.success", proximity_info.pdprofileid);
    this.setState(newState);
  },
  ////////////////Callback function to add proximity////////
  processAddProximity: function (data) {
    console.log("Response from Add Profile: " + JSON.stringify(data));
    NSN.proximityID = data.pdprofileid;
    sessionStorage.setItem("proximityID", NSN.proximityID);
    noty({ type: "success", text: 'Proximity Dimming Profile "' + data.name + '" added.' });
    ReactBootstrap.Dispatcher.emit('Proximityform.add.success', data);
    data.enableRadius = (typeof data.radius != "undefined");
    data.enableRadiusDisabled = data.enableRadius;
    var newState = React.addons.update(this.state, { proximitys: { $push: [data] }, proximityID: { $set: data.pdprofileid } });
    this.setState(newState);
  },
  ///////////////Callback function to update proximity//////
  processUpdateProximity: function (data, idx) {
    noty({ type: "success", text: 'Proximity Dimming Profile "' + data.name + '" updated.' })
    ReactBootstrap.Dispatcher.emit('Proximityform.update.success', data);
    data.enableRadius = (typeof data.radius != "undefined");
    data.enableRadiusDisabled = data.enableRadius;
    var newState = React.addons.update(this.state, { proximitys: { [idx]: { $set: data } } });
    this.setState(newState);
  },

  checkDuplicate: function(proximity_info){ 
    var duplicate = false;
        for (var i = 0, len = this.state.proximitys.length; i < len; i++) {
          console.log(JSON.stringify(this.state.proximitys[i].name) + JSON.stringify(proximity_info.name));

          if ((this.state.proximitys[i].name === proximity_info.name) && !(proximity_info.pdprofileid === this.state.proximitys[i].pdprofileid) )
            duplicate = true;
        }
        if (duplicate) {
          noty({ type: "error", text: 'Proximity Dimming Profile "' + proximity_info.name + '" already exists.' });
        }
        return duplicate;
  },
  componentDidMount: function () {
    this.init();
    var that = this;

    ReactBootstrap.Dispatcher.on("Proximitylist.select", function (proximityID, sortedFlag) {
      if(proximityID != NSN.proximityID  || (proximityID == NSN.proximityID && !sortedFlag)) {
        NSN.proximityID = proximityID
        sessionStorage.setItem("proximityID", NSN.proximityID);
        that.setState({
          "proximityID": proximityID,
          showProximityDetail: true
        });
        console.log('selected proximity', that.state.selectedProximity);
        console.log('selected proximityID', proximityID);
      }
    });

    ReactBootstrap.Dispatcher.on("Proximitylist.add", function () {
      NSN.proximityID = "0";
      sessionStorage.setItem("proximityID", NSN.proximityID);
      that.setState({
        "proximityID": "0",
        showProximityDetail: true
      });
    });

    ReactBootstrap.Dispatcher.on("Proximityform.reset", function () {
      // that.forceUpdate();
      NSN.proximityID = "0";
      sessionStorage.setItem("proximityID", NSN.proximityID);
      that.setState({
        "proximityID": "0",
        showProximityDetail: true
      });
    });

    ReactBootstrap.Dispatcher.on("Proximityform.delete", function (proximity_info) {
      var idx = helpers.get_idx(that.state.proximitys, proximity_info, 'pdprofileid');
      console.log("Deleting proximity (idx:" + idx + "; id:" + proximity_info.pdprofileid + ")");
      delete proximity_info.idx;
      DataUtil.deleteEntity('delete-proximity', proximity_info, that.processDeleteProximity, idx);
      that.setState({
        showProximityDetail: false
      });
      // $.ajax({
      //   url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/proximitydimming/' + proximity_info.pdprofileid,
      //   "type" : "DELETE",
      //   "xhrFields": {
      //      withCredentials: true
      //   },
      //   "data" : JSON.stringify(proximity_info),
      //   "dataType" : "json",
      //   "contentType" : "application/json",
      //   "processData" : false,
      //   "success" : function(data) {
      //     var newState = React.addons.update(this.state, { proximitys: { $splice: [[idx, 1]] }, proximityID: { $set : "-1"}});
      //     noty({type:"success", text:'Proximity Dimming profile "' + proximity_info.name + '" deleted.'})
      //     NSN.proximityID = "-1";
      //     sessionStorage.setItem("proximityID", NSN.proximityID);
      //     ReactBootstrap.Dispatcher.emit("Proximityform.delete.success", proximity_info.pdprofileid);
      //     this.setState(newState);
      //   }.bind(that),
      //   "error" : function() {
      //     noty({type:"error", text:'Could not delete Proximity Dimming profile.'});
      //   }
      // })
    });

    ReactBootstrap.Dispatcher.on("Proximityform.save", function (proximity_info) {
      var newState = {};
      if (proximity_info.pdprofileid == "-1" || proximity_info.pdprofileid == "0") {
        console.log("Adding proximity: " + proximity_info.name);
        
         if(!that.checkDuplicate(proximity_info)){
          delete proximity_info.idx;
          delete proximity_info.pdprofileid;
          if (!proximity_info.enableRadius) { delete proximity_info.radius; };
          delete proximity_info.enableRadius;
          delete proximity_info.enableRadiusDisabled;
          delete proximity_info.nodes;
          delete proximity_info.groups;
          delete proximity_info.sites;

          DataUtil.addEntity('proximitys', proximity_info, that.processAddProximity, '')
          that.setState({
            showProximityDetail: false
          });
          // $.ajax({
          //   url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/proximitydimming',
          //   "type" : "POST",
          //   "data" : JSON.stringify(proximity_info),
          //   "xhrFields": {
          //      withCredentials: true
          //   },
          //   "dataType" : "json",
          //   "contentType": "application/json",
          //   "processData": false,
          //   "success" : function(data) {
          //     console.log("Response from Add Profile: " + JSON.stringify(data));
          //     NSN.proximityID = data.pdprofileid;
          //     sessionStorage.setItem("proximityID", NSN.proximityID);
          //     noty({type:"success", text:'Proximity Dimming Profile "' + data.name + '" added.'});
          //     ReactBootstrap.Dispatcher.emit('Proximityform.add.success', data);
          //     data.enableRadius = (typeof data.radius != "undefined");
          //     data.enableRadiusDisabled = data.enableRadius;
          //     var newState = React.addons.update(this.state, { proximitys: { $push: [data] }, proximityID: {$set: data.pdprofileid } });
          //     this.setState(newState);
          //   }.bind(that),
          //   "error" : function() {
          //     noty({type:"error", text:"Could not add Proximity Dimming Profile."});
          //   }
          // })
        }
      } else if(!that.checkDuplicate(proximity_info)){
        var idx = helpers.get_idx(that.state.proximitys, proximity_info, 'pdprofileid');
        delete proximity_info.idx;
        if (!proximity_info.enableRadius) { delete proximity_info.radius; };
        delete proximity_info.enableRadius;
        delete proximity_info.enableRadiusDisabled;

        DataUtil.updateEntity('update-proximity', proximity_info, that.processUpdateProximity, idx);
        // $.ajax({
        //   url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/proximitydimming/' + proximity_info.pdprofileid,
        //   "type" : "POST",
        //   "xhrFields": {
        //      withCredentials: true
        //   },
        //   "data" : JSON.stringify(proximity_info),
        //   "dataType" : "json",
        //   "contentType" : "application/json",
        //   "processData" : false,
        //   "success" : function(data) {
        //     noty({type:"success", text:'Proximity Dimming Profile "' + data.name + '" updated.'})
        //     ReactBootstrap.Dispatcher.emit('Proximityform.update.success', data);
        //     data.enableRadius = (typeof data.radius != "undefined");
        //     data.enableRadiusDisabled = data.enableRadius;
        //     newState = React.addons.update(this.state, { proximitys: { [idx]: { $set: data } }});
        //     this.setState(newState);
        //   }.bind(that),
        //   "error" : function() {
        //     noty({type:"error", text:'Could not update Proximity Dimming Profile.'});
        //   }
        // })
      }
    })

  },

  componentWillUnmount: function () {
    ReactBootstrap.Dispatcher.removeAllListeners("Proximitylist.select");
    ReactBootstrap.Dispatcher.removeAllListeners("Proximitylist.add");
    ReactBootstrap.Dispatcher.removeAllListeners("Proximityform.save");
    ReactBootstrap.Dispatcher.removeAllListeners("Proximityform.reset");
    ReactBootstrap.Dispatcher.removeAllListeners("Proximityform.delete");
  },

  hideproximityDetail() {
    this.setState({
      showProximityDetail: false
    });
  },

  render() {
    var hstyle = { height: helpers.calcHeight(90, 0) + "px !important" };
    if (this.state.proximitys) {
      var Subpanels = (
        <div>
          <div className="netsense-center-panel">
            <Col sm={12} md={12} lg={12}>
              <PanelContainer>
                <Panel>
                  <PanelBody style={hstyle} >
                    <ProximityDetail show={this.state.showProximityDetail} proximitys={this.state.proximitys} hide={this.hideproximityDetail} proximityID={this.state.proximityID} />
                    <Proximitylist proximitys={this.state.proximitys} proximityID={this.state.proximityID} />
                  </PanelBody>
                </Panel>
              </PanelContainer>
            </Col>
            {/*<Col md={12} lg={7}>
                <PanelContainer>
                  <Panel>
                    <PanelBody style={hstyle} className="overflow_y">
                        <ProximityDetail proximitys={this.state.proximitys} proximityID={this.state.proximityID} />
                    </PanelBody>
                  </Panel>
                </PanelContainer>
              </Col>*/}
          </div>
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
                  <PanelBody style={hstyle} >
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