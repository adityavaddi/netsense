import classNames from 'classnames'
import Fixturelist from 'components/fixtures/fixturelist';
import FixtureDetail from 'components/fixtures/fixturedetail';
import helpers from 'global/utils/helpers';
import DataUtil from '../service/datautil';
import Header from 'common/headernew';


var Body = React.createClass({
  getInitialState: function () {
    return {
      fixtures: null,
      fixturetypes: null,
      fixtureID: NSN.fixtureID,
      showFixtureDetail: false,
      selectedFixture: null
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

    this.hstyle = { height: helpers.calcHeight(90, 0) + "px !important" };

    that.getAllFixtures();

    that.setState({
      fixturetypes: helpers.getFixtureType()
    });

    DataUtil.getAll('groups', that.processGroupData);
  },

  getFixture(fixtureID, fixtures) {
    if (fixtureID == "0" || fixtureID == "-1") {
      return {
        name: "",
        description: "",
        manufacturer: "",
        manufacturersku: "",
        fixtureid: "",
        fixtureType: "",
        nemasocket: "",
        MaxPower0: "",
        MaxPower10: "",
        MaxPower50: "",
        MaxPower100: "",
        MinPower0: "",
        MinPower10: "",
        MinPower50: "",
        MinPower100: "",
        PowerDraw: "",
        MinimumLightLevelForFailureDetection: "",
        BallastCost: "",
        BulbCost: "",
        LegacyPowerDraw: "",
        DailyOperatingTime: "",
        groups: [],
        sites: [],
        nodes: [],
        assign: "unassigned",
        assigngroups: [],
        idx: -1
      };
    };
    var fixtures = this.state.fixtures;
    for (var i = 0; i < fixtures.length; i++) {
      if (fixtures[i].fixtureid == fixtureID) {
        fixtures[i].assigngroups = fixtures[i].groups.map(function (group, index) { return group.groupid; });
        if (!fixtures[i].BallastCost) {
          fixtures[i].BallastCost = "";
        }
        if (!fixtures[i].BulbCost) {
          fixtures[i].BulbCost = "";
        }
        if (!fixtures[i].LegacyPowerDraw) {
          fixtures[i].LegacyPowerDraw = "";
        }
        if (!fixtures[i].DailyOperatingTime) {
          fixtures[i].DailyOperatingTime = "";
        }
        return (fixtures[i]);
      }
    }
    return null;
  },

  ///////////////////callback for group//////////
  makeGroupObj: function (group, index) {
    group.idx = index;
    return group;
  },
  processGroupData: function (data) {
    console.log("ajax success (groups): " + JSON.stringify(data));
    this.setState(DataUtil.assignState('groups', data, this, this.makeGroupObj))
  },
  getAllFixtures() {
    var that = this;

    //Fixtures:
    DataUtil.getAll('fixtures', that.processFixtureData);
    // $.ajax({
    //     url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/fixtures',
    //   xhrFields: {
    //     withCredentials: true
    //   },
    //   data : '',
    //   method : 'GET',
    //   dataType : 'json',
    //   success : function(data){
    //     if (data.errors) {
    //       console.log("/customers/" + NSN.customerID + "/sites/" + NSN.siteID + "/fixtures API returned error: " + JSON.stringify(data));
    //       $("#loadingmsg").html("Cannot retrieve fixtures list.  API returned: " + JSON.stringify(data));       
    //     } else {
    //       console.log("ajax success: " + JSON.stringify(data));
    //       that.setState({
    //         fixtures: data.map(function(fixture, index) {
    //           if (typeof fixture.sites == "undefined") {fixture.sites = [];};
    //           if (typeof fixture.groups == "undefined") {fixture.groups = [];};
    //           if (typeof fixture.nodes == "undefined") {fixture.nodes = [];};
    //           if (fixture.sites.length == 1 && $.isEmptyObject(fixture.sites[0])) {
    //             fixture.sites = [];
    //           };
    //           if (fixture.groups.length == 1 && $.isEmptyObject(fixture.groups[0])) {
    //             fixture.groups = [];
    //           };
    //           if (fixture.nodes.length == 1 && $.isEmptyObject(fixture.nodes[0])) {
    //             fixture.nodes = [];
    //           };
    //           fixture.assign = "unassigned";
    //           if (fixture.sites.length > 0) {
    //             fixture.assign = "sitewide";
    //           } else {
    //             if (fixture.groups.length > 0) {
    //               fixture.assign = "groups";
    //             }
    //           }
    //           fixture.idx = index;
    //           //fixture.icon = that.getIcon(fixture.fixtureType);
    //           return fixture;
    //         })
    //       })
    //     }
    //   },
    //   error : function(jqXHR, status, error){
    //     console.error("ajax failure (fixtures): ", status, error);
    //     $("#loadingmsg").html("Cannot retrieve fixture list.  API call failed.");
    //   }
    // });
  },
  ////////////////callback function to get all fixtures////////////
  processFixtureData: function (data) {
    if (data.errors) {
      console.log("/customers/" + NSN.customerID + "/sites/" + NSN.siteID + "/fixtures API returned error: " + JSON.stringify(data));
      $("#loadingmsg").html("Cannot retrieve fixtures list.  API returned: " + JSON.stringify(data));
    } else {
      console.log("ajax success: " + JSON.stringify(data));
      this.setState(DataUtil.assignState('fixtures', data, this, this.makeFixturesObj));
      this.setState({
        selectedFixture: this.getFixture(NSN.fixtureID)
      });
    }
  },

  makeFixturesObj: function (fixture, index) {
    if (typeof fixture.sites == "undefined") { fixture.sites = []; };
    if (typeof fixture.groups == "undefined") { fixture.groups = []; };
    if (typeof fixture.nodes == "undefined") { fixture.nodes = []; };
    if (fixture.sites.length == 1 && $.isEmptyObject(fixture.sites[0])) {
      fixture.sites = [];
    };
    if (fixture.groups.length == 1 && $.isEmptyObject(fixture.groups[0])) {
      fixture.groups = [];
    };
    if (fixture.nodes.length == 1 && $.isEmptyObject(fixture.nodes[0])) {
      fixture.nodes = [];
    };
    fixture.assign = "unassigned";
    if (fixture.sites.length > 0) {
      fixture.assign = "sitewide";
    } else {
      if (fixture.groups.length > 0) {
        fixture.assign = "groups";
      }
    }
    fixture.idx = index;
    //fixture.icon = that.getIcon(fixture.fixtureType);
    return fixture;
  },
  /*getIcon: function(fixtureID) {
    for (var i=0; i<this.state.fixturetypes.length; i++) {
      if (this.state.fixturetypes[i].fixtureType == fixtureID){
        return (this.state.fixturetypes[i].fixtureIcon);
      }
    }
  }, */

  // getFixture: function (fixtureID) {
  //   for (var i = 0; i < this.state.fixtures.length; i++) {
  //     if (this.state.fixtures[i].fixtureid == fixtureID) {
  //       return (this.state.fixtures[i]);
  //     }
  //   }
  // },

  fixtureAssignment(assignment, etdhprofileid) {
    var payload = [];
    if (assignment.assign == "unassigned"
      || (assignment.assign == "groups" && assignment.assigngroups.length == 0)) {
      noty({ type: "information", text: "As requested, no assignment was made." });
      return;
    }
    if (assignment.assign == "sitewide") {
      var url = NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID
        + '/fixtures/' + etdhprofileid + '/assign/site';
    } else {
      if (assignment.assign == "groups") {
        url = NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID
          + '/fixtures/' + etdhprofileid + '/assign/groups/' + assignment.assigngroups[0];
      }
    }
    $.ajax({
      url: url,
      data: '',
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
          type: "success", text: 'Fixture assigned '
          + ((assignment.assign == "sitewide") ? "sitewide" : "to group(s)")
          + ' as requested.'
        });
      },
      error: function (jqXHR, status, error) {
        if (jqXHR.status == 200) {
          noty({
            type: "success", text: 'Fixture assigned '
            + ((assignment.assign == "sitewide") ? "sitewide" : "to group(s)")
            + ' as requested.'
          });
        } else {
          noty({
            type: "error", text: 'Unable to assign Fixture '
            + 'as requested.  Return status: ' + status
          });
        }
      }
    });
  },
  ////////////////Callback function to delete fixture////////////
  processDeleteFixtures: function (fixture_info, fixture_idx, data) {
    var newState = React.addons.update(this.state, { fixtures: { $splice: [[fixture_idx, 1]] }, fixtureID: { $set: "-1" } });
    NSN.fixtureID = "-1";
    sessionStorage.setItem("fixtureID", NSN.fixtureID);
    noty({ type: "success", text: 'Fixture "' + fixture_info.name + '" deleted.' });
    ReactBootstrap.Dispatcher.emit("Fixtureform.delete.success", fixture_info.fixtureid);
    this.setState(newState);
  },
  ////////////////Callback to Add Fixture form/////////////
  processAddFixture: function (data) {

    var assignment = data.inputdata.assignment;
    console.log(JSON.stringify(assignment));
    var save_sites = data.inputdata.save_sites;
    var save_groups = data.inputdata.save_groups;
    var save_nodes = data.inputdata.save_nodes;
    console.log("Response from Add Fixtures: " + JSON.stringify(data));
    data.groups = this.recreateGroups(assignment.assigngroups, this.state.groups);
    data.assign = assignment.assign;
    data.sites = save_sites.splice(0);
    data.nodes = save_nodes.splice(0);
    NSN.fixtureID = data.fixtureid;
    sessionStorage.setItem("fixtureID", NSN.fixtureID);
    data.idx = this.state.fixtures.length;
    var newState = React.addons.update(this.state, { fixtures: { $push: [data] }, fixtureID: { $set: data.fixtureid } });
    this.getAllFixtures();
    noty({ type: "success", text: 'Fixture "' + data.name + '" added.' });
    this.fixtureAssignment(assignment, data.fixtureid);
    ReactBootstrap.Dispatcher.emit("Fixtureform.add.success", data);
    this.setState(newState);
  },
  /////////////////Callback function to update fixtures//////////////////
  processUpdateFixture: function (data, inputdata) {
    var assignment = inputdata.assignment;
    console.log(JSON.stringify(assignment));
    var save_sites = inputdata.save_sites;
    var save_groups = inputdata.save_groups;
    var save_nodes = inputdata.save_nodes;
    data.idx = inputdata.idx;
    data.groups = this.recreateGroups(assignment.assigngroups, this.state.groups);
    data.assign = assignment.assign;
    data.sites = save_sites.splice(0);
    data.nodes = save_nodes.splice(0);
    var newState = React.addons.update(this.state, { fixtures: { [inputdata.idx]: { $set: data } } });
    // this.getAllFixtures();
    noty({ type: "success", text: 'Fixture "' + data.name + '" updated.' });
    this.fixtureAssignment(assignment, data.fixtureid);
    ReactBootstrap.Dispatcher.emit("Fixtureform.update.success", data);
    this.setState(newState);
  },

  checkDuplicate: function(fixture_info){
    console.log("fixture_info in check dup",fixture_info);
        var duplicate = false;
        for (var i = 0, len = this.state.fixtures.length; i < len; i++) {
          console.log("check duplicate fixture",JSON.stringify(this.state.fixtures[i].name) + JSON.stringify(fixture_info.name));
          if ((this.state.fixtures[i].name === fixture_info.name)&&!(this.state.fixtures[i].fixtureid === fixture_info.fixtureid))
            duplicate = true;
        }
        if (duplicate) {
          noty({ type: "error", text: 'fixture profile "' + fixture_info.name + '" already exists.' });
        }
        return duplicate;
  },
  componentWillMount: function () {
    console.log("componentWillMount");
  },

  componentDidMount: function () {
    this.init();
    var that = this;

    ReactBootstrap.Dispatcher.on("Fixturelist.select", function (fixtureID, sortedFlag) {
      console.log("Fixtureform.select", fixtureID);
      if(fixtureID != NSN.fixtureID  || (fixtureID == NSN.fixtureID && !sortedFlag)) {
        NSN.fixtureID = fixtureID;
        sessionStorage.setItem("fixtureID", NSN.fixtureID);
        NSN.fixture = that.getFixture(fixtureID);
        // that.setState({ "fixtureID": fixtureID });
        that.setState({
          selectedFixture: that.getFixture(NSN.fixtureID)
        }, () => { // setState is Async, Show modal only after customer is found
          that.setState({
            showFixtureDetail: true
          });
        });
      }
    });

    ReactBootstrap.Dispatcher.on("Fixturelist.add", function (fixture_info) {
      console.log("Fixtureform.add", fixture_info);
      $("#fixture-table tbody tr").removeClass("selected");
      NSN.fixtureID = "0";
      sessionStorage.setItem("fixtureID", NSN.fixtureID);
      that.setState({
        "fixtureID": "0",
        selectedFixture: that.getFixture("0"), //New Fixture get a id of "0"
        showFixtureDetail: true
      });
    });

    ReactBootstrap.Dispatcher.on("Fixtureform.reset", function () {
      // that.forceUpdate();
      that.setState({
        selectedFixture: that.getFixture("0") // resetting fields to blank on reset
      })
    });

    ReactBootstrap.Dispatcher.on("Fixtureform.delete", function (fixture_info) {
      console.log("Fixtureform.delete", fixture_info);
      var fixture_idx = fixture_info.idx;
      delete fixture_info.idx;
      DataUtil.deleteEntity('delete-fixtures', fixture_info, that.processDeleteFixtures, fixture_idx);
      that.setState({
        showFixtureDetail: false
      });
      // $.ajax({
      //   "url" : NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/fixtures/' + fixture_info.fixtureid,
      //   "type" : "DELETE",
      //   "xhrFields": {
      //      withCredentials: true
      //   },
      //   "data" : JSON.stringify(fixture_info),
      //   "dataType" : "json",
      //   "contentType" : "application/json",
      //   "success" : function(data) {
      //     var newState = React.addons.update(this.state, { fixtures: { $splice: [[fixture_idx, 1]] }, fixtureID: {$set: "-1" } });
      //     NSN.fixtureID = "-1";
      //     sessionStorage.setItem("fixtureID", NSN.fixtureID);
      //     noty({type:"success", text:'Fixture "' + fixture_info.name + '" deleted.'});
      //     ReactBootstrap.Dispatcher.emit("Fixtureform.delete.success", fixture_info.fixtureid);
      //     this.setState(newState);
      //   }.bind(that),
      //   "error" : function(jqXHR, status, error) {
      //     console.log("ajax failure (fixtures): " + status + " - " + error);
      //     noty({type:"error", text:"Could not delete fixture."});
      //   }
      // });
    });

    ReactBootstrap.Dispatcher.on("Fixtureform.save", function (fixture_info) {
      console.log("Fixtureform.save", fixture_info);

//      fixture_info.fixtureType = $("select#fixtureType option").filter(":selected").val();

      var newState = {};
      var assignment = {
        assign: fixture_info.assign,
        assigngroups: fixture_info.assigngroups.splice(0)
      };
      console.log(JSON.stringify(assignment));
      delete fixture_info.assign;
      delete fixture_info.assigngroups;
      var save_sites = fixture_info.sites.splice(0);
      delete fixture_info.sites;
      var save_groups = fixture_info.groups.splice(0);
      delete fixture_info.groups;
      var save_nodes = fixture_info.nodes.splice(0);
      delete fixture_info.nodes;

       if (fixture_info.fixtureid == "") {
         if(!that.checkDuplicate(fixture_info)){
        delete fixture_info.idx;
        delete fixture_info.fixtureid;

          var inputdata = fixture_info;
          inputdata.assignment = assignment;
          inputdata.save_sites = save_sites;
          inputdata.save_nodes = save_nodes;
          inputdata.save_groups = save_groups;
          DataUtil.addEntity('add-fixtures', fixture_info, that.processAddFixture, inputdata);
          that.setState({
            showFixtureDetail: false
          });
          // $.ajax({
          //   "url" : NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/fixtures/',
          //   "type" : "POST",
          //   "xhrFields": {
          //      withCredentials: true
          //   },
          //   "data" : JSON.stringify(fixture_info),
          //   "dataType" : "json",
          //   "contentType": "application/json",
          //   "success" : function(data) {
          //     console.log("Response from Add Fixtures: " + JSON.stringify(data));
          //     data.groups = that.recreateGroups(assignment.assigngroups, that.state.groups);
          //     data.assign = assignment.assign;
          //     data.sites = save_sites.splice(0);
          //     data.nodes = save_nodes.splice(0);
          //     NSN.fixtureID = data.fixtureid;
          //     sessionStorage.setItem("fixtureID", NSN.fixtureID);
          //     data.idx = this.state.fixtures.length;
          //     var newState = React.addons.update(this.state, { fixtures: { $push : [data] }, fixtureID: {$set: data.fixtureid } });
          //     that.getAllFixtures();
          //     noty({type:"success", text:'Fixture "' + data.name + '" added.'});
          //     this.fixtureAssignment(assignment, data.fixtureid);
          //     ReactBootstrap.Dispatcher.emit("Fixtureform.add.success", data);
          //     this.setState(newState);
          //   }.bind(that),
          //   "error" : function(jqXHR, status, error){
          //     console.log("ajax failure (fixture save): " + status + " - " + error);
          //     noty({type:"error", text:"Could not add fixture."});
          //   }
          // })
       }
      } else if(!that.checkDuplicate(fixture_info)){
        var idx = helpers.get_idx(that.state.fixtures, fixture_info, 'fixtureid');
        var inputdata = fixture_info;
        inputdata.assignment = assignment;
        inputdata.save_sites = save_sites;
        inputdata.save_nodes = save_nodes;
        inputdata.save_groups = save_groups;
        delete fixture_info.idx;
        DataUtil.updateEntity('update-fixtures', fixture_info, that.processUpdateFixture, inputdata);
              that.setState({
        showFixtureDetail: false
      });
        // $.ajax({
        //   "url" : NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/fixtures/' + fixture_info.fixtureid,
        //   "type" : "POST",
        //   "xhrFields": {
        //      withCredentials: true
        //   },
        //   "data" : JSON.stringify(fixture_info),
        //   "dataType" : "json",
        //   "contentType" : "application/json",
        //   "success" : function(data) {
        //     data.idx = idx;
        //     data.groups = that.recreateGroups(assignment.assigngroups, that.state.groups);
        //     data.assign = assignment.assign;
        //     data.sites = save_sites.splice(0);
        //     data.nodes = save_nodes.splice(0);
        //     var newState = React.addons.update(this.state, { fixtures: { [idx]: { $set: data } }});
        //     that.getAllFixtures();
        //     noty({type:"success", text:'Fixture "' + data.name + '" updated.'});
        //     this.fixtureAssignment(assignment, data.fixtureid);
        //     ReactBootstrap.Dispatcher.emit("Fixtureform.update.success", data);
        //     this.setState(newState);
        //   }.bind(that),
        //   "error" : function(jqXHR, status, error){
        //     console.log("ajax failure (fixture update): " + status + " - " + error);
        //     noty({type:"error", text:'Could not update fixture.'});
        //   }
        // })
      }
    })
  },


  componentDidUpdate: function () {
  },

  componentWillUnmount: function () {
    ReactBootstrap.Dispatcher.removeAllListeners("Fixturelist.select");
    ReactBootstrap.Dispatcher.removeAllListeners("Fixturelist.add");
    ReactBootstrap.Dispatcher.removeAllListeners("Fixtureform.save");
    ReactBootstrap.Dispatcher.removeAllListeners("Fixtureform.delete");
    ReactBootstrap.Dispatcher.removeAllListeners("Fixtureform.reset");
  },

  /*render() {
    var hstyle = {height:helpers.calcHeight(90, 0)+"px !important",'overflowY':'auto','overflowX':'hidden'};
    if (this.state.fixtures && this.state.fixturetypes && this.state.groups) {

      var Subpanels = (
            <div>
              <Col md={12} lg={5}>
                <PanelContainer>
                  <Panel>
                    <PanelBody style={hstyle}>
                       <Fixturelist fixtures={this.state.fixtures} fixturetypes={this.state.fixturetypes} fixtureID={this.state.fixtureID}/>
                    </PanelBody>
                  </Panel>
                </PanelContainer>
              </Col>
              <Col md={12} lg={7}>
                <PanelContainer>
                  <Panel>
                    <PanelBody style={hstyle}>
                        <FixtureDetail groups={this.state.groups} fixtures={this.state.fixtures} fixturetypes={this.state.fixturetypes} fixtureID={this.state.fixtureID}/>
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
    var forSite = (NSN && NSN.siteName && NSN.siteName != "")?(" for " + NSN.siteName):"";
    return (
      <Container id='body'>
        <Grid>
          <Row>
            <Col sm={12}>
              <PanelContainer>
                <Panel>
                  <PanelBody style={hstyle} ref="loading">
                    <h2 id="loadingmsg" style={{paddingTop:"16%",textAlign:"center"}}>Loading fixture information{forSite}.</h2>
                  </PanelBody>
                </Panel>
              </PanelContainer>
            </Col>
          </Row>
        </Grid>
      </Container>
    );
  }*/

  // New Fixture panel
  hidefixtureDetail() {
    this.setState({
      showFixtureDetail: false
    })
  },

  render() {
    var hstyle = { height: helpers.calcHeight(90, 0) + "px !important", 'overflowY': 'auto', 'overflowX': 'hidden' };
    if (this.state.fixtures && this.state.fixturetypes && this.state.groups) {
      console.log("Fix Panel", this.state)
      var Subpanels = (
        <div className="netsense-center-panel">
          <Col md={12} lg={12}>
            <PanelContainer>
              <Panel>
                <PanelBody style={hstyle}>
                  <FixtureDetail show={this.state.showFixtureDetail} fixtures={this.state.fixtures} groups={this.state.groups} fixturetypes={this.state.fixturetypes} fixtureID={this.state.fixtureID} hide={this.hidefixtureDetail} fixture={this.state.selectedFixture} />
                  <Fixturelist fixtures={this.state.fixtures} groups={this.state.groups} fixturetypes={this.state.fixturetypes} fixtureID={this.state.fixtureID} />
                </PanelBody>
              </Panel>
            </PanelContainer>
          </Col>
          {/*<Col md={12} lg={7}>
                <PanelContainer>
                  <Panel>
                    <PanelBody style={hstyle}>
                        <FixtureDetail groups={this.state.groups} fixtures={this.state.fixtures} fixturetypes={this.state.fixturetypes} fixtureID={this.state.fixtureID}/>
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
                <Row>
                  {Subpanels}
                </Row>
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
                    <h2 id="loadingmsg" style={{ paddingTop: "16%", textAlign: "center" }}>Loading fixture information{forSite}.</h2>
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