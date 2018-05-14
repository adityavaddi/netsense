import classNames from 'classnames';
import helpers from 'global/utils/helpers';
import Header from 'common/headernew';
import EnergyList from 'components/energy/energylist';
import EnergyGraph from 'components/energy/energygraph';
import DataUtil from '../service/datautil';

var Body = React.createClass({
    getInitialState: function() {
        return {
            siteID: NSN.siteID,
            customerID: NSN.customerID,
            sensors: null,
            nodes: null,
            sites: null,
            selected_timeframe: null,
            selected_nodes: null,
            selected_sites: null,
            selected_conversions: ["kWh"],
            from: null,
            to: null,
            time_zone: null,
            fixtures: null,
            selected_fixture: null,
            schedules: null,
            selected_schedule: null,
            groups: null,
            selected_group: null
        }
    },
    lookerData: function (nodeid,from,to,fixtureid,scheduleid) {
        nodeid= (nodeid==undefined? " ":nodeid);
        var self = this;
        var payload = {site:NSN.siteID,node:nodeid,from:from,to:to, fixture:fixtureid, schedule:scheduleid};
        console.log("payload",payload);
        // Get Looker Url:
        $.ajax({
          url: '/getEnergyLookerUrl', 
          data : payload,
          method : 'POST',
          xhrFields: {
             withCredentials: true
          },
          success : function(data){

            self.setState({url:data.url, hostnameDetails:data.hostnameDetails, looker_production: data.lookerproduction1});
          },
          error : function(jqXHR, status, error){
            console.log("ajax failure (looker): " + status + " - " + error);
            $("#loadingmsg").html("Cannot retrieve looker list.  API call failed.");
          }
        });
    },
    componentDidMount: function() {
        var self = this;
        self.init();

        ReactBootstrap.Dispatcher.on('EnergyList.selectTimeframe', function(sensortimeframe) {
            self.setState({
                selected_timeframe: _.find(self.state.sensors, {
                    id: sensortimeframe
                })
            });
        });

        ReactBootstrap.Dispatcher.on('EnergyList.selectConversion', function(conversionIDs) {
            self.setState({
                selected_conversions: conversionIDs,
            });
        });
        
        ReactBootstrap.Dispatcher.on('EnergyList.selectNodes', function(nodeIDs) {            
            if(nodeIDs == "All Nodes"){
                async.parallel({
                    site: self.get_site
                },
                function(err, results) {
                    if (err) {
                        console.log(err);
                        $('#loadingmsg').html('Failed to retrieve API data');
                    } else {
                        self.setState({
                            selected_sites: results.site,
                            selected_nodes: null
                        });
                    }
                });
                var fromDate =  moment(self.state.from).tz(self.state.time_zone).format('YYYY/MM/DD');
                var toDate =  moment(self.state.to).add(1, 'days').tz(self.state.time_zone).format('YYYY/MM/DD');
                var fixtures = (self.state.selected_fixture== null ? "": self.state.selected_fixture);            
                var fixtureid=(fixtures==""?"":fixtures[0].fixtureid);
                var groups = (self.state.selected_group== null ? "": self.state.selected_group);            
                var groupid=(groups==""?"":groups[0].groupid); 
                self.lookerData("",fromDate,toDate,fixtureid,groupid);
            }
            else{
                var selected_nodes = nodeIDs.map(function(nodeID) {
                    return _.find(self.state.nodes, {
                        nodeid: nodeID
                    });
                });
                self.setState({
                    selected_nodes: selected_nodes,
                    selected_sites: null
                });
                var fromDate =  moment(self.state.from).tz(self.state.time_zone).format('YYYY/MM/DD');
                var toDate =  moment(self.state.to).add(1, 'days').tz(self.state.time_zone).format('YYYY/MM/DD');
                var fixtures = (self.state.selected_fixture== null ? "": self.state.selected_fixture);            
                var fixtureid=(fixtures==""?"":fixtures[0].fixtureid);
                var groups = (self.state.selected_group== null ? "": self.state.selected_group);            
                var groupid=(groups==""?"":groups[0].groupid);              
                self.lookerData(selected_nodes[0].nodeid,fromDate,toDate,fixtureid,groupid);
            }
           
        });
        ReactBootstrap.Dispatcher.on('EnergyList.selectFixtures', function(fixtureIDs){
                    var selected_fixture = fixtureIDs.map(function(fixtureID) {
                    return _.find(self.state.fixtures, {
                        fixtureid : fixtureID
                    });
                });
                self.setState({
                    selected_fixture: selected_fixture,
                    
                });
                var fromDate =  moment(self.state.from).tz(self.state.time_zone).format('YYYY/MM/DD');
                var toDate =  moment(self.state.to).add(1, 'days').tz(self.state.time_zone).format('YYYY/MM/DD');
                var nodes = (self.state.selected_nodes== null ? "": self.state.selected_nodes);
                var nodeid=(nodes==""?"":nodes[0].nodeid);
                var groups = (self.state.selected_group== null ? "": self.state.selected_group);            
                var groupid=(groups==""?"":groups[0].groupid); 
                var fixtureid=(selected_fixture[0]== undefined ? "":selected_fixture[0].fixtureid)
                self.lookerData(nodeid,fromDate,toDate,fixtureid,groupid);
        });
        // ReactBootstrap.Dispatcher.on('EnergyList.selectSchedule', function(scheduleIDs){
        //             var selected_schedule = scheduleIDs.map(function(scheduleIDs) {
        //             return _.find(self.state.schedules, {
        //                 scheduleid : scheduleIDs
        //             });
        //         });
        //         self.setState({
        //             selected_schedule: selected_schedule,
                    
        //         });
        //         var fromDate =  moment(self.state.from).tz(self.state.time_zone).format('YYYY/MM/DD');
        //         var toDate =  moment(self.state.to).add(1, 'days').tz(self.state.time_zone).format('YYYY/MM/DD');
        //         var nodes = (self.state.selected_nodes== null ? "": self.state.selected_nodes);
        //         var nodeid=(nodes==""?"":nodes[0].nodeid);
        //         var fixtures = (self.state.selected_fixture== null ? "": self.state.selected_fixture);            
        //         var fixtureid=(fixtures==""?"":fixtures[0].fixtureid);
        //         self.lookerData(nodeid,fromDate,toDate,fixtureid,selected_schedule[0].scheduleid);
        // });
                ReactBootstrap.Dispatcher.on('EnergyList.selectGroup', function(groupIDs){
                    var selected_group = groupIDs.map(function(groupIDs) {
                    return _.find(self.state.groups, {
                        groupid : groupIDs
                    });
                });
                self.setState({
                    selected_group: selected_group,
                    
                });
                var fromDate =  moment(self.state.from).tz(self.state.time_zone).format('YYYY/MM/DD');
                var toDate =  moment(self.state.to).add(1, 'days').tz(self.state.time_zone).format('YYYY/MM/DD');
                var nodes = (self.state.selected_nodes== null ? "": self.state.selected_nodes);
                var nodeid=(nodes==""?"":nodes[0].nodeid);
                var fixtures = (self.state.selected_fixture== null ? "": self.state.selected_fixture);            
                var fixtureid=(fixtures==""?"":fixtures[0].fixtureid);
                var groupid=(selected_group[0]== undefined ? "":selected_group[0].groupid)
                self.lookerData(nodeid,fromDate,toDate,fixtureid,groupid);
        });
        ReactBootstrap.Dispatcher.on('EnergyList.selectFrom', function(date) {
            self.setState({
                from: date
            });
            var fromDate =  moment(self.state.from).tz(self.state.time_zone).format('YYYY/MM/DD');
            var toDate =  moment(self.state.to).add(1, 'days').tz(self.state.time_zone).format('YYYY/MM/DD');
            var nodes = (self.state.selected_nodes== null ? "": self.state.selected_nodes);
            var nodeid=(nodes==""?"":nodes[0].nodeid);
            var fixtures = (self.state.selected_fixture== null ? "": self.state.selected_fixture);            
            var fixtureid=(fixtures==""?"":fixtures[0].fixtureid);
                var groups = (self.state.selected_group== null ? "": self.state.selected_group);            
                var groupid=(groups==""?"":groups[0].groupid); 
            self.lookerData(nodeid,fromDate,toDate,fixtureid,groupid);
        });

        ReactBootstrap.Dispatcher.on('EnergyList.selectTo', function(date) {
            self.setState({
                to: date
            });
            var fromDate =  moment(self.state.from).tz(self.state.time_zone).format('YYYY/MM/DD');
            var toDate =  moment(self.state.to).add(1, 'days').tz(self.state.time_zone).format('YYYY/MM/DD');
            var nodes = (self.state.selected_nodes== null ? "": self.state.selected_nodes);
            var fixtures = (self.state.selected_fixture== null ? "": self.state.selected_fixture);
            var nodeid=(nodes==""?"":nodes[0].nodeid);
            var fixtureid=(fixtures==""?"":fixtures[0].fixtureid);
                var groups = (self.state.selected_group== null ? "": self.state.selected_group);            
                var groupid=(groups==""?"":groups[0].groupid); 
            self.lookerData(nodeid,fromDate,toDate,fixtureid,groupid);
        });
        ReactBootstrap.Dispatcher.on('EnergyList.downloadPDF', function(btn) {
            var fixtures = (self.state.selected_fixture== null ? "": self.state.selected_fixture);
            var groups = (self.state.selected_group== null ? "": self.state.selected_group); 
            var schedules = (self.state.selected_schedule== null ? "": self.state.selected_schedule); 
            var nodes = (self.state.selected_nodes== null ? "": self.state.selected_nodes);
            var filter_config = {
                "Siteid": [{
                  "type": "=",
                  "values": [{
                    "constant": self.state.siteID
                  }, {}],
                  "id": 0
                }],
                "Days": [{
                  "type": "between",
                  "values": [{
                    "date": self.state.from,
                    "tz": true
                  }, {
                    "date": self.state.to,
                    "tz": true
                  }],
                  "id": 1
                }],
                "Node": [{
                  "type": "=",
                  "values": [{
                    "constant": (nodes==""?"":nodes[0].nodeid)
                  }, {}],
                  "id": 2
                }],
                "Fixtureid": [{
                  "type": "=",
                  "values": [{
                    "constant": (fixtures==""?"":fixtures[0].fixtureid)
                  }, {}],
                  "id": 3
                }],
                "Scheduleid": [{
                  "type": "=",
                  "values": [{
                    "constant": (groups==""?"":groups[0].groupid)
                  }, {}],
                  "id": 4
                }],
                "Resolution": [{
                  "type": "=",
                  "values": [{
                    "constant": "1hr"
                  }, {}],
                  "id": 5
                }]
              } ;
                  //var hostname = process.env.looker_url;
                  var hostnameDetails = self.state.hostnameDetails;
                  var dashboardDetails;
                  // if(typeof hostname != "undefined"){
                  //     hostnameDetails = hostname;
                  // }
                  // else{
                  //     hostnameDetails = 'analytics-dev.sensity.com:9999';
                  // }

                  //var lookerproduction = process.env.looker_production;
                  var lookerproduction = self.state.looker_production;
                  if(typeof lookerproduction != "undefined"){
                      lookerproduction = lookerproduction.toLowerCase();
                  }
                  
                  if(lookerproduction == "true"){
                      dashboardDetails = "/embed/dashboards/189"
                  }   

                  else{
                      dashboardDetails = "/embed/dashboards/181"
                  }

            var initialURL = "https://"+hostnameDetails+"/render/process/wd/1280/1"+dashboardDetails+".pdf?Siteid="+self.state.siteID+"&Node="+(nodes==""?"":nodes[0].nodeid)+"&Days="+moment(self.state.from).tz('UTC').format('YYYY/MM/DD')+" to "+moment(self.state.to).add(1, 'days').tz('UTC').format('YYYY/MM/DD')+"&Fixtureid="+(fixtures==""?"":fixtures[0].fixtureid)+"&Scheduleid="+(groups==""?"":groups[0].groupid)+"&Resolution=1hr&";
            var finalURL = "&download=yes&filename=Energy Savings Dashboard.pdf&title=Energy Savings Dashboard";
            var pdfDownloadUrl = initialURL+JSON.stringify(filter_config)+finalURL;
            console.log("sampleurl",pdfDownloadUrl);
            $('#downloadEnergyImage').attr({
              href: pdfDownloadUrl,
              "target": "_blank"                   
          });
            });
    },
    componentWillUnmount: function() {
        ReactBootstrap.Dispatcher.removeAllListeners('EnergyList.selectTimeframe');
        ReactBootstrap.Dispatcher.removeAllListeners('EnergyList.selectNodes');
        ReactBootstrap.Dispatcher.removeAllListeners('EnergyList.selectFixtures');
        ReactBootstrap.Dispatcher.removeAllListeners('EnergyList.selectGroup')
        ReactBootstrap.Dispatcher.removeAllListeners('EnergyList.selectFrom');
        ReactBootstrap.Dispatcher.removeAllListeners('EnergyList.selectTo');
        ReactBootstrap.Dispatcher.removeAllListeners('EnergyList.downloadPDF'); 
    },
    init: function() {
        var self = this;

        if (NSN.customerID === '-1') {
            $('#loadingmsg').html('Please select an Account first.');
            return;
        }

        if (NSN.siteID === '-1') {
            $('#loadingmsg').html('Please select a Site first.');
            return;
        }
        async.parallel({
                nodes: self.get_nodes,
                selected_sites: self.get_site,
                site: self.get_site
            },
            function(err, results) {
                if (err) {
                    console.log(err);
                    $('#loadingmsg').html('Failed to retrieve API data');
                } else {
                    var tz = (results.site && !_.isEmpty(results.site.time_zone)) ? results.site.time_zone : 'UTC';

                    self.setState({
                        sensors: helpers.getEnergyList(),
                        nodes: results.nodes,
                        selected_sites: results.site,
                        from: moment().tz(tz).subtract(7, 'days'),
                        to: moment().tz(tz),
                        time_zone: tz
                    });
                var fromDate =  moment(self.state.from).tz(self.state.time_zone).format('YYYY/MM/DD');
                var toDate =  moment(self.state.to).add(1, 'days').tz(self.state.time_zone).format('YYYY/MM/DD');
                    self.lookerData("",fromDate,toDate,"","");
                }
            }); 
        self.getAllFixtures();
        self.getSchedules(); 
        self.getAllLightingGroups();          
    },
    get_nodes: function(callback) {
        var self = this;
        DataUtil.getAll('energy-minnodes',self.processMinnodes,callback);
    },
    //////////callback to get minnodes////////////
    processMinnodes: function(data) {
        var callb=data.callback;        
                callb(data.errors, data);
            },
    get_site: function(callback) {
        var self = this;
        DataUtil.getAll('site',self.processMinnodes,callback)
    },
    getSchedules: function (){
        var that = this;

     $.ajax({
      url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/schedules',
      data: '',
      method: 'GET',
      "xhrFields": {
        withCredentials: true
      },
      dataType: 'json',
      success: function (data) {
        console.log("ajax success: schedule in energy looker " + JSON.stringify(data));
        $("#loadingmsg").html("Generating display.");
        if (data == "") {
          that.setState({ schedules: [] });
        } else {
          that.setState({
            schedules: data.map(function (schedule, index) {
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
    },
  getAllLightingGroups: function(){
    var that = this;
        $.ajax({
      url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/groups', 
      data : '',
      method : 'GET',
      "xhrFields": {
         withCredentials: true
      },
      dataType : 'json',
      success : function(data){
          console.log("ajax success: groups in energy looker " + JSON.stringify(data));
          that.setState({
            groups: data.filter(function(group, index) {
              return group.type == "lighting";
            }).map(function(group, index) {
              group.idx = index;
              return group;
            })
          })
      },
      error : function(){
        $("#loadingmsg").html("Cannot get groups.  API call failed.")
      }
    });
  },

getAllFixtures: function() {
    var that = this;
    //Fixtures:
    DataUtil.getAll('fixtures', that.processFixtureData);
  },
  ////////////////callback function to get all fixtures////////////
  processFixtureData: function (data) {
    if (data.errors) {
      console.log("/customers/" + NSN.customerID + "/sites/" + NSN.siteID + "/fixtures API returned error: " + JSON.stringify(data));
      $("#loadingmsg").html("Cannot retrieve fixtures list.  API returned: " + JSON.stringify(data));
    } else {
      console.log("ajax success: " + JSON.stringify(data));
      this.setState(DataUtil.assignState('fixtures', data, this, this.makeFixturesObj))
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

    render: function() {
      var self = this;
      var sensorList = (
          <Col md={12} lg={2}>
            <PanelContainer>
              <Panel>
                <PanelBody>
                  <EnergyList sensors={this.state.sensors} 
                              nodes={this.state.nodes} 
                              sites={this.state.sites}  
                              from={this.state.from} 
                              to={this.state.to} 
                              time_zone={this.state.time_zone} 
                              fixtures={this.state.fixtures} 
                              groups={this.state.groups}/>
                </PanelBody>
              </Panel>
            </PanelContainer>
          </Col>
          );
      var graphPanel = (
          <Col md={12} lg={10}>
            <PanelContainer>
              <Panel>
                <PanelBody>
                  <div id="savings" style={{"textAlign":"center","color":"#666666"}}> 
                  </div>
                  <iframe src={this.state.url} style={{border:"0", position:"absolute",width:"99%", height:"95%"}}></iframe>
                </PanelBody>
              </Panel>
            </PanelContainer>
          </Col>
          );

      if (this.state.groups && this.state.fixtures 
          && ((this.state.nodes && this.state.nodes.length > 0) 
              || (this.state.sites && this.state.sites.length > 0))) {
        if ((this.state.selected_nodes != null) || (this.state.selected_sites != null)) {
          return (
            <Container id="body">
              <Grid>
                <Row>
                  {sensorList}
                  {graphPanel}
                </Row>
              </Grid>
            </Container>
            );
        } else {
          return (
            <Container id="body">
              <Grid>
                <Row>
                  {sensorList}
                  <Col  md={12} lg={10}>
                    <PanelContainer>
                      <Panel>
                        <PanelBody>
                          <h2 id="loadingmsg" style={{ paddingTop:'16%',textAlign:'center' }}>
                            Select a node/site
                          </h2>
                        </PanelBody>
                      </Panel>
                    </PanelContainer>
                  </Col>
                </Row>
              </Grid>
            </Container>
            );
          }
      } else {
        return (
          <Container id='body'>
            <Grid>
              <Row>
                <Col sm={12}>
                  <PanelContainer>
                    <Panel>
                      <PanelBody>
                        <h2 id="loadingmsg" style={{ paddingTop:'16%',textAlign:'center' }}>
                          Generating report...
                        </h2>
                      </PanelBody>
                    </Panel>
                  </PanelContainer>
                </Col>
              </Row>
            </Grid>
          </Container>
        );
      }
    },

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