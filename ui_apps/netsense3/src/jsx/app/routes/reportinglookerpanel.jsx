import classNames from 'classnames';
import helpers from 'global/utils/helpers';
import sensors from 'global/utils/sensors';
import Header from 'common/headernew';
import ReportingList from 'components/reporting/reportinglist';
import ReportingGraph from 'components/reporting/reportinggraph';
import SensorMonitor from 'components/sensormonitor';

var Body = React.createClass({
    getInitialState: function() {
        return {
            siteID: NSN.siteID,
            customerID: NSN.customerID,
            sensors: null,
            nodes: null,
            nodetypes: [],
            selected_sensor1: null,
            selected_sensor2: null,
            selected_nodes: null,
            selected_nodes_string: null,
            from: null,
            to: null,
            fromtime: null,
            totime: null,
            time_zone: null,
            showmonitor: false,
            sensName1: null,
            sensName2: null,
            sensUnit1: null,
            sensUnit2: null
        }
    },

        lookerData: function (nodeid,from,to,sensor1id,sensor2id,fromtime,totime,timeZone) {
            console.log("nodeid,from,to,sensor1id,sensor2id,fromtime,totime",nodeid,from,to,sensor1id,sensor2id,fromtime,totime,timeZone);
        nodeid= (nodeid==undefined? " ":nodeid);
        var self = this;
        var payload = {site:NSN.siteID,node:nodeid,from:from,to:to, sensor1:sensor1id, sensor2:sensor2id, fromtime:fromtime, totime:totime, timeZone:timeZone};
        console.log("payload",payload);
        // Get Looker Url:
        $.ajax({
          url: '/getSensorLookerUrl', 
          data : payload,
          method : 'POST',
          xhrFields: {
             withCredentials: true
          },
          success : function(data){
              self.setSensor1Details(sensor1id);
              self.setSensor2Details(sensor2id);
              self.setState({url:data.url, hostnameDetails:data.hostnameDetails, looker_production: data.lookerproduction1});
          },
          error : function(jqXHR, status, error){
            console.log("ajax failure (looker): " + status + " - " + error);
            $("#loadingmsg").html("Cannot retrieve looker list.  API call failed.");
          }
        });
    },

    setSensor1Details: function(sensorid){
        for (var si in this.state.sensors){
            if(sensorid==this.state.sensors[si].id){
             this.setState({
                sensName1: this.state.sensors[si].name,
                sensUnit1: this.state.sensors[si].units
            });
            break;
            }
        }
    },
    setSensor2Details: function(sensorid){
        for (var si in this.state.sensors){
            if(sensorid==this.state.sensors[si].id){
             this.setState({
                sensName2: this.state.sensors[si].name,
                sensUnit2: this.state.sensors[si].units
            });
            break;
        }
    }
    },

    componentDidMount: function() {
        var that = this;

        ReactBootstrap.Dispatcher.on('ReportingList.selectSensor1', function(sensorID) {
            that.setState({
                selected_sensor1: _.find(that.state.sensors, {
                    id: sensorID
                })
            });
            var fromDate =  moment(that.state.from).subtract(0, 'days').tz('UTC').format('YYYY/MM/DD');
                var toDate =  moment(that.state.to).add(1, 'days').tz('UTC').format('YYYY/MM/DD');
                                var fromTime =  moment(that.state.from).tz('UTC').format('YYYY/MM/DD HH:MM');
                var toTime =  moment(that.state.to).tz('UTC').format('YYYY/MM/DD HH:MM');
                var timeZone = that.state.time_zone;
                if(that.state.selected_nodes_string!=null){
                    var  nstring = that.state.selected_nodes_string.toString();
                    }
            that.lookerData(that.state.selected_nodes_string==null?"":nstring,fromDate,toDate,sensorID,that.state.selected_sensor2==null?"Select":that.state.selected_sensor2.id,fromTime,toTime,timeZone);
        });

        ReactBootstrap.Dispatcher.on('ReportingList.selectSensor2', function(sensorID) {
            that.setState({
                selected_sensor2: _.find(that.state.sensors, {
                    id: sensorID
                })
            });
                        var fromDate =  moment(that.state.from).subtract(0, 'days').tz('UTC').format('YYYY/MM/DD');
                var toDate =  moment(that.state.to).add(1, 'days').tz('UTC').format('YYYY/MM/DD');
                                var fromTime =  moment(that.state.from).tz('UTC').format('YYYY/MM/DD HH:MM');
                var toTime =  moment(that.state.to).tz('UTC').format('YYYY/MM/DD HH:MM');
                var timeZone = that.state.time_zone;
                if(that.state.selected_nodes_string!=null){
                var  nstring = that.state.selected_nodes_string.toString();
                }
            that.lookerData(that.state.selected_nodes_string==null?"":nstring,fromDate,toDate,that.state.selected_sensor1==null?"":that.state.selected_sensor1.id,sensorID=="0"?"Select":sensorID,fromTime,toTime,timeZone);
        });

        ReactBootstrap.Dispatcher.on('ReportingList.selectNodes', function(nodeIDs) {
            console.log("nodeIDs in ReportingList.selectNodes",nodeIDs);
            var selected_nodes = nodeIDs.map(function(nodeID) {
                return _.find(that.state.nodes, {
                    nodeid: nodeID
                });
            });
            var selected_nodes_string= [];
            for (var i = 0; i < selected_nodes.length; i++) {
                selected_nodes_string.push(selected_nodes[i].nodeid);
            }
            that.setState({
                selected_nodes: selected_nodes,
                selected_nodes_string:selected_nodes_string
            });
            var  nstring = nodeIDs.toString();
                        var fromDate =  moment(that.state.from).subtract(0, 'days').tz('UTC').format('YYYY/MM/DD');
                var toDate =  moment(that.state.to).add(1, 'days').tz('UTC').format('YYYY/MM/DD');
                                var fromTime =  moment(that.state.from).tz('UTC').format('YYYY/MM/DD HH:MM');
                var toTime =  moment(that.state.to).tz('UTC').format('YYYY/MM/DD HH:MM');
                var timeZone = that.state.time_zone;
            that.lookerData(nstring,fromDate,toDate,that.state.selected_sensor1==null?"":that.state.selected_sensor1.id,that.state.selected_sensor2==null?"Select":that.state.selected_sensor2.id,fromTime,toTime,timeZone);
        });

        ReactBootstrap.Dispatcher.on('ReportingList.selectFrom', function(date) {
            that.setState({
                from: date
            });
                        var fromDate =  moment(that.state.from).subtract(0, 'days').tz('UTC').format('YYYY/MM/DD');
                var toDate =  moment(that.state.to).add(1, 'days').tz('UTC').format('YYYY/MM/DD');
                                var fromTime =  moment(that.state.from).tz('UTC').format('YYYY/MM/DD HH:MM');
                var toTime =  moment(that.state.to).tz('UTC').format('YYYY/MM/DD HH:MM');
                var timeZone = that.state.time_zone;
                if(that.state.selected_nodes_string!=null){
                    var  nstring = that.state.selected_nodes_string.toString();
                    }
            that.lookerData(that.state.selected_nodes_string==null?"":nstring,fromDate,toDate,that.state.selected_sensor1==null?"":that.state.selected_sensor1.id,that.state.selected_sensor2==null?"Select":that.state.selected_sensor2.id,fromTime,toTime,timeZone);
        });

        ReactBootstrap.Dispatcher.on('ReportingList.selectTo', function(date) {
            that.setState({
                to: date
            });
                        var fromDate =  moment(that.state.from).subtract(0, 'days').tz('UTC').format('YYYY/MM/DD');
                var toDate =  moment(that.state.to).add(1, 'days').tz('UTC').format('YYYY/MM/DD');
                                var fromTime =  moment(that.state.from).tz('UTC').format('YYYY/MM/DD HH:MM');
                var toTime =  moment(that.state.to).tz('UTC').format('YYYY/MM/DD HH:MM');
                var timeZone = that.state.time_zone;
                if(that.state.selected_nodes_string!=null){
                    var  nstring = that.state.selected_nodes_string.toString();
                    }
            that.lookerData(that.state.selected_nodes_string==null?"":nstring,fromDate,toDate,that.state.selected_sensor1==null?"":that.state.selected_sensor1.id,that.state.selected_sensor2==null?"Select":that.state.selected_sensor2.id,fromTime,toTime,timeZone);
        });

        ReactBootstrap.Dispatcher.on("ReportingList.monitor", function(switchto){
            if(switchto=="open"){
                that.setState({showmonitor:switchto=="open"});
                $('.navbar').css({'z-index':'999'});
            }else if(switchto=="close"){
                that.setState({showmonitor:switchto=="open"});
                $('.navbar').css({'z-index':'1000'});
            }
        });
        
        that.init();

        ReactBootstrap.Dispatcher.on('ReportingGraph.downloadImg', function(btn) {
            var fromTime =  moment(that.state.from).tz('UTC').toISOString();
            var toTime =  moment(that.state.to).tz('UTC').toISOString();
            var fromT =  moment(that.state.from).tz('UTC').format('YYYY/MM/DD HH:MM');
            var toT =  moment(that.state.to).tz('UTC').format('YYYY/MM/DD HH:MM');
            var filter_config = {
                "Siteid": [{
                  "type": "=",
                  "values": [{
                    "constant": that.state.siteID
                  }, {}],
                  "id": 0
                }],                
                "Nodeid": [{
                  "type": "=",
                  "values": [{
                    "constant": that.state.selected_nodes==null?"":that.state.selected_nodes[0].nodeid
                  }, {}],
                  "id": 1
                }],
                "Day": [{
                  "type": "between",
                  "values": [{
                    "date": moment(that.state.from).subtract(0, 'days').tz('UTC').toISOString(),
                    "tz": true
                  }, {
                    "date": moment(that.state.to).add(1, 'days').tz('UTC').toISOString(),
                    "tz": true
                  }],
                  "id": 2
                }],
                "Sensor1": [{
                        "type": "=",
                        "values": [{
                            "constant": that.state.selected_sensor1==null?"":that.state.selected_sensor1.id,
                        }, {}],
                        "id": 3
                    }],
                    "Sensor2": [{
                        "type": "=",
                        "values": [{
                            "constant": that.state.selected_sensor2==null?"Select":that.state.selected_sensor2.id,
                        }, {}],
                        "id": 4
                    }],
                    "Time": [{
                        "type": "between",
                        "values": [{
                            "date":fromTime,
                            "tz": true
                        },
                        {
                            "date":toTime,
                            "tz": true
                        }
                    ],
                        "id": 5
                    }]
                };
            //var hostname = process.env.looker_url;
            var hostnameDetails = that.state.hostnameDetails;
            var dashboardDetails;
            // if(typeof hostname != "undefined"){
            //     hostnameDetails = hostname;
            // }
            // else{
            //     hostnameDetails = 'analytics-dev.sensity.com:9999';
            // }

            //var lookerproduction = process.env.looker_production;
            var lookerproduction = that.state.looker_production;
            if (typeof lookerproduction != "undefined") {
                lookerproduction = lookerproduction.toLowerCase();
            }

            if (lookerproduction == "true") {
                dashboardDetails = "/embed/dashboards/195"
            }

            else {
                dashboardDetails = "/embed/dashboards/231"
            }

            var initialURL = "https://"+hostnameDetails+"/render/process/wd/1280/1"+dashboardDetails+".pdf?Siteid="+that.state.siteID+"&Nodeid="+(that.state.selected_nodes==null?"":that.state.selected_nodes[0].nodeid)+"&Day="+moment(that.state.from).subtract(0, 'days').tz('UTC').format('YYYY/MM/DD')+" to "+moment(that.state.to).add(1, 'days').tz('UTC').format('YYYY/MM/DD')+"&Sensor1="+(that.state.selected_sensor1==null?"":that.state.selected_sensor1.id)+"&Sensor2="+(that.state.selected_sensor2==null?"Select":that.state.selected_sensor2.id)+"&Time="+fromT+" to "+toT+"&filter_config=";
            var finalURL = "&download=yes&filename=Sensor Report.pdf&title=Sensor Report";
            var pdfDownloadUrl = initialURL+JSON.stringify(filter_config)+finalURL;
            console.log("sampleurl",pdfDownloadUrl);
                $('#downloadImage').attr({
                    href: pdfDownloadUrl,
                    "target": "_blank"                   
                });
            });

        
    },
    componentWillUnmount: function() {
        ReactBootstrap.Dispatcher.removeAllListeners('ReportingList.selectSensor1');
        ReactBootstrap.Dispatcher.removeAllListeners('ReportingList.selectSensor2');
        ReactBootstrap.Dispatcher.removeAllListeners('ReportingList.selectNodes');
        ReactBootstrap.Dispatcher.removeAllListeners('ReportingList.selectFrom');
        ReactBootstrap.Dispatcher.removeAllListeners('ReportingList.selectTo');
        ReactBootstrap.Dispatcher.removeAllListeners('ReportingList.monitor');
        ReactBootstrap.Dispatcher.removeAllListeners('ReportingGraph.downloadImg');
    },
    render: function() {
        var that = this;

        var sensorList = (
                <Col md={12} lg={2}>
                <PanelContainer>
                    <Panel>
                        <PanelBody>
                             <ReportingList sensors={that.state.sensors} nodes={that.state.nodes} nodetypes={that.state.nodetypes} from={that.state.from} to={that.state.to} time_zone={that.state.time_zone} />
                        </PanelBody>
                    </Panel>
                </PanelContainer>
            </Col>),
            graphPanel = (
                <Col md={12} lg={8} id="sensor-report-panel">
                    <PanelContainer>
                        <Panel>
                            <PanelBody>
                                <iframe src={this.state.url} style={{border:"0", position:"absolute",width:"99%", height:"95%"}}></iframe>
                                
                            </PanelBody>
                            {/* <div> <p style={{textAlign:"center", height:"75%"}}><span>X1: Sensor1</span> <span>   X2:Sensor2 </span></p></div> */}
                        </Panel>
                    </PanelContainer>
                </Col>
            ),
                        leftUnitPanel = (
                <Col md={12} lg={1} id="left-report-panel">

                             <div style={{width:"10px"}} > <p id="rotateTextLeft" > Y1: {that.state.sensName1} {that.state.sensUnit1} </p>
                        </div>
                </Col>
            ),
                          rightUnitPanel = (
                <Col md={12} lg={1} id="right-report-panel">

                            <div id="rotateText" style={{width:"10px"}} ><p id= "rotateTextRight"> {that.state.sensName2 == null ? "":"Y2:"} {that.state.sensName2} {that.state.sensUnit2} </p>
                                    </div>

                </Col>
                          );


        if (that.state.sensors && that.state.sensors.length > 0 && that.state.nodes && that.state.nodes.length > 0) {
            if (that.state.selected_sensor1 != null && that.state.selected_nodes != null) {
                return (
                    <Container id="body">
                        <Grid>
                            <Row>
                                {sensorList}
                                {leftUnitPanel}
                                {graphPanel} 
                                {rightUnitPanel}
                            </Row>
                        </Grid>
                        <div>
                            {
                                that.state.showmonitor ?
                                    <div>
                                        <SensorMonitor selected_sensor1={that.state.selected_sensor1}
                                                       selected_sensor2={that.state.selected_sensor2}
                                                       selected_nodes={that.state.selected_nodes}
                                                       showmonitor={that.state.showmonitor} />
                                        <div id="sensorMonitorOverlay" className="" style={{background:"transparent url(/imgs/overlay.png) repeat top left",position:"fixed",top:"0px",bottom:"0px",left:"0px",right:"0px",zIndex:1000}}></div>
                                    </div>:<div></div>
                            }
                        </div>
                    </Container>
                );
            } else {
                return (
                    <Container id="body">
                        <Grid>
                            <Row>
                                {sensorList}
                                <Col md={12} lg={10}>
                                    <PanelContainer>
                                        <Panel>
                                            <PanelBody>
                                                <h2 id="loadingmsg" style={{ paddingTop:'16%',textAlign:'center' }}>
                                                    Select a sensor / node
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
                                                No sensors / nodes available
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
    init: function() {
        var that = this;

        if (NSN.customerID === '-1') {
            $('#loadingmsg').html('Please select an Account first.');
            return;
        }

        if (NSN.siteID === '-1') {
            $('#loadingmsg').html('Please select a Site first.');
            return;
        }

        async.parallel({
                nodes: that.get_nodes,
                site: that.get_site
            },
            function(err, results) {
                if (err) {
                    console.log(err);
                    $('#loadingmsg').html('Failed to retrieve API data');
                } else {
                    var tz = (results.site && !_.isEmpty(results.site.time_zone)) ? results.site.time_zone : 'UTC';
                    var nodetypesobj = {};
                    for (var i=0; i<results.nodes.length; i++) {
                        nodetypesobj[helpers.modelType(results.nodes[i].model)] = 1;
                    };
                    var nodetypes = Object.keys(nodetypesobj);

                    var allSensors = sensors.getSensorsList(); var newSensorsList = [];
                    for(var i in allSensors){
                        if(allSensors[i].id === "jtx" || allSensors[i].id === "jty" || allSensors[i].id === "jtz"){}else{
	                        newSensorsList.push(allSensors[i]);
                        }
                    }
                    that.setState({
                        sensors: newSensorsList,
                        nodes: results.nodes,
                        nodetypes: nodetypes,
                        from: moment().tz(tz).subtract(1, 'days'),
                        to: moment().tz(tz),
                        time_zone: tz
                    });
                var fromDate =  moment(that.state.from).subtract(0, 'days').tz('UTC').format('YYYY/MM/DD');
                var toDate =  moment(that.state.to).add(1, 'days').tz('UTC').format('YYYY/MM/DD');
                var fromTime =  moment(that.state.from).tz('UTC').format('YYYY/MM/DD HH:MM');
                var toTime =  moment(that.state.to).tz('UTC').format('YYYY/MM/DD HH:MM');
                var timeZone = that.state.time_zone;
                
                    that.lookerData("",fromDate,toDate,"","",fromTime,toTime,timeZone);
                }
            });

    },
    get_nodes: function(callback) {
        var that = this;

        $.ajax({
            url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/minnodes',
            data: '',
            method: 'GET',
            xhrFields: {
                withCredentials: true
            },
            dataType: 'json',
            success: function(data) {
                callback(data.errors, data);
            },
            error: function(jqXHR, status, error) {
                callback(error);
            }
        });
    },
    get_site: function(callback) {
        var that = this;

        $.ajax({
            url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID,
            data: '',
            method: 'GET',
            xhrFields: {
                withCredentials: true
            },
            dataType: 'json',
            success: function(data) {
                callback(data.errors, data);
            },
            error: function(jqXHR, status, error) {
                callback(error);
            }
        });
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