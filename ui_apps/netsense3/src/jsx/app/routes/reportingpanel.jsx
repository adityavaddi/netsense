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
            from: null,
            to: null,
            time_zone: null,
            showmonitor: false
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
        });

        ReactBootstrap.Dispatcher.on('ReportingList.selectSensor2', function(sensorID) {
            that.setState({
                selected_sensor2: _.find(that.state.sensors, {
                    id: sensorID
                })
            });
        });

        ReactBootstrap.Dispatcher.on('ReportingList.selectNodes', function(nodeIDs) {
            var selected_nodes = nodeIDs.map(function(nodeID) {
                return _.find(that.state.nodes, {
                    nodeid: nodeID
                });
            });

            that.setState({
                selected_nodes: selected_nodes
            });
        });

        ReactBootstrap.Dispatcher.on('ReportingList.selectFrom', function(date) {
            that.setState({
                from: date
            });
        });

        ReactBootstrap.Dispatcher.on('ReportingList.selectTo', function(date) {
            that.setState({
                to: date
            });
        });

        ReactBootstrap.Dispatcher.on("ReportingList.monitor", function(switchto){
            if(switchto=="open"){
                that.setState({showmonitor:switchto=="open"});
                $('.navbar').css({'z-index':'1'});
            }else if(switchto=="close"){
                that.setState({showmonitor:switchto=="open"});
                $('.navbar').css({'z-index':'1000'});
            }
        });



        that.init();
    },
    componentWillUnmount: function() {
        ReactBootstrap.Dispatcher.removeAllListeners('ReportingList.selectSensor1');
        ReactBootstrap.Dispatcher.removeAllListeners('ReportingList.selectSensor2');
        ReactBootstrap.Dispatcher.removeAllListeners('ReportingList.selectNodes');
        ReactBootstrap.Dispatcher.removeAllListeners('ReportingList.selectFrom');
        ReactBootstrap.Dispatcher.removeAllListeners('ReportingList.selectTo');
        ReactBootstrap.Dispatcher.removeAllListeners('ReportingList.monitor');
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
                <Col md={12} lg={10}>
                    <PanelContainer>
                        <Panel>
                            <PanelBody>
                                <ReportingGraph selected_sensor1={that.state.selected_sensor1} selected_sensor2={that.state.selected_sensor2} selected_nodes={that.state.selected_nodes} from={that.state.from} to={that.state.to} time_zone={that.state.time_zone} />
                            </PanelBody>
                        </Panel>
                    </PanelContainer>
                </Col>
            );

        if (that.state.sensors && that.state.sensors.length > 0 && that.state.nodes && that.state.nodes.length > 0) {
            if (that.state.selected_sensor1 != null && that.state.selected_nodes != null) {
                return (
                    <Container id="body">
                        <Grid>
                            <Row>
                                {sensorList}
                                {graphPanel}
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
                    that.setState({
                        sensors: sensors.getSensorsList(),
                        nodes: results.nodes,
                        nodetypes: nodetypes,
                        from: moment().tz(tz).subtract(1, 'months'),
                        to: moment().tz(tz),
                        time_zone: tz
                    });
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