// import classNames from 'classnames';
// import helpers from 'global/utils/helpers';
// import Header from 'common/headernew';
// import EnergyList from 'components/energy/energylist';
// import EnergyGraph from 'components/energy/energygraph';
// import DataUtil from '../service/datautil';

// var Body = React.createClass({
//     getInitialState: function() {
//         return {
//             siteID: NSN.siteID,
//             customerID: NSN.customerID,
//             sensors: null,
//             nodes: null,
//             sites: null,
//             selected_timeframe: null,
//             selected_nodes: null,
//             selected_sites: null,
//             selected_conversions: ["kWh"],
//             from: null,
//             to: null,
//             time_zone: null
//         }
//     },
//     componentDidMount: function() {
//         var self = this;

//         ReactBootstrap.Dispatcher.on('EnergyList.selectTimeframe', function(sensortimeframe) {
//             self.setState({
//                 selected_timeframe: _.find(self.state.sensors, {
//                     id: sensortimeframe
//                 })
//             });
//         });

//         ReactBootstrap.Dispatcher.on('EnergyList.selectConversion', function(conversionIDs) {
//             self.setState({
//                 selected_conversions: conversionIDs,
//             });
//         });
        
//         ReactBootstrap.Dispatcher.on('EnergyList.selectNodes', function(nodeIDs) {            
//             if(nodeIDs == "All Nodes"){
//                 async.parallel({
//                     site: self.get_site
//                 },
//                 function(err, results) {
//                     if (err) {
//                         console.log(err);
//                         $('#loadingmsg').html('Failed to retrieve API data');
//                     } else {
//                         self.setState({
//                             selected_sites: results.site,
//                             selected_nodes: null
//                         });
//                     }
//                 });
//             }
//             else{
//                 var selected_nodes = nodeIDs.map(function(nodeID) {
//                     return _.find(self.state.nodes, {
//                         nodeid: nodeID
//                     });
//                 });

//                 self.setState({
//                     selected_nodes: selected_nodes,
//                     selected_sites: null
//                 });
//             }
//         });

//         ReactBootstrap.Dispatcher.on('EnergyList.selectFrom', function(date) {
//             self.setState({
//                 from: date
//             });
//         });

//         ReactBootstrap.Dispatcher.on('EnergyList.selectTo', function(date) {
//             self.setState({
//                 to: date
//             });
//         });

//         self.init();
//     },
//     componentWillUnmount: function() {
//         ReactBootstrap.Dispatcher.removeAllListeners('EnergyList.selectTimeframe');
//         ReactBootstrap.Dispatcher.removeAllListeners('EnergyList.selectNodes');
//         ReactBootstrap.Dispatcher.removeAllListeners('EnergyList.selectFrom');
//         ReactBootstrap.Dispatcher.removeAllListeners('EnergyList.selectTo');
//     },
//     render: function() {
//         var self = this;

//         var sensorList = (
//                 <Col  md={12} lg={2}>
//                 <PanelContainer>
//                     <Panel>
//                         <PanelBody>
//                             <EnergyList sensors={self.state.sensors} nodes={self.state.nodes} sites={self.state.sites}  from={self.state.from} to={self.state.to} time_zone={self.state.time_zone} />
//                         </PanelBody>
//                     </Panel>
//                 </PanelContainer>
//             </Col>),
//             graphPanel = (
//                 <Col md={12} lg={9}>
//                     <PanelContainer>
//                         <Panel>
//                             <PanelBody>
//                                 <div id="savings" style={{"textAlign":"center","color":"#666666"}}> 
//                                 </div>
//                                 <EnergyGraph sensors={self.state.sensors} selected_timeframe={self.state.selected_timeframe} selected_conversions={self.state.selected_conversions} selected_nodes={self.state.selected_nodes} selected_sites={self.state.selected_sites} from={self.state.from} to={self.state.to} time_zone={self.state.time_zone} />
//                             </PanelBody>
//                         </Panel>
//                     </PanelContainer>
//                 </Col>
//             );

//         if ((self.state.nodes && self.state.nodes.length > 0) || (self.state.sites && self.state.sites.length > 0)) {
//             if ((self.state.selected_nodes != null) || (self.state.selected_sites != null)) {
//                 return (
//                     <Container id="body">
//                         <Grid>
//                             <Row>
//                                 {sensorList}
//                                 {graphPanel}
//                             </Row>
//                         </Grid>
//                     </Container>
//                 );
//             } else {
//                 return (
//                     <Container id="body">
//                         <Grid>
//                             <Row>
//                                 {sensorList}
//                                 <Col  md={12} lg={10}>
//                                     <PanelContainer>
//                                         <Panel>
//                                             <PanelBody>
//                                                 <h2 id="loadingmsg" style={{ paddingTop:'16%',textAlign:'center' }}>
//                                                     Select a node/site
//                                                 </h2>
//                                             </PanelBody>
//                                         </Panel>
//                                     </PanelContainer>
//                                 </Col>
//                             </Row>
//                         </Grid>
//                     </Container>
//                 );
//             }
//         } else {
//             return (
//                 <Container id='body'>
//                     <Grid>
//                         <Row>
//                             <Col sm={12}>
//                                 <PanelContainer>
//                                     <Panel>
//                                         <PanelBody>
//                                             <h2 id="loadingmsg" style={{ paddingTop:'16%',textAlign:'center' }}>
//                                                 Generating report...
//                                             </h2>
//                                         </PanelBody>
//                                     </Panel>
//                                 </PanelContainer>
//                             </Col>
//                         </Row>
//                     </Grid>
//                 </Container>
//             );
//         }
//     },
//     init: function() {
//         var self = this;

//         if (NSN.customerID === '-1') {
//             $('#loadingmsg').html('Please select an Account first.');
//             return;
//         }

//         if (NSN.siteID === '-1') {
//             $('#loadingmsg').html('Please select a Site first.');
//             return;
//         }
//         async.parallel({
//                 nodes: self.get_nodes,
//                 selected_sites: self.get_site,
//                 site: self.get_site
//             },
//             function(err, results) {
//                 if (err) {
//                     console.log(err);
//                     $('#loadingmsg').html('Failed to retrieve API data');
//                 } else {
//                     var tz = (results.site && !_.isEmpty(results.site.time_zone)) ? results.site.time_zone : 'UTC';

//                     self.setState({
//                         sensors: helpers.getEnergyList(),
//                         nodes: results.nodes,
//                         selected_sites: results.site,
//                         from: moment().tz(tz).subtract(1, 'days'),
//                         to: moment().tz(tz).subtract(2, 'hours'),
//                         time_zone: tz
//                     });
//                 }
//             });

//     },
//     get_nodes: function(callback) {
//         var self = this;
//         DataUtil.getAll('energy-minnodes',self.processMinnodes,callback);
//         // $.ajax({
//         //     url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/minnodes',
//         //     data: '',
//         //     method: 'GET',
//         //     xhrFields: {
//         //         withCredentials: true
//         //     },
//         //     dataType: 'json',
//         //     success: function(data) {
//         //         callback(data.errors, data);
//         //     },
//         //     error: function(jqXHR, status, error) {
//         //         callback(error);
//         //     }
//         // });
//     },
//     //////////callback to get minnodes////////////
//     processMinnodes: function(data) {
//         console.log("data in energy pan",data)

//         var callb=data.callback;
        
//         console.log("callb: "+callb);
        
//                 callb(data.errors, data);
//             },
//     get_site: function(callback) {
//         var self = this;
//         DataUtil.getAll('site',self.processMinnodes,callback)
//         // $.ajax({
//         //     url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID,
//         //     data: '',
//         //     method: 'GET',
//         //     xhrFields: {
//         //         withCredentials: true
//         //     },
//         //     dataType: 'json',
//         //     success: function(data) {
//         //         callback(data.errors, data);
//         //     },
//         //     error: function(jqXHR, status, error) {
//         //         callback(error);
//         //     }
//         // });
//     }
// });

// export default class extends React.Component {
//     render() {
//         var classes = classNames({
//             'container-open': this.props.open
//         });

//         return (
//             <Container id='container' className={classes}>
//                 <Header />
//                 <Body />
//             </Container>
//         );
//     }
// }