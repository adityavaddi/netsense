import classNames from 'classnames';
import helpers from 'global/utils/helpers';

import { State, Navigation } from 'react-router';

var SensorMonitor = React.createClass({
  getInitialState: function () {
    var values = [];
    for (var i=0; i<20; i++) {
      values.push(["-","-"]);
    };
    return {values: values};
  },

  propTypes: {
    selected_sensor1: React.PropTypes.object.isRequired,
    selected_sensor2: React.PropTypes.object,
    selected_nodes: React.PropTypes.array.isRequired,
    showmonitor: React.PropTypes.bool.isRequired
  },

  intervals: [],

  getSensor: function(type, node, i, j) {
    var that = this;
    var from = new Date(), to = new Date();
    from.setTime(from.getTime() - 3600 * 1000);
    to.setTime(to.getTime() + 60000);
    $.ajax({
      url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/nodes/' + node
            + '/sensors/' + type + '/from/' + from.toISOString() + '/to/' + to.toISOString() + '/limit/1',
      data: '',
      method: 'GET',
      xhrFields: {
          withCredentials: true
      },
      dataType: 'json',
      success: function(data) {
          console.log(JSON.stringify(data));
          $("#sensorDisplayTime").html(moment().format('MMMM Do YYYY, h:mm:ss a'));
          var state = Object.assign({}, that.state);
          if (data.datapoints.length> 0 && (type == "tsys" || type == "tcore")) {
            data.datapoints[0].value /= 1000.0;
          }
          state.values[j][i] = data.datapoints.length>0?data.datapoints[0].value:"n/a";
          that.setState(state);
      },
      error: function(jqXHR, status, error) {
          console.log("error: "+ status);
      }
    })
  },

  launch: function() {
    this.setState(this.getInitialState());
    var sensortypes = [this.props.selected_sensor1.id];
    if ((typeof this.props.selected_sensor2 != "undefined") && (this.props.selected_sensor2 !== null)) {
      sensortypes.push(this.props.selected_sensor2.id);
    }
    for (var i=0; i<sensortypes.length; i++) {
      for (var j=0; j<this.props.selected_nodes.length; j++) {
        var sensor = sensortypes[i];
        if (sensor == "l-i") {
          var model = this.props.selected_nodes[j].model;
          if (model == "unode-v2" || model == "unode-v3" || model == "unode-v4") {
            sensor = "l";
          };
        };
        if (sensor == "l") {
          var model = this.props.selected_nodes[j].model;
          if (model == "unode-v5" || model == "unode-v6") {
            sensor = "l-i";
          };
        };
        this.intervals.push(setInterval(this.getSensor, 3000, sensor, this.props.selected_nodes[j].nodeid, i, j))
      }
    }
  },

  cleanup: function() {
    for (var i=0; i<this.intervals.length; i++) {
      clearInterval(this.intervals[i]);
    }
  },

  handleClose: function() {
    ReactBootstrap.Dispatcher.emit('ReportingList.monitor', 'close');
  },


  componentDidUpdate: function(){
    var that = this;
    if (this.props.showmonitor) {
      if (!$(".sensor-display").is(":visible")) {
        $("#sensorMonitorOverlay").show();
        $(".sensor-display").fadeIn('fast', function(){
          that.launch();
        });
      }
    } else {
      if ($(".sensor-display").is(":visible")) {
        $("#sensorMonitorOverlay").fadeOut('fast');
        $(".sensor-display").fadeOut('fast');
        this.cleanup();
      }
    }
  },

  componentDidMount: function(){
    this.intervals = [];
    this.launch();
  },

  componentWillUnmount: function(){
    this.cleanup();
  },

  render() {
    var that = this;
    var nos2 = (typeof this.props.selected_sensor2 == "undefined") || (this.props.selected_sensor2 === null);
    return (   
                    <div className="sensor-display" style={{zIndex:9999999,width:"800px",left:"30%",top:"15%"}}>
                    <div id="sensorDisplayTime" style={{position:"absolute",fontSize:"16px",top:"12px",right:"16px"}}></div>
                    <div style={{padding:"10px 80px 30px"}}>
                      <h2>Real-time Sensor Values</h2>
                      <div style={{margin:"0 auto"}}>
                        <table style={{width:"600px",fontSize:"18px"}}>
                          <thead>
                          <tr>
                            <th style={{textAlign:"center"}}>Node</th>
                            <th style={{textAlign:"center",width:"30%"}}>{this.props.selected_sensor1.name} {this.props.selected_sensor1.units}</th>
                            <th style={{textAlign:"center",width:"30%"}}>{nos2?"":this.props.selected_sensor2.name} {nos2?"":this.props.selected_sensor2.units}</th>
                          </tr>
                          </thead>
                          <tbody>
                          {this.props.selected_nodes.map(function(node, index) {
                            var idx = {index};
                            return <tr key={index}>
                              <td style={{textAlign:"center"}}>{node.name || node.nodeid}</td>
                              <td style={{textAlign:"center"}}>{that.state.values[idx.index][0]}</td>
                              <td style={{textAlign:"center"}}>{nos2?"":that.state.values[idx.index][1]}</td>
                            </tr>;
                          })}
                          </tbody>
                        </table>
                      </div>
                      <div style={{position:"absolute",bottom:"10px",right:"20px"}}>
                        <button className="ns-cancel-btn" id="closeModal" onClick={this.handleClose}>Close</button>
                      </div>
                    </div>
                  </div>
    );
  }
});



module.exports = SensorMonitor;
