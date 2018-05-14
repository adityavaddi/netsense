import classNames from 'classnames';
import { State, Navigation } from 'react-router';
import auth from 'global/utils/auth';
import helpers from 'global/utils/helpers';

var Trafficconfigform = React.createClass({

  getInitialState: function(){
    return this.props.trafficconfig
  },

  propTypes: {
    trafficconfig: React.PropTypes.object.isRequired,
  },
  
  handleDelete: function(e) {
    e.preventDefault();
    if (confirm("Are you sure you want to delete this trafficconfig?")) {
      ReactBootstrap.Dispatcher.emit("Trafficconfigform.delete", Object.assign({},this.state));
    };
  },
  
  handleChange: function (key) {
    return function (e) {
      var state = {};
      state[key] = e.target.value;
      this.setState(state);
    }.bind(this);
  },

  highlightGroup: function() {
    ReactBootstrap.Dispatcher.emit('Trafficconfigform.show',this.state.nodeList, this.state.type, this.state.groupid);
  },

  componentWillReceiveProps: function(nextProps){
    this.setState(nextProps.trafficconfig);

  },

  componentDidMount: function(){
   /* if (this.state.nodeList.length > 0) {
      this.highlightGroup();
    }; 

    var that = this;
    ReactBootstrap.Dispatcher.on("Groupmap.addNode", function(nodeid) {
      var newState = React.addons.update(that.state, {nodeList: { $push : [nodeid] }});
      that.setState(newState);
    });
    ReactBootstrap.Dispatcher.on("Groupmap.removeNode", function(nodeid) {
      var newNodeList = that.state.nodeList;
      newNodeList = newNodeList.filter(function(e){return e!==nodeid});
      var newState = React.addons.update(that.state, {nodeList: { $set : newNodeList }});
      that.setState(newState);
    }); */

    var that = this;
    ReactBootstrap.Dispatcher.on('Trafficconfigmap.countupdate',function(currentcount,idx){
      if((that.state.idx === idx) && (that.state.count != currentcount)){
        that.setState({count:currentcount});
      }
      
    });


  },

  componentDidUpdate: function(){ 
      /*if (this.highlight) {
        this.highlightGroup();
      } */
  },

  render: function() {
    var trafficconfig = this.state;
    console.log(trafficconfig);
    var hstyle = {overflowY:"auto",overflowX:"hidden",marginBottom:"16px",
                  marginLeft:"10px",
                  maxHeight:helpers.calcHeight(100, -230)+"px !important"};
    
    return (
      <div>
        <h2 style={{position:"relative",top:"-18px",left:"9px",width:"90%"}}>{this.state.roi.name}</h2>
        <form role="form" className="form-horizontal" data-trafficconfigid={trafficconfig.trafficconfigid} >
          <div style={hstyle}>
            <div className="form-group">
              <label htmlFor="type" className="control-label col-sm-3">Event Config Type:</label>
              <div className="col-sm-8">
                <input type="text" className="form-control" id="type" ref="type" disabled="disabled" value={this.state.type}/>
              </div>
            </div>  
            <div className="form-group">
              <label htmlFor="roiid" className="control-label col-sm-3">ROIID:</label>
              <div className="col-sm-8">
                <input type="text" className="form-control" id="roiid" ref="roiid" disabled="disabled" value={this.state.roi.roiid}/>
              </div>
            </div> 
            <div className="form-group">
              <label htmlFor="user" className="control-label col-sm-3">User:</label>
              <div className="col-sm-8">
                <input type="text" className="form-control" id="user" ref="user" disabled="disabled" value={this.state.user}/>
              </div>
            </div> 
            <div className="form-group">
              <label htmlFor="tag" className="control-label col-sm-3">Tag:</label>
              <div className="col-sm-8">
                <input type="text" className="form-control" id="tag" ref="tag" disabled="disabled" value={this.state.tag}/>
              </div>
            </div> 
            <div className="form-group">
              <label htmlFor="name" className="control-label col-sm-3">Event Name:</label>
              <div className="col-sm-8">
                <input type="text" className="form-control" id="name" ref="name" disabled="disabled" value={this.state.name}/>
              </div>
            </div> 
            <div className="form-group">
              <label htmlFor="eventid" className="control-label col-sm-3">Event ID:</label>
              <div className="col-sm-8">
                <input type="text" className="form-control" id="eventid" ref="eventid" disabled="disabled" value={this.state.eventid}/>
              </div>
            </div>  
            <div className="form-group">
              <label htmlFor="description" className="control-label col-sm-3">Event Description:</label>
              <div className="col-sm-8">
                <input type="text" className="form-control" id="description" ref="description" disabled="disabled" value={this.state.description}/>
              </div>
            </div>    
            <div className="form-group">
              <label htmlFor="configured_date" className="control-label col-sm-3">Configured Date:</label>
              <div className="col-sm-8">
                <input type="text" className="form-control" id="configured_date" ref="configured_date" disabled="disabled" value={this.state.configured_date}/>
              </div>
            </div>      
            <div className="form-group">
              <label htmlFor="channel" className="control-label col-sm-3">Channel:</label>
              <div className="col-sm-8">
                <input type="text" className="form-control" id="channel" ref="channel" disabled="disabled" value={this.state.channel}/>
              </div>
            </div>   
            <div className="form-group">
              <label htmlFor="count" className="control-label col-sm-3">Count:</label>
              <div className="col-sm-8">
                <input type="text" className="form-control" id="count" ref="count" disabled="disabled" value={this.state.count}/>
              </div>
            </div>   
            <div className="form-group">
              <label htmlFor="active" className="control-label col-sm-3">Active:</label>
              <div className="col-sm-8">
                <input type="text" className="form-control" id="active" ref="active" disabled="disabled" value={this.state.active}/>
              </div>
            </div>            
          </div>
        </form>
      </div>
      );

  }
});

  module.exports = Trafficconfigform;
