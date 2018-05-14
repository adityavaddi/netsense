import classNames from 'classnames';
import auth from 'global/utils/auth';
import helpers from 'global/utils/helpers';
import TimeField from 'components/timefield';
import { State, Navigation } from 'react-router';
import Daylighttriggerlist from 'components/daylight/daylightTriggerList';

var Daylightform = React.createClass({

  getInitialState: function(){
    return this.props.daylight
  },

  propTypes: {
    daylight: React.PropTypes.object.isRequired,
    groups: React.PropTypes.array.isRequired,
    errors: React.PropTypes.object
  },

  getDefaultProps: function() {
    return {
      errors: {}
    };
  },

  handleChange: function (key) {
    return function (e) {
      var state = {};
      if (key == 'groups') {
        state.groups = $.makeArray($(e.target).val());
      } else {
        state[key] = e.target.value;
      }
      this.setState(state);
    }.bind(this);
  },

  handleRadio: function (key) {
    return function (e) {
      var state = {};
      switch (e.target.value) {
        case "unassigned":
          state.sites = [];
          state.assign = "unassigned";
        break;
        case "sitewide":
          state.sites = [{
            siteid: NSN.siteID,
            name: NSN.siteName,
          }];
          state.assign="sitewide";
        break;
        case "groups":
          state.sites = [];
          state.assign = "groups";
        break;
      };
      this.setState(state);
    }.bind(this);
  },

   isValid: function() {
    this.props.errors = {};

    var rules = {
      name:{
        required:true
      },
      highLux: {
        required: true,
        greaterThan: "lowLux",
        type: "integer",
        min: 33,
        max: 64000
      },
      lowLux: {
        required: true,
        greaterThan: "minLux",
        type: "integer",
        min: 1,
        max: 64000
      },
      minLux: {
        type: "integer",
        min: 0,
        max: 64000
      },
      lowDriver: {
        required: true,
        greaterThan: "highDriver",
        type: "integer",
        min: 0,
        max: 100
      },
      minDriver: {
        required: true,
        greaterThan: "lowDriver",
        type: "integer",
        min: 0,
        max: 100
      },
      highDriver: {
        type: "integer",
        min: 0,
        max: 100
      },
      
      fastPoll: {
        type: "integer",
        min: 30,
        max: 120
      },
      slowPoll: {
        type: "integer",
        min: 60,
        max: 3600
      },

    };
    this.props.errors = helpers.checkValidity(this.state, rules);
    return (Object.keys(this.props.errors).length == 0);
  },

  handleSubmit: function(e) {
    e.stopPropagation();
    e.preventDefault();

    var daylight = Object.assign({}, this.state);
    daylight.highLux = parseInt(daylight.highLux);
    daylight.lowLux = parseInt(daylight.lowLux);
    daylight.lowDriver = parseInt(daylight.lowDriver);
    daylight.minDriver = parseInt(daylight.minDriver);    
    daylight.minLux = parseInt(daylight.minLux);
    daylight.highDriver = parseInt(daylight.highDriver);
    daylight.slowPoll = parseInt(daylight.slowPoll);
    daylight.fastPoll = parseInt(daylight.fastPoll);

    console.log(daylight);
    if (this.isValid()) {
      
      this.props.errors={};
      ReactBootstrap.Dispatcher.emit("Daylightform.save", daylight);
      
    } else {
      alert("Some fields are invalid");
      this.forceUpdate();
    }
    return false;
  },

  handleReset: function(e) {
    e.stopPropagation();
    e.preventDefault();
    ReactBootstrap.Dispatcher.emit("Daylightform.reset", Object.assign({},this.state));
  },

  handleDelete: function(e) {
    e.stopPropagation();
    e.preventDefault();
    if (confirm("Are you sure you want to Delete this Daylight Harvesting profile?")) {
      ReactBootstrap.Dispatcher.emit("Daylightform.delete", Object.assign({},this.state));
    };
  },

  /*handleCalibrate: function() {
    $.ajax({
      url:  NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID 
        + '/daylightharvesting/' + this.props.daylight.etdhprofileid + '/calibrate' ,
      data : '',
      method : 'GET',
      "xhrFields": {
         withCredentials: true
      },
      dataType : "json",
      contentType: "application/json",
      processData: false,
      timeout: 5000,

      success : function(data){
        console.log("ajax success: " + JSON.stringify(data));
        noty({type:"success", text:'Profile calibration successful.'});        
      },
      error : function(jqXHR, status, error){
        if (jqXHR.status == 200) {
          noty({type:"success", text:'Profile calibration successful'});        
       } else {
          noty({type:"error", text:'Calibration failed. (Status: ' + status + ')'}); 
        }
      }
    });
  }, */

  highlightNodes: function() {
    ReactBootstrap.Dispatcher.emit('Daylightform.showNodes', this.state.etdhprofileid);
  },

  componentWillReceiveProps: function(nextProps){
      this.setState(nextProps.daylight);
  },

  componentDidUpdate: function(){
      //this.highlightNodes();
  },

  componentDidMount: function(){

    var that = this;
    /* Updates the Group Detail as user selects(checks) nodes on node list */
    ReactBootstrap.Dispatcher.on("DaylightNodelist.update", function (selected_nodes) {
     var selectedTriggerNodes = [];
      for(var i=0;i<selected_nodes.length;i++){
        selectedTriggerNodes.push({"nodeid":selected_nodes[i]});
      }

      that.setState({
        triggers : selectedTriggerNodes
      }) 

    });

   /* if (this.state.nodes.length > 0) {
      this.highlightNodes();
    }; 
    var that = this;
    ReactBootstrap.Dispatcher.on("Daylightmap.addNode", function(nodeid) {
      var newState = React.addons.update(that.state, {triggers: { $push : [nodeid] }});
      that.setState(newState);
    });
    ReactBootstrap.Dispatcher.on("Daylightmap.removeNode", function(nodeid) {
      var newNodeList = $.extend(true, [], that.state.nodeList);
      newNodeList = newNodeList.filter(function(e){return e!==nodeid});
      var newState = React.addons.update(that.state, {triggers: { $set : newNodeList }});
      that.setState(newState);
    }); */

  },

  handleAddTimeWindow: function(){
    var newState = React.addons.update(this.state, { scheduled: { $push: [{"beginTime":"","endTime":""}] } });
    this.setState(newState);
  },

  handleDeleteTimeWindow: function(e){
    var idx = e.target.getAttribute("data-idx");
    var newState = React.addons.update(this.state, { scheduled: { $splice: [[idx, 1]] } });
    this.setState(newState);
  },

  handleBeginTime: function(fieldid){
    var idx = fieldid.substr(10);
    var value = document.getElementById(fieldid).value;
    var newState = React.addons.update(this.state, { scheduled: { [idx]: {beginTime: { $set:value } }} });
    this.setState(newState);
  },

  handleEndTime: function(fieldid){
    var idx = fieldid.substr(8);
    var value = document.getElementById(fieldid).value;
    var newState = React.addons.update(this.state, { scheduled: { [idx]: {endTime: { $set:value } }} });
    this.setState(newState);
  },

  handleTriggerNodes: function () {
    ReactBootstrap.Dispatcher.emit("DaylightNodelist.preselect",this.state.nodes,this.state.triggers);
  },


  render: function() {
    var hstyle = {overflowY:"auto",overflowX:"hidden",marginBottom:"16px",
                  maxHeight:helpers.calcHeight(100, -216)+"px !important"};
    var daylight = this.state;
    console.log("Daylight Form", JSON.stringify(daylight));
    var daylightfields_visibility = (daylight.etdhprofileid==0)?{display:"none"}:{};
    var triggerNodesGridVisibility = (daylight.etdhprofileid == 0)?{display:"none"}:{}
    var heading = (daylight.name=="")?"Add Profile":(<span><Icon glyph="icon-fontello-right-dir"/> {daylight.name}</span>);
    console.log(JSON.stringify(daylight.groups));

    var daylightgroups = daylight.groups;
    var daylighttriggers = daylight.triggers;
    var daylightnodes = daylight.nodes;
    var daylightid = daylight.etdhprofileid;
    console.log("daylightgroupsnodestriggers",daylightgroups,daylighttriggers,daylightnodes);
    /*var calibrateButtonVisibility = (typeof daylightgroups !== 'undefined' && daylightgroups.length > 0 && (daylight.etdhprofileid!=0))

    var calibrate = (calibrateButtonVisibility)?(
              <div style={{position:"absolute",top:"16px",right:"21px"}}>
               <button type="button" className="btn btn-success" onClick={this.handleCalibrate}>
                <Icon glyph="icon-fontello-target" /> Calibrate</button>
              </div>
                ):""; */

    var deleteBtnStyle = (daylight.etdhprofileid=="")?{display:"none"}:{};
    var that = this;

    var time = this.state.scheduled.map(function (scheduled, idx) {
      console.log(idx+": "+scheduled.beginTime+"-"+scheduled.endTime);

      var addIcon = ((that.state.scheduled.length < 6) && (idx == 0))? (
        <div className="col-sm-1">
          <Icon glyph="icon-fontello-plus-circled" onClick={that.handleAddTimeWindow} />
        </div>
      ):(
        <div className="col-sm-1" style={{display:"none"}}></div>
        );       

      var deleteIcon = (idx>0)?(
        <div className="col-sm-1">
          <Icon glyph="icon-fontello-trash" data-idx={idx} onClick={that.handleDeleteTimeWindow} />
        </div>
        ):(
        <div className="col-sm-1" style={{display:"none"}}></div>
        );  

       var labelSpace = (idx>0)?(
        <div className="col-sm-3">
        </div>
        ):(
        <div className="col-sm-3" style={{display:"none"}}></div>
        );  

      return (
        <div key={idx}>
          <div data-idx={idx}>
            <div className="col-sm-5" style={{padding:"0px"}}>
              <TimeField fieldid={"beginTime-"+idx} info="" value={scheduled.beginTime} handler={that.handleBeginTime} />                 
            </div>
            <div className="col-sm-1" style={{padding:"0px"}}>
              to
            </div>
            <div className="col-sm-5" style={{padding:"0px"}}>
              <TimeField fieldid={"endTime-"+idx} info="" value={scheduled.endTime} handler={that.handleEndTime} />                  
            </div>
            <div className="col-sm-1" style={{padding:"0px"}}>
              {addIcon}
              {deleteIcon} 
            </div>
          </div>
          <div style={{clear:"both",padding:"2px"}}></div>            
        </div>
      )

    });

    return (
      <div>
        <div className="netsense__form__header">
          <h3> {heading}</h3>
        </div>
        <div className="netsense__form__body">

          <form role="form" className="form-horizontal" data-daylightid={daylight.etdhprofileid} >
          <div style={hstyle}>
            <div className="form-group">
              <label htmlFor="name" className="control-label col-sm-3">Name:</label>
              <div className="col-sm-8">
                <input type="text" className={(this.props.errors.name)? "form-control orange":"form-control"} id="name" ref="name" value={this.state.name} onChange={this.handleChange('name')} />
                <div className="form-error">
                  {this.props.errors.name || ""}
                </div>
              </div>
            </div>
            <div className="form-group">
              <label htmlFor="highLux" className="control-label col-sm-3">High lux:</label>
              <div className="col-sm-8">
                <input type="text" className={(this.props.errors.highLux)? "form-control orange":"form-control"} id="highLux" ref="highLux" value={this.state.highLux} onChange={this.handleChange('highLux')} />
                <div className="form-error">
                  {this.props.errors.highLux || ""}
                </div>
              </div>
            </div>
            <div className="form-group">
              <label htmlFor="lowLux" className="control-label col-sm-3">Low lux:</label>
              <div className="col-sm-8">
                <input type="text" className={(this.props.errors.lowLux)? "form-control orange":"form-control"} id="lowLux" ref="lowLux" value={this.state.lowLux} onChange={this.handleChange('lowLux')} />
                <div className="form-error">
                  {this.props.errors.lowLux || ""}
                </div>
              </div>
            </div>
            <div className="form-group">
              <label htmlFor="lowDriver" className="control-label col-sm-3">Low driver:</label>
              <div className="col-sm-8">
                <input type="text" className={(this.props.errors.lowDriver)? "form-control orange":"form-control"} id="lowDriver" ref="lowDriver" value={this.state.lowDriver} onChange={this.handleChange('lowDriver')} />
                <div className="form-error">
                  {this.props.errors.lowDriver || ""}
                </div>
              </div>
            </div>
            <div className="form-group">
              <label htmlFor="minDriver" className="control-label col-sm-3">Min driver:</label>
              <div className="col-sm-8">
                <input type="text" className={(this.props.errors.minDriver)? "form-control orange":"form-control"} id="minDriver" ref="minDriver" value={this.state.minDriver} onChange={this.handleChange('minDriver')} />
                <div className="form-error">
                  {this.props.errors.minDriver || ""}
                </div>
              </div>
            </div>
            <div className="accordionWrapper">
              <div className="panel-group" id="accordion" >
                <div className="panel panel-default">
                  <div className="panel-heading">
                    <h4 className="panel-title">
                      <a data-toggle="collapse" id="basicIdentifiers" data-parent="#accordion" href="#collapseOne">
                        Advanced Settings
                      </a>
                    </h4>
                  </div>
                  <div id="collapseOne" className="panel-collapse collapse">
                    <div className="panel-body">
                      <div className="form-group">
                        <label htmlFor="highDriver" className="control-label col-sm-3">High driver:</label>
                        <div className="col-sm-8">
                          <input type="text" className={(this.props.errors.highDriver)? "form-control orange":"form-control"} id="highDriver" ref="highDriver" value={this.state.highDriver} onChange={this.handleChange('highDriver')} />
                          <div className="form-error">
                            {this.props.errors.highDriver || ""}
                          </div>
                        </div>
                      </div>
                      <div className="form-group">
                        <label htmlFor="minLux" className="control-label col-sm-3">Min lux:</label>
                        <div className="col-sm-8">
                          <input type="text" className={(this.props.errors.minLux)? "form-control orange":"form-control"} id="minLux" ref="minLux" value={this.state.minLux} onChange={this.handleChange('minLux')} />
                          <div className="form-error">
                            {this.props.errors.minLux || ""}
                          </div>
                        </div>
                      </div>
                      <div className="form-group">
                        <label htmlFor="fastPoll" className="control-label col-sm-3">Fast poll:</label>
                        <div className="col-sm-8">
                          <input type="text" className={(this.props.errors.fastPoll)? "form-control orange":"form-control"} id="fastPoll" ref="fastPoll" value={this.state.fastPoll} onChange={this.handleChange('fastPoll')} />
                          <div className="form-error">
                            {this.props.errors.fastPoll || ""}
                          </div>
                        </div>
                      </div>
                      <div className="form-group">
                        <label htmlFor="slowPoll" className="control-label col-sm-3">Slow poll:</label>
                        <div className="col-sm-8">
                          <input type="text" className={(this.props.errors.slowPoll)? "form-control orange":"form-control"} id="slowPoll" ref="slowPoll" value={this.state.slowPoll} onChange={this.handleChange('slowPoll')} />
                          <div className="form-error">
                            {this.props.errors.slowPoll || ""}
                          </div>
                        </div>
                      </div>
                      <div className="form-group">
                        <label htmlFor="scheduled" className="control-label col-sm-12">Scheduled active periods:</label>
                        {time}
                      </div>
                    </div>
                  </div>
                </div>
              </div>
            </div>
             <div className="accordionWrapper" style={triggerNodesGridVisibility}>
              <div className="panel-group" id="accordion" >
                <div className="panel panel-default">
                  <div className="panel-heading" onClick={this.handleTriggerNodes}>
                    <h4 className="panel-title">
                      <a data-toggle="collapse" id="triggersList" data-parent="#accordion" href="#collapseTwo">
                        Trigger Nodes
                      </a>
                    </h4>
                  </div>
                  <div id="collapseTwo" className="panel-collapse collapse">
                    <div className="panel-body">
                      <Daylighttriggerlist etdhprofileid={daylightid} nodes={daylightnodes} triggers={daylighttriggers} groups={daylightgroups} />                 
                    </div>
                  </div>
                </div>
              </div>
            </div>
            </div>
            <div style={{margin:"20px 0px"}}>
              <div className="col-sm-3">
                <button type="button" className="ns-delete-btn" style={deleteBtnStyle} onClick={this.handleDelete}>
                  <b>Delete</b></button>
              </div>
              <div className="col-sm-8 text-right">
                <button type="button" className="ns-reset-btn" onClick={this.handleReset}>
                  <b>Reset</b></button>
              &nbsp; &nbsp;
                <button type="button" className="ns-save-btn" id="saveDhProfile" onClick={this.handleSubmit}>
                  <b>Save</b></button>
              </div>
            </div>
          </form>
        </div>
      </div>
      );
 
  }
});

module.exports = Daylightform;
