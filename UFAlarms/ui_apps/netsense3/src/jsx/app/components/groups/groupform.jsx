import classNames from 'classnames';
import { State, Navigation } from 'react-router';
import LightControl from 'components/lightcontrol';
import auth from 'global/utils/auth';
import helpers from 'global/utils/helpers';
import ScheduleView from 'components/scheduleview';
import Groupnodelist from './groupnodelist';
import { Modal } from 'react-bootstrap';

var Groupform = React.createClass({

  getInitialState: function(){
    return this.props.group
  },

  propTypes: {
    group: React.PropTypes.object.isRequired,
    schedules: React.PropTypes.array.isRequired,
    etdhprofiles: React.PropTypes.array.isRequired,
    pdprofiles: React.PropTypes.array.isRequired,
    errors: React.PropTypes.object
  },

    getDefaultProps: function() {
    return {
      errors: {}
    };
  },

  highlight: false,

  handleChange: function (key) {
    return function (e) {
      var state = {};
      switch (e.target.id) {
        case "xschedule":
          state.schedules = e.target.value=="0"
            ?[]
            :[
            {scheduleid: e.target.value,
                   name: e.target.options[e.target.selectedIndex].text
              }];
          break;
        case "etdhprofile":
          state.etdhprofiles = e.target.value=="0"
            ?[]
            :[
            {etdhprofileid: e.target.value,
                   name: e.target.options[e.target.selectedIndex].text
              }];
          break;
        case "pdprofile":
          state.pdprofiles = e.target.value=="0"
            ?[]
            :[
            {pdprofileid: e.target.value,
                   name: e.target.options[e.target.selectedIndex].text
              }];
          break;
        default:
           state[key] = e.target.value;
      }

      this.setState(state);
    }.bind(this);
  },

  handleSubmit: function(e) {
    e.stopPropagation();
    e.preventDefault();
/*
    if (this.state.type=="lighting" || this.state.type=="site-lighting") {
      if (!window.confirm("This Lighting Group has " + this.state.nodeList.length + " nodes. "
        + " Are you sure you want to Save these changes?")) {
        return;
      };
    };
*/
      if(this.handlePDProfile()){
      if(this.state.type=="site-lighting" && this.state.schedules.length==0 ){
      noty({type:"error",text:"The Site Default Lighting Group must have a Schedule."});
    }else {
      if(this.isValid()){
        this.props.errors={};
        ReactBootstrap.Dispatcher.emit("Groupform.save", Object.assign({},this.state));
      }else {
        this.forceUpdate();
      }
    };
    }
    else{
      noty({type:"error",text:"Proximity Dimming Profile cannot be assigned to a Lighting group with an existing SCH node"});
    }
  },

  handlePDProfile: function () {
    if (this.state.type == "lighting") {
      for (var key in this.state.nodeList) {

        for (var i in this.props.nodes) {

          if (this.state.nodeList[key] == this.props.nodes[i].nodeid && this.props.nodes[i].model == "Smart City Hub") {
            if (this.state.pdprofiles.length > 0) {
              return false;
            }
          }
        }
      }
    }
    return true;
  },

  isValid: function() {
    this.props.errors = {};
    var rules = {
        name: {
            required: true
        }
    };
    this.props.errors = helpers.checkValidity(this.state, rules);
    return (Object.keys(this.props.errors).length == 0);
  },

  handleDelete: function(e) {
    e.preventDefault();
    if (confirm("Are you sure you want to delete this group?")) {
      ReactBootstrap.Dispatcher.emit("Groupform.delete", Object.assign({},this.state));
    };
  },

  handleSchedule: function(e) {
    e.stopPropagation();
    e.preventDefault();
    ReactBootstrap.Dispatcher.emit("Groupform.schedule", "open");
  },

  handlePushSchedule: function(e) {
    e.stopPropagation();
    e.preventDefault();
    ReactBootstrap.Dispatcher.emit("Groupform.pushSchedule", this.state.groupid);
  },

  applySchedule: function(e) {
    e.stopPropagation();
    e.preventDefault();
    ReactBootstrap.Dispatcher.emit("Groupform.applySchedule", this.state.groupid, this.state.scheduleid);
  },

  highlightGroup: function() {
    ReactBootstrap.Dispatcher.emit('Groupform.showGroup',this.state.nodeList, this.state.type, this.state.groupid);
    /*
    $(".groupmapCanvas img[src*='blue']").attr('src','/imgs/markers/clear-dot.png');
    var nodelist = this.props.group.nodeList;
    if (nodelist.length>0) {
      nodelist = nodelist.split(",");
      for (var i=0; i<nodelist.length; i++) {
        $(".groupmapCanvas img[title='" + nodelist[i] + "']").attr('src','/imgs/markers/blue-dot.png');
      }
    }
    */
  },

  componentWillReceiveProps: function(nextProps){ 
    if (this.props.group.groupid != nextProps.group.groupid){
      this.highlight = true;
      this.setState(nextProps.group);
    } else {
      this.highlight = false;
    }
    nextProps.errors = {};
  },

  componentDidMount: function(){
    if (this.state.nodeList.length > 0) {
      this.highlightGroup();
    };
      this.props.errors = {};
//    $("#save-group-button").easyconfirm({locale: {title:"Save Group", text:"Are you sure you want to save this Group?", button: ["Cancel","Save"]}});

    var that = this;
    /* Updates the Group Detail as user selects(checks) nodes on node list */
    ReactBootstrap.Dispatcher.on("GroupNodeList.update", function (selected_nodes) {
        that.setState({
          nodeList : selected_nodes
        })
    });
    ReactBootstrap.Dispatcher.on("Groupmap.addNode", function(nodeid) {
      var newState = React.addons.update(that.state, {nodeList: { $push : [nodeid] }});
      that.setState(newState);
    });
    ReactBootstrap.Dispatcher.on("Groupmap.removeNode", function(nodeid) {
      var newNodeList = that.state.nodeList;
      newNodeList = newNodeList.filter(function(e){return e!==nodeid});
      var newState = React.addons.update(that.state, {nodeList: { $set : newNodeList }});
      that.setState(newState);
    });

  },

  componentDidUpdate: function(){
    this.highlightGroup();
  },

  handleNodesGrid: function () {
    ReactBootstrap.Dispatcher.emit("GroupNodelist.preselect",this.state,this.props.nodes);
    this.setState({ showGroupNodeList: true });
    $('#group-list-panel').css({ position: 'initial' });
    $('#add-group').css({ display: 'none' });
    $('.groupNodeList').css({ display: 'block' });
  },

  hideNodeList() {
    this.setState({
      showGroupNodeList: false
    });
    $('#group-list-panel').css({ position: 'absolute' });
    $('#add-group').css({ display: 'inline' });
    $('.groupNodeList').css({ display: 'none' });
  },

  render: function() {
    var group = this.state;
    var isDisabled = !auth.allowed('CAN_UPDATE', 'GroupModel');
    var visibility = auth.allowed('CAN_UPDATE', 'GroupModel')?{}:{display:"none"};
    var deleteBtnStyle = (group.groupid=="" || !auth.allowed('CAN_DELETE', 'GroupModel') || group.type == "site-lighting")?{display:"none"}:{};
    var newgroup = (group.groupid=="");
    var heading = newgroup?"Add Group":(<span><Icon glyph="icon-fontello-right-dir"/> {group.name}</span>);
    var hstyle = {overflowY:"auto",overflowX:"hidden",marginBottom:"16px",
                  maxHeight:helpers.calcHeight(100, -115)+"px !important"};
    var nodelist = group.nodeList;
    var nodecount = 0;
    var NodesGridBtn = <span onClick={()=>this.handleNodesGrid()} style={{ fontSize: "21px", color: "#3c3", cursor: "pointer", zIndex: "99999", position: "absolute", right: "24px", top: "0px" }}
                         className="pulse">
                        <img src="/imgs/new-icons/add-node.svg" style={{ height: "25", width: "30", position: "relative" }} />
                        </span>
    if (nodelist.length==0) {
      nodelist = (<div>No nodes in group</div>);
    } else {
      nodecount = nodelist.length;
      nodelist = nodelist.map(function(node, index) {
        return (<div key={index} style={{textAlign:"center"}}>{node}</div>);
      })
    };
    if (newgroup) {
      var typeField = (
          <div className="form-group" style={{fontSize:"18px",fontweight:"bold"}}>
            <label htmlFor="type" className="control-label col-sm-3">Type:</label>
            <div className="col-sm-8" style={{marginTop: "10px"}}>
              <div>
                <input disabled={isDisabled} type="radio" name="type" id="select-lighting" ref="type" checked={this.state.type == "lighting"} value="lighting" onChange={this.handleChange('type')} /> 
                <span id="grp-type">Lighting</span>
                <p style={{fontSize:"14px"}}>Lighting Groups can be controlled by Schedules and adjusted through 
                  {helpers.isInternalUser() &&
                    <span> Daylight Harvesting and </span>
                  } Proximity Dimming profiles.
                A node can only belong to a single Lighting Group.</p>
              </div>
              <div>
                <input disabled={isDisabled} type="radio" name="type" id="select-organizational" ref="type" checked={this.state.type == "organizational"} value="organizational" onChange={this.handleChange('type')} /> 
                <span id="grp-type">Organizational</span>
                <p style={{fontSize:"14px"}}>Organizational Groups contain collections of related nodes defined as needed (e.g. by location or model).
                Nodes may belong to more than one Organizational Group.</p>
                <p style={{fontSize:"16px",fontStyle:"italic"}}>The type of a group cannot be changed after creation.</p>
              </div>
            </div>
          </div>
          );
    } else {
      typeField = (
          <div className="form-group" style={{fontSize:"18px",fontweight:"bold"}}>
            <label htmlFor="type" className="control-label col-sm-3">Type:</label>
            <div className="col-sm-8">
              {this.state.type=="lighting"
                ?"Lighting"
                :(this.state.type=="site-lighting"
                  ?"Site Default Lighting"
                  :"Organizational")
              }
            </div>
          </div>
          );
    }
    if (group.type == "lighting" || group.type == "site-lighting") {
      if (!newgroup && auth.allowed("CAN_UPDATE", "ScheduleModel")) {
        var lightcontrol = (
            <div style={{border:"2px solid",borderRadius:"8px",padding:"0px 0px 10px",marginRight:"10px",marginTop:"56px"}}>
              <div style={{textAlign:"center"}}>
                <h3>Group Light Control</h3>
                <button className="ns-form-btn" id="view-group-schedule" style={{width:"130px"}} onClick={this.handleSchedule}><b>View Schedule</b></button>
                <hr style={{width:"50%",margin:"16px auto",borderTop:"1px solid #666"}} />
                <h4>Manual Override</h4>
                <LightControl event="Groupform.lightLevel" isDisabled={isDisabled} />
              </div>
            </div>
            );
      } else {
        lightcontrol = "";
      };

      var dOptions = this.props.etdhprofiles.length == 0
                      ?<option>No DH Profiles defined</option>
                      :[<option value="0">-none-</option>].concat(this.props.etdhprofiles.map(function(dhprofile, index) {
                        return <option key={index} value={dhprofile.etdhprofileid}>{dhprofile.name}</option>;
                      }));
      var sOptions = this.props.schedules.length == 0
                      ?<option>No Schedules defined</option>
                      :[<option value="0">-none-</option>].concat(this.props.schedules.map(function(schedule, index) {
                        return <option key={index} value={schedule.scheduleid}>{schedule.name}</option>;
                      }));
      var pOptions = this.props.pdprofiles.length == 0
                      ?<option disabled="disabled">No PD Profiles defined</option>
                      :[<option value="0">-none-</option>].concat(this.props.pdprofiles.map(function(pdprofile, index) {
                        return <option key={index} value={pdprofile.pdprofileid}>{pdprofile.name}</option>;
                      }));
    } else {
      lightcontrol = "";
    };
    return (
      <div>
        {this.state.showGroupNodeList &&
          <Groupnodelist group={this.props.group} 
                        show={this.state.showGroupNodeList} 
                        nodes={this.props.nodes} 
                        grouptype={this.state.type} 
                        hide={this.hideNodeList}
                        pdProfile={this.state.pdprofiles}/>
        }
        <h2 style={{ position: "relative", top: "-7px", left: "10px", fontSize: "24px", width:"235px"}}>{heading}</h2>
        <form role="form" className="form-horizontal" data-groupid={group.groupid} style={{paddingLeft:"14px"}}>
        <div style={hstyle}>
          <div className="form-group">
            <label htmlFor="name" className="control-label col-sm-3">Name:</label>
            <div className="col-sm-8">
              <input disabled={isDisabled} type="text" className={(this.props.errors.name)? "form-control orange":"form-control"} id="name" ref="name" value={this.state.name} onChange={this.handleChange('name')} />
                <div className="form-error">
                      {this.props.errors.name || ""}
                </div>
            </div>
          </div>
          <div className="form-group">
            <label htmlFor="description" className="control-label col-sm-3">Description:</label>
            <div className="col-sm-8">
              <input disabled={isDisabled} type="text" className="form-control" id="description" ref="description" value={this.state.description} onChange={this.handleChange('description')} />
            </div>
          </div>
          {typeField}
          <div className="static-header" style={{position:"relative"}}>Nodes ({nodecount}):
            {NodesGridBtn}
          </div>
          <div className="form-group">
            <label htmlFor="nodelist" className="control-label col-sm-3"></label>
            <div className="col-sm-6">
              <div id="nodeList" style={{ height: "160px", width: "403px", right: "100px", top: "13px", position: "relative", padding: "2px 12px", overflow: "auto", border: "1px solid #CCC", marginBottom: "13px" }}>
                {nodelist}
              </div>
            </div>
          </div>
          <div style={this.state.type=="organizational"?{display:"none"}:{display:"block"}}>
            <div className="static-header">Schedule:</div>
            <br/>
            <div className="form-group">
              <label htmlFor="xschedule" className="control-label col-sm-3" style={{lineHeight:"18px",paddingTop:"4px",paddingLeft:"10px"}}>Apply:</label>
              <div className="col-sm-8">
                <select className="form-control" style={{width:"100%",fontSize:"18px"}} name="xschedule" id="xschedule"
                     value={this.state.schedules.length==0?0:this.state.schedules[0].scheduleid}  onChange={this.handleChange('xschedule')} >
                  {sOptions}
                </select>
              </div>
            </div>
            {
              this.state.type=="lighting" ?
              <div>
                {helpers.isInternalUser() &&
                  <div>
                    <div className="static-header">Daylight Harvesting Profile:</div>
                    <br/>
                    <div className="form-group">
                      <label htmlFor="dhprofile" className="control-label col-sm-3" style={{lineHeight:"18px",paddingTop:"4px",paddingLeft:"10px"}}>Apply:</label>
                      <div className="col-sm-8">
                        <select className="form-control" style={{width:"100%",fontSize:"18px"}} name="etdhprofile" id="etdhprofile"
                                value={this.state.etdhprofiles.length==0?0:this.state.etdhprofiles[0].etdhprofileid}  onChange={this.handleChange('etdhprofile')} >
                          {dOptions}
                        </select>
                      </div>
                    </div>
                    </div>
                }

                <div className="static-header">Proximity Dimming Profile:</div>
                <br/>
                <div className="form-group">
                  <label htmlFor="pdprofile" className="control-label col-sm-3" style={{lineHeight:"18px",paddingTop:"4px",paddingLeft:"10px"}}>Apply:</label>
                  <div className="col-sm-8">
                    <select className="form-control" style={{width:"100%",fontSize:"18px"}} name="pdprofile" id="pdprofile"
                        value={this.state.pdprofiles.length==0?0:this.state.pdprofiles[0].pdprofileid}  onChange={this.handleChange('pdprofile')} >
                      {pOptions}
                    </select>
                  </div>
                </div>
              </div>:<div/>
            }
            <div className="form-group">
              <div className="col-sm-1 text-right">
              </div>
              <div className="col-sm-10" style={{lineHeight:"18px"}}>
                * If none is selected, the schedule and profiles of the Site Default Group will apply (if defined).
              </div>
            </div>
          </div>
          <div className="form-group">
            <div style={visibility} className="col-sm-4">
              <button type="button" className="ns-delete-btn" id ="delete-group" style={deleteBtnStyle} onClick={this.handleDelete}>
                <b>Delete Group</b></button>
            </div>
	          {this.state.type == "organizational" ?
		          <div/>:
			          <div style={visibility} className="col-sm-5">
				          <button type="button" className="ns-form-btn" id="push-schedule-group" style={{width:"170px", position:"relative"}} onClick={this.handlePushSchedule}>
					          <b>Push schedule to group</b>
				          </button>
			          </div>
	          }
            <div style={visibility} className="col-sm-3 ">
              {/*<button type="button" className="ns-reset-btn" onClick={this.handleReset}>
                <b>Reset</b></button>*/}
              <button  style={{marginLeft:"0px"}} type="button" id="save-group-button" className="ns-save-btn" onClick={this.handleSubmit}>
                <b>Save</b></button>
            </div>
          </div>

          {lightcontrol}
        </div>
        </form>
        </div>
      );

  }
});

  module.exports = Groupform;