import classNames from 'classnames';
import auth from 'global/utils/auth';
import helpers from 'global/utils/helpers';
import scheduler from 'global/utils/scheduler';
import { State, Navigation } from 'react-router';

var Scheduleform = React.createClass({

  getInitialState: function(){
    return this.props.schedule
  },

  propTypes: {
    schedule: React.PropTypes.object.isRequired,
    groups: React.PropTypes.array.isRequired,
    allSchedules: React.PropTypes.array.isRequired,
    submitStatus: React.PropTypes.bool
  },

  handleChange: function (key) {
    return function (e) {
      var state = {};
      state[key] = e.target.value;
      this.setState(state);
    }.bind(this);
  },
 
  handleSubmit: function(e) {
    e.stopPropagation();
    e.preventDefault();
    var s = "";
/*    switch ($('#schedule input[name="sched-assign"]:checked').val()) {
      case "unassigned":
        s = "You are about to unassign this Schedule.\n\nAny nodes that were controlled by this Schedule ";
        s += "prior to this action will now be controlled by the default site Schedule."
        break;
      case "sitewide":
        s = "You are about to assign this Schedule to the Default Site Lighting Group.\n\nThis will apply this Schedule ";
        s += "all nodes not currently controlled by other Schedules.";
        break;
      case "groups":
        var grouplist = $('#schedule #sched-groups').val();
        var groupcount = grouplist.length, nodecount = 0;
        for (var i=0; i<grouplist.length; i++) {
          for (var j=0, found=false; !found && j<this.props.groups.length; j++) {
            found = grouplist[i] == this.props.groups[j].groupid;
          }
          if (found) {
            nodecount += this.props.groups[--j].nodeList.length;
          }
        }
        s = "Your are about to assign this Schedule to " + groupcount
            + " Lighting Group" + (groupcount>1?"s":"") + " containing " + nodecount
            + " node" +(nodecount>1?"s":"") + ".\n\nAny other Lighting Groups or nodes currently "
            + "using this Schedule will now be controlled by the default site Schedule. "
        break;
    };
    if (!window.confirm(s)) {
      return false;
    }
*/
    var errors = scheduler.validate($('#schedule'));
    if (errors.length > 0) {
      var s = "<b>Schedule not saved:</b><ul style='margin-top:16px'>" + errors.map(function(error, index){return "<li>"+error+"</li>";}).join("") + "</ul>";
      noty({type:'error', text:s});
    } else {
      var schedule = scheduler.encode($('#schedule'));
      ReactBootstrap.Dispatcher.emit("Scheduleform.save", schedule);
    };
    return false;
  },

  handleReset: function(e) {
    e.stopPropagation();
    e.preventDefault();
    ReactBootstrap.Dispatcher.emit("Scheduleform.reset", Object.assign({},this.state));
  },

  handleDelete: function(e) {
    e.stopPropagation();
    e.preventDefault();
    if (!window.confirm("Deleting this Schedule means that any Lighting Groups or nodes controlled by this Schedule will now use the default site Schedule.")) {
      return false;
    };
    ReactBootstrap.Dispatcher.emit("Scheduleform.delete", Object.assign({},this.state));
  },

  callPreviousItem:function(allItems, currentItem){ 

    $('.schedule-previous').keyup();
    $("#Schedule-grid").data("gridInstance");
    console.log($("#CSchedule-grid").data("gridInstance"));
    var currentRow = $("#Schedule-grid").data("gridInstance").getSelectedRows();
    var nextRow =  currentRow[0]-1;
    $("#Schedule-grid").data("gridInstance").setSelectedRows([nextRow])

  },

  callNextItem:function(allItems, currentItem){
 
    $('.schedule-next').keyup();
    $("#Schedule-grid").data("gridInstance");
    console.log($("#CSchedule-grid").data("gridInstance"));
    var currentRow = $("#Schedule-grid").data("gridInstance").getSelectedRows();
    var nextRow =  currentRow[0]+1
    $("#Schedule-grid").data("gridInstance").setSelectedRows([nextRow])

  },

  componentWillReceiveProps: function(nextProps){
    if (this.props.schedule.scheduleid != nextProps.schedule.scheduleid){
      this.setState(nextProps.schedule);
    };
  },

  componentDidMount: function(){
    // this is where we generate the schedule editor
    scheduler.init($('#schedule'), this.props.schedule, this.props.groups, false);
  },

  componentDidUpdate: function(){
    scheduler.destroy($('#schedule'));
    scheduler.init($('#schedule'), this.props.schedule, this.props.groups, false);   
  },

  componentWillUnmount: function() {
    scheduler.destroy($('#schedule'));
  },

  render: function() {
    $('#save-schedule').remove('disabled');      
      if(!this.props.submitStatus) {
        $('#save-schedule').attr('disabled', true);
    }
    // var allSchedulesLength = this.props.allSchedules.length;
    // var firstItem = 0;
    // var lastItem = allSchedulesLength - 1;
    // for( var a=0 ; a < allSchedulesLength; a++){
    //   if(this.props.allSchedules[a].scheduleid === this.props.schedule.scheduleid){
    //     var noNextItem = lastItem === a ? {display:"none"}:{};
    //     var noPreviousItem = firstItem === a ? {display:"none"}:{};
    //   }
    // }
    // var previousNextButtons = this.props.schedule.scheduleid === "" ? {display:"none"}:{};
	  var currentLength = $("#Schedule-grid").data("gridInstance").getData().getLength();
	  var allItemsLength = this.props.allSchedules.length;
	  var currentRow = $("#Schedule-grid").data("gridInstance").getSelectedRows();
	  var noNextItem = {}; var noPreviousItem = {};var displayArrows = {};
	  if(currentLength === allItemsLength ){
		  var firstItem = 0; var lastItem = allItemsLength - 1;
		  for (var a = 0; a < allItemsLength; a++) {
			  if (this.props.allSchedules[a]!=null && this.props.schedule!=null && this.props.allSchedules[a].scheduleid === this.props.schedule.scheduleid){
				  noNextItem = this.props.schedule.idx === lastItem ? {display:"none"}:{};
				  noPreviousItem = this.props.schedule.idx === firstItem ? {display:"none"}:{};
			  }
		  }
		  displayArrows = (this.props.schedule!=null && this.props.schedule.scheduleid === "" ) ? { display: "none" } : {};
	  }else{
		  if (this.props.schedule!=null && this.props.schedule.scheduleid != null){
			  var firstElement = 0;
			  var lastElement = currentLength-1;
			  if(currentRow[0] === firstElement){
				  //display only the next arrow and not the previous arrow
				  if(firstElement+1 === currentLength){ displayArrows={display:"none"};
				  }else{
					  noNextItem={};noPreviousItem={display:"none"}; displayArrows={};
				  }
			  }else if(currentRow[0] === lastElement){
				  //display only the previous arrow and not the next arrow
				  noNextItem={display:"none"}; noPreviousItem={}; displayArrows={};
			  }else{
				  //display both arrows
				  noNextItem={};noPreviousItem={}; displayArrows={};
			  }
		  }
	  }
	  if(this.props.schedule.scheduleid === "0"){
		  displayArrows={display:"none"};
	  }
    // var hstyle = {overflow:"auto",width:"100%",height:helpers.calcHeight(90, -216)+"px !important"};
    var hstyle = {overflow:"auto",width:"93%",margin:"0 auto",height:helpers.calcHeight(100, -280)+"px !important"};

    var schedule = this.props.schedule;
    var heading = (schedule.name=="")?"Add Schedule":(<span><Icon glyph="icon-fontello-right-dir"/> {schedule.name}</span>);

    return (
      <div>
        <div className="netsense__form__header">
          <h3 style={{margin:"0px 12px 20px"}}>{heading}</h3>
        </div>

          <div className="netsense__form__body">

            <span style={displayArrows}>
               <span style={noPreviousItem}  className="rubix-icon icon-simple-line-icons-arrow-left schedule-previous" onClick={ () => this.callPreviousItem(this.props.allSchedules, this.props.schedule)}></span>
            </span>
            <span style={displayArrows}>
               <span style={noNextItem} className="rubix-icon icon-simple-line-icons-arrow-right schedule-next" onClick={ () => this.callNextItem(this.props.allSchedules,this.props.schedule)}></span>
            </span>

            <div style={hstyle}>
              <div id="schedule"></div>
            </div>
            <div className="sched-addbar"><button onClick={scheduler.handleAddbar}><b> + add a timeline for other days or date + </b></button></div>
            <div>
              <div className="col-xs-3 col-sm-3">
                <button type="button" className="ns-delete-btn" onClick={this.handleDelete}>
                  <b> Delete</b></button>
              </div>
              <div className="col-xs-9 col-sm-9 text-right">
                <button type="submit" id="saveSchedule" className="ns-save-btn" onClick={this.handleSubmit}><b>Save</b></button>
              </div>
            </div>
          </div>
        </div>
      );
 
  }
});

  module.exports = Scheduleform;
