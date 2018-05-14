import classNames from 'classnames';
import auth from 'global/utils/auth';
import helpers from 'global/utils/helpers';
import { State, Navigation } from 'react-router';
import TimeField from 'components/timefield';

var Proximityform = React.createClass({

  getInitialState: function () {
    return this.props.proximity
  },

  propTypes: {
    proximity: React.PropTypes.object.isRequired,
    allProximities: React.PropTypes.array.isRequired,
    errors: React.PropTypes.object
  },

  getDefaultProps: function () {
    return {
      errors: {}
    };
  },

  handleChange: function (key) {
    return function (e) {
      var state = {};
      if (key == "enableRadius") {
        if (e.target.checked) {
          state.radius = 10;
        } else {
          delete state.radius;
        }
        state[key] = e.target.checked;
      } else {
        if (typeof e == "string") {
          state[key] = document.getElementById(e).value
        } else {
          state[key] = e.target.value;
        }
      }
      this.setState(state);
    }.bind(this);
  },

  isValid: function () {
    this.props.errors = {};
    var rules = {
      name: {
        required: true
      },
      minLevel: {
        required: true,
        type: "integer",
        min: 0,
        max: 100
      },
      maxLevel: {
        required: true,
        type: "integer",
        min: 0,
        max: 100
      },
      radius: {
        type: "integer",
        min: 0,
        max: 1000
      },
      detection_duration: {
        required: true,
        type: "integer",
        min: 30,
        max: 21600
      }
    };
    this.props.errors = helpers.checkValidity(this.state, rules);
    return (Object.keys(this.props.errors).length == 0);
  },

  handleSubmit: function (e) {
    e.stopPropagation();
    e.preventDefault();
    if (this.isValid()) {
      this.props.errors = {};
      var proximity = Object.assign({}, this.state);
      proximity.minLevel = parseInt(proximity.minLevel);
      proximity.maxLevel = parseInt(proximity.maxLevel);
      if (proximity.radius == "") {
        delete proximity.radius;
      } else {
        proximity.radius = parseInt(proximity.radius);
      };
      if (proximity.detection_duration == "") {
        delete proximity.detection_duration;
      } else {
        proximity.detection_duration = parseInt(proximity.detection_duration);
      }
      ReactBootstrap.Dispatcher.emit("Proximityform.save", proximity);
    } else {
      this.forceUpdate();
    }
  },

  handleReset: function (e) {
    e.stopPropagation();
    e.preventDefault();
    ReactBootstrap.Dispatcher.emit("Proximityform.reset", Object.assign({}, this.state));
  },

  handleDelete: function (e) {
    e.stopPropagation();
    e.preventDefault();
    if (confirm("Are you sure you want to Delete this Proximity Dimming profile?")) {
      ReactBootstrap.Dispatcher.emit("Proximityform.delete", Object.assign({}, this.state));
    };
  },

  callPreviousItem:function(allItems, currentItem){
    $('.proximity-previous').keyup();
    $("#Proximity-grid").data("gridInstance");
    console.log($("#Proximity-grid").data("gridInstance"));
    var currentRow = $("#Proximity-grid").data("gridInstance").getSelectedRows();
    var nextRow =  currentRow[0] -1
    $("#Proximity-grid").data("gridInstance").setSelectedRows([nextRow])
  },

  callNextItem:function(allItems, currentItem){

    $('.proximity-next').keydown();
    $("#Proximity-grid").data("gridInstance");
    console.log($("#Proximity-grid").data("gridInstance"));
    var currentRow = $("#Proximity-grid").data("gridInstance").getSelectedRows();
    var nextRow =  currentRow[0]+ 1;
    $("#Proximity-grid").data("gridInstance").setSelectedRows([nextRow])
  },

  componentWillReceiveProps: function (nextProps) {
    if (this.props.proximity.pdprofileid != nextProps.proximity.pdprofileid) {
      this.setState(nextProps.proximity);
    };
  },

  render: function () {
    var hstyle = {
      overflowY: "auto", overflowX: "hidden", marginBottom: "16px",
      maxHeight: helpers.calcHeight(100, -280) + "px !important"
    };
    var proximity = this.state;
    var heading = (proximity.name == "") ? "Add Profile" : (<span><Icon glyph="icon-fontello-right-dir" /> {proximity.name}</span>);
    var deleteBtnStyle = (proximity.pdprofileid == "") ? { display: "none" } : {};
    console.log('proximityform', this.state);

	  var currentLength = $("#Proximity-grid").data("gridInstance").getData().getLength();
	  var allItemsLength = this.props.allProximities.length;
	  var currentRow = $("#Proximity-grid").data("gridInstance").getSelectedRows();
	  var noNextItem = {}; var noPreviousItem = {};var displayArrows = {};
	  if(currentLength === allItemsLength ){
		  var firstItem = 0; var lastItem = allItemsLength - 1;
		  for (var a = 0; a < allItemsLength; a++) {
			  if (this.props.allProximities[a]!=null && this.props.proximity!=null && this.props.allProximities[a].pdprofileid === this.props.proximity.pdprofileid){
				  noNextItem = this.props.proximity.idx === lastItem ? {display:"none"}:{};
				  noPreviousItem = this.props.proximity.idx === firstItem ? {display:"none"}:{};
			  }
		  }
		  displayArrows = (this.props.proximity!=null && this.props.proximity.pdprofileid === "" ) ? { display: "none" } : {};
	  }else{
		  if(this.props.proximity!=null && this.props.proximity.pdprofileid != null){
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
	  if(proximity.pdprofileid === "0"){
		  displayArrows={display:"none"};
      }

    return (
      <div>

        <div className="netsense__form__header">
          <h3>{heading}</h3>
        </div>
        <div className="netsense__form__body">

            <span style={displayArrows}>
              <span style={noPreviousItem} className="rubix-icon icon-simple-line-icons-arrow-left proximity-previous" onClick={ () => this.callPreviousItem(this.props.allUsers,this.state.user)}></span>
           </span>

          <span style={displayArrows}>
             <span style={noNextItem} className="rubix-icon icon-simple-line-icons-arrow-right proximity-next" onClick={ () => this.callNextItem(this.props.allUsers,this.state.user)}></span>
          </span>

          <form role="form" className="form-horizontal" data-proximityid={proximity.pdprofileid} >
            <div style={hstyle}>
              <div className="form-group">
                <label htmlFor="name" className="control-label col-sm-3">Name:</label>
                <div className="col-sm-6">
                  <input type="text" className={(this.props.errors.name)? "form-control orange":"form-control"} id="name" ref="name" value={this.state.name} onChange={this.handleChange('name')} />
                  <div className="form-error">
                    {this.props.errors.name || ""}
                  </div>
                </div>
              </div>
              <div className="form-group">
                <label htmlFor="description" className="control-label col-sm-3">Description:</label>
                <div className="col-sm-6">
                  <input type="text" className="form-control" id="description" ref="description" value={this.state.description} onChange={this.handleChange('description')} />
                </div>
              </div>
              <div className="form-group">
                <label htmlFor="minLevel" className="control-label col-sm-3">Min level:</label>
                <div className="col-sm-6">
                  <input type="text" className={(this.props.errors.minLevel)? "form-control orange":"form-control"} id="minLevel" ref="minLevel" value={this.state.minLevel} onChange={this.handleChange('minLevel')} />
                  Minimum light level on absence (0-100)
              <div className="form-error">
                    {this.props.errors.minLevel || ""}
                  </div>
                </div>
              </div>
              <div className="form-group">
                <label htmlFor="maxLevel" className="control-label col-sm-3">Max level:</label>
                <div className="col-sm-6">
                  <input type="text" className={(this.props.errors.maxLevel)? "form-control orange":"form-control"} id="maxLevel" ref="maxLevel" value={this.state.maxLevel} onChange={this.handleChange('maxLevel')} />
                  Maximum light level on presence detection (0-100)
              <div className="form-error">
                    {this.props.errors.maxLevel || ""}
                  </div>
                </div>
              </div>
              <div className="form-group">
                <label htmlFor="beginTime" className="control-label col-sm-3">Begin time:</label>
                <div className="col-sm-6">
                  <TimeField fieldid="beginTime" info="Time of day to begin presence detection" value={this.state.beginTime} handler={this.handleChange('beginTime')} />
                </div>
              </div>
              <div className="form-group">
                <label htmlFor="endTime" className="control-label col-sm-3">End time:</label>
                <div className="col-sm-6">
                  <TimeField fieldid="endTime" info="Time of day to end presence detection" value={this.state.endTime} handler={this.handleChange('endTime')} />                  
                </div>
              </div>
              <div className="form-group">
                <label htmlFor="enableRadius" className="control-label col-sm-3">Specify radius?</label>
                <div className="col-sm-6 text-left">
                  <input style={{ width: "15px", height:"15px", marginLeft: "12px", marginTop: "18px" }} type="checkbox" disabled={this.state.enableRadiusDisabled} className="form-control" ref="enableRadius" checked={this.state.enableRadius} id="enableRadius" onChange={this.handleChange('enableRadius')} />
                  <br/> <br/> When selected (and the Profile is saved), you cannot unselect this option.<br />If you need to remove the radius, create a new Profile.
            </div>
              </div>
              <div className="form-group" style={{ display: this.state.enableRadius ? "block" : "none" }}>
                <label htmlFor="radius" className="control-label col-sm-3">Radius:</label>
                <div className="col-sm-6">
                  <input type="text" className={(this.props.errors.radius)? "form-control orange":"form-control"} id="radius" ref="radius" value={this.state.radius} onChange={this.handleChange('radius')} />
                  Radius in meters of co-located nodes for presence detection
              <div className="form-error">
                    {this.props.errors.radius || ""}
                  </div>
                </div>
              </div>
              <div className="form-group">
                <label htmlFor="detection_duration" className="control-label col-sm-3">Detection duration:</label>
                <div className="col-sm-6">
                  <input type="text" className={(this.props.errors.detection_duration)? "form-control orange":"form-control"} id="detection_duration" ref="detection_duration" value={this.state.detection_duration} onChange={this.handleChange('detection_duration')} />
                  Time beyond absence detection before dimming to min level (seconds)
              <div className="form-error">
                    {this.props.errors.detection_duration || ""}
                  </div>
                </div>
              </div>
            </div>
            <div style={{ margin: "20px 0px" }}>
              <div className="col-sm-3 text-right">
                <button type="button" id="pddelete"className="ns-delete-btn" style={deleteBtnStyle} onClick={this.handleDelete}>
                  <b>Delete</b></button>
              </div>
              <div className="col-sm-6 text-right">
                <button type="button" id="pdreset" className="ns-reset-btn" onClick={this.handleReset}>
                  <b>Reset</b></button>
                &nbsp; &nbsp;
              <button type="button" id="submit" className="ns-save-btn" onClick={this.handleSubmit}>
                  <b>Save</b></button>
              </div>
            </div>
          </form>
        </div>
      </div>
    );
  }
});

module.exports = Proximityform;
