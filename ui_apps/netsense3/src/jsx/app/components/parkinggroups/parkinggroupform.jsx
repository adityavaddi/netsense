import classNames from 'classnames';
import auth from 'global/utils/auth';
import helpers from 'global/utils/helpers';
import DisabledTextField from 'components/disabledtextfield'
import { State, Navigation } from 'react-router';

var Parkinggroupform = React.createClass({

  getInitialState: function(){
    return this.props.parkinggroup
  },

  propTypes: {
    parkinggroup: React.PropTypes.object.isRequired,
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
      state[key] = e.target.value;
      this.setState(state);
    }.bind(this);
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

  handleSubmit: function(e) {
    e.stopPropagation();
    e.preventDefault();
    if (this.isValid()) {
      this.props.errors={};
      ReactBootstrap.Dispatcher.emit("Parkinggroupform.save", Object.assign({}, this.state));
    } else {
      this.forceUpdate();
    }
  },

  handleReset: function(e) {
    e.stopPropagation();
    e.preventDefault();
    ReactBootstrap.Dispatcher.emit("Parkinggroupform.reset", Object.assign({},this.state));
  },

  handleDelete: function(e) {
    e.stopPropagation();
    e.preventDefault();
    if (confirm("Are you sure you want to Delete this Parking Group?")) {
      ReactBootstrap.Dispatcher.emit("Parkinggroupform.delete", Object.assign({},this.state));
    };
  },

  highlightGroup: function() {
    ReactBootstrap.Dispatcher.emit('Parkinggroupform.showGroup', this.state.zoneList, this.state.parkinggroupid);
  },

  componentWillReceiveProps: function(nextProps){
    if (this.props.parkinggroup.parkinggroupid != nextProps.parkinggroup.parkinggroupid){
      this.highlight = true;
      this.setState(nextProps.parkinggroup);
    } else {
      this.highlight = false;
    }
  },


  componentDidUpdate: function(){
    if (this.highlight) {
      this.highlightGroup();
    };
  },


  componentDidMount: function() {
    if (this.state.zoneList.length > 0) {
      this.highlightGroup();
    };
    var that = this;
    ReactBootstrap.Dispatcher.on("Parkinggroupmap.addZone", function(parkingzoneid) {
      var newState = React.addons.update(that.state, {zoneList: { $push : [parkingzoneid] }});
      that.setState(newState);
    });
    ReactBootstrap.Dispatcher.on("Parkinggroupmap.removeZone", function(parkingzoneid) {
      var newZoneList = that.state.zoneList;
      newZoneList = newZoneList.filter(function(e){return e!==parkingzoneid});
      var newState = React.addons.update(that.state, {zoneList: { $set : newZoneList }});
      that.setState(newState);
    });
  },

  componentWillUnmount: function() {
    ReactBootstrap.Dispatcher.removeAllListeners("Parkinggroupmap.addZone");
    ReactBootstrap.Dispatcher.removeAllListeners("Parkinggroupmap.removeZone");
  },

  render: function() {
    var hstyle = {overflowY:"auto",overflowX:"hidden",marginBottom:"16px",marginLeft: "13px",
                  maxHeight:helpers.calcHeight(80,-120)+"px !important"};
    var parkinggroup = this.state;
    var heading = (parkinggroup.name=="")?"Add Parking Group":(<span><Icon glyph="icon-fontello-right-dir"/> {parkinggroup.name}</span>);

    var deleteBtnStyle = (parkinggroup.parkinggroupid=="")?{display:"none"}:{};

    var zonelist = parkinggroup.zoneList, zonecount;
    if (zonelist.length==0) {
      zonelist = (<div>No zones in group</div>);
      zonecount = 0;
    } else {
      zonecount = zonelist.length;
      zonelist = zonelist.map(function(zone, index) {
        return (<div key={index} style={{textAlign:"center"}}>{zone}</div>);
      })
    };

    return (
      <div>
        <h2 style={{position:"relative",top:"-20px",left:"12px",marginRight:"36px"}}>{heading}</h2>
        <form role="form" className="form-horizontal" data-parkinggroupid={parkinggroup.parkinggroupid} >
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

{(helpers.isInternalUser() && this.props.parkinggroup.parkinggroupid != "") &&
          (
          <DisabledTextField cols={[3,8]} label="ID" fieldid="parkinggroupid" value={this.props.parkinggroup.parkinggroupid} />
          )
}
          <div className="form-group">
            <label htmlFor="description" className="control-label col-sm-3">Description:</label>
            <div className="col-sm-8">
              <input type="text" className="form-control" id="description" ref="description" value={this.state.description} onChange={this.handleChange('description')} />
            </div>
          </div>
          {/*<div className="form-group">*/}
            {/*<label htmlFor="vehicle_type" className="control-label col-sm-3">Vehicle Type:</label>*/}
            {/*<div className="col-sm-8">*/}
              {/*<select className="form-control" id="vehicle_types" ref="vehicle_types" value={this.state.vehicle_types} onChange={this.handleChange('vehicle_types')} >*/}
                {/*<option>Car</option>*/}
                {/*<option>Truck</option>*/}
                {/*<option>Motorcycle</option>*/}
                {/*<option>Other</option>*/}
              {/*</select>*/}
            {/*</div>*/}
          {/*</div>*/}
          <div className="form-group">
            <label htmlFor="zonelist" className="control-label col-sm-3">Zones ({zonecount}):</label>
            <div className="col-sm-8">
              <div style={{height:"160px",padding:"2px 12px",overflow:"auto",border:"1px solid #CCC"}}>
                {zonelist}
              </div>
            </div>
          </div>
</div>
          <div>
            <div className="col-sm-3">
              <button type="button" className="ns-delete-btn" style={deleteBtnStyle} onClick={this.handleDelete}>
                <Icon glyph="icon-fontello-trash" /> Delete </button>
            </div>
            <div className="col-sm-8 text-right">
              <button type="button" className="ns-save-btn" onClick={this.handleSubmit}>
                <Icon glyph="icon-fontello-ok" /> Save </button>
            </div>
          </div>
        </form>
      </div>
      );
 
  }
});

  module.exports = Parkinggroupform;