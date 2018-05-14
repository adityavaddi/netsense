import classNames from 'classnames';
import auth from 'global/utils/auth';
import helpers from 'global/utils/helpers';
import { State, Navigation } from 'react-router';

var Parkingzoneform = React.createClass({

  getInitialState: function(){
    return this.props.parkingzone
  },

  propTypes: {
    parkingzone: React.PropTypes.object.isRequired,
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
      ReactBootstrap.Dispatcher.emit("Parkingzoneform.save", Object.assign({}, this.state));
    } else {
      this.forceUpdate();
    }
  },

  handleReset: function(e) {
    e.stopPropagation();
    e.preventDefault();
    ReactBootstrap.Dispatcher.emit("Parkingzoneform.reset", Object.assign({},this.state));
  },

  handleDelete: function(e) {
    e.stopPropagation();
    e.preventDefault();
    if (confirm("Are you sure you want to Delete this Parking Zone?")) {
      ReactBootstrap.Dispatcher.emit("Parkingzoneform.delete", Object.assign({},this.state));
    };
  },

  componentWillReceiveProps: function(nextProps){
//    if (this.props.parkingzone.parkingzoneid != nextProps.parkingzone.parkingzoneid){
      this.setState(nextProps.parkingzone);
//    };
  },

  render: function() {

        if ($.isArray(this.state.world_bounding_box)) {
          var wbb = {lon: [], lat: []};
          for (var j=0; j<4; j++) {
            wbb.lon[j] = this.props.parkingzones[i].world_bounding_box[j].longitude;
            wbb.lat[j] = this.props.parkingzones[i].world_bounding_box[j].latitude;
          };
        } else {
          wbb = this.state.world_bounding_box;
        };

    var hstyle = {overflowY:"auto",overflowX:"hidden",marginLeft:"8px",marginBottom:"16px",
                  maxHeight:helpers.calcHeight(100, -180)+"px !important"};
    var parkingzone = this.state;
    var heading = (<span><Icon glyph="icon-fontello-right-dir"/> {parkingzone.name != ""?parkingzone.name:parkingzone.parkingzoneid}</span>);

    var deleteBtnStyle = (parkingzone.parkingzoneid=="")?{display:"none"}:{};

    var bounding_box = "null";
    if (this.state.world_bounding_box != null) {
      bounding_box = (
            <div>Lat1: <input type="text" disabled={true} className="form-control" value={wbb.lat[0]} style={{display:"inline",width:"80%"}} onChange={this.handleChange('available_spaces')} />
              <div style={{clear:"both",marginBottom:"10px"}}></div>Lng1: <input type="text" disabled={true} className="form-control" value={wbb.lon[0]} style={{display:"inline",width:"80%"}} onChange={this.handleChange('available_spaces')} />
              <div style={{clear:"both",marginBottom:"10px"}}></div>Lat2: <input type="text" disabled={true} className="form-control" value={wbb.lat[1]} style={{display:"inline",width:"80%"}} onChange={this.handleChange('available_spaces')} />
              <div style={{clear:"both",marginBottom:"10px"}}></div>Lng2: <input type="text" disabled={true} className="form-control" value={wbb.lon[1]} style={{display:"inline",width:"80%"}} onChange={this.handleChange('available_spaces')} />
              <div style={{clear:"both",marginBottom:"10px"}}></div>Lat3: <input type="text" disabled={true} className="form-control" value={wbb.lat[2]} style={{display:"inline",width:"80%"}} onChange={this.handleChange('available_spaces')} />
              <div style={{clear:"both",marginBottom:"10px"}}></div>Lng3: <input type="text" disabled={true} className="form-control" value={wbb.lon[2]} style={{display:"inline",width:"80%"}} onChange={this.handleChange('available_spaces')} />
              <div style={{clear:"both",marginBottom:"10px"}}></div>Lat4: <input type="text" disabled={true} className="form-control" value={wbb.lat[3]} style={{display:"inline",width:"80%"}} onChange={this.handleChange('available_spaces')} />
              <div style={{clear:"both",marginBottom:"10px"}}></div>Lng4: <input type="text" disabled={true} className="form-control" value={wbb.lon[3]} style={{display:"inline",width:"80%"}} onChange={this.handleChange('available_spaces')} />
            </div>
        )
    };
    return (
      <div>
        <h2 style={{position:"relative",top:"-10px",left:"12px",fontSize:"24px"}}>{heading}</h2>
        <form role="form" className="form-horizontal" data-parkingzoneid={parkingzone.parkingzoneid} >
<div style={hstyle}>
          <div className="form-group">
            <label htmlFor="name" className="control-label col-sm-3">Name:</label>
            <div className="col-sm-8">
              <input type="text" disabled={true} className={(this.props.errors.name)? "form-control orange":"form-control"} id="name" ref="name" value={this.state.name} onChange={this.handleChange('name')} />
              <div className="form-error">
                  {this.props.errors.name || ""}
              </div>
            </div>
          </div>
          <div className="form-group">
            <label htmlFor="type" className="control-label col-sm-3">Type:</label>
            <div className="col-sm-8">
              <input type="text" disabled={true} className="form-control" id="type" ref="type" value={this.state.type} onChange={this.handleChange('type')} />
            </div>
          </div>
          {this.state.type != "Demarcated"
            ?(
              <div className="form-group">
                <label htmlFor="length" className="control-label col-sm-3">Length:</label>
                <div className="col-sm-8">
                  <input type="text" disabled={true} className="form-control" id="length" ref="length" value={this.state.length} onChange={this.handleChange('length')} />
                </div>
              </div>
          ):""}
          <div className="form-group">
            <label htmlFor="parkingzoneid" className="control-label col-sm-3">ID:</label>
            <div className="col-sm-8">
              <input type="text" disabled={true} className="form-control" id="parkingzoneid" ref="parkingzoneid" value={this.state.parkingzoneid} onChange={this.handleChange('parkingzoneid')} />
            </div>
          </div>
          <div className="form-group">
            <label htmlFor="nodeid" className="control-label col-sm-3">Video Node:</label>
            <div className="col-sm-8">
              <input type="text" disabled={true} className="form-control" id="nodeid" ref="nodeid" value={this.state.nodeid} onChange={this.handleChange('nodeid')} />
            </div>
          </div>
          <div className="form-group">
            <label htmlFor="channel" className="control-label col-sm-3">Camera Channel:</label>
            <div className="col-sm-8">
              <input type="text" disabled={true} className="form-control" id="channel" ref="channel" value={this.state.channel} onChange={this.handleChange('channel')} />
            </div>
          </div>
          <div className="form-group">
            <label htmlFor="active" className="control-label col-sm-3">Active:</label>
            <div className="col-sm-1 text-left">
              <input type="checkbox" disabled={true} className="form-control" id="active" ref="active" checked={this.state.active} value={this.state.active} onChange={this.handleChange('active')} />
            </div>
          </div>
          <div className="form-group">
            <label htmlFor="max_spaces" className="control-label col-sm-3" style={{lineHeight:"18px",paddingTop:"4px"}}>Maximum Spaces (est.):</label>
            <div className="col-sm-8">
              <input type="text" disabled={true} className="form-control" id="max_spaces" ref="max_spaces" value={this.state.max_spaces} onChange={this.handleChange('max_spaces')} />
            </div>
          </div>
          <div className="form-group">
            <label htmlFor="occupied_spaces" className="control-label col-sm-3" style={{lineHeight:"18px",paddingTop:"4px"}}>Occupied Spaces:</label>
            <div className="col-sm-8">
              <input type="text" disabled={true} className="form-control" id="occupied_spaces" ref="occupied_spaces" value={this.state.occupied_spaces} onChange={this.handleChange('occupied_spaces')} />
            </div>
          </div>
          <div className="form-group">
            <label htmlFor="available_spaces" className="control-label col-sm-3" style={{lineHeight:"18px",paddingTop:"4px"}}>Available Spaces:</label>
            <div className="col-sm-8">
              <input type="text" disabled={true} className="form-control" id="available_spaces" ref="available_spaces" value={this.state.available_spaces} onChange={this.handleChange('available_spaces')} />
            </div>
          </div>
          <div className="form-group">
            <label htmlFor="parkinggroupname" className="control-label col-sm-3" style={{lineHeight:"18px",paddingTop:"4px"}}>Parking Group:</label>
            <div className="col-sm-8">
              <input type="text" disabled={true} className="form-control" id="parkinggroupname" ref="parkinggroupname" value={this.state.parkinggroupname} onChange={this.handleChange('parkinggroupname')} />
            </div>
          </div>
          <div className="form-group">
            <label htmlFor="tags" className="control-label col-sm-3" style={{lineHeight:"18px",paddingTop:"4px"}}>Tags:</label>
            <div className="col-sm-8">
              <input type="text" disabled={true} className="form-control" id="tags" ref="tags" value={this.state.tags} onChange={this.handleChange('tags')} />
            </div>
          </div>
          <div className="form-group">
            <label htmlFor="world_bounding_box" className="control-label col-sm-3" style={{lineHeight:"18px",paddingTop:"4px"}}>World Bounding Box:</label>
            <div className="col-sm-8">
              {bounding_box}
            </div>
          </div>

</div>

        </form>

      </div>
      );
 
  }
});

  module.exports = Parkingzoneform;
