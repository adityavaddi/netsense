import classNames from 'classnames';
import { State, Navigation } from 'react-router';

var Overlayform = React.createClass({

  getInitialState: function(){
    return this.props.overlay
  },

  propTypes: {
    overlay: React.PropTypes.object.isRequired
  },

  handleChange: function (key) {
    return function (e) {
      var state = {};
      state[key] = e.target.value;
      this.setState(state);
    }.bind(this);
  },
 
  handleSubmit: function() {
    ReactBootstrap.Dispatcher.emit("Overlayform.save", Object.assign({},this.state));
    return false;
  },

  handleDelete: function() {
    ReactBootstrap.Dispatcher.emit("Overlayform.delete", Object.assign({},this.state));
    return false;
  },

  componentWillReceiveProps: function(nextProps){
    //console.log("CURRENT PROPS", JSON.stringify(this.props));
    //console.log("NEXT PROPS", JSON.stringify(nextProps));

    /*if (!nextProps.overlay) {
      nextProps.overlay = {
        overlayid: "",
        fileName: "",
        description: "",
        users: "",
        imageBounds: "",
        imageData: "",
        imageType: "",
        buildingLevel: "",
        idx: -1
      };
    } */

    if (this.props.overlay.overlayid != nextProps.overlay.overlayid){
      this.setState(nextProps.overlay);
    };
  },

  render: function() {
    var overlay = this.props.overlay;
    //console.log("OVERLAY", overlay);
    var heading = (overlay.fileName=="")?"Add Overlay":("Overlay Detail - " + overlay.fileName);

    return (
      <div>
        <h2 style={{position:"relative",top:"-40px",left:"12px"}}>{heading}</h2>
        <form role="form" className="form-horizontal" data-overlayid={overlay.overlayid} >
          <div className="form-group">
            <label htmlFor="name" className="control-label col-sm-3"><h4 style={{color:"black",margin:0}}>Name</h4></label>
            <div className="col-sm-6">
              <label htmlFor="value" className="control-label col-sm-3"><h4 style={{color:"black",margin:0}}>Value</h4></label>
            </div>
          </div>

          <div className="form-group">
            <label htmlFor="fileName" className="control-label col-sm-3">File Name:</label>
            <div className="col-sm-6">
              <input type="text" className="form-control" id="fileName" ref="fileName" value={this.state.fileName} onChange={this.handleChange('fileName')}/>
            </div>
          </div>
          <div className="form-group">
            <label htmlFor="description" className="control-label col-sm-3">Description:</label>
            <div className="col-sm-6">
              <input type="text" className="form-control" id="description" ref="description" value={this.state.description} onChange={this.handleChange('description')}/>
            </div>
          </div>
          <div className="form-group">
            <label htmlFor="users" className="control-label col-sm-3">Users:</label>
            <div className="col-sm-6">
              <input type="text" className="form-control" id="users" ref="users" value={this.state.users} onChange={this.handleChange('users')}/>
            </div>
          </div>
          <div className="form-group">
            <label htmlFor="users" className="control-label col-sm-3">ID:</label>
            <div className="col-sm-6">
              <input type="text" className="form-control" id="overlayid" ref="users" value={this.state.overlayid} onChange={this.handleChange('overlayid')}/>
            </div>
          </div>
          <div className="form-group">
            <label htmlFor="imageBounds" className="control-label col-sm-3">Image Bounds:</label>
            <div className="col-sm-6">
              <input type="text" className="form-control" id="imageBounds" ref="imageBounds" value={this.state.imageBounds} onChange={this.handleChange('imageBounds')}/>
            </div>
          </div>
          <div className="form-group">
            <label htmlFor="imageData" className="control-label col-sm-3">Image Data:</label>
            <div className="col-sm-6">
              <input type="text" className="form-control" id="imageData" ref="imageData" value={this.state.imageData} onChange={this.handleChange('imageData')}/>
            </div>
          </div>
          <div className="form-group">
            <label htmlFor="imageType" className="control-label col-sm-3">Image Type:</label>
            <div className="col-sm-6">
              <input type="text" className="form-control" id="imageType" ref="imageType" value={this.state.imageType} onChange={this.handleChange('imageType')}/>
            </div>
          </div>
          <div className="form-group">
            <label htmlFor="buildingLevel" className="control-label col-sm-3">Building Level:</label>
            <div className="col-sm-6">
              <input type="text" className="form-control" id="buildingLevel" ref="buildingLevel" value={this.state.buildingLevel} onChange={this.handleChange('buildingLevel')}/>
            </div>
          </div>
          <div className="col-sm-offset-5">
            <button type="button" className="btn btn-primary btn-lg" onClick={this.handleSubmit}>Save</button>
            <button type="button" className="btn btn-primary btn-lg" onClick={this.handleDelete} style={{marginLeft:"10px"}}>Delete</button>
          </div>
        </form>
      </div>
      );
 
  }
});

module.exports = Overlayform;


