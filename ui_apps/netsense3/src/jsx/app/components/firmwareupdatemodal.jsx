import classNames from 'classnames';
import helpers from 'global/utils/helpers';
import {State, Navigation} from 'react-router';
import { Link, withRouter } from 'react-router';

var FirmwareUpdateModal = React.createClass({
  mixins: [State, Navigation],
  propTypes: {
    show: React.PropTypes.bool.isRequired,
    context: React.PropTypes.string.isRequired,
    entity: React.PropTypes.object.isRequired
  },

  handleClose: function() {
    ReactBootstrap.Dispatcher.emit(helpers.ucfirst(this.props.context) + 'form.assign', 'close');
  },

  handleTransition: function(){
    $(".firmware-display").fadeOut('fast');
    //this.transitionTo("/app/firmwareupdatepanel")
     this.props.router.push("/app/firmwareupdatepanel");
  },

  handleSubmit: function(e){
    var description = $("#description").val();
    ReactBootstrap.Dispatcher.emit("Firmwareform.save", this.props.entity,description);
    ReactBootstrap.Dispatcher.emit(helpers.ucfirst(this.props.context) + 'form.assign', 'close');
  },

  componentDidUpdate: function(){
    if (this.props.show) {
        $(".firmware-display").fadeIn('fast');
    } else {
        $(".firmware-display").fadeOut('fast');
    }
  },

  componentWillUnmount: function(){
  },

  render() {
    return (
      <div className="firmware-display">
        <div id="firmware-heading"> Firmware Update:</div>
        <div id="firmware" style={{marginBottom:"60px"}}>
          <form role="form" className="form-horizontal" style={{padding:"0 40px"}}>
            <div className="form-group">
              <label htmlFor="description" className="control-label col-sm-3">Description:</label>
              <div className="col-sm-6">
                <input type="text" className="form-control" id="description" />
              </div>
            </div>
          </form>
        </div>
        <div style={{position:"absolute",bottom:"10px",right:"10px"}}>
          <button type="button" className="ns-delete-btn" onClick={this.handleClose}>
          <Icon glyph="icon-fontello-cancel-circled"/> <b> Cancel </b></button>
          &nbsp; &nbsp;
          <button type="button" className="ns-save-btn" onClick={this.handleSubmit}>
          <Icon glyph="icon-fontello-ok" /> <b>Submit </b></button>
        </div>
      </div>
    );
  }
});


module.exports = withRouter(FirmwareUpdateModal);
