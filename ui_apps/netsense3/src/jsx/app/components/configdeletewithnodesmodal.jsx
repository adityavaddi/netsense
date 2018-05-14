import classNames from 'classnames';
import helpers from 'global/utils/helpers';
import { State, Navigation } from 'react-router';

var ConfigDeleteWithNodesModal = React.createClass({

  propTypes: {
    showdeletealert: React.PropTypes.bool.isRequired,
    context: React.PropTypes.string.isRequired,
    entity: React.PropTypes.object.isRequired
  },

  handleClose: function() {
    ReactBootstrap.Dispatcher.emit(helpers.ucfirst(this.props.context) + 'form.deleteconfigwithnodes', 'close');
  },

  componentDidUpdate: function(){

    if (this.props.showdeletealert) {
        $(".config-displayalert").fadeIn('fast');
    } else {
        $(".config-displayalert").fadeOut('fast');
    }
  },

  componentWillUnmount: function(){
  },

  render() {
    return (
      <div className="config-displayalert">
        <div id="config" style={{marginBottom:"60px",marginTop:"60px"}}>
          <div className="infoMessage"> 
            <h3 style={{margin:"20px"}}> The selected config cannot be deleted as it has been assigned to {this.props.entity} node(s). </h3>
          </div>
        </div>
        <div style={{position:"absolute",bottom:"10px",right:"10px"}}>
          <button type="button" className="btn btn-info" onClick={this.handleClose}>
          <Icon glyph="icon-fontello-ok-circled"/> OK</button>
        </div>
      </div>
    );
  }
});



module.exports = ConfigDeleteWithNodesModal;
