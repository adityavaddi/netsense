import classNames from 'classnames';
import auth from 'global/utils/auth';

import { State, Navigation } from 'react-router';
import Overlaydatatable from 'components/overlays/overlaydatatable';

var Overlaylist = React.createClass({
 
   propTypes: {
    overlays: React.PropTypes.array.isRequired,
    overlayID: React.PropTypes.string
  },

  handleAdd: function() {
    ReactBootstrap.Dispatcher.emit("Overlaylist.add");
  },

  shouldComponentUpdate: function(nextProps,nextState){
    // This component does not need to be rerendered unless overlays have been added or changed
    return this.props.overlays !== nextProps.overlays;
  },


  render() {

    var Addbutton = (<span></span>);
    //if (auth.allowed('CAN_CREATE','OverlayModel')) {
       Addbutton = (
        <span style={{position:"relative",fontSize:"30px",color:"#3c3",cursor:"pointer",zIndex:"99999"}} title="Add Overlay">
          <Icon glyph="icon-fontello-plus-circle" onClick={this.handleAdd} />
        </span>
        );
    //};

    return (
      <div id="site-table-container">
        <h2 style={{position:"absolute",top:"-10px",left:"12px"}}>Overlays {Addbutton}</h2>
        <br/> <br/>
        <Overlaydatatable overlays={this.props.overlays} overlayID={this.props.overlayID}/>
      </div>
    );
 
  }
}
);

module.exports = Overlaylist;



