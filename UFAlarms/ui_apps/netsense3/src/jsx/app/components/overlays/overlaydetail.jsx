import classNames from 'classnames';
import { State, Navigation } from 'react-router';
import Overlayform from 'components/overlays/overlayform';
import helpers from 'global/utils/helpers';

var Overlaydetail = React.createClass({

  getInitialState: function() {
    return this.getOverlay(this.props.overlayID);
  },
  
  propTypes: {
    overlays: React.PropTypes.array.isRequired,
    overlayID: React.PropTypes.string.isRequired
  },

  getOverlay(overlayID) {
    if (overlayID == "0" || overlayID == "-1") {
      return {
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
    };

    for (var i=0; i<this.props.overlays.length; i++) {
      if (this.props.overlays[i].overlayid == overlayID){
        return (this.props.overlays[i]);
      }
    }
   },

  render: function() {

    if (this.props.overlayID == "-1") {
      return (
        <h2 style={{textAlign:"center",padding:"100px 0px"}}>Please select a overlay</h2>
        );
    }

    var overlay = this.getOverlay(this.props.overlayID);
    return (
      <Overlayform overlay={overlay} />
     );
    }

});

module.exports = Overlaydetail; 



