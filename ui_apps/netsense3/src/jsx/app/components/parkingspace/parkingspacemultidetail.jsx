import classNames from 'classnames';
import { State, Navigation } from 'react-router';
import Parkingspacemultidetailform from 'components/parkingspace/parkingspacemultidetailform';
import helpers from 'global/utils/helpers';


var Parkingspacemultidetail = React.createClass({

  getInitialState: function() {
    return {
        showEditAttributes:false
    }
  },

  propTypes: {
    selected_spaces: React.PropTypes.array.isRequired,
    parkingspaces: React.PropTypes.array.isRequired,
    defaultMetadataObject:React.PropTypes.object.isRequired
  },

    togglePin: function () {
      ReactBootstrap.Dispatcher.emit('ParkingspaceDetail.togglePin');
  },


  componentWillReceiveProps: function(nextProps){
      this.forceUpdate();
  },

  render: function() {
    var pinButton = (
        <div style={{ position: "absolute", top: "0px", right: "4px" }}>
          <img src={this.props.detail_state == "pinned"? "/imgs/new-icons/detach-dock.svg":"/imgs/new-icons/attach-dock.svg"}  
                title={this.props.detail_state == "pinned"? "Undock (allow drag)":"Dock"}
                height="24" onClick={this.togglePin} />
        </div>
    );

      return (
            <div style={{marginTop:"24px"}}>
              {pinButton}
                <Parkingspacemultidetailform
                    selected_spaces={this.props.selected_spaces}
                    parkingspaces={this.props.parkingspaces}
                    defaultMetadataObject={this.props.defaultMetadataObject}/>
            </div>
          )
    }

});

module.exports = Parkingspacemultidetail;