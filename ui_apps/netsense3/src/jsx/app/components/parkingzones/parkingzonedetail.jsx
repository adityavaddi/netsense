import classNames from 'classnames';
import { State, Navigation } from 'react-router';
import Parkingzoneform from 'components/parkingzones/parkingzoneform';

var Parkingzonedetail = React.createClass({

  getInitialState: function() {
    return this.getParkingzone(this.props.parkingzoneID,this.props.parkingzones);
  },

  propTypes: {
    parkingzones: React.PropTypes.array.isRequired,
    parkingzoneID: React.PropTypes.string.isRequired
  },

  getParkingzone: function(parkingzoneID, parkingzones) {
    if (parkingzoneID == "0" || parkingzoneID == "-1") {
      return {
          parkingzoneid: "",
          name: "",
          description: "",
          type: "Demarcated",
          available_spaces_num: 0,
          occupied_spaces_num: 0,
          max_spaces_num: 1,
          address: "",
          length: 4,
          car_length: 4,
          active: true,
          lat1: "",
          lat2: "",
          lng1: "",
          lng2: "",
          x1: 0,
          x2: 0,
          y1: 0,
          y2: 0,
          site: {siteid: NSN.siteID,
                 sitename: NSN.siteName
               }
      };
    };


    for (var i=0; i<parkingzones.length; i++) {
      if (parkingzones[i].parkingzoneid == parkingzoneID){
        return (parkingzones[i]);
      }
    }
    return null;
  },

  componentDidMount: function () {
    $("#parkingzone-detail-panel").draggable({handle:"h2",cursor:"move"}).resizable({handles:"all"});
  },

  componentWillReceiveProps: function(nextProps){
      this.setState(this.getParkingzone(nextProps.parkingzoneID, nextProps.parkingzones));
  },

  togglePin: function () {
      ReactBootstrap.Dispatcher.emit('Parkingzonedetail.togglePin');
  },

  render: function() {
    var pinButton = (
        <div style={{ position: "absolute", top: "15px", right: "4px" }}>
          <img src={this.props.detail_state == "pinned"? "/imgs/new-icons/detach-dock.svg":"/imgs/new-icons/attach-dock.svg"}  
                title={this.props.detail_state == "pinned"? "Undock (allow drag/resize)":"Dock"}
                height="24" onClick={this.togglePin} />
        </div>
    );

    if (this.props.parkingzones.length == 0 && this.props.parkingzoneID != "0") {
      return (
        <div style={{padding:"20px 0px 0px 0px"}}>
          <div style={{textAlign:"center",fontSize:"54px",opacity:"0.5"}}>
              <Icon glyph="icon-fontello-info-circled" />
          </div>
          <h3 style={{textAlign:"center",lineHeight:"140%"}}>This site has no Parking Zones.</h3>
        </div>
        );
    };
    if (this.props.parkingzoneID == "-1") {
      return (
        <div>
            {pinButton}
            <h2 id="pzDetailHeading" style={{textAlign: "center",padding: "100px 0px",fontSize: "24px"}} >
                <i>Select a Zone from the table<br />or<br />from the map.</i>
            </h2>
        </div>
     );
    };
    return (
      <div>
        {pinButton}
        <Parkingzoneform parkingzone={this.state} />
      </div>
    );
  }

});

module.exports = Parkingzonedetail;