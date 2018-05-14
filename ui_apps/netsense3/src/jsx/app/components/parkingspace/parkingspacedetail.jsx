import classNames from 'classnames';
import { State, Navigation } from 'react-router';
import Parkingspaceform from 'components/parkingspace/parkingspaceform';
import { Modal } from 'react-bootstrap';

var ParkingspaceDetail = React.createClass({

  getInitialState: function() {
    return this.getparkingSpaces(this.props.parkingspaceID, this.props.parkingspaces)
  },

  propTypes: {
      parkingspaces: React.PropTypes.array.isRequired,
      parkingspaceID: React.PropTypes.string.isRequired,
      parkingspacesmetadata: React.PropTypes.array.isRequired
  },

    getparkingSpaces: function(parkingspaceID, parkingspaces) {
    if (parkingspaceID == "0" || parkingspaceID == "-1") {
      return {
          parkingspotid: "",
          name: "",
          parkinggroupid:"",
          parkinggroupname:"",
          demarcated: false,
          active: true,
          site: {siteid: NSN.siteID,
                 sitename: NSN.siteName
               }
      };
    };

      for (var i=0; i<parkingspaces.length; i++) {
         if (parkingspaces[i].parkingspaceid == parkingspaceID){
             return parkingspaces[i];
         }
      }

    return null;
  },

  componentDidMount: function () {
    $("#parkingspace-detail-panel").draggable({handle:"h2",cursor:"move"}).resizable({handles:"all"});
  },

  componentWillReceiveProps: function(nextProps){
      this.setState(this.getparkingSpaces(nextProps.parkingspaceID, nextProps.parkingspaces));
  },

  togglePin: function () {
      ReactBootstrap.Dispatcher.emit('ParkingspaceDetail.togglePin');
  },

  render: function() {
    var pinButton = (
        <div style={{ position: "absolute", top: "17px", right: "4px" }}>
          <img src={this.props.detail_state == "pinned"? "/imgs/new-icons/detach-dock.svg":"/imgs/new-icons/attach-dock.svg"}
                title={this.props.detail_state == "pinned"? "Undock (allow drag/resize)":"Dock"}
                height="24" onClick={this.togglePin} />
        </div>
    );

    if (this.props.parkingspaces.length == 0 && this.props.parkingspaceID != "0") {
      return (
        <div style={{padding:"20px 0px 0px 0px"}}>
          <div style={{textAlign:"center",fontSize:"54px",opacity:"0.5"}}>
              <Icon glyph="icon-fontello-info-circled" />
          </div>
          <h3 style={{textAlign:"center",lineHeight:"140%"}}>This site has no Space.</h3>
        </div>
        );
    };
    if (this.props.parkingspaceID == "-1") {
      return (
        <div>
            {pinButton}
            <h2 id="psDetailHeading" style={{textAlign: "center",padding: "100px 0px",fontSize: "24px"}} >
                <i>Select a Space from the table<br />or<br />from the map.</i>
            </h2>
        </div>
     );
    };

      return (
      <div>
        {pinButton}
        <Parkingspaceform parkingspace={this.state} defaultMetadataObject={this.props.defaultMetadataObject} detail_state={this.props.detail_state}/>
      </div>
    );
  }

});

module.exports = ParkingspaceDetail;