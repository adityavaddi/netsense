import classNames from 'classnames';
import { State, Navigation } from 'react-router';
import Parkinggroupform from 'components/parkinggroups/parkinggroupform';

var Parkinggroupdetail = React.createClass({

  getInitialState: function() {
    return this.getParkinggroup(this.props.parkinggroupID,this.props.parkinggroups);
  },

  propTypes: {
    parkingzones: React.PropTypes.array.isRequired,
    parkinggroups: React.PropTypes.array.isRequired,
    parkinggroupID: React.PropTypes.string.isRequired
  },

  getParkinggroup: function(parkinggroupID, parkinggroups) {
    if (parkinggroupID == "0" || parkinggroupID == "-1") {
      return {
          parkinggroupid: "",
          name: "",
          description: "",
          policy: "Standard parking",
          vehicle_types: "Car",
          site: {siteid: NSN.siteID,
                 sitename: NSN.siteName
               },
          parkingzones: "",
          zoneList: []
      };
    };


    for (var i=0; i<parkinggroups.length; i++) {
      if (parkinggroups[i].parkinggroupid == parkinggroupID){
        return (parkinggroups[i]);
      }
    }
    return null;
  },

  componentDidMount: function () {
    $("#parkinggroup-detail-panel").draggable({handle:"h2",cursor:"move"}).resizable({handles:"all"});
  },


  componentWillReceiveProps: function(nextProps){
      this.setState(this.getParkinggroup(nextProps.parkinggroupID, nextProps.parkinggroups));
  },

  togglePin: function () {
      ReactBootstrap.Dispatcher.emit('Parkinggroupdetail.togglePin');
  },

  render: function() {
    var pinButton = (
        <div style={{ position: "absolute", top: "15px", right: "4px" }}>
          <img src={this.props.detail_state == "pinned"? "/imgs/new-icons/detach-dock.svg":"/imgs/new-icons/attach-dock.svg"} 
                title={this.props.detail_state == "pinned"? "Undock (allow drag/resize)":"Dock"}
                height="24" onClick={this.togglePin} />
        </div>
    );

    if (this.props.parkingzones.length == 0) {
      return (
        <div>
          {pinButton}
          <div style={{padding:"20px 0px 0px 0px"}}>
            <div style={{textAlign:"center",fontSize:"54px",opacity:"0.5"}}>
                <Icon glyph="icon-fontello-info-circled" />
            </div>
            <h3 style={{textAlign:"center",lineHeight:"140%"}}>Authorized users can create <nobr>Parking Groups</nobr> on this page.</h3>
            <h3 style={{textAlign:"center",lineHeight:"140%"}}>However, no <nobr>Parking <i>Zones</i></nobr> have been defined for this site.</h3>          
            <h3 style={{textAlign:"center",lineHeight:"140%"}}>This must be done before <nobr>Parking Groups</nobr> can be created.</h3>
          </div>
        </div>
        );
    }
    if (this.props.parkinggroups.length == 0 && this.props.parkinggroupID != "0") {
      return (
        <div>
          {pinButton}
          <div style={{padding:"20px 0px 0px 0px"}}>
            <div style={{textAlign:"center",fontSize:"54px",opacity:"0.5"}}>
                <Icon glyph="icon-fontello-info-circled" />
            </div>
            <h3 style={{textAlign:"center",lineHeight:"140%"}}>Authorized users can create <nobr>Parking Groups</nobr> on this page.</h3>
          </div>
        </div>
        );
    };
    if (this.props.parkinggroupID == "-1") {
      return (
        <div>
          {pinButton}
          <h2 style={{textAlign:"center",padding:"100px 0px", fontSize: "24px"}}>Select a Parking Group.</h2>
        </div>
        );
    };
    return (
      <div>
        {pinButton}
        <Parkinggroupform parkinggroup={this.state} />
      </div>
    );
  }

});

module.exports = Parkinggroupdetail;