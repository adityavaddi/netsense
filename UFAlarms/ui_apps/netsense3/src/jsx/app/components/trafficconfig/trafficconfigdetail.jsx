import classNames from 'classnames';
import { State, Navigation } from 'react-router';
import Trafficconfigform from 'components/trafficconfig/trafficconfigform';

var Trafficconfigdetail = React.createClass({

  getInitialState: function() {
    return this.getTrafficconfig(this.props.trafficconfigID, this.props.trafficconfigs);
  },

  propTypes: {
    trafficconfigs: React.PropTypes.array.isRequired,
    trafficconfigID: React.PropTypes.string.isRequired
  },

  getTrafficconfig: function(trafficconfigID, trafficconfigs) {
    console.log(trafficconfigID);
    for (var i=0; i<trafficconfigs.length; i++) {
      if (trafficconfigs[i].eventid == trafficconfigID){
        return (trafficconfigs[i]);
      }
    }
    return null;
  },

  togglePin: function () {
    ReactBootstrap.Dispatcher.emit('Trafficconfigdetail.togglePin');
  },

  componentWillReceiveProps: function(nextProps){
    this.setState(this.getTrafficconfig(nextProps.trafficconfigID, nextProps.trafficconfigs));
  },

  componentDidMount: function () {
    $("#trafficconfig-detail-panel").draggable({handle:"h2",cursor:"move"}).resizable({handles:"all"});
  },

  render: function() { 
    var pinButton = (
        <div style={{ position: "absolute", top: "0px", right: "4px", cursor: "pointer" }}>
          <img src={this.props.detail_state == "pinned"? "/imgs/new-icons/detach-dock.svg":"/imgs/new-icons/attach-dock.svg"} 
                title={this.props.detail_state == "pinned"? "Undock (allow drag/resize)":"Dock"}
                height="24" onClick={this.togglePin} />
        </div>
    );

    if (this.props.trafficconfigs.length == 0 && this.props.trafficconfigID != "0") {
      return (
        <div style={{padding:"20px 0px 0px 0px"}}>
          <div style={{textAlign:"center",fontSize:"54px",opacity:"0.5"}}>
              <Icon glyph="icon-fontello-info-circled" />
          </div>
          <h3 style={{textAlign:"center",lineHeight:"140%"}}>This site has no Traffic Configurations</h3>
        </div>
        );
    };
    if (this.props.trafficconfigID == "-1") {
      return (
        <div>
            {pinButton}
            <h2 style={{textAlign: "center",padding: "100px 0px",fontSize: "24px"}} >
                <i>Select a Traffic Configuration<br />from the table<br />or<br />from the map.</i>
            </h2>
        </div>
        );
    };

    return (
      <div>
        {pinButton}
        <Trafficconfigform trafficconfig={this.state} />
      </div>
    );
  }

});

module.exports = Trafficconfigdetail;