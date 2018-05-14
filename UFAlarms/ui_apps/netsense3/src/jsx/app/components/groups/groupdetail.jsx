import classNames from 'classnames';
import { State, Navigation } from 'react-router';
import Groupform from 'components/groups/groupform';

var Groupdetail = React.createClass({

  getInitialState: function() {
    return this.getGroup(this.props.groupID, this.props.groups);
  },

  propTypes: {
    groups: React.PropTypes.array.isRequired,
    schedules: React.PropTypes.array.isRequired,
    etdhprofiles: React.PropTypes.array.isRequired,
    pdprofiles: React.PropTypes.array.isRequired,
    groupID: React.PropTypes.string.isRequired
  },

  getGroup: function(groupID, groups) {
    if (groupID == "0" || groupID == "-1") {
      return {
          name: "",
          nodeList: [],
          type: "lighting",
          description: "",
          groupid: "",
          schedules: [],
          etdhprofiles: [],
          pdprofiles: [],
          idx: -1
      };
    };
    for (var i=0; i<groups.length; i++) {
      if (groups[i].groupid == groupID){
        return (groups[i]);
      }
    }
    return null;
  },

  componentDidMount: function () {
    $("#group-detail-panel").draggable({handle:"h2",cursor:"move"}).resizable({handles:"all"});
  },

  componentWillReceiveProps: function(nextProps){
    if (this.props.groupID != nextProps.groupID){
      this.setState(this.getGroup(nextProps.groupID, nextProps.groups));
    };
  },

  togglePin: function () {
      ReactBootstrap.Dispatcher.emit('Groupdetail.togglePin');
  },

  render: function() {
    var pinButton = (
        <div style={{ position: "absolute", top: "15px", right: "4px" }}>
          <img src={this.props.detail_state == "pinned"? "/imgs/new-icons/detach-dock.svg":"/imgs/new-icons/attach-dock.svg"}  
                title={this.props.detail_state == "pinned"? "Undock (allow drag/resize)":"Dock"}
                height="24" onClick={this.togglePin} />
        </div>
    );

    if (this.props.groupID == "-1") {
      return (
        <div>
        {pinButton}
        <h2 style={{textAlign:"center",padding:"100px 0px"}}>Select a group</h2>
        </div>
        );
    }

    return (
      <div>
      {pinButton}
      <Groupform group={this.state} schedules={this.props.schedules} etdhprofiles={this.props.etdhprofiles} pdprofiles={this.props.pdprofiles} nodes={this.props.nodes}/>
      </div>
    );
  }

});

module.exports = Groupdetail;