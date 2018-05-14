import classNames from 'classnames';
import auth from 'global/utils/auth';
import helpers from 'global/utils/helpers';
import Dropdown from 'components/dropdown';

var Nodemultigroupform = React.createClass({

  propTypes: {
    selected_nodes: React.PropTypes.array.isRequired,
    selected_nodesmodel: React.PropTypes.array.isRequired,
    groups: React.PropTypes.array
  },

  getInitialState: function(){
    return {
      multigroupid: ""
    }
  },

  handleChange: function (key) {
    if (typeof key == "object") {
      var state = {};
      state[key.name] = key.value;
      this.setState(state);
    } else {
      return function (e) {
        var state = {};
        state[key] = e.target.value;
        this.setState(state);
      }.bind(this);
    };
  },

  handleGroupSubmit: function(e) {
    var that = this;
    e.stopPropagation();
    e.preventDefault();
    var groups = that.props.groups;
    var selected_group = {};
    //Add to existing group
    for (var key in groups){
      if (that.state.multigroupid === groups[key].groupid) {
        selected_group = groups[key];
        if (selected_group.type == "lighting") {
          for (var i=0; i<that.props.selected_nodes.length; i++) {
            if (helpers.modelType(that.props.selected_nodesmodel[i]) != "Lighting") {
              noty({type:"error", text:"Only Lighting nodes can be added to a Lighting Group."});
              return;
            };
          };
        };
        selected_group.nodeList = _.union(selected_group.nodeList, that.props.selected_nodes);
      };
    };

    ReactBootstrap.Dispatcher.emit("Nodemultigroupform.save",selected_group);
  },

  componentDidMount: function() {
    if (this.props.groups === null) {
      ReactBootstrap.Dispatcher.emit('Nodeform.getGroups');
    }
  },

  render: function() {

    if (this.props.groups !== null){
      var groupOptions = this.props.groups.filter(function(group, index) {
        return group.type !== "site-lighting";
      }).map(function(group, index){
        var obj = {};
        obj[group.groupid] = group.name;
        return obj;
      });
    };

    return (
      <div>
        <h5 style={{position:"relative",backgroundColor:"#b0bed9",padding:"10px",color:"white"}}> Group </h5>

{ this.props.groups === null &&
        (
        <div style={{textAlign:"center",padding:"20px 0px"}}>
          <img src="/imgs/loading.gif" alt="Loading" />
        </div>
        )
}

{ this.props.groups !== null &&
        (
        <form role="form" className="form-horizontal">
          <div className="form-group">
            <label htmlFor="server" className="control-label col-sm-4" style={{paddingLeft:"10px"}}>Group: </label>
            <div className="col-sm-7">
              <Dropdown options={groupOptions}
                        className="form-control" 
                        id="multigroupid" 
                        value={this.state.multigroupid} 
                        onChange={this.handleChange} />
            </div>
          </div>
  { auth.allowed('CAN_CHANGE', 'NodeModel') &&
          (
          <div className="col-sm-11 text-right">
            <button type="button" className="btn btn-success" onClick={this.handleGroupSubmit}>
              <Icon glyph="icon-fontello-ok" /> Apply </button>
          </div> 
          )
  }
        </form> 
        )
}
      </div>
      );
 
  }
});

module.exports = Nodemultigroupform;