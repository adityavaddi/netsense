import classNames from 'classnames';
import auth from 'global/utils/auth';
import helpers from 'global/utils/helpers';
import Dropdown from 'components/dropdown';

var Nodeconfigform = React.createClass({

  propTypes: {
    selected_nodes: React.PropTypes.array,
    selected_nodesmodel: React.PropTypes.array,
    singlenode: React.PropTypes.bool,
    node: React.PropTypes.object,
    configs: React.PropTypes.array.isRequired,
  },

  configOptions: [],

  getDefaultProps: function(){
    return {
      singlenode: true,
      node: {},
      selected_nodes: [],
      selected_nodesmodel: []
    }
  },

  getInitialState: function(){
    return {
      configid : ""
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

  handleViewConfig: function (e) {
    e.stopPropagation();
    e.preventDefault();
    ReactBootstrap.Dispatcher.emit("Nodeform.assignedconfig", "open", {configid: this.props.node.configid});
  },
 
  handleConfigSubmit: function(e) {
    var that = this;
    e.stopPropagation();
    e.preventDefault();
    if (confirm("Are you sure you want to update the configuration for the selected node(s)?")) {
      var configname = this.configOptions.filter(function(coption, index) {
                                                  return Object.keys(coption)[0] == that.state.configid;
                                                })[0][that.state.configid];
      ReactBootstrap.Dispatcher.emit("Nodeformconfig.save", 
                                      {configid:this.state.configid, name:configname}, 
                                      this.props.selected_nodes);
    };
  },

  componentDidMount: function() {
    if (!this.props.singlenode && this.props.configs === null) {
      ReactBootstrap.Dispatcher.emit('Nodeform.getConfigs');
    };
  },

  render: function() {

    var selectedmodelList = this.props.selected_nodesmodel;
    if (this.props.configs !== null){
      this.configOptions = this.props.configs.filter(function(config, index) {
        return selectedmodelList.indexOf(config.model) >= 0;
      }).map(function(config, index){
        var obj = {};
        obj[config.configid] = config.name;
        return obj;
      });
    };
 
    var bgStyle = this.props.singlenode
                  ?{ backgroundColor: (this.props.selected_nodes[0].configStatus == "pending")?"#f0ad4e":"#5cb85c", float:"left", width:"auto" }
                  :{};
    return (
      <div>

{ this.props.singlenode == false &&
        <h5 style={{position:"relative",backgroundColor:"#b0bed9",padding:"10px",color:"white"}}> Node Configuration </h5>
}

{ this.props.configs === null &&
        (
        <div style={{textAlign:"center",padding:"20px 0px"}}>
          <img src="/imgs/loading.gif" alt="Loading" />
        </div>
        )
}

{ this.props.configs !== null &&
        (
        <form role="form" className="form-horizontal">
  { this.props.singlenode &&
    (    <div>
          <div className="form-group col-sm-12 " style={{padding:"0px"}}>
            <label htmlFor="configname" className="control-label col-sm-4" style={{lineHeight:"18px",padding:"0px"}}>Configuration Name:</label>
            <div className="col-sm-8">
              <input type="text" className="form-control" disabled="disabled" id="configname" ref="configname" 
                style={{ backgroundColor: (this.props.node.configStatus == "pending")?"#f0ad4e":"#5cb85c"}} 
                value={this.props.node.configname} />
            </div>
          </div>
          <div className="form-group col-sm-12 " style={{padding:"0px"}}>
            <label htmlFor="configStatus" className="control-label col-sm-4" style={{lineHeight:"18px",padding:"0px"}}>Status:</label>
            <div className="col-sm-8">
              <input type="text" className="form-control" disabled="disabled" id="configStatus" ref="configStatus" value={this.props.node.configStatus} />
            </div>
          </div>

    { this.props.node.configname != "default" &&
            (
            <div className="text-right" style={{marginBottom:"12px"}}>
                <button className="ns-form-btn" id="viewNodeConfig" onClick={this.handleViewConfig}><b>View Configuration</b></button>
            </div>
            )
    }

        </div>
    )
  }

  {this.configOptions.length > 0 &&
          (
  <div>
          <div className="form-group">
            <label htmlFor="server" className="control-label col-sm-4">Set Configuration: </label>
            <div className="col-sm-8">
              <Dropdown options={this.configOptions}
                        className="form-control" 
                        id="configid" 
                        value={this.props.singlenode?this.props.node.configid:""} 
                        onChange={this.handleChange} />
            </div>
          </div>
    {auth.allowed('CAN_CHANGE', 'NodeModel') &&
            (
            <div className="form-group col-sm-12 text-right">
              <button className="ns-form-btn" onClick={this.handleConfigSubmit}><b>Apply</b></button>
            </div> 
            )
    }
</div>
          )
  }
  {this.configOptions.length == 0 &&
          (
          <div style={{textAlign:"center",padding:"12px"}}>
            No Configurations have been defined for this model.
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

  module.exports = Nodeconfigform;
