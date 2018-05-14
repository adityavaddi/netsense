import classNames from 'classnames';
import auth from 'global/utils/auth';
import { State, Navigation } from 'react-router';

var Nodemultirebootform = React.createClass({

  getInitialState: function(){
    return {
      cmd: "ColdReset",
    }
  },

  propTypes: {
    selected_nodes: React.PropTypes.array.isRequired,
    nodes: React.PropTypes.array.isRequired,
  },

  handleChange: function (e) {
      var cmd = e.target.value
      this.setState({cmd})
  },
 
  handleNodeReset: function() {      
      var nodes_to_reboot = []
      var nodes = this.props.nodes
      var selected_nodes = this.props.selected_nodes
      for(var i in nodes){
          for(var j in selected_nodes){
            if(nodes[i].nodeid == selected_nodes[j]){
                nodes_to_reboot.push(nodes[j])
            }
          }
      }
    if (confirm("Are you sure you want to reset the selected nodes?")) {
        for(var key in nodes_to_reboot){
            ReactBootstrap.Dispatcher.emit("Nodeform.nodereset",nodes_to_reboot[key]);
        }    
    };
  },

  render: function() {
    var resetVisibility = ((NSN.userInfo.authorization[0].type == "sensity_user") || (NSN.userInfo.authorization[0].type == "partner_admin"));
    var resetOptions = (resetVisibility)?(
              <select className="form-control" id="cmd" ref="cmd" value={this.state.cmd} onChange={(e)=>this.handleChange(e)}>
                <option value="ColdReset">Cold Reset</option>
                <option value="ResetFactory">Factory Reset</option>
                <option value="ResetProvisioning">Provisioning Reset</option>
                <option value="ChangeFWPartition">Change FW Partition</option>
              </select>
              ):(
              <select className="form-control" id="cmd" ref="cmd" value={this.state.cmd} onChange={(e)=>this.handleChange(e)}>
                <option value="ResetProvisioning">Provisioning Reset</option>
                <option value="ChangeFWPartition">Change FW Partition</option>
              </select>
              );

    return(
      <div>
        <h5 style={{position:"relative",backgroundColor:"#b0bed9",padding:"10px",color:"white"}}>Node Reset</h5>
        <form role="form" className="form-horizontal">
          <div className="form-group row">
            <label htmlFor="changereset" className="control-label col-sm-4" style={{paddingLeft:"10px"}}>Select reset type::</label>
            <div className="col-sm-6">
                {resetOptions}
            </div>
            <div className="col-sm-2"/>
          </div>        
          <div className="form-group row">
            <div className="col-sm-11 text-right">
              <button type="button" className="btn btn-success" onClick={()=>this.handleNodeReset()}>
                <Icon glyph="icon-fontello-ok" /> Apply </button>
            </div>
          </div>
        </form>
      </div>
    )
  }
});

  module.exports = Nodemultirebootform;
