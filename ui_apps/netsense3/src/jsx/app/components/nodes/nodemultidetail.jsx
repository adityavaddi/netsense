import classNames from 'classnames';
import { State, Navigation } from 'react-router';
import Nodeconfigform from 'components/nodes/nodeconfigform';
import Nodemultiserverform from 'components/nodes/nodemultiserverform';
import Nodemultifixtureform from 'components/nodes/nodemultifixtureform';
import Nodemultirebootform from 'components/nodes/nodemultirebootform';
import Nodemultialertform from 'components/nodes/nodemultialertform';
import Nodemultigroupform from 'components/nodes/nodemultigroupform';
import helpers from 'global/utils/helpers';

var Nodemultidetail = React.createClass({

  getInitialState: function() {
    return null;
  },

  propTypes: {
    selected_nodes: React.PropTypes.array.isRequired,
    selected_nodesmodel: React.PropTypes.array.isRequired,
    fixtures: React.PropTypes.array.isRequired,
    detail_state: React.PropTypes.string
  },

  togglePin: function () {
      ReactBootstrap.Dispatcher.emit('Nodedetail.togglePin');
  },
 
  componentDidMount: function(){
    ReactBootstrap.Dispatcher.emit("Nodeform.getFixtures");
  },

  componentWillReceiveProps: function(nextProps){
      this.forceUpdate();
  },

  render: function() {
    var pinButton = (
        <div style={{ position: "absolute", top: "10px", right: "4px" }}>
          <img src={this.props.detail_state == "pinned"? "/imgs/new-icons/detach-dock.svg":"/imgs/new-icons/attach-dock.svg"}  
                title={this.props.detail_state == "pinned"? "Undock (allow drag)":"Dock"}
                height="24" onClick={this.togglePin} />
        </div>
    );
      
      var hstyle = {overflowY:"auto",overflowX:"hidden",marginBottom:"16px",
                  maxHeight:helpers.calcHeight(100, -200)+"px !important"};

      var selectednodesmodel = this.props.selected_nodesmodel;
      var modelcounter = 0;
      for(var m=1;m<selectednodesmodel.length;m++){
        console.log(m,selectednodesmodel[m]);

        if((selectednodesmodel[m] == selectednodesmodel[0])){
          modelcounter++;
        }
       
      }

      if(modelcounter == selectednodesmodel.length-1){
          return (
            <div style={{marginTop:"24px"}}>
              {pinButton}
              <h2 style={{position:"relative",top:"-16px",left:"9px",marginBottom:"-10px",width:"90%"}}>Multi-Node Operations</h2>
              <div style={{textAlign:"center",fontWeight:"bold",marginBottom:"0px"}}>
                Any entered values will be applied to the <br />{this.props.selected_nodes.length} selected nodes.
              </div>
              <div style={hstyle}>
                <Nodeconfigform singlenode={false} configs={this.props.configs} selected_nodesmodel={this.props.selected_nodesmodel} selected_nodes={this.props.selected_nodes} />
                <Nodemultiserverform selected_nodes={this.props.selected_nodes} />
                <Nodemultifixtureform fixtures={this.props.fixtures} selected_nodes={this.props.selected_nodes} />
                <Nodemultialertform multi_alerts={this.props.multi_alerts} />
                <Nodemultirebootform selected_nodes={this.props.selected_nodes} nodes={this.props.nodes} />
                <Nodemultigroupform groups = {this.props.groups} selected_nodes={this.props.selected_nodes} selected_nodesmodel={this.props.selected_nodesmodel} />
              </div>
            </div>
          )
      }
      else {
          return (
            <div style={{marginTop:"24px"}}>
              {pinButton}
              <h2 style={{position:"relative",top:"-16px",left:"9px",marginBottom:"-10px",width:"90%"}}>Multi-Node Operations</h2>
              <div style={{textAlign:"center",fontWeight:"bold",marginBottom:"0px"}}>
                Any entered values will be applied to the <br />{this.props.selected_nodes.length} selected nodes.
              </div>
              <div style={hstyle}>
                <Nodemultiserverform selected_nodes={this.props.selected_nodes} />
                <Nodemultifixtureform fixtures={this.props.fixtures} selected_nodes={this.props.selected_nodes} />
                <Nodemultialertform multi_alerts={this.props.multi_alerts} />
                <Nodemultirebootform selected_nodes={this.props.selected_nodes} nodes={this.props.nodes} />
                <Nodemultigroupform groups = {this.props.groups} selected_nodes={this.props.selected_nodes} selected_nodesmodel={this.props.selected_nodesmodel} />
              </div>
            </div>
          )
      }

     
  
    }

});

module.exports = Nodemultidetail;