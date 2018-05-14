import classNames from 'classnames';
import auth from 'global/utils/auth';
import { State, Navigation } from 'react-router';

var Nodemultifixtureform = React.createClass({

  getInitialState: function(){
    return {"multifixtureid": "",
          }
  },

  propTypes: {
    selected_nodes: React.PropTypes.array.isRequired,
    fixtures: React.PropTypes.array.isRequired,
  },

  handleChange: function (key) {
    return function (e) {
      var state = {};
      state[key] = e.target.value;
      this.setState(state);
      debugger;
      console.log("vasanth here  ####", this);
      console.log("vasanth here in state ####", this.state);
    }.bind(this);
  },
 
  handleFixtureSubmit: function(e) {
    var that = this;
    e.stopPropagation();
    e.preventDefault();
    var node_info = {};
    Object.keys(this.state).forEach(function(key){
      if (that.state[key] !== "") {
        node_info[key] = that.state[key];
      }
    });
    ReactBootstrap.Dispatcher.emit("Nodemultiformfixture.save", node_info, this.props.selected_nodes);
    console.log("vasanth here #####", node_info)
  },

  componentWillReceiveProps: function(nextProps){
      if(this.props.fixtures && this.props.fixtures[0]) {
        this.setState({'multifixtureid': this.props.fixtures[0].fixtureid});
      }
  },

  render: function() {
    
    if (this.props.fixtures){
      var fOptions = this.props.fixtures.map(function(fixture, index){
        return (
          <option key={index} value={fixture.fixtureid}>{fixture.name}</option>
          );
      });
    };

    return (
      <div>
        <h5 style={{ position: "relative", backgroundColor: "#b0bed9", padding: "10px", color: "white" }}> Fixture Configuration</h5>
        <form role="form" className="form-horizontal">
          <div className="form-group row">
            <label htmlFor="multifixtureid" className="control-label col-sm-4" style={{ paddingLeft: "10px" }}>Fixture:</label>
            <div className="col-sm-6">
              <select className="form-control" id="multifixtureid" ref="multifixtureid" value={this.state.multifixtureid} onChange={this.handleChange('multifixtureid')} >
                {fOptions}
              </select>
            </div>
          </div>
{auth.allowed('CAN_CHANGE', 'NodeModel') &&
          (
          <div className="form-group row">
            <div className="col-sm-11 text-right">
              <button type="button" className="btn btn-success" onClick={this.handleFixtureSubmit}>
                <Icon glyph="icon-fontello-ok" /> Apply </button>
            </div> 
          </div>
          )
}
        </form> 
      </div>
      );
  }
});

  module.exports = Nodemultifixtureform;
