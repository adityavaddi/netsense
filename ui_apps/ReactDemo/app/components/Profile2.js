var React = require('react');
var Router = require('react-router');
var Nodelist = require('./Github/Nodelist');
var Nodemap = require('./Github/Nodemap');
var helpers = require('../utils/helpers');

var Profile2 = React.createClass({
  mixins: [Router.State, Router.Navigation],
  getInitialState: function(){
    return {
      nodes: []
    }
  },
  init: function() {
        this.setState({
          nodes: helpers.getNodelist().nodes
       });
  },
  componentDidMount: function(){
    this.init();
   },
  componentWillReceiveProps: function(){
    this.init();
  },
  render: function(){
    return (
      <div className="row">
        <div className="col-md-6">
          <Nodelist nodes={this.state.nodes} />
        </div>
        <div className="col-md-6">
          <Nodemap nodes={this.state.nodes} />
        </div>
      </div>
    )
  }
});

module.exports = Profile2;