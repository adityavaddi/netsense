var React = require('react');
var Router = require('react-router');
var Sitelist = require('./Github/Sitelist');
var CustomerInfo = require('./Github/CustomerInfo');
var helpers = require('../utils/helpers');

var Profile = React.createClass({
  mixins: [Router.State, Router.Navigation],
  getInitialState: function(){
    return {
      info: {},
      sites: []
    }
  },
  init: function() {
   helpers.getCustomerInfo(this.getParams().customerID)
      .then(function(dataObj){
        this.setState({
          info: dataObj.info,
          sites: dataObj.sites
        });
      }.bind(this));
  },
  componentDidMount: function(){
    this.init();
   },
  componentWillReceiveProps: function(){
    this.init();
  },
  render: function(){
    var customerID = this.getParams().customerID;
    return (
      <div className="row">
        <div className="col-md-6">
          <CustomerInfo customerID={customerID} info={this.state.info} />
        </div>
        <div className="col-md-6">
          <Sitelist customerID={customerID} sites={this.state.sites} />
        </div>
      </div>
    )
  }
});

module.exports = Profile;