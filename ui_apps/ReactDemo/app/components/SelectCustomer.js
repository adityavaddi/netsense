var React = require('react');
var Router = require('react-router');
var helpers = require('../utils/helpers');

var SelectCustomer = React.createClass({
  mixins: [Router.State, Router.Navigation],
  getInitialState: function(){
    return {
      customers: []
    }
  },
  handleChange: function(){
    var customerID = this.refs.customerID.getDOMNode().value;
    if (parseInt(customerID) != -1) {
      this.transitionTo('profile2', {customerID: customerID});
    }
  },
  componentDidMount: function() {
    helpers.getCustomerList()
     .then(function(dataObj){
        console.log("getCustomerList: " + JSON.stringify(dataObj));
        this.setState({
          customers: dataObj.data,
        });
      }.bind(this));
  },
  render: function(){
      var Customerlist = this.state.customers.map(function(customer, index){
      return (
        <option className="" key={index} value={customer.cid}>
          {customer.name} ({customer.cid})
        </option>
      );
    });

    return (
      <div className="col-sm-12">
        <form>
          <div className="form-group col-sm-6">
            <select className="form-control" ref="customerID" onChange={this.handleChange}>
              <option className="" value="-1">Select a Site</option>
              <option className="" value="1"> Valley Fair </option>
            </select>
          </div>
        </form>
      </div>
    )
  }
});

module.exports = SelectCustomer;