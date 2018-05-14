var React = require('react');
var Router = require('react-router');

var SearchCustomers = React.createClass({
  mixins: [Router.Navigation],
  handleSubmit: function(){
    var customerID = this.refs.customerID.getDOMNode().value;
    this.refs.customerID.getDOMNode().value = '';
    this.transitionTo('profile', {customerID: customerID});
  },
  render: function(){
    return (
      <div className="col-sm-12">
        <form onSubmit={this.handleSubmit}>
          <div className="form-group col-sm-7">
            <input type="text" className="form-control" ref="customerID" />
          </div>
          <div className="form-group col-sm-5">
            <button type="submit" className="btn btn-block btn-primary">Submit Customer ID </button>
          </div>
        </form>
      </div>
    )
  }
});

module.exports = SearchCustomers;