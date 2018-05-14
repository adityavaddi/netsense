var React = require('react');
var RouteHandler = require('react-router').RouteHandler;
var SearchCustomers = require('./SearchCustomers');
var SelectCustomer = require('./SelectCustomer');

var Main = React.createClass({
  render: function(){
    return (
      <div className="main-container">
        <nav className="navbar navbar-default" role="navigation">
          <div className="col-sm-8 col-sm-offset-4" style={{marginTop: 15}}>
            <SelectCustomer />
          </div>
        </nav>
        <div className="container">
          <RouteHandler />
        </div>
      </div>
    )
  }
});

module.exports = Main;