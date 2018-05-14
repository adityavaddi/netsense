var React = require('react');
var Main = require('../components/Main');
var Home = require('../components/Home');
var Profile2 = require('../components/Profile2');
var Router = require('react-router');
var DefaultRoute = Router.DefaultRoute;
var Route = Router.Route;

module.exports = (
  <Route name="app" path="/" handler={Main}>
    <Route name="profile2" path="profile2/:customerID" handler={Profile2} />
    <DefaultRoute handler={Home} />
  </Route>
);