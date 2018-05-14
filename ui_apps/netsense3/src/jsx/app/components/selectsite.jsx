import classNames from 'classnames';
import auth from 'global/utils/auth';
import helpers from 'global/utils/helpers';

import { State, Navigation } from 'react-router';

var Selectsite = React.createClass({
  getInitialState: function () {
    return {
      siteID: null
    }
  },
  
  propTypes: {
    sites: React.PropTypes.array,
    route: React.PropTypes.string.isRequired
  },

  getDefaultProps: function() {
    return {sites: null};
  },

  handleChange: function (key) {
    return function (e) {
      var state = {};
      state[key] = e.target.value;
      this.setState(state);
    }.bind(this);
  },

  handleContinue: function(e) {
    e.preventDefault();
    e.stopPropagation();
    // set NSN.site*
    ReactBootstrap.Dispatcher.emit('selectSite.selected', object.assign({}, this.state));
  },

  componentDidMount: function() {
    $.ajax()

  },
 
  render() {   
    if (this.props.sites) {
      return (
      <div>
        <Row>
          <Col sm={12}>
            <div style={{width:"40%",minWidth:"400px",margin:"auto"}}>
            Please select a site:
            <select></select>
            </div>
          </Col>
        </Row>
      </div>
      );     
    } else {
      return (
        <div></div>
        )
    }
});

module.exports = Lightcontrol;

