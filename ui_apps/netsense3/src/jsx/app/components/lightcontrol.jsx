import classNames from 'classnames';
import auth from 'global/utils/auth';
import helpers from 'global/utils/helpers';

import { State, Navigation } from 'react-router';

var Lightcontrol = React.createClass({
  getInitialState: function () {
    return {
      level: 50,
      duration: "15 min"
    }
  },
  propTypes: {
    event: React.PropTypes.string.isRequired,
    isDisabled: React.PropTypes.bool
  },

  getDefaultProps: function() {
    return {isDisabled: false};
  },

  handleChange: function (key) {
    return function (e) {
      var state = {};
      state[key] = e.target.value;
      this.setState(state);
    }.bind(this);
  },

  handleApply: function(e) {
    e.preventDefault();
    e.stopPropagation();
    ReactBootstrap.Dispatcher.emit(this.props.event, null, this.state.level, this.state.duration.replace(/ min/,"")*60);
  },

  handleReset: function(e) {
    e.preventDefault();
    e.stopPropagation();
    this.forceUpdate();
  },
 
  render() {   
    var visibility = this.props.isDisabled?{display:"none"}:{};   
    return (
    <div>
      <Row>
        <Col sm={12}>
          <div className="react-slider" style={{marginLeft:"10%",marginRight:"10%"}}>
            <label htmlFor="level">Level <input style={{width:"30px",border:"1px solid #CCC",textAlign:"center"}} type="text" id="currentlevel" value={this.state.level} /></label>
            <nobr>0<input disabled={this.props.isDisabled} type="range" style={{display:"inline"}} id="level" min="0" max="100" step="5" value={this.state.level} onChange={this.handleChange('level')} />100</nobr>
          </div>
        </Col>
      </Row>
      <Row>
        <Col sm={4} style={{margin:"10px"}}>
          <div className="react-select">
            <label htmlFor="duration">Duration</label>
            <select className="form-control" disabled={this.props.isDisabled} id="duration" value={this.state.duration} onChange={this.handleChange('duration')}>
              <option>5 min</option>
              <option>10 min</option>
              <option>15 min</option>
              <option>20 min</option>
              <option>30 min</option>
              <option>45 min</option>
              <option>60 min</option>
              <option>90 min</option>
              <option>120 min</option>
            </select>
          </div>
        </Col>
        <Col sm={8}>
          <div style={visibility} className="col-sm-12 text-right">
            <button type="button" id="ns-apply-light-control" className="ns-save-btn" onClick={this.handleApply}>
                <b>Apply</b>
            </button>
          </div>
        </Col>
      </Row>
    </div>
    );     
  }
});

module.exports = Lightcontrol;

