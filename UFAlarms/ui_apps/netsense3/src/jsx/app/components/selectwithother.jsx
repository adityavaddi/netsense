import classNames from 'classnames';

import { State, Navigation } from 'react-router';

var SelectWithOther = React.createClass({

  propTypes: {
    label: React.PropTypes.string.isRequired,
    field: React.PropTypes.string.isRequired,
    options: React.PropTypes.array.isRequired,
    value: React.PropTypes.string.isRequired,
    handler: React.PropTypes.func.isRequired
  },
 
  render() {   
    var options = this.props.options.map(function(option, index) {
      return <option key={index}>{option}</option>
    });
    var other = this.props.options.indexOf(this.props.value) < 0;
    return (
      <div className="form-group">
        <label htmlFor={this.props.field} className="control-label col-sm-3">{this.props.label}:</label>
        <div className="col-sm-8">
          <select className="form-control" id={this.props.field} value={other?"Other":this.props.value} onChange={function(){this.value=this.value.replace("-other","");this.props.handler(this.props.field)}.bind(this)}>
            {options}
            <option value={this.props.value + '-other'}>Other</option>
          </select>
          <input disabled={!other} id={this.props.field + "-other"} type="text" className="form-control" value={other?this.props.value:""} onChange={this.props.handler(this.props.field)} />
        </div>
      </div>
    );     
  }
});

module.exports = SelectWithOther;

