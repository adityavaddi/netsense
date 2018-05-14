import classNames from 'classnames';

import { State, Navigation } from 'react-router';

var DisabledTextField = React.createClass({
  propTypes: {
    label: React.PropTypes.string.isRequired,
    fieldid: React.PropTypes.string.isRequired,
    cols: React.PropTypes.array,
    value: React.PropTypes.string
  },

  getDefaultProps: function() {
    return {cols: [4, 7],
            value: ""
          };
  },

  handleChange: function() {
    return;
  },
 
  render() {   
    var labelClass = "control-label col-sm-" + this.props.cols[0];
    var divClass = "col-sm-" + this.props.cols[1]; 
    return (
      <div className="form-group">
        <label htmlFor={this.props.fieldid} 
                className={labelClass} 
                style={{lineHeight:"18px", top: "9px"}}>{this.props.label}:</label>
        <div className={divClass}>
          <input type="text" className="form-control" 
                  style={{textOverflow:"ellipsis", paddingRight:"0px"}}
                  disabled="disabled" 
                  id={this.props.fieldid} 
                  ref={this.props.fieldid} 
                  value={this.props.value} 
                  title={this.props.value}
                  onChange={this.handleChange()} />
        </div>
      </div>
    );     
  }
});

module.exports = DisabledTextField;

