import classNames from 'classnames';

var CheckBox = React.createClass({

/*  This is the React-based replacement for the checkbox field.  It conforms to Verizon design guidelines.
*/
  getInitialState: function() {
    return {
      checked: false,
    };
  },

  handleClick: function() {
    this.setState({checked: !this.state.checked});
  },
      
  render: function() {
    var that = this;
    var style = {display:"inline-block",
                height:this.props.size,
                width:this.props.size,
                border:"1px solid #333",
                borderRadius:"3px",
                cursor:"pointer"
              };
    var checkstyle = {position:"relative",
                      top:"-5px",
                      fontWeight:"bold",
                      fontSize:"12px"
              };
    return (
      <div style={style}
          onClick={this.handleClick}>
      {this.state.checked &&
        <span style={checkstyle}>X</span>
      }
      </div>
      );
  }
        
});

module.exports = CheckBox;
