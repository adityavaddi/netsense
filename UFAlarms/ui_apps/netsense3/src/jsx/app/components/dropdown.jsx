import classNames from 'classnames';

var Dropdown = React.createClass({

/*  This is the React-based replacement for the select tag.  It conforms to Verizon design guidelines.
    The options are passed in as an array of objects.  Each object has the format {optionvalue: optiontext}
*/
  getInitialState: function() {
    return {
      optionsVisible: false,
    };
  },

  getDefaultProps: function() {
    return {
      defaultvalue: "Select"
    }
  },
  
  select: function(option) {
    this.props.onChange({name: this.props.id, value: Object.keys(option)[0]});
  },
        
  show: function() {
    this.setState({ optionsVisible: true });
    document.addEventListener("click", this.hide);
  },
        
  hide: function() {
    this.setState({ optionsVisible: false });
    document.removeEventListener("click", this.hide);
  },

  renderOptions: function() {
    var options = [];
    var length =this.props.options.length;
    for (var i = 0; i < length; i++) {
      var option = this.props.options[i];
      options.push(<div onClick={this.select.bind(null, option)} 
                        className={Object.keys(option)[0] == this.props.value?"selected":""}>
        <span>{option[Object.keys(option)[0]]}</span>
      </div>);
    }
    return options;
  },
      
  render: function() {
    var that = this;
    var currentVal = this.props.value;
    var nonexistentVal = false;

    // if value is set but does not exist in options, give a message
    if (currentVal != "" && (typeof currentVal != "undefined") 
        && this.props.options.filter(function(option, index) {return Object.keys(option)[0] == currentVal}).length == 0) {
      console.log("In Dropdown, currentVal does not exist in options");
      nonexistentVal = true;
      currentVal = "";      
    }
    
    return (
      <dropdown>
        <div className={"dropdown-container" + (this.state.optionsVisible ? " show" : "")}>
          <div className={"dropdown-display" + (this.state.optionsVisible ? " clicked": "")} onClick={this.show}>
            <span>{(nonexistentVal || currentVal=="" || (typeof currentVal=="undefined"))?this.props.defaultvalue
                    :this.props.options.filter(function(option, index) {
                      return Object.keys(option)[0] == currentVal;
                      })[0][currentVal]}</span>
            <i className="fa fa-angle-down"></i>
          </div>
          <div className="dropdown-list">
            <div>
              {this.renderOptions()}
            </div>
          </div>
        </div>
      </dropdown>
      );
  }
        
});

module.exports = Dropdown;
