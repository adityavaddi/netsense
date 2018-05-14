import classNames from 'classnames';
import auth from 'global/utils/auth';
import helpers from 'global/utils/helpers';

import { State, Navigation } from 'react-router';

var TimeField = React.createClass({
  getInitialState: function () {
    return  { value: this.props.value }
  },

  propTypes: {
    fieldid: React.PropTypes.string.isRequired,
    info: React.PropTypes.string,
    showIcons: React.PropTypes.bool,
    handler: React.PropTypes.func.isRequired
  },

  getDefaultProps: function() {
    return {
      info: "",
      showIcons: false
    }
  },

  timeregs : ['^[012]$',
              '^([01][0-9]|2[0-3])$',
              '^([01][0-9]|2[0-3])\:$',
              '^([01][0-9]|2[0-3])\:[0-5]$',
              '^([01][0-9]|2[0-3])\:[0-5][0-9]$',
              '^([01][0-9]|2[0-3])\:[0-5][0-9]\:$',
              '^([01][0-9]|2[0-3])\:[0-5][0-9]\:[0-5]$',
              '^([01][0-9]|2[0-3])\:[0-5][0-9]\:[0-5][0-9]$'
              ],

  timecomplete :  '^([01][0-9]|2[0-3])\:[0-5][0-9]\:[0-5][0-9]$',

  sunsetregs : ['^s$',
              '^su$',
              '^sun$',
              '^suns$',
              '^sunse$',
              '^sunset$',
              '^sunset[\+\-]$',
              '^sunset[\+\-][1-9]$',
              '^sunset[\+\-][1-9][0-9]$',
              '^sunset[\+\-]([1-2][0-9][0-9]|300)$'
              ],
  sunsetcomplete : '^sunset([\+\-]([1-9]|([1-9][0-9])|([1-2][0-9][0-9])|(300)))?$',

  sunriseregs : ['^s$',
              '^su$',
              '^sun$',
              '^sunr$',
              '^sunri$',
              '^sunris$',
              '^sunrise$',
              '^sunrise[\+\-]$',
              '^sunrise[\+\-][1-9]$',
              '^sunrise[\+\-][1-9][0-9]$',
              '^sunrise[\+\-]([1-2][0-9][0-9]|300)$'
              ],
  sunrisecomplete : '^sunrise([\+\-]([1-9]|([1-9][0-9])|([1-2][0-9][0-9])|(300)))?$',

  errormsg : "<span style='color:#F00'>HH:MM:SS <i>or</i> (sunrise|sunset)+/-nnn</span>",

  componentWillReceiveProps: function (nextProps) {
    this.setState({value: nextProps.value});
  },

  cleanup: function() {
    var that = this;
    function status_good() {
      e.style.borderColor = "#0C0"; 
      status.className = "";
      status.src="/imgs/timefield/greencheck.png";
      msg.innerHTML = that.props.info;    
    };
    function status_error() {
      e.style.borderColor = "#F00";
      status.src = "/imgs/timefield/redx.png";
      status.className = "pulsenow";
      msg.innerHTML = that.errormsg; 
    };

    var e = document.getElementById(this.props.fieldid);
    var status = document.getElementById(this.props.fieldid + "-status");
    var msg = document.getElementById(this.props.fieldid + "-msg");
    var v = e.value;
    if (v.length == 0) {
      return;
    }
    if (v.match("^[0-9]$")) {
      e.value = "0" + v + ":00:00";
      status_good();
      this.setState({value: e.value});
      this.props.handler(this.props.fieldid);
      return;
    }
    if (v.match(this.timeregs[1])) {
      e.value = v + ":00:00";
      status_good();
      this.setState({value: e.value});
      this.props.handler(this.props.fieldid);
      return;
    }
    if (v.match(this.timeregs[4])) {
      e.value = v + ":00";
      status_good();
      this.setState({value: e.value});
      this.props.handler(this.props.fieldid);
      return;
    }
    if (!(v.match(this.timecomplete) || v.match(this.sunsetcomplete) || v.match(this.sunrisecomplete))) {
      status_error();
      this.setState({value: e.value});
      this.props.handler(this.props.fieldid);
    }
  },

  checkit: function() {
    var that = this;
    function status_good() {
      e.style.borderColor = "#0C0"; 
      status.className = "";
      status.src="/imgs/timefield/greencheck.png";
      msg.innerHTML = that.props.info;    
    };
    function status_error() {
      e.style.borderColor = "#F00";
      status.src = "/imgs/timefield/redx.png";
      status.className = "pulsenow";
      msg.innerHTML =that.errormsg;
    };
    function status_none() {
      e.style.borderColor = "#CCC";
      status.src = "/imgs/timefield/blank.png";
      status.className = "";
      msg.innerHTML = that.props.info;
    };
    var e = document.getElementById(this.props.fieldid);
    var status = document.getElementById(this.props.fieldid + "-status");
    var msg = document.getElementById(this.props.fieldid + "-msg");
    var v = e.value;
    switch (v) {
      case "":
        status_none();
        break;
      case "ss":
        e.value="sunset";
        status_good()
        break;
      case "sr":
        e.value="sunrise";
        status_good()
        break;
      default:
        if (v.match(this.timecomplete) || v.match(this.sunsetcomplete) || v.match(this.sunrisecomplete)) {
          status_good();
        } else {
          if (v.match(this.timeregs[Math.min(v.length - 1, this.timeregs.length - 1)])
            || v.match(this.sunsetregs[Math.min(v.length - 1, this.sunsetregs.length - 1)])
            || v.match(this.sunriseregs[Math.min(v.length - 1, this.sunriseregs.length - 1)])) {
            status_none();
          } else {
            status_error(); 
          }
        };
      };
      this.props.handler(this.props.fieldid);
      this.setState({value: e.value});
    },
 
  render: function() {  
//    console.log("rendering TimeField: this.props=" + JSON.stringify(this.props)); 
//    console.log("                     this.state=" + JSON.stringify(this.state));
    var iconstyle = {marginLeft:"8px",height:"24px",width:"24px",verticalAlign:"bottom"};
    iconstyle.display = this.props.showIcons?"block":"none";
    return (
      <div>
        <div>
          <input className="form-control timefield" type="text" autoComplete="off" id={this.props.fieldid} name={this.props.fieldid} 
              value={this.state.value} onChange={this.checkit} onBlur={this.cleanup} />
          <img src="/imgs/timefield/blank.png" style={iconstyle} id={this.props.fieldid + "-status"} />
        </div>
        <div id={this.props.fieldid + "-msg"} style={{lineHeight:"16px",paddingTop:"6px",paddingLeft:"12px",fontSize:"14px",color:"#888",fontWeight:"normal"}} >
              {this.props.info}
        </div>
      </div>
      )
  }
});

module.exports = TimeField;

