import classNames from 'classnames';
import auth from 'global/utils/auth';
import { State, Navigation } from 'react-router';

var Nodemultiserverform = React.createClass({

  getInitialState: function(){
    return {"server": "",
          }
  },

  propTypes: {
    selected_nodes: React.PropTypes.array.isRequired
  },

  handleChange: function (key) {
    return function (e) {
      var state = {};
      state[key] = e.target.value;
      this.setState(state);
    }.bind(this);
  },
 
  handleServerSubmit: function(e) {
    var that = this;
    e.stopPropagation();
    e.preventDefault();
    var form_info = {};
    Object.keys(this.state).forEach(function(key){
      if (that.state[key] !== "") {
        form_info[key] = that.state[key];
      }
    });
    ReactBootstrap.Dispatcher.emit("Nodemultiformserver.save", form_info, this.props.selected_nodes);
  },

  componentWillReceiveProps: function(nextProps){
//    if (this.props.customer.orgid != nextProps.customer.orgid){
//      this.setState(nextProps.customer);
//    };
  },

  render: function() {
    var isDisabled = !auth.allowed('CAN_CHANGE', 'NodeModel');
    return (
      <div>
        <h5 style={{backgroundColor:"#b0bed9",padding:"10px",color:"white"}}> Server </h5>
        <form role="form" className="form-horizontal">
          <div className="form-group row">
            <label htmlFor="server" className="control-label col-sm-4" style={{paddingLeft:"10px"}}>Server:</label>
            <div className="col-sm-6">
              <input disabled={isDisabled} type="text" className="form-control" id="server" ref="server" value={this.state.server} onChange={this.handleChange('server')} />
            </div>
          </div>
  {auth.allowed('CAN_CHANGE', 'NodeModel') &&
          (
          <div className="form-group row">
            <div className="col-sm-11 text-right">
              <button type="button" className="btn btn-success" onClick={this.handleServerSubmit}>
                <Icon glyph="icon-fontello-ok" /> Apply </button>
            </div> 
          </div>
          )
  }
        </form> 
      </div>
      );
 
  }
});

  module.exports = Nodemultiserverform;
