import classNames from 'classnames';
import auth from 'global/utils/auth';
import { State, Navigation } from 'react-router';

var Commissioningform = React.createClass({

  getInitialState: function () {
    return { "siteid": "" };

  },

  propTypes: {
    node: React.PropTypes.object.isRequired,
    sites: React.PropTypes.array.isRequired,
    isMultiSelected: React.PropTypes.bool.isRequired,
    nodes: React.PropTypes.array.isRequired,
  },

  calcHeight: function (pct, extra) {
    var h = (window && window.document) ? (($(window).height() - 75) * (pct / 100) + extra) : 500;
    return h;
  },

  handleChange: function (key) {
    return function (e) {
      var state = {};
      state[key] = e.target.value;
      this.setState(state);
    }.bind(this);
  },

  handleApply: function (e) {
    //e.preventDefault();
    //e.stopPropagation();
    this.props.toggleModal()
    this.props.isMultiSelected ?
      ReactBootstrap.Dispatcher.emit('Commissioningform.multiassignnode', this.state.siteid) :
      ReactBootstrap.Dispatcher.emit('Commissioningform.assignnode', this.props.node, this.state.siteid)
  },

  componentWillReceiveProps: function (nextProps) {
  },

  componentDidMount: function () {

    // If user is a sensity user:

    if (NSN.userInfo.authorization[0].type == "sensity_user") {
      $("#diagnosticwrapper").show();
    }
    else {
      $("#diagnosticwrapper").hide();
    }

  },

  render: function () {

    var sOptions = this.props.sites.map(function (site, idx) {
      return <option key={idx} value={site.siteid}>{site.name}</option>
    });
    var heading = this.props.isMultiSelected ? "Assigning Nodes: " + this.props.selected_nodes.length : "Assigning Node: " + this.props.node.nodeid;
    return (
      <div>
        <a className="CommissioningoverlayClose rubix-icon icon-ikons-close" style={{color:"#000"}} id="CommissioningoverlayClose" onClick={() => { this.props.toggleModal() }}></a>
        <div className=" netsense__form" style={{marginTop:"0px"}}>
          <div className="netsense__form__header">
            <h2 className="Commissioning-modal-header" style={{width:"90%"}}> {heading} </h2>
          </div>
          <form role="form" className="form-horizontal Commissioning-modal-content">
            <div className="form-group">
              <label htmlFor="siteid" className="control-label" style={{ fontSize: "larger", color: "black", marginLeft: "-26px" }}>Please select a site:</label>
              <select name="siteid" id="selectSites" style={{ fontSize: "20px", color: "black", position: "relative", width: "350px", marginLeft: "165px", bottom: "28px" }} onChange={this.handleChange("siteid")} value={this.state.siteid}>
                <option>Choose</option>
                {sOptions}
              </select>

            </div>
            <div className="text-right">
              <button type="button" className="ns-panel-btn" onClick={() => { this.handleApply() }}> &nbsp; Assign &nbsp; </button>
            </div>
          </form>
        </div>
      </div>
    );
  }
});
module.exports = Commissioningform;
