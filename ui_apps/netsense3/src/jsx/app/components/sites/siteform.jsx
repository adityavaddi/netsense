import classNames from 'classnames';
import { State, Navigation } from 'react-router';

import helpers from 'global/utils/helpers';
import auth from 'global/utils/auth';
import LightControl from 'components/lightcontrol';

var Siteform = React.createClass({

  getInitialState: function(){
    return this.props.site;
  },

  propTypes: {
    site: React.PropTypes.object.isRequired,
    errors: React.PropTypes.object,
    submitStatus: React.PropTypes.bool
  },

  addresses: {
    options: [],
    data: []
  },

  getDefaultProps: function() {
    return {
      errors: {}
    };
  },

  handleChange: function (key) {
    return function (e) {
      var state = {};
      state[key] = e.target.value;
      this.setState(state);
    }.bind(this);
  },

  handleAddressChange: function (key) {
      return function (e) {
          ReactBootstrap.Dispatcher.emit('Siteform.selectAddress', this, e.target.value);
      }.bind(this);
  },

  handleCheck: function(e) {
      ReactBootstrap.Dispatcher.emit('Siteform.checkAddress', this);
  },

  handleCancel: function(e){
    e.stopPropagation();
    e.preventDefault();
    ReactBootstrap.Dispatcher.emit("Siteform.cancel");
  },

  isValid: function() {
    this.props.errors = {};
    var rules = {
        name: {
            required: true
        },
        latitude: {
            required: true,
            min: -90,
            max: 90
        },
        longitude: {
            required: true,
            min: -180,
            max: 180
        }
    };
    this.props.errors = helpers.checkValidity(this.state, rules);
    return (Object.keys(this.props.errors).length == 0);
  },

  handleSubmit: function(e) {
    e.stopPropagation();
    e.preventDefault();
    if (this.isValid()) {
      this.props.errors={};
      ReactBootstrap.Dispatcher.emit("Siteform.save", Object.assign({},this.state));
    } else {
      this.forceUpdate();
    }
    return false;
  },

  handleSuspend: function(e) {
    e.stopPropagation();
    e.preventDefault();
    if (confirm("Are you sure you want to Suspend this site?")) {
      ReactBootstrap.Dispatcher.emit("Siteform.suspend", Object.assign({},this.state));
    };
  },

  componentWillReceiveProps: function(nextProps){
 //   if (this.props.site.siteid != nextProps.site.siteid){
      this.setState(nextProps.site);
      nextProps.errors = {};
      this.addresses = {
        options: [],
        data: []
      };
//    };
  },

  componentDidMount: function() {
    this.addresses = {
        options: [],
        data: []
    };
    this.props.errors = {};
  },

  render: function() {
    var hstyle = {
      overflowY: "auto", overflowX: "hidden", marginBottom: "16px",
      maxHeight: helpers.calcHeight(100, -280) + "px !important"
    };

    $('#save-site').remove('disabled');      
      if(!this.props.submitStatus) {
        $('#save-site').attr('disabled', true);
    } 
    var isDisabled = !auth.allowed('CAN_CHANGE', 'SiteModel');
    var visibility = auth.allowed('CAN_CHANGE', 'SiteModel')?{}:{display:"none"};
    var site = this.props.site;
    var suspendBtnStyle = (site.siteid=="" || !auth.allowed('CAN_SUSPEND', 'SiteModel'))?{display:"none"}:{};
    var heading = (site.name=="")?"Add Site":(<span><Icon glyph="icon-fontello-right-dir"/> {site.name}</span>);
    if (site.siteid != "" && auth.allowed("CAN_UPDATE", "ScheduleModel")) {
      var lightcontrol = (
        <div style={{border:"2px solid #000",borderRadius:"8px",padding:"10px 0px",width:"70%",margin:"72px auto 20px"}}>
          <div style={{textAlign:"center"}}>
            <h3>Site Lighting Control</h3>
            <h4>Manual Override</h4>         
            <LightControl event="Siteform.lightLevel" />
          </div>
        </div>
        );
    } else {
      lightcontrol = "";
    }
    return (
        <div>
            <div className="netsense__form__header">
            <h3>{heading}</h3>
            </div>
            <div className="netsense__form__body">
            <div style={hstyle}>
            <form role="form" className="form-horizontal" data-siteid={site.siteid}>
                <div className="form-group">
                    <label htmlFor="name" className="control-label col-sm-3">
                        Name:
                    </label>
                    <div className="col-sm-6">
                        <input disabled={isDisabled} type="text" className={(this.props.errors.name)? "form-control orange":"form-control"} id="name" ref="name" value={this.state.name} onChange={this.handleChange('name')}/>
                        <div className="form-error">
                            {this.props.errors.name || ""}
                        </div>
                    </div>
                </div>
                <div className="form-group">
                    <label htmlFor="street1" className="control-label col-sm-3">
                        Street 1:
                    </label>
                    <div className="col-sm-6">
                        <input disabled={isDisabled} type="text" className="form-control" id="street1" ref="street1" value={this.state.street1} onChange={this.handleChange('street1')}/>
                    </div>
                </div>
                <div className="form-group">
                    <label htmlFor="street2" className="control-label col-sm-3">
                        Street 2:
                    </label>
                    <div className="col-sm-6">
                        <input disabled={isDisabled} type="text" className="form-control" id="street2" ref="street2" value={this.state.street2} onChange={this.handleChange('street2')}/>
                    </div>
                </div>

                <div className="form-group">
                    <label htmlFor="contact_name" className="control-label col-sm-3">
                        Contact name:
                    </label>
                    <div className="col-sm-6">
                        <input disabled={isDisabled} type="text" className="form-control" id="contact_name" ref="contact_name" value={this.state.contact_name} onChange={this.handleChange('contact_name')}/>
                    </div>
                </div>

                <div className="form-group">
                    <label htmlFor="contact_phone" className="control-label col-sm-3">
                        Contact phone:
                    </label>
                    <div className="col-sm-6">
                        <input disabled={isDisabled} type="text" className="form-control" id="contact_phone" ref="contact_phone" value={this.state.contact_phone} onChange={this.handleChange('contact_phone')}/>
                    </div>
                </div>

                <div className="form-group">
                    <label htmlFor="contact_email" className="control-label col-sm-3">
                        Contact email:
                    </label>
                    <div className="col-sm-6">
                        <input disabled={isDisabled} type="email" className="form-control" id="contact_email" ref="contact_email" value={this.state.contact_email} onChange={this.handleChange('contact_email')}/>
                    </div>
                </div>

                <div className="form-group">
                    <label htmlFor="city" className="control-label col-sm-3">
                        City:
                    </label>
                    <div className="col-sm-6">
                        <input disabled={isDisabled} type="text" className="form-control" id="city" ref="city" value={this.state.city} onChange={this.handleChange('city')}/>
                    </div>
                </div>
                <div className="form-group">
                    <label htmlFor="state" className="control-label col-sm-3">
                        State:
                    </label>
                    <div className="col-sm-6">
                        <input disabled={isDisabled} type="text" className="form-control" id="state" ref="state" value={this.state.state} onChange={this.handleChange('state')}/>
                    </div>
                </div>
                <div className="form-group">
                    <label htmlFor="postal_code" className="control-label col-sm-3">
                        Postal code:
                    </label>
                    <div className="col-sm-6">
                        <input disabled={isDisabled} type="text" className="form-control" id="postal_code" ref="postal_code" value={this.state.postal_code} onChange={this.handleChange('postal_code')}/>
                    </div>
                </div>
                <div className="form-group">
                    <label htmlFor="country" className="control-label col-sm-3">
                        Country:
                    </label>
                    <div className="col-sm-6">
                        <input disabled={isDisabled} type="text" className="form-control" id="country" ref="country" value={this.state.country} onChange={this.handleChange('country')}/>
                    </div>
                </div>
                <div className="form-group">
                    <label htmlFor="latitude" className="control-label col-sm-3">
                        Latitude:
                    </label>
                    <div className="col-sm-6">
                        <input disabled={isDisabled} type="text" className={(this.props.errors.latitude)? "form-control orange":"form-control"} id="latitude" ref="latitude" value={this.state.latitude} onChange={this.handleChange('latitude')}/>
                        <div className="form-error">
                            {this.props.errors.latitude || ""}
                        </div>
                    </div>
                </div>
                <div className="form-group">
                    <label htmlFor="longitude" className="control-label col-sm-3">
                        Longitude:
                    </label>
                    <div className="col-sm-6">
                        <input disabled={isDisabled} type="text" className={(this.props.errors.longitude)? "form-control orange":"form-control"} id="longitude" ref="longitude" value={this.state.longitude} onChange={this.handleChange('longitude')}/>
                        <div className="form-error">
                            {this.props.errors.longitude || ""}
                        </div>
                    </div>
                </div>
                <div className={ (this.addresses && this.addresses.options.length) ? 'form-group' : 'form-group hidden'}>
                    <label htmlFor="longitude" className="control-label col-sm-3">
                        Addresses:
                    </label>
                    <div className="col-sm-6">
                        <select onChange={this.handleAddressChange()}>
                            {this.addresses.options}
                        </select>
                    </div>
                </div>

                <div className="col-sm-12 text-right">
                    <button type="button" id="ns-suspend-site" className="ns-suspend-btn" style={suspendBtnStyle} onClick={this.handleSuspend}><b>Suspend...</b></button>
                    &nbsp; &nbsp;
                    <button type="button" id="ns-validate-site" className="ns-validate-btn" onClick={this.handleCheck}><b>Validate Address</b></button>
                    &nbsp; &nbsp;
                    <button type="button" id="ns-cancel-site" className="ns-cancel-btn" onClick={this.handleCancel}><b>Cancel</b></button>
                    &nbsp; &nbsp;
                    <button style={visibility} type="submit" id="saveSite" className="ns-save-btn" onClick={this.handleSubmit}><b>Save</b></button>
                </div>
            </form>
            {lightcontrol}
            </div>
            </div>
        </div>
    );

  }
});

  module.exports = Siteform;