import classNames from 'classnames';
import helpers from '../../../../global/utils/helpers';
import auth from '../../../../global/utils/auth';
import { State, Navigation } from 'react-router';
import DataUtil from '../../service/datautil';
import React from 'react'

var Customerform = React.createClass({

  getInitialState: function () {
    return {
      customer: this.props.customer,
      // sites: []
    }
  },

  init: function () {
    // console.log("trigger")
    // DataUtil.getAll('sites', this.processSitesData)
  },

  propTypes: {
    customer: React.PropTypes.object.isRequired,
    allCustomers: React.PropTypes.array.isRequired,
    errors: React.PropTypes.object
  },

  _onlyPartners: function (customers) {
    var customersLength = customers.length;
    var partners = [];
    for (var i = 0; i < customers.length; i++) {
      if (customers[i].type === "partner") {
        partners.push(customers[i]);
      }
    }
    return partners;
  },

  getPartnerForCustomer: function (customer, customers) {

    for (var c = 0; c < customers.length; c++) {
      if (customers[c].orgid === customer.po) {

        return customers[c];

      }
    }
    return null;
  },

  getAllCustomersForPartners: function (partner, customers) {

    var sortedCustomers = [];
    if (partner != null) {
      for (var i = 0; i < customers.length; i++) {
        if (partner.orgid === customers[i].po && customers[i].type != "partner") {
          sortedCustomers.push(customers[i]);
        }
      }
    }
    return sortedCustomers;
  },



  filterEntity: function (arr) {
    var d, a = arguments, L = a.length, ax;
    while (L > 1 && arr.length) {
      d = a[--L];
      while ((ax = arr.indexOf(d)) !== -1) {
        arr.splice(ax, 1);
      }
    }
    return arr;
  },

  createCustomerList: function (eachCustomerList) {
    return <ul className="netsense__form--list">
      {eachCustomerList.map((eachCustomer) => {
        return <li onClick={() => { ReactBootstrap.Dispatcher.emit('Customerlist.select', eachCustomer.orgid) }} className="netsense__form--list--item site-list-name">{eachCustomer.name}</li>
      })}
    </ul>
  },

  addresses: {
    options: [],
    data: []
  },

  getDefaultProps: function () {
    return {
      errors: {}
    };
  },

  handleChange: function (key) {
    return function (e) {
      // debugger
      var state = this.state.customer;
      switch (key) {
        case 'type':
          state[key] = e.target.checked;
          break;
        default:
          state[key] = e.target.value;
      };
      this.setState({ customer: state });
    }.bind(this);
  },

  isValid: function () {
    this.props.errors = {};
    var rules = {
      name: {
        required: true
      }
    };
    this.props.errors = helpers.checkValidity(this.state.customer, rules);
    return (Object.keys(this.props.errors).length == 0);
  },

  handleSubmit: function (e) {
    e.stopPropagation();
    e.preventDefault();
    if (this.isValid()) {
      this.props.errors = {};
      ReactBootstrap.Dispatcher.emit("Customerform.save", Object.assign({}, this.state.customer));
    } else {
      this.forceUpdate();
    }
    return false;
  },

  handleReset: function (e) {
    // console.log("RESET",this.props)
    e.stopPropagation();
    e.preventDefault();
    this.setState({
      customer: this.props.customer
      // sites: []
    }, () => { ReactBootstrap.Dispatcher.emit("Customerform.reset", Object.assign({}, this.state.customer)); })

  },

  // handleSites: function (e) {
  //   DataUtil.getAll('sites', this.processSitesData)
  // },
  // processSitesData: function (data) {
  //   console.log("initial trigger")
  //   this.setState({
  //     sites: data
  //   });
  // },
  // ///////////////////// Callback Function to get all sites/////////////////

  // makeSiteObj: function (site, index) {

  //   site.idx = index;
  //   site.street1 = site.street1 || "";
  //   site.street2 = site.street2 || "";
  //   site.city = site.city || "";
  //   site.state = site.state || "";
  //   site.postal_code = site.postal_code || "";
  //   site.country = site.country || "";
  //   site.latitude = site.latitude || "";
  //   site.longitude = site.longitude || "";
  //   return site;
  // },

  callPreviousItem: function (allItems, currentItem) {

    $('.customer-previous').keyup();
    $("#Customer-grid").data("gridInstance");
    console.log($("#Customer-grid").data("gridInstance"));
    var currentRow = $("#Customer-grid").data("gridInstance").getSelectedRows();
    var nextRow = currentRow[0] - 1
    $("#Customer-grid").data("gridInstance").setSelectedRows([nextRow])
  },

  callNextItem: function (allItems, currentItem) {

    $('.customer-next').keydown();
    $("#Customer-grid").data("gridInstance");
    console.log($("#Customer-grid").data("gridInstance"));
    var currentRow = $("#Customer-grid").data("gridInstance").getSelectedRows();
    var nextRow = currentRow[0] + 1;
    $("#Customer-grid").data("gridInstance").setSelectedRows([nextRow])
  },


  handleSuspend: function (e) {
    e.stopPropagation();
    e.preventDefault();
    if (confirm("Are you sure you want to Suspend this customer?")) {
      ReactBootstrap.Dispatcher.emit("Customerform.suspend", Object.assign({}, this.state.customer));
    };
  },

  handleAddressChange: function (key) {
    return function (e) {
      ReactBootstrap.Dispatcher.emit('Customerform.selectAddress', this, e.target.value);
    }.bind(this);
  },

  handleCheck: function (e) {
    ReactBootstrap.Dispatcher.emit('Customerform.checkAddress', this);
  },

  componentDidMount: function () {
    // ReactBootstrap.Dispatcher.emit('Customer.select', this.props.customer.name);
  },

  componentWillReceiveProps: function (nextProps) {

    if (this.props.customer!=null && nextProps.customer!=null && this.props.customer.orgid != nextProps.customer.orgid) {
	    this.setState({
        customer: nextProps.customer
      });

    }
  },

  render: function () {
    var sites = !this.props.sites.length ? (<div >No Sites</div>) : (<div className="netsense__form--sites">
      <h4 className="sites-label">Sites: </h4>
      <div className="col-md-12">
        <ul className="netsense__form--list">
          {
            this.props.sites.map((eachSite) => {
              return <li onClick={() => { ReactBootstrap.Dispatcher.emit('Customerlist.selectSites', this.props.customer.orgid) }} className="netsense__form--list--item site-list-name">{eachSite.name}</li>
            })
          }
        </ul>
      </div>
    </div>)

    var partner = (this.state.customer.type== "partner")? (this.state.customer): this.getPartnerForCustomer(this.state.customer, this.props.allCustomers);
    var customerList = this.getAllCustomersForPartners(partner, this.props.allCustomers);

    var modifiedCustomerList = [];
     var initialList = [];
    if (this.state.customer.type== "partner"){
    var i = 0;
    var tempCustomerList = [];
   
    var isInitial = true;
    for (var each in customerList) {

      if (i < 3 && isInitial) {
        initialList.push(customerList[each]);
      } else {
        isInitial = false;
        if (i < 3) {
          tempCustomerList.push(customerList[each]);
        } else {
          modifiedCustomerList.push(tempCustomerList);
          tempCustomerList = [];
          tempCustomerList.push(customerList[each]);
          i = 0;
        }
      }
      i++;
    }
     modifiedCustomerList.push(tempCustomerList);
    }
  else {
      tempCustomerList = [];
      tempCustomerList.push(this.props.customer);
      modifiedCustomerList.push(tempCustomerList);
  }

    var hierarchy = (this.state.customer.po == undefined) ? (<div></div>) : (<div className="netsense__form--hierarchy">
      <h4> Hierarchy: </h4>
      <div className="col-md-12">
        <div className="netsense__body--partner">
          <span>
            <img src="/imgs/account1.svg" style={{ height: "25", width: "30", position: "relative" }} />
          </span>
          <ul className="netsense__form--list">
            <li className="netsense__form--list--item"> {partner ? partner.name : ""}</li>
          </ul>
        </div>
        <div className="netsense__body--customer">
          <ul className="netsense__form--list">
            <span>
              <img src="/imgs/group.svg" style={{ height: "25", width: "30",marginTop: "25px" ,position: "relative" }} />
            </span>
            <div className="netsense__site--description--body--hierarchy">
              <ul className="netsense__form--list">
                {
                  initialList.map((eachCustomer) => {
                    return <li onClick={() => { ReactBootstrap.Dispatcher.emit('Customerlist.select', eachCustomer.orgid) }} className="netsense__form--list--item site-list-name">{eachCustomer.name}</li>
                  })
                }
              </ul>

              {/* <div className="netsense__site--description--more more--customer collapsed" onClick={() => this.setState({ open: !this.state.open })}> More
              {this.state.open ?
                  <span className="more-icon rubix-icon icon-fontello-down-dir-1"></span>
                  :<span className="more-icon rubix-icon icon-fontello-right-dir-1"></span>
              }
          </div> */}

              <div>
                {/*Management*/}
                <div className="netsense__site--content">
                  {
                    modifiedCustomerList.map((eachCustomerList) => {
                      return this.createCustomerList(eachCustomerList);
                    })
                  }
                </div>
              </div>

            </div>
          </ul>
        </div>
      </div>
    </div>)
    var isDisabled = !auth.allowed('CAN_CHANGE', 'OrgModel');
    var visibility = auth.allowed('CAN_CHANGE', 'OrgModel') ? {} : { display: "none" };
    var customer = this.state.customer;
    var suspendBtnStyle = (customer.orgid == "efe5bdb3-baac-5d8e-6cae57771c13" || customer.orgid == "" || !auth.allowed('CAN_DELETE', 'OrgModel')) ? { display: "none" } : {};
    var heading = (customer.orgid == "") ? "Add Customer" : (<span><span glyph="icon-fontello-right-dir" /> {customer.name}</span>);

    var partnerTypeVisibility = (NSN.userInfo.authorization[0].type == "sensity_admin") ? {} : { display: "none" };

    var typeDisabled = (customer.orgid == "") ? "" : "disabled";
    if (this.state.customer == null) {
      return (
        <div>
          Loading . .
         </div>
      )
    }

    var hstyle = {
      overflowY: "auto", overflowX: "hidden", marginBottom: "16px",
      maxHeight: helpers.calcHeight(100, -240) + "px !important"
    };

    return (
      <div>
        {/* New UXD Spec Form Elements*/}
        <div>

          <div className="netsense__form__header">
            <h3> {this.state.customer.name} </h3>
          </div>
          <span className="ns-span-line"></span>
          <div className="netsense__form__body">
<div style={hstyle}>
            <form role="form" className="form-horizontal" data-customerid={customer.orgid}>
              <div className="col-md-12">
                <div className="col-md-5">
                  <label htmlFor="name" className="control-label netsense__form__label">Account
                      name</label>
                  <input disabled={typeDisabled} type="text" className={(this.props.errors.name)? "form-control netsense__form__input orange":"form-control netsense__form__input"} value={this.state.customer.name} id="name" ref="name" onChange={this.handleChange('name')} />
                  <div className="form-error">
                    {this.props.errors.name || ""}
                  </div>
                </div>
                <div className="col-md-6 netsense__form__second--set">
                  <label htmlFor="contact_name" className="control-label netsense__form__label">Contact name</label>
                  <input disabled={isDisabled} className="form-control netsense__form__input" id="contact_name" ref="contact_name" value={this.state.customer.contact_name} onChange={this.handleChange('contact_name')} />
                  <div className="form-error">
                  </div>
                </div>
              </div>
              <div className="col-md-12">
                <div className="col-md-5">
                  <label htmlFor="street1" className="control-label netsense__form__label">Address line
                      1</label>
                  <input disabled={isDisabled} className="form-control netsense__form__input" id="street1" ref="street1" value={this.state.customer.street1} onChange={this.handleChange('street1')} />
                  <div className="form-error">
                  </div>
                </div>
                <div className="col-md-6 netsense__form__second--set">
                  <label htmlFor="contact_phone" className="control-label netsense__form__label ">Contact phone</label>
                  <input disabled={isDisabled} className="form-control netsense__form__input" id="contact_phone" ref="contact" value={this.state.customer.contact_phone} onChange={this.handleChange('contact_phone')} />
                  <div className="form-error">
                  </div>
                </div>
              </div>

              <div className="col-md-12">
                <div className="col-md-5">
                  <label htmlFor="street2" className="control-label netsense__form__label">Address line
                      2</label>
                  <input disabled={isDisabled} className="form-control netsense__form__input" id="street2" ref="street2" value={this.state.customer.street2} onChange={this.handleChange('street2')} />
                  <div className="form-error">
                  </div>
                </div>
                <div className="col-md-6 netsense__form__second--set">
                  <label htmlFor="contact_email" className="control-label netsense__form__label ">Contact email</label>
                  <input disabled={isDisabled} className="form-control netsense__form__input" id="contact_email" ref="email" value={this.state.customer.contact_email} onChange={this.handleChange('contact_email')} />
                  <div className="form-error">
                  </div>
                </div>
              </div>
              <div className="col-md-12">
                <div className="col-md-6">
                  <div className="col-md-5 netsense__padding__left--none">
                    <label htmlFor="city" className="control-label netsense__form__label">City</label>
                    <input disabled={isDisabled} className="form-control netsense__form__input" id="city" ref="city" value={this.state.customer.city} onChange={this.handleChange('city')} />
                    <div className="form-error">
                    </div>
                  </div>
                  <div className="col-md-5">
                    <label htmlFor="state" className="control-label netsense__form__label">State</label>
                    <input disabled={isDisabled} className="form-control netsense__form__input" id="state" ref="state" value={this.state.customer.state} onChange={this.handleChange('state')} />
                    <div className="form-error">
                    </div>
                  </div>
                  <div className="col-md-1 customer-type" style={partnerTypeVisibility}>
                    <label htmlFor="type" className="control-label netsense__form__label">Partner</label>
                    <input type="checkbox" disabled={isDisabled} className="form-control netsense__form__input netsense__form__checkbox" id="type" ref="type" checked={this.state.customer.type} onChange={this.handleChange('type')} />
                    <div className="form-error">
                    </div>
                  </div>
                </div>
              </div>

              <div className="col-md-12">
                <div className="col-md-6">
                  <div className="col-md-5 netsense__padding__left--none">
                    <label htmlFor="postal_code"
                      className="control-label netsense__form__label">ZIP</label>
                    <input disabled={isDisabled} className="form-control netsense__form__input" id="postal_code" ref="postal_code" value={this.state.customer.postal_code} onChange={this.handleChange('postal_code')} />
                    <div className="form-error">
                    </div>
                  </div>
                  <div className="col-md-5">
                    <label htmlFor="country"
                      className="control-label netsense__form__label">Country</label>
                    <input disabled={isDisabled} className="form-control netsense__form__input" id="country" ref="country" value={this.state.customer.country} onChange={this.handleChange('country')} />
                    <div className="form-error">
                    </div>
                  </div>
                </div>
              </div>
              <input type="hidden" id="orgid" ref="orgid" value={this.state.customer.orgid} />

              <div className="col-md-12 netsense__form__buttons--set">
                <span style={this.props.displayArrows}>
                  <span style={this.props.noPreviousItem} className="rubix-icon icon-simple-line-icons-arrow-left customer-previous generic-previous"
                        onClick={() => this.callPreviousItem(this.props.allCustomers, this.state.customer)}></span>
                </span>
                <div className="col-md-3"><hr /></div>
                <div className="col-md-6" style={{marginLeft:"10px"}}>
                  <span style={visibility}>
                    <button id="suspend" type="button" className="ns-suspend-btn" style={suspendBtnStyle} onClick={this.handleSuspend}><b> Suspend</b></button>
                  </span>
                  <button style={visibility} id="validateAddress" type="button" className="ns-validate-btn" onClick={this.handleCheck}><b> Validate Address</b></button>
                  <button id="reset" type="button" className="ns-reset-btn" onClick={this.handleReset}><b>Reset</b></button>
                  <button id="saveCustomer" type="button" className="ns-save-btn" onClick={this.handleSubmit}><b> Save</b></button>
                </div>
                <div className="col-md-2"><hr /></div>
                <span style={this.props.displayArrows}>
                  <span style={this.props.noNextItem} className="rubix-icon icon-simple-line-icons-arrow-right customer-next generic-next"
                        onClick={() => this.callNextItem(this.props.allCustomers, this.state.customer)}></span>
                </span>
              </div>

            </form>
            <div style={{clear:"both"}}></div>
              {hierarchy}
            <div style={{clear:"both"}}></div>
            {/* IF there is a new customer then need to add, a condition only if there is a customer Id*/}
            <div>
              {sites}
            </div>
            <div style={{clear:"both"}}></div>
</div>
          </div>
        </div >
      </div >
    );
  }
});

module.exports = Customerform;