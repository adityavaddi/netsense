import classNames from 'classnames';
import { Link, withRouter } from 'react-router';

import helpers from 'global/utils/helpers';
import Customerlist from 'components/customers/customerlist';
import CustomerDetail from 'components/customers/customerdetail';
import DataUtil from '../service/datautil';

import Header from 'common/headernew';



var Body = React.createClass({
  getInitialState: function () {
    return {
      customers: null,
      customerID: NSN.customerID,
      showCustomerDetail: false,
      selectedCustomerSites: [],
      selectedCustomer: this.getCustomer(NSN.customerID)
    }
  },

  calcHeight: function (pct, extra) {
    var h = (window && window.document) ? (($(window).height() - 75) * (pct / 100) + extra) : 500;
    return h;
  },

  //////////////////////////////////////////////
  init: function () {

    DataUtil.getAll('customers', this.processCustomerObject);



  },
  getCustomer: function (customerID) {
    if (customerID == "0" || customerID == "-1") {
      return {
        name: "",
        type: "",
        street1: "",
        street2: "",
        city: "",
        state: "",
        country: "",
        postal_code: "",
        orgid: "",
        contact: "",
        contact_name: "",
        idx: -1
      };
    };
    var customers = this.state ? this.state.customers : []
    for (var i in customers) {
      if (customers[i].orgid == customerID) {
        return (customers[i]);
      }
    }
    return null;
  },

  ///////////////////// Callback Function/////////////////

  processCustomerObject: function (data) {
    this.partners = this._onlyPartners(data);
    var cdata= this.groupingPartners(this.partners,data);
    console.log("cdata: ",cdata);
    this.setState(DataUtil.assignState('customers', cdata, this, this.makeCustomerObj));

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


  groupingPartners: function(partnerList,customers){
    var tempData=customers.slice(0);
    var sortedCustomers=[];
    for (var i = 0; i < partnerList.length; i++) {

      sortedCustomers.push(partnerList[i]);
      this.filterEntity(tempData,partnerList[i]);
      for(var c=0;c<customers.length;c++){
         if (partnerList[i].orgid === customers[c].po) {
            sortedCustomers.push(customers[c]);
            this.filterEntity(tempData,customers[c]);
        }
      }

    }
    console.log("tempData",tempData);


    return sortedCustomers.concat(tempData);
  },
   filterEntity: function(arr) {
    var d, a = arguments, L = a.length, ax;
    while (L > 1 && arr.length) {
        d = a[--L];
        while ((ax= arr.indexOf(d)) !== -1) {
            arr.splice(ax, 1);
        }
    }
    return arr;
},

  makeCustomerObj: function (customer, index) {
    customer.idx = index;
    // handle any missing fields
    customer.street1 = customer.street1 || "";
    customer.street2 = customer.street2 || "";
    customer.city = customer.city || "";
    customer.type = customer.type || "";
    customer.state = customer.state || "";
    customer.postal_code = customer.postal_code || "";
    customer.country = customer.country || "";
    customer.contact = customer.contact || "";
    customer.contact_name = customer.contact_name || "";
    customer.partnername = customer.po ? this._findPartner(customer.po) : "";
    return customer;
  },


  _findPartner: function (po) {
    for (var i = 0; i < this.partners.length; i++) {
      if (po === this.partners[i].orgid) {
        return this.partners[i].name;
      }
    }
  },


  //////////////Callback funtion for Add Customer///////////////
  processAddCustomer: function (data) {
    NSN.customerID = data.orgid;
    sessionStorage.setItem("customerID", NSN.customerID);
    data.idx = this.state.customers.length;
    var newState = React.addons.update(this.state, { customers: { $push: [data] }, customerID: { $set: data.orgid } });
    noty({ type: "success", text: 'Customer "' + data.name + '" added.' });
    ReactBootstrap.Dispatcher.emit("Customerform.add.success", data);
    this.setState(newState);
  },

  ///////////////Callback function for Updating Customer//////////
  processUpdateCustomer: function (data, idx) {
    console.log("idx in update customer", idx)
    var newState = React.addons.update(this.state, { customers: { [idx]: { $set: data } } });
    noty({ type: "success", text: 'Customer "' + data.name + '" updated.' })
    ReactBootstrap.Dispatcher.emit('Customerform.update.success', data);
    this.setState(newState);
  },
  ////////////Callback function to Suspend Customer//////////
  processSuspendCustomer: function (entityData, idx, data) {
    var newState = React.addons.update(this.state, { customers: { $splice: [[idx, 1]] }, customerID: { $set: "-1" } });
    noty({ type: "success", text: 'Customer "' + entityData.name + '" has been suspended.' })
    NSN.customerID = "-1";
    sessionStorage.setItem("customerID", NSN.customerID);
    NSN.siteID = "-1";
    sessionStorage.setItem("siteID", NSN.siteID);
    helpers.clearSiteContext();
    ReactBootstrap.Dispatcher.emit("Customerform.delete.success", entityData.orgid);
    this.setState(newState);
  },


  ///////////////////////////////////////////////////
  sitesCallback: function (data) {
    this.setState({
      selectedCustomerSites: data
    })
  },
  checkDuplicate: function(customer_info){
        var duplicate = false;
        for (var i = 0, len = this.state.customers.length; i < len; i++) {
          if ((this.state.customers[i].name === customer_info.name) && !(customer_info.orgid === this.state.customers[i].orgid) )
            duplicate = true;
        }
        if (duplicate) {
          noty({ type: "error", text: 'Customer "' + customer_info.name + '" already exists.' });
        }
        return duplicate;
  },
  componentDidMount: function () {
    this.init();
    var that = this;

    function clearSite(customerID) {
      NSN.customerID = customerID;
      sessionStorage.setItem("customerID", NSN.customerID);
      NSN.siteID = "-1";
      sessionStorage.setItem("siteID", NSN.siteID);
      NSN.site = null;
      sessionStorage.removeItem("site");
      NSN.siteName = "";
      sessionStorage.setItem("siteName", "");
      helpers.clearSiteContext();
    };

    ReactBootstrap.Dispatcher.on("Customerlist.select", function (customerID, sortedFlag) {

      if(customerID != NSN.customerID  || (customerID == NSN.customerID && !sortedFlag)) {
        clearSite(customerID);
        if(NSN.customerID != null){
        DataUtil.getAll('sites', that.sitesCallback);
        }
        that.setState({
          customerID: "0",
          selectedCustomer: that.getCustomer("0"), //New Customer get a id of "0"
          //showCustomerDetail: true
        });

        //finding the selected customer from list of customers
        that.setState({
          selectedCustomer: that.getCustomer(customerID)
        }, () => { // setState is Async, Show modal only after customer is found
          that.setState({
            //showCustomerDetail: true
          });
        });
      }

    });

        ReactBootstrap.Dispatcher.on("Customerlist.editCustomer", function (customerID, sortedFlag) {
      if(customerID != NSN.customerID  || (customerID == NSN.customerID && !sortedFlag)) {
        clearSite(customerID);
        if(NSN.customerID != null){
        DataUtil.getAll('sites', that.sitesCallback);
        }
        that.setState({
          customerID: "0",
          selectedCustomer: that.getCustomer("0"), //New Customer get a id of "0"
          showCustomerDetail: true
        });

        //finding the selected customer from list of customers
        that.setState({
          selectedCustomer: that.getCustomer(customerID)
        }, () => { // setState is Async, Show modal only after customer is found
          that.setState({
            showCustomerDetail: true
          });
        });
      }
    });

    ReactBootstrap.Dispatcher.on("Customerlist.selectSites", function (customerID) {
      clearSite(customerID);
      ReactBootstrap.Dispatcher.emit("Sitetiles.select", NSN.siteID, NSN.siteName);
      that.props.router.push("/app/sitepanel")
    });

    ReactBootstrap.Dispatcher.on("Customerlist.add", function () {
      that.setState({
        customerID: "0",
        selectedCustomer: that.getCustomer("0"), //New Customer get a id of "0"
        showCustomerDetail: true
      });

    });

    ReactBootstrap.Dispatcher.on("Customerform.reset", function () {
      //   that.forceUpdate();
      that.setState({
        selectedCustomer: that.getCustomer("0") // resetting fields to blank on reset
      })
    });

    ReactBootstrap.Dispatcher.on("Customerform.suspend", function (customer_info) {
      var idx = helpers.get_idx(that.state.customers, customer_info, 'orgid');
      console.log("Deleting customer (idx:" + idx + "; id:" + customer_info.orgid + ")");
      delete customer_info.idx;
      DataUtil.suspendEntity('suspended-customers/', customer_info, that.processSuspendCustomer, idx);
      that.setState({
        showCustomerDetail: false
      });
    });

    ReactBootstrap.Dispatcher.on("Customerform.save", function (customer_info) {
      var newState = {};
      console.log(JSON.stringify(customer_info));
      // Check if it is a Partner type:
      if (customer_info.type == true || customer_info.type == "partner") {
        customer_info.type = "partner";
      }
      else {
        delete customer_info.type;
      }


       if (customer_info.orgid == "") {

          if(!that.checkDuplicate(customer_info)){
              console.log("Adding customer: " + customer_info.name);
              delete customer_info.idx;
              delete customer_info.orgid;


                DataUtil.addEntity('customers', customer_info, that.processAddCustomer, '');
                that.setState({
                  showCustomerDetail: false
                });

          }

      } else if(!that.checkDuplicate(customer_info)){
        var idx = helpers.get_idx(that.state.customers, customer_info, 'orgid');
        console.log("Updating customer (idx:" + idx + "; id:" + customer_info.orgid + ")");
        console.log("customer info #####", customer_info.idx);
        delete customer_info.idx;
        console.log("customer info #####", customer_info.idx);
        DataUtil.updateEntity('customers/', customer_info, that.processUpdateCustomer, idx);
              that.setState({
        showCustomerDetail: false
      });
        //   $.ajax({
        //     "url" : NSN.apiURL + 'customers/' + customer_info.orgid,
        //     "type" : "POST",
        //     "xhrFields": {
        //        withCredentials: true
        //     },
        //     "data" : JSON.stringify(customer_info),
        //     "dataType" : "json",
        //     "contentType" : "application/json",
        //     "processData" : false,
        //     "success" : function(data) {
        //       var newState = React.addons.update(this.state, { customers: { [idx]: { $set: data } }});
        //       noty({type:"success", text:'Customer "' + data.name + '" updated.'})
        //       ReactBootstrap.Dispatcher.emit('Customerform.update.success', data);
        //       this.setState(newState);
        //     }.bind(that),
        //     "error" : function() {
        //       noty({type:"error", text:'Could not update customer.'});
        //     }
        //   });
      }
    });

    function find_add_comp(key, address_components) {
      var found = _.find(address_components, function (comp) {
        var typeIndex = _.findIndex(comp.types, function (val) {
          return key === val;
        });

        if (typeIndex === -1) {
          return false;
        } else {
          return true;
        }
      });

      if (found) {
        return found.long_name;
      } else {
        return undefined;
      }
    }

    function populate_address(data) {
      var self = this,
        location = data.geometry.location,
        comp = data.address_components;

      var street_num = find_add_comp('street_number', comp),
        route = find_add_comp('route', comp),
        street1 = '';

      if (street_num) {
        if (route) {
          street1 = street_num + ' ' + route;
        } else {
          street1 = street_num;
        }
      } else {
        if (route) {
          street1 = route;
        }
      }
      self.state.customer.street1 = street1;
      self.state.customer.street2 = find_add_comp('subpremise', comp);
      self.state.customer.city = find_add_comp('locality', comp);
      self.state.customer.state = find_add_comp('administrative_area_level_1', comp);
      self.state.customer.postal_code = find_add_comp('postal_code', comp);
      self.state.customer.country = find_add_comp('country', comp);
      self.state.customer.latitude = location.lat();
      self.state.customer.longitude = location.lng();
      self.setState(self.state);
    }

    ReactBootstrap.Dispatcher.on('Customerform.checkAddress', function (self) {
      var geocoder = new google.maps.Geocoder(),
        address = _.join(_.values(_.pick(self.state.customer, ['street1', 'street2', 'city', 'state', 'postal_code', 'country'])), ' ');

      self.addresses.options = [];

      geocoder.geocode({
        address: address
      }, function (results) {
        if (results && results.length > 0) {
          var result = results[0];

          noty({
            type: "success",
            text: 'Found: ' + result.formatted_address
          });

          self.addresses.data = results;
          _.forEach(results, function (o, i) {
            self.addresses.options.push(
              <option key={i} value={i}>{o.formatted_address}</option>
            );
          });

          populate_address.apply(self, [result])
        } else {
          noty({
            type: "error",
            text: 'No match found for: ' + address
          });
          self.setState({});
        }
      });
    });

    ReactBootstrap.Dispatcher.on('Customerform.selectAddress', function (self, val) {
      var data = self.addresses.data[val];

      populate_address.apply(self, [data]);
    });

  },

  componentWillUnmount: function () {
    ReactBootstrap.Dispatcher.removeAllListeners("Customerlist.select");
    ReactBootstrap.Dispatcher.removeAllListeners("Customerlist.editCustomer");
    ReactBootstrap.Dispatcher.removeAllListeners("Customerlist.add");
    ReactBootstrap.Dispatcher.removeAllListeners("Customerform.save");
    ReactBootstrap.Dispatcher.removeAllListeners("Customerform.reset");
    ReactBootstrap.Dispatcher.removeAllListeners("Customerform.suspend");
    ReactBootstrap.Dispatcher.removeAllListeners("Customerform.checkAddress");
    ReactBootstrap.Dispatcher.removeAllListeners("Customerform.selectAddress");
  },

  componentWillReceiveProps: function (nextProps) {
    if (this.props.configID != nextProps.configID) {
      this.setState(this.getConfig(nextProps.configID));
    };
  },

  // render() {
  //   var hstyle = {height:helpers.calcHeight(90, 0)+"px !important"};
  //   if (this.state.customers) {
  //     var Subpanels = (
  //           <div>
  //             <Col sm={12} md={12} lg={5}>
  //               <PanelContainer>
  //                 <Panel>
  //                   <PanelBody style={hstyle}>
  //                      <Customerlist customers={this.state.customers} customerID={this.state.customerID} />
  //                   </PanelBody>
  //                 </Panel>
  //               </PanelContainer>
  //             </Col>
  //             <Col sm={12} md={12} lg={7} >
  //               <PanelContainer>
  //                 <Panel>
  //                   <PanelBody style={hstyle}>
  //                       <CustomerDetail customers={this.state.customers} customerID={this.state.customerID} />
  //                   </PanelBody>
  //                 </Panel>
  //               </PanelContainer>
  //             </Col>
  //           </div>
  //           );
  //     return (
  //       <Container id='body'>
  //         <Grid>
  //           <Row>
  //             <Col sm={12}>
  //               <Row>
  //                 {Subpanels}
  //               </Row>
  //             </Col>
  //           </Row>
  //         </Grid>
  //       </Container>
  //     );
  //   };
  //   return (
  //     <Container id='body'>
  //       <Grid>
  //         <Row>
  //           <Col sm={12}>
  //             <PanelContainer>
  //               <Panel>
  //                 <PanelBody style={hstyle}>
  //                   <h2 id="loadingmsg" style={{paddingTop:"16%",textAlign:"center"}}>Loading...</h2>
  //                 </PanelBody>
  //               </Panel>
  //             </PanelContainer>
  //           </Col>
  //         </Row>
  //       </Grid>
  //     </Container>
  //   );
  // }

  // New Customer panel
  hideCustomerDetail() {
    this.setState({
      showCustomerDetail: false
    })
  },
  render() {
    // console.log("state in render", this.state);
    var hstyle = { height: helpers.calcHeight(100, -190) + "px !important" };
    if (this.state.customers) {
      var Subpanels = (
        // <div className="netsense-center-panel">
          <Col sm={12} md={12} lg={12}>
            <PanelContainer>
              <Panel>
                <PanelBody style={hstyle}>
                  <CustomerDetail show={this.state.showCustomerDetail} hide={this.hideCustomerDetail} customer={this.state.selectedCustomer} sites={this.state.selectedCustomerSites} allCustomers={this.state.customers}/>
                  {/*<SearchOverlay overlayType='customer' customers={this.state.customers} customerID={this.state.customerID} />*/}
                  <Customerlist show={this.state.showCustomerDetail} customers={this.state.customers} customerID={this.state.customerID} selectedCustomer= {this.state.selectedCustomer}/>
                </PanelBody>
              </Panel>
            </PanelContainer>
          </Col>
        // </div>
      );
      return (
        <Container id='body'>
          <Grid>
            <Row>
              <Col sm={12}>
                <Row>
                  {Subpanels}
                </Row>
              </Col>
            </Row>
          </Grid>
        </Container>
      );
    };
    return (
      <Container id='body'>
        <Grid>
          <Row>
            <Col sm={12}>
              <PanelContainer>
                <Panel>
                  <PanelBody style={hstyle}>
                    <h2 id="loadingmsg" style={{ paddingTop: "16%", textAlign: "center" }}>Loading...</h2>
                  </PanelBody>
                </Panel>
              </PanelContainer>
            </Col>
          </Row>
        </Grid>
      </Container>
    );
  }
});
var BodyComponent = withRouter(Body);

export default class extends React.Component {
  render() {
    var classes = classNames({
      'container-open': this.props.open
    });

    return (
      <Container id='container' className={classes}>
        <Header />
        <BodyComponent />
      </Container>
    );
  }
}
