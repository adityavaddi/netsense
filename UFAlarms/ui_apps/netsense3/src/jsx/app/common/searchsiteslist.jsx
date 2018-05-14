import { withRouter } from 'react-router';
import classNames from 'classnames';
import { Link } from 'react-router';
import helpers from 'global/utils/helpers';
import DataUtil from '../service/datautil';
import DataGrid from '../components/datagrid';
import auth from 'global/utils/auth';

var SearchSites = React.createClass({
  getInitialState: function () {
    return {
      customers: null,
      selectedCustomerSites: [],
      selectedCustomerID: null,
      isSitesLoaded: "false",
      totalCustomers: []    
    }
  },

  componentDidMount: function () {
    this.filterSearch();
  },

  init: function () {
    DataUtil.getAll('customers', this.processCustomerObject);
  },

  closeSiteSelect: function() {
    ReactBootstrap.Dispatcher.emit("SearchSites.close");
  },

  componentDidUpdate: function() {
    if (this.props.siteSelectState == "open") {
      if (this.props.siteSelectCustomersLoaded == "false") {
        this.props.siteSelectCustomersLoaded = "true";
        this.init();
      };
    };
  },

  makeCustomerObj(customer, index) {
    return customer;
  },
  filterSearch: function() {
    var that = this;
    var filteredCustomers = [];
    $( "#search" ).keyup(function() {
    filteredCustomers=that.state.totalCustomers.slice();
    var searchVal = $("#search"). val();
    var foundCustomers=[];
    filteredCustomers.map((eachCustomer) => {
          if(eachCustomer.name.toLowerCase().includes(searchVal.toLowerCase())){           
            foundCustomers.push(eachCustomer);
          }
      })
        that.setState({
        customers: foundCustomers
        })
    });
  },
  handleCustomerClick: function (selectedCustomerID) {
    var that = this
    NSN.customerID = selectedCustomerID
    this.setState({
      selectedCustomerID,
      selectedCustomerSites: [],
      isSitesLoaded: false
    })
    $.ajax({
      url: NSN.apiURL + 'customers/' + selectedCustomerID + '/sites',
      data: '',
      xhrFields: {
        withCredentials: true
      },
      method: 'GET',
      dataType: 'json',
      timeout: 3000
    }).done(function (data) {
      that.setState({
        selectedCustomerSites: data,
        isSitesLoaded: true
      })
    }).fail(function (jqXHR, status, error) {
      console.log("ajax failure (customers): " + status + " - " + error);
    }).always(function () {
    })
  },

  handleSiteClick: function (selectedSite) {
    var that = this
    // Actions to perform when user selects site under a customer.
    sessionStorage.setItem("customerID", NSN.customerID);
    sessionStorage.setItem("siteID", NSN.siteID);
    NSN.site = null;
    sessionStorage.removeItem("site");
    NSN.siteName = "";
    sessionStorage.setItem("siteName", "");

    NSN.siteID = selectedSite.siteid;
    ReactBootstrap.Dispatcher.emit("Groupform.update")
    sessionStorage.setItem("siteID", selectedSite.siteid);
    NSN.siteName = selectedSite.name
    sessionStorage.setItem("siteName", NSN.siteName);
    helpers.clearSiteContext();
    ReactBootstrap.Dispatcher.emit("SiteSingleTile.select", selectedSite.siteid, selectedSite.name)
    window.location.reload(true)
/*
var currentLocation = this._reactInternalInstance._context.router.location.pathname;
console.log(currentLocation);
React.unmountComponentAtNode(document.getElementById("app-container"));
var nav = React.createElement(Header);
var body = React.createElement(Body);
React.render(nav, document.getElementById("app-container"));
React.render(body, document.getElementById("app-container"));
*/


  },

  processCustomerObject(data) {
    this.setState(DataUtil.assignState('customers', data, this, this.makeCustomerObj));
    this.setState(DataUtil.assignState('totalCustomers', data, this, this.makeCustomerObj));
  },

  componentWillReceiveProps(newprops) {
  },

  render() {
    var wrapperStyle = (this.props.siteSelectState == "open")
      ?{height:"calc(100% - 120px)",overflow:"hidden",top:"147px"}
      :{height:"calc(100% - 120px)",overflow:"hidden",top:"-2000px"};
    return (
      <div style={{textAlign:"left"}}>
        <div>
          <div className="searchSiteList" id="searchSiteList" style={{ display: "none" }}></div>
        </div>
        <div>
          <div className="searchWrapper" id="searchWrapper" style={wrapperStyle} >
            <div style={{ position:"absolute",width:"100%",height:"45px",padding: "2px"}}>
              <span className="rubix-icon icon-feather-search" id="search-site"></span>
              <input type="search" className="form-search" id="search" ref="search" placeholder="Search Customer/Site" />
              <div style={{position:"absolute",right:"6px",top:"-6px",fontSize:"24px",cursor:"pointer"}} onClick={this.closeSiteSelect} >x</div>
            </div>
            <div className="form-group" style={{marginTop:"45px",overflowX:"hidden", overflowY:"auto",height:"calc(100% - 45px)"}}>
              {this.state.customers && this.state.customers.map((eachCustomer) => {
                return (
                  <ul key={eachCustomer.orgid} className="netsense__search__sites--section" onClick={() => this.handleCustomerClick(eachCustomer.orgid)}>
                    {eachCustomer.name}
                    <ul className="netsense__search__sites">
                      {this.state.selectedCustomerSites.map((eachSite) => {
                        if (this.state.selectedCustomerID != eachCustomer.orgid) return null
                      })}
                      {
                        this.state.isSitesLoaded && !this.state.selectedCustomerSites.length ?
                          this.state.selectedCustomerID != eachCustomer.orgid ? null : <li>No Sites defined </li> :
                          this.state.selectedCustomerSites.map((eachSite) => {
                            if (this.state.selectedCustomerID != eachCustomer.orgid) return null
                            if (!this.state.isSitesLoaded) return <li>Loading sites. . </li>
                            return <li key={eachSite.siteid} className="netsense__search__sites--item" onClick={() => this.handleSiteClick(eachSite)}>{eachSite.name}</li>
                          })
                      }
                    </ul>
                  </ul>
                )
              })}
            </div>
            <br />
          </div>
        </div>
      </div>
    );
  }
});

module.exports = withRouter(SearchSites);