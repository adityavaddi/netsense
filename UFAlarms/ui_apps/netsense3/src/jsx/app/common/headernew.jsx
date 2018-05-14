import { State, Navigation } from 'react-router';
import classNames from 'classnames';
import { Link, withRouter } from 'react-router';
import auth from 'global/utils/auth';
import SearchOverlay from './searchoverlay';
import helpers from 'global/utils/helpers';
import SiteSingleTile from '../components/sites/siteSingleTile';

var moment = require('moment');

var HeaderNavigation = React.createClass({
  mixins: [State, Navigation],

  handleUpdatePassword: function () {
    var that = this;
    // Update pwd:
    var oldpassword = $("#oldPassword").val();
    var newpassword = $("#newPassword").val();
    var confirmnewpassword = $("#confirmnewPassword").val();
    console.log(newpassword, confirmnewpassword);
    if (newpassword != confirmnewpassword) {
      noty({ type: "error", text: 'Passwords do not match.  Please check and submit again' });
    }

    else {
      var passwordupdate = {
        password: newpassword,
        old_password: oldpassword
      };
      console.log(JSON.stringify(passwordupdate));
      $.ajax({
        url: NSN.apiURL + 'user/update-password',
        xhrFields: {
          withCredentials: true
        },
        data: JSON.stringify(passwordupdate),
        method: 'POST',
        contentType: "application/json; charset=utf-8",
        dataType: 'json',
        success: function (data) {
          console.log("ajax success of update pwd");
          $('#updatePasswordWrapper').animate({ 'top': '-800px' }, 500, function () {
            $('#updatePasswordOverlay').fadeOut('fast');
          });
          //noty({type:"success", text:'Password updated successfully.'});
          noty({
            type: "success",
            text: 'Password updated successfully.  Please log back in.',
            timeout: false,
            buttons: [
              {
                addClass: 'btn btn-primary', text: 'Ok', onClick: function ($noty, e) {

                  $noty.close();
                  // Call for logout:

                  $.ajax({
                    url: NSN.apiURL + 'logout',
                    type: 'GET',
                    timeout: 1500,
                    async: true,
                    xhrFields: {
                      withCredentials: true
                    },
                    dataType: 'json',
                    contentType: 'application/json',
                    success: function (data) {
                      console.log("Successful logout. ");
                      that.props.router.push("/");
                    },
                    error: function () {
                      console.log("Failure logout. ");
                      that.props.router.push("/");
                    }
                  });
                  e.stopImmediatePropagation();
                  return false;
                }
              }
            ]
          });
        },
        error: function (data) {
          $('#updatePasswordWrapper').animate({ 'top': '-800px' }, 500, function () {
            $('#updatePasswordOverlay').fadeOut('fast');
          });
          noty({ type: "error", text: 'Could not update the password.' });
        }
      });
    }


  },

  componentDidMount: function () {
    // Add 'X-CSRF-Token' header to all AJAX call -- used on page reload
    $.ajaxSetup({
      beforeSend: function (xhr, settings) {
        if (settings && settings.type != "GET") {
          xhr.setRequestHeader("X-CSRF-Token", localStorage.getItem('netsense-csrf-token'));
        }
      }
    });
    
    $('#updatePasswordLink').click(function () {
      $("#oldPassword").val("");
      $('#updatePasswordOverlay').fadeIn('fast', function () {
        $('#updatePasswordWrapper').animate({ 'top': '160px' }, 500);
      });
    });
    $('#updatePasswordoverlayClose').click(function () {
      $('#updatePasswordWrapper').animate({ 'top': '-800px' }, 500, function () {
        $('#updatePasswordOverlay').fadeOut('fast');
      });
    });

    $('#commitLink').click(function () {

      $.ajax({
        url: '/gitbuild',
        method: 'GET',
        xhrFields: {
          withCredentials: true
        },
        success: function (data) {
          console.log("ajax success git build: " + data);
          if (typeof data.build != "undefined" && data.build != "") {
            var build = data.build;
            $("#build").text(build);
            $("#commitid").text("." + build.substr(build.lastIndexOf("_") + 1, 7))
          }
        },
        error: function (jqXHR, status, error) {
          console.log("ajax failure (gitbuild): " + status + " - " + error);
        }
      });

      $('#helpOverlay').fadeIn('fast', function () {
        $('#helpWrapper').animate({ 'top': '160px' }, 500);
      });
    });
    $('#helpoverlayClose').click(function () {
      $('#helpWrapper').animate({ 'top': '-800px' }, 500, function () {
        $('#helpOverlay').fadeOut('fast');
      });
    });

    // Logout :
    var that = this;
    var logoutHandler = function (e) {
      $.ajax({
        url: NSN.apiURL + 'logout',
        type: 'GET',
        timeout: 1500,
        async: true,
        xhrFields: {
          withCredentials: true
        },
        dataType: 'json',
        contentType: 'application/json',
        success: function (data) {
          console.log("Successful logout. ");
          sessionStorage.clear();
          // Remove CSRF token (set on login) from local storage
          localStorage.removeItem('netsense-csrf-token');
          localStorage.removeItem('netsense-api-url');
          that.props.router.push("/");
        },
        error: function () {
          console.log("Failure logout. ");
          that.props.router.push("/");
        }
      });
      e.stopImmediatePropagation();
      return false;
    }

    $('#logoutLink').one('click', logoutHandler);
  },

  render() {
    var props = {
      ...this.props,
      className: classNames('pull-right', this.props.className)
    };

    return (
      <div>
        <div className="updatePasswordOverlay" id="updatePasswordOverlay" style={{ display: "none" }}></div>
        <div className="updatePasswordWrapper" id="updatePasswordWrapper">
          <a className="updatePasswordoverlayClose" id="updatePasswordoverlayClose"></a>
          <h3>Please enter the old and new Password:</h3><br />
          <form className="form-horizontal">
            <div className="form-group">
              <label htmlFor="oldPassword" className="control-label col-sm-3">Old Password:</label>
              <div className="col-sm-6">
                <input type="password" className="form-control" id="oldPassword" ref="oldPassword" />
              </div>
            </div>
            <div className="form-group">
              <label htmlFor="newPassword" className="control-label col-sm-3">New Password:</label>
              <div className="col-sm-6">
                <input type="password" className="form-control" id="newPassword" ref="newPassword" />
              </div>
            </div>
            <div className="form-group">
              <label htmlFor="confirmnewPassword" className="control-label col-sm-3">Confirm New Password:</label>
              <div className="col-sm-6">
                <input type="password" className="form-control" id="confirmnewPassword" ref="confirmnewPassword" />
              </div>
            </div>
          </form>
          <button type="button" className="ns-save-btn" onClick={this.handleUpdatePassword}>
            <b>Submit</b></button>
          <label style={{ marginTop: "15px" }}>Password Policy</label>
          <ul>
            <li>Should be at least 8 characters</li>
            <li>Must contain at least one of each :
                        <ul>
                <li>Uppercase letter (A-Z)</li>
                <li>Lowercase letter (a-z)</li>
                <li>Digit (0-9)</li>
                <li>Special character: {" ~ ; : ! $ % ^ & * - _ = + [ ] { } / | \' \" ? , < . > "}</li>
              </ul>
            </li>
          </ul>
        </div>
        <div className="helpOverlay" id="helpOverlay" style={{ display: "none" }}></div>
        <div className="helpWrapper" id="helpWrapper">
          <a className="helpoverlayClose" id="helpoverlayClose"></a>
          <form className="form-horizontal">
            <div className="form-group">
              <label htmlFor="release" className="control-label col-sm-3" style={{marginTop:"0px"}}>Release Version:</label>
              <div className="col-sm-9">
                <span className="form-control commit_info">3.0.7<span id="commitid"></span></span>
              </div>
            </div>
            <div className="form-group">
              <label htmlFor="release" className="control-label col-sm-3" style={{marginTop:"0px"}}>API Host URL:</label>
              <div className="col-sm-9">
                <span className="form-control commit_info">{(typeof NSN.apiURL != "undefined")?NSN.apiURL:"unknown"}</span>
              </div>
            </div>
            <div className="form-group">
              <label htmlFor="build" className="control-label col-sm-3" style={{marginTop:"0px"}}>Commit ID:</label>
              <div className="col-sm-9">
                <span id="build" className="form-control commit_info">n/a (Local UI)</span>
              </div>
            </div>
          </form>
          {/*}
            <div className="form-group" style={{ margin: "0px" }}>
              <p className="control-label col-sm-3" style={{ color: "#000000", fontWeight: "bold" }}> Version :</p>
              <div className="col-sm-9" style={{ padding: "0px" }}>
                <p id="build"> </p>
              </div>
            </div>
{*/}
        </div>
      </div>
    );
  }
});

var HeaderNavigationComponent = withRouter(HeaderNavigation);

var Header = React.createClass({
  getInitialState: function () {
    return {
      "user": "",
      "customer": "",
      "siteName": (NSN && NSN.siteName) ? NSN.siteName : "Click to select",
      "siteID": (NSN && NSN.siteID) ? NSN.siteID : -1,
      "active": (NSN && NSN.activeTab) ? NSN.activeTab : "management",
      "hovered": null,
      "insubnav": false,
      "link": (NSN && NSN.link) ? NSN.link : "",
      "siteSelectState": "closed",
      "siteSelectCustomersLoaded": "false"
    }
  },

  stay: false,

  shouldComponentUpdate() {
    return true;
  },

  transTo: function (e) {
    e.stopPropagation();
    e.preventDefault();
    this.props.router.push(e.currentTarget.getAttribute("data-target"));
  },

  toggleSiteSearch: function () {
    this.setState({ siteSelectState: this.state.siteSelectState == "open" ? "closed" : "open" });
  },

  getUserInfo() {
    var userInfo = sessionStorage.getItem("userInfo");
    if (userInfo) {
      NSN.userInfo = JSON.parse(userInfo);
      NSN.customerID = sessionStorage.getItem("customerID") || NSN.customerID;
      NSN.customerName = sessionStorage.getItem("customerName") || NSN.customerName;
      NSN.siteID = sessionStorage.getItem("siteID") || NSN.siteID;
      NSN.siteName = sessionStorage.getItem("siteName") || NSN.siteName;
/*
      NSN.groupID = sessionStorage.getItem("groupID") || NSN.groupID;
      NSN.nodeID = sessionStorage.getItem("nodeID") || NSN.nodeID;
      NSN.fixtureID = sessionStorage.getItem("fixtureID") || NSN.fixtureID;
      NSN.alertID = sessionStorage.getItem("alertID") || NSN.alertID;
*/
      NSN.groupID = '-1';
      sessionStorage.setItem("groupID", "-1");
      NSN.nodeID = '-1';
      sessionStorage.setItem("nodeID", "-1");
      NSN.fixtureID = '-1';
      sessionStorage.setItem("fixtureID", "-1");
      NSN.alertID = '-1';
      sessionStorage.setItem("alertID", "-1");

      return true;
    } else {
      return false;
    }
  },

  updateNavigation: function (path) {
    if (path != "/" && path != this.state.link) {

      if((path != "/app/suspendeduserspanel")){
        var subnavLink = $(".subnavlist").find("li a[data-target='" + path + "']");
        var linkName = subnavLink[0].innerText;
        if (window && window.document) {
          window.document.title = "NetSense - " + linkName;
        };
        var menu = subnavLink.closest("div").attr("id");
        NSN.activeTab = menu;
        NSN.link = path;
        this.setState({ active: menu, link: path });
      }
     
    };
  },

  componentDidMount() {
    var that = this;
    this.stay = false;
    if (this.getUserInfo()) {
      this.setState({
        "user": NSN.userInfo.name || "",
        "customer": NSN.userInfo.user ? NSN.userInfo.user.orgNames[0] : ""
      });
    } else {
      if (window.location.pathname == "/reset-password-request?key") {
        this.props.router.push(window.location.href);
      } else {
        this.props.router.push('/app/login');
      }
    }
    ReactBootstrap.Dispatcher.on("Customer.select", function (customerName) {
      NSN.customerName = customerName;
      sessionStorage.setItem("customerName", NSN.customerName);
      NSN.siteName = "";
      sessionStorage.setItem("siteName", NSN.siteName);
      NSN.siteID = "-1";
      sessionStorage.setItem("siteID", NSN.siteID);
      helpers.clearSiteContext();
      that.setState({
        customer: customerName,
        siteName: "",
        siteID: "-1"
      });
    });

    ReactBootstrap.Dispatcher.on("SiteSingleTile.select", function (siteID, siteName) {
      if (siteID == "-1") {
        that.setState({ siteID: "-1" });
      } else {
        NSN.siteName = siteName;
        sessionStorage.setItem("siteName", NSN.siteName);
        NSN.siteID = siteID;
        sessionStorage.setItem("siteID", NSN.siteID);
        helpers.clearSiteContext();
        that.setState({ "siteName": siteName, "siteID": siteID });
      }
    });

    ReactBootstrap.Dispatcher.on("SearchSites.close", function () {
      that.toggleSiteSearch();
    });

    ReactBootstrap.Dispatcher.on("Router.changed", function (path) {
      // close SSE stream, if open
      if (typeof NSN.source != "undefined" && NSN.source !== null) {
        if (typeof NSN.source.close == 'function') {
          NSN.source.close();
        }
      }
      that.updateNavigation(path);
    });

    $("a.dropdown-toggle, #account").on("mouseenter", function (e) {
      e.stopPropagation();
      e.preventDefault();
      that.setState({ hovered: $(this).data("menu") });
    });

    $("a.dropdown-toggle, #account").on("mouseleave", function (e) {
      e.stopPropagation();
      e.preventDefault();
      if (!that.state.insubnav) {
        that.setState({ hovered: null });
      }
    });

    $("div.subnav").on("mouseenter", function (e) {
      e.stopPropagation();
      e.preventDefault();
      that.setState({ insubnav: true, hovered: $(this).attr("id") });
    });

    $("div.subnav").on("mouseleave", function (e) {
      e.stopPropagation();
      e.preventDefault();
      that.setState({ insubnav: false, hovered: null });
    });

  },

  componentWillUpdate() {
    // the following is needed in case the (entire) page is being refreshed
    this.getUserInfo();
  },

  componentWillUnmount() {
    ReactBootstrap.Dispatcher.removeAllListeners("SearchSites.close");
    ReactBootstrap.Dispatcher.removeAllListeners("Sitetiles.select");
    ReactBootstrap.Dispatcher.removeAllListeners("SiteSingleTile.select");
    ReactBootstrap.Dispatcher.removeAllListeners("Customer.select");
    ReactBootstrap.Dispatcher.removeAllListeners("Router.changed");
  },

  handleParking(e) {
    localStorage.setItem('netsense-api-url', NSN.apiURL);
    return true;
  },

  render() {
    var that = this;

    var managementClass = "subnav" + (this.state.hovered == 'management' ? ' hovered' : '');
    var lightingClass = "subnav" + (this.state.hovered == 'lighting' ? ' hovered' : '');
    var parkingClass = "subnav" + (this.state.hovered == 'parking' ? ' hovered' : '');
    var trafficClass = "subnav" + (this.state.hovered == 'traffic' ? ' hovered' : '');
    var accountClass = "subnav" + (this.state.hovered == 'account' ? ' hovered' : '');
    var showFixtures = auth.allowed('CAN_READ', 'FixtureModel');

    if (auth.allowed('CAN_READ', 'OrgModel')) {
      var SelectLabel = "Select Account";
    } else {
      SelectLabel = "Select Site";
    };

    var subnavlinks = {
      "management": [{ model: "OrgModel", path: "/app/customerpanel", label: "Accounts", icon: "/imgs/navigation/Accounts.svg" },
      { model: "SiteModel", path: "/app/sitepanel", label: "Sites", icon: "/imgs/navigation/Sites.svg" },
      { model: "NodeModel", path: "/app/nodepanel", label: "Nodes", icon: "/imgs/navigation/Nodes.svg" },
      { model: "GroupModel", path: "/app/grouppanel", label: "Groups", icon: "/imgs/navigation/Groups.svg" },
      { model: "AuditModel", path: "/app/auditpanel", label: "Audits", icon: "/imgs/navigation/Audits.svg" },
      // { model: "GroupModel", id: "/app/alarmpanel", label:"Alarms", icon: "/imgs/navigation/Users.svg" },
      { model: "n/a", path: "/app/reportinglookerpanel", label: "Reporting", icon: "/imgs/navigation/Reporting.svg" },
      { model: "NotificationModel", path: "/app/notificationpanel", label: "Notifications", icon: "/imgs/navigation/Notifications.svg" },
      { model: "UFAlarmModel", path: "/app/ufalarmpanel", label: "User-Friendly Alarms", icon: "/imgs/navigation/Notifications.svg" },
      { model: "NodeModel", id: "Commissioningid", path: "/app/commissioningpanel", label: "Commissioning", icon: "/imgs/navigation/Commissioning.svg" },
      { model: "ConfigModel", path: "/app/configpanel", label: "Configurations", icon: "/imgs/navigation/Configurations.svg" },
      { model: "FirmwareModel", path: "/app/firmwarepanel", label: "Firmware Versions", icon: "/imgs/navigation/Firmware.svg" },
      { model: "UserModel", id: "userManagementLink", path: "/app/usermanagementpanel", label: "Users", icon: "/imgs/navigation/Users.svg" },
      { model: "FirmwareModel", path: "/app/firmwareupdatepanel", label: "Firmware Jobs", icon: "/imgs/navigation/Firmware.svg" },
      ],
      "lighting": [{ model: "GroupModel", path: "/app/grouppanel", label: "Groups", icon: "/imgs/navigation/Groups.svg" },
      { model: "ScheduleModel", path: "/app/schedulepanel", label: "Schedules", icon: "/imgs/navigation/Schedules.svg" },
      { model: "ScheduleModel", path: "/app/proximitypanel", label: "Proximity Dimming", icon: "/imgs/navigation/ProximityDimming.svg" },
      { model: "n/a", path: "/app/energylookerpanel", label: "Energy", icon: "/imgs/navigation/Energy.svg" }
      ],
      "parking": [{ model: "ParkingZoneModel", path: "/app/parkingzonepanel", label: "Parking Zones", icon: "/imgs/navigation/ParkingZones.svg" },
      { model: "ParkingZoneModel", path: "/app/parkinggrouppanel", label: "Parking Groups", icon: "/imgs/navigation/Groups.svg" },
      { model: "ParkingZoneModel", path: "/app/parkingdashboardpanel", label: "Parking Dashboard", icon: "/imgs/navigation/Configurations.svg" },
      { model: "ParkingZoneModel", path: "/app/parkingspacepanel", label: "Space Management"}
      ],
      "traffic": [
          { model: "TrafficObjectModel", path: "/app/trafficconfigpanel", label: "Traffic Configurations", icon: "/imgs/navigation/TrafficConfigurations.svg" },
          { model: "TrafficObjectModel", path: "/app/intersectionsafetyanalytics", label: "Intersection Safety Analytics", icon: "/imgs/navigation/TrafficConfigurations.svg" }
      ]
    };

    // show DH link only for Sensity users
    if (helpers.isInternalUser()) {
      subnavlinks.lighting.push({model: "ScheduleModel", path: "/app/daylightpanel", label: "Daylight Harvesting", icon: "/imgs/navigation/DaylightHarvesting.svg"});
    }

     // show DH link only if the logged in user has permission to view it: 
    if (showFixtures == true) {
      subnavlinks.lighting.push({model: "FixtureModel", path: "/app/fixturepanel", label: "Fixtures", icon: "/imgs/navigation/Fixtures.svg"});
    }

    var maintablinks = ["Management", "Lighting", "Parking", "Traffic"].map(function (tab, index) {
      var lctab = tab.toLowerCase();
      var tabclass = "tabtitle" + (that.state.active == lctab ? " active" : "") + (that.state.hovered == lctab ? " hovered" : "");
      return (
        <li key={index} className="dropdown">
          <a href="#" className="dropdown-toggle"
            data-menu={lctab}><span
              className={tabclass}><b>{tab}</b></span></a>
        </li>
      )
    });

    return (
      <nav className="navbar navbar-default" style={{ color: "#333", fontWeight: "bold", marginBottom: "0px" }}>
        <div className="row">
          <div className="col-sm-6" style={{ padding: "0px" }}>
            <div id="navbar">
              <div>
                <h3 style={{ fontSize: "24px", position: "absolute", top: "16px", left: "35px", marginTop: "10px" }}> NetSense </h3>
              </div>
              <ul className="nav navbar-nav" style={{ marginLeft: "154px" }}>
                {maintablinks}
              </ul>
            </div>
          </div>
          <div className="col-sm-6 text-right" id="rightNav">
            <div style={{ display: "inline-block", marginTop: "20px" }}>
              <span style={{ fontSize: "16px" }}><b>Site &nbsp; </b></span>
              <input type="text" style={{ width: "300px" }}
                onClick={this.toggleSiteSearch} value={this.state.siteName} title="Select Site" id="searchbutton" />
              <SearchOverlay overlayType='search' siteSelectState={this.state.siteSelectState} siteSelectCustomersLoaded={this.state.siteSelectCustomersLoaded} />
            </div>
            <div className="accountNav" style={{ display: "inline-block" }}>
              <img alt="Account" id="account" data-menu="account" style={{ height: "36px", margin: "10px 20px 0px 20px" }} src="/imgs/my-account.svg" />
            </div>
            <div style={{ display: "inline-block" }}>
              <img alt="Verizon logo" border="0" style={{ width: "93px", margin: "-4px 20px 0px 0px" }} src="/imgs/vzlogo_lg.png"  />
            </div>
          </div>

          <div className={managementClass} id="management" style={{ left: "166px" }}>
            <ul className="subnavlist" >
              {subnavlinks.management.map(function (link, index) {
                if (link.id == "Commissioningid") {
                  return auth.allowed('CAN_ASSIGN_TO_SITES', link.model) ? (
                    <li key={index}><a onClick={that.transTo} className={that.state.link == link.path ? "active" : ""} id={link.id ? link.id : ""}
                      data-target={link.path}><b>{link.label}</b></a></li>
                  ) : "";
                } else {
                  return auth.allowed('CAN_READ*', link.model) ? (
                    <li key={index}><a onClick={that.transTo} className={that.state.link == link.path ? "active" : ""} id={link.id ? link.id : ""}
                      data-target={link.path}><b>{link.label}</b></a></li>
                  ) : "";
                }
              })}
            </ul>
          </div>

          <div className={lightingClass} id="lighting" style={{ left: "306px" }}>
            <ul className="subnavlist">
              {subnavlinks.lighting.map(function (link, index) {
                return auth.allowed('CAN_READ', link.model) ? (
                  <li style={link.path=="/app/daylightpanel"?{display:"none"}:{}} key={index}><a onClick={that.transTo} className={that.state.link == link.path ? "active" : ""}
                    data-target={link.path}><b>{link.label}</b></a></li>
                ) : "";
              })}
            </ul>
          </div>
          <div className={parkingClass} id="parking" style={{ left: "419px" }}>
                <ul className="subnavlist">
                  {subnavlinks.parking.map(function(link, index) {
                    return auth.allowed('CAN_READ', link.model) ? (
                      <li style={link.path=="/app/parkingdashboardpanel"?{display:"none"}:{}}key={index}><a onClick={that.transTo} className={that.state.link == link.path ? "active":""} 
                              data-target={link.path}><b>{link.label}</b></a></li>
                      ) : "";
                  })}
                  {auth.allowed('CAN_READ', 'ParkingZoneModel') ? (
                    <li><a href="/parking" target="_blank" title="Opens in new tab/window" onClick={this.handleParking}><b>Parking Optimization</b> <img height="18" src="/imgs/navigation/NewWindow.png" /></a></li>
                  ) : ""
                  }
                </ul>
          </div>

          <div className={trafficClass} id="traffic" style={{ left: "526px" }}>
            <ul className="subnavlist">
              {subnavlinks.traffic.map(function (link, index) {
                return auth.allowed('CAN_READ', link.model) ? (
                  <li style={link.path=="/app/intersectionsafetyanalytics"?{display:"none"}:{}} key={index}><a onClick={that.transTo} className={that.state.link == link.path ? "active" : ""}
                    data-target={link.path}><b>{link.label}</b></a></li>
                ) : "";
              })}
	            {auth.allowed('CAN_READ', 'TrafficObjectModel') ? (
		            <li><a href="https://traffic.sensity.com" target="_blank" title="Opens in new tab/window" ><b>Intersection Safety Analytics</b> <img height="18" src="/imgs/navigation/NewWindow.png" /></a></li>
	            ) : ""
	            }
            </ul>
          </div>

          <div className={accountClass} id="account" style={{ width: "200px", right: "60px" }}>
            <ul className="subnavlist">
              <li><a id="commitLink"><b>Commit ID</b></a></li>
              <li><a id="updatePasswordLink"><b>Update Password</b></a></li>
              <li><a id="logoutLink"><b>Logout</b></a></li>
            </ul>
          </div>

        </div>

        <HeaderNavigationComponent />
      </nav>
    );
  }
});

module.exports = withRouter(Header);

