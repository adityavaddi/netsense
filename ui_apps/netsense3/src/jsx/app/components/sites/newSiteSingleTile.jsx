import classNames from 'classnames';
import auth from 'global/utils/auth';
import helpers from 'global/utils/helpers';
import { State, Navigation } from 'react-router';
import { withRouter } from 'react-router';


var SiteSingleTile = React.createClass({
  getInitialState() {
    return {
      menuOpen: false,
      moreMenuOpen: false,
      inmenu: false
    };
  },

  propTypes: {
    site: React.PropTypes.object.isRequired
  },

  transTo: function (e) {
    e.stopPropagation();
    if (this.state.menuOpen) {
      helpers.clearSiteContext();
      this.setSiteContext();
      this.props.router.push(e.currentTarget.getAttribute("data-target"));
    }
  },

  handleSelect: function (e) {
    e.stopPropagation();

    helpers.clearSiteContext();
    this.setSiteContext();
    ReactBootstrap.Dispatcher.emit("SiteSingleTile.select", NSN.siteID, NSN.siteName)
  },

  setSiteContext: function () {
    NSN.siteID = this.props.site.siteid;
    sessionStorage.setItem("siteID", NSN.siteID);

    NSN.siteName = this.props.site.name;
    sessionStorage.setItem("siteName", NSN.siteName);

    var s = JSON.stringify(this.props.site);
    NSN.site = JSON.parse(s);
    sessionStorage.setItem("site", s);
  },

  handleEdit: function (e) {
    e.stopPropagation();

    this.setSiteContext();
    ReactBootstrap.Dispatcher.emit("Sitetiles.edit", NSN.siteID, NSN.siteName);
  },

  handleParking(e) {
    localStorage.setItem('netsense-api-url', NSN.apiURL);
    return true;
  },

  componentDidMount: function () {
    var that = this;
    new google.maps.Map(this.refs.map.getDOMNode(), {
      center: {
        lat: parseFloat(that.props.site.latitude),
        lng: parseFloat(that.props.site.longitude)
      },
      disableDefaultUI: true,
      zoom: 10,
      scrollwheel: false,
      draggable: false
    });

    $(this.refs.goto.getDOMNode()).on("mouseenter", function (e) {
      e.stopPropagation();
      e.preventDefault();
      that.setState({ menuOpen: true });
    });

    $(this.refs.goto.getDOMNode()).on("mouseleave", function (e) {
      e.stopPropagation();
      e.preventDefault();
      setTimeout(function () {
        if (!that.state.inmenu) {
          that.setState({ menuOpen: false, moreMenuOpen: false });
        }
      }, 500);
    });

    $(this.refs.menu.getDOMNode()).on("mouseenter", function (e) {
      e.stopPropagation();
      e.preventDefault();
      if (that.state.menuOpen) {
        that.setState({ inmenu: true });
      }
    });

    $(this.refs.menu.getDOMNode()).on("mouseleave", function (e) {
      e.stopPropagation();
      e.preventDefault();
      that.setState({ inmenu: false, menuOpen: false, moreMenuOpen: false });
    });

  },

  render() {

    var showNodes = auth.allowed('CAN_READ*', 'NodeModel');
    var showGroups = auth.allowed('CAN_READ*', 'GroupModel');
    var showSchedules = auth.allowed('CAN_READ*', 'ScheduleModel');
    var showParkingGroups = auth.allowed('CAN_READ*', 'ParkingGroupModel');
    var showParkingZones = auth.allowed('CAN_READ*', 'ParkingZoneModel');
    var showTraffic = auth.allowed('CAN_READ*', 'TrafficObjectModel');
    var showAudits = auth.allowed('CAN_READ*', 'AuditModel');
    var showAudits = auth.allowed('CAN_READ*', 'AuditModel');
    var showConfigs = auth.allowed('CAN_READ*', 'ConfigModel');
    var showAudits = auth.allowed('CAN_READ*', 'AuditModel');
    var showFirmwares = auth.allowed('CAN_READ*', 'FirmwareModel');
    var showFixtures = auth.allowed('CAN_READ*', 'FixtureModel');
    var showNotifications = auth.allowed('CAN_READ*', 'NotificationModel');
    var showUsers = auth.allowed('CAN_READ*', 'UserModel');
    var showCommissioning = auth.allowed('CAN_ASSIGN_TO_SITES', 'NodeModel');
    var showDaylightHarvesting = NSN.userInfo.authorization[0].type === "sensity_user";
    var liClass = "";

    return (
      <div className="netsense__site__single" >
        <div className="site-tile">
          <div className="netsense__site--description--name">
            <h2 onClick={this.handleSelect}>{this.props.site.name}</h2>
          </div>
          <div className="netsense__site--description">
            <span className="netsense__site--description-location">{this.props.site.city}, {this.props.site.state}</span>
            <div className="netsense__site--description--edit" >
              <span className="ns-edit-icon" onClick={this.handleEdit} ></span>
            </div>
          </div>
          <span className="netsense__site--description--span"></span>
          <div className="ns-navigate" >
            <h5 href="#" ref="goto" className="ns-span-position" style={{ marginTop: "7.5px" }}><span className="ns-arrow-icon"></span><b>Go to</b></h5>
          </div>
          <div className="site-tile-map" ref="map">
          </div>
          <div className="ns-sitesubnav" style={{ opacity: this.state.menuOpen ? 1 : 0 }} ref="menu">
            <ul className="ns-navigationclass">
              {showNodes &&
                <li onClick={this.transTo} data-target="/app/nodepanel"><b> Nodes </b></li>
              }
              {showGroups &&
                <li onClick={this.transTo} data-target="/app/grouppanel"><b> Groups </b></li>
              }
              {showSchedules &&
                <li onClick={this.transTo} data-target="/app/schedulepanel"><b> Schedules </b></li>
              }
              {showParkingGroups &&
                <li onClick={this.transTo} data-target="/app/parkinggrouppanel"><b> Parking Groups </b></li>
              }
              {showParkingZones &&
                <li onClick={this.transTo} data-target="/app/parkingzonepanel"><b> Parking Zones </b></li>
              }
              {showTraffic &&
                <li onClick={this.transTo} data-target="/app/trafficconfigpanel"><b> Traffic Configurations </b></li>
              }
              {showAudits &&
                <li onClick={this.transTo} data-target="/app/auditpanel"><b>Audits</b></li>
              }
              {showCommissioning &&
                <li onClick={this.transTo} data-target="/app/commissioningpanel"><b> Commissioning </b></li>
              }
              {showConfigs &&
                <li onClick={this.transTo} data-target="/app/configpanel"><b> Configurations </b></li>
              }
              {showDaylightHarvesting &&
                <li style={{display:"none"}} onClick={this.transTo} data-target="/app/daylightpanel"><b> Daylight Harvesting </b></li>
              }
              <li onClick={this.transTo} data-target="/app/energylookerpanel"><b> Energy </b></li>

              {showFirmwares &&
                <li onClick={this.transTo} data-target="/app/firmwarepanel"><b> Firmware Versions </b></li>
              }
              {showFixtures &&
                <li onClick={this.transTo} data-target="/app/fixturepanel"><b>Fixtures</b></li>
              }
              {showNotifications &&
                <li onClick={this.transTo} data-target="/app/notificationpanel"><b>Notifications</b></li>
              }
              {showSchedules &&
                <li onClick={this.transTo} data-target="/app/proximitypanel"><b> Proximity Dimming </b></li>
              }
              {showParkingZones &&
                <li style={{display:"none"}} onClick={this.transTo} data-target="/app/parkingdashboardpanel"><b> Parking Dashboard </b></li>
              }

              <li onClick={this.transTo} data-target="/app/reportinglookerpanel"><b>Reporting</b></li>

              {showParkingZones &&
                <li><a href="/parking" target="_blank" title="Opens in new tab/window" onClick={this.handleParking}><b>Parking Optimization</b></a></li>
              }
              {showUsers &&
                <li onClick={this.transTo} data-target="/app/usermanagementpanel"><b>Users</b></li>
              }
                <li onClick={this.transTo} data-target="/app/parkingspacepanel"><b> Space Management </b></li>
              {showFirmwares &&
                <li onClick={this.transTo} data-target="/app/firmwareupdatepanel"><b> Firmware Jobs </b></li>
              }  
            </ul>
          </div>
        </div>
      </div>
    )
  }
});
module.exports = withRouter(SiteSingleTile);