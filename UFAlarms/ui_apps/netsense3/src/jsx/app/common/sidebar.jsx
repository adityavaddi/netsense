import {
  Sidebar, SidebarNav, SidebarNavItem,
  SidebarControls, SidebarControlBtn
} from 'global/jsx/sidebar_component';

import auth from 'global/utils/auth';
import { Link, withRouter } from 'react-router';

class ApplicationSidebar extends React.Component {
  render() {

    // Display User mgmt only if the logged in user has permission to view it:
    var UserMgmtSidebar;
    if (NSN && NSN.userInfo && NSN.userInfo.authorization) {
      if ((NSN.userInfo.authorization[0].type == "sensity_user") || (NSN.userInfo.authorization[0].type == "sensity_admin") || (NSN.userInfo.authorization[0].type == "sensity_read_only") || (NSN.userInfo.authorization[0].type == "end_user_admin") || (NSN.userInfo.authorization[0].type == "partner_admin")) {
        UserMgmtSidebar = (
          <SidebarNavItem title="User Management" glyph='icon-fontello-users-1' name='User Management' href='/app/usermanagementpanel' />
        );
      };
    };

    // Display Node mgmt only if the logged in user has permission to view it:
    var NodeMgmtSidebar;
    if (NSN && NSN.userInfo && NSN.userInfo.authorization) {
      if ((NSN.userInfo.authorization[0].type != "sensity_admin")) {
        NodeMgmtSidebar = (
          <SidebarNavItem glyph='icon-fontello-dot-circled' title="Nodes" name='Nodes' href='/app/nodepanel' />
        );
      };
    };

    // Display Firmware mgmt only if the logged in user has permission to view it:
    var FirmwareMgmtSidebar;
    if (NSN && NSN.userInfo && NSN.userInfo.authorization) {
      if ((NSN.userInfo.authorization[0].type == "sensity_user") || (NSN.userInfo.authorization[0].type == "sensity_read_only") || (NSN.userInfo.authorization[0].type == "partner_admin") || (NSN.userInfo.authorization[0].type == "partner_deployment_user") || (NSN.userInfo.authorization[0].type == "partner_read_only") || (NSN.userInfo.authorization[0].type == "end_user_admin") || (NSN.userInfo.authorization[0].type == "end_user_lighting_user") || (NSN.userInfo.authorization[0].type == "end_user_read_only") || (NSN.userInfo.authorization[0].type == "end_user_api")) {
        console.log(NSN.userInfo.authorization[0].type);
        FirmwareMgmtSidebar = (
          <SidebarNavItem glyph='icon-fontello-news' title="Firmware" name='Firmware' href='/app/firmwarepanel' />
        );
      };
    };

    // Display Notification only if the logged in user has permission to view it:
    var NotificationSidebar = auth.allowed('CAN_READ', 'NotificationModel')
      ? (<SidebarNavItem glyph='icon-fontello-bell-5' title="Notifications" name='Notifications' href='/app/notificationpanel' />)
      : "";

    var ScheduleMgmtSidebar = auth.allowed('CAN_READ', 'ScheduleModel')
      ? (<SidebarNavItem glyph='icon-fontello-clock' title="Schedules" name='Schedules' href='/app/schedulepanel' />)
      : "";
    var GroupMgmtSidebar = auth.allowed('CAN_READ', 'GroupModel')
      ? (<SidebarNavItem glyph='icon-feather-share' title="Groups" name='Groups' href='/app/grouppanel' />)
      : "";
    var TrafficconfigMgmtSidebar = auth.allowed('CAN_READ', 'TrafficObjectModel')
        ?(<SidebarNavItem glyph='icon-fontello-traffic-cone' title="Trafficconfig" name='Trafficconfig' href='/app/trafficconfigpanel' />)
        :"";
    var DHMgmtSidebar = auth.allowed('CAN_READ', 'ScheduleModel')
      ? (<SidebarNavItem glyph='icon-fontello-sun' title="Daylight Harvesting" name='Daylight Harvesting' href='/app/daylightpanel' />)
      : "";
    var PDMgmtSidebar = auth.allowed('CAN_READ', 'ScheduleModel')
      ? (<SidebarNavItem glyph='icon-fontello-move' title="Proximity Dimming" name='Proximity Dimming' href='/app/proximitypanel' />)
      : "";
    var FixtureMgmtSidebar = auth.allowed('CAN_READ', 'FixtureModel')
      ? (<SidebarNavItem glyph='icon-fontello-lightbulb-2' title="Fixtures" name='Fixtures' href='/app/fixturepanel' />)
      : "";
    var   ConfigSidebar = auth.allowed('CAN_READ', 'ConfigModel')
        ?(<SidebarNavItem glyph='icon-fontello-cog-alt' title="Config" name='Config' href='/app/configpanel' />)
        :"";
    var   ParkingGroupSidebar = true //(window && window.location && window.location.search.indexOf('enableParking') >= 0)
        ?(<SidebarNavItem glyph='icon-fontello-truck' title="Parking Groups" name='Parking Groups' href='/app/parkinggrouppanel' />)
        :"";
    var   ParkingZoneSidebar = true //(window && window.location && window.location.search.indexOf('enableParking') >= 0)
        ?(<SidebarNavItem glyph='icon-fontello-videocam' title="Parking Zones" name='Parking Zones' href='/app/parkingzonepanel' />)
        :"";

    return (
      <div>
        <Grid>
          <Row>
            <Col xs={12}>
              {/*<div className='sidebar-header'>SCREENS</div>*/}
              <div className='sidebar-nav-container'>
                <SidebarNav style={{ marginBottom: 0 }}>
                  {/*}          <SidebarNavItem glyph='icon-fontello-gauge' title="Dashboard" name='Dashboard' href='/app/dashboard' /> {*/}
                  <SidebarNavItem glyph='icon-fontello-users' title="Customers" name='Customers' href='/app/customerpanel' />
                  <SidebarNavItem glyph='icon-fontello-commerical-building' title="Sites" name='Sites' href='/app/sitepanel' />
                  {ScheduleMgmtSidebar}
                  {GroupMgmtSidebar}
                  {NodeMgmtSidebar}
                  <SidebarNavItem glyph='icon-fontello-list' title="Audits" name='Audits' href='/app/auditpanel' />
                  <SidebarNavItem glyph='icon-fontello-chart-line' title="Reporting" name='Reporting' href='/app/reportingpanel' />
                  <SidebarNavItem glyph='icon-fontello-dollar' title="Energy" name='Energy' href='/app/energypanel' />

                  {DHMgmtSidebar}
                  {PDMgmtSidebar}
                  {/*}    <SidebarNavItem glyph='icon-fontello-docs' name='Overlays' href='/app/overlaypanel' />  {*/}
                  {NotificationSidebar}
                  {FixtureMgmtSidebar}
                  {FirmwareMgmtSidebar}
                  <SidebarNavItem glyph='icon-fontello-plus-circle' title="Commissioning" name='Commissioning' href='/app/commissioningpanel' />
                  {ConfigSidebar}
                  {TrafficconfigMgmtSidebar}
                  {UserMgmtSidebar}
                  {ParkingGroupSidebar}
                  {ParkingZoneSidebar}

                  <hr style={{borderColor:"#3B4648",borderWidth:"2px",marginTop:"15px",marginBottom:"15px"}} />
          {/*}

                  <SidebarNavItem style={{marginLeft:"18px"}} name="Developer Info" />
                  <SidebarNavItem style={{marginLeft:"50px"}} glyph='icon-fontello-book-open' title="Framework" name='Framework' href='/app/docs/framework' />
                  <SidebarNavItem style={{marginLeft:"50px"}} glyph='icon-fontello-th-large-outline' title="Screens" name='Screens' href='/app/docs/screens' />
                  <SidebarNavItem style={{marginLeft:"50px"}} glyph='icon-fontello-squares' title="Components" name='Components' href='/app/docs/components' />
                  <SidebarNavItem style={{marginLeft:"50px"}} glyph='icon-fontello-book' title="API Reference" name='API Reference' href='/app/docs/apireference/apireference' />
          {*/}

                  <SidebarNavItem glyph='icon-fontello-key' title="User Permissions" name="User Permissions" href='/app/docs/permissions' />
                  {/*}<SidebarNavItem style={{marginLeft:"50px"}} glyph='icon-fontello-commerical-building' name="Load Malls" href='/app/utils/loadbrea' />  {*/}
                </SidebarNav>
              </div>
            </Col>
          </Row>
        </Grid>
      </div>
    );
  }
}

class SupportSidebar extends React.Component {
  render() {
    return (
      <div>
        <Grid>
          <Row>
            <Col xs={12}>
              <div>
                <h4 style={{ textAlign: "center", marginTop: "20px" }}>Interface to Slack-based Support Channel</h4>
                <div style={{ textAlign: "center", marginTop: "40px" }}>
                  <img src="/imgs/Slack.png" />
                </div>
              </div>
            </Col>
          </Row>
        </Grid>
      </div>
    )
  }
}

class AlertSidebar extends React.Component {
  render() {
    return (
      <div>
        <Grid>
          <Row>
            <Col xs={12}>
              <div>
                <h4 style={{ textAlign: "center", marginTop: "20px" }}>High-Priority Alerts will be displayed here.</h4>
              </div>
            </Col>
          </Row>
        </Grid>
      </div>
    )
  }
}

class DummySidebar extends React.Component {
  render() {
    return (
      <Grid>
        <Row>
          <Col xs={12}>
            <h3 style={{ textAlign: "center" }}>tbd</h3>
          </Col>
        </Row>
      </Grid>
    );
  }
}


export class Sidebar2 extends React.Component {
  constructor(props, context) {
    super(props, context);
    this.state = {
      "user": "",
      "role": "",
      "customer": ""
    }
    this.toggleSidebar = this.toggleSidebar.bind(this);
  }

  getUserInfo() {
    var userInfo = sessionStorage.getItem("userInfo");
    if (userInfo) {
      NSN.userInfo = JSON.parse(userInfo);
      NSN.customerID = sessionStorage.getItem("customerID") || NSN.customerID;
      NSN.siteID = sessionStorage.getItem("siteID") || NSN.siteID;
      NSN.siteName = sessionStorage.getItem("siteName") || NSN.siteName;
      NSN.groupID = sessionStorage.getItem("groupID") || NSN.groupID;
      NSN.nodeID = sessionStorage.getItem("nodeID") || NSN.nodeID;
      NSN.fixtureID = sessionStorage.getItem("fixtureID") || NSN.fixtureID;
      NSN.sidebarState = sessionStorage.getItem("sidebarState") || NSN.sidebarState;
      return true;
    } else {
      return false;
    }
  }

  componentDidMount() {
    let sidebarState = sessionStorage.getItem('sidebarState');
    if (sidebarState === 'open') {
      this.openSidebar();
    }

    var that = this
    if (that.getUserInfo()) {
      that.setState({
        "user": NSN.userInfo.name || "",
        "role": "",
        "customer": NSN.userInfo.user ? NSN.userInfo.user.orgNames[0] : ""
      });

    } else if (window.location.pathname == "/reset-password-request?key") {

      that.props.router.push(window.location.href);

    }
    else {
      that.props.router.push('/app/login');
    }

    ReactBootstrap.Dispatcher.on("Customer.select", function (customerName) {
      NSN.customerName = customerName
      that.setState({
        customer: customerName
      })
    });

    that.setState({
      customer: NSN.customerName
    })

  }

  toggleSidebar() {
    let sidebarState = sessionStorage.getItem('sidebarState');
    if (sidebarState === 'open') {
      this.closeSidebar();
    } else {
      this.openSidebar();
    }
  }

  openSidebar() {
    $("#step-direction")
      .removeClass("icon-fontello-step-forward")
      .addClass("icon-fontello-step-backward")
      .attr("title", "Hide menu");
    $("#body").css("marginLeft", "250px");
    $("#avatar").css("background-color", "#005B9A");
    NSN.sidebarState = 'open';
    sessionStorage.setItem('sidebarState', 'open');
  }

  closeSidebar() {
    $("#step-direction")
      .removeClass("icon-fontello-step-backward")
      .addClass("icon-fontello-step-forward")
      .attr("title", "Show menu");
    $("#avatar").css("background-color", "#008ABF");
    $("#body").css("marginLeft", "42px");
    NSN.sidebarState = 'closed';
    sessionStorage.setItem('sidebarState', 'closed');
  }

  render() {
    var avatar = (<div></div>)
    if (NSN && NSN.userInfo && NSN.userInfo.name.length > 0) {
      avatar = (
        <Row className='fg-white'>
          <Col xs={2} collapseRight style={{ padding: "12px 0px 17px 14px", backgroundColor: "#005B9A" }}>
            <Icon id="step-direction" glyph="icon-fontello-step-forward" style={{ cursor: "pointer", fontSize: "26px", opacity: "0.8" }} onClick={this.toggleSidebar} />
          </Col>
          <Col xs={10} collapseLeft id='avatar-col' style={{ paddingLeft: "18px", marginTop: "4px" }}>
            <div style={{ top: 14, fontSize: 16, lineHeight: 1, position: 'relative' }}>{this.state.user}</div>
            <div style={{ top: 18, fontSize: 12, lineHeight: 1, position: 'relative' }}>{this.state.role}</div>
            <div style={{ top: 20, fontSize: 12, lineHeight: 1, position: 'relative' }}>{this.state.customer}</div>
          </Col>
        </Row>
      )
    };
    return (
      <div id='sidebar' {...this.props}>
        <div id='avatar'>
          <Grid>
            {avatar}
          </Grid>
        </div>{/*}
        <SidebarControls>
          <SidebarControlBtn bundle='fontello' glyph='docs' sidebar={0} />
          <SidebarControlBtn bundle='fontello' glyph='chat-1' sidebar={1} />
          <SidebarControlBtn bundle='fontello' glyph='chart-pie-2' sidebar={2} />
          <SidebarControlBtn bundle='fontello' glyph='th-list-2' sidebar={3} />
          <SidebarControlBtn bundle='fontello' glyph='bell-5' sidebar={4} />
        </SidebarControls>
      {*/}
        <div id='sidebar-container'>
          <Sidebar sidebar={0} active>
            <ApplicationSidebar />
          </Sidebar>
          {/*}
          <Sidebar sidebar={1}>
            <SupportSidebar />
          </Sidebar>
          <Sidebar sidebar={2}>
            <DummySidebar />
          </Sidebar>
          <Sidebar sidebar={3}>
            <DummySidebar />
          </Sidebar>
          <Sidebar sidebar={4}>
            <AlertSidebar />
          </Sidebar>
        {*/}
        </div>
      </div>
    );
  }

  componentWillUnmount() {
    ReactBootstrap.Dispatcher.removeAllListeners("Customer.select");
  }

}

export default withRouter(Sidebar2);