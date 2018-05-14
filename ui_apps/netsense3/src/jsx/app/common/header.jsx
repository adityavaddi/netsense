import { withRouter } from 'react-router';
import classNames from 'classnames';
import { Link } from 'react-router';

import { SidebarBtn } from 'global/jsx/sidebar_component';

import auth from 'global/utils/auth';

var moment = require('moment');

class Brand extends React.Component {
  render() {
    return (
      <NavHeader {...this.props}>
        <NavBrand tabIndex='-1'>
          <img src='/imgs/logo.png' alt='rubix' height='45' style={{ position: "relative", top: "-8px" }} />
        </NavBrand>
      </NavHeader>
    );
  }
}

class Skins extends React.Component {
  switchSkin(skin, e) {
    e.preventDefault();
    e.stopPropagation();
    for (var i = 0; i < Skins.skins.length; i++) {
      $('html').removeClass(Skins.skins[i]);
    }
    $('html').addClass(skin);
    vex.close(this.props.id);
  }
  render() {
    return (
      <Grid style={{ margin: '-2em' }}>
        <Row>
          <Col xs={12} className='text-center bg-darkgrayishblue75' style={{ marginBottom: 25 }}>
            <div className='fg-white' style={{ fontSize: 24, lineHeight: 1, padding: '25px 10px' }}>
              Choose a theme:
            </div>
          </Col>
        </Row>
        <Row>
          <Col xs={4} className='text-center'>
            <a href='#' style={{ border: 'none' }} onClick={this.switchSkin.bind(this, 'default')}>
              <Icon glyph='icon-fontello-stop icon-4x' style={{ color: '#E76049' }} />
            </a>
          </Col>
          <Col xs={4} className='text-center'>
            <a href='#' style={{ border: 'none' }} onClick={this.switchSkin.bind(this, 'green')}>
              <Icon glyph='icon-fontello-stop icon-4x' className='fg-darkgreen45' />
            </a>
          </Col>
          <Col xs={4} className='text-center'>
            <a href='#' style={{ border: 'none' }} onClick={this.switchSkin.bind(this, 'blue')}>
              <Icon glyph='icon-fontello-stop icon-4x' className='fg-blue' />
            </a>
          </Col>
        </Row>
        <Row>
          <Col xs={4} className='text-center'>
            <a href='#' style={{ border: 'none' }} onClick={this.switchSkin.bind(this, 'sensity1')}>
              <Icon glyph='icon-fontello-stop icon-4x' className='fg-brightblue' />
            </a>
          </Col>
          <Col xs={4} className='text-center'>
            <a href='#' style={{ border: 'none' }} onClick={this.switchSkin.bind(this, 'brown')}>
              <Icon glyph='icon-fontello-stop icon-4x' className='fg-brown' />
            </a>
          </Col>
          <Col xs={4} className='text-center'>
            <a href='#' style={{ border: 'none' }} onClick={this.switchSkin.bind(this, 'cyan')}>
              <Icon glyph='icon-fontello-stop icon-4x' className='fg-darkcyan' />
            </a>
          </Col>
        </Row>
      </Grid>
    );
  }
}

var TopNav = React.createClass({
  getInitialState: function () {
    return {
      "siteName": (window && window.document) ? NSN.siteName : "",
      "siteID": (window && window.document) ? NSN.siteID : "-1"
    }
  },

  transTo: function (e) {
    e.stopPropagation();
    if (this.state.siteID != "-1") {
      this.props.router.push(e.currentTarget.getAttribute("data-target"));
    }
  },

  componentDidMount: function () {
    var that = this;

    /* The Sitetiles.select Event is emmited with siteID = "-1" payload when commissioning Panel is loaded.
        when siteID == -1  sessionStorage and NSN are not updated thus
        maintaining the varialbe values when user moves away from commissioning panel */

    ReactBootstrap.Dispatcher.on("Sitetiles.select", function (siteID, siteName) {
      if (siteID == "-1") {
        that.setState({ siteID: "-1" });
      } else {
        NSN.siteName = siteName;
        sessionStorage.setItem("siteName", NSN.siteName);
        NSN.siteID = siteID;
        sessionStorage.setItem("siteID", NSN.siteID);
        //      $(".nav-localtime").html((typeof NSN.site != "undefined" && typeof NSN.site.time_zone != "undefined")
        //                                ?moment(new Date()).tz(NSN.site.time_zone).format('YYYY-MM-DD HH:mm')
        //                                :"");
        that.setState({ "siteName": siteName, "siteID": siteID });
      }
    });
    this.setState({ "siteName": (NSN.siteName || ""), "siteID": (NSN.siteID || "-1") });
  },

  render() {
    if (this.state.siteID == "-1") {
      return (
        <NavHeader {...this.props}>
          <NavBrand tabIndex='-1'>
            <img src='/imgs/logo.png' alt='rubix' height='45' style={{ position: "relative", top: "-8px" }} />
          </NavBrand>
        </NavHeader>
      )
    }

    var NodeVisibility = (NSN.userInfo.authorization[0].type != "sensity_admin") ? {} : { display: "none" };
    var GroupVisibility = auth.allowed("CAN_READ", "GroupModel") ? {} : { display: "none" };
    var ScheduleVisibility = auth.allowed("CAN_READ", "ScheduleModel") ? {} : { display: "none" };
    var NotificationVisibility = auth.allowed("CAN_READ", "NotificationModel") ? {} : { display: "none" };


    return (
      <div>
        <div className="nav-sitename" onClick={this.transTo} data-target="/app/sitepanel">{this.state.siteName}</div>
        <div className="nav-icon-group">
          <div className="nav-icon" onClick={this.transTo} data-target="/app/schedulepanel" style={ScheduleVisibility}>
            <Icon glyph="icon-fontello-clock" style={{ fontSize: "24px", position: "relative", top: "-2px" }} />
            <br />schedules
          </div>
          <div className="nav-icon" onClick={this.transTo} data-target="/app/grouppanel" style={GroupVisibility}>
            <Icon glyph="icon-feather-share" style={{ fontSize: "24px", position: "relative", top: "-2px" }} />
            <br />groups
          </div>
          <div className="nav-icon" onClick={this.transTo} data-target="/app/nodepanel" style={NodeVisibility}>
            <Icon glyph="icon-fontello-dot-circled" style={{ fontSize: "24px", position: "relative", top: "-2px" }} />
            <br />nodes
          </div>
          <div className="nav-icon" onClick={this.transTo} data-target="/app/notificationpanel" style={NotificationVisibility}>
            <Icon glyph="icon-fontello-bell-5" style={{ fontSize: "24px", position: "relative", top: "-2px" }} />
            <br />notifications
          </div>
        </div>
      </div>
    )
  }
})

Skins.skins = ['default', 'green', 'blue', 'sensity1', 'brown', 'cyan'];

var HeaderNavigation = React.createClass({
  handleSkinSwitch(e) {
    e.preventDefault();
    e.stopPropagation();
    var vexContent;
    vex.dialog.open({
      afterOpen: ($vexContent) => {
        vexContent = $vexContent;
        return React.render(<Skins id={$vexContent.data().vex.id} />, $vexContent.get(0));
      },
      afterClose: () => {
        React.unmountComponentAtNode(vexContent.get(0));
      }
    });
  },
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
          $('#updatePasswordWrapper').animate({ 'top': '-500px' }, 500, function () {
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
          $('#updatePasswordWrapper').animate({ 'top': '-500px' }, 500, function () {
            $('#updatePasswordOverlay').fadeOut('fast');
          }); 
          noty({type:"error", text:data.responseJSON.message});
        }
      });
    }


  },

  componentDidMount: function () {

    console.log(NSN.userInfo.login.last_seen);

    if ((NSN.userInfo.login.last_seen == null)) {
      $('#updatePasswordOverlay').fadeIn('fast', function () {
        $('#updatePasswordWrapper').animate({ 'top': '160px' }, 500);
      });
      $('#updatePasswordoverlayClose').click(function () {
        $('#updatePasswordWrapper').animate({ 'top': '-500px' }, 500, function () {
          $('#updatePasswordOverlay').fadeOut('fast');
        });
      });
    }

    $('#updatePasswordbutton').click(function () {
      $("#oldPassword").val("");
      $('#updatePasswordOverlay').fadeIn('fast', function () {
        $('#updatePasswordWrapper').animate({ 'top': '160px' }, 500);
      });
    });
    $('#updatePasswordoverlayClose').click(function () {
      $('#updatePasswordWrapper').animate({ 'top': '-500px' }, 500, function () {
        $('#updatePasswordOverlay').fadeOut('fast');
      });
    });


    // Help:

    $('#helpbutton').click(function () {
      var timestamp = config.version.date;
      var configversion = config.version.version;
      var split = timestamp.split(" ");

      var gitdate = split[0];
      var Date = moment(gitdate, 'YYYY-M-D');
      var splitdate = Date.format('MMMM D, YYYY');

      var gitcommittedtime = splitdate + " , " + split[1];
      $("#configversion").text(configversion);
      $("#gitcommittedtime").text(gitcommittedtime);

      $('#helpOverlay').fadeIn('fast', function () {
        $('#helpWrapper').animate({ 'top': '160px' }, 500);
      });
    });
    $('#helpoverlayClose').click(function () {
      $('#helpWrapper').animate({ 'top': '-500px' }, 500, function () {
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

    $('#logoutButton').one('click', logoutHandler);
  },

  render() {
    var props = {
      ...this.props,
      className: classNames('pull-right', this.props.className)
    };

    return (
      <NavContent {...props}>
        <Nav>
          <NavItem className="updatePasswordbutton" style={{ fontSize: "40px", cursor: "pointer" }} title="Change Password" id="updatePasswordbutton">
            <img src='/imgs/changepwd.png' height='35' style={{ cursor: "pointer", position: "relative", left: "-16px", top: "-1px" }} />
          </NavItem>
          <div className="updatePasswordOverlay" id="updatePasswordOverlay" style={{display:"none"}}></div>
          <div className="updatePasswordWrapper" id="updatePasswordWrapper">
            <a className="updatePasswordoverlayClose" id="updatePasswordoverlayClose"></a>
            <h3>Please enter the old and new Password:</h3><br/>
            <div className="form-group">
              <label htmlFor="oldPassword" className="control-label col-sm-3">Old Password:</label>
              <div className="col-sm-6">
                <input type="password" className="form-control" id="oldPassword" ref="oldPassword"/>
              </div>
            </div><br/>
            <div className="form-group">
              <label htmlFor="newPassword" className="control-label col-sm-3">New Password:</label>
              <div className="col-sm-6">
                <input type="password" className="form-control" id="newPassword" ref="newPassword"/>
              </div>
            </div><br/>
            <div className="form-group">
              <label htmlFor="confirmnewPassword" className="control-label col-sm-3">Confirm New Password:</label>
              <div className="col-sm-6">
                <input type="password" className="form-control" id="confirmnewPassword" ref="confirmnewPassword"/>
              </div>
            </div><br/>
            <button type="button" className="btn btn-success" onClick={this.handleUpdatePassword}>
            <Icon glyph="icon-fontello-ok" /> Submit</button>
            <label style={{marginTop:"15px"}}>Password Policy</label>
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

          <NavItem className="helpbutton" style={{fontSize:"40px",cursor:"pointer"}} title="Commit ID" id="helpbutton">
            <img src='/imgs/commit.png' height='35' style={{cursor:"pointer",left:"-24px",top:"-1px"}} />
          </NavItem>
          <NavItem>
            <Button id='logoutButton' className='logout' style={{ fontSize: "40px", top: "-4px", backgroundColor: "#008abf", border: "none", width: "66px" }} title="Log out">
              <Icon style={{ position: "relative", top: "-2px", left: "4px", fontSize: "34px" }} bundle='fontello' glyph='logout' />
            </Button>
          </NavItem>
          <NavItem>
            <div className="updatePasswordOverlay" id="updatePasswordOverlay" style={{ display: "none" }}></div>
          </NavItem>
          <NavItem>
            <div className="updatePasswordWrapper" id="updatePasswordWrapper">
              <a className="updatePasswordoverlayClose" id="updatePasswordoverlayClose"></a>
              <h3>Please enter the old and new Password:</h3><br />
              <div className="form-group">
                <label htmlFor="oldPassword" className="control-label col-sm-5">Old Password:</label>
                <div className="col-sm-7">
                  <input type="password" className="form-control" id="oldPassword" ref="oldPassword" />
                </div>
              </div><br />
              <div className="form-group">
                <label htmlFor="newPassword" className="control-label col-sm-5">New Password:</label>
                <div className="col-sm-7">
                  <input type="password" className="form-control" id="newPassword" ref="newPassword" />
                </div>
              </div><br />
              <div className="form-group">
                <label htmlFor="confirmnewPassword" className="control-label col-sm-5">Confirm New Password:</label>
                <div className="col-sm-7">
                  <input type="password" className="form-control" id="confirmnewPassword" ref="confirmnewPassword" />
                </div>
              </div><br />
              <button type="button" className="btn btn-success" onClick={this.handleUpdatePassword}>
                <Icon glyph="icon-fontello-ok" /> Submit</button>
            </div>
          </NavItem>
          <NavItem><div className="helpOverlay" id="helpOverlay" style={{ display: "none" }}></div></NavItem>
          <NavItem>
            <div className="helpWrapper" id="helpWrapper">
              <a className="helpoverlayClose" id="helpoverlayClose"></a>
              <div style={{ padding: "10px", marginBottom: "10px", backgroundColor: "#3688bc" }}>
                <span style={{ color: "#fff" }}> Server Information </span>
              </div>
              <div className="form-group" style={{ margin: "0px" }}>
                <p className="control-label col-sm-3" style={{ color: "#3688bc", fontWeight: "bold" }}> Version :</p>
                <div className="col-sm-9" style={{ padding: "0px", wordBreak: 'break-all' }}>
                  <p id="configversion"> </p>
                </div>
              </div><br />
              <div className="form-group" style={{ margin: "0px" }}>
                <p className="control-label col-sm-3" style={{ color: "#3688bc", fontWeight: "bold" }}> Date & Time :</p>
                <div className="col-sm-9" style={{ padding: "0px" }}>
                  <p id="gitcommittedtime"> </p>
                </div>
              </div><br />
            </div>
          </NavItem>
        </Nav>
      </NavContent>

    );
  }
});
var TopNavComponent = withRouter(TopNav);
var HeaderNavigationComponent = withRouter(HeaderNavigation);
export default class Header extends React.Component {
  shouldComponentUpdate() {
    return true;
  };

  render() {
    return (
      <Grid id='navbar' {...this.props}>
        <Row>
          <Col xs={12}>
            <NavBar fixedTop id='rubix-nav-header'>
              <Container fluid>
                <Row>
                  <Col xs={5} sm={8}>
                    <Container fluid>
                      <Row>
                        <Col xs={12}>
                          <TopNavComponent />
                        </Col>
                      </Row>
                    </Container>
                  </Col>
                  <Col xs={7} sm={4}>
                    <HeaderNavigationComponent pressed={this.props.pressed} />
                  </Col>
                </Row>
              </Container>
            </NavBar>
          </Col>
        </Row>
      </Grid>
    );
  }
}