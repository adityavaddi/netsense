import React from 'react';
import { Link, withRouter } from 'react-router';
import classNames from 'classnames';
import helpers from 'global/utils/helpers';

var Body = React.createClass({

  getInitialState: function(){
    return {
      login: null,
      customers: null,
      customerID: NSN.customerID,
      sites: null,
      siteID: NSN.siteID,
      datemin:NSN.datemin,
      datemax:NSN.datemax,
      loginerrors:false,
      emailerrors: false,
      enteredEmail: null
    }
  },


  slackit: function(data){
    $.ajax({
      url: "https://hooks.slack.com/services/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",
      type: "POST",
      data: JSON.stringify({channel:"#netsense3_0-support",
                               text:"Successful login for " + data.username}),
      success: function(res){
        console.log(JSON.stringify(res));

      },
      error: function(){

      }
    })

  },

  login: function(e,datemin,datemax) {
    e.preventDefault();
    e.stopPropagation();

    var uname = $('#emailaddress').val();
    var pwd = $("#password").val();
    var that = this;

    /* Set the from and to date of audits with current date and last 24hrs date */
    var timeZone = new Date().toString().substr(28,5);
    console.log(timeZone);
    var newdate = new Date().toISOString().substr(0, 19) + timeZone;
    var Yesterday = new Date(new Date().getTime() - (24 * 60 * 60 * 1000));
    var olddate = Yesterday.toISOString().substr(0,19) + timeZone;
    console.log(newdate);
    console.log(olddate);
    NSN.datemin = olddate;
    NSN.datemax = newdate;
    var payload = {
      email: uname,
      password: pwd
    }

    /* End Set the from and to date of audits with current date and last 24hrs date */

    $.ajax({
      url: NSN.apiURL +'login',
      xhrFields: {
         withCredentials: true
      },
      data :JSON.stringify(payload),
      type : 'POST',
      dataType:'json',
      contentType: 'application/json',
      success : function(data, status, xhr) {
        // Get CSRF token from header
        const csrfToken = xhr.getResponseHeader('netsense-csrf-token');
        localStorage.setItem('netsense-csrf-token', csrfToken);
        that.setCsrfTokenHeader(); // To set CSRF token to subsequent requests
        console.log("login success: " + JSON.stringify(data, null, 3));
        //console.log("last seen in login", data.login);
        //data.login.last_seen = null;
        if(data.login.last_seen == null || data.login.last_seen == undefined || data.login.last_seen == ""){
          $("#oldPassword").val("");
          $("#loggedinPassword").val(payload.password);
          $('#updatePasswordOverlay').fadeIn('fast', function () {
            $('#updatePasswordWrapper').animate({ 'top': '160px' }, 500);
          });
        } else {
          if (typeof data.name == "undefined") {
            alert("login API returned null values for name, id, user, etc.");
            NSN.link = "/app/customerpanel";
            if (window && window.document) {
              window.document.title = "NetSense - Accounts";
            };
            that.props.router.push('/app/customerpanel');
          } else {
            data.name = data.name.replace(/([a-z])([A-Z])/g, '$1 $2');
            NSN.userInfo = data;
            sessionStorage.clear();
            sessionStorage.setItem('userInfo',JSON.stringify(data))
            if (data.user.orgs.length == 1){
              NSN.customerID = data.user.orgs[0];
              sessionStorage.setItem('customerID', NSN.customerID);
              NSN.link = "/app/sitepanel";
              if (window && window.document) {
                window.document.title = "NetSense - Sites";
              };
              that.props.router.push('/app/sitepanel');
            } else {
              console.log("Length is more than 1");
              NSN.link = "/app/customerpanel";
              if (window && window.document) {
                window.document.title = "NetSense - Accounts";
              };
              that.props.router.push('/app/customerpanel');
            }
          };
        }
      },
      error : function(){
        console.log("failure");
        that.setState({loginerrors:true});
        $(".invalidAlert").css("display","block");
      }
    });
  },

    handleUpdatePassword: function () {
    var that = this;
    // Update pwd:
    var oldpassword = $("#oldPassword").val();
    var newpassword = $("#newPassword").val();
    var confirmnewpassword = $("#confirmnewPassword").val();
    var loggedinPassword = $("#loggedinPassword").val();
    console.log(newpassword, confirmnewpassword, loggedinPassword);
    if(loggedinPassword != oldpassword) {
      $(".invalidOldPwdAlert").css("display","block");
    }
    else if (newpassword != confirmnewpassword) {
      //noty({ type: "error", text: 'Passwords do not match.  Please check and submit again' });
      $(".invalidNewPwdAlert").css("display","block");
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
        success : function(data, status, xhr) {
          // Get CSRF token from header
          const csrfToken = xhr.getResponseHeader('netsense-csrf-token');
          localStorage.setItem('netsense-csrf-token', csrfToken);
          that.setCsrfTokenHeader(); // To set CSRF token to subsequent requests
          console.log("ajax success of update pwd");
          $('#updatePasswordWrapper').animate({ 'top': '-560px' }, 500, function () {
            $('#updatePasswordOverlay').fadeOut('fast');
          });
        },
        error: function (data) {
          $('#updatePasswordWrapper').animate({ 'top': '-560px' }, 500, function () {
            $('#updatePasswordOverlay').fadeOut('fast');
          });
          noty({ type: "error", text: 'Could not update the password.' });
        }
      });
    }
  },

  handleResetPassword: function () {
    var that = this;
    if (that.state.enteredEmail == "") {
      alert("please enter an valid email address");
    } else {
      var emailinput = that.state.enteredEmail;
    }
    var passwordreset = {
      email: emailinput
    };
    $.ajax({
      // url: 'https://netsense-rajitha.sensity.com/v3.0/forgot-password',
       url: NSN.apiURL + 'forgot-password',
      xhrFields: {
        withCredentials: true
      },
      data: JSON.stringify(passwordreset),
      method: 'POST',
      contentType: "application/json; charset=utf-8",
      dataType: 'json',
      success: function (data) {
        that.setState({ emailerrors: false });
        $(".invalidEmail").css("display", "none");
        $('#resetPasswordWrapper').animate({ 'top': '-560px' }, 500, function () {
          $('#resetPasswordOverlay').fadeOut('fast');
        });
      },
      error: function () {
        that.setState({ emailerrors: true });
        $(".invalidEmail").css("display", "block");
      }
    });
  },

    
  setCsrfTokenHeader: function () {
    // Add 'X-CSRF-Token' header to all AJAX call
    $.ajaxSetup({
      beforeSend: function (xhr, settings) {
        if (settings && settings.type != "GET") {
          xhr.setRequestHeader("X-CSRF-Token", localStorage.getItem('netsense-csrf-token'));
        }
      }
    });
  }, 

  componentDidMount: function() {
    var that = this;
    if (window && window.document) {
      window.document.title = "NetSense - Login";
    }

    $('#updatePasswordbutton').click(function () {
      $("#oldPassword").val("");
      $('#updatePasswordOverlay').fadeIn('fast', function () {
        $('#updatePasswordWrapper').animate({ 'top': '160px' }, 500);
      });
    });
    $('#updatePasswordoverlayClose').click(function () {
      $('#updatePasswordWrapper').animate({ 'top': '-560px' }, 500, function () {
        $('#updatePasswordOverlay').fadeOut('fast');
      });
    });
    $('#resetPasswordbutton').click(function () {
      $("#confirmEmail").val("");
      var enteredEmail = $('#emailaddress').val();
      that.setState({ enteredEmail: enteredEmail });
      $('#resetPasswordOverlay').fadeIn('fast', function () {
        $('#resetPasswordWrapper').animate({ 'top': '160px' }, 500);
      });
    });
    $('#resetPasswordoverlayClose').click(function () {
      $('#resetPasswordWrapper').animate({ 'top': '-560px' }, 500, function () {
        $('#resetPasswordOverlay').fadeOut('fast');
        $(".invalidEmail").css("display", "none");
      });
    });
     $(function () {
      $('#emailaddress').on('keypress', function (e) {
        if (e.which == 32)
          return false;
      });
    });

    function checkZoom() {
      if ($(window).width() >= 1600) {
        clearInterval(zoomTimer);
        $("#zoommsg").fadeOut(300);
        sessionStorage.removeItem('zoomTimer');
      } else {
        $("#zoommsgpx").text($(window).width());
      }
    }
//    $('body.default').css('backgroundColor',$('body.default').css('backgroundColor'));
    $('html').removeClass('default').addClass('authentication');
    if ($(window).width() < 1600) {
      $("#zoommsgpx").text($(window).width());
      $("#zoommsg").css("visibility", "hidden");
      var zoomTimer = setInterval(checkZoom, 200);
      sessionStorage.setItem('zoomTimer', zoomTimer);
    }
  },

    onChange(e) {
    this.setState({ enteredEmail: e.target.value });
  },


  componentWillUnmount: function() {
    $('html.authentication, html.default, html.authentication body, #auth-container').css('backgroundColor','#FFFFFF');
    $('html').addClass('default').removeClass('authentication');
    $('body').css('backgroundColor','#FFFFFF');
    if (!(sessionStorage.getItem('zoomTimer') === null)) {
      clearInterval(parseInt(sessionStorage.getItem('zoomTimer')));
      sessionStorage.removeItem('zoomTimer');
    };
  },

  render: function() {
    
    var loginformValidation = 'form-control';
    if(this.state.loginerrors) loginformValidation = 'form-control orange';

    return (
      <Container id='auth-container' className='login'>
      <div id="body">
        <div className="updatePasswordOverlay" id="updatePasswordOverlay" style={{ display: "none" }}></div>
              <Container id='auth-row'>
                <div className="resetPasswordOverlay" id="resetPasswordOverlay" style={{ display: "none" }}></div>
                <Container id='auth-cell'>
                  <div className="updatePasswordWrapper" id="updatePasswordWrapper">
                  <a className="updatePasswordoverlayClose" id="updatePasswordoverlayClose"></a>
                  <h3>Please enter the old and new Password:</h3><br />
                  <form className="form-horizontal">
                    <div className="form-group">
                      <div className="invalidOldPwdAlert" style={{display:'none'}}>
                       <Alert danger collapseBottom>
                          Old password is not valid.
                       </Alert>
                      </div>
                      <div className="invalidNewPwdAlert" style={{display:'none'}}>
                         <Alert danger collapseBottom>
                            New Password and Confirm Password does not match.
                         </Alert>
                      </div>
                      <label htmlFor="oldPassword" className="control-label col-sm-3">Old Password:</label>
                      <div className="col-sm-6">
                        <input type="password" className="form-control" id="oldPassword" ref="oldPassword" />
                        <input type="hidden" id="loggedinPassword" name="loggedinPassword" value= "" />
                      </div>
                    </div>
                    <div style={{clear:"both"}}></div>
                    <div className="form-group">
                      <label htmlFor="newPassword" className="control-label col-sm-3">New Password:</label>
                      <div className="col-sm-6">
                        <input type="password" className="form-control" id="newPassword" ref="newPassword" />
                      </div>
                    </div>
                    <div style={{clear:"both"}}></div>
                    <div className="form-group">
                      <label htmlFor="confirmnewPassword" className="control-label col-sm-3">Confirm New Password:</label>
                      <div className="col-sm-6">
                        <input type="password" className="form-control" id="confirmnewPassword" ref="confirmnewPassword" />
                      </div>
                    </div>
                    <div style={{clear:"both"}}></div>
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
              <div className="resetPasswordWrapper" id="resetPasswordWrapper">
                <a className="resetPasswordoverlayClose" id="resetPasswordoverlayClose">
                  <img src='/imgs/Close1.svg' width='30' height='30' style={{ marginTop: '60px', marginRight: '8px', zIndex: 100, width: "20px", height: "20px" }} />
                </a>
                <h3 style={{ fontSize:"22px !important"}}>Please enter your user name (email) below</h3><br />
                <div className="invalidEmail" style={{ display: 'none', position: "relative", float: "none !important", bottom: "18px" }}>
                  <div className="form-error" style={{ margin: "0px", padding: "0px" }}>
                    Please enter a Valid email address.
                                        </div>
                </div>
                <div className="form-group">
                  <label htmlFor="confirmEmail" className="control-label col-sm-3" style={{ width: "125px", position: "relative", right:"11px" }}>Confirm Email:</label>
                  <div className="col-sm-8" id="emailField">
                    <input type="email" value={this.state.enteredEmail} onChange={this.onChange} className="form-control" id="confirmEmail" ref="confirmEmail" />
                  </div>
                </div><br />
                <div className="info-message">
                  <span><b>* An email will be sent with a link to reset your password.</b></span>
                </div>
                <button type="button" className="ns-save-btn" style={{ marginTop: "44px", marginLeft:"60px !important" }} onClick={this.handleResetPassword}>
                  <b>Submit</b></button>
              </div>
                  <Grid>
                    <Row>
                      <Col sm={12}>
                        <PanelContainer noControls style={{minHeight:"20px !important"}}>
                          <Panel>
                            <PanelBody style={{padding: "0px !important", minHeight: "20px !important"}}>
                              <div className='text-center fg-white'>
                                <h2 style={{float:"left",margin: 0, color:"#000000",textAlign:"left",fontWeight:"bold",padding:"25px 20px"}}>  Log in to NetSense </h2>
                                <div style={{display:"inline-block",float:"right",padding:"29px 20px"}}>
                                  <img alt="Verizon logo" border="0" style={{width: "70px"}} src="/imgs/vzlogo_lg.png" />
                                </div>
                              </div>
                              <div style={{clear:"both",borderTop:"1px solid #ddd"}}> </div>
                              <div>
                                <div style={{padding: 25, paddingTop: 0, paddingBottom: 0, margin: 'auto', marginBottom: 25, marginTop: 25}}>
                                  <Form onSubmit={this.success} ref="form">
                                     <div className="invalidAlert" style={{display:'none', position:"relative", float:"none !important"}}>
                                        <div className="form-error" style={{margin:"0px",padding:"0px"}}>
                                          Incorrect email and/or password.
                                        </div>
                                      </div>
                                    <FormGroup>
                                      <p style={{fontFamily:"NeueHaasGroteskTextW01",fontSize:"16px"}}> Email address </p>
                                      <InputGroup lg>
                                        <input type='email' id='emailaddress' ref="emailaddress"  autoFocus={true} className={loginformValidation}  placeholder='Enter email address' />
                                      </InputGroup>
                                      <div style={{clear:"both",marginBottom:"10px"}}> </div>
                                    </FormGroup>
                                    <div style={{clear:"both",marginBottom:"10px"}}> </div>
                                    <FormGroup>
                                      <p style={{fontFamily:"NeueHaasGroteskTextW01",fontSize:"16px"}}> Password </p>
                                      <InputGroup lg>
                                        <input type='password' id='password' ref="password" className={loginformValidation} placeholder='Enter Password' />
                                      </InputGroup>
                                      <div style={{clear:"both",marginBottom:"10px"}}> </div>

                                    </FormGroup>
                                    
                                    <div style={{clear:"both",marginBottom:"30px"}}> </div>
                                    <FormGroup>
                                      <Grid>
                                        <Row>
                                          <Col collapseLeft collapseRight className='text-right'>
                                            <Button outlined lg type='submit' className="loginButton" onClick={this.login}>Log in</Button>
                                          </Col>
                                        </Row>
                                      </Grid>
                                    </FormGroup>
                                  </Form>
                                  <div className="resetPwd col-sm-4" id="resetPasswordbutton" style={{ position: "relative", bottom: "40px", cursor: "pointer" }}>
                                     <a>Forgot Password</a>
                                  </div>
                                </div>
                              </div>
                            </PanelBody>
                          </Panel>
                        </PanelContainer>
                        <p id="zoommsg" style={{visibility:"hidden",textAlign:"center"}}>
                        <span style={{color:"#008ABF",fontWeight:"bold"}}>NetSense 3.0</span> is designed to provide optimal results when the browser window is at
                        least <span style={{color:"#F00"}}>1600</span> pixels wide.<br />
                        Your browser's current window width is <span id="zoommsgpx" style={{color:"#F00"}}>???</span> pixels.<br />
                        Please zoom out (&quot;ctrl -&quot; or &quot;<Icon glyph='icon-fontello-looped-square-interest' /> -&quot;) until this message disappears.
                        </p>
                      </Col>
                    </Row>
                  </Grid>
                </Container>
              </Container>
      </div>
      </Container>
    );
  }
});
var BodyComponent = withRouter(Body)

export default class extends React.Component {
  render() {
    var classes = classNames({
      'container-open': this.props.open
    });
    return (
      <Container id='container' className={classes}>
        <BodyComponent />
      </Container>
    );
  }
}