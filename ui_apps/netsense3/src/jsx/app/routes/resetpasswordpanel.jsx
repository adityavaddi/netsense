import classNames from 'classnames';
import { withRouter } from 'react-router';

var Body = React.createClass({

  getInitialState: function () {
    return {
      resetPassword: null,
    }
  },

  componentDidMount: function () {
    console.log(window.location.href);

    var apiParam = location.search.split('key=')[1];
    console.log("API key is" + apiParam);

  },


  resetPassword: function (e) {

    e.preventDefault();
    e.stopPropagation();

    var that = this;

    // Reset Password:

    var apiParam = location.search.split('key=')[1];
    console.log("API key is" + apiParam);

    var resetnewpwd = $("#resetnewpwd").val();
    var resetconfirmnewpwd = $("#resetconfirmnewpwd").val();
    console.log(resetnewpwd, resetconfirmnewpwd);
    if (resetnewpwd != resetconfirmnewpwd) {
      noty({ type: "error", text: 'Passwords do not match.  Please check and submit again' });
    }

    else {
      var passwordreset = {
        password: resetnewpwd,
      };
      console.log(JSON.stringify(passwordreset));
      $.ajax({
        url: NSN.apiURL + 'user/set-password',
        headers: { 'api_key': apiParam },
        data: JSON.stringify(passwordreset),
        method: 'POST',
        contentType: "application/json; charset=utf-8",
        dataType: 'json',
        success: function (data) {
          console.log("reset password success: " + JSON.stringify(data));
          $(".successAlert").css({ display: "block", position: "relative", bottom: "10px" });
          noty({
            type: "success",
            text: 'Password reset successfully.  Please log in with new password.',
            timeout: 3000,
          })
          that.props.router.push('/');

        },

        error: function (data) {
          console.log("failure in resetting password");
          $(".invalidAlert").css({ display: "block", position: "relative", bottom: "10px",width:"100%" });
          $(".reset-form-error").css({display: "block", position:"relative", top:"7px"});
        }
      });
    }

  },


  render: function () {
    return (
      <Container id='auth-container' className='login'>
        <Container id='auth-row'>
          <Container id='auth-cell'>
            <Grid>
              <Row>
                <Col sm={12}>
                  <PanelContainer noControls style={{ minHeight: "20px !important", height: "500px !important" }}>
                    <Panel>
                      <div className='text-center fg-white'>
                        <h2 style={{ float: "left", margin: 0, color: "#000000", textAlign: "left", fontWeight: "bold", padding: "25px 20px" }}> NetSense </h2>
                        <div style={{ display: "inline-block", float: "right", padding: "29px 20px" }}>
                          <img alt="Verizon logo" border="0" style={{ width: "70px" }} src="/imgs/vzlogo_lg.png"  />
                        </div>
                      </div>
                      <div style={{ clear: "both", borderTop: "1px solid #ddd" }}> </div>
                      <PanelBody style={{ padding: 0, minHeight: "20px !important" }}>
                        <div>
                          <div className="invalidAlert" style={{ display: 'none' }}>
                            <Alert danger collapseBottom>
                              Unable to reset password
                            </Alert>
                          </div>
                          <div style={{ clear: "both"}}> </div>
                          <div className="successAlert" style={{ display: 'none' }}>
                            <Alert success collapseBottom>
                              Password reset successfully
                            </Alert>
                          </div>
                          <div style={{ clear: "both"}}> </div>
                          <div style={{ padding: 25, paddingTop: 0, paddingBottom: 0, margin: 'auto', marginBottom: 0, marginTop: 25 }}>
                            <Form>
                              <FormGroup>
                                <InputGroup lg>
                                  <Input autoFocus type='password' id='resetnewpwd' style={{ bottom: "22px" }} className='border-focus-blue' placeholder='New password' />
                                </InputGroup>
                              </FormGroup>
                              <div style={{ clear: "both"}}> </div>
                              <FormGroup>
                                <InputGroup lg>
                                  <Input type='password' id='resetconfirmnewpwd' className='border-focus-blue' placeholder='Confirm new password' />
                                </InputGroup>
                                <div style={{ clear: "both"}}> </div>
                                <div className="reset-form-error" style={{ display: 'none' }}>
                                  New password of too low complexity
                                    </div>
                              </FormGroup>
                              <FormGroup id="empty-space">
                                <Grid>
                                  <Row>
                                    <Col collapseLeft collapseRight className='text-right'>
                                      <button type="button" className="ns-reset-btn" style={{ backgroundColor: "#000000 !important", color: "#ffffff !important", top: "20px", right: "10px" }} onClick={this.resetPassword}>Reset</button>
                                    </Col>
                                  </Row>
                                  <div className="col-sm-9" id="reqContent">
                                    <label><b>Password Policy</b></label>
                                    <ul id="details">
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
                                </Grid>
                              </FormGroup>
                            </Form>
                          </div>
                        </div>
                      </PanelBody>
                    </Panel>
                  </PanelContainer>
                </Col>
              </Row>
            </Grid>
          </Container>
        </Container>
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
        <BodyComponent />
      </Container>
    );
  }
}
