import classNames from 'classnames';
import helpers from 'global/utils/helpers';
import auth from 'global/utils/auth';
import Usermanagementlist from 'components/usermanagement/usermanagementlist';
import UsermanagementDetail from 'components/usermanagement/usermanagementdetail';

import Header from 'common/headernew';


var Body = React.createClass({
  getInitialState: function(){
    return {
      users: null,
      suspendedusers:null,
      roles: null,
      customers: null,
      customerID: NSN.customerID,
      userID: "0",
      showUserManagementDetail: false,
    }
  },

  calcHeight: function(pct, extra) {
    var h = (window&&window.document)?(($(window).height() - 75) * (pct/100) + extra):500;
    return h;
  },

  init: function() {
    var that = this;

    if (NSN.customerID=="-1") {
      $("#loadingmsg").html("Please select an Account first.")
      return;
    }

    that.getAllUsers();
   
    //All Suspended users:
    if (auth.allowed('CAN_CHANGE_ORG_USER', 'UserModel')) {
      $.ajax({
        url: NSN.apiURL + 'customers/' + NSN.customerID + '/suspended-users',
        xhrFields: {
          withCredentials: true
        },
        data : '',
        method : 'GET',
        dataType : 'json',
        success : function(data){
          console.log("ajax success: " + JSON.stringify(data));
          that.setState({
              suspendedusers: data.map(function(user, index) {
                user.idx = index;
                user.name = user.name || "";
                user.title = user.title || "";
                user.email = user.email || "";
                user.roles = user.roles || "";
                user.sites = user.sites || "";
                user.phone = user.phone || "";
                user.created = new Date(user.created).toString() || "";
                user.updated = new Date(user.updated).toString() || "";
                return user;
              })
            })
        },
        error : function(jqXHR, status, error){
          console.log("ajax failure (suspendedusers): " + status + " - " + error);
          $("#loadingmsg").html("Cannot retrieve suspendedusers.  API reported error: " + error);
        }
      });
    } else {
      this.setState({suspendedusers: []});
    }

    // Call Customers API:

    $.ajax({
      url: NSN.apiURL + 'customers',
      data : '',
      xhrFields: {
         withCredentials: true
      },
      method : 'GET',
      dataType : 'json',
      success : function(data){
        if (data.errors) {
          console.log("/customers API returned error: " + JSON.stringify(data));
          $("#loadingmsg").html("Cannot retrieve customer list. " + "/customers API returned error: " + JSON.stringify(data));
        } else {
          console.log("ajax success: " + JSON.stringify(data));
          that.setState({
            customers: data.map(function(customer, index) {
              customer.idx = index;
              // handle any missing fields
              customer.street1 = customer.street1 || "";
              customer.street2 = customer.street2 || "";
              customer.city = customer.city || "";
              customer.state = customer.state || "";
              customer.postal_code = customer.postal_code || "";
              customer.country = customer.country || "";
              customer.contact = customer.contact || "";
              customer.contact_name = customer.contact_name || "";
              return customer;
            })
          })
        };
      },
      error : function(jqXHR, status, error){
        console.log("ajax failure (customers): " + status + " - " + error);
        $("#loadingmsg").html("Cannot retrieve Customers.  API reported error: " + error);
      }
    });

    //User Roles:

    that.setState({
      roles: helpers.getUserRoles()
    });

  },

  getAllUsers() {
    var that = this;

   $.ajax({
      url: NSN.apiURL + 'customers/' + NSN.customerID + '/users',
      data : '',
      xhrFields: {
         withCredentials: true
      },
      method : 'GET',
      dataType : 'json',
      success : function(data){
        if (data.errors) {
          console.log("/users API returned error: " + JSON.stringify(data));
          $("#loadingmsg").html("Cannot retrieve user list. " + "/users API returned error: " + JSON.stringify(data));
        } else if (data == "") {
          that.setState({users:[]});
        } else {
          console.log("ajax success: " + JSON.stringify(data));
          that.setState({
            users: data.map(function(user, index) {
              user.idx = index;
              user.name = user.name || "";
              user.title = user.title || "";
              user.email = user.email || "";
              user.roles = user.roles || "";
              user.sites = user.sites || "";
              user.created = new Date(user.created).toString() || "";
              user.updated = new Date(user.updated).toString() || "";
              user.phone = user.phone || "";
              return user;
            })
          })

        };
      },
      error : function(jqXHR, status, error){
        console.log("ajax failure (users): " + status + " - " + error);
        $("#loadingmsg").html("Cannot retrieve users.  API reported error: " + error);
      }
    });
   },
  componentDidMount: function() {
    this.init();
    var that = this;

    ReactBootstrap.Dispatcher.on("Usermanagementlist.select",function(userID, sortedFlag){
      // if( userID != NSN.userID){
        if(userID != NSN.userID  || (userID == NSN.userID && !sortedFlag)) {
        NSN.userID = userID;
        sessionStorage.setItem("userID", NSN.userID);
        that.setState({
          "userID":userID,
          showUserManagementDetail: true
        });
      }
    });

    ReactBootstrap.Dispatcher.on("Usermanagementlist.add", function(){
      that.setState({"userID":"0",
        showUserManagementDetail: true
      });
    });

    ReactBootstrap.Dispatcher.on("Usermanagementform.reset", function(){
      that.forceUpdate();
    });

    ReactBootstrap.Dispatcher.on("Usermanagementform.requestpwdreset", function(user_info){
      var emailinput = user_info.email;
      var passwordreset = {
        email: emailinput
      };

      $.ajax({
        url: NSN.apiURL + 'user/request-password-reset',
        xhrFields: {
          withCredentials: true
        },
        data : JSON.stringify(passwordreset),
        method : 'POST',
        contentType: "application/json; charset=utf-8",
        dataType : 'json',
        success : function(data){
          console.log("ajax success of reset pwd");
          noty({type:"success", text:'Email has been sent to reset your password.'})
        },
        error : function(data){
          noty({type:"error", text:'Could not send email to reset password.'});
        }
      });
    });

    ReactBootstrap.Dispatcher.on("Usermanagementform.generateApiKey", function(user_info){
      var emailinput = user_info.email;
      var generatekey = {
        email: emailinput
      };

      $.ajax({
        url: NSN.apiURL + 'user/generate-api-key',
        xhrFields: {
          withCredentials: true
        },
        data : JSON.stringify(generatekey),
        method : 'POST',
        contentType: "application/json; charset=utf-8",
        dataType : 'json',
        success : function(data){
          console.log("ajax success of generate-api-key: " + JSON.stringify(data));
          noty({
            type:"success",
            text:'API Key is:"' + data.api_key + '"',
            buttons: [
            {addClass: 'ns-save-btn', text: 'Ok',onClick: function($noty) {
                $noty.close();
              }
            }
            ]
          });

        },
        error : function(data){
          console.error("ajax failure"+ JSON.stringify(data));
          noty({type:"error", text:'Could not create API Key.'});

        }
      });
    });

    ReactBootstrap.Dispatcher.on("Usermanagementform.revokeApiKey", function(user_info){
      var emailinput = user_info.email;
      var apikeyinput = user_info.apikey;
      var revokekey = {
        email: emailinput,
        key: apikeyinput
      };

      $.ajax({
        url: NSN.apiURL + 'user/revoke-api-key',
        xhrFields: {
          withCredentials: true
        },
        data : JSON.stringify(revokekey),
        method : 'POST',
        contentType: "application/json; charset=utf-8",
        dataType : 'json',
        success : function(data){
          console.log("ajax success of revoke-api-key: " + JSON.stringify(data));
          $('#apiKeyWrapper').animate({'top':'-200px'},500,function(){
            $('#apiKeyOverlay').fadeOut('fast');
          });
          noty({type:"success", text:'API Key revoke is successful'});
        },
        error : function(data){
          console.error("ajax failure"+ JSON.stringify(data));
          $('#apiKeyWrapper').animate({'top':'-200px'},500,function(){
            $('#apiKeyOverlay').fadeOut('fast');
          });
          noty({type:"error", text:'Could not revoke API Key.'});
        }
      });

    });

    ReactBootstrap.Dispatcher.on("Usermanagementform.suspend",function(user_info) {
      var idx = helpers.get_idx(that.state.users, user_info, 'userid');
      console.log("Suspending user (idx:" + idx + "; id:" + user_info.userid + ")");
      delete user_info.idx;
      var newState = {};
      $.ajax({

        "url" : NSN.apiURL + 'customers/' + NSN.customerID + '/suspended-users/' + user_info.userid,
        "type" : "PUT",
        "xhrFields": {
           withCredentials: true
        },
        "data" : JSON.stringify(user_info),
        "dataType" : "json",
        "contentType" : "application/json",
        "processData" : false,
        "success" : function(data) {
          var newState = React.addons.update(this.state, { users: { $splice: [[idx, 1]] }, suspendedusers:{ $push : [data] },userID: { $set : "-1"}});
          //var newState = React.addons.update(this.state, { users: { [idx]: { $set: data } }});
          noty({type:"success", text:'User "' + user_info.name + '" suspended.'})
          NSN.userID = "-1";
          sessionStorage.setItem("userID", NSN.userID);
          ReactBootstrap.Dispatcher.emit("Usermanagementform.delete.success", user_info.userid);
          // this.setState(newState);
          this.setState({newState:newState,
            showUserManagementDetail:false});
        }.bind(that),
        "error" : function() {
          noty({type:"error", text:'Could not suspend user.'});
        }
      })
    });

    ReactBootstrap.Dispatcher.on("Usermanagementform.save",function(user_info) {
      var newState = {};
      if (user_info.userid == "") {
        console.log("Adding user: " + user_info.name);
        user_info.roles = $("select#roles option").filter(":selected").val();
        delete user_info.idx;
        delete user_info.userid;
        delete user_info.created;
        delete user_info.updated;
        // Check for no two user names to be same:

        var duplicate = false;
        for(var i = 0, len = that.state.users.length; i < len; i++) {
          console.log(JSON.stringify(that.state.users[i].email) + JSON.stringify(user_info.email));
          if( (that.state.users[ i ].email === user_info.email))
            duplicate = true;
        }
        if(duplicate){
          noty({type:"error", text:'User ' + user_info.name + ' with email "' + user_info.email + '" already exists.'});
        }
        else
        {
          $.ajax({
            "url" : NSN.apiURL + 'customers/' + NSN.customerID + '/users',
            "type" : "POST",
            "data" : JSON.stringify(user_info),
            "xhrFields": {
               withCredentials: true
            },
            "dataType" : "json",
            "contentType": "application/json",
            "processData": false,
            "success" : function(data) {
              console.log("Response from Add User: " + JSON.stringify(data));
              NSN.userID = data.userid;
              sessionStorage.setItem("userID", NSN.userID);
              data.idx = this.state.users.length;
              var newState = React.addons.update(this.state, { users: { $push : [data] },userID: { $set : data.userid }});
              this.getAllUsers();
              noty({type:"success", text:'User "' + data.name + '" added.'})
              ReactBootstrap.Dispatcher.emit("Usermanagementform.add.success", data);
              // this.setState(newState);
              this.setState({
                newState:newState,
                showUserManagementDetail: false
              });
            }.bind(that),
            "error" : function() {
              noty({type:"error", text:"Could not add user."});
            }
          });
        }
      }
      else {
        var idx = helpers.get_idx(that.state.users, user_info, 'userid');
        console.log("Updating user (idx:" + idx + "; id:" + user_info.userid + ")");
        delete user_info.idx;

        //  Check for no two user names to be same:
        var duplicate = false;
        for(var i = 0, len = that.state.users.length; i < len; i++) {
          if( (that.state.users[ i ].email === user_info.email) && (that.state.users[i].userid != user_info.userid))
            duplicate = true;
        }
        if(duplicate){
          noty({type:"error", text: '"' + user_info.email + '" email is already been used.'});
        }

        else{

          $.ajax({
            "url" : NSN.apiURL + 'customers/' + NSN.customerID + '/users/' + user_info.userid,
            "type" : "POST",
            "xhrFields": {
               withCredentials: true
            },
            "data" : JSON.stringify(user_info),
            "dataType" : "json",
            "contentType" : "application/json",
            "processData" : false,
            "success" : function(data) {
              var newState = React.addons.update(this.state, { users: { [idx]: { $set: data } }});
              noty({type:"success", text:'User "' + data.name + '" updated.'})
              ReactBootstrap.Dispatcher.emit('Usermanagementform.update.success', data);
              // this.setState(newState);

              this.setState({
                newState:newState,
                showUserManagementDetail: false
              });
            }.bind(that),
            "error" : function() {
              noty({type:"error", text:'Could not update user.'});
            }
          });
        }
      }
    })

  },

  hideUserManagementDetail() {
    this.setState({
      showUserManagementDetail: false
    })
  },

  componentWillUnmount: function() {
    ReactBootstrap.Dispatcher.removeAllListeners("Usermanagementlist.select");
    ReactBootstrap.Dispatcher.removeAllListeners("Usermanagementlist.add");
    ReactBootstrap.Dispatcher.removeAllListeners("Usermanagementform.suspend");
    ReactBootstrap.Dispatcher.removeAllListeners("Usermanagementform.save");
    ReactBootstrap.Dispatcher.removeAllListeners("Usermanagementform.reset");
    ReactBootstrap.Dispatcher.removeAllListeners("Usermanagementform.revokeApiKey");
    ReactBootstrap.Dispatcher.removeAllListeners("Usermanagementform.generateApiKey");
    ReactBootstrap.Dispatcher.removeAllListeners("Usermanagementform.requestpwdreset");
    ReactBootstrap.Dispatcher.removeAllListeners("Usermanagementform.requestpwdupdate");
  },

  render() {
    var hstyle = {height:helpers.calcHeight(90, 0)+"px !important",'overflowY':'auto','overflowX':'hidden'}
    if (this.state.users && this.state.roles && this.state.suspendedusers && this.state.customers) {
      var Subpanels = (

             <div className="netsense-center-panel">
              <Col md={12} lg={12}>
                <PanelContainer>
                  <Panel>
                    <PanelBody>

                      <UsermanagementDetail show={this.state.showUserManagementDetail} hide={this.hideUserManagementDetail}  users={this.state.users} suspendedusers={this.state.suspendedusers} customers={this.state.customers} roles={this.state.roles} userID={this.state.userID}/>
                      <Usermanagementlist show={this.state.showUserManagementDetail} users={this.state.users} suspendedusers={this.state.suspendedusers} customers={this.state.customers} roles={this.state.roles} userID={this.state.userID} />

                    </PanelBody>
                  </Panel>
                </PanelContainer>
              </Col>

            </div>

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
                    <h2 id="loadingmsg" style={{paddingTop:"16%",textAlign:"center"}}>Loading...</h2>
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

export default class extends React.Component {
  render() {
    var classes = classNames({
      'container-open': this.props.open
    });

    return (
      <Container id='container' className={classes}>
        <Header />
        <Body />
      </Container>
    );
  }
}
