import classNames from 'classnames';
import helpers from 'global/utils/helpers';
import Suspendeduserslist from 'components/suspendedusers/suspendeduserslist';
import SuspendedusersDetail from 'components/suspendedusers/suspendedusersdetail';
import DataUtil from '../service/datautil';
import Header from 'common/headernew';


var Body = React.createClass({
  getInitialState: function () {
    return {
      users: null,
      suspendedusers: null,
      roles: null,
      customers: null,
      customerID: NSN.customerID,
      userID: "-1",
      showSuspendedUserDetail:false
    }
  },

  calcHeight: function (pct, extra) {
    var h = (window && window.document) ? (($(window).height() - 75) * (pct / 100) + extra) : 500;
    return h;
  },

  init: function () {
    var that = this;

    if (NSN.customerID == "-1") {
      $("#loadingmsg").html("Please select an Account first.")
      return;
    }

    DataUtil.getAll('users', that.processGetUsers);
    // $.ajax({
    //   url: NSN.apiURL + 'customers/' + NSN.customerID + '/users', 
    //   data : '',
    //   xhrFields: {
    //      withCredentials: true
    //   },
    //   method : 'GET',
    //   dataType : 'json',
    //   success : function(data){
    //     if (data.errors) {
    //       console.log("/users API returned error: " + JSON.stringify(data));
    //       $("#loadingmsg").html("Cannot retrieve user list. " + "/users API returned error: " + JSON.stringify(data));             
    //     } 

    //     else if (data == "") {
    //       that.setState({users:[]});
    //     } 

    //     else {
    //       console.log("ajax success: " + JSON.stringify(data));
    //       that.setState({
    //         users: data.map(function(user, index) {
    //           user.idx = index;                 
    //           user.name = user.name || "";
    //           user.title = user.title || "";
    //           user.email = user.email || "";
    //           user.roles = user.roles || "";
    //           user.sites = user.sites || "";
    //           user.created = new Date(user.created).toString() || "";
    //           user.updated = new Date(user.updated).toString() || "";
    //           user.phone = user.phone || "";
    //           return user;
    //         })
    //       })

    //     };
    //   },
    //   error : function(jqXHR, status, error){
    //     console.log("ajax failure (users): " + status + " - " + error);
    //     $("#loadingmsg").html("Cannot retrieve users.  API reported error: " + error);
    //   }
    // });

    //All Suspended users:

    DataUtil.getAll('suspendedusers', that.processGetSuspendedUser);
    // $.ajax({
    //   url: NSN.apiURL + 'customers/' + NSN.customerID + '/suspended-users',
    //   xhrFields: {
    //     withCredentials: true
    //   },
    //   data : '',
    //   method : 'GET',
    //   dataType : 'json',
    //   success : function(data){
    //     console.log("ajax success: " + JSON.stringify(data));
    //     that.setState({
    //         suspendedusers: data.map(function(user, index) {
    //           user.idx = index;                 
    //           user.name = user.name || "";
    //           user.title = user.title || "";
    //           user.email = user.email || "";
    //           user.roles = user.roles || "";
    //           user.sites = user.sites || "";
    //           user.phone = user.phone || "";
    //           user.created = new Date(user.created).toString() || "";
    //           user.updated = new Date(user.updated).toString() || "";
    //           return user;
    //         })
    //       })
    //   },
    //   error : function(jqXHR, status, error){
    //     console.log("ajax failure (suspendedusers): " + status + " - " + error);
    //     $("#loadingmsg").html("Cannot retrieve suspendedusers.  API reported error: " + error);
    //   }
    // });   

    // Call Customers API:

    DataUtil.getAll('customers', that.processGetCustomers);
    // $.ajax({
    //   url: NSN.apiURL + 'customers',
    //   data : '',
    //   xhrFields: {
    //      withCredentials: true
    //   },
    //   method : 'GET',
    //   dataType : 'json',
    //   success : function(data){
    //     if (data.errors) {
    //       console.log("/customers API returned error: " + JSON.stringify(data));
    //       $("#loadingmsg").html("Cannot retrieve customer list. " + "/customers API returned error: " + JSON.stringify(data));
    //     } else {
    //       console.log("ajax success: " + JSON.stringify(data));
    //       that.setState({
    //         customers: data.map(function(customer, index) {
    //           customer.idx = index;
    //           // handle any missing fields
    //           customer.street1 = customer.street1 || "";
    //           customer.street2 = customer.street2 || "";
    //           customer.city = customer.city || "";
    //           customer.state = customer.state || "";
    //           customer.postal_code = customer.postal_code || "";
    //           customer.country = customer.country || "";
    //           customer.contact = customer.contact || "";
    //           customer.contact_name = customer.contact_name || "";
    //           return customer;
    //         })
    //       })
    //     };
    //   },
    //   error : function(jqXHR, status, error){
    //     console.log("ajax failure (customers): " + status + " - " + error);
    //     $("#loadingmsg").html("Cannot retrieve Customers.  API reported error: " + error);
    //   }
    // });

    //User Roles:

    that.setState({
      roles: helpers.getUserRoles()
    });

  },
  //////////////callback function to get users////////////
  processGetUsers: function (data) {
    if (data.errors) {
      console.log("/users API returned error: " + JSON.stringify(data));
      $("#loadingmsg").html("Cannot retrieve user list. " + "/users API returned error: " + JSON.stringify(data));
    }
    else if (data == "") {
      this.setState({ users: [] });
    }
    else {
      console.log("ajax success: " + JSON.stringify(data));
      this.setState(DataUtil.assignState('users', data, this, this.makeUserObj));
    };
  },
  makeUserObj: function (user, index) {
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
  },
  /////////////callback to get suspended user//////////
  processGetSuspendedUser: function (data) {
    console.log("ajax success: " + JSON.stringify(data));
    this.setState(DataUtil.assignState('suspendedusers', data, this, this.makeSuspendedUserObj));
  },
  makeSuspendedUserObj: function (user, index) {
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
  },
  ///////////callback function to get customers//////////////
  processGetCustomers: function (data) {
    if (data.errors) {
      console.log("/customers API returned error: " + JSON.stringify(data));
      $("#loadingmsg").html("Cannot retrieve customer list. " + "/customers API returned error: " + JSON.stringify(data));
    } else {
      console.log("ajax success: " + JSON.stringify(data));
      this.setState(DataUtil.assignState('customers', data, this, this.makeCustomerObj));
    };
  },
  makeCustomerObj: function (customer, index) {
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
  },
  componentDidMount: function () {
    this.init();
    var that = this;

    ReactBootstrap.Dispatcher.on("Suspendeduserslist.select", function (userID) {
      NSN.userID = userID;
      sessionStorage.setItem("userID", NSN.userID);
      that.setState({ "userID": userID,
        showSuspendedUserDetail:true
      });
    });

    ReactBootstrap.Dispatcher.on("Suspendedusersform.reactivate", function (user_info) {
      var idx = helpers.get_idx(that.state.users, user_info, 'userid');
      console.log("Reactivating user (idx:" + idx + "; id:" + user_info.userid + ")");
      delete user_info.idx;
      var newState = {};
      console.log("Reactivating user: " + user_info.name);
      $.ajax({
        "url": NSN.apiURL + 'customers/' + NSN.customerID + '/suspended-users/' + user_info.userid,
        "type": "DELETE",
        "data": JSON.stringify(user_info),
        "xhrFields": {
          withCredentials: true
        },
        "dataType": "json",
        "contentType": "application/json",
        "processData": false,
        "success": function (data) {
          var newState = React.addons.update(this.state, { suspendedusers: { $splice: [[idx, 1]] }, users: { $push: [data] }, userID: { $set: "-1" } });
          //var newState = React.addons.update(this.state, { users: { [idx]: { $set: data } }});
          noty({ type: "success", text: 'User "' + user_info.name + '" reactivated.' })
          NSN.userID = "-1";
          sessionStorage.setItem("userID", NSN.userID);
          ReactBootstrap.Dispatcher.emit("Suspendedusersform.delete.success", user_info.userid);
          this.setState(
              {newState:newState,
            showSuspendedUserDetail: false});
        }.bind(that),
        "error": function () {
          console.log("ajax failure");
          noty({ type: "error", text: "Could not reactivate user." });
        }
      })
    });

  },


  componentWillUnmount: function () {
    ReactBootstrap.Dispatcher.removeAllListeners("Suspendeduserslist.select");
    ReactBootstrap.Dispatcher.removeAllListeners("Suspendedusersform.reactivate");
  },

  hideSuspendedUserDetail() {
    this.setState({
      showSuspendedUserDetail: false
    })
  },
  render() {
    var hstyle = { height: helpers.calcHeight(90, 0) + "px !important", 'overflowY': 'auto', 'overflowX': 'hidden' }
    if (this.state.roles && this.state.suspendedusers && this.state.customers) {
      var Subpanels = (

        <div className="netsense-center-panel">
          <Col  md={12} lg={12}>
            <PanelContainer>
              <Panel>
                <PanelBody>
                  <SuspendedusersDetail show={this.state.showSuspendedUserDetail} hide={this.hideSuspendedUserDetail}   suspendedusers={this.state.suspendedusers} customers={this.state.customers} roles={this.state.roles} userID={this.state.userID} />
                  <Suspendeduserslist suspendedusers={this.state.suspendedusers} customers={this.state.customers} roles={this.state.roles} userID={this.state.userID} />
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