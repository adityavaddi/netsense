import classNames from 'classnames';
import { State, Navigation } from 'react-router';
import Suspendedusersform from 'components/suspendedusers/suspendedusersform';
import { Modal } from 'react-bootstrap';

var Suspendedusersdetail = React.createClass({

  getInitialState: function() {
    return this.getUser(this.props.userID,this.props.suspendedusers,this.props.roles);
  },

  propTypes: {
    customers: React.PropTypes.array.isRequired,
    suspendedusers: React.PropTypes.array.isRequired,
    userID: React.PropTypes.string.isRequired,
    roles: React.PropTypes.string.isRequired,
  },

  getUser: function(userID, suspendedusers) {
    console.log("getuser" +userID)


    if(((NSN.userInfo.authorization[0].type == "sensity_admin") && (NSN.usertype == "end_customer") && (userID == "0")) || ((NSN.userInfo.authorization[0].type == "sensity_admin") && (NSN.usertype == "end_customer") && (userID == "-1"))){
      return {
          name: "",
          email: "",
          phone:"",
          title: "",
          roles: "",
          userid: "",
          idx: -1
      };
    }

    if (userID == "0" || userID == "-1") {
      return {
          name: "",
          email: "",
          phone:"",
          title: "",
          roles: this.props.roles,
          userid: "",
          idx: -1
      };
    };

   
    for (var i=0; i<suspendedusers.length; i++) {
      if (suspendedusers[i].userid == userID){
        return (suspendedusers[i]);
      }
    }
    return null;
  },

  componentWillReceiveProps: function(nextProps){
    this.setState(this.getUser(nextProps.userID, nextProps.suspendedusers));
  },

  render: function() {
      if (this.props.show) {
          return (
              <div className="userManagementForm" >
                  <Modal.Dialog style={{ zIndex: 100, marginTop: 80 }}>

                      <Modal.Body>
                      <a className=" " id="customeroverlayClose" onClick={() => { this.props.hide() }}>   
                        <img src='/imgs/Close1.svg' width='30' height='30'  style={{marginTop:'60px',marginRight:'8px', zIndex: 100} }/>
                      </a>
                          <Suspendedusersform user={this.state} roles={this.props.roles} customers={this.props.customers} suspendedusers={this.props.suspendedusers}/>
                      </Modal.Body>
                  </Modal.Dialog>
              </div>
          )
      }
      return null;
  }

});

module.exports = Suspendedusersdetail;

