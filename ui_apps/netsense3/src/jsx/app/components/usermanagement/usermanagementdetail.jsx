import classNames from 'classnames';
import { State, Navigation } from 'react-router';
import Usermanagementform from 'components/usermanagement/usermanagementform';
import { Modal } from 'react-bootstrap';

var Usermanagementdetail = React.createClass({

  getInitialState: function() {
    return this.getUser(this.props.userID,this.props.users,this.props.roles);
  },

  propTypes: {
    users: React.PropTypes.array.isRequired,
    customers: React.PropTypes.array.isRequired,
    suspendedusers: React.PropTypes.array.isRequired,
    userID: React.PropTypes.string.isRequired,
    roles: React.PropTypes.string.isRequired,
  },

  componentDidMount: function() {
    var that = this;
    $('body').on('keydown', function (e) {
      if($('.modal-body').is(":visible") && e.which === 27){
        that.props.hide();
      }
    });
},

  getUser: function(userID, users, suspendedusers) {
    console.log("getuser" +userID)


    if(((NSN.userInfo.authorization[0].type == "sensity_admin") && (NSN.usertype == "end_customer") && (userID == "0")) || ((NSN.userInfo.authorization[0].type == "sensity_admin") && (NSN.usertype == "end_customer") && (userID == "-1"))){
      return {
          name: "",
          email: "",
          phone:"",
          title: "",
          roles: "",
          created: "",
          updated:"",
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
          created: "",
          updated:"",
          roles: this.props.roles,
          userid: "",
          idx: -1
      };
    };


    for (var i=0; i<users.length; i++) {
      if (users[i].userid == userID){
         users[i].created = new Date(users[i].created).toString() || "";
         users[i].updated = new Date(users[i].updated).toString() || "";
        return (users[i]);
      }
    }
    return null;
  },

  componentWillReceiveProps: function(nextProps){
    this.setState(this.getUser(nextProps.userID, nextProps.users, nextProps.suspendedusers));
  },

  render: function() {
      if (this.props.show) {
          return (
              <div className="userManagementForm" >
                  <Modal.Dialog style={{ zIndex: 100, marginTop: 80 }}>

                      <Modal.Body>
                      <a className=" " id="userManagementoverlayClose" onClick={() => { this.props.hide() }}>   
                        <img src='/imgs/Close1.svg' width='30' height='30'  style={{marginTop:'60px',marginRight:'8px', zIndex: 100} }/>
                      </a>
                          <Usermanagementform user={this.state} allUsers={this.props.users} roles={this.props.roles} customers={this.props.customers}/>
                      </Modal.Body>

                  </Modal.Dialog>
              </div>
          )
      }
      return null;
  }

});

module.exports = Usermanagementdetail;

