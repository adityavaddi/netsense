import classNames from 'classnames';
import { State, Navigation } from 'react-router';
import Notificationform from 'components/notifications/notificationform';
import helpers from 'global/utils/helpers';
import { Modal } from 'react-bootstrap';

var Notificationdetail = React.createClass({

  getInitialState: function () {
    return this.getNotification(this.props.notificationID, this.props.notifications);
  },

  propTypes: {
    notifications: React.PropTypes.array.isRequired,
    notificationID: React.PropTypes.string.isRequired,
    sites: React.PropTypes.array.isRequired,
    activeUsers: React.PropTypes.array.isRequired,
    submitStatus: React.PropTypes.bool,
    alarmTypeList: React.PropTypes.array.isRequired
  },

    componentDidMount: function() {
    var that = this;
    $('body').on('keydown', function (e) {
      if($('.modal-body').is(":visible") && e.which === 27){
        that.props.hide();
      }
    });
},

  getNotification: function (notificationID, notifications) {
    if (notificationID == "0" || notificationID == "-1") {
      return {
        name: "",
        description: "",
        notificationid: "",
        severity: [],
        notificationtype: [],
        nodeid: "",
        msg: "",
        emailUsersList: [],
        additionalEmails: [],
        smsUsersList: [],
        hold_off: "",
        resend_interval: "",
        //scope: "",
        active: "",
        idx: -1
      };
    }

    var activeUsersContainer = NSN.activeUsers;
    var notificationContainer = this.props.notifications;
    console.log("activeUsersContainer is" + JSON.stringify(activeUsersContainer));
      // Email User List to be changed to user name instead of user id:
      for (var i = 0; i < activeUsersContainer.length; i++) {
        for (var j = 0; j < notificationContainer.length; j++) {
          var emailUsersList_userName = notificationContainer[j].emailUsersList;
          if(typeof emailUsersList_userName != "undefined"){
            for (var k = 0; k < emailUsersList_userName.length; k++) {
              if (activeUsersContainer[i].userid == emailUsersList_userName[k]) {
                emailUsersList_userName[k] = activeUsersContainer[i].name;
              }
            }
          }
          
        }
      }

      // SMS User List to be changed to user name instead of user id:
      for (var i = 0; i < activeUsersContainer.length; i++) {
        for (var j = 0; j < notificationContainer.length; j++) {

          var smsUsersList_userName = notificationContainer[j].smsUsersList;
          if(typeof smsUsersList_userName != "undefined"){
            for (var k = 0; k < smsUsersList_userName.length; k++) {
              if (activeUsersContainer[i].userid == smsUsersList_userName[k]) {
                smsUsersList_userName[k] = activeUsersContainer[i].name;
              }

            }
          }
        }
      }

    var alarmtypeScope = helpers.getAlarmTypeList();
    for (var i = 0; i < alarmtypeScope.length; i++) {
      for (var j = 0; j < notificationContainer.length; j++) {

        var notificationtype_alarmid = notificationContainer[j].notificationtype;

        if (notificationtype_alarmid) {

          for (var k = 0; k < notificationtype_alarmid.length; k++) {
            if (alarmtypeScope[i].alarmvalue == notificationtype_alarmid[k]) {
              notificationtype_alarmid[k] = alarmtypeScope[i].alarmtype;
            }

          }
        }
      }
    }

    for (var i = 0; i < notifications.length; i++) {
      if (notifications[i].notificationid == notificationID) {
        return (notifications[i]);
      }
    }
    return null;
  },

  componentWillReceiveProps: function (nextProps) {
    console.log("notificationdetail.jsx componentWillReceiveProps ", nextProps);
    this.setState(this.getNotification(nextProps.notificationID, nextProps.notifications));
  },

  render: function () {
    // if (this.props.notificationID == "-1") {
    //   return (
    //     <h2 style={{ textAlign: "center", padding: "100px 0px" }}>Please select a Notification</h2>
    //   );
    // }

    // return (
    //   <Notificationform notification={this.state} notifications={this.props.notifications} sites={this.props.sites} activeUsers={this.props.activeUsers} />
    //  );

    if (this.props.show) {
      return (
        <div className="notificationForm" >
          <Modal.Dialog style={{ zIndex: 100, marginTop: 80 }}>
            <Modal.Body style={{ overflowY: "auto", height: '550px' }} >
            <a className="" id="notificationOverlayClose" onClick={() => { this.props.hide() }}>   
              <img src='/imgs/Close1.svg' width='30' height='30'  style={{marginTop:'60px',marginRight:'8px', zIndex: 100} }/>
            </a>
               <Notificationform notification={this.state} notifications={this.props.notifications} sites={this.props.sites} activeUsers={this.props.activeUsers} submitStatus={this.props.submitStatus} alarmTypeList={this.props.alarmTypeList}/>
            </Modal.Body>

          </Modal.Dialog>
        </div>
      )
    }
    return null;
  }

});

module.exports = Notificationdetail;