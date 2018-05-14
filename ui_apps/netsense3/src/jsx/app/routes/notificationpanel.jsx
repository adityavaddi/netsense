import classNames from 'classnames';
import Notificationlist from 'components/notifications/notificationlist';
import NotificationDetail from 'components/notifications/notificationdetail';
import DataUtil from '../service/datautil';
import Alertlist from 'components/alerts/alertlist';
import helpers from 'global/utils/helpers';
import Header from 'common/headernew';
import Footer from 'common/footer';
import auth from 'global/utils/auth';

var Body = React.createClass({
  getInitialState: function () {
    return {
      notifications: null,
      alerts: null,
      notificationID: NSN.notificationID,
      alertID: NSN.alertID,
      activeUsers: null,
      customers: null,
      customerID: NSN.customerID,
      sites: null,
      siteID: "-1",
      showNotificationDetail: false,
      submitStatus: false,
      activeNotifications: null,
      inactiveNotifications: null,
      selectedNotificationID: null,
      selectedNotification: [],
      showDetailFlag: true,
      alarmTypeList: []
    }
  },

  calcHeight: function (pct, extra) {
    var h = (window && window.document) ? (($(window).height() - 75) * (pct / 100) + extra) : 500;
    return h;
  },

  init: function () {

    if (NSN.customerID == "-1" && NSN.siteID == "-1") {
      $("#loadingmsg").html("Please select an Account and a Site first.")
      return;
    } else {
      if (NSN.customerID == "-1") {
        $("#loadingmsg").html("Please select an Account first.")
        return;
      } else {
        if (NSN.siteID == "-1") {
          $("#loadingmsg").html("Please select a Site first.")
          return;
        }
      }
    };


    var that = this;
    this.hstyle = { height: helpers.calcHeight(90, 0) + "px !important" };

    $.ajax({
      url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/notifications',
      xhrFields: {
        withCredentials: true
      },
      data: '',
      method: 'GET',
      dataType: 'json',
      success: function (data) {
        if (data.errors) {
          console.log("/customers/" + NSN.customerID + "/notifications API returned error: " + JSON.stringify(data));
          $("#loadingmsg").html("Cannot retrieve notification list.  API returned: " + JSON.stringify(data));
        } else {

          // APi call for fetching list of users:

          $.ajax({
            url: NSN.apiURL + 'customers/' + NSN.customerID + '/users',
            data: '',
            xhrFields: {
              withCredentials: true
            },
            method: 'GET',
            dataType: 'json',
            success: function (data) {
              if (data.errors) {
                console.log("/users API returned error: " + JSON.stringify(data));
                $("#loadingmsg").html("Cannot retrieve user list. " + "/users API returned error: " + JSON.stringify(data));
              } else {
                console.log("ajax success: " + JSON.stringify(data));
                NSN.activeUsers = data;
                that.setState({
                  activeUsers: data.map(function (activeuser, index) {
                    activeuser.idx = index;
                    return activeuser;
                  })
                })
              };
            },
            error: function (jqXHR, status, error) {
              console.log("ajax failure (users): " + status + " - " + error);
              $("#loadingmsg").html("Cannot retrieve users.  API reported error: " + error);
            }
          });

          var activeUsersWrapper = NSN.activeUsers;
          console.log("activeUsersWrapper is" + JSON.stringify(activeUsersWrapper));

          if (activeUsersWrapper) {

            // Email User List to be changed to user name instead of user id:
            for (var i = 0; i < activeUsersWrapper.length; i++) {
              for (var j = 0; j < data.length; j++) {
                var emailUsersList_userName = data[j].emailUsersList;
                for (var k = 0; k < emailUsersList_userName.length; k++) {
                  if (activeUsersWrapper[i].userid == emailUsersList_userName[k]) {
                    emailUsersList_userName[k] = activeUsersWrapper[i].name;
                  }

                }
              }
            }

            // SMS User List to be changed to user name instead of user id:
            for (var i = 0; i < activeUsersWrapper.length; i++) {
              for (var j = 0; j < data.length; j++) {

                var smsUsersList_userName = data[j].smsUsersList;
                for (var k = 0; k < smsUsersList_userName.length; k++) {
                  if (activeUsersWrapper[i].userid == smsUsersList_userName[k]) {
                    smsUsersList_userName[k] = activeUsersWrapper[i].name;
                  }

                }
              }
            }
          }
          
          var alarmtypeScope = helpers.getAlarmTypeList();
          for (var i = 0; i < alarmtypeScope.length; i++) {
            for (var j = 0; j < data.length; j++) {
              var notificationtype_alarmid = data[j].notificationtype;
              if (notificationtype_alarmid) {
                for (var k = 0; k < notificationtype_alarmid.length; k++) {
                  if (alarmtypeScope[i].alarmvalue == notificationtype_alarmid[k]) {
                    notificationtype_alarmid[k] = alarmtypeScope[i].alarmtype;
                  }
                }
              }

            }
          }

          that.setState({
            notifications: data.map(function (notification, index) {
              notification.idx = index;
              notification.description = notification.description || "";
              notification.active = notification.active ? "Yes" : "No";
              notification.notificationtype = notification.notificationtype || ""
              notification.severity = notification.severity || ""
              notification.msg = notification.msg || "";
              notification.hold_off = notification.hold_off / 60;
              notification.resend_interval = notification.resend_interval / 60;
              // notification.scope = notification.scope || "";
              //notification.window = notification.window.split("@")[1] || notification.window;
              notification.additionalEmails = notification.additionalEmails || "";
              return notification;
            })
          })

          that.setState({
            activeNotifications: that.getActiveNotifications(),
            inactiveNotifications: that.getInactiveNotifications()
          })
        }
      },
      error: function (jqXHR, status, error) {
        console.error("ajax failure (notifications): ", status, error);
        $("#loadingmsg").html("Cannot retrieve notification list.  API call failed.");
      }
    });

    // API call for get Notification Types (Alarm Types)

    $.ajax({
      url: NSN.apiURL + 'notificationtypes',
      xhrFields: {
        withCredentials: true
      },
      data: '',
      method: 'GET',
      dataType: 'json',
      success: function (data) {
        
        that.setState({
          alarmTypeList: data
        })       
      },
      error: function (jqXHR, status, error) {
        console.error("ajax failure (notificationtypes): ", status, error);
        $("#loadingmsg").html("Cannot retrieve notificationtypes.  API call failed.");
      }
    });

    // Api call for alerts from devices:
    $.ajax({
      url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/alerts',
      xhrFields: {
        withCredentials: true
      },
      data: '',
      method: 'GET',
      dataType: 'json',
      success: function (data) {

        if (data.errors) {
          console.log("/customers/" + NSN.customerID + "/alerts API returned error: " + JSON.stringify(data));
          $("#loadingmsg").html("Cannot retrieve alert list.  API returned: " + JSON.stringify(data));
        } else {
          console.log("ajax success: " + JSON.stringify(data));

          that.setState({
            alerts: data.map(function (alert, index) {
              alert.idx = index;
              return alert;
            })
          })
        };
      },
      error: function (jqXHR, status, error) {
        console.error("ajax failure (alerts): ", status, error);
        $("#loadingmsg").html("Cannot retrieve alert list.  API call failed.");
      }
    });

    //Get customers list:

    if (auth.allowed('CAN_READ', 'OrgModel')) {
      DataUtil.getAll('customers', that.processCustomerData);
    } else {
      this.setState({
        customers: [{
          orgid: NSN.userInfo.user.orgs[0],
          name: NSN.userInfo.user.orgNames[0]
        }]
      });
    }

    // $.ajax({
    //   url: NSN.apiURL + 'customers',
    //   data : '',
    //   method : 'GET',
    //   xhrFields: {
    //      withCredentials: true
    //   },
    //   dataType : 'json',
    //   success : function(data){
    //     if (data.errors) {
    //       console.log("/customers API returned error: " + JSON.stringify(data));
    //       $("#loadingmsg").html("Cannot retrieve customer list. " + "/customers API returned error: " + JSON.stringify(data));
    //     } else {
    //       console.log("ajax success: " + JSON.stringify(data));
    //       NSN.customers = data;
    //       that.setState({
    //         customers: data.map(function(customer, index) {
    //           customer.idx = index;
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

    //Get sitelist:

    DataUtil.getAll('sites', that.processSiteData);
    // $.ajax({
    //   url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites',
    //   data : '',
    //   method : 'GET',
    //   xhrFields: {
    //      withCredentials: true
    //   },
    //   dataType : 'json',
    //   success : function(data){

    //       console.log("ajax success: " + JSON.stringify(data));
    //       that.setState({
    //         sites: data.map(function(site, index) {
    //           site.idx = index;
    //           return site;
    //           })
    //       });

    //   },
    //   error : function(jqXHR, status, error){
    //     console.log("ajax failure (sites): " + status + " - " + error);
    //     $("#loadingmsg").html("Cannot retrieve site list.  API call failed.");
    //   }
    // });
  },


  getNotification: function (notificationID) {
    for (var i = 0; i < this.state.notifications.length; i++) {
      if (this.state.notifications[i].notificationid == notificationID) {
        return (this.state.notifications[i]);
      }
    }
    return null;
  },
  ////////////////////Callback function to get customers//////////
  processCustomerData: function (data) {
    if (data.errors) {
      console.log("/customers API returned error: " + JSON.stringify(data));
      $("#loadingmsg").html("Cannot retrieve customer list. " + "/customers API returned error: " + JSON.stringify(data));
    } else {
      console.log("ajax success: " + JSON.stringify(data));
      NSN.customers = data;
      this.setState(DataUtil.assignState('customers', data, this, this.makeCustomerObj));
    };
  },
  makeCustomerObj: function (customer, index) {
    customer.idx = index;
    return customer;
  },
  ///////////////Callback function to get sites////////////
  processSiteData: function (data) {
    console.log("ajax success: " + JSON.stringify(data));
    this.setState(DataUtil.assignState('sites', data, this, this.makeSiteObj));
  },
  makeSiteObj: function (site, index) {
    site.idx = index;
    return site;
  },
  ////////////////Callback function to Delete Notification///////////
  processDeleteNotification: function (notification_info, idx, data) {

    var newState = React.addons.update(this.state, { notifications: { $splice: [[idx, 1]] }, notificationID: { $set: "-1" } });
    noty({ type: "success", text: 'Notification "' + notification_info.name + '" deleted.' })
    NSN.notificationID = "-1";
    sessionStorage.setItem("notificationID", NSN.notificationID);
    ReactBootstrap.Dispatcher.emit("Notificationform.delete.success", notification_info.notificationid);
    this.setState(newState);

  },
  /////////////Callback function to add Notification////////////

  // processAddNotification: function (data) {
  //   console.log("Response from Add Notifications: " + JSON.stringify(data));
  //   NSN.notificationID = data.notificationid;
  //   sessionStorage.setItem("notificationID", NSN.notificationID);
  //   data.idx = this.state.notifications.length;
  //  // data.scope = data.inputdata.selected_scope1;
  //   data.hold_off = data.hold_off / 60;
  //   data.resend_interval = data.resend_interval / 60;
  //   data.emailUsersList = data.inputdata.selected_emailUsers1;
  //   data.smsUsersList = data.inputdata.selected_smsUsers1;
  //   data.active = (data.active) ? "Yes" : "No";

  //   //var windowSplit = data.window.split("@");
  //   //data.window = windowSplit[1];
  //   var newState = React.addons.update(this.state, { notifications: { $push: [data] }, notificationID: { $set: data.notificationid } });
  //   noty({ type: "success", text: 'Notification "' + data.name + '" added.' });
  //   ReactBootstrap.Dispatcher.emit("Notificationform.add.success", data);
  //   this.setState(newState);
  // },
  /////////////////Callback function to update notification//////////
  processUpdateNotification: function (data, idx) {

    /* var selected_scopeId = $("#scope").val();
     var selected_scope = $("#scope").text();
 
     data.scope = selected_scope; */

    data.hold_off = data.hold_off / 60;

    data.resend_interval = data.resend_interval / 60;

    data.active = (data.active) ? "Yes" : "No";

    console.log("Email updating" + JSON.stringify(data));
    var newState = React.addons.update(this.state, { notifications: { [idx]: { $set: data } }, submitStatus: {$set: true} });
    noty({ type: "success", text: 'Notification "' + data.name + '" updated.' })
    ReactBootstrap.Dispatcher.emit('Notificationform.update.success', data);
    this.setState(newState);

  },

  getActiveNotifications: function(){
    console.log("outside if in getActiveNotifications in panel", this);
    var activeNotes=[];
    var index = -1;
    for(var key in this.state.notifications){
        
        var obj=this.state.notifications[key];
        if(obj.active=="Yes"){
            console.log("inside if in getActiveNotifications", obj);
            index ++;
            obj.idx = index;
            activeNotes.push(obj);
        }

    }
    return activeNotes;

  },

  getInactiveNotifications: function(){
    var inactiveNotes=[];
    var index = -1;
    for(var key in this.state.notifications){        
        var obj=this.state.notifications[key];
        if(obj.active=="No"){
            index ++;
            obj.idx = index;
            inactiveNotes.push(obj);
        }

    }
    return inactiveNotes;
  },


  componentWillMount: function () {
    console.log("componentWillMount");
  },

  componentDidMount: function () {
    console.log("componentDidMount");
    this.init();
    var that = this;

    ReactBootstrap.Dispatcher.on("Notificationlist.select", function (notificationID, sortedFlag) {
      // if(notificationID != NSN.notificationID){
      if (that.state.showDetailFlag && (notificationID != NSN.notificationID || (notificationID == NSN.notificationID && !sortedFlag))) {
        NSN.notificationID = notificationID;
        NSN.notification = that.getNotification(notificationID);
        that.setState({
          "notificationID": notificationID,
          showNotificationDetail: true,
          submitStatus: true,
          selectedNotificationID: notificationID
        });
      }
    });

    ReactBootstrap.Dispatcher.on("Notification-inactivelist.select", function (notificationID, sortedFlag) {
      // if(notificationID != NSN.notificationID){
      if (that.state.showDetailFlag &&(notificationID != NSN.notificationID || (notificationID == NSN.notificationID && !sortedFlag))) {
        NSN.notificationID = notificationID;
        NSN.notification = that.getNotification(notificationID);
        that.setState({
          "notificationID": notificationID,
          showNotificationDetail: true,
          submitStatus: true,
          selectedNotificationID: notificationID
        });
      }
    });

    ReactBootstrap.Dispatcher.on("Notificationlist.add", function (notification_info) {
      console.log("Notificationform.add", notification_info);
      $("#notification-table tbody tr").removeClass("selected");
      that.setState({
        "notificationID": "0",
        showNotificationDetail: true,
        submitStatus: true
      });
    });

    ReactBootstrap.Dispatcher.on("Notificationform.reset", function () {
      that.forceUpdate();
    });

    ReactBootstrap.Dispatcher.on("Notificationlist.deactivate", function (notificationID, idx) { 
      that.setState({
        showDetailFlag: false
      });
      // var notification_info = that.getNotiObj(that.state.activeNotifications,notificationID);
      // var idx = helpers.get_idx(that.state.activeNotifications, notification_info, 'notificationid');
      // console.log("notification_info user (idx:" + idx + "; id:" + notification_info.notificationid+ ")");
      //delete notification_info.idx;
      var deactivateObj={};
      var ids = [];
      for(var key in that.state.activeNotifications){
            var obj=that.state.activeNotifications[key];
            if(obj.notificationid==notificationID){
                deactivateObj=obj;
            }
      }
      for(var key in that.state.inactiveNotifications){
        var obj=that.state.inactiveNotifications[key];
        ids.push(obj.idx);
  }
      
      var inactive_idx =  Math.max.apply(null, ids)+1;
      
      var newState = {};
     // notiID = that.state.selectedNotificationID;
              $.ajax({
          "url" : NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/notifications/deactivate/' +notificationID,
          "type" : "POST",
          "xhrFields": {
             withCredentials: true
          },
          "data" :null,
          "dataType" : "json",
          "contentType" : "application/json",
          "success" : function(data) {
            data = deactivateObj;
            data.idx = inactive_idx;
            data.active = "No";
            var newState = React.addons.update(that.state, { activeNotifications: { $splice: [[idx, 1]] }, inactiveNotifications:{ $push : [data] }});
            that.setState({newState:newState});
            ReactBootstrap.Dispatcher.emit("Notificationform.delete.success", notificationID);
            ReactBootstrap.Dispatcher.emit('Notification-inactiveform.add.success', data);
            that.setState({
              showNotificationDetail: false,
              showDetailFlag: true
            });

            //this.setState({inactiveNotifications:obj});
          }.bind(that),
          "error" : function(jqXHR, status, error){
            console.log("ajax failure (notification update): " + status + " - " + error);
            noty({type:"error", text:'Could not update notification.'});
          }
        })
    });

    ReactBootstrap.Dispatcher.on("Notification-inactivelist.activate", function (notificationID, idx) {
      that.setState({
        showDetailFlag: false
      });

     // notiID = that.state.selectedNotificationID;
     var activateObj={};
     var ids_inactive = [];
     for(var key in that.state.inactiveNotifications){
           var obj=that.state.inactiveNotifications[key];
           if(obj.notificationid==notificationID){
            activateObj=obj;

           }
     }
     for(var key in that.state.activeNotifications){
      var obj=that.state.activeNotifications[key];
      ids_inactive.push(obj.idx);
    }

  
    
      var active_idx =  Math.max.apply(null, ids_inactive)+1;
     var newState = {};
              $.ajax({
          "url" : NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/notifications/activate/' +notificationID,
          "type" : "POST",
          "xhrFields": {
             withCredentials: true
          },
          "data" :null,
          "dataType" : "json",
          "contentType" : "application/json",
          "success" : function(data) {
            data = activateObj;
            data.idx = active_idx;
            data.active = "Yes";
            var newState = React.addons.update(that.state, { inactiveNotifications: { $splice: [[idx, 1]] }, activeNotifications:{ $push : [data] }});
            that.setState({newState:newState});
            ReactBootstrap.Dispatcher.emit("Notification-inactiveform.delete.success", notificationID);
            ReactBootstrap.Dispatcher.emit('Notificationform.add.success', data);
            that.setState({
              showNotificationDetail: false,
              showDetailFlag: true
            });

          }.bind(that),
          "error" : function(jqXHR, status, error){
            console.log("ajax failure (notification update): " + status + " - " + error);
            noty({type:"error", text:'Could not update notification.'});
          }
        })
    });

    ReactBootstrap.Dispatcher.on("Notificationform.delete", function (notification_info) {

      var idx = helpers.get_idx(that.state.notifications, notification_info, 'notificationid');
      console.log("Deleting notification (idx:" + idx + "; id:" + notification_info.notificationid + ")");
      delete notification_info.idx;

      DataUtil.deleteEntity('delete-notification', notification_info, that.processDeleteNotification, idx);
      that.setState({
        showNotificationDetail: false
      });
      // $.ajax({
      //   "url" : NSN.apiURL + 'customers/' + NSN.customerID + '/notifications/' + notification_info.notificationid,
      //   "type" : "DELETE",
      //   "xhrFields": {
      //      withCredentials: true
      //   },
      //   "data" : JSON.stringify(notification_info),
      //   "dataType" : "json",
      //   "contentType" : "application/json",
      //   "success" : function(data) {

      //     var newState = React.addons.update(this.state, { notifications: { $splice: [[idx, 1]] }, notificationID: { $set : "-1"}});
      //     noty({type:"success", text:'Notification "' + notification_info.name + '" deleted.'})
      //     NSN.notificationID = "-1";
      //     sessionStorage.setItem("notificationID", NSN.notificationID);
      //     ReactBootstrap.Dispatcher.emit("Notificationform.delete.success", notification_info.notificationid);
      //     this.setState(newState);

      //   }.bind(that),
      //   "error" : function(jqXHR, status, error){
      //     console.log("ajax failure (notifications): " + status + " - " + error);
      //     noty({type:"error", text:"Could not delete notification."});
      //   }
      // });
    });

    ReactBootstrap.Dispatcher.on("Notificationform.save", function (notification_info) {

      notification_info.name = $("select#name option").filter(":selected").val();
      that.setState({
          submitStatus: false
      });

      //EmailUsers:
      if ($("#select_emailUsersList").val() != null) {

        var selected_emailUsers = $("#select_emailUsersList").multipleSelect("getSelects", "text");

        var selected_emailUsersId = $("#select_emailUsersList").multipleSelect("getSelects", "val");
        notification_info.emailUsersList = selected_emailUsersId;
      }

      // Additional emails:

      var additionalEmailsValue = $("#additionalEmails").val();
      var additionalEmailsList = additionalEmailsValue.split(",");
      notification_info.additionalEmails = additionalEmailsList;

      //SMsUsers:

      if ($("#select_smsUsersList").val() != null) {

        var selected_smsUsers = $("#select_smsUsersList").multipleSelect("getSelects", "text");
        var selected_smsUsersId = $("#select_smsUsersList").multipleSelect("getSelects", "val");

        notification_info.smsUsersList = selected_smsUsersId;
      }

      // Severity:

      if ($("#select_severity").val() != null && $("#select_severity").val().length > 0) {
        
        var selected_severity = $("#select_severity").multipleSelect("getSelects", "text");
        notification_info.severity = selected_severity;
      }
      else{
        var severity =  $("#severity").val();
        var selected_severity = severity.split(",");
        notification_info.severity = selected_severity;
      }

      // Notification Type:

      if ($("#select_notificationtype").val() != null && $("#select_notificationtype").val().length > 0) {

        var selected_notificationtype = $("#select_notificationtype").multipleSelect("getSelects", "text");
        var selected_notificationtypeId = $("#select_notificationtype").multipleSelect("getSelects", "val");

        notification_info.notificationtype = selected_notificationtypeId;

      }
      else{
        var notificationtypes =  $("#notificationtype").val();
        var selected_notificationtype = notificationtypes.split(",");
        notification_info.notificationtype = selected_notificationtype;
      }

      // Window:

      var timezone = jstz.determine();
      var currenttimeZone = timezone.name();
      console.log("timezone is" + currenttimeZone);

      if (($("#windowday").val() != null) && ($("#windowtimemin").val() != "") && ($("#windowtimemax").val() != "")) {

        var selected_window = $("#windowday").multipleSelect("getSelects", "text");

        var time_from = $("#windowtimemin").val();
        var time_fromhours = Number(time_from.match(/^(\d+)/)[1]);
        var time_fromminutes = Number(time_from.match(/:(\d+)/)[1]);
        var time_fromAMPM = time_from.match(/(am|pm)$/)[1];
        if (time_fromAMPM == "pm" && time_fromhours < 12) time_fromhours = time_fromhours + 12;
        if (time_fromAMPM == "am" && time_fromhours == 12) time_fromhours = time_fromhours - 12;
        var time_fromsHours = time_fromhours.toString();
        var time_fromsMinutes = time_fromminutes.toString();
        console.log(time_fromhours, time_fromminutes);
        if (time_fromhours < 10) time_fromsHours = "0" + time_fromsHours;
        if (time_fromminutes < 10) time_fromsMinutes = "0" + time_fromsMinutes;

        time_from = time_fromsHours + ":" + time_fromsMinutes + time_fromAMPM;

        var time_to = $("#windowtimemax").val();
        var time_tohours = Number(time_to.match(/^(\d+)/)[1]);
        var time_tominutes = Number(time_to.match(/:(\d+)/)[1]);
        var time_toAMPM = time_to.match(/(am|pm)$/)[1];
        if (time_toAMPM == "pm" && time_tohours < 12) time_tohours = time_tohours + 12;
        if (time_toAMPM == "am" && time_tohours == 12) time_tohours = time_tohours - 12;
        var time_tosHours = time_tohours.toString();
        var time_tosMinutes = time_tominutes.toString();
        if (time_tohours < 10) time_tosHours = "0" + time_tosHours;
        if (time_tominutes < 10) time_tosMinutes = "0" + time_tosMinutes;
        time_to = time_tosHours + ":" + time_tosMinutes + time_toAMPM;

        var items = ['*', '*', '*'];
        var selected_window_val = $("#windowday").multipleSelect("getSelects", "val");

        items[0] = time_fromsHours + ":" + time_fromsMinutes + "-" + time_tosHours + ":" + time_tosMinutes;
        items[1] = selected_window_val;
        items[2] = currenttimeZone;

        notification_info.window = items.join(' ');

      }

      else if (($("#windowday").val() == null) && ($("#windowtimemin").val() != "") && ($("#windowtimemax").val() != "")) {

        var time_from = $("#windowtimemin").val();
        var time_fromhours = Number(time_from.match(/^(\d+)/)[1]);
        var time_fromminutes = Number(time_from.match(/:(\d+)/)[1]);
        var time_fromAMPM = time_from.match(/(am|pm)$/)[1];
        if (time_fromAMPM == "pm" && time_fromhours < 12) time_fromhours = time_fromhours + 12;
        if (time_fromAMPM == "am" && time_fromhours == 12) time_fromhours = time_fromhours - 12;
        var time_fromsHours = time_fromhours.toString();
        var time_fromsMinutes = time_fromminutes.toString();
        console.log(time_fromhours, time_fromminutes);
        if (time_fromhours < 10) time_fromsHours = "0" + time_fromsHours;
        if (time_fromminutes < 10) time_fromsMinutes = "0" + time_fromsMinutes;

        time_from = time_fromsHours + ":" + time_fromsMinutes + time_fromAMPM;

        var time_to = $("#windowtimemax").val();
        var time_tohours = Number(time_to.match(/^(\d+)/)[1]);
        var time_tominutes = Number(time_to.match(/:(\d+)/)[1]);
        var time_toAMPM = time_to.match(/(am|pm)$/)[1];
        if (time_toAMPM == "pm" && time_tohours < 12) time_tohours = time_tohours + 12;
        if (time_toAMPM == "am" && time_tohours == 12) time_tohours = time_tohours - 12;
        var time_tosHours = time_tohours.toString();
        var time_tosMinutes = time_tominutes.toString();
        if (time_tohours < 10) time_tosHours = "0" + time_tosHours;
        if (time_tominutes < 10) time_tosMinutes = "0" + time_tosMinutes;
        time_to = time_tosHours + ":" + time_tosMinutes + time_toAMPM;

        var items = ['*', '*', '*'];

        items[0] = time_fromsHours + ":" + time_fromsMinutes + "-" + time_tosHours + ":" + time_tosMinutes;
        items[2] = currenttimeZone;

        notification_info.window = items.join(' ');

      }

      else if (($("#windowday").val() != null) && ($("#windowtimemin").val() == "") && ($("#windowtimemax").val() == "")) {

        var selected_window = $("#windowday").multipleSelect("getSelects", "text");

        var items = ['*', '*', '*'];
        var selected_window_val = $("#windowday").multipleSelect("getSelects", "val");

        items[1] = selected_window_val;
        items[2] = currenttimeZone;

        notification_info.window = items.join(' ');

      }

      else {
        notification_info.window = "* * *";
      }

      // Hold_off:
      var hold_off_seconds = $("#hold_off").val() * 60;
      notification_info.hold_off = hold_off_seconds;

      // Resend Interval:
      var resend_interval_seconds = $("#resend_interval").val() * 60;
      notification_info.resend_interval = resend_interval_seconds;

      // Active:

      var selected_active = $("#active").val();
      console.log(selected_active);
      if (selected_active == "Yes") {
        notification_info.active = true;
      }
      else {
        notification_info.active = false;
      }

      // Scope:
      /*  var selected_scopeId = $("#scope").val();
        var selected_scope = $("#scope").text();
        notification_info.scope = selected_scopeId; */


      delete notification_info.rules;
      var inputdata = {
        //  selected_scope1: selected_scope,
        selected_emailUsers1: selected_emailUsers,
        selected_smsUsers1: selected_smsUsers
      };
      if (notification_info.notificationid == "") {
        if(notification_info.severity.length == 1 && notification_info.severity[0]== ""){
          notification_info.severity = [];
        }
        if(notification_info.notificationtype.length == 1 && notification_info.notificationtype[0]== ""){
          notification_info.notificationtype = [];
        }
        if(notification_info.additionalEmails.length == 1 && notification_info.additionalEmails[0]== ""){
          notification_info.additionalEmails = [];
        }
        if(notification_info.emailUsersList.length == 1 && notification_info.emailUsersList[0]== ""){
          notification_info.emailUsersList = [];
        }
        delete notification_info.idx;
        delete notification_info.notificationid;
        // DataUtil.addEntity('add-notification', notification_info, that.processAddNotification, inputdata);  
        $.ajax({
          "url": NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/notifications',
          "type": "POST",
          "xhrFields": {
            withCredentials: true
          },
          "data": JSON.stringify(notification_info),
          "dataType": "json",
          "contentType": "application/json",
          "success": function (data) {
            console.log("Response from Add Notifications: " + JSON.stringify(data));
            NSN.notificationID = data.notificationid;
            sessionStorage.setItem("notificationID", NSN.notificationID);
            data.idx = this.state.notifications.length;
            // data.scope = selected_scope;
            data.hold_off = data.hold_off / 60;
            data.resend_interval = data.resend_interval / 60;
            data.emailUsersList = selected_emailUsers;
            data.smsUsersList = selected_smsUsers;
            data.active = (data.active) ? "Yes" : "No";

            //var windowSplit = data.window.split("@");
            //data.window = windowSplit[1];
            var newState = React.addons.update(this.state, { notifications: { $push: [data] }, notificationID: { $set: data.notificationid } });
            console.log("newstate in add ajax call", newState);
            this.setState(newState);
            noty({ type: "success", text: 'Notification "' + data.name + '" added.' });
            ReactBootstrap.Dispatcher.emit("Notificationform.add.success", data);
            this.setState({
              showNotificationDetail: false,
              submitStatus: true
            });

          }.bind(that),
          "error": function (jqXHR, status, error) {
            console.log("ajax failure (notification save): " + status + " - " + error);
            noty({ type: "error", text: "Could not add notification." });
          }
        })
      } else {

        var idx = helpers.get_idx(that.state.notifications, notification_info, 'notificationid');
        console.log("Updating notification (idx:" + idx + "; id:" + notification_info.notificationid + ")");
        delete notification_info.idx;

        if (($("#windowday").val() == null) && ($("#windowtimemin").val() == "") && ($("#windowtimemax").val() == "")) {
          notification_info.window = $("#window").val();
        }

        // Email:
        if ($("#select_emailUsersList").val() == null) {
          var activeUsersList = that.state.activeUsers;
          var selectedEmails = $("#emailUsersList").val().split(",");
          var tempEmail = new Array();

          for (var key in activeUsersList) {
            if (activeUsersList.hasOwnProperty(key)) {
              console.log(key + "'s is " + activeUsersList[key]['name']);
              for (var i = 0; i < selectedEmails.length; i++) {
                if (selectedEmails[i] == activeUsersList[key]['name']) {
                  tempEmail.push(activeUsersList[key]['userid']);
                  console.log(tempEmail);

                }
              }
            }
          }
          notification_info.emailUsersList = tempEmail;
        }

        else {
          notification_info.emailUsersList = selected_emailUsersId;
        }

        // SMs
        if ($("#select_smsUsersList").val() == null) {
          var activeUsersList = that.state.activeUsers;
          var selectedSms = $("#smsUsersList").val().split(",");
          var tempSms = new Array();

          for (var key in activeUsersList) {
            if (activeUsersList.hasOwnProperty(key)) {
              console.log(key + "'s is " + activeUsersList[key]['name']);
              for (var i = 0; i < selectedSms.length; i++) {
                if (selectedSms[i] == activeUsersList[key]['name']) {
                  tempSms.push(activeUsersList[key]['userid']);
                  console.log(tempSms);

                }
              }
            }
          }
          notification_info.smsUsersList = tempSms;
        }
        else {
          notification_info.smsUsersList = selected_smsUsersId;
        }

        // Notification type:

        if ($("#select_notificationtype").val() == null || $("#select_notificationtype").val().length == 0 ) {

          var notificationTypevalue = $("#notificationtype").val().split(",");
          var temp = new Array();
          var alarmtypeScope = helpers.getAlarmTypeList();

          for (var j = 0; j < notificationTypevalue.length; j++) {
            for (var i = 0; i < alarmtypeScope.length; i++) {
              var actions = alarmtypeScope[i].actions;
              for(var k=0;k<actions.length;k++){
                if (actions[k].alarmvalue == notificationTypevalue[j]) {
                  temp.push(actions[k].alarmvalue);

                }
              }
              

            }
          }

          var filteredtemp = [];
          $.each(temp, function(i, el){
              if($.inArray(el, filteredtemp) === -1) filteredtemp.push(el);
          });
          notification_info.notificationtype = filteredtemp;
        }
        else {
          var temp = $("#select_notificationtype").val();
          var filteredtemp = [];
          $.each(temp, function(i, el){
              if($.inArray(el, filteredtemp) === -1) filteredtemp.push(el);
          });
          notification_info.notificationtype = filteredtemp;
        }

        // // Severity:

        // if ($("#select_severity").val() != null) {
        //   console.log("we are in select severity",selected_severity);
        //   notification_info.severity = selected_severity;
        // }

        DataUtil.updateEntity('update-notification', notification_info, that.processUpdateNotification, idx)
        // $.ajax({
        //   "url" : NSN.apiURL + 'customers/' + NSN.customerID + '/notifications/' + notification_info.notificationid,
        //   "type" : "POST",
        //   "xhrFields": {
        //      withCredentials: true
        //   },
        //   "data" : JSON.stringify(notification_info),
        //   "dataType" : "json",
        //   "contentType" : "application/json",
        //   "success" : function(data) {

        //     data.scope = selected_scope;

        //     data.hold_off = data.hold_off/60;

        //     data.resend_interval = data.resend_interval/60;

        //     data.active = (data.active)?"Yes":"No";

        //     console.log("Email updating" + JSON.stringify(data));
        //     var newState = React.addons.update(this.state, { notifications: { [idx]: { $set: data } }});
        //     noty({type:"success", text:'Notification "' + data.name + '" updated.'})
        //     ReactBootstrap.Dispatcher.emit('Notificationform.update.success', data);
        //     this.setState(newState);

        //   }.bind(that),
        //   "error" : function(jqXHR, status, error){
        //     console.log("ajax failure (notification update): " + status + " - " + error);
        //     noty({type:"error", text:'Could not update notification.'});
        //   }
        // })
      }
    })
  },

  componentWillUnmount: function () {
    ReactBootstrap.Dispatcher.removeAllListeners("Notificationlist.select");
    ReactBootstrap.Dispatcher.removeAllListeners("Notification-inactivelist.select");
    ReactBootstrap.Dispatcher.removeAllListeners("Notificationlist.add");
    ReactBootstrap.Dispatcher.removeAllListeners("Notificationform.save");
    ReactBootstrap.Dispatcher.removeAllListeners("Notificationform.delete");
    ReactBootstrap.Dispatcher.removeAllListeners("Notificationform.reset");
    ReactBootstrap.Dispatcher.removeAllListeners("Notification-inactivelist.activate");
    ReactBootstrap.Dispatcher.removeAllListeners("Notificationlist.deactivate");
  },

  //componentWillReceiveProps: function(nextProps) {
  //  // invoked when a mounted component receives new props
  //
  //},
  //
  //shouldComponentUpdate: function(nextProps, nextState) {
  //  // invoked when a component decides whether any changes warrant an update to the DOM
  //  // returns boolean
  //
  //
  //},
  //
  //componentWillUpdate: function(nextProps, nextState) {
  //  // invoked immediately before updating occurs.
  //  // Cannot use this.setState() here
  //
  //},
  //
  //componentDidUpdate: function(prevProps, prevState) {
  //  // invoked immediately after updating occurs
  //
  //},
  //
  //componentWillUnmount: function() {
  //  // invoked immediately before a component is unmounted and destroyed. Cleanup should go here.
  //
  //},

  // New Fixture panel
  hideNotificationDetail() {
    this.setState({
      showNotificationDetail: false
    })
  },

  render: function () {
    var hstyle = { height: helpers.calcHeight(90, 0) + "px !important", 'overflowY': 'auto', 'overflowX': 'hidden' }
    var hstyle2 = { height: helpers.calcHeight(90, 0) + "px !important", 'overflowY': 'none', 'overflowX': 'none' }
    if (this.state.notifications && this.state.alerts && this.state.customers && this.state.sites && this.state.activeNotifications && this.state.inactiveNotifications) {
      var Subpanels = (
        <div className="notificationsWrapper">
          <Col md={12} lg={12}>
            <PanelContainer>
              <Panel>
                <PanelBody style={hstyle2}>
                  {/*<NotificationDetail show={this.state.showNotificationDetail} hide={this.hideNotificationDetail} notifications={this.state.notifications} sites={this.state.sites} notificationID={this.state.notificationID} activeUsers={this.state.activeUsers} />*/}
                  <Notificationlist notifications={this.state.notifications} notificationID={this.state.notificationID} activeUsers={this.state.activeUsers} selectedNotificationID={this.state.selectedNotificationID} activeNotifications={this.state.activeNotifications} inactiveNotifications={this.state.inactiveNotifications}/>
                </PanelBody>
              </Panel>
            </PanelContainer>
          </Col>
          {/*<Col md={12} lg={7}>
            <PanelContainer>
              <Panel>
                <PanelBody style={hstyle}>
                  <NotificationDetail notifications={this.state.notifications} sites={this.state.sites} notificationID={this.state.notificationID} activeUsers={this.state.activeUsers} />
                </PanelBody>
              </Panel>
            </PanelContainer>
          </Col>*/}
          <Col md={12} lg={12}>
            <PanelContainer>
              <Panel>
                <PanelBody>
                  <Alertlist alerts={this.state.alerts} alertID={this.state.alertID} customers={this.state.customers} sites={this.state.sites} />
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
              <div className="netsense-center-panel">
                <NotificationDetail show={this.state.showNotificationDetail} hide={this.hideNotificationDetail} notifications={this.state.notifications} sites={this.state.sites} notificationID={this.state.notificationID} activeUsers={this.state.activeUsers} submitStatus={this.state.submitStatus} alarmTypeList={this.state.alarmTypeList} />
                <Col sm={12}>
                  <Row>
                    {Subpanels}
                  </Row>
                </Col>
              </div>
            </Row>
          </Grid>
        </Container>
      );
    };
    var forSite = (NSN && NSN.siteName && NSN.siteName != "") ? (" for " + NSN.siteName) : "";
    return (
      <Container id='body'>
        <Grid>
          <Row>
            <Col sm={12}>
              <PanelContainer>
                <Panel>
                  <PanelBody style={hstyle} ref="loading">
                    <h2 id="loadingmsg" style={{ paddingTop: "16%", textAlign: "center" }}>Loading notifications information{forSite}.</h2>
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
        <Footer />
      </Container>
    );
  }
}