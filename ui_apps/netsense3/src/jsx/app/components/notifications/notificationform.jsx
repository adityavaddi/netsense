import classNames from 'classnames';
import { State, Navigation } from 'react-router';
import helpers from 'global/utils/helpers';

var Notificationform = React.createClass({

  getInitialState: function () {
    return this.props.notification
  },

  propTypes: {
    notification: React.PropTypes.object.isRequired,
    notifications: React.PropTypes.array.isRequired,
    sites: React.PropTypes.array.isRequired,
    submitStatus: React.PropTypes.bool,
    errors: React.PropTypes.object,
    alarmTypeList: React.PropTypes.array.isRequired

  },

  handleChange: function (key) {
    return function (e) {
      var state = {};
      state[key] = e.target.value;
      this.setState(state);
    }.bind(this);
  },

  handleSubmit: function (e) {
    var message =this.state.msg 
    var holdoff = this.state.hold_off;
    var resendVal = this.state.resend_interval;
    if (holdoff % 1 !== 0) {
      $('#holdofferror').html("hold off should always be an Integer");
      return;
    }
    if (resendVal % 1 !== 0) {
      $('#resenderror').html("Resend interval value should always be an Integer");
      return;
    }
    if( message != "") {
      ReactBootstrap.Dispatcher.emit("Notificationform.save", Object.assign({}, this.state));
      e.stopPropagation();
      e.preventDefault();
      // $('.notificationForm').css("display","none");
    }
    else {
      $('#msgerror').html("Message is required");
      //this.props.errors.message == "Message is required" ;
    }
  },

  handleDelete: function (e) {
    e.preventDefault();
    e.stopPropagation();
    if (confirm("Are you sure you want to Delete this Notification?")) {
      ReactBootstrap.Dispatcher.emit("Notificationform.delete", Object.assign({}, this.state));
    };
  },

  handleReset: function (e) {
    e.stopPropagation();
    e.preventDefault();
    ReactBootstrap.Dispatcher.emit("Notificationform.reset", Object.assign({}, this.state));
  },

  componentWillReceiveProps: function (nextProps) {
    console.log("Notificationform.jsx componentWillReceiveProps", nextProps);
    //if (this.props.notification.notificationid != nextProps.notification.notificationid) {
      this.setState(nextProps.notification);
    //}
  },

  callPreviousItem: function (allItems, currentItem) {
    $('.notification-previous').keyup();
    $("#Notification-grid").data("gridInstance");
    console.log($("#Notification-grid").data("gridInstance"));
    var currentRow = $("#Notification-grid").data("gridInstance").getSelectedRows();
    var nextRow = currentRow[0] - 1
    $("#Notification-grid").data("gridInstance").setSelectedRows([nextRow])
  },

  callNextItem: function (allItems, currentItem) {
    $('.notification-next').keydown();
    $("#Notification-grid").data("gridInstance");
    console.log($("#Notification-grid").data("gridInstance"));
    var currentRow = $("#Notification-grid").data("gridInstance").getSelectedRows();
    var nextRow = currentRow[0] + 1;
    $("#Notification-grid").data("gridInstance").setSelectedRows([nextRow])
  },


  componentDidMount() {

    var activeUsersContainer = NSN.activeUsers;

    //Email dropdown:
    var sel_email = document.getElementById('select_emailUsersList');
    for (var i = 0; i < activeUsersContainer.length; i++) {

      var userName = activeUsersContainer[i].name;
      var userId = activeUsersContainer[i].userid;
      var opt = document.createElement('option');
      opt.innerHTML = userName;
      opt.value = userId;
      sel_email.appendChild(opt);
    }


    var emailplaceholder;
    emailplaceholder = "Select one or more users to send email";

    $('#select_emailUsersList').multipleSelect({
      placeholder: emailplaceholder
    });


    //Sms dropdown:
    var sel_sms = document.getElementById('select_smsUsersList');

    for (var i = 0; i < activeUsersContainer.length; i++) {

      var userName = activeUsersContainer[i].name;
      var userId = activeUsersContainer[i].userid;
      var opt = document.createElement('option');
      opt.innerHTML = userName;
      opt.value = userId;
      sel_sms.appendChild(opt);
    }

    var smsplaceholder;
    smsplaceholder = "Select one or more users to send sms";

    $('#select_smsUsersList').multipleSelect({
      placeholder: smsplaceholder
    });


    // Type & Severity selector:

    //Defining Alarmtype:

    var alarmtypeScope = helpers.getAlarmTypeList();
    var options_alarmtype = '';
    var currentCategory = '';
    var option = '';
    var totalOptions;

    // for (var i = 0; i < alarmtypeScope.length; i++) {
    //   currentCategory = alarmtypeScope[i].category;
    //   options = alarmtypeScope[i].actions;
    //   for (var j = 0; j <= options.length; j++) {
    //     if (j == 0) {
    //       options_alarmtype = '<optgroup label ="' + currentCategory + '">' + '<option value="' + options[j].alarmvalue + '">' + options[j].alarmtype + '</option>'
    //     }
    //     else if (j == options.length) {
    //       options_alarmtype += '</optgroup>';
    //     }
    //     else {
    //       options_alarmtype += '<option value="' + options[j].alarmvalue + '">' + options[j].alarmtype + '</option>';
    //     }
    //   }
    //   totalOptions += options_alarmtype;
    // }

    for(var key in this.props.alarmTypeList){
      var option = this.props.alarmTypeList[key];
      //totalOptions += '<option value="' + option.alarmtype + '">' + option.ufname + '</option>';
      totalOptions += '<optgroup label ="' +  option.ufname+'">' + '<option value="' + option.alarmtype + '">'+option.nodemodels+'</option>';
    }

    $("#select_notificationtype").html(totalOptions);

    var notificationtypeplaceholder;
    notificationtypeplaceholder = "Select one or more alarm type";

    $('#select_notificationtype').multipleSelect({
      placeholder: notificationtypeplaceholder
    });

    var severityplaceholder;
    severityplaceholder = "Select one or more severity";

    $('#select_severity').multipleSelect({
      placeholder: severityplaceholder
    });

    // Multiselect for window:

    var windowplaceholder;
    windowplaceholder = "Select one or more days";

    $('#windowday').multipleSelect({
      placeholder: windowplaceholder
    });

    // Time picker:
    $('#windowtimemin').timepicker({
      'noneOption': [
        {
          'label': '11:59pm',
          'value': '11:59pm'
        },
      ]
    });
    $('#windowtimemax').timepicker({
      'noneOption': [
        {
          'label': '11:59pm',
          'value': '11:59pm'
        },
      ]
    });

  },



  componentDidUpdate() {
    // Type & Severity selector:
    $('#typeseverityoptionselector').bind('change', function () {
      var elements = $('div.typeseverityselector').children().hide();
      var value = $(this).val();

      if (value.length) {
        elements.filter('.' + value).show();
      }
    }).trigger('change');

  },



  render: function () {
    console.log("this in noti form", this);
    this.props.errors = {};

    $('#saveNotification').remove('disabled');      
      if(!this.props.submitStatus) {
        $('#saveNotification').attr('disabled', true);
    }
    var notification = this.props.notification;
    var selected_alerttype = '';
    for(var key in this.props.alarmTypeList){
      if(this.state.notificationtype == this.props.alarmTypeList[key].alarmtype){
        selected_alerttype = this.props.alarmTypeList[key].ufname;
      }
    }
    var heading = (notification.name == "") ? "Add Notification" : ("Notification Detail - " + notification.name);
    var deleteBtnStyle = (notification.notificationid == "") ? { display: "none" } : {};

	  var currentLength = $("#Notification-grid").data("gridInstance").getData().getLength();
	  var allItemsLength = this.props.notifications.length;
	  var currentRow = $("#Notification-grid").data("gridInstance").getSelectedRows();
	  var noNextItem = {}; var noPreviousItem = {};var displayArrows = {};
	  if(currentLength === allItemsLength ){
		  var firstItem = 0; var lastItem = allItemsLength - 1;
		  for (var a = 0; a < allItemsLength; a++) {
			  if (this.props.notifications[a]!=null && this.props.notification!=null && this.props.notifications[a].notificationid === this.props.notification.notificationid){
				  noNextItem = this.props.notification.idx === lastItem ? {display:"none"}:{};
				  noPreviousItem = this.props.notification.idx === firstItem ? {display:"none"}:{};
			  }
		  }
		  displayArrows = (this.props.notification!=null && this.props.notification.notificationid === "" ) ? { display: "none" } : {};
	  }else{
		  if (this.props.notification!=null && this.props.notification.notificationid != null){
			  var firstElement = 0;
			  var lastElement = currentLength-1;
			  if(currentRow[0] === firstElement){
				  //display only the next arrow and not the previous arrow
				  if(firstElement+1 === currentLength){ displayArrows={display:"none"};
				  }else{
					  noNextItem={};noPreviousItem={display:"none"}; displayArrows={};
				  }
			  }else if(currentRow[0] === lastElement){
				  //display only the previous arrow and not the next arrow
				  noNextItem={display:"none"}; noPreviousItem={}; displayArrows={};
			  }else{
				  //display both arrows
				  noNextItem={};noPreviousItem={}; displayArrows={};
			  }
		  }
	  }


    /* var scopeOptions = this.props.sites.map(function (site, idx) {
       return <option key={idx} value={site.siteid}>{site.name}</option>
     }); */

    return (
      <div>
        <div className="netsense__form__header">
          <h3> {heading}</h3>
        </div>

        <span style={displayArrows}>
          <span style={noPreviousItem} className="rubix-icon icon-simple-line-icons-arrow-left notification-previous generic-previous" onClick={() => this.callPreviousItem(this.props.notifications, this.props.notification)}></span>
        </span>

        <span style={displayArrows}>
          <span style={noNextItem} className="rubix-icon icon-simple-line-icons-arrow-right notification-next generic-next" onClick={() => this.callNextItem(this.props.notifications, this.props.notification)}></span>
        </span>


        {/*<h2 style={{ position: "relative", top: "-40px", left: "12px" }}>{heading}</h2>*/}
        <form role="form" style={{ padding: "28px", marginLeft: "50px", color: "black", fontSize: "medium", marginRight: "70px" }} className="form-horizontal" data-notificationid={notification.notificationid} >

          <div className="form-group">
            <label htmlFor="name" className="control-label col-sm-3"> Name:</label>
            <div className="col-sm-6">
              <select className="form-control" id="name" ref="name" value={this.state.name} onChange={this.handleChange('name')}>
                <option value="DeviceAlarm"> DeviceAlarm </option>
              </select>
            </div>
          </div>

          <div className="form-group">
            <label htmlFor="description" className="control-label col-sm-3"> Description:</label>
            <div className="col-sm-6">
              <input type="text" className="form-control" id="description" readonly ref="description" value={this.state.description} onChange={this.handleChange('description')} />
            </div>
          </div>

          <div className="form-group">
            <label htmlFor="notificationtype" className="control-label col-sm-3">Alarm type:</label>
            <div className="col-sm-6">
              <input type="text" style={deleteBtnStyle} className="form-control" id="notificationtype" disabled="disabled" ref="notificationtype" value={selected_alerttype} onChange={this.handleChange('notificationtype')} />
              <select multiple="multiple" className="form-control" id="select_notificationtype" ref="select_notificationtype" value={this.state.select_notificationtype} onChange={this.handleChange('select_notificationtype')}>
              </select>
            </div>
          </div>

          <div className="form-group">
            <label htmlFor="severity" className="control-label col-sm-3">Severity:</label>
            <div className="col-sm-6">
              <input type="text" style={deleteBtnStyle} className="form-control" id="severity" disabled="disabled" ref="severity" value={this.state.severity} onChange={this.handleChange('severity')} />
              <select multiple="multiple" className="form-control" id="select_severity" ref="select_severity" value={this.state.select_severity} onChange={this.handleChange('select_severity')}>
                <option value="critical">Critical</option>
                <option value="major">Major</option>
                <option value="minor">Minor</option>
              </select>
            </div>
          </div>

          { /*<div className="form-group">
            <label htmlFor="scope" className="control-label col-sm-3">Scope:</label>
            <div className="col-sm-6">
              <select className="form-control" id="scope" ref="scope" value={this.state.scope} onChange={this.handleChange('scope')}>
                {scopeOptions}
              </select>
            </div>
          </div> */}

          <div className="form-group">
            <label htmlFor="window" className="control-label col-sm-3">Window:</label>
            <div className="col-sm-6">
              <div className="window-container" style={{ float: "left", width: "50% !important" }}>
                <input type='text' style={deleteBtnStyle} className="form-control" id='window' disabled="disabled" ref="window" value={this.state.window} onChange={this.handleChange('window')} />
                <select multiple="multiple" className="form-control" id="windowday" ref="windowday" value={this.state.windowday} onChange={this.handleChange('windowday')}>
                  <option value="1">Monday</option>
                  <option value="2">Tuesday</option>
                  <option value="3">Wednesday</option>
                  <option value="4">Thursday</option>
                  <option value="5">Friday</option>
                  <option value="6">Saturday</option>
                  <option value="7">Sunday</option>
                </select>
              </div>

              <div style={{ float: "left", width: "50%", paddingLeft: "10px" }}>
                <div style={{ float: "left", width: "calc(50% - 5px)",marginRight:"10px"}}>
                  <input type='text' className="form-control" id="windowtimemin" ref="windowtimemin" placeholder="From" value={this.state.windowtimemin} onChange={this.handleChange('windowtimemin')} />
                </div>
                <div style={{ float: "left", width: "calc(50% - 5px)" }}>
                  <input type='text' className="form-control" id='windowtimemax' ref="windowtimemax" placeholder="To" value={this.state.windowtimemax} onChange={this.handleChange('windowtimemax')} />
                </div>
              </div>

            </div>
          </div>

          <div className="form-group">
            <label htmlFor="active" className="control-label col-sm-3">Active:</label>
            <div className="col-sm-6">
              <select className="form-control" id="active" ref="active" value={this.state.active} onChange={this.handleChange('active')}>
                <option value="Yes"> Yes </option>
                <option value="No"> No </option>
              </select>
            </div>
          </div>

          <div className="form-group">
            <label htmlFor="hold_off" className="control-label col-sm-3">Hold off (min):</label>
            <div className="col-sm-6">
              <input type='text' className="form-control" id="hold_off" ref="hold_off" value={this.state.hold_off} onChange={this.handleChange('hold_off')} />
              <div id="holdofferror" className="form-error">
              </div>
            </div>
          </div>

          <div className="form-group">
            <label htmlFor="resend_interval" className="control-label col-sm-3">Resend interval (min):</label>
            <div className="col-sm-6">
              <input type='text' className="form-control" id="resend_interval" ref="resend_interval" value={this.state.resend_interval} onChange={this.handleChange('resend_interval')} />
              <div id="resenderror" className="form-error">
              </div>
            </div>
          </div>

          <div className="form-group">
            <label htmlFor="msg" className="control-label col-sm-3">Message:</label> 
            <div className="col-sm-6">
              <input type="text" className={(this.props.errors.message)? "form-control orange":"form-control"} id="msg" ref="msg" value={this.state.msg} onChange={this.handleChange('msg')} />
                <div id="msgerror" className="form-error">
                </div>
            </div>
          </div>

          <div className="form-group" id="email-notification">
            <label htmlFor="emailUsersList" className="control-label col-sm-3">Email:</label>
            <div className="col-sm-6">
              <input type="text" className="form-control" id="emailUsersList" style={deleteBtnStyle} disabled="disabled" ref="emailUsersList" value={this.state.emailUsersList} onChange={this.handleChange('emailUsersList')} />
              <select multiple="multiple" className="form-control" id="select_emailUsersList" value={this.state.select_emailUsersList} onChange={this.handleChange('select_emailUsersList')}>
              </select>
            </div>
          </div>
          <div className="form-group">
            <label htmlFor="additionalEmails" className="control-label col-sm-3">Additional emails:</label>
            <div className="col-sm-6">
              <input type="email" className="form-control" multiple id="additionalEmails" ref="additionalEmails" value={this.state.additionalEmails} onChange={this.handleChange('additionalEmails')} />
            </div>
          </div>
          <div className="form-group" id="text-notification">
            <label htmlFor="smsUsersList" className="control-label col-sm-3">Phone:</label>
            <div className="col-sm-6">
              <input type="text" className="form-control" id="smsUsersList" style={deleteBtnStyle} disabled="disabled" ref="smsUsersList" value={this.state.smsUsersList} onChange={this.handleChange('smsUsersList')} />
              <select multiple="multiple" className="form-control" id="select_smsUsersList" value={this.state.select_smsUsersList} onChange={this.handleChange('select_smsUsersList')}>
              </select>
            </div>
          </div>
          <div style={{ margin: "20px", padding: "20px" }}>
            <div className="col-xs-3 col-sm-3">
              <button style={{ marginLeft: "-50px" }} type="button" className="ns-delete-btn" onClick={this.handleDelete}>
                <b>Delete</b></button>
            </div>
            <div className="col-xs-9 col-sm-9 text-right">
              {/*<button type="button" className="ns-reset-btn" onClick={this.handleReset}>
                <b>Reset</b></button>
              &nbsp; &nbsp;*/}
              <button id="saveNotification" type="button" className="ns-save-btn" onClick={this.handleSubmit}>
               <b>Save</b></button>
            </div>
          </div>
        </form>
      </div>
    );
  }
});

module.exports = Notificationform;


