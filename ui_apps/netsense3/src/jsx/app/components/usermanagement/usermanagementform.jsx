import classNames from 'classnames';
import auth from 'global/utils/auth';
import helpers from 'global/utils/helpers';
import { State, Navigation } from 'react-router';
var Usermanagementform = React.createClass({
  getInitialState: function(){
    return this.props.user
  },

  propTypes: {
    user: React.PropTypes.object.isRequired,
    allUsers:React.PropTypes.array.isRequired,
  },

  handleChange: function (key) {
    return function (e) {
      var state = {};
      state[key] = e.target.value;
      this.setState(state);
    }.bind(this);
  },

  handleSubmit: function(e) {
    e.stopPropagation();
    e.preventDefault();
    ReactBootstrap.Dispatcher.emit("Usermanagementform.save", Object.assign({},this.state));
  },

  handleReset: function(e) {
    e.stopPropagation();
    e.preventDefault();
    ReactBootstrap.Dispatcher.emit("Usermanagementform.reset", Object.assign({},this.state));
  },

  handleResetPassword: function(e) {
    e.stopPropagation();
    e.preventDefault();
    ReactBootstrap.Dispatcher.emit("Usermanagementform.requestpwdreset", Object.assign({},this.state));
  },

  handleSuspend: function(e) {
    e.stopPropagation();
    e.preventDefault();
    if (confirm("Are you sure you want to Suspend this user?")) {
      ReactBootstrap.Dispatcher.emit("Usermanagementform.suspend", Object.assign({},this.state));
    };
  },

  handleReactivate: function(e){
    e.stopPropagation();
    e.preventDefault();
    if (confirm("Are you sure you want to reactivate this user?")) {
      ReactBootstrap.Dispatcher.emit("Usermanagementform.reactivate", Object.assign({},this.state));
    };
  },

  handleGenerateApiKey:function(e){
    e.stopPropagation();
    e.preventDefault();
    if (confirm("Are you sure you want to generate API Key?")) {
      ReactBootstrap.Dispatcher.emit("Usermanagementform.generateApiKey", Object.assign({},this.state));
    };
  },

  handleRevokeApiKey: function(e){
    e.stopPropagation();
    e.preventDefault();
    if (confirm("Are you sure you want to revoke API Key?")) {
      ReactBootstrap.Dispatcher.emit("Usermanagementform.revokeApiKey", Object.assign({},this.state));
    };
  },

  handleDelete: function(e) {
    e.stopPropagation();
    e.preventDefault();
    if (confirm("Are you sure you want to Delete this user?")) {
      ReactBootstrap.Dispatcher.emit("Usermanagementform.delete", Object.assign({},this.state));
    };
  },

  callPreviousItem:function(allItems, currentItem){
    $('.user-previous').keyup();
    $("#Usermanagement-grid").data("gridInstance");
    console.log($("#Usermanagement-grid").data("gridInstance"));
    var currentRow = $("#Usermanagement-grid").data("gridInstance").getSelectedRows();
    var nextRow =  currentRow[0] -1
    $("#Usermanagement-grid").data("gridInstance").setSelectedRows([nextRow])
  },

  callNextItem:function(allItems, currentItem){

    $('.user-next').keydown();
    $("#Usermanagement-grid").data("gridInstance");
    console.log($("#Usermanagement-grid").data("gridInstance"));
    var currentRow = $("#Usermanagement-grid").data("gridInstance").getSelectedRows();
    var nextRow =  currentRow[0]+ 1;
    $("#Usermanagement-grid").data("gridInstance").setSelectedRows([nextRow])
  },

  componentDidMount: function(){
    console.log("logged in as " + NSN.userInfo.authorization[0].type);
    console.log("selected customer " + NSN.usertype);

    // Revoke API Key:
    $('#revokeApiKeybutton').click(function(){
        $('#apiKeyOverlay').fadeIn('fast',function(){
            $('#apiKeyWrapper').animate({'top':'160px'},500);
        });
    });
    $('#overlayClose').click(function(){
        $('#apiKeyWrapper').animate({'top':'-250px'},500,function(){
            $('#apiKeyOverlay').fadeOut('fast');
        });
    });

   //Roles based on login type & org type:

    console.log("from usermgmt form customer list" + JSON.stringify(this.props.customers));

    // Find out what type of customer is selected:

    console.log(NSN.customerID);
    for(var i=0;i< this.props.customers.length;i++){
      if(NSN.customerID == this.props.customers[i].orgid){
        console.log("returned data" + JSON.stringify(this.props.customers[i]));

        if(this.props.customers[i].orgid == "efe5bdb3-baac-5d8e-6cae57771c13"){
          console.log("It is sensity user");
          NSN.usertype = "sensity_customer";
        }

        else if (this.props.customers[i].type == "partner"){
          console.log("customerid" + NSN.customerID);
          console.log("orgid" + this.props.customers[i].orgid);
          console.log("It is partner user");
          NSN.usertype = "partner_customer";
        }

        else{
          console.log("It is end user");
          NSN.usertype = "end_customer";
        }

      }
    }

    var rolesContainer = this.props.roles;
    var that = this;
      if((NSN.userInfo.authorization[0].type == "sensity_user") && (NSN.usertype == "sensity_customer")){
        var rolesWrapper = rolesContainer[0].userRole.toString();
        $.each(rolesWrapper.split(','), function(key, value) {
          if(that.props.user.roles == value){
            $('#roles').append('<option selected="selected" value="' + value + '">' + value + '</option>');
          }
          else{
            $('#roles').append('<option value="' + value + '">' + value + '</option>');
          }
        });
      }
      else if ((NSN.userInfo.authorization[0].type == "sensity_user") && (NSN.usertype == "partner_customer")){
        var rolesWrapper = rolesContainer[1].userRole.toString();
        $.each(rolesWrapper.split(','), function(key, value) {
          if(that.props.user.roles == value){
            $('#roles').append('<option selected="selected" value="' + value + '">' + value + '</option>');
          }
          else{
            $('#roles').append('<option value="' + value + '">' + value + '</option>');
          }
        });
      }
      else if ((NSN.userInfo.authorization[0].type == "sensity_user") && (NSN.usertype == "end_customer")){
        var rolesWrapper = rolesContainer[2].userRole.toString();
        console.log(that.props.user.roles);
        $.each(rolesWrapper.split(','), function(key, value) {
          if(that.props.user.roles == value){
            $('#roles').append('<option selected="selected" value="' + value + '">' + value + '</option>');
          }
          else{
            $('#roles').append('<option value="' + value + '">' + value + '</option>');
          }
        });
      }
      else if ((NSN.userInfo.authorization[0].type == "sensity_admin") && (NSN.usertype == "sensity_customer")){
        var rolesWrapper = rolesContainer[3].userRole.toString();
        $.each(rolesWrapper.split(','), function(key, value) {
          if(that.props.user.roles == value){
            $('#roles').append('<option selected="selected" value="' + value + '">' + value + '</option>');
          }
          else{
            $('#roles').append('<option value="' + value + '">' + value + '</option>');
          }
        });
      }
      else if ((NSN.userInfo.authorization[0].type == "sensity_admin") && (NSN.usertype == "partner_customer")){
        var rolesWrapper = rolesContainer[4].userRole.toString();
        $.each(rolesWrapper.split(','), function(key, value) {
          if(that.props.user.roles == value){
            $('#roles').append('<option selected="selected" value="' + value + '">' + value + '</option>');
          }
          else{
            $('#roles').append('<option value="' + value + '">' + value + '</option>');
          }
        });
      }
      else if ((NSN.userInfo.authorization[0].type == "sensity_admin") && (NSN.usertype == "end_customer")){
        var rolesWrapper = rolesContainer[2].userRole.toString();
        console.log(that.props.user.roles);
        $.each(rolesWrapper.split(','), function(key, value) {
          if(that.props.user.roles == value){
            $('#roles').append('<option selected="selected" value="' + value + '">' + value + '</option>');
          }
          else{
            $('#roles').append('<option value="' + value + '">' + value + '</option>');
          }
        });
      }
      else if ((NSN.userInfo.authorization[0].type == "partner_admin") && (NSN.usertype == "partner_customer")){
        var rolesWrapper = rolesContainer[5].userRole.toString();
        $.each(rolesWrapper.split(','), function(key, value) {
          if(that.props.user.roles == value){
            $('#roles').append('<option selected="selected" value="' + value + '">' + value + '</option>');
          }
          else{
            $('#roles').append('<option value="' + value + '">' + value + '</option>');
          }
        });
      }
      else if ((NSN.userInfo.authorization[0].type == "partner_admin") && (NSN.usertype == "end_customer")){
        var rolesWrapper = rolesContainer[6].userRole.toString();
        $.each(rolesWrapper.split(','), function(key, value) {
          if(that.props.user.roles == value){
            $('#roles').append('<option selected="selected" value="' + value + '">' + value + '</option>');
          }
          else{
            $('#roles').append('<option value="' + value + '">' + value + '</option>');
          }
        });
      }
      else if ((NSN.userInfo.authorization[0].type == "end_user_admin") && (NSN.usertype == "end_customer")){
        var rolesWrapper = rolesContainer[7].userRole.toString();
        $.each(rolesWrapper.split(','), function(key, value) {
          if(that.props.user.roles == value){
            $('#roles').append('<option selected="selected" value="' + value + '">' + value + '</option>');
          }
          else{
            $('#roles').append('<option value="' + value + '">' + value + '</option>');
          }
        });
      }
      else{
        var rolesWrapper = "";
        $.each(rolesWrapper.split(','), function(key, value) {
          $('#roles').append('<option value="' + value + '">' + value + '</option>');
        });
      }
     if(((NSN.userInfo.authorization[0].type == "sensity_admin") && (this.props.user.userid != "")) 
      || ((NSN.userInfo.authorization[0].type == "sensity_user") && (this.props.user.userid != "")) 
      || ((NSN.userInfo.authorization[0].type == "sensity_user")) 
      || ((NSN.userInfo.authorization[0].type == "partner_admin") && (this.props.user.userid != ""))){
        $("#generateApiKeybutton").show();
        $("#revokeApiKeybutton").show();
      }
      else{
        $("#generateApiKeybutton").hide();
        $("#revokeApiKeybutton").hide();
      }
  },

  componentDidUpdate() {

    if(((NSN.userInfo.authorization[0].type == "sensity_admin") && (this.props.user.userid != "")) 
      || ((NSN.userInfo.authorization[0].type == "sensity_user") && (this.props.user.userid != "")) 
      || ((NSN.userInfo.authorization[0].type == "sensity_user")) 
      || ((NSN.userInfo.authorization[0].type == "partner_admin") && (this.props.user.userid != ""))){
      $("#generateApiKeybutton").show();
      $("#revokeApiKeybutton").show();
    }
    else{
      $("#generateApiKeybutton").hide();
      $("#revokeApiKeybutton").hide();
    }
  },

  componentWillReceiveProps: function(nextProps){
    this.setState(nextProps.user);
  },

  render: function() {
    var user = this.props.user;

    var existingUser = (user.userid != "");
    var heading = (!existingUser)?"Add User":("User: " + user.name);

    var isDisabled = (/* ((NSN.userInfo.authorization[0].type == "sensity_admin") && (NSN.usertype == "end_customer")) 
                    || */ (NSN.userInfo.authorization[0].type == "end_user_read_only") && (NSN.usertype == "end_customer") 
                    || (user.email == "sensity_user@sensity.com") 
                    || (user.email == "sensity_admin@sensity.com") 
                    || (user.email == "sensity_read_only@sensity.com"));

    var hide_button = (/* ((NSN.userInfo.authorization[0].type == "sensity_admin") && (NSN.usertype == "end_customer")) 
                    || */ (user.email == "sensity_user@sensity.com") 
                    || (user.email == "sensity_admin@sensity.com") 
                    || (user.email == "sensity_read_only@sensity.com"));

    var hide_buttonStyle = (hide_button)?{display:"none"}:{};

	  var currentLength = $("#Usermanagement-grid").data("gridInstance").getData().getLength();
	  var allItemsLength = this.props.allUsers.length;
	  var currentRow = $("#Usermanagement-grid").data("gridInstance").getSelectedRows();
	  var noNextItem = {}; var noPreviousItem = {};var displayArrows = {};
	  if(currentLength === allItemsLength ){
		  var firstItem = 0; var lastItem = allItemsLength - 1;
		  for (var a = 0; a < allItemsLength; a++) {
			  if (this.props.allUsers[a]!=null && this.props.user!=null && this.props.allUsers[a].userid === this.props.user.userid){
				  noNextItem = this.props.user.idx === lastItem ? {display:"none"}:{};
				  noPreviousItem = this.props.user.idx === firstItem ? {display:"none"}:{};
			  }
		  }
		  displayArrows = (this.props.user!=null && this.props.user.userid === "" ) ? { display: "none" } : {};
	  }else{
		  if (this.props.user!=null && this.props.user.userid != null){
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


/*
    if((NSN.userInfo.authorization[0].type == "sensity_admin") && (NSN.usertype == "end_customer")){
      var rolesWrapper = this.props.user.roles;
      $("#roles").empty();
      $('#roles').append('<option value="' + rolesWrapper + '">' + rolesWrapper + '</option>');
    }
*/

    var hstyle = {
      overflowY: "auto", overflowX: "hidden", marginBottom: "16px",
      maxHeight: helpers.calcHeight(100, -240) + "px !important"
    };

    return (

      <div>
        <div className="netsense__form__header">
          <h3> {heading} </h3>
        </div>
        <div className="netsense__form__body">

           <span style={displayArrows}>
              <span style={noPreviousItem} className="rubix-icon icon-simple-line-icons-arrow-left user-previous generic-previous" onClick={ () => this.callPreviousItem(this.props.allUsers,this.state.user)}></span>
           </span>
          <span style={displayArrows}>
             <span style={noNextItem} className="rubix-icon icon-simple-line-icons-arrow-right user-next generic-next" onClick={ () => this.callNextItem(this.props.allUsers,this.state.user)}></span>
          </span>

          <form role="form" className="form-horizontal" data-userid={user.userid} >
<div style={hstyle}>
          <div className="form-group">
            <label htmlFor="name" className="control-label col-sm-3">Name:</label>
            <div className="col-sm-8">
              <input type="text" className="form-control" id="name" ref="name" disabled={isDisabled} value={this.state.name} onChange={this.handleChange('name')} />
            </div>
          </div>
          <div className="form-group">
            <label htmlFor="email" className="control-label col-sm-3">Email:</label>
            <div className="col-sm-8">
              <input type="text" className="form-control" id="email" ref="email" disabled={isDisabled} value={this.state.email} onChange={this.handleChange('email')} />
            </div>
          </div>
          <div className="form-group">
            <label htmlFor="title" className="control-label col-sm-3">Title:</label>
            <div className="col-sm-8">
              <input type="text" className="form-control" id="title" ref="title" disabled={isDisabled} value={this.state.title} onChange={this.handleChange('title')} />
            </div>
          </div>
          <div className="form-group">
            <label htmlFor="phone" className="control-label col-sm-3">Phone:</label>
            <div className="col-sm-8">
              <input type="text" className="form-control" id="phone" ref="phone" disabled={isDisabled} value={this.state.phone} onChange={this.handleChange('phone')} />
            </div>
          </div>
          <div className="form-group">
            <label htmlFor="roles" className="control-label col-sm-3">Roles:</label>
            <div className="col-sm-8">
               <select className="form-control" id="roles" ref="roles" disabled={isDisabled} value={this.state.roles} onChange={this.handleChange('roles')}>
              </select>
            </div>
          </div>
{ existingUser && 
          (
          <div>
          <div className="form-group" id="createdWrapper">
            <label htmlFor="created" className="control-label col-sm-3">Created:</label>
            <div className="col-sm-8">
              <input type="text" className="form-control" id="created" ref="created" disabled={isDisabled} value={this.state.created} />
            </div>
          </div>
          <div className="form-group" id="updatedWrapper">
            <label htmlFor="updated" className="control-label col-sm-3">Updated:</label>
            <div className="col-sm-8">
              <input type="text" className="form-control" id="updated" ref="updated" disabled={isDisabled} value={this.state.updated} />
            </div>
          </div>
          </div>
          )
}
          <div className="apiKeyOverlay" id="apiKeyOverlay" style={{display:"none"}}></div>
          <div className="apiKeyWrapper" id="apiKeyWrapper">
              <a className="overlayClose" id="overlayClose"></a>
              <h3>Please enter the Key:</h3>
              <div className="form-group" id="apikey">
                <label htmlFor="apikey" className="control-label col-sm-3">API key:</label>
                <div className="col-sm-6">
                  <input type="text" className="form-control" id="apikey" ref="apikey" value={this.state.apikey} onChange={this.handleChange('apikey')}/>
                </div>
            </div>
            <button type="button" className="ns-save-btn" onClick={this.handleRevokeApiKey}>
            <Icon glyph="icon-fontello-ok" /><b> Submit</b></button>
          </div>
</div>
          <div className="col-xs-6 col-sm-7 col-md-6 text-right">
            <button type="button" className="ns-api-key-btn" id="generateApiKeybutton" onClick={this.handleGenerateApiKey}><b>Generate API Key</b></button>
            
            <button type="button" className="ns-api-key-btn revokeApiKeybutton" id="revokeApiKeybutton"><b>Revoke API Key</b></button>
          </div>
          <div className="col-xs-6 col-sm-5 col-md-6" style={hide_buttonStyle}>

{ existingUser &&
            (
            <button type="button" id="suspendUser" className="ns-suspend-btn" onClick={this.handleSuspend}><b>Suspend</b></button>
            )
}
            { existingUser &&
            (
            <button type="button" className="ns-reset-pwd-btn" onClick={this.handleResetPassword} disabled={isDisabled} id="resetPasswordbutton"><b> Reset Password</b></button>
            )
}
<button type="button" className="ns-save-btn" id="saveUser" onClick={this.handleSubmit}><b> Save</b></button>
          </div>
        </form>
        </div>
      </div>
      );
  }
});

  module.exports = Usermanagementform;
