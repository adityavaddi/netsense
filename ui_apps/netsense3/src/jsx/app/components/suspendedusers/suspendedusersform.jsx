import classNames from 'classnames';
import auth from 'global/utils/auth';
import { State, Navigation } from 'react-router';
var SuspendedUsersform = React.createClass({
  getInitialState: function(){
    return this.props.user
  },

  propTypes: {
    user: React.PropTypes.object.isRequired,
	  suspendedusers:  React.PropTypes.array.isRequired
  },

  handleChange: function (key) {
    return function (e) {
      var state = {};
      state[key] = e.target.value;
      this.setState(state);
    }.bind(this);
  },
 
  handleReactivate: function(e){
    e.stopPropagation();
    e.preventDefault();
    if (confirm("Are you sure you want to reactivate this user?")) {
      ReactBootstrap.Dispatcher.emit("Suspendedusersform.reactivate", Object.assign({},this.state));
    };
  },

  componentDidMount: function(){
    
    
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

      if((NSN.userInfo.authorization[0].type == "sensity_user") && (NSN.usertype == "sensity_customer")){
        var rolesWrapper = rolesContainer[0].userRole.toString();
        console.log(rolesWrapper);
        $.each(rolesWrapper.split(','), function(key, value) {
          $('#roles').append('<option value="' + value + '">' + value + '</option>');
        });
      }
      else if ((NSN.userInfo.authorization[0].type == "sensity_user") && (NSN.usertype == "partner_customer")){
        var rolesWrapper = rolesContainer[1].userRole.toString();
        console.log(rolesWrapper);
        $.each(rolesWrapper.split(','), function(key, value) {
          $('#roles').append('<option value="' + value + '">' + value + '</option>');
        });    
      }
      else if ((NSN.userInfo.authorization[0].type == "sensity_user") && (NSN.usertype == "end_customer")){
        var rolesWrapper = rolesContainer[2].userRole.toString();
        console.log(rolesWrapper);
        $.each(rolesWrapper.split(','), function(key, value) {
          $('#roles').append('<option value="' + value + '">' + value + '</option>');
        });    
      }
      else if ((NSN.userInfo.authorization[0].type == "sensity_admin") && (NSN.usertype == "sensity_customer")){
        var rolesWrapper = rolesContainer[3].userRole.toString();
        console.log(rolesWrapper);
        $.each(rolesWrapper.split(','), function(key, value) {
          $('#roles').append('<option value="' + value + '">' + value + '</option>');
        });    
      }
      else if ((NSN.userInfo.authorization[0].type == "sensity_admin") && (NSN.usertype == "partner_customer")){
        var rolesWrapper = rolesContainer[4].userRole.toString();
        console.log(rolesWrapper);
        $.each(rolesWrapper.split(','), function(key, value) {
          $('#roles').append('<option value="' + value + '">' + value + '</option>');
        });    
      }
      else if ((NSN.userInfo.authorization[0].type == "partner_admin") && (NSN.usertype == "partner_customer")){
        var rolesWrapper = rolesContainer[5].userRole.toString();
        console.log(rolesWrapper);
        $.each(rolesWrapper.split(','), function(key, value) {
          $('#roles').append('<option value="' + value + '">' + value + '</option>');
        });    
      }
      else if ((NSN.userInfo.authorization[0].type == "partner_admin") && (NSN.usertype == "end_customer")){
        var rolesWrapper = rolesContainer[6].userRole.toString();
        console.log(rolesWrapper);
        $.each(rolesWrapper.split(','), function(key, value) {
          $('#roles').append('<option value="' + value + '">' + value + '</option>');
        });    
      }
      else if ((NSN.userInfo.authorization[0].type == "end_user_admin") && (NSN.usertype == "end_customer")){
        var rolesWrapper = rolesContainer[7].userRole.toString();
        console.log(rolesWrapper);
        $.each(rolesWrapper.split(','), function(key, value) {
          $('#roles').append('<option value="' + value + '">' + value + '</option>');
        });    
      }
      else{
        var rolesWrapper = "";
         $.each(rolesWrapper.split(','), function(key, value) {
          $('#roles').append('<option value="' + value + '">' + value + '</option>');
        });  
      }

  },

	callPreviousItem:function(allItems, currentItem){
		$('.user-previous').keyup();
		$("#Suspendedusers-grid").data("gridInstance");
		console.log($("#Suspendedusers-grid").data("gridInstance"));
		var currentRow = $("#Suspendedusers-grid").data("gridInstance").getSelectedRows();
		var nextRow =  currentRow[0] -1;
		$("#Suspendedusers-grid").data("gridInstance").setSelectedRows([nextRow]);
	},

	callNextItem:function(allItems, currentItem){

		$('.user-next').keydown();
		$("#Suspendedusers-grid").data("gridInstance");
		console.log($("#Suspendedusers-grid").data("gridInstance"));
		var currentRow = $("#Suspendedusers-grid").data("gridInstance").getSelectedRows();
		var nextRow =  currentRow[0]+ 1;
		$("#Suspendedusers-grid").data("gridInstance").setSelectedRows([nextRow])
	},

	componentWillReceiveProps: function(nextProps){
    this.setState(nextProps.user);
  },

  render: function() {
    var user = this.props.user;

    var heading = ("SuspendedUser: " + user.name);
    var deleteBtnStyle = (user.userid=="")?{display:"none"}:{};
    var isDisabled = (((NSN.userInfo.authorization[0].type == "sensity_admin") && (NSN.usertype == "end_customer")) || (user.email == "sensity_user@sensity.com") || (user.email == "sensity_admin@sensity.com") || (user.email == "sensity_read_only@sensity.com"));

    var hide_button = (((NSN.userInfo.authorization[0].type == "sensity_admin") && (NSN.usertype == "end_customer")) || (user.email == "sensity_user@sensity.com") || (user.email == "sensity_admin@sensity.com") || (user.email == "sensity_read_only@sensity.com"));
    console.log(hide_button);
    var hide_buttonStyle = (hide_button)?{display:"none"}:{};

    if((NSN.userInfo.authorization[0].type == "sensity_admin") && (NSN.usertype == "end_customer")){
      var rolesWrapper = this.props.user.roles;
      $("#roles").empty();
      $('#roles').append('<option value="' + rolesWrapper + '">' + rolesWrapper + '</option>');
    }

	  var currentLength = $("#Suspendedusers-grid").data("gridInstance").getData().getLength();
	  var allItemsLength = this.props.suspendedusers.length;
	  var currentRow = $("#Suspendedusers-grid").data("gridInstance").getSelectedRows();
	  var noNextItem = {}; var noPreviousItem = {};var displayArrows = {};
	  if(currentLength === allItemsLength ){
		  var firstItem = 0; var lastItem = allItemsLength - 1;
		  for (var a = 0; a < allItemsLength; a++) {
			  if (this.props.suspendedusers[a]!=null && this.props.user!=null && this.props.suspendedusers[a].userid === this.props.user.userid){
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
          <div className="form-group">
            <label htmlFor="name" className="control-label col-sm-3">Name:</label>
            <div className="col-sm-6">
              <input type="text" className="form-control" id="name" disabled={isDisabled} ref="name" value={this.state.name} onChange={this.handleChange('name')} />
            </div>
          </div>
          <div className="form-group">
            <label htmlFor="email" className="control-label col-sm-3">Email:</label>
            <div className="col-sm-6">
              <input type="text" className="form-control" id="email" disabled={isDisabled} ref="email" value={this.state.email} onChange={this.handleChange('email')} />
            </div>
          </div>
          <div className="form-group">
            <label htmlFor="title" className="control-label col-sm-3">Title:</label>
            <div className="col-sm-6">
              <input type="text" className="form-control" id="title" disabled={isDisabled} ref="title" value={this.state.title} onChange={this.handleChange('title')} />
            </div>
          </div>
          <div className="form-group">
            <label htmlFor="phone" className="control-label col-sm-3">Phone:</label>
            <div className="col-sm-6">
              <input type="text" className="form-control" id="phone" disabled={isDisabled} ref="phone" value={this.state.phone} onChange={this.handleChange('phone')} />
            </div>
          </div>
          <div className="form-group">
            <label htmlFor="roles" className="control-label col-sm-3">Roles:</label>
            <div className="col-sm-6">
               <select className="form-control" id="roles" ref="roles" disabled="disabled" value={this.state.roles} onChange={this.handleChange('roles')}>
              </select> 
            </div>
          </div>
          <div className="form-group" id="createdWrapper">
            <label htmlFor="created" className="control-label col-sm-3">Created:</label>
            <div className="col-sm-6">
              <input type="text" className="form-control" id="created" disabled={isDisabled} ref="created" value={this.state.created} />
            </div>
          </div>
          <div className="form-group" id="updatedWrapper">
            <label htmlFor="updated" className="control-label col-sm-3">Updated:</label>
            <div className="col-sm-6">
              <input type="text" className="form-control" id="updated" disabled={isDisabled} ref="updated" value={this.state.updated} />
            </div>
          </div>
          <div className="col-sm-12 text-right" style={hide_buttonStyle}>
            <button type="button" className="ns-med-btn" onClick={this.handleReactivate}><b>Reactivate</b></button>
          </div>
        </form>
        </div>
      </div>
      );   
  }


});

  module.exports = SuspendedUsersform;
