import classNames from 'classnames';
import { State, Navigation } from 'react-router';
import DataGrid from 'components/datagrid';
import Listeditor  from 'components/listeditor';

var Notificationlist = React.createClass({

        getInitialState: function(){
            var component = "Notification";
            var userRole = NSN.userInfo.name;
            var componentColumns = "NotificationColumnWidths_"+ userRole;
            var notificationStoredWidths = [];

            // if(localStorage.getItem(componentColumns)){
            //     notificationStoredWidths = JSON.parse(localStorage.getItem(componentColumns));
            // }else{
                // default widths
                notificationStoredWidths = [
                    {name:"Name", field:"name", id:"name",sortable:true, width:100, checked:true, required:true},
                    {name:"Active", field:"active", id:"active", sortable:true, width:90, checked:true, required:false},
                    {name:"Type", field:"notificationtype", id:"notificationtype", sortable:true, width:60, checked:true, required:false},
                    {name:"Severity", field:"severity", id:"severity", sortable:true, width:60, checked:true, required:true},
                    {id:"action",name:"Action",field:"action", width:60, maxWidth: 75, checked:true,required:true, disableSearch:true}
                ]
            // }
            
            return {
                showListEditor:false,
                notificationStoredWidths:notificationStoredWidths,
                componentColumns:componentColumns,
                component:component
            }
        },
   propTypes: {
    notifications: React.PropTypes.array.isRequired,
    notificationID: React.PropTypes.string.isRequired,
    activeNotifications: React.PropTypes.array.isRequired,
    inactiveNotifications : React.PropTypes.array.isRequired
  },

  handleAdd: function() {
    ReactBootstrap.Dispatcher.emit("Notificationlist.add");
  }, 


  componentDidMount: function () {
    $("#inactive-notification-grid").hide();
  },

  shouldComponentUpdate: function(nextProps,nextState){
    // This component does not need to be rerendered unless fixtures have been added or changed
    // return this.props.notifications !== nextProps.notifications;
      return true;
  },

   handleOpen : function(){
      this.setState({showListEditor:!this.state.showListEditor});
   },

   handleOnChange: function(){
    var value = $("#activeNotification").val();
    if(value == "Active"){
        $("#active-notification-grid").show();
        $("#inactive-notification-grid").hide();
    }
    else{
        //var notiVal= this.getInActiveNotifications();
       //this.setState({selectedNotification: this.state.inactiveNotifications});
       $("#active-notification-grid").hide();
       $("#inactive-notification-grid").show();
    }
   },

  render() {
    var that = this;
    //console.log("state in notification list render",this);
    var Addbutton = (
        <button id="addNotification" title="Add Notification" className="ns-big-btn" onClick={this.handleAdd}><b>Add Notification</b></button>
        ); 
    var AddbuttonVisibility = (NSN.userInfo.authorization[0].type =="end_user_read_only") ?{} :{Addbutton}  ;
    return (
      <div id="notification-table-container">  
        <h2 className="netsense__table__title" style={{marginTop:"-4px"}}>Notifications {AddbuttonVisibility}
            {/*<span onClick={()=>this.handleOpen()}*/}
                  {/*className="rubix-icon icon-nargela-align-right" style={{display:"inline-block", cursor:"pointer", float:"right"}}></span>*/}
        </h2>
        <Select id="activeNotification" style={{width: '10%', marginLeft: "30px"}} onChange={this.handleOnChange}>
            <option value= "Active">Active</option>
            <option value= "Inactive">Inactive</option>
        </Select>    
          {/*{*/}
              {/*this.state.showListEditor ?*/}
                  {/*<div className="ns-list-editor">*/}
                      {/*<Listeditor show={this.state.showListEditor}*/}
                                  {/*component={this.state.component}*/}
                                  {/*componentColumns ={this.state.componentColumns}*/}
                                  {/*columns={*/}
                                      {/*this.state.notificationStoredWidths.map(function(column, index){*/}
                                          {/*if(column.name === "Type"){*/}
                                              {/*return(*/}
                                              {/*{name:column.name,field:column.field,id: column.id, checked: column.checked, required: column.required,width: column.width,*/}
                                                  {/*sortable:column.sortable,*/}
                                                  {/*formatter: function(row, cell, value) {*/}
                                                      {/*return '<span title="' + value + '">' + value + '</span>';*/}
                                                  {/*}*/}
                                              {/*}*/}
                                              {/*)*/}
                                          {/*}else{*/}
                                              {/*return(*/}
                                              {/*{*/}
                                                  {/*name:column.name,field:column.field, id: column.id,checked: column.checked,required: column.required,*/}
                                                  {/*width: column.width, sortable:column.sortable,*/}
                                              {/*}*/}
                                              {/*)*/}
                                          {/*}*/}
                                      {/*})*/}
                                  {/*}/>*/}
                  {/*</div>*/}
                  {/*:null*/}
          {/*}*/}
          <div id="active-notification-grid">
          <DataGrid component="Notification"
                  dataArray={this.props.activeNotifications}
                  dataID={this.props.notificationID} 
                  dataIdField="notificationid"
                  componentColumns ={this.state.componentColumns}
                  options={{gridHeight:{windowPct:50,offset:-120}}}
                  style={{display: "none"}}
                  columns={
                      this.state.notificationStoredWidths.map(function(column, index){
                         
                          if(column.name === "Type"){
                              return(
                              {name:column.name,field:column.field,id: column.id, checked: column.checked, required: column.required,width: column.width,
                                  sortable:column.sortable,
                                  formatter: function(row, cell, value) {
                                      
                                      return '<span title="' + value + '">' + value + '</span>';
                                  }
                              }
                              )
                          }
                          else if(column.name === "Action"){
                            return(
                            {name:column.name,field:column.field,id: column.id, checked: column.checked, required: column.required,width: column.width,
                                sortable:column.sortable,
                                formatter:function(row) {
                                    var deactiveID = that.props.activeNotifications[row]== undefined? " ":that.props.activeNotifications[row];
                                    //var event = "ReactBootstrap.Dispatcher.emit('Notificationlist.deactivate','"+JSON.stringify(deactiveID)+"')";
                                    //var nobj = JSON.stringify(deactiveID);
                                    //console.log("nobj", nobj);
                                    var event = "ReactBootstrap.Dispatcher.emit('Notificationlist.deactivate', '"+deactiveID.notificationid+"', '"+deactiveID.idx+"')";
                                    return '<button id="deactivate" style="positon:relative; top:-10px; " title="Deactivate" class="ns-form-btn" onclick="' + event + '"><b>Deactivate</b></button>';
                                }
                            }
                            )
                        }
                          else{
                              return(
                              {
                                  name:column.name,field:column.field, id: column.id,checked: column.checked,required: column.required,
                                  width: column.width, sortable:column.sortable,
                              }
                              )
                          }
                      })
                  } />
                  </div>
                  <div id="inactive-notification-grid" >
          <DataGrid component="Notification-inactive"
                  dataArray={this.props.inactiveNotifications}
                  dataID={this.props.notificationID} 
                  dataIdField="notificationid"
                  componentColumns ={this.state.componentColumns}
                  options={{gridHeight:{windowPct:90,offset:-112}}}
                  style={{display: "none"}}
                  columns={
                      this.state.notificationStoredWidths.map(function(column, index){
                          if(column.name === "Type"){
                              return(
                              {name:column.name,field:column.field,id: column.id, checked: column.checked, required: column.required,width: column.width,
                                  sortable:column.sortable,
                                  formatter: function(row, cell, value) {
                                      return '<span title="' + value + '">' + value + '</span>';
                                  }
                              }
                              )
                          }
                          else if(column.name === "Action"){
                            return(
                            {name:column.name,field:column.field,id: column.id, checked: column.checked, required: column.required,width: column.width,
                                sortable:column.sortable,
                                formatter:function(row) {
                                    var activeID = that.props.inactiveNotifications[row]== undefined? " ":that.props.inactiveNotifications[row];
                                   // console.log("this in formatter active", activeID)
                                    var event = "ReactBootstrap.Dispatcher.emit('Notification-inactivelist.activate','"+activeID.notificationid+"', '"+activeID.idx+"')";
                                    return '<button id="activate" style="positon:relative; top:-10px; " title="Activate" class="ns-form-btn" onclick="' + event + '"><b>Activate</b></button>';
                                }
                            }
                            )
                        }
                          else{
                              return(
                              {
                                  name:column.name,field:column.field, id: column.id,checked: column.checked,required: column.required,
                                  width: column.width, sortable:column.sortable,
                              }
                              )
                          }
                      })
                  } />
                  </div>
      </div>
    );
 
  }
}
);

module.exports = Notificationlist;



