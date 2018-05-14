import classNames from 'classnames';
import auth from 'global/utils/auth';
import { State, Navigation } from 'react-router';
import DataGrid from 'components/datagrid';
import Listeditor  from 'components/listeditor';
import helpers from 'global/utils/helpers';

var Customerlist = React.createClass({

  getInitialState: function(){
     var component = "Customer";
     var userRole = NSN.userInfo.name;
     var componentColumns = "CustomerColumnWidths_"+ userRole;
     var customerStoredWidths = [];
     var height = "auto";

     customerStoredWidths = helpers.getColumnSettings(componentColumns,
     	[
             {id:"edit",name:"Edit",field:"edit", width:30, maxWidth: 75, checked:true,required:true, disableSearch:true},
             {id:"name",name:"Name",field:"name",  width:80,   checked:true,required:true, sortable:true},
             {id:"type",name:"Type", field:"type", width:30,  checked:true, required:false, sortable:true},
             {id:"partnername",name:"Partner Name", field:"partnername", width:60,  checked:true,required:false, sortable:true},
             {id:"city",name:"City",field:"city",  width:40,  checked:true,required:false, sortable:true },
             {id:"state",name:"State", field:"state",  width:50,  checked:true,required:false, sortable:true},
             {id:"orgid",name:"Sites",field:"orgid", width:10, checked:true,required:true, disableSearch:true}
         ]
     );
     return {
         showListEditor:false,
         customerStoredWidths:customerStoredWidths,
         componentColumns:componentColumns,
         component:component,
         height:height
     }
  },

  propTypes: {
    customers: React.PropTypes.array.isRequired,
    customerID: React.PropTypes.string.isRequired,
    selectedCustomer: React.PropTypes.string.isRequired
  },

  handleAdd: function () {
    ReactBootstrap.Dispatcher.emit("Customerlist.add");
  },

  shouldComponentUpdate: function (nextProps, nextState) {
    // This component does not need to be rerendered unless customers have been added or changed
    // return this.props.customers !== nextProps.customers;
      return true;
  },

  toggleListEditor : function(){
     this.setState({showListEditor:!this.state.showListEditor});
  },

  render() {


    var custOrg = this.props.selectedCustomer==null?"":this.props.selectedCustomer.orgid;
    var Addbutton = (<span></span>);
    if (auth.allowed('CAN_CREATE', 'OrgModel')) {
      Addbutton = (
        <button id="addCustomer" className="ns-big-btn" onClick={this.handleAdd}><b>Add account</b></button>
      );
    };

    return (
      <div id="customer-table-container">
        <h2 className="netsense__table__title">Accounts {Addbutton}
            <span onClick={()=>this.toggleListEditor()}
                 className="ns-filter-icon"></span>
        </h2>
           {
              this.state.showListEditor ?
                  <div className="ns-list-editor">
                      <Listeditor show={this.state.showListEditor}
                            component={this.state.component}
                            componentColumns ={this.state.componentColumns}
                            handleToggle = {this.toggleListEditor}
                            height = {this.state.height}
                            columns={
                                this.state.customerStoredWidths.map(function(column, index){
                                    if(column.name === "Sites"){
                                         return(
                                              {name:column.name,field:column.field,id: column.id, checked: column.checked, required: column.required,width: column.width,
                                                  disableSearch: true, cssClass:"text-center" , headerCssClass:"text-center",
                                                  formatter:function(row, cell, value, columnDef, dataContext) {
                                                      var event = "ReactBootstrap.Dispatcher.emit('Customerlist.selectSites','" + value + "')";
                                                      return '<div onclick="' + event + '"><span title="Go to Sites" '
                                                          + ' class="ns-customer-site-icon"></span></div>';
                                                  }
                                              }
                                         )
                                    }else if(column.name === "Edit"){
                                        return(
                                        {name:column.name,field:column.field,id: column.id, checked: column.checked, required: column.required, width: column.width, maxWidth: column.maxWidth,
                                            disableSearch: true, cssClass:"text-center" ,
                                            formatter:function() {
                                                var event = "ReactBootstrap.Dispatcher.emit('Customerlist.editCustomer',"+custOrg+")";
                                                return '<div class="ns-customer-edit-icon" style="cursor:pointer" onclick="' + event + '"></div>';
                                            }
                                        }
                                        )
                                    }else{
                                          return(
                                              {
                                                  name:column.name,field:column.field, id: column.id,checked: column.checked,required: column.required,
                                                  width: column.width, sortable:column.sortable,
                                              }
                                          )
                                    }
                                })
                      }/>
                  </div>
              :null
            }

            {/* New DATA GRID for New designs*/}
           <DataGrid component={this.state.component}
               componentName="Account"
               dataArray={this.props.customers}
               dataID={this.props.customerID}
               dataIdField="orgid"
               componentColumns ={this.state.componentColumns}
               columns={
 					this.state.customerStoredWidths.map(function(column, index){
                       if(column.name === "Sites"){
                           return(
                                {name:column.name,field:column.field,id: column.id, checked: column.checked, required: column.required,width: column.width,
                                    disableSearch: true, cssClass:"text-center" , headerCssClass:"text-center",
                                    formatter:function(row, cell, value, columnDef, dataContext) {
                                        var event = "ReactBootstrap.Dispatcher.emit('Customerlist.selectSites','" + value + "')";
                                        return '<div><img style="cursor:pointer" onclick="' + event + '"src="/imgs/new-icons/site.svg" height="20" title="View Sites" /></div>';
                                    }
                                }
                           )
                       }else if(column.name === "Edit"){ 
                           console.log("custorg",custOrg);
                           return(
                               {name:column.name,field:column.field,id: column.id, checked: column.checked, required: column.required, width: column.width, maxWidth: column.maxWidth,
                                   sortable:column.sortable, disableSearch: true, cssClass:"text-center" ,
                                   formatter:function() {
                                       var event = "ReactBootstrap.Dispatcher.emit('Customerlist.editCustomer',"+custOrg+")";
                                       return '<div class="ns-customer-edit-icon" style="cursor:pointer" onclick="' + event + '"></div>';
                                   }
                               }
                           )
                       }else{
                           return(
                                {name:column.name,field:column.field, id: column.id,checked: column.checked,required: column.required,
                                    width: column.width, sortable:column.sortable}
                           )
                       }
                   })
           } />
      </div>
    );
  }
});

module.exports = Customerlist;