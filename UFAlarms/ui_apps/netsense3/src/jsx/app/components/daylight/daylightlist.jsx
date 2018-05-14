import classNames from 'classnames';
import auth from 'global/utils/auth';
import { State, Navigation } from 'react-router';
import DataGrid from 'components/datagrid';
import Listeditor  from 'components/listeditor'

var Daylightlist = React.createClass({

    getInitialState: function(){
        var component = "Daylight";
        var userRole = NSN.userInfo.name;
        var componentColumns = "DaylightColumnWidths_"+ userRole;
        var daylightStoredWidths = [];
        if(localStorage.getItem(componentColumns)){
            daylightStoredWidths = JSON.parse(localStorage.getItem(componentColumns));
        }else{
            // default widths
            daylightStoredWidths = [
                {name:"Name", field:"name", id:"name",sortable:true, width:100, checked:true, required:true},
                {name:"Description", field:"description", id:"description",sortable:true, width:150, checked:true, required:true},
                {name:"Groups", field:"groups", id:"groups",sortable:true, width:100, checked:true, required:false},
                {name:"Nodes", field:"nodes", id:"nodes",sortable:true, width:100, checked:true, required:false}
            ]
        }

        return {
            showListEditor:false,
            daylightStoredWidths:daylightStoredWidths,
            componentColumns:componentColumns,
            component:component
        }
    },

  propTypes: {
    daylights: React.PropTypes.array.isRequired,
    daylightID: React.PropTypes.string
  },

    toggleListEditor : function(){
        this.setState({showListEditor:!this.state.showListEditor});
    },

    shouldComponentUpdate: function (nextProps, nextState) {
        return true;
    },


    handleAdd: function() {
    ReactBootstrap.Dispatcher.emit("Daylightlist.add");
  },
 
  render() {
    var Addbutton = (<span></span>);
    if (auth.allowed('CAN_CREATE','ScheduleModel')) {
       Addbutton = (
        <button title="Add Daylight Harvesting profile" className="ns-big-btn" id="addDh" onClick={this.handleAdd}><b>Add profile</b></button>
        );
    };

    return (
      <div id="daylight-table-container">
        <h2 className="netsense__table__title" style={{marginTop:"-4px"}}>Daylight Harvesting {Addbutton}
            <span onClick={()=>this.toggleListEditor()} className="rubix-icon icon-nargela-align-right"
                  className="ns-filter-icon"></span>
        </h2>
          {
              this.state.showListEditor ?
                  <div className="ns-list-editor">
                      <Listeditor show={this.state.showListEditor}
                                  component={this.state.component}
                                  componentColumns ={this.state.componentColumns}
                                  handleToggle = {this.toggleListEditor}
                                  columns={
                                    this.state.daylightStoredWidths.map(function(column, index){
                                        if(column.name ==="Groups" ){
                                            return(
                                            {
                                                name:column.name,field:column.field, id: column.id,checked: column.checked,required: column.required,
                                                width: column.width, sortable:column.sortable,
                                                formatter: function(row, cell, value, columnDef, dataContext) {
                                                  var groupList = "";
                                                  if (typeof value !== 'undefined') {
                                                    for(var i=0;i<value.length;i++){
                                                      if(i === value.length-1){
                                                        groupList += value[i].name;
                                                      }
                                                      else{
                                                        groupList += value[i].name + ",";
                                                      }
                                                    }
                                                  }
                                                  return groupList;
                                                }
                                            }
                                            )
                                          }
                                          else if(column.name ==="Nodes" ){
                                            return(
                                            {
                                                name:column.name,field:column.field, id: column.id,checked: column.checked,required: column.required,
                                                width: column.width, sortable:column.sortable,
                                                formatter: function(row, cell, value, columnDef, dataContext) {
                                                  var nodeList = "";
                                                  if (typeof value !== 'undefined') {
                                                    for(var i=0;i<value.length;i++){
                                                      if(i === value.length-1){
                                                        nodeList += value[i].nodeid;
                                                      }
                                                      else{
                                                        nodeList += value[i].nodeid + ",";
                                                      }
                                                    }
                                                  }
                                                  return nodeList;
                                                }
                                            }
                                            )
                                        }else{
                                            return({
                                                name:column.name,field:column.field,id: column.id, checked: column.checked, required: column.required,width: column.width,
                                                sortable:column.sortable
                                            })
                                        }
                                    })
                                  }/>
                  </div>:
                  null
          }

          <DataGrid component="Daylight"
                  componentName="Profile"
                  dataArray={this.props.daylights}
                  dataID={this.props.daylightID} 
                  dataIdField="etdhprofileid"
                  columns={
                  this.state.daylightStoredWidths.map(function(column, index){
                      if(column.name ==="Groups" ){
                          return(
                          {
                              name:column.name,field:column.field, id: column.id,checked: column.checked,required: column.required,
                              width: column.width, sortable:column.sortable,
                              formatter: function(row, cell, value, columnDef, dataContext) {
                                var groupList = "";
                                if (typeof value !== 'undefined') {
                                  for(var i=0;i<value.length;i++){
                                    if(i === value.length-1){
                                      groupList += value[i].name;
                                    }
                                    else{
                                      groupList += value[i].name + ",";
                                    }
                                  }
                                }
                                return groupList;
                              }
                          }
                          )
                        }
                        else if(column.name ==="Nodes" ){
                          return(
                          {
                              name:column.name,field:column.field, id: column.id,checked: column.checked,required: column.required,
                              width: column.width, sortable:column.sortable,
                              formatter: function(row, cell, value, columnDef, dataContext) {
                                var nodeList = "";
                                if (typeof value !== 'undefined') {
                                  for(var i=0;i<value.length;i++){
                                    if(i === value.length-1){
                                      nodeList += value[i].nodeid;
                                    }
                                    else{
                                      nodeList += value[i].nodeid + ",";
                                    }
                                  }
                                }
                                return nodeList;
                              }
                          }
                          )
                      }else{
                          return({
                              name:column.name,field:column.field,id: column.id, checked: column.checked, required: column.required,width: column.width,
                              sortable:column.sortable
                          })
                      }
                  })
                  } />
      </div>
      );
  }
});

module.exports = Daylightlist;

