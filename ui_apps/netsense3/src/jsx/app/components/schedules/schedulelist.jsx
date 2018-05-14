import classNames from 'classnames';
import auth from 'global/utils/auth';

import { State, Navigation } from 'react-router';
import DataGrid from 'components/datagrid';
import Listeditor  from 'components/listeditor';

var Schedulelist = React.createClass({

    getInitialState: function(){
        var component = "Schedule";
        var userRole = NSN.userInfo.name;
        var componentColumns = "ScheduleColumnWidths_"+ userRole;
        var scheduleStoredWidths = [];

        if(localStorage.getItem(componentColumns)){
            scheduleStoredWidths = JSON.parse(localStorage.getItem(componentColumns));
        }else{
            // default widths
            scheduleStoredWidths =[
                {name:"Name", field:"name", id:"name",sortable:true, width:100, checked:true, required:true },
                {name:"Description", field:"description", id:"description", sortable:true, width:150, checked:true, required:false }
            ]
        }
        return {
            showListEditor:false,
            scheduleStoredWidths:scheduleStoredWidths,
            componentColumns:componentColumns,
            component:component,
        }
    },

  propTypes: {
    schedules: React.PropTypes.array.isRequired,
    scheduleID: React.PropTypes.string
  },

  handleAdd() {
    ReactBootstrap.Dispatcher.emit("Schedulelist.add", "-1");
  },

    shouldComponentUpdate: function (nextProps, nextState) {
        return true;
    },

    toggleListEditor : function(){
        this.setState({showListEditor:!this.state.showListEditor});
    },

  render() {      
    // var Addbutton = (auth.allowed('CAN_CREATE','ScheduleModel'))
    //   (
    //     <span style={{display:"inline-block",position:"relative",fontSize:"30px",color:"#3c3",cursor:"pointer",zIndex:"99999"}} className="pulse" title="Add Schedule">
    //       <Icon glyph="icon-fontello-plus-circle" onClick={this.handleAdd} />
    //     </span>
    //   );


        var Addbutton = (<span></span>);
        if (auth.allowed('CAN_CREATE', 'ScheduleModel')) {
            Addbutton = (
                <button id="addSchedule" title="Add Schedule" className="ns-big-btn" onClick={this.handleAdd}><b>Add schedule</b></button>
            );
        };


      return (
      <div id="schedule-table-container">
        <h2 className="netsense__table__title" style={{marginTop:"-4px"}}>Schedules {Addbutton}
            <span onClick={()=>this.toggleListEditor()}
                  className="ns-filter-icon"></span>
        </h2>

          {
              this.state.showListEditor ?
                  <div className="ns-list-editor">
                      <Listeditor show={this.state.showListEditor}
                                  component={this.state.component}
                                  handleToggle = {this.toggleListEditor}
                                  componentColumns ={this.state.componentColumns}
                                  columns={
                                      this.state.scheduleStoredWidths.map(function(column, index){
                                          if(column.name === "Name"){
                                              return(
                                              {name:column.name,field:column.field,id: column.id, checked: column.checked, required: column.required,width: column.width,
                                                  sortable:column.sortable, 
                                                  formatter: function(row, cell, value, columnDef, dataContext) {
                                                      var siteIcon = "";
                                                      if (dataContext.sites.length > 0) {
                                                          siteIcon = '<span style="font-size:16px;margin-left:6px;" title="Site Default Schedule" '
                                                              + ' class="rubix-icon icon-fontello-commerical-building"></span>';
                                                      };
                                                      return (value + siteIcon);
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

        <DataGrid component="Schedule" 
                  dataArray={this.props.schedules}
                  dataID={this.props.scheduleID} 
                  dataIdField="scheduleid"
                  columns={
                      this.state.scheduleStoredWidths.map(function(column, index){
                          if(column.name === "Name"){
                              return(
                              {name:column.name,field:column.field,id: column.id, checked: column.checked, required: column.required,width: column.width,
                                  sortable:column.sortable, 
                                  formatter: function(row, cell, value, columnDef, dataContext) {
                                      var siteIcon = "";
                                      if (dataContext.sites.length > 0) {
                                          siteIcon = '<span style="font-size:16px;margin-left:6px;" title="Site Default Schedule" '
                                              + ' class="rubix-icon icon-fontello-commerical-building"></span>';
                                      };
                                      return (value + siteIcon);
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
      ); 
  }
});

module.exports = Schedulelist;

