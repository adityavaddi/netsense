import classNames from 'classnames';
import auth from 'global/utils/auth';
import { State, Navigation } from 'react-router';
import DataGrid from 'components/datagrid';
import Listeditor  from 'components/listeditor';

var Parkinggrouplist = React.createClass({
    getInitialState: function(){
        var component = "Parkinggroup";
        var userRole = NSN.userInfo.name;
        var componentColumns = "ParkinggroupColumnWidths_"+ userRole;
        var parkingGroupStoredWidths = [];

        if(localStorage.getItem(componentColumns)){
            parkingGroupStoredWidths = JSON.parse(localStorage.getItem(componentColumns));
           for(var a in parkingGroupStoredWidths){
               if(parkingGroupStoredWidths[a].name === "Policy"){
	               parkingGroupStoredWidths.splice(a, 1);
               }
           }
        }else{
            // default widths
            parkingGroupStoredWidths = [
                {name:"Name", field:"name", id:"name",sortable:true, width:100, checked:true, required:true},
                {name:"Description", field:"description", id:"description", sortable:true, width:150, checked:true, required:false},
                {name:"Type", field:"vehicle_types", id:"vehicle_types", sortable:true, width:70, minWidth:70, checked:true, required:false},
                {name:"Zones", field:"parkingzones", id:"parkingzones", cssClass:"text-center", headerCssClass:"gridHeader",
                    sortable:true, width:60, checked:true, required:true}
            ]
        }
        return {
            showListEditor:false,
            parkingGroupStoredWidths:parkingGroupStoredWidths,
            componentColumns:componentColumns,
            component:component,
        }
    },
  propTypes: {
    parkinggroups: React.PropTypes.array.isRequired,
    parkinggroupID: React.PropTypes.string,
    minmax: React.PropTypes.string,
    detail_state: React.PropTypes.string,
    ui_state: React.PropTypes.object
  },
  
    maximize: function () {
        if (this.props.ui_state.detail == "pinned") {
            ReactBootstrap.Dispatcher.emit('Parkinggroupdetail.togglePin');
        }
        $("#parkinggroup-list-panel").data("state", "open").css({ width: "100%" });
        $(window).trigger('resize');
    },

  minimize: function() {
    $("#parkinggroup-list-panel").data("state","closed").css({width:"33%"});
    $(window).trigger('resize');
  },

  togglegrid: function() {
    if ($("#parkinggroup-list-panel").data("state") == "closed") {
      this.maximize();
    } else {
      this.minimize();
    }
  },

  handleAdd: function() {
    ReactBootstrap.Dispatcher.emit("Parkinggrouplist.add");
  },

  toggleListEditor : function(){
        this.setState({showListEditor:!this.state.showListEditor});
    },

    toggleDetail: function () {
        if ($("#parkinggroup-list-panel").data("state") == "open") {
            this.minimize();
            ReactBootstrap.Dispatcher.emit("Parkinggrouplist.toggleDetail");
        } else {
            ReactBootstrap.Dispatcher.emit("Parkinggrouplist.toggleDetail");
        }
    },

  shouldComponentUpdate: function(nextProps,nextState){
    // This component does not need to be rerendered unless parkinggroups have been added or changed
    // return this.props.parkinggroups !== nextProps.parkinggroups;
      return true;
  },

  render() {
    var Addbutton = (<span></span>);
    if (auth.allowed('CAN_CREATE','ParkingZoneModel')) {
       Addbutton = (
        <button id="add-parkinggroup" onClick={this.handleAdd} className="ns-med-btn" title="Add Parking group profile">
          <b>Add group</b></button>);
    };

    var Minmaxlist = (<div></div>);
    if (this.props.minmax) {
      //var glyph = (this.props.minmax === "expand")?"icon-fontello-step-backward":"icon-fontello-resize-horizontal";
      var glyph = "icon-fontello-resize-horizontal";
      var handler = this.togglegrid;
      Minmaxlist = (
        <div style={{position:"absolute",cursor:"pointer",top:"-5px",right:"33px",height:"20px",width:"20px",fontSize:"28px"}}
              onClick={handler} title="Toggle Full-Screen Table">
          <Icon bundle={this.props.bundle} glyph={glyph} />
        </div> );
      };

        return (
            <div style={{ height: "100%" }}>
                {Minmaxlist}
                <h2 className="netsense__map__table__title" style={{paddingLeft:"22px !important"}}>Parking Groups {Addbutton}
                     <span onClick={() => this.toggleListEditor()}
                        className="ns-map-filter-icon" style={{top:"8px !important", left:"68% !important"}}></span>
                </h2>

        {
            this.state.showListEditor ?
                <div className="ns-list-editor" style={{top:"76px !important", right:"140px !important"}}>
                    <Listeditor show={this.state.showListEditor}
                                component={this.state.component}
                                handleToggle = {this.toggleListEditor}
                                componentColumns ={this.state.componentColumns}
                                columns={
                                    this.state.parkingGroupStoredWidths.map(function(column, index){
                                        if(column.name === "Zones"){
                                            return(
                                            {name:column.name,field:column.field,id: column.id, checked: column.checked, required: column.required,width: column.width,
                                                sortable:column.sortable, disableSearch: true, cssClass:"text-center" , headerCssClass:"text-center",
                                                formatter: function(row, cell, value) {
                                                    if (value == "") return 0;
                                                    return value.split(",").length;
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

      <div id="parkinggroup-table-container">
        <DataGrid component={this.state.component}
                  componentName="Parking Group"
                  dataArray={this.props.parkinggroups}
                  dataID={this.props.parkinggroupID}
                  options={{gridHeight: { windowPct: 100, offset: -180 }}}
                  dataIdField="parkinggroupid"
                  match="contains"
                  componentColumns ={this.state.componentColumns}
                  columns={
                      this.state.parkingGroupStoredWidths.map(function(column, index){
                          if(column.name === "Zones"){
                              return(
                              {name:column.name,field:column.field,id: column.id, checked: column.checked, required: column.required,width: column.width,
                                  sortable:column.sortable, disableSearch: true, cssClass:"text-center" , headerCssClass:"text-center",
                                  formatter: function(row, cell, value) {
                                      if (value == "") return 0;
                                      return value.split(",").length;
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
      <div style={{textAlign:"center"}}>
            <button id="showDetailsParkingGroups" className="ns-big-btn" style={{float:"none",width:"300px !important"}} onClick={this.toggleDetail}>{
                this.props.detail_state=="hidden"
                ?<b>Show/Edit Details</b>
                :<b>Hide Details</b>
              }</button>
      </div>
      </div>
      );
  }
});

module.exports = Parkinggrouplist;