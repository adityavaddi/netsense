import classNames from 'classnames';
import auth from 'global/utils/auth';
import { State, Navigation } from 'react-router';
import DataGrid from 'components/datagrid';
import Listeditor  from 'components/listeditor';

var Grouplist = React.createClass({

    getInitialState: function(){
        var component = "Group";
        var userRole = NSN.userInfo.name;
        var componentColumns = "GroupColumnWidths_"+ userRole;
        var groupStoredWidths = [];

        if(localStorage.getItem(componentColumns)){
            groupStoredWidths = JSON.parse(localStorage.getItem(componentColumns));
        }else{
            // default widths
            groupStoredWidths = [
                {name:"Type", field:"type", id:"type",sortable:true, width:40, cssClass:"text-center", headerCssClass:"text-center", required:false, checked:true},
                {name:"Name", field:"name", id:"name",sortable:true, width:100, required:true, checked:true},
                {name:"Description", field:"description", id:"description",sortable:true, width:150, required:false, checked:true}
            ]
        }
        return {
            showListEditor:false,
            groupStoredWidths:groupStoredWidths,
            componentColumns:componentColumns,
            component:component,
        }
    },

  propTypes: {
    groups: React.PropTypes.array.isRequired,
    groupID: React.PropTypes.string,
    minmax: React.PropTypes.string,
    detail_state: React.PropTypes.string,
    ui_state: React.PropTypes.object
  },

    maximize: function () {
        if (this.props.ui_state.detail == "pinned") {
            ReactBootstrap.Dispatcher.emit('Groupdetail.togglePin');
        }
        $("#group-list-panel").data("state", "open").css({ width: "100%" });
        $(window).trigger('resize');
    },

    minimize: function() {
    $("#group-list-panel").data("state","closed").css({width:"33%"});
    $(window).trigger('resize');
  },

  togglegrid: function() {
    if ($("#group-list-panel").data("state") == "closed") {
      this.maximize();
    } else {
      this.minimize();
    }
  },

  handleAdd: function() {
    ReactBootstrap.Dispatcher.emit("Grouplist.add");
    $("#Group-grid").data("gridInstance").setSelectedRows([])
  },

  toggleListEditor : function(){
    this.setState({showListEditor:!this.state.showListEditor});
  },

    toggleDetail: function () {
        if ($("#group-list-panel").data("state") == "open") {
            this.minimize();
            ReactBootstrap.Dispatcher.emit('Grouplist.toggleDetail');
        } else {
            ReactBootstrap.Dispatcher.emit("Grouplist.toggleDetail");
        }
    },

  shouldComponentUpdate: function(nextProps,nextState){
    // This component does not need to be rerendered unless customers have been added or changed
    // return this.props.groups !== nextProps.groups;
      return true;
  },

  render() {
    var Addbutton = (<span></span>);
    if (auth.allowed('CAN_CREATE','GroupModel')) {
       Addbutton = (
        <button id="add-group" onClick={this.handleAdd} className="ns-med-btn" title="Add Group">
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
    <div style={{height:"100%"}}>
       {Minmaxlist}
        <h2 className="netsense__map__table__title">Groups {Addbutton}
            <span onClick={()=>this.toggleListEditor()}
                  className="ns-map-filter-icon"></span>
        </h2>

          {
              this.state.showListEditor ?
                  <div className="ns-list-editor" style={{top:"119px !important"}}>
                      <Listeditor show={this.state.showListEditor}
                                  component={this.state.component}
                                  handleToggle = {this.toggleListEditor}
                                  componentColumns ={this.state.componentColumns}
                                  columns={
                                      this.state.groupStoredWidths.map(function(column, index){
                                          if(column.name === "Type"){
                                              return(
                                              {name:column.name,field:column.field,id: column.id, checked: column.checked, required: column.required,width: column.width,
                                                  sortable:column.sortable, disableSearch: true, cssClass:"text-center" , headerCssClass:"text-center",
                                                  formatter: function(row, cell, value, columnDef, dataContext) {
                                                      if (value == "organizational") {
                                                          return "<img src='/imgs/new-icons/orggroup-icon.svg' width='18' title='Organizational Group' />"
                                                      } else {
                                                          return "<img src='/imgs/new-icons/lightinggroup-icon.svg' width='18' title='Lighting Group' />"
                                                      };
                                                  }
                                              }
                                              )
                                          }else if(column.name ==="Name" ){
                                              return(
                                              {
                                                  name:column.name,field:column.field, id: column.id,checked: column.checked,required: column.required,
                                                  width: column.width, sortable:column.sortable,
                                                  formatter: function(row, cell, value, columnDef, dataContext) {
                                                      var siteIcon = "";
                                                      if (dataContext.type == "site-lighting") {
                                                          siteIcon = '<span style="font-size:16px;margin-left:6px;" title="Site Default Lighting Group" '
                                                              + ' class="rubix-icon icon-fontello-commerical-building"></span>';
                                                      };
                                                      return (value + siteIcon);
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
                  </div>
                  :null
          }

        <div>
          <DataGrid component="Group"
                  dataArray={this.props.groups}
                  dataID={this.props.groupID}
                  options={{gridHeight: { windowPct: 100, offset: -180 }}}
                  componentColumns ={this.state.componentColumns}
                  dataIdField="groupid"
                  columns={
                      this.state.groupStoredWidths.map(function(column, index){
                      if(column.name === "Type"){
                          return(
                          {name:column.name,field:column.field,id: column.id, checked: column.checked, required: column.required,width: column.width,
                              sortable:column.sortable, disableSearch: true, cssClass:"text-center" , headerCssClass:"text-center",
                              formatter: function(row, cell, value, columnDef, dataContext) {
                                  if (value == "organizational") {
                                      return "<img src='/imgs/new-icons/orggroup-icon.svg' width='18' title='Organizational Group' />"
                                  } else {
                                      return "<img src='/imgs/new-icons/lightinggroup-icon.svg' width='18' title='Lighting Group' />"
                                  };
                              }
                          }
                          )
                      }else if(column.name ==="Name" ){
                          return(
                          {
                              name:column.name,field:column.field, id: column.id,checked: column.checked,required: column.required,
                              width: column.width, sortable:column.sortable,
                              formatter: function(row, cell, value, columnDef, dataContext) {
                                  var siteIcon = "";
                                  if (dataContext.type == "site-lighting") {
                                      siteIcon = '<span style="font-size:16px;margin-left:6px;" title="Site Default Lighting Group" '
                                          + ' class="rubix-icon icon-fontello-commerical-building"></span>';
                                  };
                                  return (value + siteIcon);
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
      <div style={{textAlign:"center"}}>
            <button id="showDetailsGroups"className="ns-big-btn" style={{float:"none",width:"300px !important"}} onClick={this.toggleDetail}>{
                this.props.detail_state=="hidden" 
                ?<b>Show/Edit Details</b>
                :<b>Hide Details</b>
                }</button>
      </div>
      </div>
      );
  }
});

module.exports = Grouplist;