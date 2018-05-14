import classNames from 'classnames';
import auth from 'global/utils/auth';
import { State, Navigation } from 'react-router';
import DataGrid from 'components/datagrid';

import Listeditor  from 'components/listeditor';

var Parkingspacelist = React.createClass({
    getInitialState: function(){
         var component = "Parkingspacelist";
         var userRole = NSN.userInfo.name;
         var componentColumns = "ParkingspaceColumnWidths_"+ userRole;
         var parkingspaceStoredWidths = [];

         if(localStorage.getItem(componentColumns)){
             parkingspaceStoredWidths = JSON.parse(localStorage.getItem(componentColumns));
             for(var i =0; i<parkingspaceStoredWidths.length; i++){

                 if(parkingspaceStoredWidths[i].id === "_checkbox_selector"){
                     parkingspaceStoredWidths.splice(i,1 );
                 }
             }
         }else{
             // default widths
             parkingspaceStoredWidths = [
                 {name:"Space name", field:"name", id:"name", sortable:true, width:200, checked:true, required:false},
                 {name:"Group", field:"parkinggroupname", id:"parkinggroupname",sortable:true, width:150, checked:true, required:false},
                 {name:"Space Type", field:"demarcated", id:"demarcated", width:120, checked:true, required:false}
             ]
         }
         return {
             showListEditor:false,
             parkingspaceStoredWidths:parkingspaceStoredWidths,
             componentColumns:componentColumns,
             component:component
         }
     },
  propTypes: {
    parkingspaces:React.PropTypes.array.isRequired,
    parkingspacesmetadata:React.PropTypes.array.isRequired,
    parkingspaceID: React.PropTypes.string,
    minmax: React.PropTypes.string,
    detail_state: React.PropTypes.string,
    activeFilter: React.PropTypes.string,
    ui_state: React.PropTypes.object,

  },
  
  maximize: function() {
    $("#parkingspace-list-panel").data("state","open").css({width:"100%", zIndex: "401"});
    $(window).trigger('resize');
  },

  minimize: function() {
    $("#parkingspace-list-panel").data("state","closed").css({width:"33%", zIndex: "300"});
    $(window).trigger('resize');
  },

  togglegrid: function() {
    if ($("#parkingspace-list-panel").data("state") == "closed") {
      this.maximize();
    } else {
      this.minimize();
    }
  },

    toggleListEditor: function () {
        this.setState({ showListEditor: !this.state.showListEditor });
    },

    toggleDetail: function () {
        if ($("#parkingspace-list-panel").data("state") == "open") {
            this.minimize();
            ReactBootstrap.Dispatcher.emit("Parkingspacelist.toggleDetail");
        } else {
            ReactBootstrap.Dispatcher.emit("Parkingspacelist.toggleDetail");
        }
    },

    // displaySpaceAttribute:function

  shouldComponentUpdate: function(nextProps,nextState){
    // This component does not need to be rerendered unless parkingzones have been added or changed
   // return this.props.parkingzones !== nextProps.parkingzones;
   return this.props.activeFilter = true;
  },
 
  render() {

    var Minmaxlist = (<div></div>);
    if (this.props.minmax) {
      //var glyph = (this.props.minmax === "expand")?"icon-fontello-step-backward":"icon-fontello-resize-horizontal";
      var glyph = "icon-fontello-resize-horizontal";
      var handler = this.togglegrid;
      Minmaxlist = (
        <div style={{position:"absolute",cursor:"pointer",top:"-5px",right:"33px",height:"20px",width:"20px",fontSize:"28px"}}
          onClick={handler} title="Toggle Full-Screen Table">
          <Icon bundle={this.props.bundle} glyph={glyph} />
        </div>);
     };

    var allParkingSpaces = this.props.parkingspaces;
    for(var i in  allParkingSpaces){
        if(allParkingSpaces[i].parkinggroupname ==="No Group"){
	        allParkingSpaces[i].parkinggroupname = " ";
        }
    }

    return (
      <div style={{ height: "100%" }} >
        {Minmaxlist}
        <h2 className="netsense__map__table__title">Space management
                </h2>
          {
              this.state.showListEditor ?
                  <div className="ns-list-editor">
                      <Listeditor show={this.state.showListEditor}
                                  component={this.state.component}
                                  handleToggle = {this.toggleListEditor}
                                  componentColumns ={this.state.componentColumns}
                                  columns={this.state.parkingspaceStoredWidths}/>
                  </div>:
                  null
          }
          <div id="parkingspace-table-container">
            <DataGrid component="Parkingspace"
                      componentName="Parking Space"
                      dataArray={allParkingSpaces}
                      dataID={this.props.parkingspaceID}
                      dataIdField="parkingspaceid"
                      match="contains"
                      componentColumns ={this.state.componentColumns}
                      options={{multiSelect: true, selectable: true ,gridHeight:{windowPct:90,offset:-112}}}
                      columns = {
                          this.state.parkingspaceStoredWidths.map(function(column, index){
                              if(column.id === "demarcated"){
                                  return(
                                      {name:column.name,field:column.field,id: column.id, checked: column.checked, required: column.required,width: column.width,
                                           disableSearch: true, cssClass:"text-center" , headerCssClass:"text-center",
                                              formatter: function(row, cell, value) {
                                                if(value == true){
                                                    return '<img height="24" title="" alt="'
                                                        + '" src="/imgs/new-icons/demarcated.png" alt="' + value + '" />'
                                                }else{
                                                    return '<img height="24" title="" alt="'
                                                        + '" src="/imgs/new-icons/not_demarcated.png" alt="' + value + '" />'
                                                }
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
                          })}


            />

        </div>
        {/*<div>*/}
            <button id="showDetailsParkingspace" className="ns-big-btn" style={{width:"40%",right:"32%",position:"relative"}}
            onClick={this.toggleDetail}>{
                this.props.detail_state=="hidden"
                ?<b>Show / Edit Details</b>
                :<b>Hide Details</b>
              }</button>

        {/*</div>*/}
      </div>
      );
  }
});

module.exports = Parkingspacelist;

