import classNames from 'classnames';
import auth from 'global/utils/auth';
import { State, Navigation } from 'react-router';
import DataGrid from 'components/datagrid';
import Listeditor  from 'components/listeditor';

var Parkingzonelist = React.createClass({
    getInitialState: function(){
         var component = "Parkingzone";
         var userRole = NSN.userInfo.name;
         var componentColumns = "ParkingzoneColumnWidths_"+ userRole;
         var parkingZoneStoredWidths = [];

         if(localStorage.getItem(componentColumns)){
             parkingZoneStoredWidths = JSON.parse(localStorage.getItem(componentColumns));
         }else{
             // default widths
             parkingZoneStoredWidths = [
                 {name:"Name", field:"name", id:"name", sortable:true, width:200, checked:true, required:true},
                 {name:"Type", field:"type", id:"type", sortable:true, width:150, checked:true, required:true},
                 {name:"Active", field:"active", id:"active", sortable:true, width:80, checked:true, required:true},
                 {name:"Max Spaces", field:"max_spaces", id:"max_spaces", sortable:true, width:80, checked:true, required:true},
                 {name:"Node ID", field:"nodeid", id:"nodeid",sortable:true, width:150, checked:true, required:true},
                 {name:"Parking Group", field:"parkinggroupname", id:"parkinggroupname", sortable:true, width:150, checked:true, required:true}
             ]
         }
 
         return {
             showListEditor:false,
             parkingZoneStoredWidths:parkingZoneStoredWidths,
             componentColumns:componentColumns,
             component:component
         }
     },
  propTypes: {
    parkinggroups: React.PropTypes.array.isRequired,
    parkingzones: React.PropTypes.array.isRequired,
    parkingzoneID: React.PropTypes.string,
    minmax: React.PropTypes.string,
    detail_state: React.PropTypes.string,
    activeFilter: React.PropTypes.string,
    ui_state: React.PropTypes.object
  },

  maximize: function () {
        if (this.props.ui_state.detail == "pinned") {
            ReactBootstrap.Dispatcher.emit('Parkingzonedetail.togglePin');
        }
        $("#parkingzone-list-panel").data("state", "open").css({ width: "100%" });
        $(window).trigger('resize');
    },

  minimize: function() {
    $("#parkingzone-list-panel").data("state","closed").css({width:"33%", zIndex: "300"});
    $(window).trigger('resize');
  },

  togglegrid: function() {
    if ($("#parkingzone-list-panel").data("state") == "closed") {
      this.maximize();
    } else {
      this.minimize();
    }
  },

  handleAdd: function() {
    ReactBootstrap.Dispatcher.emit("Parkingzonelist.add");
  },

  toggleListEditor : function(){
      this.setState({ showListEditor: !this.state.showListEditor});
  },

  toggleDetail: function () {
        if ($("#parkingzone-list-panel").data("state") == "open") {
            this.minimize();
            ReactBootstrap.Dispatcher.emit("Parkingzonelist.toggleDetail");
        } else {
            ReactBootstrap.Dispatcher.emit("Parkingzonelist.toggleDetail");
        }
    },

  shouldComponentUpdate: function(nextProps,nextState){
    // This component does not need to be rerendered unless parkingzones have been added or changed
   // return this.props.parkingzones !== nextProps.parkingzones;
  //  return this.props.activeFilter != nextProps.activeFilter || this.props.detail_state != nextProps.detail_state;
  return true;
  },
 
  render() {
    var pgnames = {"Unknown":"--"};
    for (var i=0; i<this.props.parkinggroups.length; i++) {
      pgnames[this.props.parkinggroups[i].parkinggroupid] = this.props.parkinggroups[i].name;
    };
    for (i=0; i<this.props.parkingzones.length; i++) {
      this.props.parkingzones[i].parkinggroupname = pgnames[this.props.parkingzones[i].parkinggroupid];
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
        </div>);
     };
    return (
      <div style={{ height: "100%" }} >
        {Minmaxlist}
        <h2 className="netsense__map__table__title">Parking Zones
                    <span onClick={() => this.toggleListEditor()}
                        className="ns-map-filter-icon" style={{top:"8px !important"}}></span>
                </h2>
          {
              this.state.showListEditor ?
                  <div className="ns-list-editor" style={{top:"70px !important"}}>
                      <Listeditor show={this.state.showListEditor}
                                  component={this.state.component}
                                  handleToggle = {this.toggleListEditor}
                                  componentColumns ={this.state.componentColumns}
                                  columns={this.state.parkingZoneStoredWidths}/>
                  </div>:
                  null
          }
        <div id="parkingzone-table-container">
          <DataGrid component="Parkingzone" 
                    componentName="Parking Zone"
                    dataArray={this.props.parkingzones}
                    dataID={this.props.parkingzoneID} 
                    options={{gridHeight: { windowPct: 100, offset: -180 }}}
                    dataIdField="parkingzoneid"
                    match="contains"
                    componentColumns ={this.state.componentColumns}
                    columns={this.state.parkingZoneStoredWidths} />
        </div>
        <div style={{textAlign:"center"}}>
            <button id="showDetailsParkingZones" className="ns-big-btn" style={{float:"none",width:"300px !important"}} onClick={this.toggleDetail}>{
                this.props.detail_state=="hidden"
                ?<b>Show Details</b>
                :<b>Hide Details</b>
              }</button>
          </div>
      </div>
      );
  }
});

module.exports = Parkingzonelist;

