import classNames from 'classnames';
import auth from 'global/utils/auth';
import { State, Navigation } from 'react-router';
import DataGrid from 'components/datagrid';
import Listeditor  from 'components/listeditor';

var Firmwarelist = React.createClass({
    getInitialState: function(){
        var component = "Firmware";
        var userRole = NSN.userInfo.name;
        var componentColumns = "FirmwareColumnWidths_"+ userRole;
        var firmwareStoredWidths = [];
        if(localStorage.getItem(componentColumns)){
            firmwareStoredWidths = JSON.parse(localStorage.getItem(componentColumns));
        }else{
            // default widths
            firmwareStoredWidths = [
                {name:"Name", field:"name", id:"name", width:40,sortable:true, width:100, checked:true, required:true},
                {name:"Type", field:"type", id:"type", width:40,sortable:true, width:100, checked:true, required:true},
                {name:"Release", field:"release", id:"release", width:40,sortable:true, width:100, checked:true, required:true},
            ]
        }
        return {
            showListEditor:false,
            firmwareStoredWidths:firmwareStoredWidths,
            componentColumns:componentColumns,
            component:component
        }
    },
  propTypes: {
    firmwares: React.PropTypes.array.isRequired,
    otas: React.PropTypes.array.isRequired,
    firmwareID: React.PropTypes.string
  },

  handleAdd: function() {
    ReactBootstrap.Dispatcher.emit("Firmwarelist.add");
  },

  shouldComponentUpdate: function(nextProps,nextState){
    // This component does not need to be rerendered unless firmwares have been added or changed
    // return this.props.firmwares !== nextProps.firmwares;
      return true;
  },

    toggleListEditor : function(){
        this.setState({showListEditor:!this.state.showListEditor});
    },

  render() {
    /*var Addbutton = (<span></span>);
    if (auth.allowed('CAN_CREATE','FirmwareModel')) {
       Addbutton = (
        <button title="Add Firmware  profile" className="ns-big-btn" onClick={this.handleAdd}><b>Add profile</b></button>
        );
    };*/
    var that = this;
    return (
      <div id="firmware-table-container">
        <h2 className="netsense__table__title" style={{marginTop:"-4px"}}>Firmware Versions
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
                                  columns= {
                                  this.state.firmwareStoredWidths.map(function(column, index){
                                    return({
                                        name:column.name,field:column.field,id: column.id, checked: column.checked, required: column.required,width: column.width,
                                        sortable:column.sortable
                                    })
                                  })

                              }/>

                  </div>:
                  null
          }
        <DataGrid component="Firmware"
                  dataArray={this.props.firmwares}
                  dataID={this.props.firmwareID}
                  dataIdField="firmwareid"
                  columns= {
                    this.state.firmwareStoredWidths.map(function(column, index){
                      if(column.name ==="Name"){
                          return(
                          {
                              name:column.name,field:column.field, id: column.id,checked: column.checked,required: column.required,
                              width: column.width, sortable:column.sortable,
                              formatter: function(row, cell, value, columnDef, dataContext) {
                                if(typeof dataContext.when != "undefined"){
                                  var time = "";
                                  time = dataContext.when.substring(0,10);
                                  return (value + "_" + time);
                                }
                                else{
                                  return value; 
                                }
                                  
                              }
                          }
                          )
                      }
                      else{
                        return({
                            name:column.name,field:column.field,id: column.id, checked: column.checked, required: column.required,width: column.width,
                            sortable:column.sortable
                        })
                      }
                    })

                  }/>
      </div>
      );
  }
});

module.exports = Firmwarelist;

