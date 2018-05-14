import classNames from 'classnames';
import auth from 'global/utils/auth';
import { State, Navigation } from 'react-router';
import DataGrid from 'components/datagrid';
import Listeditor  from 'components/listeditor';

var UFAlarmlist = React.createClass({
    getInitialState: function(){
        var component = "ufalarm";
        var userRole = NSN.userInfo.name;
        var componentColumns = "ufalarmColumnWidths_"+ userRole;
        var ufalarmStoredWidths = [];
        if(localStorage.getItem(componentColumns)){
            ufalarmStoredWidths = JSON.parse(localStorage.getItem(componentColumns));
        }else{
            // default widths
            ufalarmStoredWidths = [
                { name: "Name", field: "ufname", id: "ufname", sortable: true, width: 100, required:true, checked:true},
                { name: "Alarm Type", field: "alarmtype", id: "alarmtype", sortable: true, width: 50, required:true, checked:true},
                { name: "Node Models", field: "nodemodels", id: "nodemodels", sortable: true, width: 50, required:true, checked:true},
                { name: "Display to Partner", field: "displaytopartner", id: "displaytopartner", sortable: true, width: 25, required:true, checked:true},
                { name: "Display to Customer", field: "displaytocustomer", id: "displaytocustomer", sortable: true, width: 25, required:true, checked:true},
            ]
        }
        return {
            showListEditor:false,
            ufalarmStoredWidths:ufalarmStoredWidths,
            componentColumns:componentColumns,
            component:component
        }
    },
      propTypes: {
        ufalarm: React.PropTypes.array.isRequired,
        ufalarm_ID: React.PropTypes.string
      },

    toggleListEditor : function(){
        this.setState({showListEditor:!this.state.showListEditor});
    },


    handleAdd: function () {
    ReactBootstrap.Dispatcher.emit("ufalarmlist.add");
  },

  shouldComponentUpdate: function (nextProps, nextState) {
    // This component does not need to be rerendered unless proximitys have been added or changed
    // return this.props.proximitys !== nextProps.proximitys;
      return true;
  },

  render() {
      console.log("ufalrms in ufalarm list", this.props);
    var Addbutton = (<span></span>);
    if (auth.allowed('CAN_CREATE', 'ScheduleModel')) {
      Addbutton = (
        <button id="add-alarm" title="Add User Friendly Alarm" className="ns-big-btn" onClick={this.handleAdd}><b>Add Alarm</b></button>
      );
    };

    return (
      <div id="ufalarm-table-container">
        <h2 className="netsense__table__title" style={{ marginTop: "-4px" }}>User Friendly Alarms {Addbutton}
            <span onClick={()=>this.toggleListEditor()} className="rubix-icon icon-nargela-align-right"
                  className="ns-filter-icon"></span>
        </h2>

          {
              this.state.showListEditor ?
                  <div className="ns-list-editor">
                      <Listeditor show={this.state.showListEditor}
                                  component={this.state.component}
                                  handleToggle = {this.toggleListEditor}
                                  componentColumns ={this.state.componentColumns}
                                  columns={this.state.ufalarmStoredWidths}/>
                  </div>:
                  null
          }
        <DataGrid  component={this.state.component}
          componentName="ufalarm"
          dataArray={this.props.ufalarm}
          dataID={this.props.ufalarm_ID}
          dataIdField="mappingid"
          match="contains"
          columns={this.state.ufalarmStoredWidths} />
      </div>
    );
  }
});

module.exports = UFAlarmlist;
