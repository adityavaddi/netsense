import classNames from 'classnames';
import auth from 'global/utils/auth';
import { State, Navigation } from 'react-router';
import DataGrid from 'components/datagrid';
import Listeditor  from 'components/listeditor';

var Proximitylist = React.createClass({
    getInitialState: function(){
        var component = "Proximity";
        var userRole = NSN.userInfo.name;
        var componentColumns = "ProximityColumnWidths_"+ userRole;
        var proximityStoredWidths = [];
        if(localStorage.getItem(componentColumns)){
            proximityStoredWidths = JSON.parse(localStorage.getItem(componentColumns));
        }else{
            // default widths
            proximityStoredWidths = [
                { name: "Name", field: "name", id: "name", sortable: true, width: 100, required:true, checked:true},
                { name: "Description", field: "description", id: "description",sortable: true, width: 150, required:false, checked:true }
            ]
        }
        return {
            showListEditor:false,
            proximityStoredWidths:proximityStoredWidths,
            componentColumns:componentColumns,
            component:component
        }
    },
      propTypes: {
        proximitys: React.PropTypes.array.isRequired,
        proximityID: React.PropTypes.string
      },

    toggleListEditor : function(){
        this.setState({showListEditor:!this.state.showListEditor});
    },


    handleAdd: function () {
    ReactBootstrap.Dispatcher.emit("Proximitylist.add");
  },

  shouldComponentUpdate: function (nextProps, nextState) {
    // This component does not need to be rerendered unless proximitys have been added or changed
    // return this.props.proximitys !== nextProps.proximitys;
      return true;
  },

  render() {
    var Addbutton = (<span></span>);
    if (auth.allowed('CAN_CREATE', 'ScheduleModel')) {
      Addbutton = (
        <button id="add-profile" ttitle="Add Proximity Dimming profile" className="ns-big-btn" onClick={this.handleAdd}><b>Add profile</b></button>
      );
    };

    return (
      <div id="proximity-table-container">
        <h2 className="netsense__table__title" style={{ marginTop: "-4px" }}>Proximity Dimming Profiles {Addbutton}
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
                                  columns={this.state.proximityStoredWidths}/>
                  </div>:
                  null
          }
        <DataGrid  component={this.state.component}
          componentName="Profile"
          dataArray={this.props.proximitys}
          dataID={this.props.proximityID}
          dataIdField="pdprofileid"
          match="contains"
          columns={this.state.proximityStoredWidths} />
      </div>
    );
  }
});

module.exports = Proximitylist;
