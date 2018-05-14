import classNames from 'classnames';
import auth from 'global/utils/auth';
import { State, Navigation } from 'react-router';
import DataGrid from 'components/datagrid';
import helpers from 'global/utils/helpers';
import Listeditor  from 'components/listeditor';

var Otalist = React.createClass({

  getInitialState: function(){
        var component = "Ota";
        var userRole = NSN.userInfo.name;
        var componentColumns = "FirmwareOtaColumnWidths_"+ userRole;
        var otaStoredWidths = [];
        if(localStorage.getItem(componentColumns)){
           otaStoredWidths = JSON.parse(localStorage.getItem(componentColumns));
        }else{
           
            // default widths
            otaStoredWidths = [
                { name: "Firmware ID", field: "firmwareid", id: "firmwareid", sortable: true, width: 100, required:true, checked:true},
                { name: "Target Name", field: "target_name", id: "target_name", sortable: true, width: 100, required:true, checked:true},
                { name: "Model", field: "model", id: "model", sortable: true, width: 100, required:false, checked:true},
                { name: "When", field: "when", id: "when", sortable: true, width: 100, required:true, checked:true},
                { name: "Job ID", field: "jobid", id: "jobid", sortable: true, width: 100, required:false, checked:true},
                { name: "Description", field: "description", id: "description", sortable: true, width: 100, required:false, checked:true},
            ]
        }
        return {
            showListEditor:false,
            otaStoredWidths:otaStoredWidths,
            componentColumns:componentColumns,
            component:component
        }
    },

  propTypes: {
    otas: React.PropTypes.array.isRequired,
    otaID: React.PropTypes.string
  },

  toggleListEditor : function(){
    this.setState({showListEditor:!this.state.showListEditor});
  },

  shouldComponentUpdate: function(nextProps,nextState){
    // This component does not need to be rerendered unless otas have been added or changed
    return true;
  },
  
  render() {

   // Sort the data by date using moment.js
    this.props.otas.sort(function (left, right) {
        var leftDate = moment.utc(left.when);
        var rightDate = moment.utc(right.when);
        var diff = rightDate.diff(leftDate);
        return diff > 0;
    });

    return (

      <div id="ota-table-container">
        <h2 className="netsense__table__title" style={{ marginTop: "-4px" }}>Firmware Jobs
            <span onClick={()=>this.toggleListEditor()} className="ns-filter-icon" style={{left:"0px !important"}}></span>
        </h2>

          {
              this.state.showListEditor ?
                  <div className="ns-list-editor">
                      <Listeditor show={this.state.showListEditor}
                                  component={this.state.component}
                                  handleToggle = {this.toggleListEditor}
                                  componentColumns ={this.state.componentColumns}
                                  columns={this.state.otaStoredWidths}/>
                  </div>:
                  null
          }
        <DataGrid  component={this.state.component}
          componentName="OTA"
          dataArray={this.props.otas}
          dataID={this.props.otaID}
          dataIdField="otaid"
          match="contains"
          columns={this.state.otaStoredWidths} />
      </div>
     
      );
  }
});

module.exports = Otalist;

