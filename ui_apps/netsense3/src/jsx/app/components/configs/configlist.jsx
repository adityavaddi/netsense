import classNames from 'classnames';
import auth from 'global/utils/auth';
import { State, Navigation } from 'react-router';
import DataGrid from 'components/datagrid';
import Listeditor  from 'components/listeditor';

var Configlist = React.createClass({

    getInitialState: function(){
        var component = "Config";
        var userRole = NSN.userInfo.name;
        var componentColumns = "ConfigColumnWidths_"+ userRole;
        var configStoredWidths = [];
        if(localStorage.getItem(componentColumns)){
            configStoredWidths = JSON.parse(localStorage.getItem(componentColumns));
        }else{
            // default widths
            configStoredWidths = [
                {name:"Name", field:"name", id:"name",checked:true, required:true, sortable:true, width:100},
                {name:"Model", field:"model", id:"model",checked:true, required:false, sortable:true, width:80, },
                {name:"# Nodes", field:"nodes", id:"nodes",checked:true, required:true, sortable:true, width:60,disableSearch:true,
                    cssClass:"text-center",headerCssClass:"text-center"
                }
            ]
        }

        return {
            showListEditor:false,
            configStoredWidths:configStoredWidths,
            componentColumns:componentColumns,
            component:component
        }
    },
  propTypes: {
    configs: React.PropTypes.array.isRequired,
    defaultmodel: React.PropTypes.object.isRequired,
    configID: React.PropTypes.string
  },

  handleAdd: function () {
    ReactBootstrap.Dispatcher.emit("Configlist.add");
    $('.configForm').css('display', '');
  },

    toggleListEditor : function(){
        this.setState({showListEditor:!this.state.showListEditor});
  },

  shouldComponentUpdate: function (nextProps, nextState) {
    // This component does not need to be rerendered unless configs have been added or changed
    // return this.props.configs !== nextProps.configs;
      return true;
  },

  render() {
    var Addbutton = (<span></span>);
    if (auth.allowed('CAN_CREATE', 'ConfigModel')) {
      Addbutton = (
        <button id="addConfig" title="Add Config" className="ns-big-btn" onClick={this.handleAdd}><b>Add configuration</b></button>
      );
    };

    return (
      <div id="config-table-container">
        <h2 className="netsense__table__title" style={{ marginTop: "-4px" }}>Configurations {Addbutton}
            <span onClick={()=>this.toggleListEditor()}
                  className="ns-filter-icon"></span>
        </h2>
          {
              this.state.showListEditor ?
                  <div className="ns-list-editor">
                      <Listeditor show={this.state.showListEditor}
                                  component={this.state.component}
                                  componentColumns ={this.state.componentColumns}
                                  handleToggle = {this.toggleListEditor}
                                  columns={this.state.configStoredWidths}/>
                  </div>:
                  null
          }

          <DataGrid component={this.state.component}
                    dataArray={this.props.configs}
                    dataID={this.props.configID}
                    dataIdField="configid"
                    componentColumns ={this.state.componentColumns}
                    columns={this.state.configStoredWidths} />
      </div>
    );
  }
});

module.exports = Configlist;

