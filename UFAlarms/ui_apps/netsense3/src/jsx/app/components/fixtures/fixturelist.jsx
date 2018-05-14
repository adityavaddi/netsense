import classNames from 'classnames';
import { State, Navigation } from 'react-router';
import DataGrid from 'components/datagrid';
import Listeditor  from 'components/listeditor';

var Fixturelist = React.createClass({
  getInitialState: function(){

      var component = "Fixture";
      var userRole = NSN.userInfo.name;
      var componentColumns = "FixtureColumnWidths_" + userRole;
      var fixtureStoredWidths = [];

      if (localStorage.getItem(componentColumns)) {
           fixtureStoredWidths = JSON.parse(localStorage.getItem(componentColumns));
      } else {
           // default widths
           fixtureStoredWidths = [
               {name:"Type", field:"fixtureType", id:"fixtureType", sortable:true, width:40,
                    cssClass:"text-center", headerCssClass:"text-center",checked:true, required:false},
               {name:"Name", field:"name", id:"name", sortable:true, width:120,checked:true, required:true},
               {name:"Description", field:"description", id:"description", sortable:true, width:150,checked:true, required:false}
           ]
      }
      return {
          showListEditor: false,
          fixtureStoredWidths: fixtureStoredWidths,
          componentColumns: componentColumns,
          component: component,
      }
  },


  propTypes: {
    fixtures: React.PropTypes.array.isRequired,
    fixtureID: React.PropTypes.string
  },

  handleAdd: function () {
    ReactBootstrap.Dispatcher.emit("Fixturelist.add");
  },



  shouldComponentUpdate: function (nextProps, nextState) {
    // This component does not need to be rerendered unless fixtures have been added or changed
    // return this.props.fixtures !== nextProps.fixtures;
      return true;
  },

  toggleListEditor : function(){
     this.setState({showListEditor:!this.state.showListEditor});
  },

  render() {
    var Addbutton = (
      <button id="addFixture" title="Add Fixture" className="ns-big-btn" onClick={this.handleAdd}><b>Add Fixture</b></button>
    );

    return (
      <div id="fixture-table-container">
        <h2 className="netsense__table__title" style={{ marginTop: "-4px" }}>Fixtures {Addbutton}
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
                             columns={
                                this.state.fixtureStoredWidths.map(function(column, index){
                                   if(column.name === "Type"){
                                        return(
                                            {name:column.name,field:column.field,id: column.id, checked: column.checked, required: column.required,width: column.width,
                                                  sortable:column.sortable, disableSearch: true, cssClass:"text-center" , headerCssClass:"text-center",
                                                  formatter: function(row,cell,value) {
                                                      var imageName = value.toLowerCase().replace(/\s/g,'');
                                                      return '<img height="24" title="" alt="'
                                                          + '" src="/imgs/fixturetypes/'
                                                          + imageName
                                                          + '.png" alt="' + value + '" />'
                                                  }
                                            }
                                        )
                                   }else{
                                       return (
                                       { name: column.name,field: column.field,id: column.id, checked: column.checked, required: column.required,
                                           width: column.width,sortable: column.sortable}
                                       )
                                   }
                                })
                      } />
                  </div>:
                  null
          }
          <DataGrid component="Fixture"
                dataArray={this.props.fixtures}
                dataID={this.props.fixtureID}
                dataIdField="fixtureid"
                componentColumns ={this.state.componentColumns}
                columns={
                    this.state.fixtureStoredWidths.map(function(column, index){
                       if(column.name === "Type"){
                          return(
                               {name:column.name,field:column.field,id: column.id, checked: column.checked, required: column.required,width: column.width,
                                   sortable:column.sortable, disableSearch: true, cssClass:"text-center" , headerCssClass:"text-center",
                               formatter: function(row,cell,value) {
                                   var imageName = value.toLowerCase().replace(/\s/g,'');
                                    return '<img height="24" title="" alt="'
                                            + '" src="/imgs/fixturetypes/'
                                            + imageName
                                            + '.png" alt="' + value + '" />'
                               }
                             }
                          )
                       }else{
                         return(
                            { name:column.name,field:column.field, id: column.id,checked: column.checked,required: column.required,
                                    width: column.width, sortable:column.sortable}
                         )
                       }
                    })
                    } />
      </div>
    );

  }
}
);
module.exports = Fixturelist;
