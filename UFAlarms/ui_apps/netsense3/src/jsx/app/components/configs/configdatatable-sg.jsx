import classNames from 'classnames';
import { Link } from 'react-router';
import auth from 'global/utils/auth';
import helpers from 'global/utils/helpers';

var Configdatatable = React.createClass({
  propTypes: {
    configs: React.PropTypes.array.isRequired,
    configID: React.PropTypes.string.isRequired,
    component: React.PropTypes.string
  },

  gridOptions: {
    forceFitColumns: true, 
    fullWidthRows: true,
    rowHeight: 30,
    headerRowHeight: 36,
    multiSelect: false
  },

  columns: [
    {name:"Name", field:"name", id:"name", 
      sortable:true, width:100},
    {name:"Model", field:"model", id:"model", 
      sortable:true, width:150},
    {name:"Nodes", field:"nodes", id:"nodes", 
      sortable:true, width:150}
  ],
   
  componentDidUpdate() {
    this.grid.setData(this.props.configs, "configid");
    this.grid.invalidate();
  },
  
  componentDidMount() {
    var that = this;
    var searchList = [];
    $("#config-table").height(helpers.calcHeight(100,-150));
    
    // initialize grid and dataview
    this.dataView = new Slick.Data.DataView();
    this.grid = new Slick.Grid("#config-table", this.dataView, this.columns, this.gridOptions);
    this.grid.setData(this.props.configs, "configid");
    this.grid.render();

    // set up selection handler
    this.grid.setSelectionModel(new Slick.RowSelectionModel());
    this.grid.onSelectedRowsChanged.subscribe(function(e, args) {
      var selectedData = [],
          modelrows = [],
          selectedIndexes;

      selectedIndexes = that.grid.getSelectedRows();
      jQuery.each(selectedIndexes, function (index, value) {
        selectedData.push(that.grid.getData()[value]);
      });

      if (selectedData.length == 1) {
        ReactBootstrap.Dispatcher.emit(that.props.component+"list.select", selectedData[0].configid);
      } else {
        ReactBootstrap.Dispatcher.emit(that.props.component+"list.multiSelect", 
          selectedData.map(function(config) {
            return config.configid;
          }), []);
      }
    });

    // Make the grid respond to DataView change events.
    this.dataView.onRowCountChanged.subscribe(function (e, args) {
      that.grid.updateRowCount();
      that.grid.render();
    });

    this.dataView.onRowsChanged.subscribe(function (e, args) {
      that.grid.invalidateRows(args.rows);
      that.grid.render();
    });

    // set up column sorting
    this.grid.onSort.subscribe(function(e, args){ // args: sort information. 
      var field = args.sortCol.field;

      that.props.configs.sort(function(a, b){
          var result = 
              a[field] > b[field] ? 1 :
              a[field] < b[field] ? -1 :
              0;

          return args.sortAsc ? result : -result;
      });

      that.grid.invalidate();         
    });

     // wire up the search textbox to apply the filter to the model
    $("#config-search").keyup(function (e) {
      // clear on Esc
      if (e.which == 27) {
        this.value = "";
      }
      searchList = $.trim(this.value.toLowerCase()).split(' ');
      updateFilter();
    });

    function updateFilter() {
      that.dataView.setFilterArgs({
        searchList: searchList
      });
//      that.dataView.refresh();
      that.dataView.refresh();
    };

    function gridFilter(item) {
      var found;
      for (var i = 0; i < searchList.length; i += 1) {
        found = false;
        $.each(item, function(obj, objValue) {
          if (typeof objValue !== 'undefined' && objValue != null 
          && objValue.toString().toLowerCase().indexOf(searchList[i]) != -1) {
              found = true;
              return false; //this breaks the $.each loop
          };
        });
        if (!found) {
          return false;
        };
      };
      return true;
    };


// initialize the model after all the events have been hooked up
  this.dataView.beginUpdate();
  this.dataView.setItems(this.props.configs, "configid");
  this.dataView.setFilterArgs({
    searchList: searchList
  });
  this.dataView.setFilter(gridFilter);
  this.dataView.endUpdate();

  this.dataView.syncGridSelection(this.grid, true);

  },

  render(){
    return (
      <div>
        <div style={{textAlign:"right",fontSize:"16px",display:"none"}}>
          <input type="text" name="config-search" id="config-search" 
              style={{width:"40%",border:"1px solid silver",marginBottom:"10px"}} placeholder=" Search..." />
          </div>
        <div id="config-table" width="100%">
        </div>
      </div>
    );
  }

});
module.exports = Configdatatable;

