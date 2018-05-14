import classNames from 'classnames';
import { Link } from 'react-router';
import auth from 'global/utils/auth';
import helpers from 'global/utils/helpers';

var Daylightdatatable = React.createClass({
  propTypes: {
    daylights: React.PropTypes.array.isRequired,
    daylightID: React.PropTypes.string.isRequired,
    component: React.PropTypes.string
  },

  gridOptions: {
    enableCellNavigation: true,
    forceFitColumns: true, 
    fullWidthRows: true,
    rowHeight: 30,
    showHeaderRow: true,
    explicitInitialization: true,
    headerRowHeight: 36,
    multiSelect: false
  },

  columns: [
    {name:"Name", field:"name", id:"name", 
      sortable:true, width:100},
    {name:"Description", field:"description", id:"description", 
      sortable:true, width:150}
  ],

  columnFilters: {},

  shouldComponentUpdate() {
    return false;
  },
  
  componentDidMount() {
    var that = this;
    $("#daylight-table").height(helpers.calcHeight(100,-150));
    
    // initialize grid and dataview
    this.dataView = new Slick.Data.DataView();
    this.grid = new Slick.Grid("#daylight-table", this.dataView, this.columns, this.gridOptions);

    // Make the grid respond to DataView change events.
    this.dataView.onRowCountChanged.subscribe(function (e, args) {
      that.grid.updateRowCount();
      that.grid.render();
    });

    this.dataView.onRowsChanged.subscribe(function (e, args) {
      that.grid.invalidateRows(args.rows);
      that.grid.render();
    });

    ReactBootstrap.Dispatcher.on('Daylightform.delete.success', function(etdhprofileid) {
      that.dataView.deleteItem(etdhprofileid);
    });
    ReactBootstrap.Dispatcher.on('Daylightform.add.success', function(daylight) {
      that.dataView.addItem(daylight);
    });
    ReactBootstrap.Dispatcher.on('Daylightform.update.success', function(daylight) {
      that.dataView.updateItem(daylight.etdhprofileid, daylight);
    });

    // set up column sorting
    this.grid.onSort.subscribe(function(e, args){ // args: sort information. 
      var field = args.sortCol.field;

      that.props.daylights.sort(function(a, b){
          var result = 
              a[field] > b[field] ? 1 :
              a[field] < b[field] ? -1 :
              0;

          return args.sortAsc ? result : -result;
      });

      that.grid.invalidate();         
    });
   
    function columnFilter(item) {
      for (var columnId in that.columnFilters) {
        if (columnId !== undefined && that.columnFilters[columnId] !== "") {
          var c = that.grid.getColumns()[that.grid.getColumnIndex(columnId)];
          if (item[c.field] != that.columnFilters[columnId]) {
            return false;
          }
        }
      }
      return true;
    };

    $(this.grid.getHeaderRow()).delegate(":input", "change keyup", function (e) {
      var columnId = $(this).data("columnId");
      if (columnId != null) {
        that.columnFilters[columnId] = $.trim($(this).val());
        that.dataView.refresh();
      }
    });

    this.grid.onHeaderRowCellRendered.subscribe(function(e, args) {
        $(args.node).empty();
        $("<input type='text' placeholder='Search...'>")
           .data("columnId", args.column.id)
           .val(that.columnFilters[args.column.id])
           .appendTo(args.node);
    });

    this.grid.init();

    // set up selection handler
    this.grid.setSelectionModel(new Slick.RowSelectionModel());
    this.grid.onSelectedRowsChanged.subscribe(function(e, args) {
      var selectedIds = that.dataView.mapRowsToIds(that.grid.getSelectedRows());
      if (selectedIds.length == 1) {
        ReactBootstrap.Dispatcher.emit(that.props.component+"list.select", selectedIds[0]);
      } else {
        ReactBootstrap.Dispatcher.emit(that.props.component+"list.multiSelect", selectedIds);
      }
    });

  // initialize the model after all the events have been hooked up
    this.dataView.beginUpdate();
    this.dataView.setItems(this.props.daylights, "etdhprofileid");
    this.dataView.setFilter(columnFilter);
    this.dataView.endUpdate();

    this.dataView.syncGridSelection(this.grid, true);
  },

  componentWillUnmount(){
    ReactBootstrap.Dispatcher.removeAllListeners("Daylightform.delete.success");
    ReactBootstrap.Dispatcher.removeAllListeners("Daylightform.add.success");
    ReactBootstrap.Dispatcher.removeAllListeners("Daylightform.update.success");
    this.grid.destroy();
    this.grid = null;
  },

  render(){
    return (
      <div id="daylight-table" width="100%"></div>
    );
  }

});
module.exports = Daylightdatatable;

