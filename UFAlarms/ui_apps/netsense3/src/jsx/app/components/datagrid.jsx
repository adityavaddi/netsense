import classNames from 'classnames';
import { Link } from 'react-router';
import auth from 'global/utils/auth';
import helpers from 'global/utils/helpers';

var DataGrid = React.createClass({

  propTypes: {
    dataArray: React.PropTypes.array.isRequired,
    dataID: React.PropTypes.string.isRequired,
    component: React.PropTypes.string.isRequired,
    componentName: React.PropTypes.string,
    columns: React.PropTypes.array.isRequired,
    dataIdField: React.PropTypes.string,
    match: React.PropTypes.string,
    options: React.PropTypes.object
  },

  getDefaultProps: function () {
    return {
      dataIdField: 'id',
      match: 'contains',
      options: {},
      componentName: null
    }
  },

  gridOptions: {
    enableCellNavigation: true,
    forceFitColumns: true,
    fullWidthRows: true,
    rowHeight: 40,
    showHeaderRow: true,
    explicitInitialization: true,
    headerRowHeight: 40,
    multiSelect: true,
    gridHeight: { windowPct: 100, offset: -112 }
  },

  columnFilters: {},

  shouldComponentUpdate: function () {
    return false;
  },

  checkDataErrMsg: "",

  checkData: function () {
    // this will catch corruptions in the dataArray (duplicate or empty ids)
    var hash = {};
    for (var i = 0; i < this.props.dataArray.length; i++) {
      if (typeof hash[this.props.dataArray[i][this.props.dataIdField]] == "undefined") {
        hash[this.props.dataArray[i][this.props.dataIdField]] = 0;
      } else {
        this.checkDataErrMsg = 'Data problem!! The API is returning at least two ' + this.props.component + 's with the same '
          + this.props.dataIdField + ' (' + this.props.dataArray[i][this.props.dataIdField] + ').'
          + ' This page cannot be displayed until the problem is fixed.';
        return false;
      }
    }
    this.checkDataErrMsg = "";
    return true;
  },

  resizeEnd: null,
  handleResizeEnd: function () {
    $('#' + this.props.component + '-grid').height(helpers.calcHeight(this.gridOptions.gridHeight.windowPct, this.gridOptions.gridHeight.offset));
    this.grid.resizeCanvas();
  },

  handleResize: function () {
    clearTimeout(this.resizeEnd);
    this.resizeEnd = setTimeout(this.handleResizeEnd, 200);
  },

  componentDidMount: function () {

    //DataGrid Checkbox
    if (this.props.options.selectable) {
      var checkboxSelector = new Slick.CheckboxSelectColumn({
        cssClass: "slick-cell-checkboxsel",
        unfiltered: true
      });
      this.props.columns.unshift(checkboxSelector.getColumnDefinition());
    }
    if (this.checkDataErrMsg != "") {
      return;
    };
    var that = this;

    this.props.componentName = this.props.componentName || this.props.component;

    this.columnFilters = {};

    for (var option in this.props.options) {
      this.gridOptions[option] = this.props.options[option];
    };

    $('#' + this.props.component + '-grid').height(helpers.calcHeight(this.gridOptions.gridHeight.windowPct, this.gridOptions.gridHeight.offset));

    // initialize grid and dataview
    this.dataView = new Slick.Data.DataView();

    // On page Load, check for the ones that are checked and not the ones that are unchecked and pass them when creating a grid
    var currentProps = this.props.columns;
    var newProps = [];
    for (var j = 0; j < currentProps.length; j++) {

      if (currentProps[j].checked === undefined) {
      //For pages that do not have checked added yet - Temporary code : will be removed once all the pages have it
    //    console.log(" Inside if the checked is not present ");
        newProps.push(currentProps[j]);
    //    console.log("currentProps[j]", currentProps[j]);

      } else {
          if (currentProps[j].checked === true) { // New Customize list views
              newProps.push(currentProps[j])
          }
      };
    }
    this.props.columns = newProps;

    this.grid = new Slick.Grid('#' + this.props.component + '-grid', this.dataView, this.props.columns, this.gridOptions);
    $('#' + this.props.component + '-grid').data("gridInstance", that.grid);

    if (this.props.options.selectable) {
      this.grid.registerPlugin(checkboxSelector);
    }

    if (this.props.component === "GroupNode") {
      /* Functionality to checkbox select nodes is only available in Groups Panel */
      this.grid.setSelectionModel(new Slick.RowSelectionModel({ selectActiveRow: false }));
      //this.grid.setSelectedRows(that.props.selectedRows);
    }

    // Make the grid respond to DataView change events.
    this.dataView.onRowCountChanged.subscribe(function (e, args) {
      if (that.props.component === "ufalarm") {
        // Deselect all rows in grid to prevent overlay popup on search 
        that.grid.setSelectedRows([]);
      }
      that.grid.updateRowCount();
      that.grid.render();
      var len = that.dataView.getLength();
      $("#" + that.props.component + '-grid-footer').html(len + " " + (that.props.componentName || that.props.component) + (len == 1 ? "" : "s"));
    });
    // Adding auto tooltip
    that.grid.registerPlugin(new Slick.AutoTooltips({ enableForHeaderCells: true }));
    that.grid.render();

    this.dataView.onRowsChanged.subscribe(function (e, args) {
      that.grid.invalidateRows(args.rows);
      that.grid.render();
      var len = that.dataView.getLength();
      $("#" + that.props.component + '-grid-footer').html(len + " " + (that.props.componentName || that.props.component) + (len == 1 ? "" : "s"));
    });

    that.grid.onColumnsResized.subscribe(function(e, args) {
      var newChangedWidthColumns = []
      for (var i = 0, totI = that.grid.getColumns().length; i < totI; i++) {
        var column = that.grid.getColumns()[i];
        if (column.width != column.previousWidth) {
          newChangedWidthColumns.push(column);
        }
      }

      console.log("newChangedWidthColumns - ", newChangedWidthColumns)

      //************************* To Update in the local Storage ******************
      if (typeof(Storage) !== "undefined") {
        // var ComponentColumns = that.props.component + "ColumnWidths";
        var ComponentColumns = that.props.componentColumns;
        if(localStorage.getItem(ComponentColumns)){
          var localStorageItems = JSON.parse(localStorage.getItem(ComponentColumns));
          for(var i=0; i<localStorageItems.length; i++){

            for(var k=0;k<newChangedWidthColumns.length;k++){
              if(localStorageItems[i].id === newChangedWidthColumns[k].id && localStorageItems[i].width !== newChangedWidthColumns[k].width  ){
                localStorageItems[i].width = newChangedWidthColumns[k].width;

                console.log("localStorageItems[i].width ", localStorageItems[i].width )
                console.log("newChangedWidthColumns[k].width", newChangedWidthColumns[k].width )
              }
            }
          }
          localStorage.setItem(ComponentColumns, JSON.stringify(localStorageItems));
        }
        else{
          var newColumns = [];
          for (var j = 0, totI = that.grid.getColumns().length; j < totI; j++) {
            var column = that.grid.getColumns()[j];
            newColumns.push(column);
          }
          localStorage.setItem(ComponentColumns, JSON.stringify(newColumns));
        }
      } else {
        console.log(" No Local Storage")
      }
    });

    that.grid.onColumnsReordered.subscribe(function(e, args){

      var newChangedOrderColumns = [];
      for (var i = 0, totI = that.grid.getColumns().length; i < totI; i++) {
        var column = that.grid.getColumns()[i];
        newChangedOrderColumns.push(column);
      }

      // var ComponentColumns = that.props.component + "ColumnWidths";
      var ComponentColumns = that.props.componentColumns;
      if(localStorage.getItem(ComponentColumns)){
        var localStorageItems = JSON.parse(localStorage.getItem(ComponentColumns));
        var notPresentItems = [];

        for(var i=0;i < localStorageItems.length;i++){
          var found= false;
          for(var j =0;j < newChangedOrderColumns.length; j++){
            if(localStorageItems[i].id === newChangedOrderColumns[j].id ){
              found = true;
              break;
            }
          }
          if(found=== false){
            notPresentItems.push(localStorageItems[i]);
          }
        }
        for(var q=0; q < notPresentItems.length; q++){
          newChangedOrderColumns.push(notPresentItems[q])
        }
        localStorage.setItem(ComponentColumns, JSON.stringify(newChangedOrderColumns));
      } else {
        var newColumns = [];
        for (var j = 0, totI = that.grid.getColumns().length; j < totI; j++) {
          var column = that.grid.getColumns()[j];
          newColumns.push(column);
        }
        localStorage.setItem(ComponentColumns, JSON.stringify(newColumns));
      }
    });

    ReactBootstrap.Dispatcher.on(this.props.component + 'map.filter', function (filters) {
      // filters is a hash of columnids and values
      for (var i=0; i<filters.length; i++) {
        that.columnFilters[filters[i].columnId] = filters[i].value=="all"?"":filters[i].value;
      };
      if(that.dataView){
      that.dataView.refresh();
      }
    });

    ReactBootstrap.Dispatcher.on(this.props.component + 'form.delete.success', function (dataId) {
      if ($.isArray(dataId)) {
        that.dataView.beginUpdate();
        dataId.forEach(function (item) {
          that.dataView.deleteItem(item)
        });
        that.dataView.endUpdate();
      } else {
        that.dataView.deleteItem(dataId);
      }
    });
    ReactBootstrap.Dispatcher.on(this.props.component + 'form.add.success', function (dataElement) {
      if ($.isArray(dataElement)) {
        that.dataView.beginUpdate();
        dataElement.forEach(function (item) {
          that.dataView.addItem(item)
        });
        that.dataView.endUpdate();
      } else {
        that.dataView.addItem(dataElement);
        that.grid.scrollRowIntoView(dataElement.idx);
        for(var key in that.props.columns){
          that.grid.flashCell(dataElement.idx,key,300);
        }
      }
    });
    ReactBootstrap.Dispatcher.on(this.props.component + 'form.update.success', function (dataElement) {
      if ($.isArray(dataElement)) {
        that.dataView.beginUpdate();
        dataElement.forEach(function (item) {
          that.dataView.updateItem(item[that.props.dataIdField], item)
        });
        that.dataView.endUpdate();
      } else {
        that.dataView.updateItem(dataElement[that.props.dataIdField], dataElement);
      }
    });

    ReactBootstrap.Dispatcher.on(this.props.component + 'list.update.success', function (dataElement) {

      if ($.isArray(dataElement)) {
        that.grid.invalidateAllRows();
        that.dataView.setItems(dataElement);
        that.grid.render();
      }
      else {
        //that.dataView.updateItem(dataElement[that.props.dataIdField], dataElement);
      }
    });

    ReactBootstrap.Dispatcher.on(this.props.component + 'list.selectrow', function (dataId) {
      that.grid.sortedFlag = false;
      var idx = that.dataView.getIdxById(dataId);
      that.grid.scrollRowIntoView(idx);
      that.grid.setSelectedRows([idx]);
    });

    // Pre-select rows 
    ReactBootstrap.Dispatcher.on(this.props.component + 'form.selectedRows.update', function (selectedRows) {
      that.grid.setSelectedRows(selectedRows);
    });

    // Catching the Change in Number Of Columns for the Grid
    ReactBootstrap.Dispatcher.on("Listeditor.selectedColumns", function (columns) {
      var columnNames = that.grid.getColumns();
      console.log("Before setting the columns - get the column width ", columnNames);
      that.grid.setColumns(columns);
      var columnNames = that.grid.getColumns();
      console.log("AFTER setting the columns",  columnNames);
    });

    // A separate handle for the commissioning as there is a checkbox needed
    ReactBootstrap.Dispatcher.on("Listeditor.selectedColumnsCommissioning", function (columns) {
      var columnNamesCommissioning = that.grid.getColumns();

        // find the position in which the checkbox is there
        for(var k=0; k<columnNamesCommissioning.length; k++){
                if(columnNamesCommissioning[k].id === "_checkbox_selector"){
                    var itemToBeinserted = columnNamesCommissioning[k];
                    var idInIndex = k;
                    break;
            }
        }
        // once found insert in the place where it is so it can set the columns
        for(var l = 0; l< columns.length; l++){
            if(l=== idInIndex){
                columns.splice(l,0,itemToBeinserted);
            }
        }
        for(var d=0; d<columnNamesCommissioning.length; d++){
            for(var e=0; e<columns.length; e++) {
                if(columnNamesCommissioning[d].id=== columns[e].id){
                    columnNamesCommissioning[d].checked = columns[e].checked;
                }
            }
        }
          that.grid.setColumns(columns);
    });
    
    $(window).on('resize', this.handleResize);
    that.grid.sortedFlag = false;
    // set up column sorting
    this.grid.onSort.subscribe(function (e, args) { // args: sort information.
      var field = args.sortCol.field;
      if(field == "when" || field == "updated"){
        that.props.dataArray.sort(function (a, b) {
          var leftDate = moment(a[field]);
          var rightDate = moment(b[field]);
          var diff = rightDate.diff(leftDate);
          var result =
            leftDate > rightDate ? 1 :
              leftDate < rightDate ? -1 :
                0;
          that.grid.sortedFlag = true;
          return args.sortAsc ? result : -result;
        });
        that.dataView.refresh();
      }
      else{
          var comparer = function(a, b) {
          return (a[args.sortCol.field] > b[args.sortCol.field]) ? 1 : -1;
            }
         that.dataView.sort(comparer, args.sortAsc);
        // that.props.dataArray.sort(function (a, b) {
        //   var result =
        //     a[field] > b[field] ? 1 :
        //       a[field] < b[field] ? -1 :
        //         0;
        //   if(typeof a[field] == "undefined" && typeof b[field] != "undefined"){
        //     result = 1
        //   }
        //   if(typeof a[field] != "undefined" && typeof b[field] == "undefined"){
        //     result = -1
        //   }
        //   // this.dataView.setItems(that.props.dataArray, that.props.dataIdField);
        //   that.grid.sortedFlag = true;
        //   return args.sortAsc ? result : -result;
        // });
      }

      //that.dataView.refresh();
    });

    function columnFilter(item) {
      for (var columnId in that.columnFilters) {
        if (columnId !== undefined && that.columnFilters[columnId] !== "") {
          var c = that.grid.getColumns()[that.grid.getColumnIndex(columnId)];
          var source = item[c.field];
          if (typeof source == "undefined") {
            return false
          }
          if ($.isArray(source)) {
            source = JSON.stringify(source).replace(/[\[\]\"]/g, '');
          };

          if(source === null){
            source = "null";
          }
          
          switch (that.props.match) {
            case "exact":
              if (source != that.columnFilters[columnId]) { return false; };
              break;
            case "startswith":
              if (source.indexOf(that.columnFilters[columnId]) != 0) { return false; };
              break;
            case "contains":
              if (source.toLowerCase().indexOf(that.columnFilters[columnId].toLowerCase()) < 0) { return false; };
          };
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

    this.grid.onHeaderRowCellRendered.subscribe(function (e, args) {
      $(args.node).empty();
      if ((typeof args.column.disableSearch == "undefined" || !(args.column.disableSearch === true)) && !(args.column.id === "_checkbox_selector")) {
        $("<input type='text' placeholder='&#xf002;'>")
          .data("columnId", args.column.id)
          .val(that.columnFilters[args.column.id])
          .appendTo(args.node);
      } else {
        // args.node.style.backgroundColor = "#7CCCFE";
        args.node.style.backgroundColor = "#F6F6F6";
      }
    });

    this.grid.init();

    // set up selection handler
    this.grid.setSelectionModel(new Slick.RowSelectionModel());
    this.grid.onSelectedRowsChanged.subscribe(function (e, args) {
      var selectedIds = that.dataView.mapRowsToIds(that.grid.getSelectedRows());
      if (selectedIds.length == 1) {
        ReactBootstrap.Dispatcher.emit(that.props.component + "list.select", selectedIds[0], that.grid.sortedFlag);
        if(that.grid.sortedFlag){
          that.grid.sortedFlag = false;
        }
      } else {
        ReactBootstrap.Dispatcher.emit(that.props.component + "list.multiSelect", selectedIds);
      }
      // if(that.grid.sortedFlag === false){
      //   if (selectedIds.length == 1) {
      //     ReactBootstrap.Dispatcher.emit(that.props.component + "list.select", selectedIds[0]);
      //   } else {
      //     ReactBootstrap.Dispatcher.emit(that.props.component + "list.multiSelect", selectedIds);
      //   }
      // }
    });

    // initialize the model after all the events have been hooked up
    this.dataView.beginUpdate();
    this.dataView.setItems(this.props.dataArray, this.props.dataIdField);
    this.dataView.setFilter(columnFilter);
    this.dataView.endUpdate();

    this.dataView.syncGridSelection(this.grid, true);
  },

  componentWillUnmount: function () {
    $(window).off('resize', this.handleResize);
    ReactBootstrap.Dispatcher.removeAllListeners(this.props.component + 'form.delete.success');
    ReactBootstrap.Dispatcher.removeAllListeners(this.props.component + 'form.add.success');
    ReactBootstrap.Dispatcher.removeAllListeners(this.props.component + 'form.update.success');
    ReactBootstrap.Dispatcher.removeAllListeners(this.props.component + 'list.update.success');
    ReactBootstrap.Dispatcher.removeAllListeners(this.props.component + 'form.selectedRows.update');
    ReactBootstrap.Dispatcher.removeAllListeners("Listeditor.selectedColumns");
    ReactBootstrap.Dispatcher.removeAllListeners("Listeditor.selectedColumnsCommissioning");

    ReactBootstrap.Dispatcher.removeAllListeners(this.props.component + 'list.selectrow');
    if (typeof this.dataView != "undefined") {
      this.dataView = null;
    };
    if (typeof this.grid != "undefined") {
      this.grid.destroy();
      this.grid = null;
    };

  },

  render: function () {
    if (!this.checkData()) {
      return (<div><b>{this.checkDataErrMsg}</b></div>);
    }

    var gridDOMId = this.props.component + "-grid",
      gridFooterId = this.props.component + "-grid-footer";
    return (
      <div>
        <div id={gridDOMId} width="100%"></div>
        <div style={{ fontSize: "16px", fontWeight: "bold", paddingLeft: "19px", marginTop:"20px" }} id={gridFooterId}></div>
      </div>
    );
  }

});
module.exports = DataGrid;