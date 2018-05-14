import classNames from 'classnames';
import { Link} from 'react-router';
import auth from 'global/utils/auth';
import helpers from 'global/utils/helpers';

var alertdatatable = React.createClass({
  propTypes: {
    alerts: React.PropTypes.array.isRequired,
    alertID: React.PropTypes.string.isRequired,
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
    {name:"Type", field:"type", id:"type", 
      sortable:true, width:100},
    {name:"Severity", field:"severity", id:"severity", 
      sortable:true, width:150},
    {name:"Orgid", field:"orgid", id:"orgid", 
      sortable:true, width:150},
    {name:"Siteid", field:"siteid", id:"siteid", 
      sortable:true, width:150},
    {name:"Nodeid", field:"nodeid", id:"nodeid", 
      sortable:true, width:150},
    {name:"Date & Time", field:"updated", id:"updated", 
      sortable:true, width:150},        
  ],
     
  componentDidUpdate() {
    this.grid.setData(this.props.alerts, "alertid");
    this.grid.invalidate();
  },
  
  componentDidMount() {
    var that = this;
    var searchList = [];
    $("#alert-table").height(helpers.calcHeight(100,-150));
    
    // initialize grid and dataview
    this.dataView = new Slick.Data.DataView();
    this.grid = new Slick.Grid("#alert-table", this.dataView, this.columns, this.gridOptions);
    this.grid.setData(this.props.alerts, "alertid");
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
        ReactBootstrap.Dispatcher.emit(that.props.component+"list.select", selectedData[0].alertid);
      } else {
        ReactBootstrap.Dispatcher.emit(that.props.component+"list.multiSelect", 
          selectedData.map(function(alert) {
            return alert.alertid;
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

      that.props.alerts.sort(function(a, b){
          var result = 
              a[field] > b[field] ? 1 :
              a[field] < b[field] ? -1 :
              0;

          return args.sortAsc ? result : -result;
      });

      that.grid.invalidate();         
    });

     // wire up the search textbox to apply the filter to the model
    $("#alert-search").keyup(function (e) {
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
  this.dataView.setItems(this.props.alerts, "alertid");
  this.dataView.setFilterArgs({
    searchList: searchList
  });
  this.dataView.setFilter(gridFilter);
  this.dataView.endUpdate();

  this.dataView.syncGridSelection(this.grid, true);

  },

  render(){

    var alertsData = this.props.alerts;
    var customersData = this.props.customers;
    var sitesData = this.props.sites;

    // Convert customerid to readable format:
    for(var i=0;i<alertsData.length;i++){
      for(var j=0;j<customersData.length;j++){
        if(alertsData[i].orgid == customersData[j].orgid){
          console.log(customersData[j].name);
          alertsData[i].orgid = customersData[j].name;
        }
      }
    }

    // Convert siteid to readable format:
    for(var i=0;i<alertsData.length;i++){
      for(var j=0;j<sitesData.length;j++){
        if(alertsData[i].siteid == sitesData[j].siteid){
          console.log(sitesData[j].name);
          alertsData[i].siteid = sitesData[j].name;
        }
      }
    }

    // Convert date & time to readable format:
    for(var i=0;i<alertsData.length;i++){
      alertsData[i].updated = new Date(alertsData[i].updated).toString();
    }    

    return (
      <div>
        <div style={{textAlign:"right",fontSize:"16px",display:"none"}}>
          <input type="text" name="alert-search" id="alert-search" 
              style={{width:"40%",border:"1px solid silver",marginBottom:"10px"}} placeholder=" Search..." />
          </div>
        <div id="alert-table" width="100%">
        </div>
      </div>
    );
  }

});
module.exports = alertdatatable;

