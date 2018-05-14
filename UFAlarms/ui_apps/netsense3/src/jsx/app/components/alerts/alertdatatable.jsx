
import classNames from 'classnames';
import { Link,} from 'react-router';
import helpers from 'global/utils/helpers';

var Alertdatatable = React.createClass({

  propTypes: {
    alerts: React.PropTypes.array.isRequired,
    alertID: React.PropTypes.string.isRequired,
    concise:React.PropTypes.bool
  },

  getDefaultProps: function () { 
    return {
      concise:false
    };
  },


  calcHeight: function(pct, extra) {
    var h = ($(window).height() - 130) * (pct/100) + extra;
    return h;
  },

  tableOptions: {
    paging: false,
    select: "single",
    language: {
      search: "_INPUT_",
      searchPlaceholder: "Search...",
      info: "_TOTAL_ alerts",
      infoEmpty: "",
      select: {
        rows: {
          _: "%d selected",
          0: "none selected"
        }
      }
    }
  },

  setHandlers() {
    var table = $("#alert-table").DataTable();
    table.on( 'select', function ( e, dt, type, indexes ) {
      $("#alert-table tbody tr").removeClass("selected");
      var row = table[ type ]( indexes ).nodes().to$();
      row.addClass("selected");
      console.log("alertdatatable.jsx Emitting Alertlist.select", type, indexes);
      ReactBootstrap.Dispatcher.emit("Alertlist.select", row.attr("data-alertid"));
      e.stopPropagation();
      e.preventDefault();
    });

  },

  buildTable(selector) {
    $(selector).dataTable(this.tableOptions);
    $(selector + " tbody tr").removeClass("selected");
    $(selector + " tbody tr[data-alertid='"+this.props.alertID+"']").addClass("selected");
    this.setHandlers();
  },

  componentWillReceiveProps(){
    $("#alert-table").DataTable().destroy();
  },

  
  componentDidUpdate() {
    if(!this.props.concise){
      //$("#notification-table").dataTable({
      //  scrollY: helpers.calcHeight(60,80),
      //  paging: false,
      //  select: "single",
      //  language: {
      //    search: "_INPUT_",
      //    searchPlaceholder: "Search..."
      //  }
      //});

      this.buildTable("#alert-table");
      //$("#notification-table tbody tr[data-notificationid='"+this.props.notificationID+"']").addClass("selected");
    }
    else {
      $("#alert-table").dataTable({
        scrollY: helpers.calcHeight(27, 27),
        paging: false,
        bFilter: false,
        info: false
      });
      this.setHandlers();
    }
  },

    componentDidMount() {

    if(!this.props.concise){

      this.tableOptions.scrollY = helpers.calcHeight(27,27);
      this.buildTable("#alert-table");
      //
      //$("#notification-table").dataTable({
      //  scrollY: helpers.calcHeight(60,80),
      //  paging: false,
      //  select: "single",
      //  language: {
      //    search: "_INPUT_",
      //    searchPlaceholder: "Search..."
      //  }
      //});

      //$("#notification-table tbody tr[data-notificationid='"+this.props.notificationID+"']").addClass("selected");
    }
    else{
      $("#alert-table").dataTable({
        scrollY: helpers.calcHeight(27,27),
        paging: false,
        bFilter: false,
        info: false
      });
      this.setHandlers();
    }
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

    var that = this;

    var Alerttablerows = this.props.alerts.map(function(alert, index){
      var alertID = alert.alertid;
      return (

        <tr key={index} data-idx={index} data-alertid={alertID} style={{cursor:"pointer"}}>
          <td>
            {alert.type}
          </td>
          <td>
            {alert.severity}
          </td>
           <td>
            {alert.orgid}
          </td>
          <td>
            {alert.siteid}
          </td>
           <td>
            {alert.nodeid}
          </td>
          <td>
            {new Date(alert.updated).toString()}
          </td>
         
        </tr>
      );
    });

    if(this.props.concise){
      return (
      <table id="alert-table" className="table table-condensed table-hover table-striped" width="100%" style={{margin:"0px"}}>
        <thead><tr>
          <th data-column-id="type">Alarm Type</th>
          <th data-column-id="severity">Severity</th>
          <th data-column-id="orgid">Customer</th>
          <th data-column-id="siteid">Site</th>
          <th data-column-id="nodeid">Node</th>
          <th data-column-id="date">Date & Time </th>
        </tr></thead>
        <tbody>
          {Alerttablerows}
        </tbody>
      </table>
      );
    }
    else{
    return (
      <table id="alert-table" className="table table-condensed table-hover table-striped">
        <thead><tr>
          <th data-column-id="type">Alarm Type</th>
          <th data-column-id="severity">Severity</th>
          <th data-column-id="orgid">Customer</th>
          <th data-column-id="siteid">Site</th>
          <th data-column-id="nodeid">Node</th>
          <th data-column-id="date">Date & Time</th>
        </tr></thead>
        <tbody>
          {Alerttablerows}
        </tbody>
      </table>
      );
    }
  }
});
module.exports = Alertdatatable;





