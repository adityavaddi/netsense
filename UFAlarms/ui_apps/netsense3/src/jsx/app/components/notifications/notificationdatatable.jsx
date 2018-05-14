
import classNames from 'classnames';
import { State, Navigation } from 'react-router';
import helpers from 'global/utils/helpers';

var Notificationdatatable = React.createClass({

  propTypes: {
    notifications: React.PropTypes.array.isRequired,
    notificationID: React.PropTypes.string.isRequired,
    concise:React.PropTypes.bool
  },

  getDefaultProps: function () { 
    return {
      concise:false
    };
  },


  calcHeight: function(pct, extra) {
    var h = (window && window.document)?(($(window).height() - 75) * (pct/100) + extra):500;
    return h;
  },

  tableOptions: {
    paging: false,
    select: "single",
    language: {
      search: "_INPUT_",
      searchPlaceholder: "Search...",
      info: "_TOTAL_ notifications",
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
    var table = $("#notification-table").DataTable();
    table.on( 'select', function ( e, dt, type, indexes ) {
      $("#notification-table tbody tr").removeClass("selected");
      var row = table[ type ]( indexes ).nodes().to$();
      row.addClass("selected");
      console.log("notificationdatatable.jsx Emitting Notificationlist.select", type, indexes);
      ReactBootstrap.Dispatcher.emit("Notificationlist.select", row.attr("data-notificationid"));
      e.stopPropagation();
      e.preventDefault();
    });

  },

  buildTable(selector) {
    $(selector).dataTable(this.tableOptions);
    $(selector + " tbody tr").removeClass("selected");
    $(selector + " tbody tr[data-notificationid='"+this.props.notificationID+"']").addClass("selected");
    this.setHandlers();
  },

  componentWillReceiveProps(){
    $("#notification-table").DataTable().destroy();
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
      this.buildTable("#notification-table");
      //$("#notification-table tbody tr[data-notificationid='"+this.props.notificationID+"']").addClass("selected");
    }
    else {
      $("#notification-table").dataTable({
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
      this.buildTable("#notification-table");
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
      $("#notification-table").dataTable({
        scrollY: helpers.calcHeight(27,27),
        paging: false,
        bFilter: false,
        info: false
      });
      this.setHandlers();
    }
  },


  render(){

    var that = this;

    if(this.props.concise){
      var Notificationtablerows = this.props.notifications.map(function(notification, index){
        var notificationID = notification.notificationid;

        return (

          <tr key={index} data-idx={index} data-notificationid={notificationID} style={{cursor:"pointer"}}>
            <td>
              {notification.name}
            </td>
            <td>
              {notification.active?"Yes":"No"}
            </td>
          </tr>
        );
      });

      return (
        <table id="notification-table" className="table table-condensed table-hover table-striped" width="100%" style={{margin:"0px"}}>
          <thead><tr>
            <th data-column-id="notificationname">Name</th>
            <th data-column-id="notificationactive">Active</th>
          </tr></thead>
          <tbody>
            {Notificationtablerows}
          </tbody>
        </table>
        );
    } else {
      var Notificationtablerows = this.props.notifications.map(function(notification, index){
        var notificationID = notification.notificationid;

        return (

          <tr key={index} data-idx={index} data-notificationid={notificationID} style={{cursor:"pointer"}}>
            <td>
              {notification.name}
            </td>
            <td>
              {notification.active}
            </td>
            <td id="notificationtyperow">
              {notification.notificationtype}
            </td>
            <td id="notificationseverityrow">
              {notification.severity}
            </td>
          </tr>
        );
      });

    return (
      <table id="notification-table" className="table table-condensed table-hover table-striped">
        <thead><tr>
          <th data-column-id="notificationname">Name</th>
          <th data-column-id="notificationactive">Active</th>
          <th data-column-id="notificationtype">Type</th>
          <th data-column-id="notificationseverity">Severity</th>
        </tr></thead>
        <tbody>
          {Notificationtablerows}
        </tbody>
      </table>
      );
    }
  }
});

module.exports = Notificationdatatable;





