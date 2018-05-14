import classNames from 'classnames';
import { State, Navigation } from 'react-router';
import auth from 'global/utils/auth';
import helpers from 'global/utils/helpers';

var Scheduledatatable = React.createClass({

  propTypes: {
    schedules: React.PropTypes.array.isRequired,
    scheduleID: React.PropTypes.string
  },

  tableOptions:  {
    paging: false,
    select: "single",
    language: {
      search: "_INPUT_",
      searchPlaceholder: "Search...",
      info: "_TOTAL_ schedules",
      infoEmpty: "",
      select: {
          rows: {
              _: "%d selected",
              0: "none selected"
          }
        }
      }
    },

  calcHeight: function(pct, extra) {
    var h = (window && window.document)?(($(window).height() - 75) * (pct/100) + extra):500;
    return h;
  },

 setHandlers() {
    var table = $("#schedule-table").DataTable();
    table.on( 'select', function ( e, dt, type, indexes ) {
      e.stopPropagation();
      e.preventDefault();
      $("#schedule-table tbody tr").removeClass("selected"); 
      var row = table[ type ]( indexes ).nodes().to$();
      row.addClass("selected");
      ReactBootstrap.Dispatcher.emit("Schedulelist.select", row.attr("data-scheduleid"));
      });
  },

  unsetHandlers() {
    var table = $("#schedule-table").DataTable();
    table.off('select');
  },
  
  buildTable(selector){
    $(selector).dataTable(this.tableOptions);
    $(selector + " tbody tr").removeClass("selected");
    $(selector + " tbody tr[data-scheduleid='"+this.props.scheduleID+"']").addClass("selected");
    this.setHandlers();
  },

  componentWillReceiveProps(){
    var table = $("#schedule-table").DataTable();
    table.off('select');
    table.destroy();
  },

  componentDidUpdate() {
    this.buildTable("#schedule-table");
  },
  
  componentDidMount() {
    this.tableOptions.scrollY = helpers.calcHeight(60,80);
    this.tableOptions.language.emptyTable = ("No Schedules have been defined."
                  + (auth.allowed('CAN_CREATE', 'ScheduleModel')?"<br />Use the green button to add a Schedule.":""));
    this.buildTable("#schedule-table");
  },

  render(){
    var that = this;
    var Scheduletablerows = this.props.schedules.map(function(schedule, index){
      var siteicon = "";
      var Selected = (schedule.scheduleid == that.props.scheduleid)?"selected row_selected":"";
      if (schedule.sites.length == 1 && $.isEmptyObject(schedule.sites[0])) {
        schedule.sites = [];
      };
      if (schedule.sites.length > 0) {
        siteicon = (<Icon glyph="icon-fontello-commerical-building" 
                       style={{fontSize:"16px",marginLeft:"6px"}} title='Sitewide Default Schedule' />);
      };
      return (
        <tr key={index} data-idx={index} data-scheduleid={schedule.scheduleid} className={Selected} style={{cursor:"pointer"}}>
          <td>
            {schedule.name}  {siteicon}
          </td>
          <td>
            {schedule.description}
          </td>
        </tr>
      );
    });
    return (
      <table id="schedule-table" className="table table-condensed table-hover table-striped" width="100%">
        <thead><tr>
          <th data-column-id="scheduleid">Name</th>
          <th data-column-id="scheduledescription">Description</th>
        </tr></thead>
        <tbody>
          {Scheduletablerows}
        </tbody>
      </table>
    );
  }


});
module.exports = Scheduledatatable;

