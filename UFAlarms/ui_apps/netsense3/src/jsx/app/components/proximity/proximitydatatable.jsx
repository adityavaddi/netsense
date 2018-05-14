import classNames from 'classnames';
import { Link } from 'react-router';
import auth from 'global/utils/auth';
import helpers from 'global/utils/helpers';

var Proximitydatatable = React.createClass({
  propTypes: {
    proximitys: React.PropTypes.array.isRequired,
    proximityID: React.PropTypes.string.isRequired
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
      info: "_TOTAL_ Proximity Dimming profiles",
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
    var table = $("#proximity-table").DataTable();
    table.on( 'select', function ( e, dt, type, indexes ) {
      e.stopPropagation();
      e.preventDefault();
      $("#proximity-table tbody tr").removeClass("selected");      
      var row = table[ type ]( indexes ).nodes().to$();
      row.addClass("selected")
      ReactBootstrap.Dispatcher.emit("Proximitylist.select", row.attr("data-proximityid"));
      });
  },

  buildTable(selector){
    $(selector).dataTable(this.tableOptions);
    $(selector + " tbody tr").removeClass("selected");
    $(selector + " tbody tr[data-proximityid='"+this.props.proximityID+"']").addClass("selected");
    this.setHandlers();
  },
  
  componentWillReceiveProps(){
    $("#proximity-table").DataTable().destroy();
  },
  
  componentDidUpdate() {
    this.buildTable("#proximity-table");
  },
  
  componentDidMount() {
    this.tableOptions.scrollY = helpers.calcHeight(60,80);
    this.tableOptions.language.emptyTable = ("No Proximity Dimming profiles have been defined."
                  + (auth.allowed('CAN_CREATE', 'ScheduleModel')?"<br />Use the green button to add a profile.":""));

    this.buildTable("#proximity-table");
  },

  render(){
    var that = this;
    var Proximitytablerows = this.props.proximitys.map(function(proximity, index){
      var proximityID = proximity.pdprofileid;
      return (
        <tr key={index} data-idx={index} data-proximityid={proximityID} style={{cursor:"pointer"}}>
          <td>
            {proximity.name}
          </td>
          <td>
            {proximity.description}
          </td>
        </tr>
      );
  });
  return (
      <table id="proximity-table" className="table table-condensed table-hover table-striped" width="100%">
        <thead><tr>
          <th data-column-id="proximityname" style={{width:"40%"}}>Name</th>
          <th data-column-id="proximitydescription">Description</th>
        </tr></thead>
        <tbody>
          {Proximitytablerows}
        </tbody>
      </table>
    );
  }

});
module.exports = Proximitydatatable;

