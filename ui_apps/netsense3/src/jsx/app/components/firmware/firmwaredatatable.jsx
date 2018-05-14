import classNames from 'classnames';
import { Link } from 'react-router';
import auth from 'global/utils/auth';
import helpers from 'global/utils/helpers';

var Firmwaredatatable = React.createClass({
  propTypes: {
    firmwares: React.PropTypes.array.isRequired,
    firmwareID: React.PropTypes.string.isRequired
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
      info: "_TOTAL_ Firmware versions",
      infoEmpty: ""
      }
    },

  setHandlers() {
    var table = $("#firmware-table").DataTable();
    table.on( 'select', function ( e, dt, type, indexes ) {
      e.stopPropagation();
      e.preventDefault();
      $("#firmware-table tbody tr").removeClass("selected");      
      var row = table[ type ]( indexes ).nodes().to$();
      row.addClass("selected")
      ReactBootstrap.Dispatcher.emit("Firmwarelist.select", row.attr("data-firmwareid"));
      });
  },

  buildTable(selector){
    $(selector).dataTable(this.tableOptions);
    $(selector + " tbody tr").removeClass("selected");
    $(selector + " tbody tr[data-firmwareid='"+this.props.firmwareID+"']").addClass("selected");
    this.setHandlers();
  },
  
  componentWillReceiveProps(){
    $("#firmware-table").DataTable().destroy();
  },
  
  componentDidUpdate() {
    this.buildTable("#firmware-table");
  },
  
  componentDidMount() {
    this.tableOptions.scrollY = helpers.calcHeight(60,80);
    this.tableOptions.language.emptyTable = ("No Firmware versions have been defined."
                  + (auth.allowed('CAN_CREATE', 'ScheduleModel')?"<br />Use the green button to add a version.":""));
    this.buildTable("#firmware-table");
  },

  render(){
    var that = this;
    var Firmwaretablerows = this.props.firmwares.map(function(firmware, index){
      var firmwareID = firmware.firmwareid;
      return (
        <tr key={index} data-idx={index} data-firmwareid={firmwareID} style={{cursor:"pointer"}}>
          <td>
            {firmware.name}
          </td>
          <td>
            {firmware.release}
          </td>
        </tr>
      );
  });
  return (
      <table id="firmware-table" className="table table-condensed table-hover table-striped" width="100%">
        <thead><tr>
          <th data-column-id="firmwarename" style={{width:"40%"}}>Name</th>
          <th data-column-id="firmwaredescription">Release</th>
        </tr></thead>
        <tbody>
          {Firmwaretablerows}
        </tbody>
      </table>
    );
  }

});
module.exports = Firmwaredatatable;

