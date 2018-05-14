import classNames from 'classnames';
import { Link } from 'react-router';
import auth from 'global/utils/auth';
import helpers from 'global/utils/helpers';

var Configdatatable = React.createClass({
  propTypes: {
    configs: React.PropTypes.array.isRequired,
    configID: React.PropTypes.string.isRequired
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
      info: "_TOTAL_ configs",
      infoEmpty: ""
      }
    },

  setHandlers() {
    var table = $("#config-table").DataTable();
    table.on( 'select', function ( e, dt, type, indexes ) {
      e.stopPropagation();
      e.preventDefault();
      $("#config-table tbody tr").removeClass("selected");      
      var row = table[ type ]( indexes ).nodes().to$();
      row.addClass("selected")
      ReactBootstrap.Dispatcher.emit("Configlist.select", row.attr("data-configid"));
      });
  },

  buildTable(selector){
    $(selector).dataTable(this.tableOptions);
    $(selector + " tbody tr").removeClass("selected");
    $(selector + " tbody tr[data-configid='"+this.props.configID+"']").addClass("selected");
    this.setHandlers();
  },
  
  componentWillReceiveProps(){
    $("#config-table").DataTable().destroy();
  },
  
  componentDidUpdate() {
    this.buildTable("#config-table");
  },
  
  componentDidMount() {
    this.tableOptions.scrollY = helpers.calcHeight(60,80);
    this.tableOptions.language.emptyTable = ("No Configs have been defined."
                  + (auth.allowed('CAN_CREATE', 'OrgModel')?"<br />Use the green button to add a Config.":"")),
    this.buildTable("#config-table");
  },

  render(){
    var that = this;
    var Configtablerows = this.props.configs.map(function(config, index){
      var configID = config.configid;
      return (
        <tr key={index} data-idx={index} data-configid={configID} style={{cursor:"pointer"}}>
          <td>
            {config.name}
          </td>
          <td>
            {config.model}
          </td>
          <td>
            {config.nodes}
          </td>
        </tr>
      );
  });
  return (
      <table id="config-table" className="table table-condensed table-hover table-striped" width="98%">
        <thead><tr>
          <th data-column-id="configname" style={{width:"40%"}}>Name</th>
          <th data-column-id="configmodel" style={{width:"40%"}}>Model</th>
          <th data-column-id="configname" style={{width:"20%"}}>Nodes</th>
        </tr></thead>
        <tbody>
          {Configtablerows}
        </tbody>
      </table>
    );
  }

});
module.exports = Configdatatable;

