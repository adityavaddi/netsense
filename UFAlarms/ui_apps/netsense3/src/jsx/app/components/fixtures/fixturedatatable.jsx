
import classNames from 'classnames';
import { State, Navigation } from 'react-router';
import auth from 'global/utils/auth';
import helpers from 'global/utils/helpers';

var Fixturedatatable = React.createClass({

  propTypes: {
    fixtures: React.PropTypes.array.isRequired,
    fixtureID: React.PropTypes.string.isRequired,
    fixturetypes: React.PropTypes.array.isRequired,
  },

  tableOptions: {
    paging: false,
    select: "single",
    language: {
      search: "_INPUT_",
      searchPlaceholder: "Search...",
      info: "_TOTAL_ fixtures",
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
    var table = $("#fixture-table").DataTable();
    table.on( 'select', function ( e, dt, type, indexes ) {
      e.stopPropagation();
      e.preventDefault();
      $("#fixture-table tbody tr").removeClass("selected"); 
      var row = table[ type ]( indexes ).nodes().to$();
      row.addClass("selected");
      ReactBootstrap.Dispatcher.emit("Fixturelist.select", row.attr("data-fixtureid"));
      });
  },
  
  buildTable(selector){
    $(selector).dataTable(this.tableOptions);
    $(selector + " tbody tr").removeClass("selected");
    $(selector + " tbody tr[data-fixtureid='"+this.props.fixtureID+"']").addClass("selected");
    this.setHandlers();
  },

  componentWillReceiveProps(){
    $("#fixture-table").DataTable().destroy();
  },

  componentDidUpdate() {
    this.buildTable("#fixture-table");
  },

  componentDidMount() {
    this.tableOptions.scrollY = helpers.calcHeight(60,80);
    this.tableOptions.language.emptyTable = ("No Fixtures have been defined."
                  + (auth.allowed('CAN_CREATE', 'FixtureModel')?"<br />Use the green button to define a Fixture.":""));
    this.buildTable("#fixture-table");
  },

  render(){
    var that = this;
    var Fixturetablerows = this.props.fixtures.map(function(fixture, index){
      var fixtureID = fixture.fixtureid;
      return (
        <tr key={index} data-idx={index} data-fixtureid={fixtureID} style={{cursor:"pointer"}}>
          <td> <img style={{height:"35px"}} src={fixture.icon} /> </td>
          <td>
            {fixture.name}
          </td>
          <td>
            {fixture.description}
          </td>
        </tr>
      );
    });
    return (
      <table id="fixture-table" className="table table-condensed table-hover table-striped" width="100%">
        <thead><tr>
          <th data-column-id="fixtureicon">Icon</th>
          <th data-column-id="fixturename">Name</th>
          <th data-column-id="fixturedescription">Description</th>
        </tr></thead>
        <tbody>
          {Fixturetablerows}
        </tbody>
      </table>
    );
  }
});
module.exports = Fixturedatatable;



