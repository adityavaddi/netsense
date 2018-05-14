import classNames from 'classnames';
import { Link, withRouter } from 'react-router';
import auth from 'global/utils/auth';
import helpers from 'global/utils/helpers';

var Customerdatatable = React.createClass({
  propTypes: {
    customers: React.PropTypes.array.isRequired,
    customerID: React.PropTypes.string.isRequired
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
      info: "_TOTAL_ customers",
      infoEmpty: ""
      }
    },

  handleSites(e) {
    e.preventDefault();
    NSN.customerID = e.currentTarget.getAttribute("data-customerid");
    sessionStorage.setItem("customerID", NSN.customerID)
    this.props.router.push("/app/sitepanel")
  },

  setHandlers() {
    var table = $("#customer-table").DataTable();
    table.on( 'select', function ( e, dt, type, indexes ) {
      e.stopPropagation();
      e.preventDefault();
      $("#customer-table tbody tr").removeClass("selected");
      var row = table[ type ]( indexes ).nodes().to$();
      row.addClass("selected")
      ReactBootstrap.Dispatcher.emit("Customerlist.select", row.attr("data-customerid"));
      });
  },

  buildTable(selector){
    $(selector).dataTable(this.tableOptions);
    $(selector + " tbody tr").removeClass("selected");
    $(selector + " tbody tr[data-customerid='"+this.props.customerID+"']").addClass("selected");
    this.setHandlers();
  },

  componentWillReceiveProps(){
    $("#customer-table").DataTable().destroy();
  },

  componentDidUpdate() {
    this.buildTable("#customer-table");
  },

  componentDidMount() {
    this.tableOptions.scrollY = helpers.calcHeight(60,80);
    this.tableOptions.language.emptyTable = ("No Customers have been defined."
                  + (auth.allowed('CAN_CREATE', 'OrgModel')?"<br />Use the green button to add a Customer.":"")),
    this.buildTable("#customer-table");
  },

  render(){
    var that = this;
    var Customertablerows = this.props.customers.map(function(customer, index){
      var customerID = customer.orgid;
      return (
        <tr key={index} data-idx={index} data-customerid={customerID} style={{cursor:"pointer"}}>
          <td>
            {customer.name}
          </td>
          <td>
            {customer.type}
          </td>
          <td>
            {customer.city}
          </td>
          <td>
            {customer.state}
          </td>
          <td className="dt-center">
            <div onClick={that.handleSites} data-customerid={customerID} className="pulse" title="View Sites">
              <Icon glyph="icon-fontello-commerical-building" style={{fontSize:"16px"}}/>
            </div>
          </td>
        </tr>
      );
  });
  return (
      <table id="customer-table" className="table table-condensed table-hover table-striped" width="98%">
        <thead><tr>
          <th data-column-id="customername" style={{width:"40%"}}>Name</th>
          <th data-column-id="customertype" style={{width:"15%"}}>Type</th>
          <th data-column-id="customercity" style={{width:"20%"}}>City</th>
          <th data-column-id="customerstate" style={{width:"10%"}}>State</th>
          <th data-column-id="customersites" className="dt-center">Sites</th>
        </tr></thead>
        <tbody>
          {Customertablerows}
        </tbody>
      </table>
    );
  }

});
module.exports = withRouter(Customerdatatable);