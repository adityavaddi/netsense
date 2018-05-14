import classNames from 'classnames';
import { State, Navigation } from 'react-router';
import auth from 'global/utils/auth';
import helpers from 'global/utils/helpers';

var Groupdatatable = React.createClass({

  propTypes: {
    groups: React.PropTypes.array.isRequired,
    groupID: React.PropTypes.string
  },

  tableOptions: {
    paging: false,
    select: "single",
    language: {
      search: "_INPUT_",
      searchPlaceholder: "Search...",
      info: "_TOTAL_ groups",
      infoEmpty: ""
      }
    },

  setHandlers() {
    var table = $("#group-table").DataTable();
    table.on( 'select', function ( e, dt, type, indexes ) {
      e.stopPropagation();
      e.preventDefault();
      $("#group-table tbody tr").removeClass("selected"); 
      var row = table[ type ]( indexes ).nodes().to$();
      row.addClass("selected");
      ReactBootstrap.Dispatcher.emit("Grouplist.select", row.attr("data-groupid"));
      });
  },
  
  buildTable(selector){
    $(selector).dataTable(this.tableOptions);
    $(selector + " tbody tr").removeClass("selected");
    $(selector + " tbody tr[data-groupid='"+this.props.groupID+"']").addClass("selected");
    this.setHandlers();
  },

  componentWillReceiveProps(){
    $("#group-table").DataTable().destroy();
  },

  componentDidUpdate() {
    this.buildTable("#group-table");
  },
  
  componentDidMount() {
    this.tableOptions.scrollY = helpers.calcHeight(60,80);
    this.tableOptions.language.emptyTable = ("No Groups have been defined."
                  + (auth.allowed('CAN_CREATE', 'GroupModel')?"<br />Use the green button to add a Group.":""));
    this.buildTable("#group-table");
  },

  render(){
    var that = this;
    var Grouptablerows = this.props.groups.map(function(group, index){
      var groupicon = "", siteicon = "";
      if (group.type=="lighting") {
        groupicon = (<img src='/imgs/light-default.png' width='16' title='Lighting Group' />);
      };
      if (group.type=="organizational") {
        groupicon = (<img src='/imgs/new-icons/orggroup-icon.svg' width='16' title='Organizational Group' />);
      };
      if (group.type=="site-lighting") {
        groupicon = (<img src='/imgs/light-default.png' width='16' title='Site Default Lighting Group' />);
        siteicon = (<Icon glyph="icon-fontello-commerical-building" 
                       style={{fontSize:"16px",marginLeft:"6px"}} title='Site Default Lighting Group' />);
      };
      return (
        <tr key={index} data-idx={index} data-groupid={group.groupid} style={{cursor:"pointer"}}>
          <td className="dt-center">
            {groupicon}
          </td>
          <td>
            {group.name}  {siteicon}
          </td>
          <td>
            {group.description}
          </td>
        </tr>
      );
    });
    return (
      <table id="group-table" className="table table-condensed table-hover table-striped" width="100%">
        <thead><tr>
          <th data-column-id="grouptype" style={{width:"16px"}}>Type</th>
          <th data-column-id="groupid">Name</th>
          <th data-column-id="groupdescription">Description</th>
        </tr></thead>
        <tbody>
          {Grouptablerows}
        </tbody>
      </table>
    );
  }


});
module.exports = Groupdatatable;

