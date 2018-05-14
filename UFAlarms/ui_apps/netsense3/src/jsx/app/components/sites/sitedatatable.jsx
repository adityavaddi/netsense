import classNames from 'classnames';
import { Link, withRouter } from 'react-router';
import helpers from 'global/utils/helpers';

var Sitedatatable = React.createClass({
  propTypes: {
    sites: React.PropTypes.array.isRequired,
    siteID: React.PropTypes.string.isRequired,
    concise:React.PropTypes.bool,
  },

  getDefaultProps: function () { 
    return {
      concise:false,
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
      info: "_TOTAL_ sites",
      infoEmpty: ""
      }
    },

  handleNodes: function(e) {
    e.preventDefault();
    var siteID = e.currentTarget.getAttribute("data-siteid"),
        siteName = e.currentTarget.getAttribute("data-sitename");
    ReactBootstrap.Dispatcher.emit("Sitelist.select", siteID, siteName);
    NSN.siteID = siteID;
    sessionStorage.setItem("siteID", NSN.siteID);
    this.props.router.push("/app/nodepanel")
  },
  
  handleGroups: function(e) {
    e.preventDefault();
    var siteID = e.currentTarget.getAttribute("data-siteid"),
        siteName = e.currentTarget.getAttribute("data-sitename");
    ReactBootstrap.Dispatcher.emit("Sitelist.select", siteID, siteName);
    NSN.siteID = siteID;
    sessionStorage.setItem("siteID", NSN.siteID);
    this.props.router.push("/app/grouppanel")
  },


  setHandlers: function() {
    var table = $("#site-table").DataTable();
    table.on( 'select', function ( e, dt, type, indexes ) {
      e.stopPropagation();
      e.preventDefault();
      $("#site-table tbody tr").removeClass("selected");      
      var row = table[ type ]( indexes ).nodes().to$();
      row.addClass("selected");
      ReactBootstrap.Dispatcher.emit("Sitelist.select", row.attr("data-siteid"), row.attr("data-sitename"));
      });

  },

  buildTable: function(selector){
    $(selector).dataTable(this.tableOptions);
    $(selector + " tbody tr").removeClass("selected");
    $(selector + " tbody tr[data-siteid='"+this.props.siteID+"']").addClass("selected");
    this.setHandlers();
  },
  
  componentWillReceiveProps: function(){
    $("#site-table").DataTable().destroy();
  },
  
  componentDidUpdate: function() {
    if(!this.props.concise){
      this.buildTable("#site-table");
    }
    else{    
      $("#site-table").dataTable({
        scrollY: helpers.calcHeight(27,27),
        paging: false,
        bFilter: false,
        info: false
      });
      this.setHandlers();
    }
  },
  
  componentDidMount: function() {
    if(!this.props.concise){
      this.tableOptions.scrollY = helpers.calcHeight(60,80);
      this.buildTable("#site-table");
    }
    else{    
      $("#site-table").dataTable({
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
    var Sitetablerows = this.props.sites.map(function(site, index){
      var siteID = site.siteid, siteName = site.name;
      var groupTip = "View " + siteName + " Groups",
          nodeTip = "View " + siteName + " Nodes";
      return (
        <tr key={index} data-idx={index} data-siteid={siteID} data-sitename={siteName} style={{cursor:"pointer"}} >
          <td>
            {site.name}
          </td>
          <td>
            {site.city}
          </td>
          <td>
            {site.state}
          </td>
          <td className="dt-center">
            <span onClick={that.handleGroups} data-siteid={siteID} data-sitename={siteName} title={groupTip}>
            <Icon glyph="icon-feather-share" style={{fontSize:"16px",position:"relative",top:"-2px"}}/>
            </span> &nbsp;
          </td>
          <td className="dt-center">
            <span onClick={that.handleNodes} data-siteid={siteID} data-sitename={siteName} title={nodeTip}>
            <Icon glyph="icon-fontello-dot-circled" style={{fontSize:"16px"}}/>
            </span>
          </td>
        </tr>
      );
  });

  if(this.props.concise){
      return (
        <table id="site-table" className="table table-condensed table-hover table-striped" style={{margin:"0px"}}>
          <thead><tr>
            <th data-column-id="sitename" style={{width:"40%"}}>Name</th>
            <th data-column-id="sitecity" style={{width:"20%"}}>City</th>
            <th data-column-id="sitestate" style={{width:"12%"}}>State</th>
            <th data-column-id="sitegroups" className="dt-center" style={{width:"14%"}}>Groups</th>
            <th data-column-id="sitesnodes" className="dt-center">Nodes</th>
          </tr></thead>
          <tbody>
            {Sitetablerows}
          </tbody>
        </table>
      );
    }
    else{
      return (
        <table id="site-table" className="table table-condensed table-hover table-striped">
          <thead><tr>
            <th data-column-id="sitename" style={{width:"40%"}}>Name</th>
            <th data-column-id="sitecity" style={{width:"20%"}}>City</th>
            <th data-column-id="sitestate" style={{width:"12%"}}>State</th>
            <th data-column-id="sitegroups" className="dt-center" style={{width:"14%"}}>Groups</th>
            <th data-column-id="sitesnodes" className="dt-center">Nodes</th>
          </tr></thead>
          <tbody>
            {Sitetablerows}
          </tbody>
        </table>
      );
    }

  
  }


});
module.exports = withRouter(Sitedatatable);