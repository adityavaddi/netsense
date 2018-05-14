import classNames from 'classnames';
import { State, Navigation } from 'react-router';
import auth from 'global/utils/auth';
import helpers from 'global/utils/helpers';

var Nodedatatable = React.createClass({

  propTypes: {
    nodes: React.PropTypes.array.isRequired,
    alerts: React.PropTypes.array.isRequired,
    selected_nodes: React.PropTypes.array.isRequired,
    component: React.PropTypes.string
  },

  calcHeight: function(pct, extra) {
    var h = (window && window.document)?(($(window).height() - 75) * (pct/100) + extra):500;
    h = Math.min(h, 1200); // for mobile devices
    return h;
  },

  tableOptions: {
    paging: false,
    scrollX: true,
    orderCellsTop:true,
    select: {style: "os"},
    language: {
      search: "_INPUT_",
      searchPlaceholder: "Search...",
      info: "_TOTAL_ nodes",
      infoEmpty: "",
      select: {
          rows: {
              _: "%d selected",
              0: "none selected"
          }
        }
      }
    },

  statusAlts: {
    none: "3 - n/a",
    good: "2 - good",
    warn: "1 - warn",
    error: "0 - error"
  },

  isMultiLevel: function(){
     if (this.props.nodes.length < 2) {
       return false;
     };
     var multi = false;
     for (var i=0, first=this.props.nodes[0].level; !multi && i<this.props.nodes.length; i++) {
        multi = this.props.nodes[i].level != first;
     };
     return multi;
  },

  setHandlers() {
/*    ReactBootstrap.Dispatcher.on('updateNode', function(idx , latLng){
      var latColumn = $("#node-table > tbody > tr[data-idx='"+idx+"']>td:nth-child(3)");
      var lonColumn = $("#node-table > tbody > tr[data-idx='"+idx+"']>td:nth-child(4)");
      var oldLatitude = latColumn.removeClass("target-fade");
      var oldLongitude = lonColumn.removeClass("target-fade");
      setTimeout(function(){latColumn.addClass("target-fade");lonColumn.addClass("target-fade");},100);
      oldLatitude.value = latLng.lat();
      oldLongitude.value = latLng.lng();
      var newLatitude = latColumn.html(oldLatitude.value);
      var newLongitude = lonColumn.html(oldLongitude.value);
      $(oldLatitude).replaceWith(newLatitude);
      $(oldLongitude).replaceWith(newLongitude);
    });
*/

    var that = this;
     
    var table = $("#node-table").DataTable();

      // Apply the filter
      $(".dataTables_scrollHeadInner table thead tr#filterRow input").on( 'keyup change', function () {
        table
            .column( $(this).parent().index()+':visible' )
            .search( this.value )
            .draw();
      } );


    table.on( 'select', function ( e, dt, type, indexes ) {
      e.stopPropagation();
      e.preventDefault();
      var rows = [];
      dt.rows('.selected').data().each(function(key){
        rows.push(key[0]);
      });
      console.log((new Date()) + ": " + indexes.length + " rows selected (" + rows + ")");
//      $("#node-table tbody tr").removeClass("selected");
//      var row = table[ type ]( indexes ).nodes().to$();
//      row.addClass("selected");

       // Check for the model:
        var modelrows = [];
        dt.rows('.selected').data().each(function(key){
          modelrows.push(key[2]);
        });

        console.log("Model rows list is " + modelrows);



      if (rows.length == 1) {
        ReactBootstrap.Dispatcher.emit(that.props.component+"list.select", rows[0]);
      } else {
        ReactBootstrap.Dispatcher.emit(that.props.component+"list.multiSelect", rows,modelrows);
      }
      });

  },

  buildTable(selector){
    $(selector).dataTable(this.tableOptions);
    $(selector + " tbody tr").removeClass("selected");
//    $(selector + " tbody tr[data-siteid='"+this.props.nodeID+"']").addClass("selected");
    this.setHandlers();
  },

  modelName(model){
    var modelNames = {"unode-v2":"Core V2","unode-v3":"Core INT","unode-v4":"Core EX","unode-v5":"Core EX-C"};
    return (typeof modelNames[model] == "undefined")?"???":modelNames[model];
  },

  unsetHandlers(){
    var table = $("#node-table").DataTable();
    table.off( 'select' );
 //   ReactBootstrap.Dispatcher.off('updateNode');
  },

  componentWillReceiveProps(){
//    $("#node-table").DataTable().destroy();
  },

  componentDidUpdate() {
/*    $("#node-table").dataTable({
      scrollY: helpers.calcHeight(100,-202),
      paging: false,
      select: "multi",
      language: {
        search: "_INPUT_",
        searchPlaceholder: "Search..."
      }
    });
    $("#node-table").DataTable().rows('.row_selected').deselect();
    $("#node-table tr").removeClass('row_selected');
    for (var i=0; i<this.props.selected_nodes.length; i++) {
      $("#node-table").dataTable().$("tr[data-idx='"+this.props.selected_nodes[i]+"']").addClass('row_selected');
    };
    $("#node-table").DataTable().rows('.row_selected').select();
    if (this.props.selected_nodes.length > 0) {
      $("#node-table-container .dataTables_scrollBody")
        .animate({scrollTop:$("tr[data-idx='"+this.props.selected_nodes[0]+"']").position().top-102},"normal");
    };
//      $("#node-table tr").not(".row_selected").hide();

    this.setHandlers();
*/
  },

  updateSummary(type) {
    var good = 0, warn = 0, error = 0, none=0;
    switch (type) {
      case "network":
        good = $("#node-table td > img[src='/imgs/network-good.png']").length;
        warn = $("#node-table td > img[src='/imgs/network-warn.png']").length;
        error = $("#node-table td > img[src='/imgs/network-error.png']").length;
        none = $("#node-table td > img[src='/imgs/network-none.png']").length;
        $("#nodesummary tr:nth-child(3) td:nth-child(2)").html(good);
        $("#nodesummary tr:nth-child(3) td:nth-child(3)").html(warn);
        $("#nodesummary tr:nth-child(3) td:nth-child(4)").html(error);
        $("#nodesummary tr:nth-child(3) td:nth-child(5)").html(none);
        // no break statement here because we need to update lighting also
      case "light":
        good = $("#node-table td > img[src='/imgs/light-good.png']").length;
        warn = $("#node-table td > img[src='/imgs/light-warn.png']").length;
        error = $("#node-table td > img[src='/imgs/light-error.png']").length;
        none = $("#node-table td > img[src='/imgs/light-none.png']").length;
        $("#nodesummary tr:nth-child(2) td:nth-child(2)").html(good);
        $("#nodesummary tr:nth-child(2) td:nth-child(3)").html(warn);
        $("#nodesummary tr:nth-child(2) td:nth-child(4)").html(error);
        $("#nodesummary tr:nth-child(2) td:nth-child(5)").html(none);
        break;
      case "sensor":
        good = $("#node-table td > img[src='/imgs/sensor-good.png']").length;
        warn = $("#node-table td > img[src='/imgs/sensor-warn.png']").length;
        error = $("#node-table td > img[src='/imgs/sensor-error.png']").length;
        none = $("#node-table td > img[src='/imgs/sensor-none.png']").length;
        $("#nodesummary tr:nth-child(4) td:nth-child(2)").html(good);
        $("#nodesummary tr:nth-child(4) td:nth-child(3)").html(warn);
        $("#nodesummary tr:nth-child(4) td:nth-child(4)").html(error);
        $("#nodesummary tr:nth-child(4) td:nth-child(5)").html(none);
        break;
    }
  },

  componentDidMount() {
    var that = this;
    ReactBootstrap.Dispatcher.on('Nodemap.ConnectionStatus', function(nodeid, status){
      var row = $('#node-table tr[data-nodeid="' + nodeid + '"]');
      var item = row.find('img')[1];
      if (item)
          item.src="/imgs/network-" + status + ".png";
      item = row.find('img')[0];
      var node = null;
      if (item) {
        for (var i=0; i<that.props.nodes.length; i++) {
          if(nodeid === that.props.nodes[i].nodeid)
            node = that.props.nodes[i];
        }
        if(node && (node.scheduleid=="" || node.fixtureid==""))
          status = 'warn';

        item.src="/imgs/light-" + status + ".png";
        item.alt= that.statusAlts[status];
      };
      that.updateSummary("network");
    });
    ReactBootstrap.Dispatcher.on('Nodemap.SensorSample', function(nodeid, status){
      var row = $('#node-table tr[data-nodeid="' + nodeid + '"]');
      var item = row.find('img')[2];
      if (item) {
          item.src="/imgs/sensor-" + status + ".png";
          item.alt= that.statusAlts[status];
        };
      that.updateSummary("sensor");
    });
    ReactBootstrap.Dispatcher.on('Nodemap.LightStatus', function(nodeid, status){
      var row = $('#node-table tr[data-nodeid="' + nodeid + '"]');
      var item = row.find('img')[0];
      if (item) {
          item.src="/imgs/light-" + status + ".png";
          item.alt= that.statusAlts[status];
        };
      that.updateSummary("light");
    });

    this.tableOptions.scrollY = helpers.calcHeight(100,-400);
    this.tableOptions.columnDefs = [{type:"alt-string", targets:this.isMultiLevel()?[4,5,6]:[3,4,5]}];
    if (this.props.component == "Node") {
      this.tableOptions.language.emptyTable = ("No Nodes have been defined."
                  + (auth.allowed('CAN_CREATE', 'NodeModel')?"<br />Use the green button to add a Node.":""));
    } else {
      this.tableOptions.language.emptyTable = "No unassigned nodes are detected.";
    };
    NSN.perfD = new Date();

    $('#node-table thead tr#filterRow th').each( function () {
      var title = $('#node-table thead th').eq( $(this).index()).text();
      $(this).html( '<input type="text" placeholder="Search '+title+'" />' );
    });


    this.buildTable("#node-table");
    NSN.perfE = new Date();
    this.updateSummary("network");
    this.updateSummary("sensor");

    $("#node-table").DataTable().rows('.row_selected').deselect();
    $("#node-table tr").removeClass('row_selected');
    for (var i=0; i<this.props.selected_nodes.length; i++) {
      $("#node-table").dataTable().$("tr[data-idx='"+this.props.selected_nodes[i]+"']").addClass('row_selected');
    };
    $("#node-table").DataTable().rows('.row_selected').select();
    if (this.props.selected_nodes.length > 0) {
      $("node-table").DataTable().row("tr[data-idx='"+this.props.selected_nodes[0]+"']").scrollTo();
    };
//      $("#node-table tr").not(".row_selected").hide();

  },

  componentWillUnmount() {
    this.unsetHandlers();
  },

  render(){

    var searchvisibility = (this.props.minmax == "expand")? {}:{display:"none"};
    var that = this;
    var multiHeader = (<span></span>);
    var multiColumn = (<span></span>);
    if (this.isMultiLevel()) {
      var Nodetablerows = this.props.nodes.map(function(node, index){
        return (
          <tr key={index} data-idx={index} data-nodeid={node.nodeid} style={{cursor:"pointer"}}>
            <td>
              {node.nodeid}
            </td>
            <td style={{overflow:"hidden",textOverflow:"ellipsis"}} title={node.name} >
              {node.name}
            </td>
             <td style={{overflow:"hidden",textOverflow:"ellipsis"}} className="dt-center">
              {that.modelName(node.model)}
            </td>
            <td className="dt-center">
              {node.level}
            </td>
            <td className="dt-center" style={{padding:"0px"}}>
              <img height="20" width="20" title="" alt={node.lig_stat=="on"?"2 - good":"0 - error"}
                src={"/imgs/light-" + (node.lig_stat?((node.lig_stat=="on")?"good":"error"):"none") + ".png"} />
            </td>
            <td className="dt-center" style={{padding:"0px"}}>
              <img height="20" width="20" title="" alt={node.net_stat?"2 - good":"0 - error"}
                src={"/imgs/network-" + (node.net_stat?"good":"error") + ".png"} />
            </td>
            <td className="dt-center" style={{padding:"0px"}}>
              <img height="20" width="20" title="" alt="3 - n/a"
                src={"/imgs/sensor-" + (node.sen_stat?"good":"none") + ".png"} />
            </td>
          </tr>
        );
      });
      return (
        <table id="node-table" className="table table-condensed table-hover table-striped" width="100%">
          <thead><tr>
            <th data-column-id="nodeid" style={{width:"20%"}}>ID</th>
            <th data-column-id="nodename">Name</th>
            <th data-column-id="nodemodel" className="dt-center" style={{width:"20%"}}>Model</th>
            <th data-column-id="nodelevel" className="dt-center" style={{width:"30px"}}>Level</th>
            <th data-column-id="nodelightstatus" style={{width:"24px",padding:"0px 0px 10px 0px"}} className="dt-center"><img width="20" src="/imgs/light-default.png" /></th>
            <th data-column-id="nodenetworkstatus" style={{width:"24px",padding:"0px 0px 10px 0px"}} className="dt-center"><img width="20" src="/imgs/network-default.png" /></th>
            <th data-column-id="nodelsensorstatus" style={{width:"24px",padding:"0px 0px 10px 0px"}} className="dt-center"><img width="20" src="/imgs/sensor-default.png" /></th>
          </tr></thead>
          <tbody>
            {Nodetablerows}
          </tbody>
        </table>
      );
    } else {
    if (typeof NSN != "undefined") NSN.perfC = new Date();
    var Nodetablerows = this.props.nodes.map(function(node, index){
      return (
        <tr key={index} data-idx={index} data-nodeid={node.nodeid} style={{cursor:"pointer"}}>
          <td className="dt-center" style={{whiteSpace:"nowrap",overflow:"hidden",textOverflow:"ellipsis"}}>
            {node.nodeid}
          </td>
            <td className="dt-center" style={{whiteSpace:"nowrap",overflow:"hidden",textOverflow:"ellipsis"}} title={node.name} >
            {node.name}
          </td>
          <td className="dt-center">
            {that.modelName(node.model)}
          </td>
          <td className="dt-center" style={{padding:"0px"}}>
            <img height="20" width="20" title="" alt={node.lig_stat=="on"?"2 - good":"0 - error"}
              src={"/imgs/light-" + (node.lig_stat?((node.lig_stat=="on")?"good":"error"):"none") + ".png"} />
          </td>
          <td className="dt-center" style={{padding:"0px"}}>
            <img height="20" width="20" title="" alt={node.net_stat?"2 - good":"0 - error"}
              src={"/imgs/network-" + (node.net_stat?"good":"error") + ".png"} />
          </td>
          <td className="dt-center" style={{padding:"0px"}}>
            <img height="20" width="20" title="" alt="3 - n/a"
              src={"/imgs/sensor-" + (node.sen_stat?"good":"none") + ".png"} />
          </td>
          <td className="dt-center" style={{overflow:"hidden",textOverflow:"ellipsis"}} >
            {node.fixturename}
          </td>
          <td className="dt-center" style={{overflow:"hidden",textOverflow:"ellipsis"}} >
            {node.groupnamelist}
          </td>
          <td className="dt-center" style={{overflow:"hidden",textOverflow:"ellipsis"}} >
            {node.apn}
          </td>
          <td className="dt-center" style={{overflow:"hidden",textOverflow:"ellipsis"}} >
            {node.softwareVersion}
          </td>
          <td className="dt-center" style={{overflow:"hidden",textOverflow:"ellipsis"}} >
            {node.schedulename}
          </td>
        </tr>
      );
    });
    return (
      <table id="node-table" className="table table-condensed table-hover table-striped">
        <thead><tr>
          <th data-column-id="nodeid" className="dt-center">ID</th>
          <th data-column-id="nodename" className="dt-center">Name</th>
          <th data-column-id="nodemodel" className="dt-center">Model</th>
          <th data-column-id="nodelightstatus" style={{width:"24px",padding:"0px 0px 10px 0px"}} className="dt-center"><img width="20" src="/imgs/light-default.png" /></th>
          <th data-column-id="nodenetworkstatus" style={{width:"24px",padding:"0px 0px 10px 0px"}} className="dt-center"><img width="20" src="/imgs/network-default.png" /></th>
          <th data-column-id="nodelsensorstatus" style={{width:"24px",padding:"0px 0px 10px 0px"}} className="dt-center"><img width="20" src="/imgs/sensor-default.png" /></th>
          <th data-column-id="nodefixturename" className="dt-center">Fixture Name</th>
          <th data-column-id="nodegroupname" className="dt-center">Group Name</th>
          <th data-column-id="nodeapn" className="dt-center">APN</th>
          <th data-column-id="nodesoftwareversion" className="dt-center">Firmware Version </th>
          <th data-column-id="nodeschedulename" className="dt-center">Schedule Name </th>
        </tr>
        <tr id="filterRow" style={searchvisibility}>
          <th>ID</th>
          <th>Name</th>
          <th>Model</th>
          <th style={{visibility:"hidden"}}></th> 
          <th style={{visibility:"hidden"}}></th>
          <th style={{visibility:"hidden"}}></th>
          <th>Fixture</th>
          <th>Group</th>
          <th>APN</th>
          <th>Software Version</th>
          <th>Schedule</th>
        </tr>
        </thead>
        <tbody>
          {Nodetablerows}
        </tbody>
      </table>
    );
  }
  }


});
module.exports = Nodedatatable;
