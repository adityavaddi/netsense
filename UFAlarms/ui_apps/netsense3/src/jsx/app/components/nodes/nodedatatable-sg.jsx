import classNames from 'classnames';
import { State, Navigation } from 'react-router';
import auth from 'global/utils/auth';
import helpers from 'global/utils/helpers';

var Nodedatatable = React.createClass({

  propTypes: {
    nodes: React.PropTypes.array.isRequired,
    selected_nodes: React.PropTypes.array.isRequired,
    component: React.PropTypes.string
  },

  calcHeight: function(pct, extra) {
    var h = (window && window.document)?(($(window).height() - 75) * (pct/100) + extra):500;
    h = Math.min(h, 1200); // for mobile devices
    return h;
  },

  onSelectionChanged: function() {
    var selectedRows = this.gridOptions.api.getSelectedRows();
    var that = this;
    selectedRows.forEach( function(selectedRow, index) {
      ReactBootstrap.Dispatcher.emit(that.props.component+"list.select", selectedRow.nodeid);
    });

  },

  grid: null,

  statusAlts: {
    none: "3 - n/a",
    good: "2 - good",
    warn: "1 - warn",
    error: "0 - error"
  },

  columns: [
    {name:"ID", field:"nodeid", id:"nodeid", 
      sortable:true, cssClass:"text-center", headerCssClass:"text-center", minWidth:120},
    {name:"Model", field:"model", id:"model", 
      sortable:true, cssClass:"text-center", headerCssClass:"text-center"},
    {name:"Level", field:"level", id:"level", 
      sortable:true, cssClass:"text-center", headerCssClass:"text-center", maxWidth:100},
    {name:"L", field:"lig_stat", id:"lig_stat", 
      sortable:true, maxWidth:30, headerCssClass:"text-center",
          formatter: function(row, cell, value) {
            return '<img height="20" width="20" title="" alt="'
                  + (value=="on"?"2 - good":"0 - error")
                  + '" src="/imgs/light-'
                  + (value?((value=="on")?"good":"error"):"none")
                  + '.png" />'
            }},
    {name:"N", field:"net_stat", id:"net_stat", 
      sortable:true, maxWidth:30, headerCssClass:"text-center",
          formatter: function(row, cell, value) {
            return '<img height="20" width="20" title="" alt="'
                  + (value?"2 - good":"0 - error")
                  + '" src="/imgs/network-'
                  + (value?"good":"error")
                  + '.png" />'
            }},
    {name:"S", field:"sen_stat", id:"sen_stat", 
      sortable:true, maxWidth:30, headerCssClass:"text-center",
          formatter: function(row, cell, value) {
            return '<img height="20" width="20" title="" alt="'
                  + (value?"2 - good":"0 - error")
                  + '" src="/imgs/sensor-'
                  + (value?"good":"error")
                  + '.png" />'
            }}
//    ,
//    {name:"Latitude", field:"latitude", id:"latitude", 
//      sortable:true, cssClass:"text-center", headerCssClass:"text-center", minWidth:200},
//    {name:"Longitude", field:"longitude", id:"longitude", 
//      sortable:true, cssClass:"text-center", headerCssClass:"text-center", minWidth:200}
],

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
/*    var that = this;
    var table = $("#node-table").DataTable();
    table.on( 'select', function ( e, dt, type, indexes ) {
      e.stopPropagation();
      e.preventDefault();
      var rows = [];
      dt.rows('.selected').data().each(function(key){
        rows.push(key[0]);
      });
      console.log((new Date()) + ": " + indexes.length + " rows selected (" + rows + ")");
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
*/
  },

  modelName(model){
    var modelNames = {"unode-v2":"Core V2","unode-v3":"Core INT","unode-v4":"Core EX","unode-v5":"Core EX-C","unode-v6":"VZ-Pigeon"};
    return (typeof modelNames[model] == "undefined")?"???":modelNames[model];
  },

  unsetHandlers(){
//    var table = $("#node-table").DataTable();
//    table.off( 'select' );
 //   ReactBootstrap.Dispatcher.off('updateNode');
  },

  componentWillReceiveProps(){
//    $("#node-table").DataTable().destroy();
  },

  componentDidUpdate() {
    console.log("grid update");
    this.dataView.setItems(this.props.nodes, "nodeid");
    this.grid.setData(this.props.nodes, "nodeid");
    this.grid.invalidate();
    this.grid.render();

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
/*    var that = this;
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

*/
    var that = this;
    $("#node-table").height(helpers.calcHeight(100,-300));
    NSN.perfD = new Date();
    this.grid = new Slick.Grid("#node-table", this.props.nodes, this.columns,
                {forceFitColumns: true, fullWidthRows: true});
    this.grid.setSelectionModel(new Slick.RowSelectionModel());
    this.grid.onSelectedRowsChanged.subscribe(function(e, args) {
      var selectedData = [],
          modelrows = [],
          selectedIndexes;

      selectedIndexes = that.grid.getSelectedRows();
      jQuery.each(selectedIndexes, function (index, value) {
        selectedData.push(that.grid.getData()[value]);
      });
      if (selectedData.length == 1) {
        ReactBootstrap.Dispatcher.emit(that.props.component+"list.select", selectedData[0].nodeid);
      } else {
        ReactBootstrap.Dispatcher.emit(that.props.component+"list.multiSelect", 
          selectedData.map(function(node) {
            return node.nodeid;
          }),
          selectedData.map(function(node) {
            return node.model;
          }));
      }
    });
    NSN.perfE = new Date();
//    this.gridOptions.api.sizeColumnsToFit();


/*
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
*/
//      $("#node-table tr").not(".row_selected").hide();

  },

  componentWillUnmount() {
    this.unsetHandlers();
  },

  render(){
    if (typeof NSN != "undefined") NSN.perfC = new Date();
    return (
      <div id="node-table" style={{height:helpers.calcHeight(100,-400)}}></div>
      );
  }


});
module.exports = Nodedatatable;
