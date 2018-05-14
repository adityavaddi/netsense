import classNames from 'classnames';
import auth from 'global/utils/auth';
import helpers from 'global/utils/helpers';

import { State, Navigation } from 'react-router';
import DataGrid from 'components/datagrid';
import Listeditor  from 'components/listeditor';

var Commissioninglist = React.createClass({

  getInitialState: function(){

    var component = "Commissioning";
    var userRole = NSN.userInfo.name;
    var componentColumns = "CommissioningColumnWidths_"+ userRole;
    var commissioningStoredWidths = [];

    if(localStorage.getItem(componentColumns)){
      commissioningStoredWidths = JSON.parse(localStorage.getItem(componentColumns));

      for(var i =0; i<commissioningStoredWidths.length; i++){

        if(commissioningStoredWidths[i].id === "_checkbox_selector"){
          commissioningStoredWidths.splice(i,1 );
        }
      }

      console.log("commissioningStoredWidths -- ", commissioningStoredWidths)
    }else{
      // default widths
      commissioningStoredWidths = [
        {name: "ID", field: "nodeid", id: "nodeid", sortable: true, width: 25, minWidth: 120, checked:true, required:true},
        {name: "Model", field: "model", id: "model",sortable: true, width: 25, checked:true, required:true},
        {name: "Status", field: "net_stat", id: "net_stat",sortable: true, width:30,maxWidth: 60, disableSearch: true, checked:true, required:true}
      ]
    }
    return {
      showListEditor:false,
      commissioningStoredWidths:commissioningStoredWidths,
      componentColumns:componentColumns,
      component:component,
      fileReady:false
    }
  },
  propTypes: {
    nodes: React.PropTypes.array.isRequired,
    selected_nodes: React.PropTypes.array.isRequired
  },

  componentDidMount: function() {
    var that = this;
    $('.rubix-panel').on('keydown', function (e) {
      if($('.modal-body').is(":visible") && e.which === 27){
        that.props.toggleModal();
      }
    });
},

  toggleListEditor : function(){
    this.setState({showListEditor:!this.state.showListEditor});
  },

  shouldComponentUpdate: function(nextProps,nextState){
    return true;
  },

  uploadFile: function (){

    var that = this
    var fileSelect = $('#file-select');
//    var postURL = NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/nodes/' + 'assign'
    var postURL = NSN.apiURL + 'nodes';
    var data = new FormData();
    var fileData = fileSelect[0].files[0]
    if(this.state.fileReady){
      console.log("Upload file type: " + fileData.type);
      if(fileData.type == "text/csv" || fileData.type == "application/vnd.ms-excel"){
        data.append("csvNodeList",fileData)
        $.ajax({
          "type": "POST",
          // "url": "https://qastage2.sensity.com/v3.0/customers/f996ce70-1f0f-11e7-a58c-692acefa12ae/sites/7dd33160-1f10-11e7-a58c-692acefa12ae/nodes/assign",
          "url": postURL,
          "data": data,
          "cache": false,
          "processData": false,
          "contentType": false
        }).done((data) => {
          that.setState({
            fileReady: false
          })
          alert("Success")
        }).fail((err) => {
          console.log("Error", err);
          alert("Failed. Try again")
        })
      }else{
        alert("File format not supported")
      }
    }else{
      alert("Please choose a file")
    }
  },

  render() {
    var lig_sum = [0, 0, 0, 0], net_sum = [0, 0, 0, 0], sen_sum = [0, 0, 0, 0];
    for (var i = 0; i < this.props.nodes.length; i++) {
      var node = this.props.nodes[i];
      lig_sum[node.lig_stat ? (node.lig_stat == "on" ? 0 : 2) : 3]++;
      net_sum[node.net_stat === null ? 3 : (node.net_stat ? 0 : 2)]++;
      sen_sum[node.sen_stat ? 0 : 3]++;
    };
    //Button - disabled state
    var btnComponent = <button id="commission-btn-test" type="button" disabled='disabled' className="btn btn-commission-disable">
      <Icon id="commission-btn" />Commission</button>;

    if (this.props.selected_nodes.length > 0) {
      //Btn  Enabled
      btnComponent = <button id="commission-btn-test" type="button" onClick={() => { this.props.toggleModal() }} className="btn btn-commission-enable">
        <Icon id="commission-btn" />Commission</button>;
    }

    return (
      <div style={{ height: "100%" }}>
        <div id="node-table-container">
          <h2 className="netsense__table__title">Commissioning</h2>

          <div style={{position:"absolute",top:"12px",right:"12px"}}>
            <span onClick={()=>this.toggleListEditor()}
                className="ns-filter-icon" style={{cursor:"pointer",fontSize:"30px"}}></span>
          </div>

          <h4 className="netsense__table__title" style={{ marginTop:"12px",fontSize:"28px"}}>Unassigned Nodes
          <span style={{fontSize:"18px"}}> (<span style={{color:"green"}}>{net_sum[0]} </span>
             Connected, <span style={{color:"red"}}>{net_sum[2]}</span> Disconnected) </span>
          </h4>

          <div className="row">
            <div className="col-sm-5 text-center">
              <span style={{position:"relative",top:"3px",color:"#333",fontSize:"17px"}}>Select nodes from list, then </span>
              <button id="commission-btn-test" type="button" className="btn btn-commission-enable"
                  onClick={() => { this.props.toggleModal() }}>
                <Icon id="commission-btn" /><b>Commission</b>
              </button> 
            </div> 
            <div className="col-sm-1 text-right">        
              <span style={{fontSize:"24px",color:"#333",fontWeight:"bold"}}><i>- or -</i></span>
            </div>
            <div className="col-sm-6 text-center">
              <label className="btn btn-commission-enable btn-file"
                title="Upload a CSV file with values on each line for nodeid, model, orgid, siteid, latitude and longitude (in that order).  Also include a header row with those column names." style={{paddingTop:"7px"}} >
                <b>Select CSV File to Upload </b><input type="file" id="file-select" onChange={(e)=>{e != "" ? this.setState({fileReady:true}): null}} style={{display:"none"}} />
              </label>
              {this.state.fileReady &&
                <span> &nbsp;
                  <button type="button" onClick={()=>this.uploadFile()}  
                      className="btn btn-commission" 
                      style={{color:"white",backgroundColor:this.state.fileReady?"green":"#C0C0C0",fontSize:"100%"}}>
                    <Icon id="commission-btn" />Upload
                  </button>
                </span>
              }
            </div>
          </div>

          {
            this.state.showListEditor ?
                <div className="ns-list-editor">
                  <Listeditor 
                    show={this.state.showListEditor}
                    component={this.state.component}
                    componentColumns ={this.state.componentColumns}
                    options={{multiSelect: true, selectable: true }}
                    handleToggle = {this.toggleListEditor}
            columns={ this.state.commissioningStoredWidths.map(function(column, index){
              if(column.name === "Status"){
                return(
                  {name:column.name,field:column.field,id: column.id, checked: column.checked, required: column.required,
                    width: column.width, maxWidth: 60, sortable:column.sortable, disableSearch: true,
                    formatter: function (row, cell, value) {
                      return '<img height="20" width="20" title="" alt="'
                          + ["Connected", "Disconnected"][value ? 0 : 1]
                          + '" src="/imgs/Network_'
                          + ["Working", "NA"][value ? 0 : 1]
                          + '.svg" />'
                    }
                  }
                )
              } else {
                if (column.name === "Model") {
                  return(
                  {name:column.name,field:column.field,id: column.id, checked: column.checked, required: column.required,
                    width: column.width, sortable:column.sortable,
                    formatter: function (row, cell, value) {
                      return (value + " <span style='color:#999'>(" + helpers.modelName(value) + ")</span>");
                    }
                  }
                  )
                } else {
                return(
                  { name:column.name, field:column.field, id: column.id, checked: column.checked, required: column.required,
                    width: column.width, sortable:column.sortable }
                )
                }
              }
            })} />
                </div>:
                null
          }

          <DataGrid 
            component={this.state.component}
            componentName="Node"
            dataArray={this.props.nodes}
            dataID={this.props.nodeID}
            dataIdField="nodeid"
            componentColumns ={this.state.componentColumns}
            match="contains"
            options={{ gridHeight: { windowPct: 100, offset: -250 }, multiSelect: true, selectable: true }}
            columns={ this.state.commissioningStoredWidths.map(function(column, index){
              if(column.name === "Status"){
                return(
                  {name:column.name,field:column.field,id: column.id, checked: column.checked, required: column.required,
                    width: column.width, maxWidth: 60, sortable:column.sortable, disableSearch: true,
                    formatter: function (row, cell, value) {
                      return '<img height="20" width="20" title="" alt="'
                          + ["good", "error"][value ? 0 : 1]
                          + '" src="/imgs/Network_'
                          + ["Working", "Error"][value ? 0 : 1]
                          + '.svg" />'
                    }
                  }
                )
              } else {
                if (column.name === "Model") {
                  return(
                  {name:column.name,field:column.field,id: column.id, checked: column.checked, required: column.required,
                    width: column.width, sortable:column.sortable,
                    formatter: function (row, cell, value) {
                      return (value + " <span style='color:#999'>(" + helpers.modelName(value) + ")</span>");
                    }
                  }
                  )
                } else {
                return(
                  { name:column.name, field:column.field, id: column.id, checked: column.checked, required: column.required,
                    width: column.width, sortable:column.sortable }
                )
                }
              }
            })} />
        </div>
      </div>
    );
  }
});

module.exports = Commissioninglist;
