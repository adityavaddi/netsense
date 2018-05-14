import classNames from 'classnames';
import { State, Navigation } from 'react-router';
import DataGrid from 'components/datagrid';
import Listeditor  from 'components/listeditor';

var Alertlist = React.createClass({

    getInitialState: function(){
        var component = "Alert";
        var userRole = NSN.userInfo.name;
        var componentColumns = "AlertColumnWidths_"+ userRole;
        var alertStoredWidths = [];
        // if(localStorage.getItem(componentColumns)){
        //     alertStoredWidths = JSON.parse(localStorage.getItem(componentColumns));
        // }else{
            // default widths
            alertStoredWidths = [
                {name:"Name", field:"ufname", id:"ufname", sortable:true, width:150, checked:true, required:true},
                {name:"Type", field:"type", id:"type",sortable:true, width:150, checked:true, required:true},
                {name:"Severity", field:"severity", id:"severity", sortable:true, width:80, checked:true, required:true},
                {name:"Description", field:"description", id:"description",sortable:true, width:150, checked:true, required:false},
                {name:"Node", field:"nodeid", id:"nodeid",sortable:true, width:100, checked:true, required:false},
                {name:"Date & Time", field:"updated", id:"updated",sortable:true, width:200, checked:true, required:true}
            ]
        // }

        return {
            showListEditor:false,
            alertStoredWidths:alertStoredWidths,
            componentColumns:componentColumns,
            component:component
        }
    },
   propTypes: {
    alerts: React.PropTypes.array.isRequired,
    alertID: React.PropTypes.string.isRequired,
    customers: React.PropTypes.array.isRequired,
    sites: React.PropTypes.array.isRequired
  },

    handleOpen : function(){
        this.setState({showListEditor:!this.state.showListEditor});
    },

    shouldComponentUpdate: function (nextProps, nextState) {
        return true;
    },

  render() {

    var alertsData = this.props.alerts;
    var customersData = this.props.customers;
    var sitesData = this.props.sites;

    // Convert customerid to readable format:
    for(var i=0;i<alertsData.length;i++){
      for(var j=0;j<customersData.length;j++){
        if(alertsData[i].orgid == customersData[j].orgid){
          alertsData[i].orgid = customersData[j].name;
        }
      }
    }

    // Convert siteid to readable format:
    for(i=0;i<alertsData.length;i++){
      for(j=0;j<sitesData.length;j++){
        if(alertsData[i].siteid == sitesData[j].siteid){
          alertsData[i].siteid = sitesData[j].name;
        }
      }
    }

    // Convert date & time to readable format:
    for(i=0;i<alertsData.length;i++){
      if (typeof alertsData[i].alertid == "undefined") {
        alertsData[i].alertid = "Missing-" + Math.floor(100000*Math.random());
      }
      alertsData[i].updated = new Date(alertsData[i].updated).toString();
    }    

    return (
      <div id="site-table-container">
        <h2 className="netsense__table__title"  style={{marginTop:"-4px"}}>Alerts
            {/*<span onClick={()=>this.handleOpen()} className="rubix-icon icon-nargela-align-right"*/}
                  {/*style={{display:"inline-block", cursor:"pointer", float:"right"}}></span>*/}
        </h2>
          {/*{*/}
              {/*this.state.showListEditor ?*/}
                  {/*<div className="ns-list-editor">*/}
                      {/*<Listeditor show={this.state.showListEditor}*/}
                                  {/*component={this.state.component}*/}
                                  {/*componentColumns ={this.state.componentColumns}*/}
                                  {/*columns={this.state.alertStoredWidths}/>*/}
                  {/*</div>:*/}
                  {/*null*/}
          {/*}*/}
        <DataGrid component="Alert" 
                  dataArray={this.props.alerts}
                  dataID={this.props.alertID} 
                  dataIdField="alertid"
                  options={{gridHeight:{windowPct:50,offset:-112}}}
                  componentColumns ={this.state.componentColumns}
                  columns={this.state.alertStoredWidths} />
      </div>
      );
    }
 
  }
);

module.exports = Alertlist;