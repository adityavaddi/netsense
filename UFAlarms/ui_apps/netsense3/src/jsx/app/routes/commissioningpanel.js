import classNames from 'classnames';

import helpers from 'global/utils/helpers';
import CommissioningList from 'components/commissioning/commissioninglist';
import CommissioningDetail from 'components/commissioning/commissioningdetail';
import Commissioningmultidetail from 'components/commissioning/commissioningmultidetail';
import DataUtil from '../service/datautil';


import Header from 'common/headernew';


var Body = React.createClass({
  getInitialState: function () {
    return {
      nodes: null,
      sites: null,
      siteID: "-1",
      nodeID: "-1",
      selected_nodes: [],
      open: false
    }
  },

  calcHeight: function (pct, extra) {
    var h = (window && window.document) ? (($(window).height() - 75) * (pct / 100) + extra) : 500;
    return h;
  },

  init: function () {
    var that = this;
    if (NSN.customerID == "-1") {
      $("#loadingmsg").html("Please select an Account first.")
      return;
    };

    DataUtil.getAll('commissioningNodes', this.processCommissioningNodes);
    // $.ajax({
    //   url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + '_nosite_' + '/nodes',
    //   data : '',
    //   method : 'GET',
    //   "xhrFields": {
    //      withCredentials: true
    //   },
    //   dataType : 'json',
    //   success : function(data){
    //     console.log("ajax success: " + JSON.stringify(data));
    //     $("#loadingmsg").html("Generating display.");
    //     if (data.length > 0 && typeof data[0].lig_stat == "undefined") {
    //       $.ajax({
    //         url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + '_nosite_' + '/node_status',
    //         data: '',
    //         method: 'GET',
    //         xhrFields: {
    //             withCredentials: true
    //         },
    //         dataType: 'json',
    //         success: function(statusdata) {
    //           // statusdata = helpers.getNodeStatus();
    //           for (var i = 0; i<data.length; i++) {
    //             var nodeid = data[i].nodeid;
    //             for (var j = 0; j<statusdata.length; j++) {
    //               if (nodeid == statusdata[j].nodeid) {
    //                 data[i].lig_stat = statusdata[j].lig_stat;
    //                 data[i].net_stat = statusdata[j].net_stat;
    //                 data[i].sen_stat = statusdata[j].sen_stat;
    //                 statusdata.splice(j, 1);
    //                 };
    //               };
    //             };
    //           that.setState({
    //             nodes: data.map(function(node, index) {
    //               if (typeof node.level == 'undefined') {
    //                 node.level = '1';
    //               };
    //      //          if (index%10 < 3) { node.latitude = ""; node.longitude = "";}
    //               node.idx = index;
    //               return node;
    //               })
    //           });
    //         },
    //         error: function(){
    //             console.log("node_status API failed");
    //         }
    //       });
    //     } else {
    //         that.setState({
    //           nodes: data.map(function(node, index) {
    //             if (typeof node.level == 'undefined') {
    //               node.level = '1';
    //             };
    //    //          if (index%10 < 3) { node.latitude = ""; node.longitude = "";}
    //             node.idx = index;
    //             return node;
    //             })
    //         });
    //   }
    //   },
    //   error : function(){
    //     console.log("ajax failure");
    //     $("#loadingmsg").html("Cannot retrieve node list.  API call failed.");
    //   }
    // });

    $.ajax({
      url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites',
      data: '',
      method: 'GET',
      xhrFields: {
        withCredentials: true
      },
      dataType: 'json',
      success: function (data) {
        console.log("ajax success: " + JSON.stringify(data));
        that.setState({
          sites: data.map(function (site, index) {
            site.idx = index;
            return site;
          })
        });
      },
      error: function (jqXHR, status, error) {
        console.log("ajax failure (sites): " + status + " - " + error);
        $("#loadingmsg").html("Cannot retrieve site list.  API call failed.");
      }
    });

  },

  ////////////////Callback/////////////////
  processCommissioningNodes: function (data) {
    var that = this;
    console.log("ajax success: " + JSON.stringify(data));
    $("#loadingmsg").html("Generating display.");
    if (data.length > 0 && typeof data[0].lig_stat == "undefined") {
      DataUtil.nodedata = data;
      DataUtil.getAll('commissioningNodestatus', this.processCommissioningNodeStatus);
      // $.ajax({
      //     url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + '_nosite_' + '/node_status',
      //     data: '',
      //     method: 'GET',
      //     xhrFields: {
      //         withCredentials: true
      //     },
      //     dataType: 'json',
      //     success: function(statusdata) {
      //         // statusdata = helpers.getNodeStatus();
      //         for (var i = 0; i<data.length; i++) {
      //             var nodeid = data[i].nodeid;
      //             for (var j = 0; j<statusdata.length; j++) {
      //                 if (nodeid == statusdata[j].nodeid) {
      //                     data[i].lig_stat = statusdata[j].lig_stat;
      //                     data[i].net_stat = statusdata[j].net_stat;
      //                     data[i].sen_stat = statusdata[j].sen_stat;
      //                     statusdata.splice(j, 1);
      //                 };
      //             };
      //         };
      //         that.setState({
      //             nodes: data.map(function(node, index) {
      //                 if (typeof node.level == 'undefined') {
      //                     node.level = '1';
      //                 };
      //                 //          if (index%10 < 3) { node.latitude = ""; node.longitude = "";}
      //                 node.idx = index;
      //                 return node;
      //             })
      //         });
      //     },
      //     error: function(){
      //         console.log("node_status API failed");
      //     }
      // });
    } else {
      that.setState({
        nodes: data.map(function (node, index) {
          if (typeof node.level == 'undefined') {
            node.level = '1';
          };
          //          if (index%10 < 3) { node.latitude = ""; node.longitude = "";}
          node.idx = index;
          return node;
        })
      });
    }
  },

  processCommissioningNodeStatus: function (statusdata) {
    // statusdata = helpers.getNodeStatus();
    var that = this;
    for (var i = 0; i < DataUtil.nodedata.length; i++) {
      var nodeid = DataUtil.nodedata[i].nodeid;
      for (var j = 0; j < statusdata.length; j++) {
        if (nodeid == statusdata[j].nodeid) {
          DataUtil.nodedata[i].lig_stat = statusdata[j].lig_stat;
          DataUtil.nodedata[i].net_stat = statusdata[j].net_stat;
          DataUtil.nodedata[i].sen_stat = statusdata[j].sen_stat;
          statusdata.splice(j, 1);
        };
      };
    };
    that.setState({
      nodes: DataUtil.nodedata.map(function (node, index) {
        if (typeof node.level == 'undefined') {
          node.level = '1';
        };
        //          if (index%10 < 3) { node.latitude = ""; node.longitude = "";}
        node.idx = index;
        return node;
      })
    });
  },

  ///////Callback for update Commissioning nodes///////////
  processUpdateCommissioning: function (data, inputdata) {
    console.log("Response from Assign Site to Node: " + JSON.stringify(""));
    //              NSN.nodeID = "-1";
    //              sessionStorage.setItem("nodeID", NSN.nodeID);
    var newState = React.addons.update(this.state, { nodes: { $splice: [[inputdata.index, 1]] }, nodeID: { $set: "-1" } });
    noty({ type: "success", text: 'Node has been assigned to Site' })
    ReactBootstrap.Dispatcher.emit("Commissioningform.delete.success", inputdata.node.nodeid);
    this.setState(newState);
  },

  componentDidMount: function () {
    this.init();
    var that = this;

    ReactBootstrap.Dispatcher.emit("Sitetiles.select", "-1", "");

    ReactBootstrap.Dispatcher.on("Commissioninglist.select", function (nodeID) {      
      //      NSN.nodeID = nodeID;
      //      sessionStorage.setItem("nodeID", NSN.nodeID);
      that.setState({
        "nodeID": nodeID,
        selected_nodes: [nodeID]
      })
    });

    ReactBootstrap.Dispatcher.on("Commissioninglist.multiSelect", function (nodes) {
      /* 
        Emitted when multiple nodes are selected on the commissioning page
        (Hold Swift to select multiple nodes)
       */
      console.log("This State Nodes", that.state.nodes)
      console.log("Site Location", NSN)
      that.setState({
        selected_nodes: nodes
      });
    });

    ReactBootstrap.Dispatcher.on('Commissioningform.multiassignnode', function (siteID) {
      // Emitted when user clicks on Apply Button upon selecting multiple nodes
      /* 
          @Aditya
          ajax calls here rather than in dataUtil was intentional as suggested by Dave 
          - #Vikram
      */

      // Construct JSON object from selected Nodes with orgId and siteId in order to convert to CSV
      var selectedNodesJSON = that.state.selected_nodes.map((node) => {
        for (var i=0, found=false; !found && i<that.state.nodes.length; i++) {
          found = (node == that.state.nodes[i].nodeid);
        }
        if (found) {
          node = that.state.nodes[--i];
        }
        return {
          nodeid: node.nodeid,
          orgid: NSN.customerID,
          siteid: siteID,
          model: node.model || "",
          latitude: node.latitude || "",
          longitude: node.longitude || ""
        }
      });

      var selectedNodesCSV = '';
      // Converting JSON to CSV 
      for (var i = 0; i < selectedNodesJSON.length; i++) {
        var line = '';
        for (var index in selectedNodesJSON[i]) {
          if (line != '') line += ','
          line += selectedNodesJSON[i][index];
        }
        selectedNodesCSV += line + '\n';
      }
      var selectedNodesCSVPayload = "nodeid,orgid,siteid,model,latitude,longitude" + "\n" + selectedNodesCSV;

      var csvPayload = JSON.stringify({ csvNodeList: selectedNodesCSVPayload })
      var selected_nodes = that.state.selected_nodes

      console.log("csv", csvPayload);
      $.ajax({
        url: NSN.apiURL + 'nodes',
        data: csvPayload,
        type: "PUT",
        "xhrFields": {
          withCredentials: true
        },
        "contentType": "application/json",
        "processData": false,
        "success": function (data) {
          //Loop through the selected nodes to emit events that will delete from table on Success
          //for (var key in selected_nodes) {
            ReactBootstrap.Dispatcher.emit("Commissioningform.delete.success", selected_nodes);
          //}
          that.setState({ selected_nodes: [] })
          noty({ type: "success", text: 'Nodes have been assigned to Site' })
        }.bind(that),
        "error": function (jqXHR, status, error) {
          console.log("ajax failure (sites): " + status + " - " + error);
          noty({ type: "error", text: "Could not assign Nodes to Site: " + status });
        }
      }); 
    })

    ReactBootstrap.Dispatcher.on("Commissioningform.assignnode", function (node, siteID) {
      var index = helpers.get_idx(that.state.nodes, node, 'nodeid');
      var inputdata = { node: node, siteID: siteID, index: index };
      DataUtil.updateEntity('commissioningUpdate', '', that.processUpdateCommissioning, inputdata);

      //       $.ajax({
      //           url : NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + siteID + '/nodes/' + node.nodeid + '/assign',
      //           data: "",
      //           type : "POST",
      //           "xhrFields": {
      //              withCredentials: true
      //           },
      //           "contentType": "application/json",
      //           "processData": false,
      //           "success" : function(data) {
      //             console.log("Response from Assign Site to Node: " + JSON.stringify(""));
      // //              NSN.nodeID = "-1";
      // //              sessionStorage.setItem("nodeID", NSN.nodeID);
      //               var newState = React.addons.update(this.state, { nodes: { $splice: [[index, 1]] }, nodeID: { $set : "-1"}});
      //               noty({type:"success", text:'Node has been assigned to Site'})
      //               ReactBootstrap.Dispatcher.emit("Commissioningform.delete.success", node.nodeid);
      //               this.setState(newState);
      //             }.bind(that),
      //             "error" : function(jqXHR, status, error) {
      //               if(jqXHR.status == 200) {
      //                 console.log("Response from Assign Site to Node: " + JSON.stringify(""));
      // //                NSN.nodeID = "-1";
      // //                sessionStorage.setItem("nodeID", NSN.nodeID);
      //                 var newState = React.addons.update(this.state, { nodes: { $splice: [[index, 1]] }, nodeID: { $set : "-1"}});
      //                 noty({type:"success", text:'Node has been assigned to Site'})
      //                 ReactBootstrap.Dispatcher.emit("Commissioningform.delete.success", node.nodeid);
      //                 this.setState(newState);
      //               } else {
      //                 console.log("ajax failure (sites): " + status + " - " + error);
      //                 noty({type:"error", text: "Could not assign Node to Site: " + status});
      //               }
      //             }.bind(that)
      //           });
    })
  },

  componentWillUnmount: function () {
    NSN.nodeID = "-1";
    sessionStorage.setItem("nodeID", NSN.nodeID);
    ReactBootstrap.Dispatcher.removeAllListeners("Commissioninglist.select");
    ReactBootstrap.Dispatcher.removeAllListeners("Commissioningform.assignnode");
    ReactBootstrap.Dispatcher.removeAllListeners("Commissioningform.multiassignnode");
  },

  toggleModal: function () {
    this.setState({
      open: !this.state.open
    })
  },

  render() {
    var hstyle = { height: helpers.calcHeight(90, 0) + "px !important" };
    if (this.state.nodes && this.state.sites) {
      var Subpanels = (
        <div className="netsense-center-panel">
          <div>
            <Col md={5} lg={12}>
              <PanelContainer>
                <Panel>
                  <PanelBody style={hstyle}>
                    <CommissioningList nodes={this.state.nodes} nodeID={this.state.nodeID} selected_nodes={this.state.selected_nodes} toggleModal={this.toggleModal} />
                    {
                      this.state.selected_nodes.length > 1 ?
                        <Commissioningmultidetail sites={this.state.sites} selected_nodes={this.state.selected_nodes} nodes={this.state.nodes} show={this.state.open} toggleModal={this.toggleModal} /> :
                        <CommissioningDetail nodeID={this.state.nodeID} sites={this.state.sites} selected_nodes={[]} nodes={this.state.nodes} show={this.state.open} toggleModal={this.toggleModal} />
                    }
                  </PanelBody>
                </Panel>
              </PanelContainer>
            </Col>
          </div>
        </div>
      );
      return (
        <Container id='body'>
          <Grid>
            <Row>
              <Col sm={12}>
                <Row>
                  {Subpanels}
                </Row>
              </Col>
            </Row>
          </Grid>
        </Container>
      );
    };
    return (
      <Container id='body'>
        <Grid>
          <Row>
            <Col sm={12}>
              <PanelContainer>
                <Panel>
                  <PanelBody style={hstyle}>
                    <h2 id="loadingmsg" style={{ paddingTop: "16%", textAlign: "center" }}>Loading...</h2>
                  </PanelBody>
                </Panel>
              </PanelContainer>
            </Col>
          </Row>
        </Grid>
      </Container>
    );
  }
});

export default class extends React.Component {
  render() {
    var classes = classNames({
      'container-open': this.props.open
    });

    return (
      <Container id='container' className={classes}>
        <Header />
        <Body />
      </Container>
    );
  }
}