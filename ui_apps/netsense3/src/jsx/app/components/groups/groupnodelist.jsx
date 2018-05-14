import classNames from 'classnames';
import { State, Navigation } from 'react-router';
import DataGrid from 'components/datagrid';
import Groupform from 'components/groups/groupform';
import helpers from 'global/utils/helpers';
import { Modal } from 'react-bootstrap';

var Groupnodelist = React.createClass({

    getInitialState: function () {
        return {
            selected_nodes : [],
            selectRowIndexes : []
        }
    },

    propTypes: {
        group: React.PropTypes.array.isRequired,
        nodes: React.PropTypes.array.isRequired,
        grouptype: React.PropTypes.string.isRequired,
        pdProfile: React.PropTypes.array.isRequired
    },

    handleSubmit: function (e) {
        e.stopPropagation();
        e.preventDefault();
        if (this.validateNodes()){
            ReactBootstrap.Dispatcher.emit("GroupNodeList.update",this.state.selected_nodes)
            this.props.hide();
        }
        else {
            noty({type:"error",text:"SCH node cannot be added to Lighting Group with existing Proximity Dimming Profile"});
        }
    },

    validateNodes: function (){
        if(this.props.pdProfile.length > 0){
            for (var key in this.state.selected_nodes ){
               var value = this.getNodeModelName(this.state.selected_nodes[key]) ;
               if( value == "Smart City Hub"){
                   return false;
               }
            }
        }
        return true ;
    },

    getNodeModelName: function(id){
        for(var key in this.props.nodes){
            if(this.props.nodes[key].nodeid == id){
                return this.props.nodes[key].model ;
            }
        }
    },

    init: function () {
        if (NSN.customerID == '-1') {
            $('#loadingmsg').html('Please select an Account first.')
            return;
        };
        if (NSN.siteID == '-1') {
            $('#loadingmsg').html('Please select a Site first.')
            return;
        };

    },

    componentDidMount: function () {
        var that = this

        // check the checkboxes for nodes in the group
        this.autoSelectRows();

        /* Emits when selecting single Node */
        ReactBootstrap.Dispatcher.on("GroupNodelist.select", function (nodeID,id) {
            that.setState({
                selected_nodes: [nodeID]
            })  
        })

        /* Emits when selecting Multiple Nodes */
        ReactBootstrap.Dispatcher.on("GroupNodelist.multiSelect", function (nodeIDs,ids) {
            that.setState({
                selected_nodes: nodeIDs
            })
        })

    },

    autoSelectRows: function(){

        var selectRowIndexes = [];
        var groupedNodes = this.props.group.nodeList;
        var allNodes = this.props.nodes;

        /* Data grid needs the index of the row to defaultly select the row on load */
        for(var i in allNodes){
            for(var j in groupedNodes){
                if(allNodes[i].nodeid == groupedNodes[j]){
                    selectRowIndexes.push(i)
                }
            }
        }

        this.setState({selectRowIndexes : selectRowIndexes},
            ()=>{ ReactBootstrap.Dispatcher.emit("GroupNodeform.selectedRows.update",selectRowIndexes) });
    },

    handleCancel:function(){
        this.props.hide()
    }, 

    render: function () {
        var grouptype=this.props.grouptype;
        var nodes = this.props.nodes.map(function(node, index) {
            node.grouptype = grouptype;
            return node;
        });
        var grouptypename = {"lighting":"Lighting",
                             "site-lighting":"Default Site Lighting",
                             "organizational":"Organizational"}[grouptype];
        return (
            <div className="groupNodeList" >
                <Modal.Dialog style={{ zIndex: 100, paddingTop: 100 }}>
                    <Modal.Body style={{ zIndex: 100, position: "relative" }}>
                        <div className="group-header">{grouptypename} Group Membership</div>
                        <div style={{padding:"6px 14px",fontSize:"16px"}}>
                        {(grouptype == "lighting") ?
                            (<span><b>Select:</b> (Only Lighting Nodes can be included in a Lighting Group.)</span>)
                            : (grouptype == "site-lighting" ?
                                (<span>The Default Site Lighting Group contains any Lighting Nodes
                                   not assigned to other Lighting Groups.</span>)
                                : (<span><b>Select:</b> </span>)) }
                        </div>
                        {/*//grid showing the node list*/}
                        <div id="group-node-small" style={{margin:"6px 12px"}}>
                            <DataGrid component="GroupNode"
                                componentName="Node"
                                dataArray={nodes}
                                selectedRows={this.state.selectRowIndexes}
                                //dataID={this.props.nodeID}
                                dataIdField="nodeid"
                                match="contains"
                                options={{ gridHeight: { windowPct: 100, offset: -288 }, 
                                        multiSelect: true, 
                                        selectable: true
                                         }}
                                columns={[
                                    {
                                        name: "Name", field: "name", id: "name",
                                        sortable: true, width: 160
                                    },
                                    {
                                        name: "ID", field: "nodeid", id: "nodeid",
                                        sortable: true, cssClass: "text-center", headerCssClass: "text-center", minWidth: 120, width: 120
                                    },
                                    {
                                        name: "Model", field: "model", id: "model",
                                        sortable: true, cssClass: "text-center", headerCssClass: "text-center", minWidth: 120, width: 120
                                    },
                                    {
                                        name: "Level", field: "level", id: "level",
                                        sortable: true, cssClass: "text-center", headerCssClass: "text-center", width: 60
                                    },
                                ]}
                            />
                        </div>

                        <div className="buttons-groups-overlay">
                            <button className="ns-cancel-btn" style={{ position: "absolute", width: "150px", bottom: "48px", left: "414px" }} onClick={() => { this.handleCancel() }}><b>Cancel</b></button>
                            <button className="ns-save-btn" style={{ position: "absolute", width: "150px", bottom: "48px", left: "700px" }} onClick={this.handleSubmit}><b>Apply</b></button>
                        </div>
                    </Modal.Body>
                </Modal.Dialog>
            </div>
        )
    }
});

module.exports = Groupnodelist;