import classNames from 'classnames';
import { State, Navigation } from 'react-router';
import DataGrid from 'components/datagrid';
import Daylightform from 'components/daylight/daylightform';
import helpers from 'global/utils/helpers';

var Daylighttriggerlist = React.createClass({

    getInitialState: function () {
        return {
            selected_nodes : [],
            selectRowIndexes : []
        }
    },

    propTypes: {
        groups: React.PropTypes.array.isRequired,
        triggers:React.PropTypes.array.isRequired,
        nodes: React.PropTypes.array.isRequired,
        etdhprofileid: React.PropTypes.string.isRequired,
    },

    handleSubmit: function (e) {
        e.stopPropagation();
        e.preventDefault();
        ReactBootstrap.Dispatcher.emit("DaylightNodelist.update",this.state.selected_nodes)
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

        ReactBootstrap.Dispatcher.on("DaylightNodelist.preselect",function(nodes,triggers){
            var props = { nodes : nodes, triggers: triggers };
            that.autoSelectRows(props)
        })

        ReactBootstrap.Dispatcher.on("DaylightNodelist.select", function (nodeID,id) {
            that.setState({
                selected_nodes: [nodeID]
            })
        })

        ReactBootstrap.Dispatcher.on("DaylightNodelist.multiSelect", function (nodeIDs,ids) {
            that.setState({
                selected_nodes: nodeIDs
            })
        })

    },

    autoSelectRows: function(props){
        var that = this
        var selectRowIndexes = []
        var triggernodes = props.triggers
        var allNodes = props.nodes

        for(var i in allNodes){
            for(var j in triggernodes){
                if(allNodes[i].nodeid == triggernodes[j].nodeid){
                    selectRowIndexes.push(i)
                }
            }
        }
        this.setState({
            selectRowIndexes
        },()=>{ ReactBootstrap.Dispatcher.emit("DaylightNodeform.selectedRows.update",selectRowIndexes) })
    },

    render: function () {
        console.log("props in render",this.props.nodes, this.props.etdhprofileid);
        return (
           <div className="daylightTriggerList" key={this.props.etdhprofileid}>
                <div id="daylight-trigger-small">
                    <DataGrid component="DaylightNode"
                        dataArray={this.props.nodes}
                        selectedRows = {this.state.selectRowIndexes}
                        dataIdField="nodeid"
                        match="contains"
                        options={{ gridHeight: { windowPct: 100, offset: -750 }, multiSelect: true, selectable: true }}
                        columns={[
                            {
                                name: "ID", field: "nodeid", id: "nodeid",
                                sortable: true, cssClass: "text-center", headerCssClass: "text-center", minWidth: 120, width: 120
                            },

                        ]}
                    />
                    <div>
                    <button className="btn-success " style={{ position: "relative", float: "right" }} onClick={this.handleSubmit}>Apply</button>
                    </div>
                </div>

            </div>

        )
    }
});

module.exports = Daylighttriggerlist;
