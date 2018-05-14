import classNames from 'classnames';
import auth from 'global/utils/auth';
import helpers from 'global/utils/helpers';

import {
    State,
    Navigation
} from 'react-router';

var ReportingList = React.createClass({
    getInitialState: function(){
        return {
            selected_types: this.props.nodetypes,
            enableRealTimeUpdate: false
        };
    },
    propTypes: {
        sensors: React.PropTypes.array.isRequired,
        nodes: React.PropTypes.array.isRequired,
        nodetypes: React.PropTypes.array.isRequired,
        time_zone: React.PropTypes.string.isRequired,
        show: React.PropTypes.bool.isRequired
    },
    componentDidMount: function() {
        var self = this;
        self.initDatePicker();
        $('#sensor-y1').on('change',function(){
            $('#sensor-y2').attr('disabled',false);
        });
    },
    handleMonitor: function(e) {
        if(this.state.enableRealTimeUpdate){
            console.log("we are in handle monitor");
            e.stopPropagation();
            e.preventDefault();
            ReactBootstrap.Dispatcher.emit("ReportingList.monitor", "open");    
        }
    },
    handleSensor1Change: function(e) {
        e.preventDefault();

        ReactBootstrap.Dispatcher.emit('ReportingList.selectSensor1', e.target.value);

    },
    handleSensor2Change: function(e) {
        e.preventDefault();

        ReactBootstrap.Dispatcher.emit('ReportingList.selectSensor2', e.target.value);
    },
    handleNodeChange: function(e) {
        e.preventDefault();

        var options = e.target.options,
            values = [];

        for (var i = 0; i < options.length; i++) {
            if (options[i].selected) {
                values.push(options[i].value);
            }
        }
        this.setState({enableRealTimeUpdate: true})

        ReactBootstrap.Dispatcher.emit('ReportingList.selectNodes', values);
    },
    handleNodeType: function() {
        var checked = [];
        $(".nodetypecheckboxes").each(function(nodetype, index) {
            if (this.checked) {
                checked.push(this.id);
            };
        });
        this.setState({selected_types: checked});
        var values = [];
        ReactBootstrap.Dispatcher.emit('ReportingList.selectNodes', values);
        $("#nodeList").val('');
        $("#sensor-y1").val('');
        $("#sensor-y2").val('');
        
    },
    handleToggleLines: function(e) {
        e.preventDefault();

        ReactBootstrap.Dispatcher.emit('ReportingGraph.toggleLines');
    },
    handleDownloadImg: function(e) {
        ReactBootstrap.Dispatcher.emit('ReportingGraph.downloadImg', e.target);
    },
    handleDownloadCSV: function(e) {
        ReactBootstrap.Dispatcher.emit('ReportingGraph.downloadCSV', e.target);
    },
    render: function() {
        var self = this;
        var rawSensorList = self.props.sensors.filter(function(sensor, index) {
            return _.intersection(self.state.selected_types, sensor.type).length > 0;
            });
        rawSensorList = rawSensorList.sort(function(a,b) { 
            return a.name > b.name ? 1 : a.name < b.name ? -1 : 0;
            });
        var sensorList = [rawSensorList[0]];
        for (var i=1;i<rawSensorList.length; i++) {
            if (rawSensorList[i].name != rawSensorList[i-1].name) {
                sensorList.push(rawSensorList[i]);
            };
        };
        var diagnosticSensorList = sensorList.filter(function(sensor) {return sensor.category == "Diagnostic"});
        var networkSensorList = sensorList.filter(function(sensor) {return sensor.category == "Network"});
        var ambientSensorList = sensorList.filter(function(sensor) {return sensor.category == "Ambient"});
        var energySensorList = sensorList.filter(function(sensor) {return sensor.category == "Energy"});

        var sensorOptions = (
            <span>
            <option selected="true" value="0"> </option>
            <optgroup label="Diagnostic Sensors" style={{fontWeight:"bold"}}>
            {diagnosticSensorList.map(function(sensor, index) {
            return (
                <option key={index} value={sensor.id}>{sensor.name}</option>
                );
            })
            }
            </optgroup>
            <optgroup label="Network Sensors">
            {networkSensorList.map(function(sensor, index) {
            return (
                <option key={index} value={sensor.id}>{sensor.name}</option>
                );
            })
            }
            </optgroup>
            <optgroup label="Ambient Sensors">
            {ambientSensorList.map(function(sensor, index) {
            return (
                <option key={index} value={sensor.id}>{sensor.name}</option>
                );
            })
            }
            </optgroup>
            <optgroup label="Energy Sensors">
            {energySensorList.map(function(sensor, index) {
            return (
                <option key={index} value={sensor.id}>{sensor.name}</option>
                );
            })
            }
            </optgroup>
            </span>
            );

            var nNode = 
            {latitude: "",
            longitude:"",
            model:"",
            nodeid:"",
            name: "--None--"
            };
            var nList=self.props.nodes;
            nList.sort(function(a,b) {return (a.nodeid > b.nodeid) ? 1 : ((b.nodeid > a.nodeid) ? -1 : 0);} ); 
            nList.splice(0, 0, nNode);

            var nodeList = nList.filter(function(node, index) {
            if (index == 0){
                return true;
            }
            else {
                return self.state.selected_types.indexOf(helpers.modelType(node.model)) >=0;
            }
            })
                .map(function(node, index) {
                return (
                    <option key={index} value={node.nodeid}>{(node.name) ? node.name : node.nodeid}</option>
                );
            });

        var css = {
            'width': '100%',

        };
        var dwButtonCSS = {
            'width': '100%',
            'padding' : '9px',
            'marginLeft' : '30px'
        }
        var nodeCSS = {
            'width': '100%',
            'height': '80px'

        };

        var typeselection = (this.props.nodetypes.length < 2)?"":
             (
                <FormGroup style={{textAlign:"center",fontWeight:"16px"}}>
                {this.props.nodetypes.map(function(nodetype, index) {
                    return (
                        <span>
                        <input key={index} className="nodetypecheckboxes" id={nodetype} ref={nodetype} type="checkbox" onChange={self.handleNodeType} checked={self.state.selected_types.indexOf(nodetype)>=0} />
                        &nbsp; <b>{nodetype}</b> &nbsp; &nbsp;
                        </span> 
                    )
                })}
                </FormGroup>
                );

        return (
            <Form className="reportingForm">
                {typeselection}
                <FormGroup>
                    <Label>
                        Sensor Y1
                    </Label>
                    <Select id="sensor-y1" onChange={self.handleSensor1Change} style={css}>
                        {sensorOptions}
                    </Select>
                </FormGroup>
                <FormGroup>
                    <Label>
                        Sensor Y2
                    </Label>
                    <Select  id="sensor-y2" disabled="disabled" onChange={self.handleSensor2Change} style={css}>
                        {sensorOptions}
                    </Select>
                </FormGroup>
                <FormGroup>
                    <Label>
                        Nodes
                    </Label>
                    <Select id="nodeList" onChange={self.handleNodeChange} multiple style={nodeCSS}>
                        {nodeList}
                    </Select>
                </FormGroup>
                <FormGroup>
                    <Label>
                        Timezone
                    </Label>
                    <input type="text" style={css} value={self.props.time_zone} readOnly/>
                </FormGroup>
                <FormGroup>
                    <Label>
                        From
                    </Label>
                    <input type="text" className="datetimepicker-from" style={css} placeholder="From"/>
                </FormGroup>
                <FormGroup>
                    <Label>
                        To
                    </Label>
                    <input type="text" className="datetimepicker-to" style={css} placeholder="To"/>
                </FormGroup>
                <FormGroup>
                    {/*}<Button bsStyle='primary' onClick={self.handleToggleLines} style={css}>
                            Toggle lines
                    </Button> {*/}
                </FormGroup>
                 <FormGroup>
                    <button type="button" id="showUpdates"  className="ns-form-btn" style={css} onClick={self.handleMonitor} >
                        <b>Show Real-Time Updates</b></button>
                </FormGroup> 
                <FormGroup>
                        <b><a id="downloadImage" className="ns-form-btn" style={dwButtonCSS} onClick={self.handleDownloadImg}>Download Image</a></b>
                </FormGroup>
            </Form>
        );
    },
    initDatePicker: function() {
        var self = this,
            $from = $('.datetimepicker-from'),
            $to = $('.datetimepicker-to');

        $from
            .datetimepicker({
                defaultDate: self.props.from
            })
            .on('dp.hide', function(e) {
                ReactBootstrap.Dispatcher.emit('ReportingList.selectFrom', e.date);
            })
            .on('dp.change', function(e) {
                var toDate = $to.data('DateTimePicker').date,
                    fromDate = e.date;

                $to.data('DateTimePicker').setMinDate(fromDate);

                if (fromDate >= toDate) {
                    $from.data('DateTimePicker').setDate(toDate.subtract(1, 'minutes'));
                }
            })
            .data('DateTimePicker')
            .setMaxDate(moment().tz(self.props.time_zone));

        $to
            .datetimepicker({
                useCurrent: false,
                defaultDate: self.props.to
            })
            .on('dp.change', function(e) {
                var toDate = e.date,
                    fromDate = $from.data('DateTimePicker').date;

                $from.data('DateTimePicker').setMaxDate(e.date);

                if (toDate <= fromDate) {
                    $from.data('DateTimePicker').setDate(toDate.subtract(1, 'minutes'));
                }

            })
            .on('dp.hide', function(e) {
                ReactBootstrap.Dispatcher.emit('ReportingList.selectTo', e.date);
            })
            .data('DateTimePicker')
            .setMaxDate(moment().tz(self.props.time_zone));
    }
});

module.exports = ReportingList;
