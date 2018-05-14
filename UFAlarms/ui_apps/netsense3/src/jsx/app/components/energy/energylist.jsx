import classNames from 'classnames';
import auth from 'global/utils/auth';
import helpers from 'global/utils/helpers';

import {
    State,
    Navigation
} from 'react-router';

var EnergyList = React.createClass({
    propTypes: {
        sensors: React.PropTypes.array.isRequired,
        nodes: React.PropTypes.array.isRequired,
        sites: React.PropTypes.array.isRequired,
        time_zone: React.PropTypes.string.isRequired,
        fixtures: React.PropTypes.array.isRequired,
        groups: React.PropTypes.array.isRequired
    },
    componentDidMount: function() {
        var self = this;

        self.initDatePicker();
    },
    handleTimeframeChange: function(e) {
        e.preventDefault();
        ReactBootstrap.Dispatcher.emit('EnergyList.selectTimeframe', e.target.value);
    },

    /*handleFilterChange: function(e){
        if(e.currentTarget.selectedIndex == 0){
            $("#siteselector").hide();
            $("#nodeselector").show();
        }
        else {
            $("#nodeselector").hide();
            $("#siteselector").show();
        }
    }, */

    handleNodeChange: function(e) {
        e.preventDefault();

        var options = e.target.options,
            values = [];

        for (var i = 0; i < options.length; i++) {
            if (options[i].selected) {
                values.push(options[i].value);
            }
        }

        ReactBootstrap.Dispatcher.emit('EnergyList.selectNodes', values);
    },
    handleFixtureChange: function(e) {
        e.preventDefault();

        var options = e.target.options,
            values = [];

        for (var i = 0; i < options.length; i++) {
            if (options[i].selected) {
                values.push(options[i].value);
            }
        }

        ReactBootstrap.Dispatcher.emit('EnergyList.selectFixtures', values);
    },
    handleGroupChange: function(e) {
        e.preventDefault();

        var options = e.target.options,
            values = [];

        for (var i = 0; i < options.length; i++) {
            if (options[i].selected) {
                values.push(options[i].value);
            }
        }

        ReactBootstrap.Dispatcher.emit('EnergyList.selectGroup', values);
    },

    handleConversionChange: function(e) {
        e.preventDefault();

        var options = e.target.options,
            values = [];

        for (var i = 0; i < options.length; i++) {
            if (options[i].selected) {
                values.push(options[i].value);
            }
        }

        ReactBootstrap.Dispatcher.emit('EnergyList.selectConversion', values);
    },
    handleToggleLines: function(e) {
        e.preventDefault();

        ReactBootstrap.Dispatcher.emit('EnergyGraph.toggleLines');
    },
    handleDownloadPdf: function(e) {
        ReactBootstrap.Dispatcher.emit('EnergyList.downloadPDF', e.target);
    },
    // handleDownloadCSV: function(e) {
    //     ReactBootstrap.Dispatcher.emit('EnergyList.downloadCSV', e.target);
    // },
    render: function() {       
        var self = this,
            sensorList = self.props.sensors.map(function(sensor, i) {
                return (
                    <option value={sensor.id}>{sensor.name}</option>
                );
            }),
            nodeList = self.props.nodes.map(function(n, i) {
                return (
                    <option value={n.nodeid}>{(n.name) ? n.name : n.nodeid}</option>
                );
            }),

            fixtureList = self.props.fixtures.map(function(f,i) {
            return (
                    <option value={f.fixtureid}>{(f.name) ? f.name : f.fixtureid}</option>
                );  
            }),

            groupList = self.props.groups.map(function(g,i) {
            return (
                    <option value={g.groupid}>{(g.name) ? g.name : g.groupid}</option>
                );  
            });

        sensorList.unshift(<option selected="true" value="0">None</option>);

        var css = {
            'text-align': 'center',
            'width': '100%'
        };
        var dwButtonCSS = {
            'width': '100%',
            'padding' : '9px',
            'marginLeft' : '30px'
        }

        return (
            <Form className="energyForm">
                <FormGroup>
                    <Label>
                        Conversion
                    </Label>
                    <Select onChange={self.handleConversionChange} style={css}>
                        <option value="kWh">kWh</option>
                        { /*<option value="Barrels" >Barrels</option>
                        <option value="Trees">Trees</option>
                        <option value="Cars" >Cars</option>
                        <option value="CO2">CO2</option> */}
                    </Select>
                </FormGroup>
                <FormGroup>
                    <Label>
                        Filter by Node
                    </Label>
                    <Select onChange={self.handleNodeChange} style={css}>
                        <option>All Nodes</option>
                        {nodeList}
                    </Select>
                </FormGroup>
                <FormGroup>
                    <Label>
                        Filter by Fixture
                    </Label>
                    <Select onChange={self.handleFixtureChange} style={css}>
                        <option value=" "> - - </option>
                        {fixtureList}
                    </Select>
                </FormGroup>
                <FormGroup>
                    <Label>
                        Filter by Lighting Group
                    </Label>
                    <Select onChange={self.handleGroupChange} style={css}>
                        <option value=" "> - - </option>
                        {groupList}
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
                    </Button>{*/}
                </FormGroup>
                <FormGroup>
                    {/* <button type="button" className="ns-form-btn" style={css} onClick={self.handleDownloadPdf}>
                        <b><a id="downloadImage">Download PDF</a></b></button> */}
                        <b><a id="downloadEnergyImage" className="ns-form-btn" style={dwButtonCSS} onClick={self.handleDownloadPdf}>Download PDF</a></b>
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
                pickTime: false,
                defaultDate: self.props.from
            })
            .on('dp.hide', function(e) {
                ReactBootstrap.Dispatcher.emit('EnergyList.selectFrom', e.date);
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
                pickTime: false,
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
                ReactBootstrap.Dispatcher.emit('EnergyList.selectTo', e.date);
            })
            .data('DateTimePicker')
            .setMaxDate(moment().tz(self.props.time_zone));
    }
});

module.exports = EnergyList;