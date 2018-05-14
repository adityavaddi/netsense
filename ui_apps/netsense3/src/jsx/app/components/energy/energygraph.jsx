import classNames from 'classnames';
import auth from 'global/utils/auth';
import helpers from 'global/utils/helpers';

import {
    State,
    Navigation
} from 'react-router';

var EnergyGraph = React.createClass({
    propTypes: {
        sensors: React.PropTypes.array.isRequired,
        selected_timeframe: React.PropTypes.object.isRequired,
        selected_nodes: React.PropTypes.array.isRequired,
        selected_sites: React.PropTypes.array.isRequired,
        selected_conversions: React.PropTypes.array.isRequired,
        time_zone: React.PropTypes.string.isRequired
    },
    componentDidMount: function() {
        var self = this;
        self.drawChart(self.props);

        ReactBootstrap.Dispatcher.on('EnergyGraph.toggleLines', function() {
            self.toggleLines();
        });

        ReactBootstrap.Dispatcher.on('EnergyGraph.downloadImg', function(btn) {
            $(btn).attr({
                href: self.chart.toBase64Image(),
                download: 'graph.png'
            });
        });

        ReactBootstrap.Dispatcher.on('EnergyGraph.downloadCSV', function(btn) {
            var data = [],
                csvContent = "data:text/csv;charset=utf-8,",
                dataString = '';
            self.currentData.forEach(function(res) {
                res.data.forEach(function(d) {
                    
                    if(res.site !== undefined){
                        data.push([
                            moment(d.time)
                                .tz(self.props.time_zone)
                                .format('MM/DD/YY HH:mm:ss z (Z)'),
                            res.site.siteid,
                            res.sensor.name,
                            d.legacy_energy_consumption,
                            d.led_energy_consumption,
                            d.actual_energy_consumption
                        ]);
                      
                    }
                    else{
                        data.push([
                            moment(d.time)
                                .tz(self.props.time_zone)
                                .format('MM/DD/YY HH:mm:ss z (Z)'),
                            res.node.nodeid,
                            res.sensor.name,
                            d.legacy_energy_consumption,
                            d.led_energy_consumption,
                            d.actual_energy_consumption
                        ]);
                       
                    }
                    
                });
            });

             data.unshift([
                            'time',
                            'node/site',
                            'sensor',
                            'legacy_energy_consumption',
                            'led_energy_consumption',
                            'actual_energy_consumption'
                        ]);
           

            data.forEach(function(infoArray, index) {
                dataString = infoArray.join(",");
                csvContent += index < data.length ? dataString + "\n" : dataString;
            });

            $(btn).attr({
                href: encodeURI(csvContent),
                download: 'data.csv'
            });
        });
    },
    componentWillReceiveProps: function(nextProps) {
        var self = this;

        self.chart.destroy();

        self.drawChart(nextProps);
    },
    componentWillUnmount: function() {
        ReactBootstrap.Dispatcher.removeAllListeners('EnergyGraph.toggleLines');
        ReactBootstrap.Dispatcher.removeAllListeners('EnergyGraph.downloadImg');
        ReactBootstrap.Dispatcher.removeAllListeners('EnergyGraph.downloadCSV');
    },
    render: function() {
        var self = this;

        return (
            <Container>
                <Grid>
                    <Row>
                        <Col sm={12}>
                            <canvas id="chart"></canvas>
                        </Col>
                    </Row>
                </Grid>
            </Container>
        );
    },

    drawLines: function(sensors, nodes, sites, conversions, from, to, time_zone) {
        var self = this,
            queries = [];
        if(nodes != null){
            nodes.forEach(function(node) {
                sensors.forEach(function(sensor) {
                    queries.push({
                        node: node,
                        sensor: sensor
                    });
                });
            });

            async.map(queries, function(query, cb) {
                if(typeof query.node !== "undefined"){
                    self.node_query_api(query.node.nodeid, query.sensor.id, from, to, 100, function(err, data) {
                        query.data = data.datapoints;
                        cb(err, query);
                    });
                }
            }, function(err, results) {
                if (!err) {

                    self.currentData = results;

                    var datasets1 = results.map(function(query) {
                        var node = query.node;
                        var sensor = query.sensor;

                        if(conversions == "Barrels"){

                            // Total Savings:
                            var total_actual_usage = 0;
                            for(var i=0; i < query.data.length; i++) 
                            { 
                                total_actual_usage += query.data[i].actual_energy_consumption * 0.002;
                            } 

                            var total_sensor_savings = 0;
                            for(var j=0; j < query.data.length; j++) 
                            { 
                                total_sensor_savings += query.data[j].savings_legacy_vs_actual * 0.002;
                            } 

                            var total_led_savings = 0;
                            for(var k=0; k < query.data.length; k++) 
                            { 
                                total_led_savings += query.data[k].savings_legacy_vs_led * 0.002;
                            } 

                            var total_actual_savings;
                            total_actual_savings = total_led_savings + total_sensor_savings;
                            $("#savings").empty();
                            $("#savings").html("<span>" + "<b>Total Usage:</b> " + total_actual_usage.toFixed(2) + " Barrels" + "</span>&nbsp;&nbsp;" + "<span>" + "<b>LED Savings:</b> " + total_led_savings.toFixed(2) + " Barrels" + "</span>&nbsp;&nbsp;" + "<span>" + "<b>Sensor Savings:</b> " + total_sensor_savings.toFixed(2) + " Barrels" + "</span>&nbsp;&nbsp;" + "<span>" + "<b>Total Savings:</b> " + total_actual_savings.toFixed(2) + " Barrels" + "</span>&nbsp;&nbsp;");

                            var xyData1 = query.data.map(function(d) {
                                return {
                                    x: moment(d.startdt).tz(time_zone).format(),
                                    y: d.actual_energy_consumption * 0.002
                                }
                            });
                        }
                        else if(conversions == "Trees"){

                            // Total Savings:
                            var total_actual_usage = 0;
                            for(var i=0; i < query.data.length; i++) 
                            { 
                                total_actual_usage += query.data[i].actual_energy_consumption * 0.018;
                            } 

                            var total_sensor_savings = 0;
                            for(var j=0; j < query.data.length; j++) 
                            { 
                                total_sensor_savings += query.data[j].savings_legacy_vs_actual * 0.018;
                            } 

                            var total_led_savings = 0;
                            for(var k=0; k < query.data.length; k++) 
                            { 
                                total_led_savings += query.data[k].savings_legacy_vs_led * 0.018;
                            } 

                            var total_actual_savings;
                            total_actual_savings = total_led_savings + total_sensor_savings;
                            $("#savings").empty();
                            $("#savings").html("<span>" + "<b>Total Usage:</b> " + total_actual_usage.toFixed(2) + " Trees" + "</span>&nbsp;&nbsp;" + "<span>" + "<b>LED Savings:</b> " + total_led_savings.toFixed(2) + " Trees" + "</span>&nbsp;&nbsp;" + "<span>" + "<b>Sensor Savings:</b> " + total_sensor_savings.toFixed(2) + " Trees" + "</span>&nbsp;&nbsp;" + "<span>" + "<b>Total Savings:</b> " + total_actual_savings.toFixed(2) + " Trees" + "</span>&nbsp;&nbsp;");


                            var xyData1 = query.data.map(function(d) {
                                return {
                                    x: moment(d.startdt).tz(time_zone).format(),
                                    y: d.actual_energy_consumption * 0.018
                                }
                            });

                        }
                        else if(conversions == "Cars"){

                            // Total Savings:
                            var total_actual_usage = 0;
                            for(var i=0; i < query.data.length; i++) 
                            { 
                                total_actual_usage += query.data[i].actual_energy_consumption * 0.0001;
                            } 

                            var total_sensor_savings = 0;
                            for(var j=0; j < query.data.length; j++) 
                            { 
                                total_sensor_savings += query.data[j].savings_legacy_vs_actual * 0.0001;
                            } 

                            var total_led_savings = 0;
                            for(var k=0; k < query.data.length; k++) 
                            { 
                                total_led_savings += query.data[k].savings_legacy_vs_led * 0.0001;
                            } 

                            var total_actual_savings;
                            total_actual_savings = total_led_savings + total_sensor_savings;
                            $("#savings").empty();
                            $("#savings").html("<span>" + "<b>Total Usage:</b> " + total_actual_usage.toFixed(2) + " Cars" + "</span>&nbsp;&nbsp;" + "<span>" + "<b>LED Savings:</b> " + total_led_savings.toFixed(2) + " Cars" + "</span>&nbsp;&nbsp;" + "<span>" + "<b>Sensor Savings:</b> " + total_sensor_savings.toFixed(2) + " Cars" + "</span>&nbsp;&nbsp;" + "<span>" + "<b>Total Savings:</b> " + total_actual_savings.toFixed(2) + " Cars" + "</span>&nbsp;&nbsp;");

                            var xyData1 = query.data.map(function(d) {
                                return {
                                    x: moment(d.startdt).tz(time_zone).format(),
                                    y: d.actual_energy_consumption * 0.0001
                                }
                            });
                        }
                        else if(conversions== "CO2"){

                            // Total Savings:
                            var total_actual_usage = 0;
                            for(var i=0; i < query.data.length; i++) 
                            { 
                                total_actual_usage += query.data[i].actual_energy_consumption * 0.0007;
                            } 

                            var total_sensor_savings = 0;
                            for(var j=0; j < query.data.length; j++) 
                            { 
                                total_sensor_savings += query.data[j].savings_legacy_vs_actual * 0.0007;
                            } 

                            var total_led_savings = 0;
                            for(var k=0; k < query.data.length; k++) 
                            { 
                                total_led_savings += query.data[k].savings_legacy_vs_led * 0.0007;
                            } 

                            var total_actual_savings;
                            total_actual_savings = total_led_savings + total_sensor_savings;
                            $("#savings").empty();
                            $("#savings").html("<span>" + "<b>Total Usage:</b> " + total_actual_usage.toFixed(2) + " CO2" + "</span>&nbsp;&nbsp;" + "<span>" + "<b>LED Savings:</b> " + total_led_savings.toFixed(2) + " CO2" + "</span>&nbsp;&nbsp;" + "<span>" + "<b>Sensor Savings:</b> " + total_sensor_savings.toFixed(2) + " CO2" + "</span>&nbsp;&nbsp;" + "<span>" + "<b>Total Savings:</b> " + total_actual_savings.toFixed(2) + " CO2" + "</span>&nbsp;&nbsp;");

                            var xyData1 = query.data.map(function(d) {
                                return {
                                    x: moment(d.startdt).tz(time_zone).format(),
                                    y: d.actual_energy_consumption * 0.0007
                                }
                            });
                        }
                        else{

                            // Total Savings:
                            var total_actual_usage = 0;
                            for(var i=0; i < query.data.length; i++) 
                            { 
                                total_actual_usage += query.data[i].actual_energy_consumption;
                            } 

                            var total_sensor_savings = 0;
                            for(var j=0; j < query.data.length; j++) 
                            { 
                                total_sensor_savings += query.data[j].savings_legacy_vs_actual;
                            } 

                            var total_led_savings = 0;
                            for(var k=0; k < query.data.length; k++) 
                            { 
                                total_led_savings += query.data[k].savings_legacy_vs_led;
                            } 

                            var total_actual_savings;
                            total_actual_savings = total_led_savings + total_sensor_savings;
                            $("#savings").empty();
                            $("#savings").html("<span>" + "<b>Total Usage:</b> " + total_actual_usage.toFixed(2) + " kWh" + "</span>&nbsp;&nbsp;" + "<span>" + "<b>LED Savings:</b> " + total_led_savings.toFixed(2) + " kWh" + "</span>&nbsp;&nbsp;" + "<span>" + "<b>Sensor Savings:</b> " + total_sensor_savings.toFixed(2) + " kWh" + "</span>&nbsp;&nbsp;" + "<span>" + "<b>Total Savings:</b> " + total_actual_savings.toFixed(2) + " kWh" + "</span>&nbsp;&nbsp;");

                            var xyData1 = query.data.map(function(d) {
                                return {
                                    x: moment(d.startdt).tz(time_zone).format(),
                                    y: d.actual_energy_consumption
                                }
                            });
                        }
                        
                        var colors = self.RGBAcolors();

                        return {
                            steppedLine:true,
                            label: "Actual Energy Consumption",
                            data: xyData1,
                            borderColor: colors[0],
                            backgroundColor: colors[0],
                            borderWidth: 2
                        };
                    });

                    var datasets2 = results.map(function(query) {
                        var node = query.node;
                        var sensor = query.sensor;

                        if(conversions == "Barrels"){
                            var xyData2 = query.data.map(function(d) {
                                return {
                                    x: moment(d.startdt).tz(time_zone).format(),
                                    y: d.led_energy_consumption * 0.002
                                }
                            });
                            var xyData2Tooltip = query.data.map(function(d) {
                                return d.savings_legacy_vs_actual * 0.002
                            });
                        }
                        else if(conversions == "Trees"){
                            var xyData2 = query.data.map(function(d) {
                                return {
                                    x: moment(d.startdt).tz(time_zone).format(),
                                    y: d.led_energy_consumption * 0.018
                                }
                            });

                            var xyData2Tooltip = query.data.map(function(d) {
                                return d.savings_legacy_vs_actual * 0.018
                            });

                        }
                        else if(conversions == "Cars"){
                            var xyData2 = query.data.map(function(d) {
                                return {
                                    x: moment(d.startdt).tz(time_zone).format(),
                                    y: d.led_energy_consumption * 0.0001
                                }
                            });

                            var xyData2Tooltip = query.data.map(function(d) {
                                return d.savings_legacy_vs_actual * 0.0001
                            });
                        }
                        else if(conversions== "CO2"){
                            var xyData2 = query.data.map(function(d) {
                                return {
                                    x: moment(d.startdt).tz(time_zone).format(),
                                    y: d.led_energy_consumption * 0.0007
                                }
                            });
                            var xyData2Tooltip = query.data.map(function(d) {
                                return d.savings_legacy_vs_actual * 0.0007
                            });
                        }
                        else{
                            var xyData2 = query.data.map(function(d) {
                                return {
                                    x: moment(d.startdt).tz(time_zone).format(),
                                    y: d.led_energy_consumption
                                }
                            });

                            var xyData2Tooltip = query.data.map(function(d) {
                                return d.savings_legacy_vs_actual
                            });
                        }
                           
                        var colors = self.RGBAcolors();

                        return {
                            steppedLine:true,
                            label: "LED Energy Consumption",
                            data: xyData2,
                            borderColor: colors[1],
                            backgroundColor: colors[1],
                            labels: xyData2Tooltip,
                            borderWidth: 2
                        };
                    });

                    var datasets3 = results.map(function(query) {
                        var node = query.node;
                        var sensor = query.sensor;

                        if(conversions == "Barrels"){
                            var xyData3 = query.data.map(function(d) {
                                return {
                                    x: moment(d.startdt).tz(time_zone).format(),
                                    y: d.legacy_energy_consumption * 0.002
                                }
                            });

                            var xyData3Tooltip = query.data.map(function(d) {
                                return d.savings_legacy_vs_led * 0.002
                            });
                        }
                        else if(conversions == "Trees"){
                            var xyData3 = query.data.map(function(d) {
                                return {
                                    x: moment(d.startdt).tz(time_zone).format(),
                                    y: d.legacy_energy_consumption * 0.018
                                }
                            });

                            var xyData3Tooltip = query.data.map(function(d) {
                                return d.savings_legacy_vs_led * 0.018
                            });

                        }
                        else if(conversions == "Cars"){
                            var xyData3 = query.data.map(function(d) {
                                return {
                                    x: moment(d.startdt).tz(time_zone).format(),
                                    y: d.legacy_energy_consumption * 0.0001
                                }
                            });

                            var xyData3Tooltip = query.data.map(function(d) {
                                return d.savings_legacy_vs_led * 0.0001
                            });
                        }
                        else if(conversions== "CO2"){
                            var xyData3 = query.data.map(function(d) {
                                return {
                                    x: moment(d.startdt).tz(time_zone).format(),
                                    y: d.legacy_energy_consumption * 0.0007
                                }
                            });

                            var xyData3Tooltip = query.data.map(function(d) {
                                return d.savings_legacy_vs_led * 0.0007
                            });
                        }
                        else{
                            var xyData3 = query.data.map(function(d) {
                                return {
                                    x: moment(d.startdt).tz(time_zone).format(),
                                    y: d.legacy_energy_consumption
                                }
                            });

                            var xyData3Tooltip = query.data.map(function(d) {
                                return d.savings_legacy_vs_led
                            });
                        }
                        
                        var colors = self.RGBAcolors();

                        return {
                            steppedLine:true,
                            label: "Legacy Energy Consumption",
                            data: xyData3,
                            borderColor: colors[2],
                            backgroundColor: colors[2],
                            labels: xyData3Tooltip,
                            borderWidth: 2
                        };
                    });

                   
                    self.chart.data.datasets.push(datasets1[0],datasets2[0],datasets3[0]);
                    self.chart.update();
                }
            }); 
        }
        
        if(sites !=null){
            sites.forEach(function(site) {
                sensors.forEach(function(sensor) {
                    queries.push({
                        site: site,
                        sensor: sensor
                    });
                });
            });

            async.map(queries, function(query, cb) {
                if((typeof query.site !== "undefined") && (query.site != null)){
                    self.site_query_api(query.site.siteid, query.sensor.id, from, to, 100, function(err, data) {
                        query.data = data.datapoints;
                        cb(err, query);
                    });
                }
            }, function(err, results) {
                if (!err) {

                    self.currentData = results;

                    var datasets1 = results.map(function(query) {
                        var site = query.site;
                        var sensor = query.sensor;

                        if(conversions == "Barrels"){

                            // Total Savings:
                            var total_actual_usage = 0;
                            for(var i=0; i < query.data.length; i++) 
                            { 
                                total_actual_usage += query.data[i].actual_energy_consumption * 0.002;
                            } 

                            var total_sensor_savings = 0;
                            for(var j=0; j < query.data.length; j++) 
                            { 
                                total_sensor_savings += query.data[j].savings_legacy_vs_actual * 0.002;
                            } 

                            var total_led_savings = 0;
                            for(var k=0; k < query.data.length; k++) 
                            { 
                                total_led_savings += query.data[k].savings_legacy_vs_led * 0.002;
                            } 

                            var total_actual_savings;
                            total_actual_savings = total_led_savings + total_sensor_savings;
                            $("#savings").empty();
                            $("#savings").html("<span>" + "<b>Total Usage:</b> " + (total_actual_usage/1000).toFixed(4) + " Barrels" + "</span>&nbsp;&nbsp;" + "<span>" + "<b>LED Savings:</b> " + (total_led_savings/1000).toFixed(4) + " Barrels" + "</span>&nbsp;&nbsp;" + "<span>" + "<b>Sensor Savings:</b> " + (total_sensor_savings/1000).toFixed(4) + " Barrels" + "</span>&nbsp;&nbsp;" + "<span>" + "<b>Total Savings:</b> " + (total_actual_savings/1000).toFixed(4) + " Barrels" + "</span>&nbsp;&nbsp;");

                            var xyData1 = query.data.map(function(d) {
                                return {
                                    x: moment(d.startdt).tz(time_zone).format(),
                                    y: d.actual_energy_consumption * 0.002
                                }
                            });
                        }
                        else if(conversions == "Trees"){

                            // Total Savings:
                            var total_actual_usage = 0;
                            for(var i=0; i < query.data.length; i++) 
                            { 
                                total_actual_usage += query.data[i].actual_energy_consumption * 0.018;
                            } 

                            var total_sensor_savings = 0;
                            for(var j=0; j < query.data.length; j++) 
                            { 
                                total_sensor_savings += query.data[j].savings_legacy_vs_actual * 0.018;
                            } 

                            var total_led_savings = 0;
                            for(var k=0; k < query.data.length; k++) 
                            { 
                                total_led_savings += query.data[k].savings_legacy_vs_led * 0.018;
                            } 

                            var total_actual_savings;
                            total_actual_savings = total_led_savings + total_sensor_savings;
                            $("#savings").empty();
                            $("#savings").html("<span>" + "<b>Total Usage:</b> " + (total_actual_usage/1000).toFixed(4) + " Trees" + "</span>&nbsp;&nbsp;" + "<span>" + "<b>LED Savings:</b> " + (total_led_savings/1000).toFixed(4) + " Trees" + "</span>&nbsp;&nbsp;" + "<span>" + "<b>Sensor Savings:</b> " + (total_sensor_savings/1000).toFixed(4) + " Trees" + "</span>&nbsp;&nbsp;" + "<span>" + "<b>Total Savings:</b> " + (total_actual_savings/1000).toFixed(4) + " Trees" + "</span>&nbsp;&nbsp;");


                            var xyData1 = query.data.map(function(d) {
                                return {
                                    x: moment(d.startdt).tz(time_zone).format(),
                                    y: d.actual_energy_consumption * 0.018
                                }
                            });

                        }
                        else if(conversions == "Cars"){

                            // Total Savings:
                            var total_actual_usage = 0;
                            for(var i=0; i < query.data.length; i++) 
                            { 
                                total_actual_usage += query.data[i].actual_energy_consumption * 0.0001;
                            } 

                            var total_sensor_savings = 0;
                            for(var j=0; j < query.data.length; j++) 
                            { 
                                total_sensor_savings += query.data[j].savings_legacy_vs_actual * 0.0001;
                            } 

                            var total_led_savings = 0;
                            for(var k=0; k < query.data.length; k++) 
                            { 
                                total_led_savings += query.data[k].savings_legacy_vs_led * 0.0001;
                            } 

                            var total_actual_savings;
                            total_actual_savings = total_led_savings + total_sensor_savings;
                            $("#savings").empty();
                            $("#savings").html("<span>" + "<b>Total Usage:</b> " + (total_actual_usage/1000).toFixed(4) + " Cars" + "</span>&nbsp;&nbsp;" + "<span>" + "<b>LED Savings:</b> " + (total_led_savings/1000).toFixed(4) + " Cars" + "</span>&nbsp;&nbsp;" + "<span>" + "<b>Sensor Savings:</b> " + (total_sensor_savings/1000).toFixed(4) + " Cars" + "</span>&nbsp;&nbsp;" + "<span>" + "<b>Total Savings:</b> " + (total_actual_savings/1000).toFixed(4) + " Cars" + "</span>&nbsp;&nbsp;");

                            var xyData1 = query.data.map(function(d) {
                                return {
                                    x: moment(d.startdt).tz(time_zone).format(),
                                    y: d.actual_energy_consumption * 0.0001
                                }
                            });
                        }
                        else if(conversions== "CO2"){

                            // Total Savings:
                            var total_actual_usage = 0;
                            for(var i=0; i < query.data.length; i++) 
                            { 
                                total_actual_usage += query.data[i].actual_energy_consumption * 0.0007;
                            } 

                            var total_sensor_savings = 0;
                            for(var j=0; j < query.data.length; j++) 
                            { 
                                total_sensor_savings += query.data[j].savings_legacy_vs_actual * 0.0007;
                            } 

                            var total_led_savings = 0;
                            for(var k=0; k < query.data.length; k++) 
                            { 
                                total_led_savings += query.data[k].savings_legacy_vs_led * 0.0007;
                            } 

                            var total_actual_savings;
                            total_actual_savings = total_led_savings + total_sensor_savings;
                            $("#savings").empty();
                            $("#savings").html("<span>" + "<b>Total Usage:</b> " + (total_actual_usage/1000).toFixed(4) + " CO2" + "</span>&nbsp;&nbsp;" + "<span>" + "<b>LED Savings:</b> " + (total_led_savings/1000).toFixed(4) + " CO2" + "</span>&nbsp;&nbsp;" + "<span>" + "<b>Sensor Savings:</b> " + (total_sensor_savings/1000).toFixed(4) + " CO2" + "</span>&nbsp;&nbsp;" + "<span>" + "<b>Total Savings:</b> " + (total_actual_savings/1000).toFixed(4) + " CO2" + "</span>&nbsp;&nbsp;");

                            var xyData1 = query.data.map(function(d) {
                                return {
                                    x: moment(d.startdt).tz(time_zone).format(),
                                    y: d.actual_energy_consumption * 0.0007
                                }
                            });
                        }
                        else{

                            // Total Savings:
                            var total_actual_usage = 0;
                            for(var i=0; i < query.data.length; i++) 
                            { 
                                total_actual_usage += query.data[i].actual_energy_consumption;
                            } 

                            var total_sensor_savings = 0;
                            for(var j=0; j < query.data.length; j++) 
                            { 
                                total_sensor_savings += query.data[j].savings_legacy_vs_actual;
                            } 

                            var total_led_savings = 0;
                            for(var k=0; k < query.data.length; k++) 
                            { 
                                total_led_savings += query.data[k].savings_legacy_vs_led;
                            } 

                            var total_actual_savings;
                            total_actual_savings = total_led_savings + total_sensor_savings;
                            $("#savings").empty();
                            $("#savings").html("<span>" + "<b>Total Usage:</b> " + (total_actual_usage/1000).toFixed(4) + " kWh" + "</span>&nbsp;&nbsp;" + "<span>" + "<b>LED Savings:</b> " + (total_led_savings/1000).toFixed(4) + " kWh" + "</span>&nbsp;&nbsp;" + "<span>" + "<b>Sensor Savings:</b> " + (total_sensor_savings/1000).toFixed(4) + " kWh" + "</span>&nbsp;&nbsp;" + "<span>" + "<b>Total Savings:</b> " + (total_actual_savings/1000).toFixed(4) + " kWh" + "</span>&nbsp;&nbsp;");

                            var xyData1 = query.data.map(function(d) {
                                return {
                                    x: moment(d.startdt).tz(time_zone).format(),
                                    y: d.actual_energy_consumption
                                }
                            });
                        }

                            
                        var colors = self.RGBAcolors();

                        return {
                            steppedLine:true,
                            label: "Actual Energy Consumption",
                            data: xyData1,
                            borderColor: colors[0],
                            backgroundColor: colors[0],
                            borderWidth: 2
                        };
                    });

                    var datasets2 = results.map(function(query) {
                        var site = query.site;
                        var sensor = query.sensor;
                            
                        if(conversions == "Barrels"){
                            var xyData2 = query.data.map(function(d) {
                                return {
                                    x: moment(d.startdt).tz(time_zone).format(),
                                    y: d.led_energy_consumption * 0.002
                                }
                            });
                            var xyData2Tooltip = query.data.map(function(d) {
                                return d.savings_legacy_vs_actual * 0.002
                            });
                        }
                        else if(conversions == "Trees"){
                            var xyData2 = query.data.map(function(d) {
                                return {
                                    x: moment(d.startdt).tz(time_zone).format(),
                                    y: d.led_energy_consumption * 0.018
                                }
                            });

                            var xyData2Tooltip = query.data.map(function(d) {
                                return d.savings_legacy_vs_actual * 0.018
                            });

                        }
                        else if(conversions == "Cars"){
                            var xyData2 = query.data.map(function(d) {
                                return {
                                    x: moment(d.startdt).tz(time_zone).format(),
                                    y: d.led_energy_consumption * 0.0001
                                }
                            });

                            var xyData2Tooltip = query.data.map(function(d) {
                                return d.savings_legacy_vs_actual * 0.0001
                            });
                        }
                        else if(conversions== "CO2"){
                            var xyData2 = query.data.map(function(d) {
                                return {
                                    x: moment(d.startdt).tz(time_zone).format(),
                                    y: d.led_energy_consumption * 0.0007
                                }
                            });
                            var xyData2Tooltip = query.data.map(function(d) {
                                return d.savings_legacy_vs_actual * 0.0007
                            });
                        }
                        else{
                            var xyData2 = query.data.map(function(d) {
                                return {
                                    x: moment(d.startdt).tz(time_zone).format(),
                                    y: d.led_energy_consumption
                                }
                            });
                            var xyData2Tooltip = query.data.map(function(d) {
                                return d.savings_legacy_vs_actual
                            });
                        }

                            
                        var colors = self.RGBAcolors();

                        return {
                            steppedLine:true,
                            label: "LED Energy Consumption",
                            data: xyData2,
                            borderColor: colors[1],
                            backgroundColor: colors[1],
                            labels: xyData2Tooltip,
                            borderWidth: 2
                        };
                    });

                    var datasets3 = results.map(function(query) {
                        var site = query.site;
                        var sensor = query.sensor;

                        if(conversions == "Barrels"){
                            var xyData3 = query.data.map(function(d) {
                                return {
                                    x: moment(d.startdt).tz(time_zone).format(),
                                    y: d.legacy_energy_consumption * 0.002
                                }
                            });

                            var xyData3Tooltip = query.data.map(function(d) {
                                return d.savings_legacy_vs_led * 0.002
                            });
                        }
                        else if(conversions == "Trees"){
                            var xyData3 = query.data.map(function(d) {
                                return {
                                    x: moment(d.startdt).tz(time_zone).format(),
                                    y: d.legacy_energy_consumption * 0.018
                                }
                            });

                            var xyData3Tooltip = query.data.map(function(d) {
                                return d.savings_legacy_vs_led * 0.018
                            });

                        }
                        else if(conversions == "Cars"){
                            var xyData3 = query.data.map(function(d) {
                                return {
                                    x: moment(d.startdt).tz(time_zone).format(),
                                    y: d.legacy_energy_consumption * 0.0001
                                }
                            });

                            var xyData3Tooltip = query.data.map(function(d) {
                                return d.savings_legacy_vs_led * 0.0001
                            });
                        }
                        else if(conversions== "CO2"){
                            var xyData3 = query.data.map(function(d) {
                                return {
                                    x: moment(d.startdt).tz(time_zone).format(),
                                    y: d.legacy_energy_consumption * 0.0007
                                }
                            });

                            var xyData3Tooltip = query.data.map(function(d) {
                                return d.savings_legacy_vs_led * 0.0007
                            });
                        }
                        else{
                            var xyData3 = query.data.map(function(d) {
                                return {
                                    x: moment(d.startdt).tz(time_zone).format(),
                                    y: d.legacy_energy_consumption
                                }
                            });

                            var xyData3Tooltip = query.data.map(function(d) {
                                return d.savings_legacy_vs_led
                            });
                        }
                        
                        var colors = self.RGBAcolors();

                        return {
                            steppedLine:true,
                            label: "Legacy Energy Consumption",
                            data: xyData3,
                            borderColor: colors[2],
                            backgroundColor: colors[2],
                            labels: xyData3Tooltip,
                            borderWidth: 2
                        };
                    });

                   
                    self.chart.data.datasets.push(datasets1[0],datasets2[0],datasets3[0]);
                    self.chart.update();
                }
            });  
        }
        
    },

    drawChart: function(props) {
        var self = this,
            sensors = props.sensors;

        if(props.selected_conversions == "Barrels"){
            var units = " (Barrels)";
        }
        else if(props.selected_conversions == "Trees"){
            var units = " (Trees)";
        }
        else if(props.selected_conversions == "Cars"){
            var units = " (Cars)";
        }

        else if(props.selected_conversions == "CO2"){
            var units = " (CO2)";
        }
        else{
            var units = " (Wh)";
        }

        var sensor1 = sensors[0],
            yAxes = [{
                type: 'linear',
                position: 'left',
                id: sensor1.id,
                scaleLabel: {
                    display: true,
                    labelString: sensor1.name + units
                }
            }];

        var titleData;
        if(props.selected_sites !=null){
            titleData = "Chart displayed for selected site - " + props.selected_sites.name;
        }
        else {
            for(var i=0;i<props.selected_nodes.length;i++){
                titleData = "Chart displayed for selected node - " + props.selected_nodes[i].nodeid;
            }
        } 

        self.chart = new Chart(document.getElementById('chart'), {
            type: 'line',
            data: {
                datasets: []
            },
            options: {
                showLines: true,
                title: {
                    display: true,
                    fontSize: 14,
                    text: titleData
                },

                scales: {
                    xAxes: [{
                        type: 'time',
                        time: {
                            parser: function (value) {
                                return moment(value).tz(props.time_zone);
                            }                
                        },
                        position: 'bottom',
                        scaleLabel: {
                            display: true,
                            labelString: 'Time'
                        },
                           
                    }],
                    yAxes: yAxes
                },
                tooltips: {
                    bodyFontStyle: "bold",
                    titleSpacing:6,
                    callbacks: {
                        title: function(tooltips,data) {
                            
                            if(tooltips[0].datasetIndex == 2){
                                var label = "LED Savings";
                                var value = data.datasets[tooltips[0].datasetIndex].labels[tooltips[0].index];
                                var t = tooltips[0],
                                date = t.xLabel;
                                var formattedDate =  moment(date).tz(props.time_zone).format('MM/DD/YY HH:mm:ss z (Z)');
                                return [[formattedDate] , [label + ': ' + value]];
                            }
                            else if(tooltips[0].datasetIndex == 1){
                                var label = "Sensor Savings ";
                                var value = data.datasets[tooltips[0].datasetIndex].labels[tooltips[0].index];
                                var t = tooltips[0],
                                date = t.xLabel;
                                var formattedDate =  moment(date).tz(props.time_zone).format('MM/DD/YY HH:mm:ss z (Z)');
                                return [formattedDate , label + ': ' + value];
                            }
                            else {
                                var t = tooltips[0],
                                date = t.xLabel;
                                var formattedDate =  moment(date).tz(props.time_zone).format('MM/DD/YY HH:mm:ss z (Z)');
                                return [formattedDate];
                            } 


                        }
                    }
                }
            }
        });
    
        self.drawLines(
            sensors,
            props.selected_nodes,
            [props.selected_sites],
            props.selected_conversions,
            props.from,
            props.to,
            props.time_zone,
        );
    },
    
    toggleLines: function() {
        var self = this,
            chart = self.chart;

        if (chart.options.showLines) {
            chart.options.showLines = false;
        } else {
            chart.options.lineTension = 0;
            chart.options.showLines = true;
        }

        chart.update();
    },
    node_query_api: function(node, sensor, from, to, limit, callback) {
        var self = this,
            duration = moment.duration(moment(to).diff(moment(from))),
            limit = 500;

        /*if (duration < moment.duration(1, 'hours')) {
            limit = 60;
        } else if (duration < moment.duration(1, 'days')) {
            limit = 100;
        } else if (duration < moment.duration(1, 'weeks')) {
            limit = 200;
        } else if (duration < moment.duration(1, 'months')) {
            limit = 500;
        } else {
            limit = 100;
        } */

        $.ajax({
            url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/nodes/' + node + '/sensors/' + 'energy' + '/from/' + from.toISOString() + '/to/' + to.toISOString() + '/limit/' + limit + '/period/' + '1hr',
            data: '',
            method: 'GET',
            xhrFields: {
                withCredentials: true
            },
            dataType: 'json',
            success: function(data) {
                callback(null, data);
            },
            error: function(jqXHR, status, error) {
                callback(error);
            }
        });
        
    },

    site_query_api: function(site, sensor, from, to, limit, callback) {
        var self = this,
            duration = moment.duration(moment(to).diff(moment(from))),
            limit = 500;

       /* if (duration < moment.duration(1, 'hours')) {
            limit = 60;
        } else if (duration < moment.duration(1, 'days')) {
            limit = 100;
        } else if (duration < moment.duration(1, 'weeks')) {
            limit = 200;
        } else if (duration < moment.duration(1, 'months')) {
            limit = 500;
        } else {
            limit = 100;
        } */

        $.ajax({
            url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + site + '/sensors/' + 'energy' + '/from/' + from.toISOString() + '/to/' + to.toISOString() + '/limit/' + limit + '/period/' + '1hr',
            data: '',
            method: 'GET',
            xhrFields: {
                withCredentials: true
            },
            dataType: 'json',
            success: function(data) {
                console.log(data);
                callback(null, data);
            },
            error: function(jqXHR, status, error) {
                callback(error);
            }
        });
        
    },
    RGBAcolors: function() {
        return [
           'rgba(' + 212 + ', ' + 144 + ', ' + 195 + ', 0.5)',
           'rgba(' + 195 + ', ' + 212 + ', ' + 144 + ', 0.25)',
           'rgba(' + 144 + ', ' + 195 + ', ' + 212 + ', 0.25)',
        ];
    }
});

module.exports = EnergyGraph;
