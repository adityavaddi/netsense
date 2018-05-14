import classNames from 'classnames';
import auth from 'global/utils/auth';
import helpers from 'global/utils/helpers';

import {
    State,
    Navigation
} from 'react-router';

var ReportingGraph = React.createClass({
    propTypes: {
        selected_sensor1: React.PropTypes.object.isRequired,
        selected_nodes: React.PropTypes.array.isRequired,
        time_zone: React.PropTypes.string.isRequired
    },
    componentDidMount: function() {
        var self = this;

        self.drawChart(self.props);

        ReactBootstrap.Dispatcher.on('ReportingGraph.toggleLines', function() {
            self.toggleLines();
        });

        // ReactBootstrap.Dispatcher.on('ReportingGraph.downloadImg', function(btn) {
        //     $(btn).attr({
        //         href: self.chart.toBase64Image(),
        //         download: 'graph.png'
        //     });
        // });

        ReactBootstrap.Dispatcher.on('ReportingGraph.downloadCSV', function(btn) {
            var data = [],
                csvContent = "data:text/csv;charset=utf-8,",
                dataString = '';

            self.currentData.forEach(function(res) {
                res.data.forEach(function(d) {
                    data.push([
                        moment(d.time)
                            .tz(self.props.time_zone)
                            .format('MM/DD/YY HH:mm:ss z (Z)'),
                        res.node.nodeid,
                        res.sensor.name,
                        d.value
                    ]);
                });
            });

            data.unshift([
                'time',
                'node',
                'sensor',
                'value'
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

        if (typeof self.chart != "undefined") {
            self.chart.destroy();
        };

        self.drawChart(nextProps);
    },
    componentWillUnmount: function() {
        ReactBootstrap.Dispatcher.removeAllListeners('ReportingGraph.toggleLines');
        // ReactBootstrap.Dispatcher.removeAllListeners('ReportingGraph.downloadImg');
        ReactBootstrap.Dispatcher.removeAllListeners('ReportingGraph.downloadCSV');
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
    drawChart: function(props) {
        var self = this,
            sensors = [props.selected_sensor1];

        if (props.selected_sensor2) {
            sensors.push(props.selected_sensor2);
        }

        var sensor1 = sensors[0];
        if((sensor1.id == "tsys") || (sensor1.id == "tcore")){
            sensor1.units = " (C)";
             var yAxes = [{
                type: 'linear',
                position: 'left',
                id: sensor1.id,
                scaleLabel: {
                    display: true,
                    labelString: sensor1.name + sensor1.units
                }
            }];
        }
        else{
             var yAxes = [{
                type: 'linear',
                position: 'left',
                id: sensor1.id,
                scaleLabel: {
                    display: true,
                    labelString: sensor1.name + sensor1.units
                }
            }];
        }
       

        if (sensors.length > 1) {
            var sensor2 = sensors[1];

            if((sensor2.id == "tsys") || (sensor2.id == "tcore")){
                sensor2.units = " (C)";
                yAxes.push({
                    type: 'linear',
                    position: 'right',
                    id: sensor2.id,
                    scaleLabel: {
                        display: true,
                        labelString: sensor2.name + sensor2.units
                    }
                });
            }
            else{
                yAxes.push({
                    type: 'linear',
                    position: 'right',
                    id: sensor2.id,
                    scaleLabel: {
                        display: true,
                        labelString: sensor2.name + sensor2.units
                    }
                });
            }

            
        }

        self.chart = new Chart(document.getElementById('chart'), {
            type: 'line',
            data: {
                datasets: []
            },
            options: {
                showLines: true,
                legend: {
                    position: 'bottom',
                    onClick: function(ev, item) {
                        var n = item.datasetIndex,
                            dataset = self.chart.data.datasets[n];

                        if (!ev.altKey) {
                            if (dataset.hidden === null) {
                                self.chart.data.datasets[n].hidden = true;
                            } else {
                                self.chart.data.datasets[n].hidden = !dataset.hidden;
                            }
                        } else {
                            var colors = self.RGBAcolors();

                            self.chart.data.datasets[n].borderColor = colors[0];
                            self.chart.data.datasets[n].backgroundColor = colors[1];
                        }

                        self.chart.update();

                    }
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
                    callbacks: {
                        title: function(tooltips) {
                            var t = tooltips[0],
                                date = t.xLabel;

                            return moment(date)
                                .tz(props.time_zone)
                                .format('MM/DD/YY HH:mm:ss z (Z)');
                        }
                    }
                }
            }
        });

        self.drawLines(
            sensors,
            props.selected_nodes,
            props.from,
            props.to,
            props.time_zone,
        );
    },
    drawLines: function(sensors, nodes, from, to, time_zone) {
        var self = this,
            queries = [];

        nodes.forEach(function(node) {
            sensors.forEach(function(sensor) {
                queries.push({
                    node: node,
                    sensor: sensor
                });
            });
        });

        async.map(queries, function(query, cb) {

            // unode-v2 model specific sensors:
            if(query.node.model == "unode-v2"){
                if(query.sensor.id == "mw"){
                    query.sensor.id = "w";
                }
                if(query.sensor.id == "mP"){
                    query.sensor.id = "P";
                }
                if(query.sensor.id == "mPF"){
                    query.sensor.id = "PF";
                }
                if(query.sensor.id == "mi"){
                    query.sensor.id = "i";
                }
            }
            else{
                if((query.sensor.id == "mw") || (query.sensor.id == "w")){
                    query.sensor.id = "mw";
                }
                if((query.sensor.id == "mP") || (query.sensor.id == "P")){
                    query.sensor.id = "mP";
                }
                if((query.sensor.id == "mPF") || (query.sensor.id == "PF")){
                    query.sensor.id = "mPF";
                }
                if((query.sensor.id == "mi") || (query.sensor.id == "i")){
                    query.sensor.id = "mi";
                }
            }

            self.query_api(query.node.nodeid, query.sensor.id, from, to, 100, function(err, data) {
                if(typeof data !== "undefined"){
                    query.data = data.datapoints;
                }
                cb(err, query);
            });
        }, function(err, results) {
            if (!err) {

                self.currentData = results;

                var datasets = results.map(function(query) {
                    var node = query.node,
                        sensor = query.sensor;
                        if((query.sensor.id == "tsys") || (query.sensor.id == "tcore")){
                            var xyData = query.data.map(function(d) {
                                return {
                                    x: moment(d.time).tz(time_zone).format(),
                                    y: d.value/1000
                                }
                            });
                        }
                        else{
                            var xyData = query.data.map(function(d) {
                                return {
                                    x: moment(d.time).tz(time_zone).format(),
                                    y: d.value
                                }
                            });
                        }

                        return {
                            steppedLine:'before',
                            label: node.nodeid + ' - ' + sensor.name,
                            yAxisID: sensor.id,
                            data: xyData,
                            borderWidth: 2
                        };
                });

                self.chart.data.datasets = datasets;

                var colors = self.RGBAcolors();

               if(sensors.length>1){
                self.chart.data.datasets[0].backgroundColor = colors[0];
                self.chart.data.datasets[0].borderColor = colors[0];
                self.chart.data.datasets[1].backgroundColor = colors[1];
                self.chart.data.datasets[1].borderColor = colors[1];
               }
               else{
                self.chart.data.datasets[0].backgroundColor = colors[1];
                self.chart.data.datasets[0].borderColor = colors[1];
               }

                self.chart.update();
            }
        });
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
    query_api: function(node, sensor, from, to, limit, callback) {
        var self = this,
            duration = moment.duration(moment(to).diff(moment(from))),
            limit = 100;

        if (duration < moment.duration(1, 'hours')) {
            limit = 60;
        } else if (duration < moment.duration(1, 'days')) {
            limit = 100;
        } else if (duration < moment.duration(1, 'weeks')) {
            limit = 200;
        } else if (duration < moment.duration(1, 'months')) {
            limit = 500;
        } else {
            limit = 100;
        }

        $.ajax({
            url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/nodes/' + node + '/sensors/' + sensor + '/from/' + from.toISOString() + '/to/' + to.toISOString() + '/limit/' + limit,
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
        })
    },
    RGBAcolors: function() {
        return [
           'rgba(' + 212 + ', ' + 144 + ', ' + 195 + ', 0.5)',
           'rgba(' + 195 + ', ' + 212 + ', ' + 144 + ', 0.25)',
           'rgba(' + 144 + ', ' + 195 + ', ' + 212 + ', 0.25)',
        ];
    }
});

module.exports = ReportingGraph;