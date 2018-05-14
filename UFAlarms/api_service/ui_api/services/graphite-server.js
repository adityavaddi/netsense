var net = require('net'),
    configManager = require('kea-config');

configManager.setup('./config/');

var init = function(callback) {
    try{
        //var connectCount = 0;
        var server = net.createServer(function (client) {
            console.log("Reporter connected.");
            client.on('data', function(data) {
                var log_to_console = configManager.get('graphite').log_to_console;
                if(log_to_console===undefined || log_to_console){
                    console.log("Got data: %s", data);
                }
            });

        });

        server.on('error', function(err) {
            console.log("Got error :( %s", err);
        });

        console.log("Starting fake graphite server.");
        server.listen(configManager.get('graphite').port||2003, function() {
            var address = server.address();
            console.log("Fake graphite server started at %s:%s", address.address, address.port);
        });

        return server;

    } catch (err){
        global.log.error("Could not start fake graphite server %s", err.message, err);

    }
};


module.exports = init();
