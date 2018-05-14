var config = {}

config.datadealer = {}

config.datadealer.URL = {
    mpkrequester: 'tcp://localhost:5522',
    jsrequester: 'tcp://localhost:5521',
    jsrequester_act_log: 'tcp://localhost:5511'
}
config.mosca = {
    port: 3002
}

module.exports = config;
