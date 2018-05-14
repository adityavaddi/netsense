var config = {}

config.datadealer = {}

config.datadealer.URL = {
    mpkrequester: 'tcp://ip-10-1-0-192:5522',
    jsrequester: 'tcp://ip-10-1-0-192:5521',
    jsrequester_act_log: 'tcp://ip-10-1-0-192:5511'
}
config.mosca = {
    port: 3002
}

module.exports = config;
