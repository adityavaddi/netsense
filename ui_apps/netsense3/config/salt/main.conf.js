var config = {}

config.referenceImplementation = {}

config.referenceImplementation = {
    protocol: 'https',
    host: '{{api-fqdn}}',
    port: 8090,
    webpackServerPort: 8089
}

config.interface = {}
config.interface = {
    protocol: 'https',
    host: '{{api-fqdn}}',
    port: 10010,
    version: 'v3.0',
}

module.exports = config;
