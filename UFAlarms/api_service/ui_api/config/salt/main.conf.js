var config = {};

config.domain = {
    ui: '{{api-fqdn}}', // customer root domain
};
config.datadealer = {};

config.datadealer.URL = {
    mpkrequester: 'tcp://{{datadealer.local-ip}}:5522',
    jsrequester: 'tcp://{{datadealer.local-ip}}:5521',
    jsrequester_act_log: 'tcp://{{datadealer.local-ip}}:5511'
};

config.mosca = {
    skip: true,
    host: "localhost",
    port: 3002
};

config.sse = {
    url:'mqtt://{{mqtt.local-hostname}}:1883',
};

config.ldap = {
    URL:'ldap://{{ldap.local-ip}}:389',  // OpenLDAP
    jsURL:'ldap://127.0.0.1:1389',
    suffixUsers: 'ou=People,dc=sensity,dc=com',
    suffixApiKeys: 'ou=ApiKeys,dc=sensity,dc=com',
    adminDN: 'cn=admin,dc=sensity,dc=com',
    adminPass: 'admin123'
    //suffixUsers: 'o=users',
    //suffixApiKeys: 'o=apikeys',
};

config.sendmail = {
    path: '/usr/sbin/sendmail',
    disabled: false
};

config.password_reset = {
    from: 'Sensity Admin <admin@sensity.com>', // sender address
    subject: 'Password reset', // Subject line
    text: 'Click here to reset password: [url]', // plaintext body
    html: 'Click <a href="[url]">here</a> to reset password' // html body
};

config.password_send = {
    from: 'Sensity Admin <admin@sensity.com>', // sender address
    subject: 'Your account is ready', // Subject line
    text: 'new-account.txt', // plaintext body
    html: 'new-account.html' // html body
};

config.api_key = {
    from: 'Sensity Admin <admin@sensity.com>', // sender address
    subject: 'Sensity API Key', // Subject line
    text: 'API Key: [api_key]', // plaintext body
    html: 'API Key: [api_key]' // html body

};

config.notifications = {
    subscriptions: [
        {   // API service local
            uri:'mqtt://10.0.8.2:3002',
            topics:['/test/alert', '/streamv1/+/+/+/DeviceAlarm']
        }
    ],
    sms:{
        disabled: false
    },
    email: {
        disabled: false,
        from: 'Sensity Admin <admin@sensity.com>', // sender address
        subject: 'Sensity notification', // Subject line
    }

};

config.acl = {
    disabled: false
};

config.graphite = {
    host: '{{graphite.local-ip}}',
    port: 2003,
    reporting_interval: 5000,
    log_to_console: false
};

module.exports = config;

