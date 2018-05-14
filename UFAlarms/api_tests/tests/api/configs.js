/**
 * Created by brefsdal on 3/14/16.
 */
"use strict";
var server_url = process.env.stack_url;
var should = require('should');
var request = require('supertest')(server_url);
var superagent = require('superagent');
var agent = superagent.agent();
var version = '/v3.0';
var default_unode_v4 = {
    model: 'unode-v4',
    aux_power: true,
    debugmode: true,
    name: 'default_unode-v4',
    networkXPasskey: 'kentspeed',
    networkXSSID: 'XeraL6',
    networkXSecurity: 'wpa2p',
    networkYPasskey: 'kentspeed',
    networkYSSID: 'XeraL4',
    networkYSecurity: 'wpa2p',
    network_region: 'US',
    ota_disable: false,
    sensor_T_dint: 30000,
    sensor_T_mode: 1,
    sensor_T_pint: 3600000,
    sensor_aPF_dint: 30000,
    sensor_aPF_mode: 1,
    sensor_aPF_pint: 3600000,
    sensor_aP_dint: 30000,
    sensor_aP_mode: 1,
    sensor_aP_pint: 3600000,
    sensor_ai_dint: 30000,
    sensor_ai_mode: 1,
    sensor_ai_pint: 3600000,
    sensor_aip_dint: 30000,
    sensor_aip_mode: 1,
    sensor_aip_pint: 3600000,
    sensor_aw_dint: 30000,
    sensor_aw_mode: 1,
    sensor_aw_pint: 3600000,
    sensor_lIR_dint: 30000,
    sensor_lIR_i_dint: 30000,
    sensor_lIR_i_mode: 1,
    sensor_lIR_i_pint: 3600000,
    sensor_lIR_mode: 1,
    sensor_lIR_pint: 3600000,
    sensor_l_dint: 30000,
    sensor_l_i_dint: 30000,
    sensor_l_i_mode: 1,
    sensor_l_i_pint: 3600000,
    sensor_l_mode: 1,
    sensor_l_pint: 3600000,
    sensor_mPF_dint: 30000,
    sensor_mPF_mode: 1,
    sensor_mPF_pint: 3600000,
    sensor_mP_dint: 30000,
    sensor_mP_mode: 1,
    sensor_mP_pint: 3600000,
    sensor_mi_dint: 30000,
    sensor_mi_mode: 1,
    sensor_mi_pint: 3600000,
    sensor_mip_dint: 30000,
    sensor_mip_mode: 1,
    sensor_mip_pint: 3600000,
    sensor_mt_dint: 30000,
    sensor_mt_mode: 1,
    sensor_mt_pint: 3600000,
    sensor_mw_dint: 30000,
    sensor_mw_mode: 1,
    sensor_mw_pint: 3600000,
    sensor_p_dint: 5000,
    sensor_p_mode: 2,
    sensor_p_pint: 1000,
    sensor_pc_dint: 30000,
    sensor_pc_mode: 1,
    sensor_pc_pint: 3600000,
    sensor_rf_dint: 30000,
    sensor_rf_mode: 1,
    sensor_rf_pint: 3600000,
    sensor_t_dint: 30000,
    sensor_t_mode: 1,
    sensor_t_pint: 3600000,
    sensor_v_dint: 30000,
    sensor_v_mode: 1,
    sensor_v_pint: 3600000,
    sensor_vp_dint: 30000,
    sensor_vp_mode: 1,
    sensor_vp_pint: 3600000,
    server: 'nsn-local.sensity.com',
    telnet: true,
    vpn_on_demand: false
};

var default_falcon_q_config = {
    'name': 'Falcon default configuration',
    'Genetec.General.Brand.CompanyName': 'Sensity',
    'Genetec.General.Brand.ModelName': 'Video Node',
    'Genetec.General.FirmwareVersion': '1.00',
    'Genetec.General.ProtocolVersion': '1.00',
    'Genetec.Network.Services.Http.Port': 8000,
    'Genetec.custom_events': 'enter,left',
    'alarm.redis.memory_threshold': 0.9,
    application_server: 'webrtc.sensity.com',
    application_server_port: 8443,
    aux_power: true,
    'camera.0.autoexposure': true,
    'camera.0.autogain': true,
    'camera.0.colortemp': 6500,
    'camera.0.enabled': true,
    'camera.0.exposure': 15,
    'camera.0.flickermode': '60Hz',
    'camera.0.gain': 15,
    'camera.0.indoormode': false,
    'camera.0.master': 0,
    'camera.0.scanmode': '',
    'camera.0.streamH.bitrate': 2500000,
    'camera.0.streamH.format': '720p',
    'camera.0.streamH.framerate': 15,
    'camera.0.streamH.gopsize': 30,
    'camera.0.streamH.path': '/0/H',
    'camera.0.streamH.storage.diskquota': 0.25,
    'camera.0.streamH.storage.path': '/0/H',
    'camera.0.streamH.storage.recordevents': 'Car Entering',
    'camera.0.streamH.storage.recordmode': 0,
    'camera.0.streamH.storage.recordpostamble': 30,
    'camera.0.streamH.storage.recordpreamble': 30,
    'camera.0.streamH.storage.recordrestart': '0',
    'camera.0.streamH.storage.vaqa.0.duration': '3m',
    'camera.0.streamH.storage.vaqa.0.interval': '1h',
    'camera.0.streamH.storage.vaqa.0.qty': 24,
    'camera.0.streamH.storage.vaqa.1.duration': '3m',
    'camera.0.streamH.storage.vaqa.1.interval': '25h',
    'camera.0.streamH.storage.vaqa.1.qty': 0,
    'camera.0.streamH.storage.vaqa.status': '',
    'camera.0.streamL.bitrate': 500000,
    'camera.0.streamL.format': '360p',
    'camera.0.streamL.framerate': 5,
    'camera.0.streamL.gopsize': 5,
    'camera.0.streamL.path': '/0/L',
    'camera.0.streamL.storage.diskquota': 0.25,
    'camera.0.streamL.storage.path': '/0/L',
    'camera.0.streamL.storage.recordevents': 'Car Entering',
    'camera.0.streamL.storage.recordmode': 1,
    'camera.0.streamL.storage.recordpostamble': 30,
    'camera.0.streamL.storage.recordpreamble': 30,
    'camera.0.streamL.storage.recordrestart': '0',
    'camera.0.wdrmode': true,
    'camera.1.autoexposure': true,
    'camera.1.autogain': true,
    'camera.1.colortemp': 6500,
    'camera.1.enabled': true,
    'camera.1.exposure': 15,
    'camera.1.flickermode': '60Hz',
    'camera.1.gain': 15,
    'camera.1.indoormode': false,
    'camera.1.scanmode': '',
    'camera.1.streamH.bitrate': 2500000,
    'camera.1.streamH.format': '720p',
    'camera.1.streamH.framerate': 15,
    'camera.1.streamH.gopsize': 30,
    'camera.1.streamH.path': '/1/H',
    'camera.1.streamH.storage.diskquota': 0.25,
    'camera.1.streamH.storage.path': '/1/H',
    'camera.1.streamH.storage.recordevents': 'Car Entering',
    'camera.1.streamH.storage.recordmode': 0,
    'camera.1.streamH.storage.recordpostamble': 30,
    'camera.1.streamH.storage.recordpreamble': 30,
    'camera.1.streamH.storage.recordrestart': '0',
    'camera.1.streamH.storage.vaqa.0.duration': '3m',
    'camera.1.streamH.storage.vaqa.0.interval': '1h',
    'camera.1.streamH.storage.vaqa.0.qty': 24,
    'camera.1.streamH.storage.vaqa.1.duration': '3m',
    'camera.1.streamH.storage.vaqa.1.interval': '25h',
    'camera.1.streamH.storage.vaqa.1.qty': 0,
    'camera.1.streamH.storage.vaqa.status': '',
    'camera.1.streamL.bitrate': 500000,
    'camera.1.streamL.format': '360p',
    'camera.1.streamL.framerate': 5,
    'camera.1.streamL.gopsize': 5,
    'camera.1.streamL.path': '/1/L',
    'camera.1.streamL.storage.diskquota': 0.25,
    'camera.1.streamL.storage.path': '/1/L',
    'camera.1.streamL.storage.recordevents': 'Car Entering',
    'camera.1.streamL.storage.recordmode': 1,
    'camera.1.streamL.storage.recordpostamble': 30,
    'camera.1.streamL.storage.recordpreamble': 30,
    'camera.1.streamL.storage.recordrestart': '0',
    'camera.1.wdrmode': true,
    debugmode: true,
    'evtgrab.capint': 3600,
    'evtgrab.dint': 600,
    'evtgrab.dir': '/disk/evtgrab',
    'evtgrab.nfiles': 1000,
    'medianode.commissioned': false,
    'mediaserver.nomve': false,
    'mediaserver.video_codec': 'h264',
    'mediaserver.wmm_qos': true,
    model: 'falcon-q',
    'network.eth-x.ip': '172.17.100.1',
    'network.eth-x.method': 'auto',
    'network.eth-x.netmask': 24,
    'network.eth-x.port-forwarding.genetec': '',
    'network.eth-x.port-forwarding.https': '',
    'network.eth-x.port-forwarding.rtsp': '',
    'network.eth-x.port-forwarding.ssh': '',
    'network.eth-x.whitelist': '',
    'network.firewall.ports': '',
    'network.firewall.protocols': '',
    'network.nocell': true,
    'network.nowifi': false,
    'network.ppp-x.apn': 'VZWINTERNET',
    'network.region': 'US',
    'network.server.mqtt': 'mqtt.sensity.com',
    'network.server.vpn': 'medianodevpn.sensity.com',
    'network.vpn_on_demand': false,
    'network.wlan-x.ipv6.method': 'ignore',
    'network.wlan-x.security': 'wpa2p',
    'network.wlan-x.security.bgscan-long-interval': 360,
    'network.wlan-x.security.bgscan-short-interval': 60,
    'network.wlan-x.security.bgscan-signal-threshold': -60,
    'network.wlan-x.security.psk': 'netsense',
    'network.wlan-x.ssid': 'SensityDefault',
    'network.wlan-y.ipv6.method': 'ignore',
    'network.wlan-y.security': 'wpa2p',
    'network.wlan-y.security.bgscan-long-interval': 360,
    'network.wlan-y.security.bgscan-short-interval': 60,
    'network.wlan-y.security.bgscan-signal-threshold': -60,
    'network.wlan-y.security.psk': 'kentspeed',
    'network.wlan-y.ssid': 'XeraL2',
    'rtsp.service': 554,
    'sensor-events.maxqlen': 1024,
    'sensor.p.dint': 530,
    'sensor.p.mode': 2,
    'sensor.p.pint': 300000000,
    server: 'prod.sensity.com',
    'storage.maxsessions': 15,
    'storageserver.cache_size': 10000000000,
    'storageserver.max_clip_size': 200000000,
    'vaevent.bucket_depth': 100,
    'vaevent.bucket_drop': 10,
    'vaevent.bucket_interval': '1',
    'vaevent.qlen': 500
}
var grpVideoConfigId = '',
    grpCoreConfigId = '',
    siteVideoConfigId = '',
    siteCoreConfigId = '',
    grpid = '';

const helpers = require('./../../utils/helpers');
let csrfToken = null;
/*
 Datadealer needs to be running for these tests to be successful
 % lein with-profile testjs run all

 Run suite of tests with
 % cd Farallones/api_service/ui_api
 % mocha --recursive
 */


describe('Configs', function () {

    describe('GET /customers/{orgid}/sites/{siteid}/configs', function () {

        var existing_configs = 0;
        var configid = 0;

        it('should fail without login credentials', function (done) {
            request
                .get(version + '/customers/uberorg/sites/ubersite/configs')
                .set('Accept', 'application/json')
                .expect('Content-Type', /json/)
                .expect(403)
                .end(function (err, res) {
                    should.not.exist(err);
                    res.body.should.eql({
                        error: true,
                        message: 'Access denied',
                        status: 403
                    });
                    done();
                });
        });

        it('should sign in with correct credentials', function (done) {
            var data = {
                email: 'uberuser@sensity.com',
                password: 'ubeR$23'
            };

            request
                .post(version + '/login')
                .send(data)
                .set('Accept', 'application/json')
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    agent.saveCookies(res);
                    csrfToken = helpers.getCsrfToken(res);
                    done();
                });

        });

        it('should access config with login credentials', function (done) {
            var req = request.get(version + '/customers/uberorg/sites/ubersite/configs');

            agent.attachCookies(req);
            req
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    existing_configs = res.body.length;
                    done();
                });
        });
        it('should create a config using credentials', function (done) {
            var data = JSON.parse(JSON.stringify(default_unode_v4)),
                req = request.post(version + '/customers/uberorg/sites/ubersite/configs');

            agent.attachCookies(req);
            req.send(data)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    configid = res.body.configid;
                    done();
                });
        });

        it('should get, update, and delete config using credentials', function (done) {

            var uri = version + '/customers/uberorg/sites/ubersite/configs/' + configid,
                data = JSON.parse(JSON.stringify(default_unode_v4)),
                req = request.get(uri);

            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {

                    delete res.body.configid;
                    delete res.body.nodes;
                    res.body.should.eql(data);

                    var updated = JSON.parse(JSON.stringify(default_unode_v4)),
                        req2 = request.post(uri);
                    //updated.server = 'dev.sensity.com';

                    agent.attachCookies(req2);
                    req2.send(updated)
                        .set('Accept', 'application/json')
                        .set('X-CSRF-Token', csrfToken)
                        .expect('Content-Type', /json/)
                        .expect(200)
                        .end(function (err, res2) {
                            should.not.exist(err);

                            var result = res2.body;

                            delete result.configid;
                            delete result.nodes;
                            //result.should.eql(updated);

                            var delete_req = request.delete(uri);

                            agent.attachCookies(delete_req);
                            delete_req
                                .set('Accept', 'application/json')
                                .set('X-CSRF-Token', csrfToken)
                                .expect(204)
                                .end(function (err, res) {
                                    should.not.exist(err);

                                    var get_req = request.get(uri);

                                    agent.attachCookies(get_req);
                                    get_req.set('Accept', 'application/json')
                                        .expect('Content-Type', /json/)
                                        .expect(404)
                                        .end(function (err, res) {
                                            done();
                                        });
                                });
                        });
                });
        });

        it('should access config with login credentials', function (done) {
            var req = request.get(version + '/customers/uberorg/sites/ubersite/configs');

            agent.attachCookies(req);
            req
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    res.body.length.should.eql(existing_configs);
                    done();
                });
        });
    });

    describe('GET /configs/{model}', function () {

        it('should fail without login credentials', function (done) {
            request
                .get(version + '/configs/unode-v4')
                .set('Accept', 'application/json')
                .expect('Content-Type', /json/)
                .expect(403)
                .end(function (err, res) {
                    should.not.exist(err);
                    res.body.should.eql({
                        error: true,
                        message: 'Access denied',
                        status: 403
                    });
                    done();
                });
        });

        it('should sign in with correct credentials', function (done) {
            var data = {
                email: 'uberuser@sensity.com',
                password: 'ubeR$23'
            };

            request
                .post(version + '/login')
                .send(data)
                .set('Accept', 'application/json')
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    agent.saveCookies(res);
                    csrfToken = helpers.getCsrfToken(res);
                    done();
                });
        });

        it('should access default config with login credentials', function (done) {
            var req = request.get(version + '/configs/unode-v4');

            agent.attachCookies(req);
            req
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    //res.body.configid.should.eql('default');
                    res.body.name.should.eql('default_unode-v4');
                    done();
                });
        });

        it('should access default config with login credentials for ubersite', function (done) {
            var req = request.get(version + '/customers/uberorg/sites/ubersite/config/unode-v4');

            agent.attachCookies(req);
            req
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    // The site is in US by lat/lng but that's also the default
                    // so we cheat to do a better test
                    res.body.network_region.should.eql('UK');
                    done();
                });
        });
    });

    describe('Core Node Config', function () {
        var configid;

        it('should create a v3 node (N3CONFIG) using credentials', function (done) {

            var data = { "csvNodeList": "nodeid,model,orgid,siteid\nN3CONFIG,unode-v3,uberorg,ubersite" },
                req = request.put(version + '/nodes');

            agent.attachCookies(req);
            req.send(data)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    var result = res.body;
                    delete result.time_zone;
                    done();
                });
        });

        it('should create a v4 node (N4CONFIG) using credentials', function (done) {

            var data = { "csvNodeList": "nodeid,model,orgid,siteid\nN4CONFIG,unode-v4,uberorg,ubersite" },
                req = request.put(version + '/nodes');

            agent.attachCookies(req);
            req.send(data)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    var result = res.body;
                    delete result.time_zone;
                    done();
                });
        });

        it('should create a config', function (done) {
            var data = {
                model: 'unode-v4',
                aux_power: true,
                debugmode: true,
                name: 'default_unode-v4',
                networkXPasskey: 'kentspeed',
                networkXSSID: 'XeraL6',
                networkXSecurity: 'wpa2p',
                networkYPasskey: 'kentspeed',
                networkYSSID: 'XeraL4',
                networkYSecurity: 'wpa2p',
                network_region: 'US',
                ota_disable: false,
                sensor_T_dint: 30000,
                sensor_T_mode: 1,
                sensor_T_pint: 3600000,
                sensor_aPF_dint: 30000,
                sensor_aPF_mode: 1,
                sensor_aPF_pint: 3600000,
                sensor_aP_dint: 30000,
                sensor_aP_mode: 1,
                sensor_aP_pint: 3600000,
                sensor_ai_dint: 30000,
                sensor_ai_mode: 1,
                sensor_ai_pint: 3600000,
                sensor_aip_dint: 30000,
                sensor_aip_mode: 1,
                sensor_aip_pint: 3600000,
                sensor_aw_dint: 30000,
                sensor_aw_mode: 1,
                sensor_aw_pint: 3600000,
                sensor_lIR_dint: 30000,
                sensor_lIR_i_dint: 30000,
                sensor_lIR_i_mode: 1,
                sensor_lIR_i_pint: 3600000,
                sensor_lIR_mode: 1,
                sensor_lIR_pint: 3600000,
                sensor_l_dint: 30000,
                sensor_l_i_dint: 30000,
                sensor_l_i_mode: 1,
                sensor_l_i_pint: 3600000,
                sensor_l_mode: 1,
                sensor_l_pint: 3600000,
                sensor_mPF_dint: 30000,
                sensor_mPF_mode: 1,
                sensor_mPF_pint: 3600000,
                sensor_mP_dint: 30000,
                sensor_mP_mode: 1,
                sensor_mP_pint: 3600000,
                sensor_mi_dint: 30000,
                sensor_mi_mode: 1,
                sensor_mi_pint: 3600000,
                sensor_mip_dint: 30000,
                sensor_mip_mode: 1,
                sensor_mip_pint: 3600000,
                sensor_mt_dint: 30000,
                sensor_mt_mode: 1,
                sensor_mt_pint: 3600000,
                sensor_mw_dint: 30000,
                sensor_mw_mode: 1,
                sensor_mw_pint: 3600000,
                sensor_p_dint: 5000,
                sensor_p_mode: 2,
                sensor_p_pint: 1000,
                sensor_pc_dint: 30000,
                sensor_pc_mode: 1,
                sensor_pc_pint: 3600000,
                sensor_rf_dint: 30000,
                sensor_rf_mode: 1,
                sensor_rf_pint: 3600000,
                sensor_t_dint: 30000,
                sensor_t_mode: 1,
                sensor_t_pint: 3600000,
                sensor_v_dint: 30000,
                sensor_v_mode: 1,
                sensor_v_pint: 3600000,
                sensor_vp_dint: 30000,
                sensor_vp_mode: 1,
                sensor_vp_pint: 3600000,
                server: 'nsn-local.sensity.com',
                telnet: true,
                vpn_on_demand: false
            },
                req = request.post(version + '/customers/uberorg/sites/ubersite/configs');

            agent.attachCookies(req);
            req.send(data)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);

                    var result = res.body;

                    configid = result.configid;

                    delete result.configid;
                    delete result.nodes;

                    result.should.eql(data);
                    done();
                });
        });

        it('should apply config to v4 node', function (done) {
            var data = {
                nodeList: ['N4CONFIG']
            },
                req = request.post(version + '/customers/uberorg/sites/ubersite/configs/' + configid + '/apply/nodes');

            agent.attachCookies(req);
            req.send(data)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);

                    var result = res.body;

                    should.exist(result.nodes[0]);
                    result.nodes[0].nodeid.should.eql('N4CONFIG');
                    done();
                });
        });

        it('should fail to apply config to v3 node', function (done) {
            var data = {
                nodeList: ['N3CONFIG']
            },
                req = request.post(version + '/customers/uberorg/sites/ubersite/configs/' + configid + '/apply/nodes');

            agent.attachCookies(req);
            req.send(data)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(400)
                .end(function (err, res) {
                    done();
                });
        });


    });


    describe('Video Node Config', function () {

        var configid = 0;
        var nodeid = 'N2flcnqCONFIG';

        it('should fail without login credentials', function (done) {
            request
                .get(version + '/configs/falcon-q')
                .set('Accept', 'application/json')
                .expect('Content-Type', /json/)
                .expect(403)
                .end(function (err, res) {
                    should.not.exist(err);
                    res.body.should.eql({
                        error: true,
                        message: 'Access denied',
                        status: 403
                    });
                    done();
                });
        });

        it('should sign in with correct credentials', function (done) {
            // this.timeout(1e5);

            var data = {
                email: 'uberuser@sensity.com',
                password: 'ubeR$23'
            };

            request
                .post(version + '/login')
                .send(data)
                .set('Accept', 'application/json')
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    agent.saveCookies(res);
                    csrfToken = helpers.getCsrfToken(res);
                    done();
                });
        });

        it('should create a config using credentials', function (done) {
            var data = JSON.parse(JSON.stringify(default_falcon_q_config)),
                req = request.post(version + '/customers/uberorg/sites/ubersite/configs');

            agent.attachCookies(req);
            req.send(data)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    configid = res.body.configid;
                    should.not.exist(err);
                    done();
                });
        });

        it('should get, update, and delete config using credentials', function (done) {
            // this.timeout(1e5);

            var uri = version + '/customers/uberorg/sites/ubersite/configs/' + configid,
                data = JSON.parse(JSON.stringify(default_falcon_q_config)),
                req = request.get(uri);

            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {

                    delete res.body.configid;
                    delete res.body.nodes;
                    //res.body.should.eql(data);

                    var updated = JSON.parse(JSON.stringify(default_falcon_q_config)),
                        req2 = request.post(uri);
                    updated.debugmode = false;

                    agent.attachCookies(req2);
                    req2.send(updated)
                        .set('Accept', 'application/json')
                        .set('X-CSRF-Token', csrfToken)
                        .expect('Content-Type', /json/)
                        .expect(200)
                        .end(function (err, res2) {
                            should.not.exist(err);

                            var delete_req = request.delete(uri);

                            agent.attachCookies(delete_req);
                            delete_req
                                .set('Accept', 'application/json')
                                .set('X-CSRF-Token', csrfToken)
                                .expect(204)
                                .end(function (err, res) {
                                    should.not.exist(err);

                                    var get_req = request.get(uri);

                                    agent.attachCookies(get_req);
                                    get_req.set('Accept', 'application/json')
                                        .expect('Content-Type', /json/)
                                        .expect(404)
                                        .end(function (err, res) {
                                            done();
                                        });
                                });
                        });
                });
        });

        var total_configs = 0;
        it('should access config with login credentials', function (done) {
            var req = request.get(version + '/customers/uberorg/sites/ubersite/configs');

            agent.attachCookies(req);
            req
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    total_configs = res.body.length;
                    done();
                });
        });


        it('should create a video node (N2flcnqCONFIG) using credentials', function (done) {
            // this.timeout(1e5);

            var data = { "csvNodeList": "nodeid,name,model,orgid,siteid\nN2flcnqCONFIG,N2flcnqCONFIG,falcon-q,uberorg,ubersite" },
                req = request.put(version + '/nodes');

            agent.attachCookies(req);
            req.send(data)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);

                    var result = res.body;

                    delete result.time_zone;

                    //result.should.eql(data);
                    done();
                });
        });

        it('should create a config using credentials', function (done) {
            var data = JSON.parse(JSON.stringify(default_falcon_q_config)),
                req = request.post(version + '/customers/uberorg/sites/ubersite/configs');

            agent.attachCookies(req);
            req.send(data)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);

                    var result = res.body;
                    configid = result.configid;

                    delete result.configid;
                    delete result.nodes;

                    //result.should.eql(data);
                    done();
                });
        });

        it('should access config with login credentials', function (done) {
            var req = request.get(version + '/customers/uberorg/sites/ubersite/configs');

            agent.attachCookies(req);
            req
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    res.body.length.should.eql(total_configs + 1);
                    done();
                });
        });

        it('should apply config to video node', function (done) {
            var req = request.put(version + '/customers/uberorg/sites/ubersite/nodes/' + nodeid + '/configs/' + configid + '/apply');

            agent.attachCookies(req);
            req.send({})
                .set('X-CSRF-Token', csrfToken)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);

                    var result = res.body;

                    should.exist(result.nodes[0]);
                    result.nodes[0].nodeid.should.eql(nodeid);
                    done();
                });
        });

        it('should fail to apply config to v3 node', function (done) {
            var data = {
                nodeList: ['N3CONFIG']
            },
                req = request.post(version + '/customers/uberorg/sites/ubersite/configs/' + configid + '/apply/nodes');

            agent.attachCookies(req);
            req.send(data)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(400)
                .end(function (err, res) {
                    done();
                });
        });

        it('should delete config ', function (done) {
            var uri = version + '/customers/uberorg/sites/ubersite/configs'
            var delete_req = request.delete(uri + '/' + configid);

            agent.attachCookies(delete_req);
            delete_req
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect(204)
                .end(function (err, res) {
                    should.not.exist(err);

                    var get_req = request.get(uri + '/' + configid);

                    agent.attachCookies(get_req);
                    get_req.set('Accept', 'application/json')
                        .expect('Content-Type', /json/)
                        .expect(404)
                        .end(function (err, res) {
                            done();
                        });
                });
        });

        it('should access config with login credentials', function (done) {
            var req = request.get(version + '/customers/uberorg/sites/ubersite/configs');

            agent.attachCookies(req);
            req
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    res.body.length.should.eql(total_configs);
                    done();
                });
        });
    });

    describe('ApplyConfigToGroup /customers/{orgid}/sites/{siteid}/configs/{configid}/apply/groups/{groupid}', function () {

        it('should create a video config using credentials', function (done) {
            var data = JSON.parse(JSON.stringify(default_falcon_q_config)),
                req = request.post(version + '/customers/uberorg/sites/ubersite/configs');

            agent.attachCookies(req);
            req.send(data)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    grpVideoConfigId = res.body.configid;
                    should.not.exist(err);
                    done();
                });
        });

        it('should create a core config using credentials', function (done) {
            var data = JSON.parse(JSON.stringify(default_unode_v4)),
                req = request.post(version + '/customers/uberorg/sites/ubersite/configs');

            agent.attachCookies(req);
            req.send(data)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);

                    var result = res.body;
                    grpCoreConfigId = result.configid;

                    delete result.configid;
                    delete result.nodes;

                    result.should.eql(data);
                    console.log("Core Config ID: ", grpCoreConfigId);
                    done();
                });
        });

        it("should create a group using credentials", function (done) {
            var data = {
                nodeList: [],
                name: "Mixed group",
                type: "organizational",
                description: "My mixed group"
            };
            var req = request.post(version + '/customers/uberorg/sites/ubersite/groups');
            agent.attachCookies(req);
            req.send(data)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    var result = res.body;
                    grpid = result.groupid;
                    delete result.groupid;
                    delete result.pdprofiles;
                    delete result.dhprofiles;
                    delete result.schedules;
                    result.should.eql(data);
                    console.log("Group ID: ", grpid);
                    done();
                });
        });

        it('should create core nodes (N5CONFIG & N6CONFIG) using credentials', function (done) {
            var data = { "csvNodeList": "nodeid,name,model,orgid,siteid,latitude,longitude\nN5CONFIG,N5CONFIG,unode-v2,uberorg,ubersite,37.32,-121.94\nN6CONFIG,N6CONFIG,unode-v2,uberorg,ubersite,37.32,-121.94" },
                req = request.put(version + '/nodes');

            agent.attachCookies(req);
            req.send(data)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    var result = res.body;
                    delete result.time_zone;
                    done();
                });
        });

        it('should create a video node (N3flcnqCONFIG) using credentials', function (done) {
            var data = { "csvNodeList": "nodeid,name,model,orgid,siteid\nN3flcnqCONFIG,N3flcnqCONFIG,falcon-q,uberorg,ubersite" },
                req = request.put(version + '/nodes');

            agent.attachCookies(req);
            req.send(data)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);

                    var result = res.body;

                    delete result.time_zone;

                    //result.should.eql(data);
                    done();
                });
        });

        it("should add a core node to group using credentials", function (done) {
            var req = request.post(version + '/customers/uberorg/sites/ubersite/groups/' + grpid + '/add/N5CONFIG');
            agent.attachCookies(req);
            req.send()
                .set('Content-Type', 'application/json')
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect(204)
                .end(function (err, res) {
                    should.not.exist(err);
                    done();
                });
        });

        it("should add a core node to group using credentials", function (done) {
            var req = request.post(version + "/customers/uberorg/sites/ubersite/groups/" + grpid + "/add/N6CONFIG");
            agent.attachCookies(req);
            req.send()
                .set('Content-Type', 'application/json')
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect(204)
                .end(function (err, res) {
                    should.not.exist(err);
                    done();
                });
        });

        it("should add a video node to group using credentials", function (done) {
            var req = request.post(version + "/customers/uberorg/sites/ubersite/groups/" + grpid + "/add/N3flcnqCONFIG");
            agent.attachCookies(req);
            req.send()
                .set('Content-Type', 'application/json')
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect(204)
                .end(function (err, res) {
                    should.not.exist(err);
                    done();
                });
        });

        it('should fail apply incorrect configid to correct groupid', function (done) {
            var req = request.post(version + '/customers/uberorg/sites/ubersite/configs/XXXXX' + '/apply/groups/' + grpid);

            agent.attachCookies(req);
            req.send({})
                .set('X-CSRF-Token', csrfToken)
                .expect(404)
                .end(function (err, res) {
                    should.not.exist(err);
                    done();
                });
        });

        it('should apply video config to the group', function (done) {
            var req = request.post(version + '/customers/uberorg/sites/ubersite/configs/' + grpVideoConfigId + '/apply/groups/' + grpid);        
            agent.attachCookies(req);
            req.send({})
                .set('X-CSRF-Token', csrfToken)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    var result = res.body;
                    should.exist(result.nodes[0]);
                    done();
                });
        });

        it('should apply core config to the group', function (done) {
            var req = request.post(version + '/customers/uberorg/sites/ubersite/configs/' + grpCoreConfigId + '/apply/groups/' + grpid);

            agent.attachCookies(req);
            req.send({})
                .set('X-CSRF-Token', csrfToken)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    var result = res.body;
                    should.exist(result.nodes[0]);
                    done();
                });
        });
    });

    describe('ApplyConfigToSite /customers/{orgid}/sites/{siteid}/configs/{configid}/apply/site', function () {

        it('should create a video config using credentials', function (done) {
            var data = JSON.parse(JSON.stringify(default_falcon_q_config)),
                req = request.post(version + '/customers/uberorg/sites/ubersite/configs');

            agent.attachCookies(req);
            req.send(data)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    siteVideoConfigId = res.body.configid;
                    should.not.exist(err);
                    done();
                });
        });

        it('should create a core config using credentials', function (done) {
            var data = JSON.parse(JSON.stringify(default_unode_v4)),
                req = request.post(version + '/customers/uberorg/sites/ubersite/configs');

            agent.attachCookies(req);
            req.send(data)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);

                    var result = res.body;
                    siteCoreConfigId = result.configid;

                    delete result.configid;
                    delete result.nodes;

                    result.should.eql(data);
                    console.log("Core Config ID: ", siteCoreConfigId);
                    done();
                });
        });

        it('should fail apply video config to incorrect site', function (done) {
            var req = request.post(version + '/customers/uberorg/sites/XXXXX/configs/' + siteVideoConfigId + '/apply/site');
            agent.attachCookies(req);
            req.send({})
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(403)
                .end(function (err, res) {
                    should.not.exist(err);
                    done();
                });
        });

        it('should fail apply incorrect configid to site', function (done) {
            var req = request.post(version + '/customers/uberorg/sites/ubersite/configs/ZZZZZ/apply/site');
            agent.attachCookies(req);
            req.send({})
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(404)
                .end(function (err, res) {
                    should.not.exist(err);
                    done();
                });
        });

        it('should apply video config to correct site', function (done) {
            var req = request.post(version + '/customers/uberorg/sites/ubersite/configs/' + siteVideoConfigId + '/apply/site');
            agent.attachCookies(req);
            req.send({})
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    var result = res.body;
                    should.exist(result.config);
                    should.exist(result.nodes);
                    done();
                });
        });

        it('should apply core config to correct site', function (done) {
            var req = request.post(version + '/customers/uberorg/sites/ubersite/configs/' + siteCoreConfigId + '/apply/site');
            agent.attachCookies(req);
            req.send({})
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    var result = res.body;
                    should.exist(result.config);
                    should.exist(result.nodes);
                    done();
                });
        });

    });

    describe('Cleanup Configs and Nodes ', function () {

        it('should clean up nodes after', function (done) {
            var nodeids = ['N3CONFIG', 'N4CONFIG', 'N5CONFIG', 'N6CONFIG', 'N2flcnqCONFIG', 'N3flcnqCONFIG'];
            var nodeid = null;
            var dirty = nodeids.length;
            while (nodeid = nodeids.shift()) {
                var uri = version + '/customers/uberorg/sites/ubersite/nodes';
                var delete_req = request.delete(uri + "/" + nodeid);
                agent.attachCookies(delete_req);
                delete_req
                    .set('Accept', 'application/json')
                    .set('X-CSRF-Token', csrfToken)
                    .expect(204)
                    .end(function (err, res) {
                        should.not.exist(err);

                        if (!(--dirty))
                            done();
                    });
            }
        });

        it('should delete configs after', function (done) {
            var configIds = [grpCoreConfigId, grpVideoConfigId, siteCoreConfigId, siteVideoConfigId];
            var configId = null;
            var dirty = configIds.length;
            while (configId = configIds.shift()) {
                var uri = version + '/customers/uberorg/sites/ubersite/configs';
                if (configId) {
                    console.log('Deleted Config:', configId);
                    var delete_req = request.delete(uri + '/' + configId);

                    agent.attachCookies(delete_req);
                    delete_req
                        .set('Accept', 'application/json')
                        .set('X-CSRF-Token', csrfToken)                        
                        .end(function (err, res) {                            
                            if (!(--dirty))
                                done();
                        });
                }
            }
        });
        it('should delete group after', function (done) {
            var uri = version + "/customers/uberorg/sites/ubersite/groups";
            var delete_req = request.delete(uri + "/" + grpid);
            agent.attachCookies(delete_req);
            delete_req.set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect(200)
                .expect('Content-Type', /json/)
                .end(function (err, res) {
                    should.not.exist(err);
                    console.log('Delete Group:', grpid);
                    done();
                });
        });
    });
});
