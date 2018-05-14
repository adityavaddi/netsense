/**
 * Created by Senthil on 10/04/17.
 */

const server_url = process.env.stack_url;
const should = require('should');
const request = require('supertest')(server_url);
const superagent = require('superagent');
const agent = superagent.agent();
const uuid = require('uuid');
const version = '/v3.0';

const helpers = require('./../../utils/helpers');
let csrfToken = null;

const uri = version + '/customers/uberorg/sites/ubersite/parkingspaces/spaceattributes';
const parkingSpotId1 = "spot1-" + uuid.v4(),
    parkingSpotId2 = "spot2-" + uuid.v4(),
    parkingSpotId3 = "spot3-" + uuid.v4();

function getParkingMetadataObj(name, spotId) {
    return {
        "parkingspaceid": spotId,
        "name": name,
        "typeOfVehicle": [
            "car"
        ],
        "reservation": false,
        "handicap": false,
        "monitoringSensorid": "monitoringSensorid1",
        "businessUse": "loading",
        "howMetered": "not-metered",
        "active": true,
        "PPV": false,
        "areaType": [
            "school"
        ],
        "meterid": "meterid",
        "paystationid": "paystationid",
        "level": "LL",
        "geoCoordinates": {
            "p1": {
                "latitude": 0,
                "longitude": 0
            },
            "p2": {
                "latitude": 0,
                "longitude": 0
            },
            "p3": {
                "latitude": 0,
                "longitude": 0
            },
            "p4": {
                "latitude": 0,
                "longitude": 0
            }
        },
        "parkingSpaceType": "curb-side"
    }
}

describe('controllers', function () {
    describe('parkingmetadata', function () {
        describe('/customers/{orgid}/sites/{siteid}/parkingspaces/spaceattributes', function () {

            it('should fail without login credentials', function (done) {
                request
                    .get(version + '/login')
                    .set('Accept', 'application/json')
                    .expect('Content-Type', /json/)
                    .expect(404)
                    .end(function (err, res) {
                        should.not.exist(err);
                        res.body.should.eql({
                            error: true,
                            message: 'User not found',
                            "status": 404
                        });
                        done();
                    });
            });

            it('should sign in with correct credentials', function (done) {
                const data = { email: "uberuser@sensity.com", password: "ubeR$23" };
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

            it("should fail to create a parking spaces for invalid siteid", function (done) {
                let req = request.put(version + '/customers/uberorg/sites/XXXXX/parkingspaces/spaceattributes');
                agent.attachCookies(req);
                const payloadData = [getParkingMetadataObj("New Uber Parking spot1", parkingSpotId1)];
                req.send(payloadData)
                    .set('Accept', 'application/json')
                    .set('X-CSRF-Token', csrfToken)
                    .expect('Content-Type', /json/)
                    .expect(404)
                    .end(function (err, res) {
                        should.not.exist(err);
                        done();
                    });
            });


            it("should fail to create a parking spot for invalid payload", function (done) {
                let req = request.put(uri);
                agent.attachCookies(req);
                const invalidParkingMetadataPayload = [{
                    name: "New Uber Parking spot"
                }];
                req.send(invalidParkingMetadataPayload)
                    .set('Accept', 'application/json')
                    .set('X-CSRF-Token', csrfToken)
                    .expect('Content-Type', /json/)
                    .expect(400)
                    .end(function (err, res) {
                        should.not.exist(err);
                        res.body.message.should.eql('Validation errors');
                        done();
                    });
            });

            it("should create a parking spot using credentials", function (done) {
                let req = request.put(uri);
                agent.attachCookies(req);
                const payloadData = [getParkingMetadataObj("New Uber Parking spot1", parkingSpotId1)];
                req.send(payloadData)
                    .set('Accept', 'application/json')
                    .set('X-CSRF-Token', csrfToken)
                    .expect('Content-Type', /json/)
                    .expect(200)
                    .end(function (err, res) {
                        should.not.exist(err);
                        console.log('Created Parking Spot1:', parkingSpotId1);
                        res.body.should.eql({
                            "success": true
                        });
                        done();
                    });
            });

            it("should create two parking spots using credentials", function (done) {
                let req = request.put(uri);
                agent.attachCookies(req);
                const payloadData = [getParkingMetadataObj("New Uber Parking spot2", parkingSpotId2), getParkingMetadataObj("New Uber Parking spot3", parkingSpotId3)];
                req.send(payloadData)
                    .set('Accept', 'application/json')
                    .set('X-CSRF-Token', csrfToken)
                    .expect('Content-Type', /json/)
                    .expect(200)
                    .end(function (err, res) {
                        should.not.exist(err);
                        console.log('Created Parking Spot2:', parkingSpotId2);
                        console.log('Created Parking Spot3:', parkingSpotId3);
                        res.body.should.eql({
                            "success": true
                        });
                        done();
                    });
            });

            it('should fail to get parking spot details for empty parking spot ids', function (done) {
                let req = request.post(uri);
                agent.attachCookies(req);
                const payloadData = {
                    parkingspaceids: [""]
                };
                req.send(payloadData)
                    .set('Accept', 'application/json')
                    .set('X-CSRF-Token', csrfToken)
                    .expect('Content-Type', /json/)
                    .expect(400)
                    .end(function (err, res) {
                        should.not.exist(err);
                        res.body.message.should.eql('Request contains invalid parking space ids');
                        done();
                    });
            });

            it('should fail to get parking spot details for invalid parking spot id', function (done) {
                let req = request.post(uri);
                agent.attachCookies(req);
                const payloadData = {
                    parkingspaceids: ['ZZZZZ']
                };
                req.send(payloadData)
                    .set('Accept', 'application/json')
                    .set('X-CSRF-Token', csrfToken)
                    .expect('Content-Type', /json/)
                    .expect(200)
                    .end(function (err, res) {
                        should.not.exist(err);
                        res.body.should.eql([]);
                        done();
                    });
            });

            it('should get parking spot details for valid parking spot id', function (done) {
                let req = request.post(uri);
                agent.attachCookies(req);
                const payloadData = {
                    parkingspaceids: [parkingSpotId1]
                };
                req.send(payloadData)
                    .set('Accept', 'application/json')
                    .set('X-CSRF-Token', csrfToken)
                    .expect('Content-Type', /json/)
                    .expect(200)
                    .end(function (err, res) {
                        should.not.exist(err);
                        const result = res.body[0];
                        delete result.createdOn;
                        delete result.lastUpdated;
                        result.should.eql(getParkingMetadataObj("New Uber Parking spot1", parkingSpotId1));
                        done();
                    });
            });

            it('should fail to update parking spot with invalid parking spot details', function (done) {
                let req = request.put(uri);
                agent.attachCookies(req);
                let payloadData = [];
                payloadData.push(getParkingMetadataObj("New Uber Parking spot1", parkingSpotId1));
                // Invalid typeOfVehicle
                payloadData[0].typeOfVehicle = [
                    "GGGGG"
                ];
                req.send(payloadData)
                    .set('Accept', 'application/json')
                    .set('X-CSRF-Token', csrfToken)
                    .expect('Content-Type', /json/)
                    .expect(404)
                    .end(function (err, res) {
                        should.not.exist(err);
                        done();
                    });
            });

            it('should update a parking spot detail with valid parking spot id', function (done) {
                let req = request.put(uri);
                agent.attachCookies(req);
                let payloadData = [];
                payloadData.push(getParkingMetadataObj("Updated Uber Parking spot1", parkingSpotId1));
                req.send(payloadData)
                    .set('Accept', 'application/json')
                    .set('X-CSRF-Token', csrfToken)
                    .expect('Content-Type', /json/)
                    .expect(200)
                    .end(function (err, res) {
                        should.not.exist(err);
                        console.log('Update Parking Spot1 %s with name Updated Uber Parking spot1', parkingSpotId3);
                        res.body.should.eql({
                            "success": true
                        });
                        done();
                    });
            });

            it('shoud fail to delete parking spot with invalid parking spot id', function (done) {
                let req = request.delete(uri);
                agent.attachCookies(req);
                const payloadData = {
                    parkingspaceids: ["CCCCC"]
                };
                req.send(payloadData)
                    .set('Accept', 'application/json')
                    .set('X-CSRF-Token', csrfToken)
                    .expect('Content-Type', /json/)
                    .expect(404)
                    .end(function (err, res) {
                        should.not.exist(err);
                        done();
                    });
            });

            it('shoud fail delete 1 parking spot and return error for invalid parking spot', function (done) {
                let req = request.delete(uri);
                agent.attachCookies(req);
                const payloadData = {
                    parkingspaceids: [parkingSpotId1, "DDDDD"]
                };
                req.send(payloadData)
                    .set('Accept', 'application/json')
                    .set('X-CSRF-Token', csrfToken)
                    .expect('Content-Type', /json/)
                    .expect(200)
                    .end(function (err, res) {
                        should.not.exist(err);
                        console.log('Deleted Parking Spot1:', parkingSpotId1);
                        console.log('Error', res.body);
                        res.body.should.eql({
                            "success": true,
                            "invalidParkingspaceids": [
                                "DDDDD"
                            ]
                        });
                        done();
                    });
            });

            it('shoud delete 2 parking spots with valid parking spot ids', function (done) {
                let req = request.delete(uri);
                agent.attachCookies(req);
                const payloadData = {
                    parkingspaceids: [parkingSpotId2, parkingSpotId3]
                };
                req.send(payloadData)
                    .set('Accept', 'application/json')
                    .set('X-CSRF-Token', csrfToken)
                    .expect('Content-Type', /json/)
                    .expect(200)
                    .end(function (err, res) {
                        should.not.exist(err);
                        console.log('Deleted Parking Spot2:', parkingSpotId2);
                        console.log('Deleted Parking Spot3:', parkingSpotId3);
                        res.body.should.eql({
                            "success": true
                        });
                        done();
                    });
            });
        });
    });
});