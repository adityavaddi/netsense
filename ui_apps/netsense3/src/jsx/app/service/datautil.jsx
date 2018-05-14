/**
 * Created by vaddiad on 3/15/17.
 */
import React from 'react';
import { State, Navigation } from 'react-router';
import { Link, withRouter } from 'react-router';
import classNames from 'classnames';

var DataUtil =
    {
        nodedata: [],
        ///////////////////To Get All/////////////////
        getAll: function (entity, ...callback) {
            $.ajax({
                url: DataUtil.getURL(entity, ''),
                data: '',
                xhrFields: {
                    withCredentials: true
                },
                method: 'GET',
                dataType: 'json',
                timeout: 15000
            }).done(function (data) {
                console.log(" ajax success: " + JSON.stringify(data));
                //console.log("callback...",callback[1]);
                if (!(callback[1] == undefined)) {
                    data.callback = callback[1];
                }
                callback[0](data);

            })
                .fail(function (jqXHR, status, error) {
                    //alert( "error" );
                    console.log("ajax failure (customers): " + status + " - " + error);
                    DataUtil.handleError(jqXHR);
                    //$("#loadingmsg").html("Cannot retrieve entity.  API reported error: " + error);
                })
                .always(function () {
                    //alert( "complete" );
                })

        },
        //////////////////To Get One////////////////////
        getOne: function (entity, callback) {
            $.ajax({
                url: DataUtil.getURL(entity, ''),
                headers: { 'X-Request-Id': Math.round(Math.random() * 100000000000) },
                data: '',
                xhrFields: {
                    withCredentials: true
                },
                method: 'GET',
                dataType: 'json',
                timeout: 10000
            }).done(function (data) {
                console.log(" ajax success: " + JSON.stringify(data));
                callback(data);

            })
                .fail(function (jqXHR, status, error) {
                    //alert( "error" );
                    console.log("ajax failure (customers): " + status + " - " + error);
                    DataUtil.handleError(jqXHR);
                    //$("#loadingmsg").html("Cannot retrieve entity.  API reported error: " + error);
                })
                .always(function () {
                    //alert( "complete" );
                })

        },
        /////////////To Add an Entity/////////////////
        addEntity: function (entity, entityData, callback, inputdata) {
            console.log("callback in addEntity", callback)
            $.ajax({
                "url": DataUtil.getURL(entity, entityData),
                "type": "POST",
                "data": JSON.stringify(entityData),
                "xhrFields": {
                    withCredentials: true
                },
                "dataType": "json",
                "timeout": 10000,
                "contentType": "application/json",
                "processData": false
            })
                .done(function (data) {
                    data.inputdata = inputdata;
                    callback(data);

                })
                .fail(function (jqXHR, status, error) {
                    //alert( "error" );
                    DataUtil.handleError(jqXHR);
                    noty({ type: "error", text: "Could not add entity." });
                })
                .always(function () {
                    //alert( "complete" );
                })
        },
        /////////////To Update an Entity/////////////
        updateEntity: function (entity, entityData, callback, inputdata) {
            $.ajax({
                "url": DataUtil.getURL(entity, entityData, inputdata),
                "type": "POST",
                "xhrFields": {
                    withCredentials: true
                },
                "data": ((entityData == "") ? "" : JSON.stringify(entityData)),
                "dataType": "json",
                "timeout": 10000,
                "contentType": "application/json",
                "processData": false
            })

                .done(function (data) {
                    console.log("idx in datautil update", inputdata)
                    callback(data, inputdata);

                })
                .fail(function (jqXHR, status, error) {
                    //alert( "error" );
                    DataUtil.handleError(jqXHR);
                    noty({ type: "error", text: "Could not update entity." });
                })
                .always(function () {
                    //alert( "complete" );
                })
        },
        //////////////To suspend Entity//////////////
        suspendEntity: function (entity, entityData, callback, idx) {
            $.ajax({
                "url": DataUtil.getURL(entity, entityData),
                "type": "PUT",
                "xhrFields": {
                    withCredentials: true
                },
                "data": JSON.stringify(entityData),
                "timeout": 10000,
                "dataType": "json",
                "contentType": "application/json",
                "processData": false
            })
                .done(function (data) {
                    callback(entityData, idx, data);

                })
                .fail(function (jqXHR, status, error) {
                    //alert( "error" );
                    DataUtil.handleError(jqXHR);
                    noty({ type: "error", text: "Could not suspend entity." });
                })
                .always(function () {
                    //alert( "complete" );
                })
        },
        ////////////To Delete Entity//////////
        deleteEntity: function (entity, entityData, callback, idx) {
            $.ajax({
                url: DataUtil.getURL(entity, entityData),
                "type": "DELETE",
                "xhrFields": {
                    withCredentials: true
                },
                "data": JSON.stringify(entityData),
                "dataType": "json",
                "timeout": 10000,
                "contentType": "application/json",
                "processData": false
            })
                .done(function (data) {
                    console.log("entity data", entityData);
                    callback(entityData, idx, data);

                })
                .fail(function (jqXHR, status, error) {
                    //alert( "error" );
                    DataUtil.handleError(jqXHR);
                    noty({ type: "error", text: "Could not delete entity." });
                })
                .always(function () {
                    //alert( "complete" );
                })
        },

        assignState: function (panelName, data, that, callback) {

            var res = {};
            res[panelName] = data.map(callback);
            return res;

            // that.setState(res)
        },
        ////////////To handle Error////////////
        handleError: function (jqXHR) {
            console.log("error type in handleError", jqXHR);
            switch (jqXHR.status) {

                case 400:
                    noty({ type: "error", text: "Validation: Missing or invalid data was received :" + jqXHR.responseText });
                    //$("#loadingmsg").html("Bad request: The request had bad syntax or was inherently impossible to be satisfied :" );
                    break;

                case 401:
                    noty({ type: "error", text: "Unauthorized: The parameter to this message gives a specification of authorization schemes which are acceptable :" + jqXHR.responseText });
                    //$("#loadingmsg").html("Unauthorized: The parameter to this message gives a specification of authorization schemes which are acceptable :" + jqXHR.responseText);
                    break;

                case 402:
                    noty({ type: "error", text: "PaymentRequired: The parameter to this message gives a specification of charging schemes acceptable :" + jqXHR.responseText });
                    //$("#loadingmsg").html("PaymentRequired: The parameter to this message gives a specification of charging schemes acceptable :" + jqXHR.responseText);
                    break;

                case 403:
                    // noty({ type: "error", text: "Authorization: Your login does not provide access to this data or command." });
                    $.ajax({
                        url: NSN.apiURL + 'login',
                        xhrFields: {
                            withCredentials: true
                        },
                        type: 'GET',
                        contentType: 'application/json',
                        success: function (data) {
                            console.log("login success: ");
                        },
                        error: function () {
                            alert("Your session has timed out. Please login in again.")
                            window.location = '/';

                        }
                    });
                    //$("#loadingmsg").html("Forbidden: The request is for something forbidden. Authorization will not help :" + jqXHR.responseText);
                    break;

                case 404:
                    noty({ type: "error", text: "Not found: The server has not found anything matching the URI given :" + jqXHR.responseText });
                    //$("#loadingmsg").html("Not found: The server has not found anything matching the URI given :" + jqXHR.responseText);
                    break;

                case 408:
                    noty({ type: "error", text: "Timeout:  The server took longer than the allowed time to complete this request." });
                    //$("#loadingmsg").html("Not found: The server has not found anything matching the URI given :" + jqXHR.responseText);
                    break;

                case 500:
                    noty({ type: "error", text: "Internal Error: The server encountered an unexpected condition which prevented it from fulfilling the request :" + jqXHR.responseText });
                    //$("#loadingmsg").html("Internal Error: The server encountered an unexpected condition which prevented it from fulfilling the request :" + jqXHR.responseText);
                    break;

                case 501:
                    noty({ type: "error", text: "Not implemented: The server does not support the facility required. :" + jqXHR.responseText });
                    //$("#loadingmsg").html("Not implemented: The server does not support the facility required. :" + jqXHR.responseText);
                    break;

                case 502:
                    noty({ type: "error", text: "Service temporarily overloaded: The server cannot process the request due to a high load (whether HTTP servicing or other requests). The implication is that this is a temporary condition which maybe alleviated at other times. :" + jqXHR.responseText });
                    //$("#loadingmsg").html("Service temporarily overloaded: The server cannot process the request due to a high load (whether HTTP servicing or other requests). The implication is that this is a temporary condition which maybe alleviated at other times. :" + jqXHR.responseText);
                    break;

                case 503:
                    noty({ type: "error", text: "Gateway timeout :" + jqXHR.responseText });
                    //$("#loadingmsg").html("Gateway timeout :" + jqXHR.responseText);
                    break;

                default:
                // jQuery sometimes throws false errors.  
                //    If an unknown error occurs, the user does not need to be notified
                //noty({ type: "error", text: "Unexpected error:  Refreshing the page may resolve the problem." });
                //$("#loadingmsg").html("Error occurred");

            }

        },


        getURL: function (entity, entityData, inputdata) {
            switch (entity) {
                case 'customers':
                    return NSN.apiURL + 'customers';
                    break;
                case 'sites':
                    return NSN.apiURL + 'customers/' + NSN.customerID + '/sites';
                    break;
                case 'nodes':
                    return NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/nodes';
                    break;
                case 'nodestatus':
                    return NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/node_status' +
                        '?t=' + (new Date()).getTime();
                    break;
                case 'site':
                    return NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID;
                    break;
                case 'configs':
                    return NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/configs';
                    break;
                case 'firmwares':
                    return NSN.apiURL + 'firmwares';
                    break;
                case 'firmwares/':
                    return NSN.apiURL + 'firmwares/' + entityData.firmwareid;
                    break;
                case 'update-firmwares':
                    return NSN.apiURL + 'firmwares/' + entityData.firmwareid;
                    break;
                case 'customers/':
                    return NSN.apiURL + 'customers/' + entityData.orgid;
                    break;
                case 'suspended-customers/':
                    return NSN.apiURL + 'suspended-customers/' + entityData.orgid;
                    break;
                case 'site-update':
                    return NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + entityData.siteid;
                    break;
                case 'users':
                    return NSN.apiURL + 'customers/' + NSN.customerID + '/users';
                    break;
                case 'audits':
                    return NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/audits/' + 'from/' + NSN.datemin + '/to/' + NSN.datemax;
                    break;
                case 'commissioningNodes':
                    return NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + '_nosite_' + '/nodes';
                    break;
                case 'commissioningNodestatus':
                    return NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + '_nosite_' + '/node_status';
                    break;
                case 'commissioningUpdate':
                    return NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + inputdata.siteID + '/nodes/' + inputdata.node.nodeid + '/assign';
                    break;
                case 'groups':
                    return NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/groups';
                    break;
                case 'otas':
                    return NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/ota_status';
                    break;
                case 'add-configs':
                    return NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/configs/';
                    break;
                case 'delete-configs':
                    return NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/configs/' + entityData.configid;
                    break;
                case 'energy-minnodes':
                    return NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/minnodes';
                    break;
                case 'fixtures':
                    return NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/fixtures';
                    break;
                case 'add-fixtures':
                    return NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/fixtures/';
                    break;
                case 'update-fixtures':
                    return NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/fixtures/' + entityData.fixtureid;
                    break;
                case 'set-fixture':
                    return NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/fixtures/' + entityData + '/assign/node/' + NSN.nodeID;
                    break;
                case 'delete-fixtures':
                    return NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/fixtures/' + entityData.fixtureid;
                    break;
                case 'schedules':
                    return NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/schedules';
                    break;
                case 'update-schedules':
                    return NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/schedules/' + NSN.scheduleID;
                    break;
                case 'delete-schedules':
                    return NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/schedules/' + entityData.scheduleid;
                    break;
                case 'etdhprofiles':
                    return NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/daylightharvesting';
                    break;
                case 'pdprofiles':
                    return NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/proximitydimming';
                    break;
                case 'add-notification':
                    return NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/notifications';
                    break;
                case 'update-notification':
                    return NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/notifications/' + entityData.notificationid;
                    break;
                case 'delete-notification':
                    return NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/notifications/' + entityData.notificationid;
                    break;
                case 'add-overlay':
                    return NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/overlays/';
                    break;
                case 'update-overlay':
                    return NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/overlays/' + entityData.overlayid;
                    break;
                case 'delete-overlay':
                    return NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/overlays/' + entityData.overlayid;
                    break;
                case 'proximitys':
                    return NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/proximitydimming';
                    break;
                case 'update-proximity':
                    return NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/proximitydimming/' + entityData.pdprofileid;
                    break;
                case 'delete-proximity':
                    return NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/proximitydimming/' + entityData.pdprofileid;
                    break;
                case 'suspendedusers':
                    return NSN.apiURL + 'customers/' + NSN.customerID + '/suspended-users';
                    break;
            }
        }

    };
module.exports = DataUtil;