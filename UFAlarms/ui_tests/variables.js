import { config } from './config';
var randomName = require('./util/random.js');

var date = new Date();

console.log("Timestamp", date);
let allCustomers = {};

let variable = {
    "baseUrl": config.baseUrl,
    schedule: {
        "name": 'someScheduleName',
        "desc": 'someScheduleDescription'
    },
    pdInfo: {
        "name": "someName",
        "desc": "someDesc",
        "minLevel": "",
        "maxLevel": "",
        "beginTime": "",
        "endTime": "",
        "detectionDuration": ""
    },
    "notification": {
        "weekdays": "Monday",
        "name": "Severity",
        "description": "description12",
        "hold_off": "13",
        "resend_interval": "1000",
        "msg": "msg",
        "additionalEmails": "sensity@gmail.com",
        "active": "Yes",
    },
    "audits": {
        "active": "createGroup",
    },
};

let nodeVariable1 = {
    "name": "N01232e07"+ + new Date(),
    "nodeid": "N01232e07" + + new Date(),
    "latitude": "62.325944790869585",
    "longitude": "-124.94748993526446",
    "ip": "192.168.1.1",
    "model": "unode-v2",
    "building": "2",
    "level": "1",
    "meshId": "Mxeralux1",
    "note": "Node Note",
    "baseStation": "Mac address",
    "publicKey": "public key",
    "signature": "signature",
    "remoteNetwork": "XeraL",
    "bssid": "bssid",
    "configToken": "token",
    "softwareVersion": "e16b568",
    "mfgDate": "2014-03-06T22:15:12.074Z",
    "circuit": "1"
};

let nodeVariable2 = {
    "name": "N01232e89" + new Date(),
    "nodeid": "N01232e89"+ new Date(),
    "latitude": "32.877144",
    "longitude": "-96.954853",
    "ip": "192.168.1.1",
    "model": "unode-v3",
    "building": "2",
    "level": "1",
    "meshId": "Mxeralux1",
    "note": "Node Note",
    "baseStation": "Mac address",
    "publicKey": "public key",
    "signature": "signature",
    "remoteNetwork": "XeraL",
    "bssid": "bssid",
    "configToken": "token",
    "softwareVersion": "e16b568",
    "mfgDate": "2014-03-06T22:15:12.074Z",
    "circuit": "1"
};


// Config Varaibles  - To create Configs
let configModelOptions = {
    "default_unode-v2": "Core Node",
    "default_unode-v3": "Internal Core Node",
    "default_unode-v4": "Core Node EX Wifi",
    "default_unode-v5": "Core Node EX Cellular",
    "default_unode-v6": "Core Node EX LTE"

};
let configVariable = {
    "networkXPasskey": 40,
    "networkYPasskey": 50
};
let configModelTypes = {
    "type2":{
      name: "Core Node",
      value:"default_unode-v2"
    },
    "type3":{
        name: "Internal Core Node",
        value:"default_unode-v3"
    },
    "type4":{
        name: "Core Node EX Wifi",
        value:"default_unode-v4"
    },
    "type5":{
        name: "Core Node EX Cellular",
        value:"default_unode-v5"
    },
    "type6":{
        name: "Core Node EX LTE",
        value:"default_unode-v6"
    }
};


let mockCustomerParkingGroup = "Greatmall";

// let mockCustomerConfig = "Amin customer";
// Customer to pick to run test cases related to Proximity dimming profile
let mockCustomerPd = "falcon";
let mockCustomer = "Sensity Systems";
let mockCustomerConfig = "Swapnil Test Customer";
// qastage 3
// let mockCustomerConfig = "Swapnil Partner Customer";

let mockPartner = "Ashwini Partner Customer";
let trafficConfigData = {
    "customerName": "Sarah customer"
};

let fixtureVariable = {
    "name": "Fixture DemoLine " + date,
    "description": " New",
    "manufacturer": "raytheon ",
    "manufacturersku": "as90edjweu8789",
    "fixtureType": "Bollard",
    "PowerDraw": "50",
    "nemasocket": "Yes",
    "MaxPower0": "2",
    "MaxPower10": "30",
    "MaxPower50": "34",
    "MaxPower100": "34",
    "MinPower0": "2",
    "MinPower10": "45",
    "MinPower50": "34",
    "MinPower100": "22",
    "MinimumLightLevelForFailureDetection": "10",
    "BallastCost": "23",
    "BulbCost": "5",
    "LegacyPowerDraw": "45",
    "DailyOperatingTime": "34",
};

let commisionVariable = {
    "siteToBeSelected": "SampleSite",
};

let endUserAdminVariable = {
    "name": "DemoLine 01" + date,
    "email": "demoLine01@sensity.com",
    "title": "end_user_admin",
    "phone": "(808)080-8292",
    "roles": "end_user_admin",
};

let sites = {
    siteVariable: {
        "name": "DemoLine" + date,
        "street1": " 700 Hidden Ridge",
        "street2": "0",
        "contact_name": "Smith Jones",
        "contact_phone": "(978)456-3324",
        "contact_email": "verizon@one.verizon.com",
        "city": "Irving",
        "state": "Texas",
        "postal_code": "75038",
        "country": "United States",
        "latitude": "32.876945",
        "longitude": "-96.953288"
    }, newSiteVariable: {
        "name": "DemoLine 01",
        "street1": "9830 Reseda Blvd",
        "street2": "Block A",
        "contact_name": "Smith Jones",
        "contact_phone": "(978)456-3324",
        "contact_email": "verizon@one.verizon.com",
        "city": "Northridge",
        "state": "California",
        "postal_code": "91324",
        "country": "United States",
        "latitude": "78.39",
        "longitude": "90.28",
    }
};

let groups = {
    "name": "DemoLine 01" + date,
    "description": "New  Group Added",
    "xschedule": "Default schedule",
    "dhprofile": "No DH Profiles defined",
    "pdprofile": "No PD Profiles defined"
};

//Profile Details to fill the form to create Pd profile
let pdVariable = {
    "name": "Sample-Pd-Name " + date,
    "desc": "Sample-Pd-Description",
    "minLevel": "",
    "maxLevel": "",
    "beginTime": "",
    "endTime": "",
    "detectionDuration": ""
};


let endUserVariable = {
    "name":"End User Admin "+ randomName,
    "email":"endUserAdmin"+randomName+"@verizon.com",
    "title":"End User Admin",
    "phone":"1232341234",
    "role":"end_user_admin"
};

let parkingOwnerVariable = {
    "name":"Parking Owner"+ randomName,
    "email":"parkingowner"+randomName+"@verizon.com",
    "title":"Parking Owner",
    "phone":"9098089909",
    "role":"parking_owner_admin"
};

let partnerAdminVariable = {
    "name":"Partner Admin " + randomName,
    "email":"partnerAdmin"+randomName+"@verizon.com",
    "title":"Partner Admin",
    "phone":"1232341234",
    "role":"partner_admin"
}

let userWithPartnerApi = {
    "name":"New Partner API "+ randomName,
    "email":"testtrial"+randomName+"@verizon.com",
    "title":"Partner API",
    "phone":"1232341234",
    "role":"partner_api"
};

let userWithEndUserApi = {
    "name":"New EndUserAPI " + randomName,
    "email":"End User API "+randomName+"@verizon.com",
    "title":"New EndUserAPI ",
    "phone":"1232341234",
    "role":"end_user_api"
};

let parkingGroupsVariable = {
    "name":"Test DemoLine ",
    "description":"Testing the functionality",
    "parkingType":"Car",
    "policy":"Standard parking",
};

let mockCustomerUserManagement ="Swapnil Test Customer";

let nodename = "JSV3_67";

let daylightHarvestingVariable= {
    "name":" DH Profile - " + randomName,
    "highLux":"43",
    "lowLux":"11",
    "lowDriver":"1",
    "minDriver":"2",
};

  let singleCustomerTestExecution = "Verizon UI Test Execution";

export {
    variable,
    randomName,
    allCustomers,
    nodeVariable1,
    nodeVariable2,
    configVariable,
    fixtureVariable,
    endUserAdminVariable,
    parkingOwnerVariable,
    partnerAdminVariable,
    sites,
    groups,
    pdVariable,
    endUserVariable,
    mockCustomerUserManagement,
    mockCustomerConfig,
    mockCustomerPd,
    mockCustomerParkingGroup,
    parkingGroupsVariable,
    trafficConfigData,
    mockCustomer,
    userWithPartnerApi,
    mockPartner,
    userWithEndUserApi,
    commisionVariable,
    nodename,
    configModelOptions,
    configModelTypes,
    singleCustomerTestExecution,
    daylightHarvestingVariable
}
