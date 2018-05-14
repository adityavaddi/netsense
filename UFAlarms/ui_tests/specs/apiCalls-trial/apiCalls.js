 import {config as config} from '../../config';
 import {allCustomers as allCustomers} from '../../variables';
 // var request = require('request');

 var DataUtilCalls = {

     getAllData :function(entity, customerCreated){
         browser.manage().getCookies().then(function(cookie) {
             for(var i in cookie){
                 if(cookie[i].name === 'sess'){
                     var sessionCookie = cookie[i].value;
                 }
             }
             var opts = {
                 method: 'GET',
                 uri: DataUtilCalls.getUrl(entity),
                 contentType: "application/json",
                 headers: {
                     'Cookie': 'sess='+ sessionCookie
                 },
                 withCredentials: true,
             };

             var request = require('request');
             request(opts, function (err, resp, body) {
                 var data = resp.body;
                 var resultJson = JSON.parse(body);

                 if(entity === "customers"){
                     for(var i = 0; i < resultJson.length; i++ ){
                         if(resultJson[i].name === customerCreated.name){
                             allCustomers.orgid = resultJson[i].orgid;
                         }
                     }
                 } else{
                     for(var i = 0; i < resultJson.length; i++ ){
                         if(resultJson[i].name === customerCreated.name){
                             allCustomers.siteid = resultJson[i].siteid;
                         }
                     }
                 }
             });

         });

     },

     postData:function(entity, entityData){

         browser.manage().getCookies().then(function(cookie) {
             for(var i in cookie){
                 if(cookie[i].name === 'sess'){
                     var sessionCookie = cookie[i].value;
                 }
             }

             var opts = {
                 method: 'POST',
                 uri: DataUtilCalls.getUrl(entity),
                 json:true,
                 body: entityData,
                 headers: {
                     'Cookie': 'sess='+sessionCookie,
                     'Content-Type':'application/json'
                 },
                 withCredentials: true,
             };

             var request = require('request');
             request(opts, function (err, resp, body) {
                 // Just to know if the node was added successfully while running the test case on the terminal
                 console.log(" Node Created Successfully");
             });

         });
     },

     getUrl: function(entity){
         switch(entity){
             case 'customers':
                 return config.baseUrl+'/v3.0/' + 'customers';
                 break;
             case 'sites':
                 return config.baseUrl+'/v3.0/' + 'customers/' + allCustomers.orgid + '/sites';
                 break;
             case 'nodes':
                 return config.baseUrl+'/v3.0/' + 'customers/' + allCustomers.orgid + '/sites/'+ allCustomers.siteid +'/nodes';
                 break;
         }
     }

 };

 module.exports  = DataUtilCalls;


