var crypto = require('crypto');
var querystring = require('querystring')

function nonce(len) {
   var text = "";
   var possible = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";

   for (var i = 0; i < len; i++)
   text += possible.charAt(Math.floor(Math.random() * possible.length));

   return text;
}

function forceUnicodeEncoding(string) {
   return decodeURIComponent(encodeURIComponent(string));
}

function created_signed_embed_url(options) {
   // looker options
   var secret = options.secret;
   var host = options.host;

   // user options
   var json_external_user_id = JSON.stringify(options.external_user_id);
   var json_first_name = JSON.stringify(options.first_name);
   var json_last_name = JSON.stringify(options.last_name);
   var json_permissions = JSON.stringify(options.permissions);
   var json_models = JSON.stringify(options.models);
   var json_group_ids = JSON.stringify(options.group_ids);
   var json_external_group_id = JSON.stringify(options.external_group_id || "");
   var json_user_attributes = JSON.stringify(options.user_attributes || {});
   var json_access_filters = JSON.stringify(options.access_filters);

   // url/session specific options
   var embed_path = '/login/embed/' + encodeURIComponent(options.embed_url);
   var json_session_length = JSON.stringify(options.session_length);
   var json_force_logout_login = JSON.stringify(options.force_logout_login);

   // computed options
   var json_time = JSON.stringify(Math.floor((new Date()).getTime() / 1000));
   var json_nonce = JSON.stringify(nonce(16));

   // compute signature
   var string_to_sign = "";
   string_to_sign += host + "\n";
   string_to_sign += embed_path + "\n";
   string_to_sign += json_nonce + "\n";
   string_to_sign += json_time + "\n";
   string_to_sign += json_session_length + "\n";
   string_to_sign += json_external_user_id + "\n";
   string_to_sign += json_permissions + "\n";
   string_to_sign += json_models + "\n";
   string_to_sign += json_group_ids + "\n";
   string_to_sign += json_external_group_id + "\n";
   string_to_sign += json_user_attributes + "\n";
   string_to_sign += json_access_filters;

   var signature = crypto.createHmac('sha1', secret).update(forceUnicodeEncoding(string_to_sign)).digest('base64').trim();

   // construct query string
   var query_params = {
       nonce: json_nonce,
       time: json_time,
       session_length: json_session_length,
       external_user_id: json_external_user_id,
       permissions: json_permissions,
       models: json_models,
       access_filters: json_access_filters,
       first_name: json_first_name,
       last_name: json_last_name,
       group_ids: json_group_ids,
       external_group_id: json_external_group_id,
       user_attributes: json_user_attributes,
       force_logout_login: json_force_logout_login,
       signature: signature
   };

   var query_string = querystring.stringify(query_params);

   return host + embed_path + '?' + query_string;
}


var  sample = (site)=> {
   var one_day = 1440 * 60;
   console.log("parking looker platform",process.env.env_suffix,process.env.looker_production,process.env.looker_url);

   var hostname;
   var hostnameDetails;
   var secretkeyDetails;
   var platformname;
   var platformDetails;
   var dashboardDetails;
   
   platformname = process.env.env_suffix;

   if(typeof platformname != "undefined"){
       platformDetails = 'dwh_' + process.env.env_suffix;
   }
   else{
       platformDetails = 'dwh_scale2';
   }

   hostname = process.env.looker_url;
   if(typeof hostname != "undefined"){
       hostnameDetails = hostname;
   }
   else{
       hostnameDetails = 'analytics-dev.sensity.com:9999';
   }

   var lookerproduction = process.env.looker_production;
   if(typeof lookerproduction != "undefined"){
       lookerproduction = lookerproduction.toLowerCase();
   }
   
   if(lookerproduction == "true"){
       secretkeyDetails = '74ae41f685560ae6fc4f099bcc061d500c6f6dd77f2c29592646e8410a276489';
       dashboardDetails = "/embed/dashboards/191?siteid=" + site;
   }   

   else if(lookerproduction == "false"){
       secretkeyDetails = 'e5cd8b70c442b247e9d564f3e12d2de79379796a40573b350f0ac2f3e3ad720e';
       dashboardDetails = "/embed/dashboards/182?siteid=" + site;
   }

   else{
       secretkeyDetails = '79bb8a8d851b2f55d885a4a70767e8b74f90f477ec9963ecbca4334843641ceb';
       dashboardDetails = "/embed/dashboards/182?siteid=" + site;
   }

   var url_data = {
       host: hostnameDetails,
       secret: secretkeyDetails,
       external_user_id: '57',
       first_name: 'Embed Steve',
       last_name: 'Krouse',
       permissions: ['see_user_dashboards', 'see_lookml_dashboards', 'access_data', 'see_looks'],
       group_ids: [3],
       models: ['parkingModel'],
       access_filters: {
           fake_model: {
               id: 1
           }
       },
       user_attributes: {"platform": platformDetails},
       session_length: one_day,
       embed_url: dashboardDetails,
       force_logout_login: true,
       user_timezone: "UTC"
   };

   var url = created_signed_embed_url(url_data);
   return "https://" + url;
}

exports.sample = sample;

