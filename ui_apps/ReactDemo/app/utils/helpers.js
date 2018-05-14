var axios = require('axios');
var ui_api_host = location.protocol + "//" + location.hostname + ":10010";

function getSitelist(customerID){
  return axios.get(ui_api_host + '/customers/' + customerID + '/sites');
};

function getCustomerInfo(customerID){
  return axios.get(ui_api_host + '/customers/' + customerID);
};

function getCustomerlist(customerID){
  return axios.get(ui_api_host + '/customers');
};

var helpers = {
  getCustomerInfo: function(customerID){
    return axios.all([getSitelist(customerID), getCustomerInfo(customerID)])
      .then(function(arr){
        return {
          sites: arr[0].data,
          info: arr[1].data
        }
      });
  },
  getCustomerList: function(){
    return axios.get(ui_api_host + '/customers');
  },
  getNodelist: function(){
    return {"nodes":[
  {
    "ID":"N0123076y",
    "Hardware Model":"unode-v2",
    "Latitude":"37.32641132781336",
    "Longitude":"-121.94854657312135",
    "Local IP":"192.168.101.89"
  },
  {
    "ID":"N0123076c",
    "Hardware Model":"unode-v2",
    "Latitude":"37.32611962515193",
    "Longitude":"-121.94861916359514",
    "Local IP":"192.168.100.188"
  },
  {
    "ID":"N0123076e",
    "Hardware Model":"unode-v2",
    "Latitude":"37.325952193496924",
    "Longitude":"-121.94840133188791",
    "Local IP":"192.168.100.159"
  },
  {
    "ID":"N0123076f",
    "Hardware Model":"unode-v2",
    "Latitude":"37.32629922149461",
    "Longitude":"-121.94806169080726",
    "Local IP":"192.168.100.80"
  },
  {
    "ID":"N01230770",
    "Hardware Model":"unode-v2",
    "Latitude":"37.32645025283504",
    "Longitude":"-121.94882753459751",
    "Local IP":"192.168.102.40"
  },
  {
    "ID":"N01230771",
    "Hardware Model":"unode-v2",
    "Latitude":"37.32628708766094",
    "Longitude":"-121.94772246444245",
    "Local IP":"192.168.103.193"
  },
  {
    "ID":"N01230772",
    "Hardware Model":"unode-v2",
    "Latitude":"37.32595272671936",
    "Longitude":"-121.94812774656839",
    "Local IP":"192.168.100.237"
  },
  {
    "ID":"N01230773",
    "Hardware Model":"unode-v2",
    "Latitude":"37.32645073529117",
    "Longitude":"-121.9476356015922",
    "Local IP":"192.168.100.23"
  },
  {
    "ID":"N01230776",
    "Hardware Model":"unode-v2",
    "Latitude":"37.325875389908845",
    "Longitude":"-121.94839073974231",
    "Local IP":"192.168.102.62"
  },
  {
    "ID":"N01230778",
    "Hardware Model":"unode-v2",
    "Latitude":"37.32637133636518",
    "Longitude":"-121.94923925359944",
    "Local IP":"192.168.102.101"
  },
  {
    "ID":"N0123077c",
    "Hardware Model":"unode-v2",
    "Latitude":"37.3258114926448",
    "Longitude":"-121.94782935093826",
    "Local IP":"192.168.101.49"
  },
  {
    "ID":"N0123077d",
    "Hardware Model":"unode-v2",
    "Latitude":"37.32604914091605",
    "Longitude":"-121.94785516071312",
    "Local IP":"192.168.100.76"
  },
  {
    "ID":"N0123077e",
    "Hardware Model":"unode-v2",
    "Latitude":"37.32579167385587",
    "Longitude":"-121.94829954463574",
    "Local IP":"192.168.102.26"
  },
  {
    "ID":"N0123077f",
    "Hardware Model":"unode-v2",
    "Latitude":"37.3262828218923",
    "Longitude":"-121.94860759337644",
    "Local IP":"192.168.100.210"
  },
  {
    "ID":"N01230780",
    "Hardware Model":"unode-v2",
    "Latitude":"37.325956972979014",
    "Longitude":"-121.94762966293428",
    "Local IP":"192.168.100.30"
  },
  {
    "ID":"N01230782",
    "Hardware Model":"unode-v2",
    "Latitude":"37.32595217397738",
    "Longitude":"-121.94839543360803",
    "Local IP":"192.168.102.34"
  },
  {
    "ID":"N01230783",
    "Hardware Model":"unode-v2",
    "Latitude":"37.32612264097973",
    "Longitude":"-121.94822489237788",
    "Local IP":"192.168.100.98"
  },
  {
    "ID":"N01230785",
    "Hardware Model":"unode-v2",
    "Latitude":"37.32621575270065",
    "Longitude":"-121.94872180275911",
    "Local IP":"192.168.100.103"
  },
  {
    "ID":"N01230786",
    "Hardware Model":"unode-v2",
    "Latitude":"37.32579439070425",
    "Longitude":"-121.94883155784964",
    "Local IP":"192.168.101.59"
  },
  {
    "ID":"N01230787",
    "Hardware Model":"unode-v2",
    "Latitude":"37.326037489517006",
    "Longitude":"-121.94883445553774",
    "Local IP":"192.168.101.200"
  },
  {
    "ID":"N01230789",
    "Hardware Model":"unode-v2",
    "Latitude":"37.32587912246956",
    "Longitude":"-121.9476967181597",
    "Local IP":"192.168.101.138"
  },
  {
    "ID":"N0123078c",
    "Hardware Model":"unode-v2",
    "Latitude":"37.32629183589464",
    "Longitude":"-121.94936294419779",
    "Local IP":"192.168.100.89"
  },
  {
    "ID":"N0123078d",
    "Hardware Model":"unode-v2",
    "Latitude":"37.32621670256536",
    "Longitude":"-121.94769966566582",
    "Local IP":"192.168.100.140"
  },
  {
    "ID":"N0123078e",
    "Hardware Model":"unode-v2",
    "Latitude":"37.32604073957833",
    "Longitude":"-121.94840106328229",
    "Local IP":"192.168.100.181"
  },
  {
    "ID":"N0123078f",
    "Hardware Model":"unode-v2",
    "Latitude":"37.32571760648265",
    "Longitude":"-121.94848823507527",
    "Local IP":"192.168.102.92"
  },
  {
    "ID":"N01230790",
    "Hardware Model":"unode-v2",
    "Latitude":"37.32629770131455",
    "Longitude":"-121.94774011425955",
    "Local IP":"192.168.100.98"
  },
  {
    "ID":"N01230791",
    "Hardware Model":"unode-v2",
    "Latitude":"37.32637661782925",
    "Longitude":"-121.94861604529956",
    "Local IP":"192.168.101.160"
  },
  {
    "ID":"N01230792",
    "Hardware Model":"unode-v2",
    "Latitude":"37.32621931793764",
    "Longitude":"-121.94822233481415",
    "Local IP":"192.168.100.116"
  },
  {
    "ID":"N01230793",
    "Hardware Model":"unode-v2",
    "Latitude":"37.32596285794468",
    "Longitude":"-121.94759532811986",
    "Local IP":"192.168.100.219"
  },
  {
    "ID":"N01230794",
    "Hardware Model":"unode-v2",
    "Latitude":"37.326053406689724",
    "Longitude":"-121.94742466616623",
    "Local IP":"192.168.101.57"
  },
  {
    "ID":"N01230796",
    "Hardware Model":"unode-v2",
    "Latitude":"37.326294501994674",
    "Longitude":"-121.94850990824688",
    "Local IP":"192.168.101.89"
  },
  {
    "ID":"N01230797",
    "Hardware Model":"unode-v2",
    "Latitude":"37.32572452276559",
    "Longitude":"-121.94849529844532",
    "Local IP":"192.168.100.58"
  },
  {
    "ID":"N01230798",
    "Hardware Model":"unode-v2",
    "Latitude":"37.32612461321167",
    "Longitude":"-121.9480496208667",
    "Local IP":"192.168.100.94"
  },
  {
    "ID":"N0123079b",
    "Hardware Model":"unode-v2",
    "Latitude":"37.32571760648268",
    "Longitude":"-121.94828706939916",
    "Local IP":"192.168.101.130"
  },
  {
    "ID":"N0123079e",
    "Hardware Model":"unode-v2",
    "Latitude":"37.32621131962923",
    "Longitude":"-121.94917451901449",
    "Local IP":"192.168.100.99"
  },
  {
    "ID":"N0123079f",
    "Hardware Model":"unode-v2",
    "Latitude":"37.32637021919601",
    "Longitude":"-121.94831554157747",
    "Local IP":"192.168.102.43"
  },
  {
    "ID":"N012307a0",
    "Hardware Model":"unode-v2",
    "Latitude":"37.326451801728936",
    "Longitude":"-121.94742437763216",
    "Local IP":"192.168.103.211"
  },
  {
    "ID":"N012307a2",
    "Hardware Model":"unode-v2",
    "Latitude":"37.32613101186577",
    "Longitude":"-121.94773848462097",
    "Local IP":"192.168.100.74"
  },
  {
    "ID":"N012307a3",
    "Hardware Model":"unode-v2",
    "Latitude":"37.325799722939465",
    "Longitude":"-121.94822001417378",
    "Local IP":"192.168.100.190"
  },
  {
    "ID":"N012307a5",
    "Hardware Model":"unode-v2",
    "Latitude":"37.326115390592825",
    "Longitude":"-121.94944578369359",
    "Local IP":"192.168.101.126"
  },
  {
    "ID":"N012307a6",
    "Hardware Model":"unode-v2",
    "Latitude":"37.32628602120457",
    "Longitude":"-121.94823208415528",
    "Local IP":"192.168.101.103"
  },
  {
    "ID":"N012307a9",
    "Hardware Model":"unode-v2",
    "Latitude":"37.325787545818905",
    "Longitude":"-121.94904922205541",
    "Local IP":"192.168.101.97"
  },
  {
    "ID":"N012307aa",
    "Hardware Model":"unode-v2",
    "Latitude":"37.326042288513015",
    "Longitude":"-121.94861729009153",
    "Local IP":"192.168.100.79"
  },
  {
    "ID":"N012307ab",
    "Hardware Model":"unode-v2",
    "Latitude":"37.32635960553634",
    "Longitude":"-121.94829779823522",
    "Local IP":"192.168.102.66"
  },
  {
    "ID":"N012307ac",
    "Hardware Model":"unode-v2",
    "Latitude":"37.32605083958084",
    "Longitude":"-121.94780454034117",
    "Local IP":"192.168.100.253"
  },
  {
    "ID":"N012307ad",
    "Hardware Model":"unode-v2",
    "Latitude":"37.32579012491576",
    "Longitude":"-121.94944846592307",
    "Local IP":"192.168.100.37"
  },
  {
    "ID":"N012307af",
    "Hardware Model":"unode-v2",
    "Latitude":"37.3259639243894",
    "Longitude":"-121.94750502711031",
    "Local IP":"192.168.103.230"
  }
]}
  }
};

module.exports = helpers;
