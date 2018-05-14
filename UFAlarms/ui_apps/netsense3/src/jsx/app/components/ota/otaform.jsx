import classNames from 'classnames';
import auth from 'global/utils/auth';
import helpers from 'global/utils/helpers';
import { State, Navigation } from 'react-router';

var Otaform = React.createClass({

  getInitialState: function(){
    return this.props.ota
  },

  propTypes: {
    ota: React.PropTypes.array.isRequired,
    firmwares: React.PropTypes.array.isRequired,
  },

  componentWillReceiveProps: function(nextProps){
      this.setState(nextProps.ota);
  },

  componentDidMount: function(){
   
  },

  componentDidUpdate: function(){

    $('.collapse').on('shown.bs.collapse', function(){
    $(this).parent().find(".glyphicon-plus-sign").removeClass("glyphicon-plus-sign").addClass("glyphicon-minus-sign");
    }).on('hidden.bs.collapse', function(){
    $(this).parent().find(".glyphicon-minus-sign").removeClass("glyphicon-minus-sign").addClass("glyphicon-plus-sign");
    });

    var that = this;
    $("#handleRefresh").click(function(e){
      var refresh_jobId = that.state[0];
      if ('job_info' in refresh_jobId){
        ReactBootstrap.Dispatcher.emit("Otaform.refresh", that.state[0].job_info.jobid); 
      }
      else{
        ReactBootstrap.Dispatcher.emit("Otaform.refresh", that.state[0][0].jobid);
      }
    }); 

  },

  handleSlow: function(e){
    e.stopPropagation();
    e.preventDefault();
    if (confirm("Are you sure you want to slow down the selected OTA job process?")) {
      ReactBootstrap.Dispatcher.emit("Otaform.slow", Object.assign({},this.state));
    };
  },

  handleFast: function(e){
    e.stopPropagation();
    e.preventDefault();
    if (confirm("Are you sure you want to fast the selected OTA job process?")) {
      ReactBootstrap.Dispatcher.emit("Otaform.fast", Object.assign({},this.state));
    };
  },

   handleStop: function(e){
    e.stopPropagation();
    e.preventDefault();
    if (confirm("Are you sure you want to stop the selected OTA job process?")) {
      ReactBootstrap.Dispatcher.emit("Otaform.stop", Object.assign({},this.state));
    };
  },

  handleStatusIcon: function(status) {
      var r = '';

      switch (status) {
          case "COMMAND_SENT":
              r = 'COMMAND SENT';
              break;
          case "NODE_REBOOTING":
              r = 'NODE REBOOTING';
              break;
          case "START_DOWNLOAD":
              r = 'DOWNLOAD STARTED';
              break;
          case "STOP_DOWNLOAD":
              r = 'DOWNLOAD STOPPED';
              break;
          case "UPDATE_SUCCESSFUL":
              r = 'UPDATE SUCCESSFUL';
              break;
          case "UPDATE_FAILED":
            r = 'UPDATE FAILED';
            break;
          case "NODE_OFFLINE":
            r = 'NODE OFFLINE';
            break;
          case "JOB_SENT":
            r = 'JOB SENT';
            break;    
          case "JOB_RECEIVED":
            r = 'JOB RECEIVED';
            break;   
          case "JOB_DONE":
            r = 'JOB DONE';
            break;  
          case "DOWNLOAD_SENT":
            r = 'DOWNLOAD SENT';
            break;  
          case "DOWNLOAD_IN_PROGRESS":
            r = 'DOWNLOAD IN PROGRESS';
            break;  
          case "DOWNLOAD_SUCCESSFUL":
            r = 'DOWNLOAD SUCCESSFUL';
            break;  
          case "INSTALL_SUCCESSFUL":
            r = 'INSTALL SUCCESSFUL';
            break;   
          case "FIRMWARE_NOT_FOUND":
            r = 'FIRMWARE NOT FOUND';
            break;   
          case "FAIL_REGISTRATION":
            r = 'FAILED REGISTRATION';
            break;   
          case "FAIL_JOB_REGISTRATION":
            r = 'FAILED JOB REGISTRATION';
            break;   
          default:
              r = status;
      } 
      return r;

  },

  render: function() {
    var hstyle = {overflowY:"auto",overflowX:"hidden",marginBottom:"16px",
                  maxHeight:helpers.calcHeight(100, -210)+"px !important"};

    var ota = this.props.ota[0];
    
    if(ota && typeof ota!== "undefined"){


      // Video Node:

      if ('job_status' in ota){
        console.log("video ota job status",ota.job_status);


      // Count:


 var nodeDetails = [];
        var videonodeStatus = ota.nodes_status;

        console.log("Video OTA Node status",videonodeStatus);
        

        $.each(videonodeStatus,function(key,value){

          for(var i=0;i<value.length;i++){
            nodeDetails.push({'nodeid': value[i].nodeid, 'status': value[i].status, 'when':value[i].when,'progress':value[i].progress});
          }
        });
       


        var ota_node = [];
        var otaNodeList = ota.nodes_status;
         $.each(otaNodeList, function(key,value){
          for(var i=0;i<value.length;i++){
          //if(value !== undefined && value.nodeid !== null){
              ota_node.push({'when': new Date(value[i].when).toString(),'nodeid': value[i].nodeid, 'status': value[i].status});
            }
          //}
        });
        ota_node.sort(function(a,b) {
          var aNodeid = a.nodeid;
          var bNodeid = b.nodeid;
          var aTime = a.when;
          var bTime = b.when;

          if(aNodeid == bNodeid)
          {
              return (aTime > bTime) ? -1 : (aTime < bTime) ? 1 : 0;
          }
          else
          {
              return (aNodeid < bNodeid) ? -1 : 1;
          }

        });

        console.log("ota_node",ota_node);
        var current_node = "";
        var filtered_node = [];
        for(var k=0;k<ota_node.length;k++){
          
          if(ota_node[k].nodeid !== current_node){
            current_node = ota_node[k].nodeid;
            filtered_node.push({'when': new Date(ota_node[k].when).toString(),'nodeid': ota_node[k].nodeid, 'status': ota_node[k].status});
          }
          
        }

        console.log("filtered_node",filtered_node);

        var ota_job = [];
        var otaJobList = ota.job_status;
        $.each(otaJobList, function(key,value){
          for(var i=0;i<value.length;i++){
          //if(value !== undefined && value.nodeid === null){
            ota_job.push({'when': value[i].when, 'nodeid': value[i].nodeid, 'status': value.status});
          //}
          }
        }); 
        ota_job.sort(function(a,b) {return (b.when < a.when) ? 1 : ((b.when < a.when) ? -1 : 0);} );
        
        // Node Count:
        var otaJobCount = "";
        var otaJobInfo = ota.job_info;
        $.each(otaJobInfo, function(key,value){
          if(key == "count"){
            console.log("Video OTA Job Node Count",value);
            otaJobCount = value;
          }
        });

        var nodecount = otaJobCount;
        var pending = nodecount;
        var failed = 0;
        var success = 0;

        for(var k=0;k<filtered_node.length;k++){
          if(filtered_node[k].status === "INSTALL_SUCCESSFUL"){
            success++;
            pending--;
          }

          if((filtered_node[k].status === "INSTALL_INVALID") || (filtered_node[k].status === "FIRMWARE_NOT_FOUND") || (filtered_node[k].status === "INSTALL_ERROR") || (filtered_node[k].status === "INSTALL_FAILED")){
            failed++;
            pending--;
          }
         
        }

        for(var j=0;j<ota_job.length;j++){
          if((ota_job[j].status === "FIRMWARE_NOT_FOUND") || (ota_job[j].status === "JOB_NOT_FOUND") || (ota_job[j].status === "JOB_STOPPED") || (ota_job[j].status === "DS_REQ_TIMEOUT") || (ota_job[j].status === "DS_REQ_ERROR")){
            failed = nodecount;
            pending = 0;
            success=0;
          }
        }

        console.log("Success",success);
        console.log("Pending",pending);
        console.log("Failed",failed);   


        var jobDetails = ota.job_status;
      

       // Job Summary:
        jobDetails.sort(function(a,b) {return (a.when < b.when) ? 1 : ((b.when < a.when) ? -1 : 0);} ); 

        var jobData = "";
        for(var i=0;i<jobDetails.length;i++){ 

          var jobStatus = this.handleStatusIcon(jobDetails[i].status);

          jobData += 
              '<span style="width:33.33%;float:left;padding-left:10px">' +  jobStatus + '</span>' +
              '<span style="width:33.33%;float:left;padding-left:10px">' +  new Date(jobDetails[i].when).toString() + '</span>' + 
              '<div style="clear:both;">' + '</div>';
        }

        if(jobDetails.length>0){
          var jobSummaryStatus = "";
          jobSummaryStatus =  jobDetails[0].status;
          var jobSummaryTimestamp = "";
          jobSummaryTimestamp = jobDetails[0].when;
          
          var jobSummary = '<div>' + '<div class="netsense__form__header">' + '<h2 style="display:inline-block;position:relative;margin:0px;line-height:50px;padding-left:10px;">' + "Summary:" + '</h2>' + '</div>' + '<br/>' + '<div style="display:inline-block;float:right;margin-right:10px;">' + '<button type="button" class="ns-form-btn" id="handleRefresh" >' + "Refresh " + '</button>' + '</div>' + '</div>' + '<span style="font-weight:bold;float:left;font-size:18px;margin-left:10px;">' + 'Current OTA Job Status: ' + '</span>' + '<span style="float:left;font-size:18px;">' + jobSummaryStatus + '@' + new Date(jobSummaryTimestamp).toString() + '</span>' + '<div style="clear:both;">' + '</div>' +
          '<br/>'+ 
          '<div class="row" style="margin:0 10px;background-color:#eeeeee;border:1px solid #eeeeee">' +
            '<div class="col-md-3" style="padding:10px;background:#55b0de;border:1px solid #55b0de;outline:none">' + '<p style="color:#ffffff;text-align:center;margin-bottom:0px">'+ "Node Count: " + nodecount + '</p>' + '</div>' +
            '<div class="col-md-3" style="padding:10px;background:#55c87e;border:1px solid #55c87e;outline:none">' + '<p style="color:#ffffff;text-align:center;margin-bottom:0px">'+ "Success: " + success  + '</p>' + '</div>' +
            '<div class="col-md-3" style="padding:10px;background:#f3a055;border:1px solid #f3a055;outline:none">'+ '<p style="color:#ffffff;text-align:center;margin-bottom:0px">'+ "Pending: " + pending  + '</p>' + '</div>' +
            '<div class="col-md-3" style="padding:10px;background:#e37269;border:1px solid #e37269;outline:none">'+ '<p style="color:#ffffff;text-align:center;margin-bottom:0px">'+ "Failed: " + failed  + '</p>' + '</div>' +
          '</div>' +
          '<h4 style="font-weight:bold;background-color:#eeeeee;border:1px solid #eeeeee;padding:10px;margin:10px;" >' + 'Job Details:' + '</h4>' + 
            '<div style="clear:both;">' + '</div>'+ 
            '<div style="border:1px solid #eee;margin:20px">'+ 
            '<h4 style="text-decoration:underline;font-weight:bold;width:33.33%;float:left;padding-left:10px">' +  "Status" + '</h4>' +
            '<h4 style="text-decoration:underline;font-weight:bold;width:33.33%;float:left;padding-left:10px">' +  "Timestamp" + '</h4>' +
            '<div style="clear:both;">' + '</div>'+
            jobData +
            '</div>';
        }  
        var buttonVisibility = {display:"none"};

        // Node Summary:
        var nodeDetails = [];
        var videonodeStatus = ota.nodes_status;

        console.log("Video OTA Node status",videonodeStatus);
        

        $.each(videonodeStatus,function(key,value){

          for(var i=0;i<value.length;i++){
            nodeDetails.push({'nodeid': value[i].nodeid, 'status': value[i].status, 'when':value[i].when,'progress':value[i].progress});
          }
        });
       

        nodeDetails.sort(function(a,b) {
          var aNodeid = a.nodeid;
          var bNodeid = b.nodeid;
          var aTime = a.when;
          var bTime = b.when;

          if(aNodeid == bNodeid)
          {
            if(aTime == bTime){
              return 1;
            }
            else{
              return (aTime > bTime) ? -1 : (aTime < bTime) ? 1 : 0;
            }
              
          }
          else
          {
              return (aNodeid < bNodeid) ? -1 : 1;
          }

        });
      
        console.log("Video OTA Node list sorted",nodeDetails);

        var nodedata = "";
        var currentNodeId = "";
        var index = 0;
      for(var i=0;i<nodeDetails.length;i++){  

        if((nodeDetails[i].nodeid !== currentNodeId) && (i === 0)){
          currentNodeId = nodeDetails[i].nodeid;
          var hash = "#" + index;
          var nodesuccess = (nodeDetails[i].success)? ('<span style="color:#5cb85c;padding-left:20px;" class="glyphicon glyphicon-ok"></span>'):('<span style="color:#d9534f;padding-left:20px;" class="glyphicon glyphicon-remove"></span>');
          
          var nodeStatus = this.handleStatusIcon(nodeDetails[i].status);

          //Progress bar:
          var firmwareimagesize = 0;
          for(var m=0;m<this.props.firmwares.length;m++){
            var that = this;
            $.each(otaJobInfo, function(key,value){
              if(key == "firmwareid"){
                var otaJobid = value;
                if(that.props.firmwares[m].firmwareid === otaJobid){
                  firmwareimagesize = that.props.firmwares[m].image_size;
                }
              }
            });
          }
          var nodeProgress = "";
          var prev = nodeDetails[i+1];

          if((nodeDetails[i].progress === -1) && (typeof prev === "undefined")){
            nodeProgress = 0;
          }

          else if((nodeDetails[i].progress === -1) && (prev.status === "DOWNLOAD_SUCCESSFUL") && (prev.progress === firmwareimagesize)){
            nodeProgress = 100;
          }

          else if (((nodeDetails[i].progress === -1)|| (nodeDetails[i].progress !== -1)) && (firmwareimagesize === 0) && (prev.progress !== firmwareimagesize)){
            nodeProgress = "N/A";
          }
          else{
            nodeProgress = (nodeDetails[i].progress/firmwareimagesize).toFixed(2);
          }

          var nodeProgressStyle = "";
          if(nodeProgress === "N/A"){
            nodeProgressStyle = nodeProgress;
          }
          else{
            nodeProgressStyle = nodeProgress + "%";
          }

          nodedata += '<h4 style="font-weight:bold;background-color:#eeeeee;border:1px solid #eeeeee;padding:10px;margin:0 10px;" >' + 'Node(s) Details:' + '</h4>' +
          '<div style="clear:both;">' + '</div>'+
          '<h4 style="text-decoration:underline;font-weight:bold;width:20%;float:left;padding-left:30px">' +  "NodeID" + '</h4>' +
          '<h4 style="text-decoration:underline;font-weight:bold;width:20%;float:left;padding-left:27px">' +  "Status" + '</h4>' +
          '<h4 style="text-decoration:underline;font-weight:bold;width:20%;float:left;padding-left:10px">' +  "Progress" + '</h4>' +
          '<h4 style="text-decoration:underline;font-weight:bold;width:25%;float:right">' +  "Timestamp" + '</h4>' +
          '<div style="clear:both;">' + '</div>'+
          '<div>' +
            '<div style="margin:20px;margin-bottom:0px;" key=' + index + '>' +
              '<div class="nodeAccordionWrapper">' +
                '<div class="panel-group" id="accordion">' +
                  '<div class="panel panel-default">' +
                    '<div style="padding:"10px" class="panel-heading">' +
                      '<h4 style="float:left;width:20%;font-size:14px;" class="panel-title">' +
                      '<a data-toggle="collapse" data-parent="#accordion" href=' + hash + '>' +
                        '<span class="glyphicon glyphicon-plus-sign">' + '</span>' + '&nbsp;' +
                      nodeDetails[i].nodeid +
                      '</a>' +
                      '</h4>' +
                      '<h4 style="width:20%;float:left;margin:0px;font-size:14px;">' +  nodeStatus + '</h4>' +

                      '<div class="progress" style="width:20%;height:20px;float:left;background-color:#55b0de">' +
                        '<div class="progress-bar-striped progress-bar bg-success" role="progressbar" style="width: '+ nodeProgressStyle +' ;line-height:20px" aria-valuenow='+ nodeProgress + ' aria-valuemin="0" aria-valuemax="100">' + 
                         nodeProgressStyle +
                        '</div>' +
                      '</div>' +

                      '<h4 style="width:25%;float:right;padding-left:23px;margin:0px;text-overflow:ellipsis;overflow:hidden;font-size:14px;">' +  new Date(nodeDetails[i].when).toString() + '</h4>' + 
                      
                      '<div style="clear:both;">' + '</div>'+
                    '</div>' +
                    '<div id="' + index + '"class="panel-collapse collapse">' +
                      '<div class="panel-body">' +
                        '<h4 style="font-weight:bold;width:33.33%;float:left;padding-left:10px">' +  "Status" + '</h4>' +
                        '<h4 style="font-weight:bold;width:33.33%;float:left;padding-left:10px">' +  "Timestamp" + '</h4>' + 
                        '<div style="clear:both;">' + '</div>' +
                        '<span style="width:33.33%;float:left;padding-left:10px">' +  nodeStatus + '</span>' +
                        '<span style="width:33.33%;float:left;padding-left:10px">' +  new Date(nodeDetails[i].when).toString() + '</span>' + 
                        '<div style="clear:both;">' + '</div>';
        }

        else if ((nodeDetails[i].nodeid !== currentNodeId)){
          currentNodeId = nodeDetails[i].nodeid;
          var nodesuccess = (nodeDetails[i].success)? ('<span style="color:#5cb85c;padding-left:20px;" class="glyphicon glyphicon-ok"></span>'):('<span style="color:#d9534f;padding-left:20px;" class="glyphicon glyphicon-remove"></span>');
          var nodeStatus = this.handleStatusIcon(nodeDetails[i].status);

          index = index +1;
          var hash = "#" + index;
          nodedata+=  '</div>' +
                    '</div>' +
                  '</div>' +
                '</div>' +
              '</div>' +
            '</div>' +
          '</div>' +
          '<div>' +
            '<div style="margin:20px;margin-bottom:0px;" key=' + index + '>' +
              '<div class="nodeAccordionWrapper">' +
                '<div class="panel-group" id="accordion">' +
                  '<div class="panel panel-default">' +
                    '<div class="panel-heading">' +
                      '<h4 style="float:left;width:20%;font-size:14px;" class="panel-title">' +
                        '<a data-toggle="collapse" data-parent="#accordion" href=' + hash + '>' +
                        '<span class="glyphicon glyphicon-plus-sign">' + '</span>' + '&nbsp;' +
                        nodeDetails[i].nodeid +
                        '</a>' +
                        '<h4 style="width:30%;float:left;margin:0px;font-size:14px;">' +  nodeStatus + '</h4>' +
                        '<h4 style="width:35%;float:left;margin:0px;text-overflow:ellipsis;overflow:hidden;font-size:14px;">' +  new Date(nodeDetails[i].when).toString() + '</h4>' + 
                      '</h4>' +
                      '<div style="clear:both;">' + '</div>'+
                    '</div>' +
                    '<div id="' + index + '"class="panel-collapse collapse">' +
                      '<div class="panel-body">' +
                        '<h4 style="font-weight:bold;width:33.33%;float:left;padding-left:10px">' +  "Status" + '</h4>' +
                        '<h4 style="font-weight:bold;width:33.33%;float:left;padding-left:10px">' +  "Timestamp" + '</h4>' + 
                        '<div style="clear:both;">' + '</div>' +
                        '<span style="width:33.33%;float:left;padding-left:10px">' +  nodeStatus + '</span>' +
                        '<span style="width:33.33%;float:left;padding-left:10px;">' +  new Date(nodeDetails[i].when).toString() + '</span>' + 
                        '<div style="clear:both;">' + '</div>';

        }
        else{
          var nodesuccess = (nodeDetails[i].success)? ('<span style="color:#5cb85c;padding-left:20px;" class="glyphicon glyphicon-ok"></span>'):('<span style="color:#d9534f;padding-left:20px;" class="glyphicon glyphicon-remove"></span>');
          var nodeStatus = this.handleStatusIcon(nodeDetails[i].status);

          nodedata+=  '<span style="width:33.33%;float:left;padding-left:10px">' +  nodeStatus + '</span>' +
                      '<span style="width:33.33%;float:left;padding-left:10px">' +  new Date(nodeDetails[i].when).toString() + '</span>' + 
                      '<div style="clear:both;">' + '</div>';
        }
      }  


      }

      // Core Node:
      else{
      // Count:

      var ota_node = [];
       $.each(ota, function(key,value){
        if(value !== undefined && value.nodeid !== null){
          ota_node.push({'when': new Date(value.when).toString(),'success': value.success, 'nodeid': value.nodeid, 'status': value.status, 'node_count':value.node_count});
        }
      });
      ota_node.sort(function(a,b) {
        var aNodeid = a.nodeid;
        var bNodeid = b.nodeid;
        var aTime = a.when;
        var bTime = b.when;

        if(aNodeid == bNodeid)
        {
            return (aTime > bTime) ? -1 : (aTime < bTime) ? 1 : 0;
        }
        else
        {
            return (aNodeid < bNodeid) ? -1 : 1;
        }

      });

      console.log("ota_node",ota_node);
      var current_node = "";
      var filtered_node = [];
      for(var k=0;k<ota_node.length;k++){
        
        if(ota_node[k].nodeid !== current_node){
          current_node = ota_node[k].nodeid;
          filtered_node.push({'when': new Date(ota_node[k].when).toString(),'success': ota_node[k].success, 'nodeid': ota_node[k].nodeid, 'status': ota_node[k].status});
        }
        
      }

      console.log("filtered_node",filtered_node);

      var ota_job = [];
      $.each(ota, function(key,value){
        if(value !== undefined && value.nodeid === null){
          ota_job.push({'when': value.when,'success': value.success, 'nodeid': value.nodeid, 'status': value.status, 'node_count':value.node_count});
        }
      }); 
      ota_job.sort(function(a,b) {return (b.when < a.when) ? 1 : ((b.when < a.when) ? -1 : 0);} );
      
      console.log("ota_job",ota_job[0]);
      var nodecount = ota_job[0].node_count;
      var pending = nodecount;
      var failed = 0;
      var success = 0;

      for(var k=0;k<filtered_node.length;k++){
        if(filtered_node[k].status === "UPDATE_SUCCESSFUL"){
          success++;
          pending--;
        }

        if((filtered_node[k].status === "FAIL_REGISTRATION") || (filtered_node[k].status === "FIRMWARE_NOT_FOUND") || (filtered_node[k].status === "NODE_OFFLINE") || (filtered_node[k].status === "UPDATE_FAILED")){
          failed++;
          pending--;
        }
       
      }

      for(var j=0;j<ota_job.length;j++){
        if((ota_job[j].status === "FIRMWARE_NOT_FOUND") || (ota_job[j].status === "FAIL_JOB_REGISTRATION")){
          failed = nodecount;
          pending = 0;
          success=0;
        }
      }
      

      console.log("Success",success);
      console.log("Pending",pending);
      console.log("Failed",failed);   

      var jobDetails = [];
      $.each(ota, function(key,value){
        if(value !== undefined && value.nodeid === null){
          jobDetails.push({'success': value.success, 'nodeid': value.nodeid, 'status': value.status, 'when':value.when});
        }
      });
    

     // Job Summary:
      jobDetails.sort(function(a,b) {return (a.when < b.when) ? 1 : ((b.when < a.when) ? -1 : 0);} ); 

      var jobData = "";
      for(var i=0;i<jobDetails.length;i++){ 

        var jobsuccess = (jobDetails[i].success)? ('<span style="color:#5cb85c;padding-left:20px;" class="glyphicon glyphicon-ok"></span>'):('<span style="color:#d9534f;padding-left:20px;" class="glyphicon glyphicon-remove"></span>');
        var jobStatus = this.handleStatusIcon(jobDetails[i].status);

        jobData += 
            '<span style="width:33.33%;float:left;padding-left:10px">' +  jobStatus + '</span>' +
            '<span style="width:33.33%;float:left;padding-left:10px">' +  jobsuccess + '</span>' +
            '<span style="width:33.33%;float:left;padding-left:10px">' +  new Date(jobDetails[i].when).toString() + '</span>' + 
            '<div style="clear:both;">' + '</div>';
      }

      if(jobDetails.length>0){
        var jobSummaryStatus = "";
        jobSummaryStatus =  jobDetails[0].status;
        var jobSummaryTimestamp = "";
        jobSummaryTimestamp = jobDetails[0].when;
        
        var jobSummary = '<div>' + '<div class="netsense__form__header">' + '<h2 style="display:inline-block;position:relative;margin:0px;line-height:50px;padding-left:10px;">' + "Summary:" + '</h2>' + '</div>' + '<br/>' + '<div style="display:inline-block;float:right;margin-right:10px;">' + '<button type="button" class="ns-form-btn" id="handleRefresh" >' + "Refresh " + '</button>' + '</div>' + '</div>' + '<span style="font-weight:bold;float:left;font-size:18px;margin-left:10px;">' + 'Current OTA Job Status: ' + '</span>' + '<span style="float:left;font-size:18px;">' + jobSummaryStatus + '@' + new Date(jobSummaryTimestamp).toString() + '</span>' + '<div style="clear:both;">' + '</div>' +
        '<br/>'+ 
        '<div class="row" style="margin:0 10px;background-color:#eeeeee;border:1px solid #eeeeee">' +
          '<div class="col-md-3" style="padding:10px;background:#55b0de;border:1px solid #55b0de;outline:none">' + '<p style="color:#ffffff;text-align:center;margin-bottom:0px">'+ "Node Count: " + nodecount + '</p>' + '</div>' +
          '<div class="col-md-3" style="padding:10px;background:#55c87e;border:1px solid #55c87e;outline:none">' + '<p style="color:#ffffff;text-align:center;margin-bottom:0px">'+ "Success: " + success  + '</p>' + '</div>' +
          '<div class="col-md-3" style="padding:10px;background:#f3a055;border:1px solid #f3a055;outline:none">'+ '<p style="color:#ffffff;text-align:center;margin-bottom:0px">'+ "Pending: " + pending  + '</p>' + '</div>' +
          '<div class="col-md-3" style="padding:10px;background:#e37269;border:1px solid #e37269;outline:none">'+ '<p style="color:#ffffff;text-align:center;margin-bottom:0px">'+ "Failed: " + failed  + '</p>' + '</div>' +
        '</div>' +
        '<h4 style="font-weight:bold;background-color:#eeeeee;border:1px solid #eeeeee;padding:10px;margin:10px;" >' + 'Job Details:' + '</h4>' + 
          '<div style="clear:both;">' + '</div>'+ 
          '<div style="border:1px solid #eee;margin:20px">'+ 
          '<h4 style="text-decoration:underline;font-weight:bold;width:33.33%;float:left;padding-left:10px">' +  "Status" + '</h4>' +
          '<h4 style="text-decoration:underline;font-weight:bold;width:33.33%;float:left;padding-left:10px">' +  "Success" + '</h4>' + 
          '<h4 style="text-decoration:underline;font-weight:bold;width:33.33%;float:left;padding-left:10px">' +  "Timestamp" + '</h4>' +
          '<div style="clear:both;">' + '</div>'+
          jobData +
          '</div>';
      }  
      var buttonVisibility = ((jobSummaryStatus=="JOB_DONE") || (jobSummaryStatus=="FIRMWARE_NOT_FOUND") || (jobSummaryStatus=="FAIL_JOB_REGISTRATION"))?{display:"none"}:{};

       // Node Summary:
      var nodeDetails = [];

       $.each(ota, function(key,value){
        if(value !== undefined && value.nodeid !== null){
          nodeDetails.push({'success': value.success, 'nodeid': value.nodeid, 'status': value.status, 'when':value.when});
        }
      });

      nodeDetails.sort(function(a,b) {
      //return (a.nodeid > b.nodeid) ? 1 : ((b.nodeid > a.nodeid) ? -1 : 0);
        var aNodeid = a.nodeid;
        var bNodeid = b.nodeid;
        var aTime = a.when;
        var bTime = b.when;

        if(aNodeid == bNodeid)
        {
            return (aTime > bTime) ? -1 : (aTime < bTime) ? 1 : 0;
        }
        else
        {
            return (aNodeid < bNodeid) ? -1 : 1;
        }

      });
      
      var nodedata = "";
      var currentNodeId = "";
      var index = 0;
      for(var i=0;i<nodeDetails.length;i++){  

        if((nodeDetails[i].nodeid !== currentNodeId) && (i === 0)){
          currentNodeId = nodeDetails[i].nodeid;
          var hash = "#" + index;
          var nodesuccess = (nodeDetails[i].success)? ('<span style="color:#5cb85c;padding-left:20px;" class="glyphicon glyphicon-ok"></span>'):('<span style="color:#d9534f;padding-left:20px;" class="glyphicon glyphicon-remove"></span>');
          
          var nodeStatus = this.handleStatusIcon(nodeDetails[i].status);

          nodedata += '<h4 style="font-weight:bold;background-color:#eeeeee;border:1px solid #eeeeee;padding:10px;margin:0 10px;" >' + 'Node(s) Details:' + '</h4>' +
          '<div style="clear:both;">' + '</div>'+
          '<h4 style="text-decoration:underline;font-weight:bold;width:20%;float:left;padding-left:30px">' +  "NodeID" + '</h4>' +
          '<h4 style="text-decoration:underline;font-weight:bold;width:30%;float:left;padding-left:20px">' +  "Status" + '</h4>' +
          '<h4 style="text-decoration:underline;font-weight:bold;width:15%;float:left">' +  "Success" + '</h4>' + 
          '<h4 style="text-decoration:underline;font-weight:bold;width:35%;float:left">' +  "Timestamp" + '</h4>' +
          '<div style="clear:both;">' + '</div>'+
          '<div>' +
            '<div style="margin:20px;margin-bottom:0px;" key=' + index + '>' +
              '<div class="nodeAccordionWrapper">' +
                '<div class="panel-group" id="accordion">' +
                  '<div class="panel panel-default">' +
                    '<div style="padding:"10px" class="panel-heading">' +
                      '<h4 style="float:left;width:20%;font-size:14px;" class="panel-title">' +
                        '<a data-toggle="collapse" data-parent="#accordion" href=' + hash + '>' +
                          '<span class="glyphicon glyphicon-plus-sign">' + '</span>' + '&nbsp;' +
                        nodeDetails[i].nodeid +
                        '</a>' +
                        '</h4>' +
                        '<h4 style="width:30%;float:left;margin:0px;font-size:14px;">' +  nodeStatus + '</h4>' +
                        '<h4 style="width:15%;float:left;margin:0px;font-size:14px;">' +  nodesuccess + '</h4>' +
                        '<h4 style="width:35%;float:left;margin:0px;text-overflow:ellipsis;overflow:hidden;font-size:14px;">' +  new Date(nodeDetails[i].when).toString() + '</h4>' + 
                      
                      '<div style="clear:both;">' + '</div>'+
                    '</div>' +
                    '<div id="' + index + '"class="panel-collapse collapse">' +
                      '<div class="panel-body">' +
                        '<h4 style="font-weight:bold;width:33.33%;float:left;padding-left:10px">' +  "Status" + '</h4>' +
                        '<h4 style="font-weight:bold;width:33.33%;float:left;padding-left:10px">' +  "Success" + '</h4>' +
                        '<h4 style="font-weight:bold;width:33.33%;float:left;padding-left:10px">' +  "Timestamp" + '</h4>' + 
                        '<div style="clear:both;">' + '</div>' +
                        '<span style="width:33.33%;float:left;padding-left:10px">' +  nodeStatus + '</span>' +
                        '<span style="width:33.33%;float:left;padding-left:10px">' +  nodesuccess + '</span>' +
                        '<span style="width:33.33%;float:left;padding-left:10px">' +  new Date(nodeDetails[i].when).toString() + '</span>' + 
                        '<div style="clear:both;">' + '</div>';
        }

        else if ((nodeDetails[i].nodeid !== currentNodeId)){
          currentNodeId = nodeDetails[i].nodeid;
          var nodesuccess = (nodeDetails[i].success)? ('<span style="color:#5cb85c;padding-left:20px;" class="glyphicon glyphicon-ok"></span>'):('<span style="color:#d9534f;padding-left:20px;" class="glyphicon glyphicon-remove"></span>');
          var nodeStatus = this.handleStatusIcon(nodeDetails[i].status);

          index = index +1;
          var hash = "#" + index;
          nodedata+=  '</div>' +
                    '</div>' +
                  '</div>' +
                '</div>' +
              '</div>' +
            '</div>' +
          '</div>' +
          '<div>' +
            '<div style="margin:20px;margin-bottom:0px;" key=' + index + '>' +
              '<div class="nodeAccordionWrapper">' +
                '<div class="panel-group" id="accordion">' +
                  '<div class="panel panel-default">' +
                    '<div class="panel-heading">' +
                      '<h4 style="float:left;width:20%;font-size:14px;" class="panel-title">' +
                        '<a data-toggle="collapse" data-parent="#accordion" href=' + hash + '>' +
                        '<span class="glyphicon glyphicon-plus-sign">' + '</span>' + '&nbsp;' +
                        nodeDetails[i].nodeid +
                        '</a>' +
                        '<h4 style="width:30%;float:left;margin:0px;font-size:14px;">' +  nodeStatus + '</h4>' +
                        '<h4 style="width:15%;float:left;margin:0px;font-size:14px;">' +  nodesuccess + '</h4>' +
                        '<h4 style="width:35%;float:left;margin:0px;text-overflow:ellipsis;overflow:hidden;font-size:14px;">' +  new Date(nodeDetails[i].when).toString() + '</h4>' + 
                      '</h4>' +
                      '<div style="clear:both;">' + '</div>'+
                    '</div>' +
                    '<div id="' + index + '"class="panel-collapse collapse">' +
                      '<div class="panel-body">' +
                        '<h4 style="font-weight:bold;width:33.33%;float:left;padding-left:10px">' +  "Status" + '</h4>' +
                        '<h4 style="font-weight:bold;width:33.33%;float:left;padding-left:10px">' +  "Success" + '</h4>' +
                        '<h4 style="font-weight:bold;width:33.33%;float:left;padding-left:10px">' +  "Timestamp" + '</h4>' + 
                        '<div style="clear:both;">' + '</div>' +
                        '<span style="width:33.33%;float:left;padding-left:10px">' +  nodeStatus + '</span>' +
                        '<span style="width:33.33%;float:left;padding-left:10px">' +  nodesuccess + '</span>' +
                        '<span style="width:33.33%;float:left;padding-left:10px;">' +  new Date(nodeDetails[i].when).toString() + '</span>' + 
                        '<div style="clear:both;">' + '</div>';

        }
        else{
          var nodesuccess = (nodeDetails[i].success)? ('<span style="color:#5cb85c;padding-left:20px;" class="glyphicon glyphicon-ok"></span>'):('<span style="color:#d9534f;padding-left:20px;" class="glyphicon glyphicon-remove"></span>');
          var nodeStatus = this.handleStatusIcon(nodeDetails[i].status);

          nodedata+=  '<span style="width:33.33%;float:left;padding-left:10px">' +  nodeStatus + '</span>' +
                      '<span style="width:33.33%;float:left;padding-left:10px">' +  nodesuccess + '</span>' +
                      '<span style="width:33.33%;float:left;padding-left:10px">' +  new Date(nodeDetails[i].when).toString() + '</span>' + 
                      '<div style="clear:both;">' + '</div>';
        }
      }  
      }                  


    }
   
    return (
      <div style={hstyle}>
       
        <div>

          <div className="col-sm-12" style={{position:"absolute",bottom:"10px",right:"35px"}}>
            <div style={buttonVisibility}>
              <button type="button" className="ns-form-btn" style={{float:"right"}} onClick={this.handleFast}>
                  Fast OTA </button>
              <button type="button" className="ns-form-btn" style={{float:"right",marginRight:"10px"}} onClick={this.handleSlow}>
                  Slow OTA </button>
            </div>
            <div>
              {((jobSummaryStatus!="JOB_DONE") && (jobSummaryStatus!="FIRMWARE_NOT_FOUND") && (jobSummaryStatus!="FAIL_JOB_REGISTRATION") && (jobSummaryStatus!="DS_REQ_TIMEOUT")) && (
                  <button type="button" className="ns-med-btn" style={{marginRight:"10px"}} onClick={this.handleStop}>
                    Stop OTA</button>
                )
              }
            </div>

            <div style={{clear:"both"}}></div>

          </div>

          <div>
            <div dangerouslySetInnerHTML={{__html: jobSummary}}>
            </div>
          </div>
          <div>
            <div dangerouslySetInnerHTML={{__html: nodedata}}>
            </div>
          </div>
          <div style={{clear:"both"}}></div>
        </div>
      </div>
      

      );
 
  }
});

module.exports = Otaform;
