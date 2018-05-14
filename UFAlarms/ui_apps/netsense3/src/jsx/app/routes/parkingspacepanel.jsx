import classNames from 'classnames';
import {
    State,
    Navigation
} from 'react-router';
import helpers from 'global/utils/helpers';
import Parkingspacelist from 'components/parkingspace/parkingspacelist';
import ParkingspaceDetail from 'components/parkingspace/parkingspacedetail';
import Parkingspacemultidetail from 'components/parkingspace/parkingspacemultidetail';
import Parkingspacemap from 'components/parkingspace/parkingspacemap';
import Parkingspacemultirenameform from 'components/parkingspace/parkingspacemultirenameform';
import Parkingspacemultidetaileditform from 'components/parkingspace/parkingspacemultidetaileditform';

import Header from 'common/headernew';

var Body = React.createClass({
  getInitialState: function(){
    return {
      // parkingroups: null,
      activeFilter: 'active',
      site: null,
      parkingspaceID: "-1",
      ui: {detail: "hidden",  // one of "unpinned", "pinned", "hidden"
           prevDetail: "pinned",   // one of "unpinned", "pinned", "hidden"
           list: "normal",  // "normal" or "expanded"
           map: "normal"  // "normal" or "expanded"
       },
      showAttributeDetail:false,
      activeFilter: "active",
      selected_spaces: [],
        hoveredSpaces:[]

    }
  },

  detailUnpinnedPosition: {
      left: "34%",
      top:  "6%",
      height: "80%",
      width: "25%"
  },

  hideDetail: function() {
      if (this.state.ui.detail == "unpinned") {
          this.detailUnpinnedPosition = {
              left: $("#parkingspace-detail-panel").position().left + "px",
              top: $("#parkingspace-detail-panel").position().top + "px",
              width: $("#parkingspace-detail-panel").width() + "px",
              height: $("#parkingspace-detail-panel").height() + "px"
              };
      };
      this.setState({ui: $.extend({}, this.state.ui, {detail: "hidden", prevDetail: this.state.ui.detail})});
  },

  showDetail: function() {
      this.setState({ui: $.extend({}, this.state.ui, {detail: this.state.ui.prevDetail, prevDetail: "hidden"})});
  },

  pinDetail: function() {
      this.detailUnpinnedPosition = {
          left: $("#parkingspace-detail-panel").position().left + "px",
          top: $("#parkingspace-detail-panel").position().top + "px",
          width: $("#parkingspace-detail-panel").width() + "px",
          height: $("#parkingspace-detail-panel").height() + "px"
      };
      this.setState({ui: $.extend({}, this.state.ui, {detail: "pinned", prevDetail: "unpinned"})});
  },

  unpinDetail: function() {
      this.setState({ui: $.extend({}, this.state.ui, {detail: "unpinned", prevDetail: "pinned"})});
  },

  applyFilter: function(zone) {
    switch (this.state.activeFilter) {
      case "active":
        return typeof zone.active == "undefined" || zone.active;
      case "inactive":
        return typeof zone.active != "undefined" && !zone.active;
      case "all":
        return true;
    }
  },

  init: function() {
    var that = this;
    if (NSN.customerID=="-1" && NSN.siteID=="-1") {
      $("#loadingmsg").html("Please select an Account and a Site first.")
      return;
    } else {
      if (NSN.customerID=="-1") {
        $("#loadingmsg").html("Please select an Account first.")
        return;
      } else {
        if (NSN.siteID=="-1") {
          $("#loadingmsg").html("Please select a Site first.")
          return;
        }
      }
    };

	  //  Get parking Groups
	  $.ajax({
		  // url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/parkinggroups'+ '?t=' + (new Date()).getTime(),
		  url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/parkinggroups',
		  data : '',
		  xhrFields: {
			  withCredentials: true
		  },
		  method : 'GET',
		  dataType : 'json',
		  success : function(data){
			  if (data == "") {
				  that.setState({parkinggroups:[]});
			  } else {
				  that.setState({
					  parkinggroups: data.map(function(parkinggroup, index) {
						  if (typeof parkinggroup.parkingzones == "undefined" || parkinggroup.parkingzones == "") {
							  parkinggroup.numzones = 0;
						  } else {
							  parkinggroup.numzones = parkinggroup.parkingzones.split(" ").length;
						  }
						  return parkinggroup;
					  })
				  })
			  }
		  },
		  error : function(jqXHR, status, error){
			  console.log("ajax failure (parkinggroups): " + status + " - " + error);
			  $("#loadingmsg").html("Cannot retrieve Parking Groups.  API reported error: " + error);
		  }
	  });

    // Default metadat for parking spaces that dont have - metadata
      var defaultMetadataObject =  {
          typeOfVehicle:["unrestricted"],
          name:" No Name",
          handicap:false,
          reservation:false,
          businessUse:"general",
          howMetered:"not-metered",
          active:true,
	      parkingSpaceType :"surface-lot",
          // PPV:false,
          areaType:["mixed"],
          paystationid:"not-metered",
      };

      var parkingzones = [];

      $.ajax({
          url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/parkingzones'
          + '?t=' + (new Date()).getTime(),
          data : '',
          xhrFields: {
              withCredentials: true
          },
          method : 'GET',
          dataType : 'json',
          success : function(data){
              if (data == "") {

                  that.setState({parkingspaces:[],
                        parkingspacesmetadata:[],
                        parkingzones:[],
                        defaultMetadataObject
                  });

              } else {
                  for (var i=0; i<data.length; i++) {
                      data[i].active = data[i].active?"true":"false";
                      data[i].available_spaces = Math.max(0, data[i].available_spaces);
                  }
                  parkingzones = data;
                  var parkingSpotids =  {
                      "parkingspaceids": []
                  };
                  var demarcatedSpaces = [];
                  var unDemarcatedSpaces = [];

                  // get the parkingspot id's and get all the
                  for(var a in parkingzones){
                      if(parkingzones[a].type === "Demarcated"){ // draw circle and use the spot id only
                          if(parkingzones[a].config.spots.length >= 1){ // check if the spots is not an empty array
                              for(var j in parkingzones[a].config.spots ){
                                  parkingzones[a].config.spots[j].nodeid = parkingzones[a].nodeid;
	                              parkingzones[a].config.spots[j].parkinggroupid = parkingzones[a].parkinggroupid;
                                  console.log("parkingzones[a].config.spots[j].parkinggroupid -- ", parkingzones[a].config.spots[j].parkinggroupid)
	                              parkingSpotids.parkingspaceids.push(parkingzones[a].config.spots[j].uuid);
	                              demarcatedSpaces.push(parkingzones[a].config.spots[j]);
                              }
                          }
                      }else if(parkingzones[a].type === "NonDemarcated"){ // draw polygon and use the zone id to push inside the parkingspot id as the spot
                          parkingSpotids.parkingspaceids.push(parkingzones[a].parkingzoneid);
                          unDemarcatedSpaces.push(parkingzones[a])
                      }
                  }
                  var parkingSpaces = [];
                  $.ajax({
                      url: NSN.apiURL  + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/parkingspaces/spaceattributes', // url changes
                      xhrFields: {
                          withCredentials: true
                      },
                      data: JSON.stringify(parkingSpotids),
                      method : 'POST',
                      dataType : 'json',
                      contentType: "application/json",
                      success : function(metaData){
                          var metaDataParkingSpaces = metaData;

                          // for demarcated spaces - that have metadata
                          for(var i = 0; i < demarcatedSpaces.length ; i ++){
                              for(var j = 0; j<metaDataParkingSpaces.length; j++){
                                  // the ones that have metadata
                                  do{
                                      if( demarcatedSpaces[i].uuid == metaDataParkingSpaces[j].parkingspaceid){
                                          demarcatedSpaces[i].metadata = metaDataParkingSpaces[j];
                                          demarcatedSpaces[i].name = metaDataParkingSpaces[j].name;
                                          demarcatedSpaces[i].parkingspaceid = demarcatedSpaces[i].uuid;
                                          demarcatedSpaces[i].demarcated = true;
                                          parkingSpaces.push(demarcatedSpaces[i]);
                                      }
                                  }while(j == metaDataParkingSpaces.length);
                              }
                          }
                          // for demarcated spaces - that DON'T have metadata
                          for (var a in demarcatedSpaces) {
                              if (demarcatedSpaces[a].metadata == undefined) {
                                  var default1 = {
                                      typeOfVehicle: ["unrestricted"],
                                      name: "No Name",
                                      handicap: false,
                                      reservation: false,
                                      businessUse: "general",
                                      howMetered: "not-metered",
                                      active: true,
	                                  parkingSpaceType:"",
                                      // PPV: false,
                                      areaType: ["mixed"],
                                      paystationid: "not-metered",
                                      parkingspaceid: ""
                                  };
                                  demarcatedSpaces[a].metadata = default1;
                                  demarcatedSpaces[a].name = "No Name";
                                  demarcatedSpaces[a].metadata.parkingspaceid = demarcatedSpaces[a].uuid;
                                  demarcatedSpaces[a].parkingspaceid = demarcatedSpaces[a].uuid;
                                  demarcatedSpaces[a].demarcated = true;
                                  parkingSpaces.push(demarcatedSpaces[a]);
                                  default1 = {};
                              }
                          }

                          // for undemaracted spaces that have Metadata
                          for(var i = 0; i < unDemarcatedSpaces.length ; i ++){
                              for(var j = 0; j< metaDataParkingSpaces.length; j++){
                                  // the ones that have metadata
                                  do{
                                      if( unDemarcatedSpaces[i].parkingzoneid == metaDataParkingSpaces[j].parkingspaceid){
                                          unDemarcatedSpaces[i].metadata = metaDataParkingSpaces[j];
                                          unDemarcatedSpaces[i].name = metaDataParkingSpaces[j].name;
                                          unDemarcatedSpaces[i].parkingspaceid = unDemarcatedSpaces[i].parkingzoneid;
                                          unDemarcatedSpaces[i].demarcated = false;
                                          parkingSpaces.push(unDemarcatedSpaces[i]);
                                      }
                                  }while(j == metaDataParkingSpaces.length);
                              }
                          }

                          // for undemarcated spaces - that DON'T have metadata
                          for(var a in unDemarcatedSpaces){
                              if(unDemarcatedSpaces[a].metadata == undefined) {
                                  var default1 =  {
                                      typeOfVehicle:["unrestricted"],
                                      name:"No Name",
                                      handicap:false,
                                      reservation:false,
                                      businessUse:"general",
                                      howMetered:"not-metered",
                                      active:true,
	                                  parkingSpaceType:"",
                                      // PPV:false,
                                      areaType:["mixed"],
                                      paystationid:"not-metered",
                                      parkingspaceid:""
                                  };
                                  unDemarcatedSpaces[a].metadata = default1;
                                  unDemarcatedSpaces[a].name="No Name";
                                  unDemarcatedSpaces[a].metadata.parkingspaceid = unDemarcatedSpaces[a].parkingzoneid;
                                  unDemarcatedSpaces[a].parkingspaceid = unDemarcatedSpaces[a].parkingzoneid;
                                  unDemarcatedSpaces[a].demarcated = false;
                                  parkingSpaces.push(unDemarcatedSpaces[a]);
                                  default1 = {};
                              }
                          }

                          console.log(" seeing of we have the parking groups", that.state.parkinggroups)
                          var parkinggroups = that.state.parkinggroups

		                      for(var i=0; i < parkinggroups.length;i++){
			                      for(var j = 0 ; j< parkingSpaces.length;j++){

									  if(parkingSpaces[j].parkinggroupid  === "Unknown" || parkingSpaces[j].parkinggroupid  === ""|| parkingSpaces[j].parkinggroupid  === undefined){
										  parkingSpaces[j].parkinggroupname = "No Group";
										  console.log(" NO Group Name parkingSpaces[j].parkinggroupname", parkingSpaces[j].parkinggroupname )
									  }

				                      if(parkingSpaces[j].parkinggroupid === parkinggroups[i].parkinggroupid){
					                      parkingSpaces[j].parkinggroupname = parkinggroups[i].name;
					                      console.log("  parkingSpaces[j].parkinggroupname", parkingSpaces[j].parkinggroupname )
				                      }

			                      }
		                      }

	                      that.setState({parkingspaces:parkingSpaces.map(function(space,index){
                                  space.idx = index;
                                  return space;
                              }),
                              parkingspacesmetadata: metaDataParkingSpaces,
                              defaultMetadataObject,
                              parkingzones
                          })
                      },
                      error : function(jqXHR, status, error){
                          console.log("ajax failure (Space Management): " + status + " - " + error+ " - " + jqXHR.responseJSON.message);
                          $("#loadingmsg").html("Cannot retrieve Parking Spaces.  API reported error: " + error);
                          that.setState({parkingspaces:[],
                              parkingspacesmetadata:[],
                              defaultMetadataObject
                          });
                      }
                  });
              }
          },
          error : function(jqXHR, status, error){
              console.log("ajax failure (parkingzones): " + status + " - " + error);
              $("#loadingmsg").html("Cannot retrieve Parking Zones.  API reported error: " + error);
          }
      });

    // get Min nodes
    $.ajax({
      url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/minnodes',
      data : '',
      method : 'GET',
      "xhrFields": {
         withCredentials: true
      },
      dataType : 'json',
      success : function(data){
        that.setState({
          nodes: data.filter(function(node, index) {
              if(node.model == "merlin" || node.model == "vdkmaster" || node.model == "falcon-q" ){
                  return (typeof node.model != "undefined") && helpers.modelType(node.model) == "Video";
              }
            })
        });
      },
      error : function(){
        console.log("ajax failure");
      }
    });

    // getting Site Info
    $.ajax({
      url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID,
      data : '',
      method : 'GET',
      "xhrFields": {
         withCredentials: true
      },
      dataType : 'json',
      success : function(data){
        if (data.errors) {
          $("#loadingmsg").html("Cannot retrieve site info. " + "/nodes API returned error: " + JSON.stringify(data));
        } else {
          $("#loadingmsg").html("Generating display.");
          that.setState({
            site: data
          })
        };
      },
      error : function(jqXHR, status, error){
        console.log("ajax failure (site): " + status + " - " + error);
        $("#loadingmsg").html("Cannot retrieve Site information.  API reported error: " + error);
      }
    });

      var delayInMilliseconds = 4000; //1 second

      setTimeout(function() {
          //your code to be executed after 1 second
      }, 4000);

  },

  componentDidMount: function() {
    this.init();
    var that = this;

    ReactBootstrap.Dispatcher.on("Parkingspacelist.select",function(parkingspaceID){
      NSN.parkingspaceID = parkingspaceID;
      sessionStorage.setItem("parkingspaceID", NSN.parkingspaceID);
      that.setState({
          parkingspaceID:parkingspaceID,
          selected_spaces: []
      });

    });

    ReactBootstrap.Dispatcher.on('Parkingspacelist.selectSpace', function(parkingspotid) {
          NSN.parkingspaceID = parkingspotid;
          ReactBootstrap.Dispatcher.emit('Parkingspacelist.selectrow', parkingspotid)
          that.setState({
              selected_spaces: [],
              parkingspaceID: parkingspotid
          });
      });

      ReactBootstrap.Dispatcher.on('Parkingspacemap.selectSpace',function(parkingspaceid){
          NSN.parkingspaceID = parkingspaceid;
          ReactBootstrap.Dispatcher.emit('Parkingspacelist.selectrow', parkingspaceid)
          that.setState({
              selected_spaces: [],
              parkingspaceID: parkingspaceid
          });
      });

      ReactBootstrap.Dispatcher.on("Parkingspaceform.save",function(parkingSpaceInfo) {
          console.log('Parkingspaceform.save');
          var newState = {};
          if (parkingSpaceInfo.parkingspaceid == "") {
              var duplicate = false;
              for (var i = 0, len = that.state.parkingspaces.length; i < len; i++) {
                  if ((that.state.parkingspaces[i].name === parkingSpaceInfo.name))
                      duplicate = true;
              }
              if(duplicate){
                  noty({type:"error", text:'Parking Space "' + parkingSpaceInfo.name + '" already exists.'});
              }
          }else{
              var duplicate = false;
              for (var i = 0, len = that.state.parkingspaces.length; i < len; i++) {
                  if (that.state.parkingspaces[i].name === parkingSpaceInfo.name &&
                      that.state.parkingspaces[i].parkingspaceid !== parkingSpaceInfo.parkingspaceid){
                      duplicate = true;
                  }
              }
	          var idx = helpers.get_idx(that.state.parkingspaces, parkingSpaceInfo, 'parkingspaceid');

              if(duplicate){
                  noty({type:"error", text:'Parking Space "' + parkingSpaceInfo.name + '" already exists.'});
                  console.log("parkingSpaceInfo - duplicate is true", parkingSpaceInfo)
                  console.log("parkingSpaceInfo - duplicate is true", parkingSpaceInfo.oldname )
                  parkingSpaceInfo.name = parkingSpaceInfo.oldname;
                  parkingSpaceInfo.metadata.name = parkingSpaceInfo.oldname;

                  console.log(" after getting the old name and not the new name ",parkingSpaceInfo.name ,  parkingSpaceInfo.metadata.name  )
                  console.log(" what is the state",that.state);
	              newState = React.addons.update(that.state, { parkingspaces: { [idx]: { $set: parkingSpaceInfo } }});
	              that.setState(newState);
              }else{
                  // var idx = helpers.get_idx(that.state.parkingspaces, parkingSpaceInfo, 'parkingspaceid');
                  delete parkingSpaceInfo.idx;
                  var parkingspaces = that.state.parkingspaces;
                  var parkingspacesmetadata = that.state.parkingspacesmetadata;
                  var passedData = [];
                  delete parkingSpaceInfo.metadata.createdOn;
                  delete parkingSpaceInfo.metadata.lastUpdated;
                  passedData.push(parkingSpaceInfo.metadata);
	              var argumentStack = [];
	              var isSending = false;
		          argumentStack.push(passedData);
		          trySend();

	              function trySend() {
		              if(!isSending && argumentStack.length > 0) {
			              var passedData = argumentStack.pop();
			              isSending = true;

			              $.ajax({
				              url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/parkingspaces/spaceattributes',
				              "type" : "PUT",
				              "xhrFields": {
					              withCredentials: true
				              },
				              "data" : JSON.stringify(passedData),
				              "dataType" : "json",
				              "contentType" : "application/json",
				              "success" : function(data) {
					              noty({type:"success", text:'Parking Space "' + passedData[0].name + '" updated.'})
					              
					              isSending = false;
					              setTimeout(trySend, 0);
				              }.bind(that),
				              "complete": function (data) {

					              if(data.status == 200){
						              delete passedData[0].metadata;
						              passedData[0].parkingspaceid = passedData[0].parkingspaceid;
						              passedData[0].idx = idx;
						              for(var a in parkingspacesmetadata){
							              if(parkingspacesmetadata[a].parkingspaceid === passedData[0].parkingspaceid){
								              parkingspacesmetadata[a] = passedData[0];
							              }
						              }

						              var idTobeUpdated = passedData[0].parkingspaceid;
						              var parkingSpaceToBeUpdated;
						              for(var j in parkingspaces){
							              if(parkingspaces[j].parkingspaceid == idTobeUpdated){
								              parkingspaces[j].metadata = passedData[0];
								              parkingspaces[j].name = passedData[0].name;
								              parkingspaces[j].idx = idx;
								              parkingSpaceToBeUpdated = parkingspaces[j]
							              }
						              }
						              ReactBootstrap.Dispatcher.emit('Parkingspaceform.update.success',parkingSpaceToBeUpdated );
						              newState = React.addons.update(that.state, { parkingspaces: { [idx]: { $set: parkingSpaceToBeUpdated } }});
						              that.setState(newState);
					              }

				              }.bind(that),
				              "error" : function() {
					              console.log("error", error)
					              noty({type:"error", text:'Could not update Parking Space.'});
					              isSending = false;
					              setTimeout(trySend, 0);
				              }

			              })

		              }
	              }


              }

          }
      });

      ReactBootstrap.Dispatcher.on("Parkingspacemultidetaileditform.save",function(parkingSpaces) {
          var newState = {};
          var allParkingSpaces = that.state.parkingspaces;
          var parkingspacesmetadata = that.state.parkingspacesmetadata;
          var passedData = [];
          var indices =[];
          for(var k = 0 ; k < parkingSpaces.length; k++){
                 var item={};
                 item.idx = helpers.get_idx(allParkingSpaces, parkingSpaces[k], 'parkingspaceid');
                 item.parkingspaceid = parkingSpaces[k].parkingspaceid;
                 item.metadata = parkingSpaces[k].metadata;
                 item.data = parkingSpaces[k];
                 indices.push(item);
                 delete parkingSpaces[k].idx;
                 passedData.push(parkingSpaces[k].metadata);
          }

          var duplicate = false;
          var duplicateName="";
          for (var i = 0, len = that.state.parkingspaces.length; i < len; i++) {
              for( var j = 0 , leng = parkingSpaces.length; j< leng; j++){
                  if (that.state.parkingspaces[i].name === parkingSpaces[j].name &&
                      that.state.parkingspaces[i].parkingspaceid !== parkingSpaces[j].parkingspaceid){
                      duplicate = true;
                      duplicateName = parkingSpaces[j].name;
                  }
              }
          }

          if(duplicate){
              noty({type:"error", text:'Parking Space "' + duplicateName + '" already exists.'});
          }else{

              $.ajax({
                  url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/parkingspaces/spaceattributes',
                  "type" : "PUT",
                  "xhrFields": {
                      withCredentials: true
                  },
                  "data" : JSON.stringify(passedData),
                  "dataType" : "json",
                  "contentType" : "application/json",
                  "success" : function(data) {
                      noty({type:"success", text:'Parking Spaces updated.'})
                  }.bind(that),
                  "complete": function (data) {
                      if(data.status == 200){
                          for(var a in parkingspacesmetadata){
                              for(var b in indices){
                                  if(parkingspacesmetadata[a].parkingspaceid === indices[b].parkingspaceid){
                                      parkingspacesmetadata[a] = indices[b].metadata;
                                  }

                              }
                          }

                          var selectedRowIndices = [], data = [];
                          for(var i = 0; i < allParkingSpaces.length ; i ++){
                              for(var j = 0; j<indices.length; j++){
                                  do{
                                      if(allParkingSpaces[i].parkingspaceid == indices[j].parkingspaceid){
                                          allParkingSpaces[i].metadata = indices[j].metadata;
                                          allParkingSpaces[i].name = indices[j].metadata.name;
                                          allParkingSpaces[i].idx = indices[j].idx;
                                          selectedRowIndices.push(indices[j].idx)
                                          data.push(indices[j].data)
                                      }
                                  }while(j == indices.length);
                              }
                          }
                          ReactBootstrap.Dispatcher.emit("Parkingspacemultidetaileditform.selectedRows.update",selectedRowIndices)
                          for(var j in indices){
                              newState = React.addons.update(this.state, { parkingspaces: { [indices[j]]: { $set: indices[j].data } }});
                          }
                          this.setState(newState);
                      }

                  }.bind(that),
                  "error" : function() {
                      noty({type:"error", text:'Could not update Parking Space.'});
                  }

              })
          }
      });

      ReactBootstrap.Dispatcher.on("Parkingspacemap.renameSpaces", function(spacesToBeRenamed){
          var newState = {};
          var allParkingSpaces = that.state.parkingspaces;
          var parkingspacesmetadata = that.state.parkingspacesmetadata;
          var passedData = [];
          var indices =[];
          for(var k = 0 ; k < spacesToBeRenamed.length; k++){
              var item={};
              item.idx = helpers.get_idx(allParkingSpaces, spacesToBeRenamed[k], 'parkingspaceid');
              item.parkingspaceid = spacesToBeRenamed[k].parkingspaceid;
              item.metadata = spacesToBeRenamed[k].metadata;
              spacesToBeRenamed[k].metadata.parkingspaceid = spacesToBeRenamed[k].parkingspaceid
              item.data = spacesToBeRenamed[k];
              indices.push(item);
              delete spacesToBeRenamed[k].idx;
              passedData.push(spacesToBeRenamed[k].metadata);
          }

          var duplicate = false;
          var duplicateName="";
              for (var i = 0, len = that.state.parkingspaces.length; i < len; i++) {
              for( var j = 0 , leng = spacesToBeRenamed.length; j< leng; j++){
                  // We are comparing the parkingspace entirely not just the metadata - so the id should be parkingspotid and not parkingspaceid
                  if (that.state.parkingspaces[i].name === spacesToBeRenamed[j].name &&
                      that.state.parkingspaces[i].parkingspaceid !== spacesToBeRenamed[j].parkingspaceid ){
                      duplicate = true;
                      duplicateName = spacesToBeRenamed[j].name;
                  }
              }
          }

          if(duplicate){
              noty({type:"error", text:'Parking Space "' + duplicateName + '" already exists.'});
          }else{
              $.ajax({
                  url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/parkingspaces/spaceattributes',
                  "type" : "PUT",
                  "xhrFields": {
                      withCredentials: true
                  },
                  "data" : JSON.stringify(passedData),
                  "dataType" : "json",
                  "contentType" : "application/json",
                  "success" : function(data) {
                      noty({type:"success", text:'Parking Spaces updated.'})
                  }.bind(that),
                  "complete": function (data) {
                      if(data.status == 200){
                          for(var a in parkingspacesmetadata){
                              for(var b in indices){
                                  if(parkingspacesmetadata[a].parkingspaceid === indices[b].parkingspaceid){
                                      parkingspacesmetadata[a] = indices[b].metadata;
                                  }
                              }
                          }

                          var selectedRowIndices = [], data = [];
                          for(var i = 0; i < allParkingSpaces.length ; i ++){
                              for(var j = 0; j<indices.length; j++){
                                  do{
                                      if(allParkingSpaces[i].parkingspaceid == indices[j].parkingspaceid){
                                          allParkingSpaces[i].metadata = indices[j].metadata;
                                          allParkingSpaces[i].name = indices[j].metadata.name;
                                          allParkingSpaces[i].idx = indices[j].idx;
                                          selectedRowIndices.push(indices[j].idx)
                                          data.push(indices[j].data)
                                      }
                                  }while(j == indices.length);
                              }
                          }
                          ReactBootstrap.Dispatcher.emit("Parkingspacemap.selectedRows.update",selectedRowIndices)
                          for(var j in indices){
                              newState = React.addons.update(this.state, { parkingspaces: { [indices[j]]: { $set: indices[j].data } }});

                          }
                          newState.selected_spaces=[];
	                      this.setState(newState);
                      }
                  }.bind(that),
                  "error" : function() {
                      noty({type:"error", text:'Could not update Parking Spaces.'});
                  }

              })
          }

      });

      ReactBootstrap.Dispatcher.on("Parkingspacelist.multiSelect", function (parkingspaceids) {
          /*
            Emitted when multiple nodes are selected on the commissioning page
            (Hold Swift to select multiple nodes)
           */
          that.setState({
              selected_spaces: parkingspaceids
          });
      });

      ReactBootstrap.Dispatcher.on('Parkingspacemap.multiSelect',function(parkingSpaceIds){
          that.setState({
              selected_spaces: parkingSpaceIds,
          });
      });

      ReactBootstrap.Dispatcher.on("Parkingspacemap.cancelRenameSpaces", function(selectedSpaces){
          that.setState({
              selected_spaces: selectedSpaces,
          });
          that.hideDetail();
      });

      ReactBootstrap.Dispatcher.on('Parkingspacemultirenameform.cancelRenameSpaces', function(selectedSpaces){
          that.setState({
              selected_spaces: selectedSpaces,
          });
          that.hideDetail();
      });

      ReactBootstrap.Dispatcher.on('Parkingspacemultidetailform.cancelRenameSpaces', function(selectedSpaces){
          that.setState({
              selected_spaces: selectedSpaces,
          });
          that.hideDetail();
      });


      ReactBootstrap.Dispatcher.on('Parkingspacelist.toggleDetail', function () {
        if (that.state.ui.detail == "hidden") {
            that.showDetail();
        } else {
            that.hideDetail();
        }
      });

      ReactBootstrap.Dispatcher.on('Parkingspaceform.toggleDetail', function () {
          if (that.state.ui.detail == "hidden") {
              that.showDetail();
          } else {
              that.hideDetail();
          }
      });


      ReactBootstrap.Dispatcher.on('ParkingspaceDetail.togglePin', function () {
        if (that.state.ui.detail == "pinned") {
            that.unpinDetail();
        } else {
            that.pinDetail();
        }
    });
  },

  hideAttributeDetail() {
        this.setState({
            showAttributeDetail: false
        })
    },

  componentDidUpdate: function(prevProps, prevState) {
    if (this.state.ui.detail != prevState.ui.detail) {
        $(window).trigger('resize');
    };
  },

  componentWillUnmount: function() {
    ReactBootstrap.Dispatcher.removeAllListeners("Parkingspacelist.select");
    ReactBootstrap.Dispatcher.removeAllListeners("Parkingspacelist.toggleDetail");
    ReactBootstrap.Dispatcher.removeAllListeners("ParkingspaceDetail.togglePin");
    ReactBootstrap.Dispatcher.removeAllListeners("Parkingspacelist.selectSpace");
    ReactBootstrap.Dispatcher.removeAllListeners("Parkingspacelist.multiSelect");
  },

  render() {

      var that = this;

      var hstyle = {height: helpers.calcHeight(90, 0)+"px !important"};
    if (that.state.nodes && that.state.site && that.state.parkingspaces && that.state.parkinggroups) {

        var Detail;
        if (this.state.selected_spaces.length > 0) {
            Detail = (
                <Parkingspacemultidetail show={this.state.showAttributeDetail} selected_spaces={this.state.selected_spaces}
                                         hide={this.hideAttributeDetail} detail_state={that.state.ui.detail}
                                         parkingspaceID={that.state.parkingspaceID} parkinggroups={that.state.parkinggroups}
                                         parkingspaces={that.state.parkingspaces}
                                         parkingspacesmetadata={this.state.parkingspacesmetadata}
                                         defaultMetadataObject={this.state.defaultMetadataObject}
                                         parkinggroups = {this.state.parkinggroups}/>
            );
        } else {
            Detail = (
                <ParkingspaceDetail show={this.state.showAttributeDetail}
                                    hide={this.hideAttributeDetail} detail_state={that.state.ui.detail}
                                    parkingspaceID={that.state.parkingspaceID} parkinggroups={that.state.parkinggroups}
                                    activeFilter={that.state.activeFilter} parkingspaces={that.state.parkingspaces}
                                    parkingspacesmetadata={this.state.parkingspacesmetadata}
                                    defaultMetadataObject={this.state.defaultMetadataObject}
                                    parkinggroups = {this.state.parkinggroups}/>
            );
        };



      var listStyle = {width:"33%",height:"100%",overflow:"hidden",backgroundColor:"#FFF",position:"absolute",zIndex:"300",left:"0px",top:"0px",boxShadow:"2px 0px 2px 0px #CCC"};
      switch (that.state.ui.detail) {
          case "unpinned": 
              var detailStyle = {opacity:"0.9",position:"absolute",zIndex:"500",backgroundColor:"#FFF",overflowY:"hidden",overflowX:"hidden",border:"1px solid #EEE"};
              $.extend(detailStyle, this.detailUnpinnedPosition);
              var mapStyle = {width:"67%",height:"100%",position:"absolute",top:"0px",left:"33%",zIndex:"100"};
              $("#parkingspace-detail-panel").draggable("enable").resizable("enable");
              break;
          case "pinned":
              detailStyle = {position:"absolute", zIndex:"200", backgroundColor:"#FFF", left:"33%", width:"25%", height:"100%"};
              mapStyle = {width:"42%",height:"100%",position:"absolute",top:"0px",left:"58%",zIndex:"100"};
              $("#parkingspace-detail-panel").draggable("disable").resizable("disable");
              break;
          case "hidden":
              detailStyle = {position:"absolute", left:"-3000px"};
              mapStyle = {width:"67%",height:"100%",position:"absolute",top:"0px",left:"33%",zIndex:"100"}; 
              break;
      };
      
      return (
        <Container id='body' className="parkingspace-body" style={{backgroundColor:"#FFF",marginTop:"0px !important"}}>
          <div id="parkingspace-list-panel" data-state="closed" style={listStyle}>
               <Parkingspacelist minmax="max"
                                 show={this.state.showAttributeDetail}
                                 detail_state={that.state.ui.detail}
                                 parkingspaceID={that.state.parkingspaceID}
                                 parkinggroups={that.state.parkinggroups}
                                 activeFilter={that.state.activeFilter}
                                 parkingspaces={that.state.parkingspaces}
                                 parkingspacesmetadata={this.state.parkingspacesmetadata}
                                 selected_spaces={this.state.selected_spaces}
                                    />
          </div>
          <div id="parkingspace-detail-panel" style={detailStyle}>
              {/* which detail should the data be visible*/}
              {Detail}
          </div>
          <div id="parkingspace-map-panel" data-state="closed" style={mapStyle}>
                <Parkingspacemap minmax="max" nodes={that.state.nodes}
                                 parkinggroups={that.state.parkinggroups}
                                 parkingspaces={that.state.parkingspaces}
                                 parkingspacesmetadata={this.state.parkingspacesmetadata}
                                 site={that.state.site}
                                 selected_spaces={this.state.selected_spaces}
                                 parkingzones={this.state.parkingzones}
                                 nodes={this.state.nodes}/>
          </div>
        </Container>
      );
    };
    var forSite = (NSN && NSN.siteName && NSN.siteName != '') ? (' for ' + NSN.siteName) : '';
    return (
      <Container id='body'>
        <Grid>
          <Row>
            <Col sm={12}>
              <PanelContainer>
                <Panel>
                  <PanelBody style={hstyle}>
                    <h2 id="loadingmsg" style={{ paddingTop: "16%", textAlign: "center" }}>Loading Space Management information{forSite}.</h2>
                    <div style={{ textAlign: "center" }}><img src="/imgs/loading.gif" /></div>
                  </PanelBody>
                </Panel>
              </PanelContainer>
            </Col>
          </Row>
        </Grid>
      </Container>
    );
  }
});

export default class extends React.Component {
  render() {
    var classes = classNames({
      'container-open': this.props.open
    });

    return (
      <Container id='container' className={classes}>
        <Header />
        <Body />
      </Container>
    );
  }
}
