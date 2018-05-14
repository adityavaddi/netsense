import classNames from 'classnames';
import { State, Navigation } from 'react-router';
import auth from 'global/utils/auth';
import helpers from 'global/utils/helpers';

var Parkingzonemap = React.createClass({

  propTypes: {
    nodes: React.PropTypes.array.isRequired,
    parkingzones: React.PropTypes.array.isRequired,
    site: React.PropTypes.object.isRequired
  },

  getDefaultProps: function () { 
    return {
      initialZoom: 8,
      readonly: false,
      map:null,
      mapCenterLat: 37.3807927,  // Sensity HQ
      mapCenterLng: -121.9928375,
      zones:[]
    };
  },

  currentStatus: "all",

  maximize: function() {
    $("#parkingzone-map-panel").data("state","open").css({left:"0px",width:"100%",zIndex:"400"});
    $(window).trigger('resize');
    google.maps.event.trigger(this.map, "resize");
  },

  minimize: function() {
    $("#parkingzone-map-panel").data("state","closed").css({left:"33%",width:"67%",zIndex:"100"});
    $(window).trigger('resize');
    google.maps.event.trigger(this.map, "resize");
  },
  
  togglemap: function() {
    if ($("#parkingzone-map-panel").data("state") == "closed") {
      this.maximize();
    } else {
      this.minimize();
    }
  },

  zones: [],

  bearing: function (n, z) {
    var y = Math.sin(z.lng-n.lng) * Math.cos(z.lat);
    var x = Math.cos(n.lat)*Math.sin(z.lat) -
            Math.sin(n.lat)*Math.cos(z.lat)*Math.cos(z.lng-n.lng);
    var b = Math.atan2(y, x)*(180/Math.PI);
    return b<0?(360+b):b;
  },

  buildZoneInfoWindow: function(zone){
    var i = 0, found = false;
    for (var i=0, match=0;  i<this.props.parkingzones.length; i++) {
      if (zone.parkingzoneid == this.props.parkingzones[i].parkingzoneid){
        match = i;
      }
      this.zones[i].setOptions({fillOpacity:0.35});
    };
    
    var parkingzone = this.props.parkingzones[match];

    return ['<table style="margin-bottom:12px;font-size:14px"><tr style="font-size:16px"><td style="text-align:right"><b>Zone ID:</b></td><td><b>' 
          , "<span style='cursor:pointer' title='Center and Zoom' onclick='javascript:ReactBootstrap.Dispatcher.emit("+'"Parkingzonemap.zoomin","'+_.escape(parkingzone.parkingzoneid)+'"' +")'>"+_.escape(parkingzone.parkingzoneid)+"</span></b>"          
          , '</td></tr>'
          , (typeof parkingzone.name != "undefined" && parkingzone.name.length>0)?('<tr><td style="text-align:right"><b>Name:</b></td><td>' + parkingzone.name + '</td></tr>'):""
          , '<tr><td style="text-align:right;font-size:16px"><b>Camera:</b></td><td style="font-size:16px"><b>' , parkingzone.nodeid, '</b></td></tr>'
          , '<tr><td style="font-size:16px"><b>Type:</b></td><td style="font-size:16px"><b>' , parkingzone.type, '</b></td></tr>'
          , '</table><table width="100%"><tr><th style="text-align:center;width:33%">Total</th>'
          , '<th style="text-align:center;width:33%">Available</th>'
          , '<th style="text-align:center;width:33%">Occupied</th></tr>'
          , '<tr><td align="center" style="font-size:30px;color:#666666"><b>' , parkingzone.max_spaces, '</b></td>'
          , '<td align="center" style="font-size:30px;color:#00BB00"><b>' , parkingzone.available_spaces, '</b></td>'
          , '<td align="center" style="font-size:30px;color:#FF3333"><b>' , parkingzone.occupied_spaces, '</b></td>'
          , '</table>'
         ].join("");
  },

  buildCameraInfoWindow: function(marker){
    var totals = [0,0,0];
    for (var i=0, count=0; i<this.props.parkingzones.length; i++) {
      if (this.props.parkingzones[i].nodeid == marker.nodeid) {
        count++;
        totals[0] += this.props.parkingzones[i].max_spaces;
        totals[1] += this.props.parkingzones[i].available_spaces;
        totals[2] += this.props.parkingzones[i].occupied_spaces;
        this.zones[i].setOptions({fillOpacity:1});
      } else {
        this.zones[i].setOptions({fillOpacity:0.35});
      }
    };

    for (i = 0; i<this.props.nodes.length; i++) {
      if (marker.nodeid == this.props.nodes[i].nodeid) {
        var model = this.props.nodes[i].model;
      };
    };

    var s = ['<table style="margin-bottom:12px;font-size:14px"><tr style="font-size:18px"><td style="text-align:right"><b>ID:</b></td><td><b>' , marker.nodeid , '</b></td></tr>'
          , (typeof marker.name != "undefined" && marker.name.length>0)?('<tr><td style="text-align:right"><b>Name:</b></td><td>' + marker.name + '</td></tr>'):""
          , '<tr><td style="text-align:right;font-size:16px"><b>Zones:</b></td><td style="font-size:16px"><b>' , (count==0?"None defined.":count) , '</b></td></tr>'
          , '</table><table width="100%"><tr><th style="text-align:center;width:33%">Total</th>'
          , '<th style="text-align:center;width:33%">Available</th>'
          , '<th style="text-align:center;width:33%">Occupied</th></tr>'
          , '<tr><td align="center" style="font-size:30px;color:#666666"><b>' , (count==0?"-":totals[0]), '</b></td>'
          , '<td align="center" style="font-size:30px;color:#00BB00"><b>' , (count==0?"-":totals[1]), '</b></td>'
          , '<td align="center" style="font-size:30px;color:#FF3333"><b>' , (count==0?"-":totals[2]), '</b></td></tr>'
         ].join("");
    s += '<tr><td colspan="3" style="text-align:center">';
    s += "<button id='viewImage' className='btn btn-info' style='border-color:#333;border-radius:10px' ";
    s += "onclick='javascript:ReactBootstrap.Dispatcher.emit("+'"Parkingzoneform.image", "open", {nodeid:"'+marker.nodeid+'",model:"'+model+'"})' + "'><b>View Image</b></button>";
    s += '</td></tr></table>';
    return s;

  },

  showZoneTooltip: function(zone) {
    var content = this.buildZoneInfoWindow(zone);
    this.infoWindow.setContent(content);
    this.infoWindow.setPosition(zone.center);
    this.infoWindow.open(this.map, zone);
  },

  showCameraTooltip: function(marker) {
    var content = this.buildCameraInfoWindow(marker);
    this.infoWindow.setContent(content);
    this.infoWindow.setPosition(marker.position);
    this.infoWindow.open(this.map, marker);
  },

  getCenter: function (box) {
    var minLat = Math.min.apply(null, box.lat),
      minLng = Math.min.apply(null, box.lon),
      maxLat = Math.max.apply(null, box.lat),
      maxLng = Math.max.apply(null, box.lon);
    return ({lat:minLat + (Math.abs(maxLat - minLat) / 2),
             lng:minLng + (Math.abs(maxLng - minLng) / 2)
           });    
  },

  clearZones: function() {
    for (var i=0; i<this.zones.length; i++) {
      this.zones[i].setOptions({fillOpacity:0.35});
      this.zones[i].highlighted = false;
    }
  },

  updateSpotOccupancy: function(parkingzone, occ) {
    var spots = this.spotlookup[parkingzone.parkingzoneid].spots;
//  This code will correctly draw the occupied spots when a data issue is fixed
/*
    console.log(parkingzone.parkingzoneid+":"+spots.length);
    for (var i = 0; i<spots.length; i++) {
      console.log(parkingzone.spots.length);
      for (var j = 0, found = false; !found && j<occ.length; j++) {
        found = spots[i].spotid == occ[j].uuid;
        console.log("     " + spots[i].spotid + ":" + parkingzone.spots[j].uuid + " (" + found + ")");
      }
      spots[i].setOptions({fillOpacity: found?1:0});
    };
*/
    for (var i=0; i<spots.length; i++) {
      spots[i].setOptions({fillOpacity: (i<occ.length)?1:0});
    }

  },

  componentDidMount: function () {

    if (typeof this.props.site.latitude == "undefined" || this.props.site.latitude == "") {
      this.props.site.latitude = this.mapCenterLat;
      this.props.site.longitude = this.mapCenterLng;
    };

    // one infoWindow object that will be re-used for multiple purposes
    this.infoWindow = new google.maps.InfoWindow();

    // determine if there are multiple building levels and generate ordered array of levels
/*
    this.multiLevel = false;
    this.levelselect= "";
    for (var j=0,hash={}; j<this.props.nodes.length; j++) {
      hash[this.props.nodes[j].level] = 1;
    };
    this.levels = Object.keys(hash).sort();
    if (this.levels.length > 1) {
      this.multiLevel = true;
      this.currentLevel = this.levels[0];
      this.levelselect = "";
      for (j=0; j<this.levels.length; j++){
        this.levelselect += "<option value='" + this.levels[j] + "'>" +  this.levels[j] + "</option>"
      }
      $("#levelselect").append(this.levelselect);
    } else {
      $("#levelcontrol").css({visibility:"hidden"});
    };
*/
    this.polyStyles = {
      member: {fillColor:"#FF0000",strokeColor:"#FF0000"},
      nonmember: {fillColor:"#008ABF", strokeColor:"#008ABF"},
      highlighted: {strokeWeight: 6},
      normal: {strokeWeight: 2}
    };

    var nightMap = new google.maps.StyledMapType(helpers.getNightMap(), {name: "Night"});

    var mapOptions = {
      center: this.mapCenterLatLng(),
      zoom: this.props.initialZoom,
      mapTypeControlOptions: {
        mapTypeIds: ['roadmap', 'night', 'satellite']
        },
      mapTypeId: localStorage.getItem('parkingzoneMapType') === null
                    ?google.maps.MapTypeId.ROADMAP
                    :localStorage.getItem('parkingzoneMapType'),
      styles: [
        {
          featureType: "poi",
          elementType: "labels",
          stylers: [
            { visibility: "off" }
          ]
        }
      ],
      zoomControl: true,
      fullscreenControl: false
    };
    this.map = new google.maps.Map($("#parkingzonemap").get(0), mapOptions);
    this.map.mapTypes.set('night', nightMap);
//    this.map.mapTypes.roadmap.name='Day';

    var that = this;

    google.maps.event.addListener( this.map, 'maptypeid_changed', function() { 
      localStorage.setItem('parkingzoneMapType', that.map.getMapTypeId());
      });

    var bounds = new google.maps.LatLngBounds();
    if (this.props.parkingzones.length == 0) {
      bounds.extend(this.mapCenterLatLng());
    }

    this.zonelookup = {};
    this.spotlookup = {};

    for (i=0; i<this.props.parkingzones.length; i++) {

        // support old and new formats for world_bounding_box
        if ($.isArray(this.props.parkingzones[i].world_bounding_box)) {
          var wbb = {lon: [], lat: []};
          for (var j=0; j<4; j++) {
            wbb.lon[j] = this.props.parkingzones[i].world_bounding_box[j].longitude;
            wbb.lat[j] = this.props.parkingzones[i].world_bounding_box[j].latitude;
          };
          delete this.props.parkingzones[i].world_bounding_box;
          // deep copy
          this.props.parkingzones[i].world_bounding_box = JSON.parse(JSON.stringify(wbb));
        };

        var parkingzone = this.props.parkingzones[i];

        this.zonelookup[parkingzone.parkingzoneid] = i;

        // Define the LatLng coordinates for the polygon's path.
        var zoneCoords = [];
        for (var j=0; j<4; j++) {
          zoneCoords.push({
            lat: parkingzone.world_bounding_box.lat[j],
            lng: parkingzone.world_bounding_box.lon[j]
          });
        };

        // Construct the polygon.
        var pzColor = helpers.parkingColor(100*(parkingzone.occupied_spaces/parkingzone.max_spaces));
        var zone = new google.maps.Polygon({
          idx: i,
          paths: zoneCoords,
          strokeColor: pzColor,
          strokeOpacity: 0.8,
          strokeWeight: 2,
          fillColor: pzColor,
          fillOpacity: 0.35,
          nodeid: parkingzone.nodeid,
          parkingzoneid: parkingzone.parkingzoneid,
          occupied_spaces: parkingzone.occupied_spaces,
          max_spaces: parkingzone.max_spaces,
          center: this.getCenter(parkingzone.world_bounding_box),
          active: parkingzone.active,
          highlighted: false
        });
        zone.setMap(this.map);

        // On click zone
        google.maps.event.addListener(zone,'click', function(event){
          for (var i=0; i<that.zones.length; i++) {
            if (that.zones[i].highlighted) {
              that.zones[i].highlighted = false;
              that.zones[i].setOptions(that.polyStyles.normal);
            }
          }
          this.setOptions(that.polyStyles.highlighted);
/*          var content = that.buildZoneInfoWindow(this);
          that.infoWindow.setContent(content);
          that.infoWindow.setPosition({lat:event.latLng.lat()+.00005, lng:event.latLng.lng()});
          that.infoWindow.open(that.map, this);
*/
          ReactBootstrap.Dispatcher.emit('Parkingzonemap.selectZone', this.parkingzoneid)
        });

        if (parkingzone.type == "Demarcated" && typeof parkingzone.config != "undefined") {

          this.spotlookup[parkingzone.parkingzoneid] = {spots : []};

          for (j=0; j<this.props.parkingzones[i].config.spots.length; j++) {
            var spot = this.props.parkingzones[i].config.spots[j];
            var spotmarker = new google.maps.Circle({
              idx: j,      
              strokeWeight: 2,
              strokeColor: pzColor,
              fillColor: pzColor,
              fillOpacity: 0,
              nodeid: parkingzone.nodeid,
              parkingzoneid: parkingzone.parkingzoneid,
              spotid: spot.uuid,
              center: this.getCenter(spot.world),
              radius:0.9
            });
            this.spotlookup[parkingzone.parkingzoneid].spots.push(spotmarker)
            spotmarker.setMap(this.map);

          };

          this.updateSpotOccupancy(parkingzone, parkingzone.spots);

        };

        this.zones.push(zone);

        bounds.extend(zoneCoords[0]);
        this.map.fitBounds(bounds);
        bounds.extend(zoneCoords[2]);
        this.map.fitBounds(bounds);
    };

    console.log(JSON.stringify(this.zonelookup));

    this.markers = [];
    var rotations = [];
    for (var i=0; i<this.props.nodes.length; i++) {
      // display Video Node
      var latlng = {lat:parseFloat(this.props.nodes[i].latitude), lng:parseFloat(this.props.nodes[i].longitude)};

      var bearings = this.zones.filter(function(z, index) {
        return z.nodeid == that.props.nodes[i].nodeid;
      }).map(function(z, index){
        return that.bearing(latlng, z.center);
      });
      for (var j=0, sum=0; j<bearings.length; j++) {
        sum += bearings[j];
      };
      var rotation = bearings.length==0?0:((sum/bearings.length) - 90);  // icon already points at 90 degrees
      if (rotation>180) rotation -= 360;
      rotations.push({nodeid:this.props.nodes[i].nodeid, rotation:rotation});
      var marker = new google.maps.Marker({
        idx: i,
        position: latlng, 
        map: this.map,
        icon: helpers.genMarker({type:"Video",selected:false,status:"none"}),
        draggable:false,
        optimized: false,
        highlighted: false,
        name: this.props.nodes[i].name,
        nodeid: this.props.nodes[i].nodeid,
        title: this.props.nodes[i].nodeid
      });

      google.maps.event.addListener(marker,'click', function(event){
        for (var i=0; i<that.markers.length; i++) {
          if (that.markers[i].highlighted) {
            that.markers[i].highlighted = false;
          }
        }
        that.showCameraTooltip(this);
      });


      this.markers.push(marker);

      bounds.extend(latlng);
      this.map.fitBounds(bounds);
    };

    this.markerCluster = new MarkerClusterer(this.map, this.markers,
       {imagePath: '/imgs/markers/m', 
        minimumClusterSize: 50, 
        maxZoom: 17,
        styles:[{
          url: "/imgs/markers/cluster1.png",
          height: 50,
          width: 50,
          textSize:18, 
          textColor:"#FFFFFF"
        },{
          url: "/imgs/markers/cluster2.png",
          height: 60,
          width: 60,
          textSize:18,
          textColor:"#FFFFFF"
        },{
          url: "/imgs/markers/cluster4.png",
          height: 80,
          width: 80,
          textSize:20, 
          textColor:"#FFFFFF"
        },{
          url: "/imgs/markers/cluster6.png",
          height: 110,
          width: 110,
          textSize:24, 
          textColor:"#FFFFFF"
        },{
          url: "/imgs/markers/cluster7.png",
          height: 150,
          width: 150,
          textSize:28, 
          textColor:"#FFFFFF"
        }]});

    var that = this;
    var listener = google.maps.event.addListener(this.map, "idle", function() { 
      if (that.map.getZoom() > 20) that.map.setZoom(20); 
/*
      setTimeout(function(){
      for (var i=0; i<rotations.length; i++) {
        $('img[src="/imgs/markers/video-camera.png#' + rotations[i].nodeid + '"]').css({
            transform: 'rotate(' + rotations[i].rotation + 'deg)'
        });
      };
    },5000);
*/
      google.maps.event.removeListener(listener); 
    });      

    ReactBootstrap.Dispatcher.on('Parkingzonelist.select', function(parkingzoneid) {
      for (var i=0; i<that.zones.length; i++) {
        if (that.zones[i].parkingzoneid == parkingzoneid) {
          that.showZoneTooltip(that.zones[i]); 
          that.zones[i].setOptions(that.polyStyles.highlighted);   
          that.zones[i].highlighted = true;      
        } else {
          if (that.zones[i].highlighted) {
            that.zones[i].highlighted = false;
            that.zones[i].setOptions(that.polyStyles.normal);
          }
        }
      }
    });

    ReactBootstrap.Dispatcher.on('Parkingzonemap.zoomin', function (parkingzoneid) {
      var z = that.zones[that.zonelookup[parkingzoneid]];
      that.map.panTo(z.center);
      that.map.setZoom(20); 
    });

    this.changeStatus("active");

  },

  changeStatus: function (status) {
    if (this.currentStatus == status) {
      return;
    }
    this.infoWindow.close();
    for (var i = 0; i < this.zones.length; i++) {
      var m = this.zones[i];
      m.setVisible(status == "all" 
                  || (m.active == "true" && status == "active") 
                  || (m.active == "false" && status == "inactive"));
      if (typeof this.spotlookup[m.parkingzoneid] != "undefined"
            && typeof this.spotlookup[m.parkingzoneid].spots != "undefined") {
        var spots = this.spotlookup[m.parkingzoneid].spots;
        for (var j = 0; j < spots.length; j++) {
          spots[j].setVisible(status == "all" 
                              || (m.active == "true" && status == "active") 
                              || (m.active == "false" && status == "inactive"));
        };
      };
    };
    this.markerCluster.repaint();
    this.currentStatus = status;
    ReactBootstrap.Dispatcher.emit("Parkingzonemap.filter", [{columnId:"active",value:(status=="all")?"":((status=="active")?"true":"false")}]);
  },

  mapCenterLatLng: function () {
    if (this.props.nodes.length == 0) {
      return new google.maps.LatLng(
        parseFloat(this.props.site.latitude),
        parseFloat(this.props.site.longitude));
    }
    return new google.maps.LatLng(
      this.props.mapCenterLat,
      this.props.mapCenterLng);
  },

  componentWillUnmount: function() {
    ReactBootstrap.Dispatcher.removeAllListeners("Parkingzoneform.showNodes");
    ReactBootstrap.Dispatcher.removeAllListeners("Parkingzonelist.select");
    if (typeof NSN.source == 'object') {
      console.log("Closing EventSource...");
      NSN.source.close();
      NSN.source = null;
    };

  },

/*
  changeLevel: function(level){
    if (this.currentLevel == level) {
      return;
    }
    for (var i=0; i<this.markers.length; i++){
      var m = this.markers[i];
      if (m.level == this.currentLevel) {
        m.setVisible(false);
      } else {
        if (m.level == level) {
          m.setVisible(true);
        };
      };
    };
    this.currentLevel = level;
  },
*/

  render: function () {
    var that = this;
    var Level = React.createClass({
      handleChangeLevel: function () {
        var elem = React.findDOMNode(this.refs.levelselect);
        that.changeLevel(elem.options[elem.selectedIndex].value);
      },
      handleChangeStatus: function () {
        var elem = React.findDOMNode(this.refs.statusselect);
        that.changeStatus(elem.options[elem.selectedIndex].value);
      },
      render: function () {
        return (
          <div style={{ position: "absolute", zIndex: "999999", top: "10px", left: "180px", fontSize: "14px", padding: "4px 8px 3px", borderRadius: "2px", border: "1px solid #CCC", boxShadow: "0px 1px 4px -1px rgba(0, 0, 0, 0.3)", color: "#000", backgroundColor: "#FFF" }}>
            {that.multiLevel && (
              <span>
              Level: &nbsp;
              <select ref='levelselect' id='levelselect' style={{ fontSize: "15px" }} onChange={this.handleChangeLevel}>
              </select> &nbsp;
              </span> )
            } 
            Status: &nbsp;<select ref='statusselect' id='statusselect' value={this.currentStatus} style={{ fontSize: "15px" }} onChange={this.handleChangeStatus}>
            <option value="active">Active</option>
            <option value="inactive">Inactive</option>
            <option value="all">All</option>
            </select>
          </div>
        )
      }
    });

    var ToggleZoneMonitor = React.createClass({
      toggleZoneMonitor: function() {
        if (this.refs.toggleZoneMonitor.getDOMNode().checked) {
          if (!!window.EventSource) {
            if (typeof NSN.source == 'object' && NSN.source !== null) {
              NSN.source.close();
              console.log('Parking EventSource closed.');
            };
            that.clearZones();
            var url = NSN.mqttURL + '/' + NSN.customerID + '/' + NSN.siteID + '/+/ParkingEvent';
            NSN.source = new EventSource(url, {withCredentials : true});
            console.log('Parking EventSource requested: ' + url);

            NSN.source.addEventListener('message', function(e) {
              var msg = JSON.parse(e.data);
              var idx = that.zonelookup[msg.parkingzoneid];
              if (typeof idx == "undefined") {
                console.log("Parking Event received for zone " + msg.parkingzoneid + "; not in this site.");
                return;
              }
              if (typeof msg.spots.occ != "undefined") {
                var occupied = msg.spots.occ.spotuuids.length;
                console.log(idx + " : " + occupied + " : " + msg.parkingzoneid);
                if (that.zones[idx].occupied_spaces != occupied) {
                  that.zones[idx].occupied_spaces = occupied;
                  that.props.parkingzones[idx].occupied_spaces = occupied;
                  var pzColor = helpers.parkingColor(100 * occupied / that.zones[idx].max_spaces);
                  that.zones[idx].setOptions({fillColor: pzColor, strokeColor: pzColor});

                  if (that.props.parkingzones[idx].type == "Demarcated") {
                    for (var i = 0; i<that.spotlookup[msg.parkingzoneid].spots.length; i++) {
                      that.spotlookup[msg.parkingzoneid].spots[i].setOptions({fillColor:pzColor, strokeColor: pzColor});
                    }
                    that.updateSpotOccupancy(that.props.parkingzones[idx], msg.spots.occ.spotuuids);
                  };

                }
                that.zones[idx].setOptions({fillOpacity:1.0});
                setTimeout(function(){that.zones[idx].setOptions({fillOpacity:0.35})}, 300);
              };
            });
            NSN.source.onopen = function (e) {
              console.log('Parking EventSource opened: ' + JSON.stringify(e));
            };
            NSN.source.onerror = function (e) {
              console.log('Parking EventSource failed: ' + JSON.stringify(e));
            };  
          } else {
            alert('The Monitor uses Server-Sent Events (SSE). a feature which is not supported in Microsoft Internet Explorer or Microsoft Edge.');
          }
        } else {
          if (typeof NSN.source == 'object' && NSN.source !== null) {
            NSN.source.close();
            console.log('Parking EventSource closed.');
          }
        }
      },
      render: function(){
        return (
          <div style={{position:"absolute",zIndex:"999999",top:"29px",right:"20px",fontSize:"14px",padding:"5px 8px",borderRadius:"2px",border:"1px solid #CCC",boxShadow:"0px 1px 4px -1px rgba(0, 0, 0, 0.3)",color:"#000",backgroundColor:"#FFF"}}>
            Monitor? &nbsp;<input title="Toggle real-time display of zone occupancy changes" ref="toggleZoneMonitor" type="checkbox" style={{height:"16px",width:"16px"}} onChange={this.toggleZoneMonitor} />
          </div>
          );
      }
    });

    var Minmax = (<div></div>);
      var glyph = "icon-fontello-resize-horizontal";
      var handler = this.togglemap;
    Minmax = (
      <div style={{ position: "absolute", cursor: "pointer", top: "-12px", right: "16px", zIndex: "101", color: "#000000", height: "20px", fontSize: "36px" }}
        onClick={handler} title="Toggle Full-Screen Map">
        <Icon bundle={this.props.bundle} glyph={glyph} />
      </div>);

   return (
      <div style={{height:"100%"}}>
         {Minmax}
        <div id="parkingzonemap" className='mapCanvas parkingzonemapCanvas'></div> 
        <ToggleZoneMonitor />
        <Level />
      </div>
    );
  }
});

module.exports = Parkingzonemap;

