import classNames from 'classnames';
import { State, Navigation } from 'react-router';
import auth from 'global/utils/auth';
import helpers from 'global/utils/helpers';

var Parkinggroupmap = React.createClass({

  propTypes: {
    parkinggroups: React.PropTypes.array.isRequired,
    parkingzones: React.PropTypes.array.isRequired,
    nodes: React.PropTypes.array.isRequired,
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

  zones: [],

  toggleMembership: function(zone){
    zone.highlighted = !zone.highlighted;
    if (zone.highlighted) {
      zone.setOptions(this.polyStyles.member);
    } else {
      zone.setOptions(this.polyStyles.nonmember);
    }
  },

  clearAllZones: function(){
    for (var i=0; i<this.zones.length; i++) {
      this.zones[i].highlighted = false;
      this.zones[i].setOptions(this.polyStyles.nonmember);
    }
  },
  
  maximize: function() {
    $("#parkinggroup-map-panel").data("state","open").css({left:"0px",width:"100%",zIndex:"400"});
    $(window).trigger('resize');
    google.maps.event.trigger(this.map, "resize");
  },

  minimize: function() {
    $("#parkinggroup-map-panel").data("state","closed").css({left:"33%",width:"67%",zIndex:"100"});
    $(window).trigger('resize');
    google.maps.event.trigger(this.map, "resize");
  },

  togglemap: function() {
    if ($("#parkinggroup-map-panel").data("state") == "closed") {
      this.maximize();
    } else {
      this.minimize();
    }
  },
  zoneAction: function (zone) {
    zone.highlighted = !zone.highlighted;
    if (!zone.highlighted) {
      zone.setOptions(this.polyStyles.nonmember);
      ReactBootstrap.Dispatcher.emit("Parkinggroupmap.removeZone", zone.parkingzoneid)
    } else {
      zone.setOptions(this.polyStyles.member);
      ReactBootstrap.Dispatcher.emit("Parkinggroupmap.addZone", zone.parkingzoneid);
    };
  },

  // build the content for the tooltip
  buildInfoWindow: function(zone){
    var i = 0, found = false;
    while (!found && i<this.props.parkingzones.length) {
      found = (zone.parkingzoneid == this.props.parkingzones[i].parkingzoneid);
      i++;
    };
    var parkingzone = this.props.parkingzones[--i];

    return ['<table style="font-size:14px"><tr style="font-size:16px"><td style="text-align:right"><b>ID:</b></td><td><b>' , parkingzone.parkingzoneid , '</b></td></tr>'
          , (typeof parkingzone.name != "undefined" && parkingzone.name.length>0)?('<tr><td style="text-align:right"><b>Name:</b></td><td>' + parkingzone.name + '</td></tr>'):""
          , '<tr><td style="text-align:right;font-size:16px">Spaces:</td><td style="font-size:16px">' , parkingzone.max_spaces, '</td></tr>'
          , '</table>'
         ].join("");
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
      member: {fillColor:"#33FF33",strokeColor:"#009900",fillOpacity:1.0},
      nonmember: {fillColor:"#008ABF", strokeColor:"#008ABF",fillOpacity:0.20},
      unassigned: {fillColor:"#999999", strokeColor:"#999999",fillOpacity:0.20},
      mouseover: {strokeWeight: 4},
      mouseout: {strokeWeight: 2}
    };

    var nightMap = new google.maps.StyledMapType([
      {elementType: 'geometry', stylers: [{color: '#242f3e'}]},
      {elementType: 'labels.text.stroke', stylers: [{color: '#242f3e'}]},
      {elementType: 'labels.text.fill', stylers: [{color: '#746855'}]},
      {
        featureType: 'administrative.locality',
        elementType: 'labels.text.fill',
        stylers: [{color: '#d59563'}]
      },
      {
        featureType: "poi",
        elementType: "labels",
        stylers: [{ visibility: "off" }]
      },
      {
        featureType: 'poi.park',
        elementType: 'geometry',
        stylers: [{color: '#263c3f'}]
      },
      {
        featureType: 'landscape.man_made',
        elementType: 'geometry',
        stylers: [{color: '#555555'}]
      },
      {
        featureType: 'poi.park',
        elementType: 'labels.text.fill',
        stylers: [{color: '#6b9a76'}]
      },
      {
        featureType: 'road',
        elementType: 'geometry',
        stylers: [{color: '#666666'}]
      },
      {
        featureType: 'road',
        elementType: 'geometry.stroke',
        stylers: [{color: '#555555'}]
      },
      {
        featureType: 'road',
        elementType: 'labels.text.fill',
        stylers: [{color: '#BBBBBB'}]
      },
      {
        featureType: 'road.highway',
        elementType: 'geometry',
        stylers: [{color: '#746855'}]
      },
      {
        featureType: 'road.highway',
        elementType: 'geometry.stroke',
        stylers: [{color: '#1f2835'}]
      },
      {
        featureType: 'road.highway',
        elementType: 'labels.text.fill',
        stylers: [{color: '#f3d19c'}]
      },
      {
        featureType: 'transit',
        elementType: 'geometry',
        stylers: [{color: '#2f3948'}]
      },
      {
        featureType: 'transit.station',
        elementType: 'labels.text.fill',
        stylers: [{color: '#d59563'}]
      },
      {
        featureType: 'water',
        elementType: 'geometry',
        stylers: [{color: '#17263c'}]
      },
      {
        featureType: 'water',
        elementType: 'labels.text.fill',
        stylers: [{color: '#515c6d'}]
      },
      {
        featureType: 'water',
        elementType: 'labels.text.stroke',
        stylers: [{color: '#17263c'}]
      }
    ],
    {name: "Night"});

    var mapOptions = {
      center: this.mapCenterLatLng(),
      zoom: this.props.initialZoom,
      mapTypeControlOptions: {
        mapTypeIds: ['roadmap', 'night', 'satellite']
        },
      mapTypeId: localStorage.getItem('parkinggroupMapType') === null
                    ?google.maps.MapTypeId.ROADMAP
                    :localStorage.getItem('parkinggroupMapType'),
   // disableDefaultUI: true,
      styles: [
          {
              featureType: "poi",
              elementType: "labels",
              stylers: [
                    { visibility: "off" }
              ]
          }
      ],
      zoomControl: true
    };
    this.map = new google.maps.Map($("#parkinggroupmap").get(0), mapOptions);
    this.map.mapTypes.set('night', nightMap);

    var that = this;

    google.maps.event.addListener( this.map, 'maptypeid_changed', function() { 
      localStorage.setItem('parkinggroupMapType', that.map.getMapTypeId());
      });

    var bounds = new google.maps.LatLngBounds();
    if (this.props.parkingzones.length == 0) {
      bounds.extend(this.mapCenterLatLng());
    }

/*
    var latlng;
    for (var i=0; i<this.props.nodes.length; i++){
      if (typeof this.props.nodes[i].latitude == "undefined"
          || this.props.nodes[i].latitude == ""
          || !$.isNumeric(this.props.nodes[i].latitude) 
          || this.props.nodes[i].latitude < -90
          || this.props.nodes[i].latitude > 90
          ||  typeof this.props.nodes[i].longitude == "undefined"
          || this.props.nodes[i].longitude == ""
          || !$.isNumeric(this.props.nodes[i].longitude)  
          || this.props.nodes[i].longitude < -180
          || this.props.nodes[i].longitude > 180) {
        var n = this.unmappednodes.push(this.props.nodes[i].nodeid);
        latlng = new google.maps.LatLng(
            parseFloat(this.props.site.latitude) + Math.sin(0.4*n) * 0.000018 * n,
            parseFloat(this.props.site.longitude) + Math.cos(0.4*n) * 0.00002 * n
            );
      } else {
        latlng = new google.maps.LatLng(
            parseFloat(this.props.nodes[i].latitude), 
            parseFloat(this.props.nodes[i].longitude)
            )
      };

      var marker = new google.maps.Marker({
        position: latlng, 
        map: this.map,
        icon: this.iconOff,
        draggable:false,
        idx: this.props.nodes[i].idx,
        nodeid: this.props.nodes[i].nodeid,
        title: this.props.nodes[i].nodeid,
        isTrigger:false,
        level: this.props.nodes[i].level,
        visible: (!this.multiLevel || this.props.nodes[i].level == this.levels[0])
      });
*/

    this.zones = [];
    for (var i=0; i<this.props.parkingzones.length; i++) {

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

        // Define the LatLng coordinates for the polygon's path.
        var zoneCoords = [];
        for (var j=0; j<4; j++) {
          zoneCoords.push({
            lat: parkingzone.world_bounding_box.lat[j],
            lng: parkingzone.world_bounding_box.lon[j]
          });
        };

        // Construct the polygon.
        var zone = new google.maps.Polygon({
          paths: zoneCoords,
          strokeColor: this.polyStyles.nonmember.strokeColor,
          strokeOpacity: 0.8,
          strokeWeight: 2,
          fillColor: this.polyStyles.nonmember.fillColor,
          fillOpacity: this.polyStyles.nonmember.fillOpacity,
          highlighted: false,
          parkingzoneid: parkingzone.parkingzoneid
        });
        zone.setMap(this.map);


/*
      if (auth.allowed('CAN_UPDATE', 'ZoneModel')) {
          google.maps.event.addListener(marker, 'mouseover', function() {
            if ($("#event").val() == "Hover") {
              that.markerAction(this.nodeid);
            }
          });
          google.maps.event.addListener(marker, 'click', function() {
            if ($("#event").val() == "Click") {
              that.markerAction(this.nodeid);
            }
          });
      };
      this.nodeidlookup[this.props.nodes[i].nodeid] = i;
*/


/*
      google.maps.event.addListener(zone,"mouseover",function(event){
       this.setOptions(that.polyStyles.mouseover);
       var content = that.buildInfoWindow(this);
       that.infoWindow.setContent(content);
       that.infoWindow.setPosition(event.latLng);
       that.infoWindow.open(that.map, this);
      }); 

      google.maps.event.addListener(zone,"mouseout",function(){
       this.setOptions(that.polyStyles.mouseout);
       that.infoWindow.close();
      });
*/
        // On click zone
      google.maps.event.addListener(zone,'mousedown', function(){
        that.zoneAction(this);
        ReactBootstrap.Dispatcher.emit('Parkinggroupmap.selectZone', this.parkingzoneid)
      });

      this.zones.push(zone);

      bounds.extend(zoneCoords[0]);
      this.map.fitBounds(bounds);
      bounds.extend(zoneCoords[2]);
      this.map.fitBounds(bounds);
    };

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

      this.markers.push(marker);

      bounds.extend(latlng);
      this.map.fitBounds(bounds);
    };

/*
    var markerCluster = new MarkerClusterer(this.map, this.markers,
       {imagePath: '/imgs/markers/m', 
        minimumClusterSize: 20, 
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
*/

/*
    if (this.unmappednodes.length > 0) {
      console.log("***** Nodes with missing lat or lng: " + this.unmappednodes);
      noty({type:"warning", text:this.unmappednodes.length + ' node(s) found with missing or invalid latitude/longitude.<br />'
            + 'These nodes are temporarily displayed near the Site location.<br />'
            + 'Please correct these locations as soon as possible.'})
    }

*/
    var that = this;
    var listener = google.maps.event.addListener(this.map, "idle", function() { 
      if (that.map.getZoom() > 20) that.map.setZoom(20); 
      google.maps.event.removeListener(listener); 
    });

    ReactBootstrap.Dispatcher.on('Parkinggroupform.showGroup', function(zoneList, parkinggroupid){
      if (zoneList.length == 0) {
        // unhighlight all markers
        that.clearAllZones();
        that.map.setCenter(that.mapCenterLatLng());
        that.map.setZoom(that.props.initialZoom);
      } else {
        if (parkinggroupid != "") {  // empty groupid means user is adding a group;  keep map unchanged
          var bounds = new google.maps.LatLngBounds();
          var done = false;
          for (var j = 0; !done && j<that.zones.length; j++) {
              if (zoneList.indexOf(that.zones[j].parkingzoneid) < 0) {
                if (that.zones[j].highlighted) {
                  that.toggleMembership(that.zones[j]);
                }
              } else {
                //bounds.extend(that.zones[j].paths[0]);
                if (!that.zones[j].highlighted) {
                  that.toggleMembership(that.zones[j]);
                }
              }
          }
//          that.map.fitBounds(bounds);
//          that.map.setCenter(bounds.getCenter());
          var listener = google.maps.event.addListener(that.map, "idle", function() { 
            if (that.map.getZoom() < 15) that.map.setZoom(16); 
            google.maps.event.removeListener(listener); 
          });
        };
      }
    });      
/*
    ReactBootstrap.Dispatcher.on('Parkingzoneform.showNodes', function(parkingzoneid){
      that.clearNodes();
      if (parkingzoneid == "") {
        var nodeList = [];
      } else {
        var idx = helpers.get_idx(that.props.parkingzones, {parkingzoneid:parkingzoneid}, 'parkingzoneid');
        nodeList = that.props.parkingzones[idx].nodes.map(function(node, index) { return node.nodeid});
      };
      $("#parkingzonestatusgroups").html("1");
      $("#parkingzonestatusnodes").html(nodeList.length);
      $("#parkingzonestatustriggers").html(that.props.parkingzones[idx].triggers.length);

      var bounds = new google.maps.LatLngBounds();
      for (var i=0; i<nodeList.length; i++) {
        var marker_idx = that.nodeidlookup[nodeList[i]];
        if (that.props.parkingzones[idx].triggers.indexOf(nodeList[i]) >= 0) {
          // trigger marker
          that.markers[marker_idx].setIcon(that.iconTrigger);
          that.markers[marker_idx].isTrigger = true;          
        } else {
          // on marker
          that.markers[marker_idx].setIcon(that.iconOn);
          that.markers[marker_idx].isTrigger = false;           
        }
        bounds.extend(that.markers[marker_idx].position);
      }

      if (parkingzoneid != "") {  // empty groupid means user is adding a group;  keep map unchanged
        that.map.fitBounds(bounds);
        that.map.setCenter(bounds.getCenter());
        var listener = google.maps.event.addListener(that.map, "idle", function() { 
          if (that.map.getZoom() < 15) that.map.setZoom(16); 
          google.maps.event.removeListener(listener); 
        });
    }
    }) 
*/        
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
    ReactBootstrap.Dispatcher.removeAllListeners("Parkinggroupform.showNodes");
  },

  shouldComponentUpdate: function(nextprops, nextstate) {
    return true; // (this.props.nodes != nextprops.nodes)
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

/*
    var that = this;
    var Level = React.createClass({
      handleChangeLevel: function(){
        var elem = React.findDOMNode(this.refs.levelselect);
        that.changeLevel(elem.options[elem.selectedIndex].value);
      },
      render: function(){
        return (
            <div id="levelcontrol" style={{position:"absolute",zIndex:"999999",top:"30px",left:"136px",
              fontSize:"16px",padding:"4px 8px",backgroundColor:"rgba(255,255,255,0.85)"}}>
               <span style={{fontSize:"18px"}}>Level:</span> &nbsp;
               <select ref='levelselect' id='levelselect' onChange={this.handleChangeLevel}>
               </select>
            </div>
          )
      }
    });
*/
    var Minmax = (<div></div>);
      var glyph = "icon-fontello-resize-horizontal";
      var handler = this.togglemap;
      Minmax = (
        <div style={{position:"absolute",cursor:"pointer",top:"-12px",right:"45px",zIndex:"101",color:"#000000",height:"20px",fontSize:"36px"}}
              onClick={handler} title="Toggle Full-Screen Map">
          <Icon bundle={this.props.bundle} glyph={glyph} />
        </div> );
   return (
      <div style={{height:"100%"}}>
        {Minmax}
        <div id="parkinggroupmap" className='mapCanvas parkinggroupmapCanvas'></div> 
      </div>
    );
  }
});

module.exports = Parkinggroupmap;