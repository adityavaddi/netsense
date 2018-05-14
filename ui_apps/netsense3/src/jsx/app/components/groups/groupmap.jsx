import classNames from 'classnames';
import { State, Navigation } from 'react-router';
import auth from 'global/utils/auth';
import helpers from 'global/utils/helpers';

var Groupmap = React.createClass({

  propTypes: {
    nodes: React.PropTypes.array.isRequired,
    readonly: React.PropTypes.bool,
    groupID: React.PropTypes.string.isRequired,
    site: React.PropTypes.object.isRequired
  },

  getDefaultProps: function () { 
    return {
      initialZoom: 16,
      readonly: false,
      map:null,
      mapCenterLat: 37.3807927,  // Sensity HQ
      mapCenterLng: -121.9928375,
    };
  },

  markers: [],
  unmappednodes: [],

  toggleMembership: function(marker){
    marker.setIcon(helpers.genGMarker(marker.nodetype, marker.connected, !marker.highlighted));
    marker.highlighted = !marker.highlighted;
  },

  maximize: function() {
    $("#group-map-panel").data("state","open").css({left:"0px",width:"100%",zIndex:"400"});
    $(window).trigger('resize');
    google.maps.event.trigger(this.map, "resize");
  },

  minimize: function() {
    $("#group-map-panel").data("state","closed").css({left:"33%",width:"67%",zIndex:"100"});
    $(window).trigger('resize');
    google.maps.event.trigger(this.map, "resize");
  },

  togglemap: function() {
    if ($("#group-map-panel").data("state") == "closed") {
      this.maximize();
    } else {
      this.minimize();
    }
  },


  clearAllMarkers: function(){
    for (var i=0; i<this.markers.length; i++) {
      var marker = this.markers[i];
      if (marker.highlighted) {
        marker.highlighted = false;
        marker.setIcon(helpers.genGMarker(marker.nodetype, marker.connected, marker.highlighted));
      }
    }
  },

  markerAction: function (marker) {
    if (this.props.readonly) {
      return;
    };
    if (marker.nodetype != "Lighting" && this.grouptype && this.grouptype == "lighting") {
      if ($("#event").val() == "Click") {
          alert("This is a Video node and cannot be added to a Lighting Group.");
      };
      return;
    };
    switch ($("#action").val()) {
      case "Toggle":
        marker.highlighted = !marker.highlighted;
        if (!marker.highlighted) {
          marker.setIcon(helpers.genGMarker(marker.nodetype, marker.connected, marker.highlighted));
          ReactBootstrap.Dispatcher.emit("Groupmap.removeNode", marker.nodeid)
        } else {
          marker.setIcon(helpers.genGMarker(marker.nodetype, marker.connected, marker.highlighted));
          ReactBootstrap.Dispatcher.emit("Groupmap.addNode", marker.nodeid);
        };
        break;
      case "Include":
        if (!marker.highlighted) {
          marker.highlighted = true;
          marker.setIcon(helpers.genGMarker(marker.nodetype, marker.connected, marker.highlighted));
          ReactBootstrap.Dispatcher.emit("Groupmap.addNode", marker.nodeid)
        };
        break;
      case "Exclude":
        if (marker.highlighted) {
          marker.highlighted = false;
          marker.setIcon(helpers.genGMarker(marker.nodetype, marker.connected, marker.highlighted));
          ReactBootstrap.Dispatcher.emit("Groupmap.removeNode", marker.nodeid)
        };
      }
  },

  componentDidMount: function () {

    if (typeof this.props.site.latitude == "undefined" || this.props.site.latitude == "") {
      this.props.site.latitude = this.mapCenterLat;
      this.props.site.longitude = this.mapCenterLng;
    };

    // create icons
    var svgHTML = '<svg id="svg" width="40" height="40" viewPort="0 0 40 40" version="1.1" xmlns="http://www.w3.org/2000/svg">';

    var svgOnInActiveNode = svgHTML + ('<circle r="15" cx="20" cy="20" fill="#FFF" stroke-width="4" stroke="#008ABF"></circle>'
        + '<circle r="10" cx="20" cy="20" fill="red" stroke="none" stroke-width="0"></circle></svg>');  

    var svgOnActiveNode = svgHTML + ('<circle r="15" cx="20" cy="20" fill="#FFF" stroke-width="4" stroke="#008ABF"></circle>'
        + '<circle r="10" cx="20" cy="20" fill="#008ABF" fstroke="none" stroke-width="0"></circle></svg>');       

    var svgOffActiveNode = svgHTML + ('<circle r="15" cx="20" opacity="0.3" cy="20" fill="#FFF" stroke-width="4" stroke="#008ABF"></circle>'
        + '<circle r="10" cx="20" cy="20" opacity="0.3" fill="#008ABF" fstroke="none" stroke-width="0"></circle></svg>');          

    var svgOffInActiveNode = svgHTML + ('<circle r="15" cx="20" opacity="0.3" cy="20" fill="#FFF" stroke-width="4" stroke="#008ABF"></circle>'
        + '<circle r="10" cx="20" cy="20" fill="red" opacity="0.3" fstroke="none" stroke-width="0"></circle></svg>');     
    

    this.iconOnInActiveNode = {
      url: 'data:image/svg+xml;charset=UTF-8;base64,' + btoa(svgOnInActiveNode),
      size: new google.maps.Size(40, 40),
      origin: new google.maps.Point(0,0), // origin
      anchor: new google.maps.Point(20, 20), // anchor
    }

     this.iconOnActiveNode = {
      url: 'data:image/svg+xml;charset=UTF-8;base64,' + btoa(svgOnActiveNode),
      size: new google.maps.Size(40, 40),
      origin: new google.maps.Point(0,0), // origin
      anchor: new google.maps.Point(20, 20), // anchor
    }

     this.iconOffActiveNode = {
      url: 'data:image/svg+xml;charset=UTF-8;base64,' + btoa(svgOffActiveNode), 
      size: new google.maps.Size(40, 40),
      origin: new google.maps.Point(0,0), // origin
      anchor: new google.maps.Point(20, 20), // anchor
    }

    this.iconOffInActiveNode = {
      url: 'data:image/svg+xml;charset=UTF-8;base64,' + btoa(svgOffInActiveNode),
      size: new google.maps.Size(40, 40),
      origin: new google.maps.Point(0,0), // origin
      anchor: new google.maps.Point(20, 20), // anchor
    }

    this.multiLevel = false;
    this.multiType = false;
    this.currentType = "all";
    this.currentLevel = "all";
    this.levelselect = "";
    for (var j = 0, levelhash = {}, typehash = {}; j < this.props.nodes.length; j++) {
      var node = this.props.nodes[j];
      if (typeof levelhash[node.level] == "undefined") {
        levelhash[node.level] = 1;
      } else {
        levelhash[node.level]++;
      }
      var type = helpers.modelType(node.model);
      if (typeof typehash[type] == "undefined") {
        typehash[type] = 1;
      } else {
        typehash[type]++;
      }
    };
    this.levels = Object.keys(levelhash).sort();
    this.types = Object.keys(typehash).sort();
    if (this.levels.length > 1) {
      this.multiLevel = true;
      this.levelselect = "<option value='all' selected>All (" + this.props.nodes.length + ")</option>";
      for (j = 0; j < this.levels.length; j++) {
        this.levelselect += "<option value='" + this.levels[j] + "'>" + this.levels[j] + " (" + levelhash[this.levels[j]] + ")</option>"
      }
      $("#levelselect").append(this.levelselect);
    };
    if (this.types.length > 1) {
      this.multiType = true;
      this.typeselect = "<option value='all' selected>All (" + this.props.nodes.length + ")</option>";
      for (j = 0; j < this.types.length; j++) {
        this.typeselect += "<option value='" + this.types[j] + "'>" + this.types[j] + " (" + typehash[this.types[j]] + ")</option>"
      }
      $("#typeselect").append(this.typeselect);
    };

    var nightMap = new google.maps.StyledMapType(helpers.getNightMap(), {name: "Night"});

    var mapOptions = {
      center: this.mapCenterLatLng(),
      zoom: this.props.initialZoom,
      mapTypeControlOptions: {
        mapTypeIds: ['roadmap', 'night', 'satellite']
      },
      mapTypeId: localStorage.getItem('groupMapType') === null
                    ?google.maps.MapTypeId.ROADMAP
                    :localStorage.getItem('groupMapType'),

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
    this.map = new google.maps.Map($("#groupmap").get(0), mapOptions);
    this.map.mapTypes.set('night', nightMap);

    var that = this;

    google.maps.event.addListener( this.map, 'maptypeid_changed', function() { 
      localStorage.setItem('groupMapType', that.map.getMapTypeId());
      });

    var bounds = new google.maps.LatLngBounds();

    this.markers = [];
    this.unmappednodes = [];
    var latlng;
    for (var i=0; i<this.props.nodes.length; i++){
      var thisnode = this.props.nodes[i];
      if (typeof thisnode.latitude == "undefined"
          || thisnode.latitude == ""
          || !$.isNumeric(thisnode.latitude) 
          || thisnode.latitude < -90
          || thisnode.latitude > 90
          ||  typeof thisnode.longitude == "undefined"
          || thisnode.longitude == ""
          || !$.isNumeric(thisnode.longitude)  
          || thisnode.longitude < -180
          || thisnode.longitude > 180) {
        var n = this.unmappednodes.push(thisnode.nodeid);
        latlng = new google.maps.LatLng(
            parseFloat(this.props.site.latitude) + Math.sin(0.4*n) * 0.000018 * n,
            parseFloat(this.props.site.longitude) + Math.cos(0.4*n) * 0.00002 * n
            );
      } else {
        latlng = new google.maps.LatLng(
            parseFloat(thisnode.latitude), 
            parseFloat(thisnode.longitude)
            );
      };

      var nodetype = helpers.modelType(this.props.nodes[i].model);
      var icon = helpers.genGMarker(nodetype, this.props.nodes[i].net_stat, false);
      var marker = new google.maps.Marker({
        position: latlng, 
        map: this.map,
        icon: icon,
        draggable:false,
        idx: this.props.nodes[i].idx,
        nodeid: this.props.nodes[i].nodeid,
        nodetype: nodetype,
        connected:this.props.nodes[i].net_stat,
        title: this.props.nodes[i].nodeid + " (" + nodetype + ")",
        highlighted:false,
        level: this.props.nodes[i].level,
        visible: (!this.multiLevel || this.props.nodes[i].level == this.levels[0])
      });

      if (auth.allowed('CAN_UPDATE', 'GroupModel')) {
          google.maps.event.addListener(marker, 'mouseover', function() {
            if ($("#event").val() == "Hover") {
              that.markerAction(this);
            }
          });
          google.maps.event.addListener(marker, 'click', function() {
            if ($("#event").val() == "Click") {
              that.markerAction(this);
            }
          });
      };

      this.markers.push(marker);

      bounds.extend(latlng);
      this.map.fitBounds(bounds);
    };
    
    this.initialCenter = this.map.getCenter();
    
    this.markerCluster = new MarkerClusterer(this.map, this.markers,
       {ignoreHidden: true,
        imagePath: '/imgs/markers/m', 
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



    if (this.unmappednodes.length > 0) {
      console.log("***** Nodes with missing lat or lng: " + this.unmappednodes);
      noty({type:"warning", text:this.unmappednodes.length + ' node(s) found with missing or invalid latitude/longitude.<br />'
            + 'These nodes are temporarily displayed near the Site location.<br />'
            + 'Please correct these locations as soon as possible.'})
    }


    var that = this;
    var listener = google.maps.event.addListener(this.map, "idle", function() { 
      if (that.map.getZoom() > 20) that.map.setZoom(20); 
      google.maps.event.removeListener(listener); 
    });
    
    ReactBootstrap.Dispatcher.on('Grouplist.add', function(){
      that.clearAllMarkers();
    });
  
    ReactBootstrap.Dispatcher.on('Groupform.showGroup', function(nodeList, type, groupid){
      // 

      $("#markercontrol").css("visibility",(type=="site-lighting")?"hidden":"visible");
      that.grouptype = type;
      if (nodeList.length == 0) {
        // unhighlight all markers
        for (var i=0; i<that.markers.length; i++){
          if (that.markers[i].highlighted) {
            that.toggleMembership(that.markers[i]);
          }
        }
        that.map.setCenter(that.mapCenterLatLng());
        that.map.setZoom(that.props.initialZoom);
      } else {
        if (groupid != "") {  // empty groupid means user is adding a group;  keep map unchanged
          var bounds = new google.maps.LatLngBounds();
          var done = false;
          for (var j = 0; !done && j<that.markers.length; j++) {
              if (nodeList.indexOf(that.markers[j].nodeid) < 0) {
                if (that.markers[j].highlighted) {
                  that.toggleMembership(that.markers[j]);
                }
              } else {
                bounds.extend(that.markers[j].position);
                if (!that.markers[j].highlighted) {
                  that.toggleMembership(that.markers[j]);
                }
              }
          }
          //that.map.fitBounds(bounds);
          //that.map.setCenter(bounds.getCenter());
          var listener = google.maps.event.addListener(that.map, "idle", function() { 
            if (that.map.getZoom() < 15) that.map.setZoom(16); 
            google.maps.event.removeListener(listener); 
          });
        };
      }
    })         
  },

  mapCenterLatLng: function () {
    if (typeof this.props.site.latitude != "undefined") {
      return new google.maps.LatLng(parseFloat(this.props.site.latitude), parseFloat(this.props.site.longitude));
    };
    return new google.maps.LatLng(this.props.mapCenterLat, this.props.mapCenterLng);
  },

  componentWillUnmount: function() {
    for (var i=0; i<this.markers.length; i++){
      google.maps.event.clearInstanceListeners(this.markers[i]);
      this.markers[i].setMap(null);
    };
    this.markers = [];
    this.unmappednodes = [];
    if (typeof this.source == 'object') {
      this.source.close();
    };
    ReactBootstrap.Dispatcher.removeAllListeners("Grouplist.add");
    ReactBootstrap.Dispatcher.removeAllListeners("Groupform.showGroup");
  },

  shouldComponentUpdate: function(nextProps, nextState) {
    return false; // (this.props.groupID != nextProps.groupID);
  },

  changeLevel: function (level) {
    if (this.currentLevel == level) {
      return;
    }
    for (var i = 0; i < this.markers.length; i++) {
      var m = this.markers[i];
      m.setVisible((level == "all" || m.level == level) && (this.currentType == "all" || m.nodetype == this.currentType));
    };
    this.markerCluster.repaint();
    this.currentLevel = level;
  },

  changeType: function (type) {
    if (this.currentType == type) {
      return;
    }
    for (var i = 0; i < this.markers.length; i++) {
      var m = this.markers[i];
      m.setVisible((type == "all" || m.nodetype == type) && (this.currentLevel == "all" || m.level == this.currentLevel));
    };
    this.markerCluster.repaint();
    this.currentType = type;
  },

  render: function () {
    var that = this;
    var Level = React.createClass({
      handleChangeLevel: function () {
        var elem = React.findDOMNode(this.refs.levelselect);
        that.changeLevel(elem.options[elem.selectedIndex].value);
      },
      handleChangeType: function () {
        var elem = React.findDOMNode(this.refs.typeselect);
        that.changeType(elem.options[elem.selectedIndex].value);
      },
      render: function () {
        return (
          <div style={{ position: "absolute", zIndex: "999999", top: "10px", left: "172px", fontSize: "11px", padding: "3px 8px 4px", borderRadius: "2px", border: "1px solid #CCC", boxShadow: "0px 1px 4px -1px rgba(0, 0, 0, 0.3)", color: "#000", backgroundColor: "#FFF" }}>
            Level: &nbsp;<select ref='levelselect' id='levelselect' style={{ fontSize: "11px" }} onChange={this.handleChangeLevel}>
            </select> &nbsp; 
            Node Type: &nbsp;<select ref='typeselect' id='typeselect' style={{ fontSize: "11px" }} onChange={this.handleChangeType}>
            </select>
          </div>
        )
      }
    });

    var Control = React.createClass({
      render: function(){
        return (
          <div id="markercontrol" style={{position:"absolute",zIndex:"999999",top:"30px",right:"12px",
            padding:"4px 8px",fontSize:"16px",backgroundColor:"rgba(255,255,255,0.85)"}}>
            <span style={{fontSize:"18px"}}>Membership:</span> &nbsp;
              <select ref="action" id="action">
                <option>Toggle</option>
                <option>Include</option>
                <option>Exclude</option>
              </select>
              &nbsp; <span style={{fontSize:"18px"}}>on</span> &nbsp;
              <select ref="event" id="event">
                <option>Click</option>
                <option>Hover</option>
              </select>
          </div>
          )
      }
    });

    var Minmax = (<div></div>);
      var glyph = "icon-fontello-resize-horizontal";
      var handler = this.togglemap;
      Minmax = (
        <div style={{position:"absolute",cursor:"pointer",top:"-12px",right:"16px",zIndex:"101",color:"#000000",height:"20px",fontSize:"36px"}}
              onClick={handler} title="Toggle Full-Screen Map">
          <Icon bundle={this.props.bundle} glyph={glyph} />
        </div> );
    return (
      <div style={{height:"100%"}}>
        {Minmax}
        <div id="groupmap" className='mapCanvas groupmapCanvas'></div> 
        <Level />
        <Control />
      </div>
    );
  }
});

module.exports = Groupmap;

