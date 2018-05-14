import classNames from 'classnames';
import { State, Navigation } from 'react-router';
import auth from 'global/utils/auth';
import helpers from 'global/utils/helpers';

var Daylightmap = React.createClass({

  propTypes: {
    nodes: React.PropTypes.array.isRequired,
    daylights: React.PropTypes.array.isRequired,
    site: React.PropTypes.object.isRequired
  },

  getDefaultProps: function () { 
    return {
      initialZoom: 8,
      readonly: false,
      map:null,
      mapCenterLat: 37.3807927,  // Sensity HQ
      mapCenterLng: -121.9928375,
    };
  },

  markers: [],
  unmappednodes: [],
  nodeidlookup: {},

  getNodes: function() {
    // return all nodes controlled by a DH profile
    var nodes = [];
    if (this.props.dayLightID != "0" || this.props.daylightID != "-1") {
      for (var i=0; i<this.props.groups.length; i++) {
        var group = this.props.groups[i];
        if (group.etdhprofileid == this.props.daylightID) {
          nodes = nodes.concat(group.nodes);
        };
      };
    };
    return nodes;
  },

  clearNodes: function(){
    for (var i = 0; i<this.markers.length; i++) {
      this.markers[i].isTrigger = false;
      if (this.markers[i].getIcon().state != "off") {
        this.markers[i].setIcon(this.iconOff);
      };
    };
  },

  toggleTrigger: function(marker){
    if (marker.isTrigger) {
      $("#daylightstatustriggers").html(parseInt($("#daylightstatustriggers").html()) - 1);
      marker.setIcon(this.iconOn);
    } else {
      $("#daylightstatustriggers").html(1 + parseInt($("#daylightstatustriggers").html()));
      marker.setIcon(this.iconTrigger);
    }
    marker.isTrigger = !marker.isTrigger;
  },

  markerAction: function (nodeid) {
    var idx = this.nodeidlookup[nodeid];
    if (this.props.readonly || this.markers[idx].getIcon().state == "off") {
      return;
    };
    switch ($("#action").val()) {
      case "Toggle":
        this.markers[idx].isTrigger = !this.markers[idx].isTrigger;
        if (!this.markers[idx].isTrigger) {
          this.markers[idx].setIcon(this.iconOn);
          $("#daylightstatustriggers").html(parseInt($("#daylightstatustriggers").html()) - 1);
//          ReactBootstrap.Dispatcher.emit("Daylightmap.removeNode", nodeid)
        } else {
          this.markers[idx].setIcon(this.iconTrigger);
          $("#daylightstatustriggers").html(1 + parseInt($("#daylightstatustriggers").html()));
//          ReactBootstrap.Dispatcher.emit("Daylightmap.addNode", nodeid);
        };
        break;
      case "Include":
        if (!this.markers[idx].isTrigger) {
          this.markers[idx].isTrigger = true;
          this.markers[idx].setIcon(this.iconTrigger);
          $("#daylightstatustriggers").html(1 + parseInt($("#daylightstatustriggers").html()));
          ReactBootstrap.Dispatcher.emit("Daylightmap.addNode", nodeid)
        };
        break;
      case "Exclude":
        if (this.markers[idx].isTrigger) {
          this.markers[idx].isTrigger = false;
          $("#daylightstatustriggers").html(parseInt($("#daylightstatustriggers").html()) - 1);
          this.markers[idx].setIcon(this.iconOn);
          ReactBootstrap.Dispatcher.emit("Daylightmap.removeNode", nodeid)
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
    var svgOn = svgHTML + ('<circle r="15" cx="20" cy="20" fill="#FFF" stroke-width="4" stroke="#008ABF"></circle>'
         + '<circle r="10" cx="20" cy="20" fill="#008ABF" stroke="none" stroke-width="0"></circle></svg>');
    var svgOff = svgHTML + ('<circle r="15" cx="20" cy="20" fill="#FFF" opacity="0.4" stroke-width="4" stroke="#008ABF"></circle>'
         + '<circle r="10" cx="20" cy="20" fill="#008ABF" opacity="0.3" stroke="none" stroke-width="0"></circle></svg>');
    var svgTrigger = svgHTML + ('<circle r="15" cx="20" cy="20" fill="#FFF" stroke-width="4" stroke="#008ABF"></circle>'
         + '<circle r="10" cx="20" cy="20" fill="#FF0" stroke="none" stroke-width="0"></circle>'
         + '<text class="txt" x="50%" y="55%" text-anchor="middle" dominant-baseline="central" font-weight="bold" font-family="NHaasGroteskDSPro-75Bd" font-size="20" stroke="none" '
         + 'fill="#666">T</text>'
         + '</svg>');

    this.iconOn = {
      url: 'data:image/svg+xml;charset=UTF-8;base64,' + btoa(svgOn),
      size: new google.maps.Size(40, 40),
      origin: new google.maps.Point(0,0), // origin
      anchor: new google.maps.Point(20, 20), // anchor
      state: "on"
    }
    this.iconOff = {
      url: 'data:image/svg+xml;charset=UTF-8;base64,' + btoa(svgOff),
      size: new google.maps.Size(40, 40),
      origin: new google.maps.Point(0,0), // origin
      anchor: new google.maps.Point(20, 20), // anchor
      state: "off"
    }
    this.iconTrigger = {
      url: 'data:image/svg+xml;charset=UTF-8;base64,' + btoa(svgTrigger),
      size: new google.maps.Size(40, 40),
      origin: new google.maps.Point(0,0), // origin
      anchor: new google.maps.Point(20, 20), // anchor
      state: "trigger"
    }


    // determine if there are multiple building levels and generate ordered array of levels
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

    var mapOptions = {
      center: this.mapCenterLatLng(),
      zoom: this.props.initialZoom,
      mapTypeId: localStorage.getItem('daylightMapType') === null
                    ?google.maps.MapTypeId.ROADMAP
                    :localStorage.getItem('daylightMapType'),

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
    this.map = new google.maps.Map(this.getDOMNode().childNodes[0], mapOptions);

    var that = this;

    google.maps.event.addListener( this.map, 'maptypeid_changed', function() { 
      localStorage.setItem('daylightMapType', that.map.getMapTypeId());
      });

    var bounds = new google.maps.LatLngBounds();

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
        icon: this.iconTrigger,
        draggable:false,
        idx: this.props.nodes[i].idx,
        nodeid: this.props.nodes[i].nodeid,
        title: this.props.nodes[i].nodeid,
        isTrigger:false,
        level: this.props.nodes[i].level,
        visible: (!this.multiLevel || this.props.nodes[i].level == this.levels[0])
      });

      if (auth.allowed('CAN_UPDATE', 'GroupModel')) {
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
      this.markers.push(marker);

      bounds.extend(latlng);
      this.map.fitBounds(bounds);
    };

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
    

    ReactBootstrap.Dispatcher.on('Daylightform.showNodes', function(etdhprofileid){
      that.clearNodes();
      if (etdhprofileid == "") {
        var nodeList = [];
      } else {
        var idx = helpers.get_idx(that.props.daylights, {etdhprofileid:etdhprofileid}, 'etdhprofileid');
        nodeList = that.props.daylights[idx].nodes.map(function(node, index) { return node.nodeid});
      };
      $("#daylightstatusgroups").html("1");
      $("#daylightstatusnodes").html(nodeList.length);
      $("#daylightstatustriggers").html(that.props.daylights[idx].triggers.length);

      var bounds = new google.maps.LatLngBounds();
      for (var i=0; i<nodeList.length; i++) {
        var marker_idx = that.nodeidlookup[nodeList[i]];
        if (that.props.daylights[idx].triggers.indexOf(nodeList[i]) >= 0) {
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

      if (etdhprofileid != "") {  // empty groupid means user is adding a group;  keep map unchanged
        that.map.fitBounds(bounds);
        that.map.setCenter(bounds.getCenter());
        var listener = google.maps.event.addListener(that.map, "idle", function() { 
          if (that.map.getZoom() < 15) that.map.setZoom(16); 
          google.maps.event.removeListener(listener); 
        });
    }
    })         
  },

  mapCenterLatLng: function () {
    if (this.props.nodes.length == 0) {
      return new google.maps.LatLng(parseFloat(this.props.site.latitude), parseFloat(this.props.site.longitude));
    }
    return new google.maps.LatLng(this.props.mapCenterLat, this.props.mapCenterLng);
  },

  componentWillUnmount: function() {
    ReactBootstrap.Dispatcher.removeAllListeners("Daylightform.showNodes");
    for (var i=0; i<this.markers.length; i++){
      google.maps.event.clearInstanceListeners(this.markers[i]);
      this.markers[i].setMap(null);
    };
    this.markers = [];
    if (typeof this.source == 'object') {
      this.source.close();
    };
  },

  shouldComponentUpdate: function(nextprops, nextstate) {
    return true; // (this.props.nodes != nextprops.nodes)
  },

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


  render: function () {
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

    var Control = React.createClass({
      render: function(){
        return (
          <div id="markercontrol" style={{position:"absolute",zIndex:"999999",top:"30px",right:"12px",
            padding:"4px 8px",fontSize:"16px",backgroundColor:"rgba(255,255,255,0.85)"}}>
            <span style={{fontSize:"18px"}}>Control:</span> &nbsp;
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

    var Status = React.createClass({
      render: function(){
        return (
          <div id="daylightstatus" style={{position:"absolute",zIndex:"999999",bottom:"18px",left:"18px",
            padding:"4px 8px",fontSize:"16px",backgroundColor:"rgba(255,255,255,0.85)"}}>
            <span style={{fontSize:"18px"}}>Groups: <span id="daylightstatusgroups">0</span> &nbsp; &nbsp;
            Nodes: <span id="daylightstatusnodes">0</span> &nbsp; &nbsp;
            Triggers: <span id="daylightstatustriggers">0</span>
            </span>
          </div>
          )
      }
    });

   return (
      <div>
        <div className='mapCanvas daylightmapCanvas'></div> 
        <Level />
        <Control />
        <Status />
      </div>
    );
  }
});

module.exports = Daylightmap;

