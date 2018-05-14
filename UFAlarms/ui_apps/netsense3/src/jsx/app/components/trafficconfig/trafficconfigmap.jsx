import classNames from 'classnames';
import { State, Navigation } from 'react-router';
import auth from 'global/utils/auth';
import helpers from 'global/utils/helpers';

var Trafficconfigmap = React.createClass({

  propTypes: {
    readonly: React.PropTypes.bool,
    trafficconfigs: React.PropTypes.array.isRequired,
    trafficconfigID: React.PropTypes.string.isRequired,
    filterevent:React.PropTypes.string.isRequired,
    site: React.PropTypes.object.isRequired,
    nodes: React.PropTypes.array.isRequired,
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

  trafficconfigs: [],

  maximize: function() {
    $("#trafficconfig-map-panel").data("state","open").css({left:"0px",width:"100%",zIndex:"400"});
    $(window).trigger('resize');
    google.maps.event.trigger(this.map, "resize");
  },

  minimize: function() {
    $("#trafficconfig-map-panel").data("state","closed").css({left:"33%",width:"67%",zIndex:"100"});
    $(window).trigger('resize');
    google.maps.event.trigger(this.map, "resize");
  },

  togglemap: function() {
    if ($("#trafficconfig-map-panel").data("state") == "closed") {
      this.maximize();
    } else {
      this.minimize();
    }
  },

  buildTrafficconfigInfoWindow: function(trafficconfig){
    var i = 0, found = false;
    for (var i=0, match=0;  i<this.props.trafficconfigs.length; i++) {

      if (trafficconfig.trafficconfigid == this.props.trafficconfigs[i].eventid){
        match = i;
      }
      this.trafficconfigs[i].setOptions({strokeOpacity:0.8,fillOpacity:0.35,strokeWeight:2});

    };
    
    var trafficconfig = this.props.trafficconfigs[match];

    if (typeof trafficconfig !== 'undefined') {
      return ['<table style="margin-bottom:12px;font-size:14px"><tr style="font-size:18px"><td style="text-align:right"><b>Nodeid:</b></td><td><b>' , trafficconfig.nodeid , '</b></td></tr>'
          , (trafficconfig.type.length>0)?('<tr><td style="text-align:right"><b>Event:</b></td><td>' + trafficconfig.type + '</td></tr>'):""
          , '<tr><td style="text-align:right"><b>Event ID:</b></td><td>' , trafficconfig.eventid, '</td></tr>'
          , '<tr><td style="text-align:right"><b>Date:</b></td><td>' , trafficconfig.configured_date, '</td></tr>'
          , '</table>'
         ].join("");
    }
   
  },

  buildCameraInfoWindow: function(marker){
    for (var i=0, count=0; i<this.props.trafficconfigs.length; i++) {
      if (this.props.trafficconfigs[i].nodeid == marker.nodeid) {
        count++;
        this.trafficconfigs[i].setOptions({strokeOpacity:1,fillOpacity:1,strokeWeight:4});
      }
       
      else{
        this.trafficconfigs[i].setOptions({strokeOpacity:0.8,fillOpacity:0.35,strokeWeight:2});
      }
     
    }

    for (i = 0; i<this.props.nodes.length; i++) {
      if (marker.nodeid == this.props.nodes[i].nodeid) {
        var model = this.props.nodes[i].model;
      };
    };

    var s = ['<table style="margin-bottom:12px;font-size:14px"><tr style="font-size:20px"><td style="text-align:right"><img height="24" src="/imgs/markers/video-camera.png"><b> ID: </b></td><td><b>' , marker.nodeid , '</b></td></tr>'
          , (typeof marker.name != "undefined" && marker.name.length>0)?('<tr><td style="text-align:right"><b>Name:</b></td><td>' + marker.name + '</td></tr>'):""
          , '<tr><td style="text-align:right;font-size:16px"><b>Traffic Configs:</b></td><td style="font-size:16px"><b>' , (count==0?"None defined.":count) , '</b></td></tr>'
         ].join("");
    s += '<tr><td colspan="3" style="text-align:center">';
    s += "<button id='viewImage' className='btn btn-info' style='border-color:#333;border-radius:10px' ";
    s += "onclick='javascript:ReactBootstrap.Dispatcher.emit("+'"Trafficconfigform.image", "open", {nodeid:"'+marker.nodeid+'",model:"'+model+'"})' + "'><b>View Image</b></button>";
    s += '</td></tr></table>';
    return s;

  },

  showTrafficconfigTooltip: function(trafficconfig) {
    var content = this.buildTrafficconfigInfoWindow(trafficconfig);
    this.infoWindow.setContent(content);
    this.infoWindow.setPosition(trafficconfig.center);
    this.infoWindow.open(this.map, trafficconfig);
  },

  showCameraTooltip: function(marker) {
    var content = this.buildCameraInfoWindow(marker);
    this.infoWindow.setContent(content);
    this.infoWindow.setPosition(marker.position);
    this.infoWindow.open(this.map, marker);
  },

   getCenter: function (box) {
    var latitudeArray =[];
    var longitudeArray =[];
    for(var m=0;m<box.length;m++){
      latitudeArray.push(box[m].latitude);
      longitudeArray.push(box[m].longitude);
    }
    var center = {latitudeArray,longitudeArray};

    var minLat = Math.min.apply(null, center.latitudeArray),
      minLng = Math.min.apply(null, center.longitudeArray),
      maxLat = Math.max.apply(null, center.latitudeArray),
      maxLng = Math.max.apply(null, center.longitudeArray);
    return ({lat:minLat + (Math.abs(maxLat - minLat) / 2),
             lng:minLng + (Math.abs(maxLng - minLng) / 2)
           });     
  },

  componentDidMount: function () {

    if (typeof this.props.site.latitude == "undefined" || this.props.site.latitude == "") {
      this.props.site.latitude = this.mapCenterLat;
      this.props.site.longitude = this.mapCenterLng;
    };
    
    var mapOptions = {
      center: this.mapCenterLatLng(),
      zoom: this.props.initialZoom,
      mapTypeControlOptions: {
        mapTypeIds: ['roadmap', 'satellite']
        },
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
    this.map = new google.maps.Map($("#trafficconfigmap").get(0), mapOptions);

    var that = this;

    this.infoWindow = new google.maps.InfoWindow();

    var bounds = new google.maps.LatLngBounds();
    if (this.props.trafficconfigs.length == 0) {
      bounds.extend(this.mapCenterLatLng());
    }

    this.trafficconfiglookup = {};

    this.polyStyles = {
      member: {fillColor:"#FF0000",strokeColor:"#FF0000"},
      nonmember: {fillColor:"#008ABF", strokeColor:"#008ABF"},
      highlighted: {strokeWeight: 6},
      normal: {strokeWeight: 2}
    };

    for(var i=0;i<this.props.trafficconfigs.length;i++){
      var trafficconfig = this.props.trafficconfigs[i];
      this.trafficconfiglookup[trafficconfig.trafficconfigid] = i;
      var trafficconfigCoords = [];
      var world_bounding_box_length = trafficconfig.roi.world_bounding_box[0].length;
      for (var j=0; j<world_bounding_box_length; j++) {
        trafficconfigCoords.push({
          lat: trafficconfig.roi.world_bounding_box[0][j].latitude,
          lng: trafficconfig.roi.world_bounding_box[0][j].longitude
        });
      };
    
      var strokeConfigColor;
      if(trafficconfig.type == "ObjectDwellConfig"){
        strokeConfigColor = "#525252"; //grey
      }
      else if(trafficconfig.type == "ObjectEnteringConfig"){
        strokeConfigColor = "#4DFA90"; //green
      }
      else if(trafficconfig.type == "ObjectLeavingConfig"){
        strokeConfigColor = "#FF5468"; //red
      }
      else{
        strokeConfigColor = "#4641f4"; //blue
      }

      var trafficconfig = new google.maps.Polygon({
        idx: i,
        paths: trafficconfigCoords,
        strokeColor: strokeConfigColor,
        strokeOpacity: 0.8,
        strokeWeight: 2,
        fillColor: strokeConfigColor,
        fillOpacity: 0.35,
        nodeid: trafficconfig.nodeid,
        active: trafficconfig.active,
        trafficconfigid: trafficconfig.eventid,
        configevent:trafficconfig.type,
        center: this.getCenter(trafficconfig.roi.world_bounding_box[0]),
        highlighted: false
      });
      trafficconfig.setMap(this.map);

      // On click trafficconfig
      google.maps.event.addListener(trafficconfig,'click', function(event){
        for (var i=0; i<that.trafficconfigs.length; i++) {
          if (that.trafficconfigs[i].highlighted) {
            that.trafficconfigs[i].highlighted = false;
            that.trafficconfigs[i].setOptions(that.polyStyles.normal);

          }
        }
        this.setOptions(that.polyStyles.highlighted);
        ReactBootstrap.Dispatcher.emit('Trafficconfigmap.selectTrafficconfig', this.trafficconfigid);
      }); 

      this.trafficconfigs.push(trafficconfig);
      bounds.extend(trafficconfigCoords[0]);
      this.map.fitBounds(bounds);
      bounds.extend(trafficconfigCoords[1]);
      this.map.fitBounds(bounds);    
    }

    this.markers = [];
    for (var i=0; i<this.props.nodes.length; i++) {
      // display Video Node
      var latlng = {lat:parseFloat(this.props.nodes[i].latitude), lng:parseFloat(this.props.nodes[i].longitude)};

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


    // Create the legend and display on the map
    var legend = document.createElement('div');
    legend.id = 'trafficconfig_legend';
    var content = [];
    content.push('<h3 style="margin-top:0px;">Config Event:</h3>');
    content.push('<p><div class="trafficconfig_color trafficconfig_grey"></div>ObjectDwellConfig</p>');
    content.push('<p><div class="trafficconfig_color trafficconfig_green"></div>ObjectEnteringConfig</p>');
    content.push('<p><div class="trafficconfig_color trafficconfig_red"></div>ObjectLeavingConfig</p>');
    content.push('<p><div class="trafficconfig_color trafficconfig_yellow"></div>LineCrossingConfig</p>');
    legend.innerHTML = content.join('');
    legend.index = 1;
    this.map.controls[google.maps.ControlPosition.RIGHT_BOTTOM].push(legend);

    var that = this;
    var listener = google.maps.event.addListener(this.map, "idle", function() { 
      if (that.map.getZoom() > 20) that.map.setZoom(20); 
      google.maps.event.removeListener(listener); 
    });    

    ReactBootstrap.Dispatcher.on('Trafficconfiglist.select', function(trafficconfigid) {
      for (var i=0; i<that.trafficconfigs.length; i++) {
        if (that.trafficconfigs[i].trafficconfigid == trafficconfigid) {
          that.map.setZoom(20);
          //that.map.panTo(marker.position);
          that.showTrafficconfigTooltip(that.trafficconfigs[i]);  
          that.trafficconfigs[i].setOptions(that.polyStyles.highlighted);   
          that.trafficconfigs[i].highlighted = true;               
        } else {
          if (that.trafficconfigs[i].highlighted) {
            that.trafficconfigs[i].highlighted = false;
            that.trafficconfigs[i].setOptions(that.polyStyles.normal);
          }
        }
      }
    });
      
  },

  mapCenterLatLng: function () {
    if (this.props.nodes.length == 0) {
      return new google.maps.LatLng(parseFloat(this.props.site.latitude), parseFloat(this.props.site.longitude));
    }
    return new google.maps.LatLng(this.props.mapCenterLat, this.props.mapCenterLng);
  },

  componentDidUpdate: function(nextprops,nextstate){
    if((this.props.filterevent != nextprops.filterevent) && (this.props.filterevent !="All") && (this.props.filterevent !="Active") && (this.props.filterevent !="Inactive")){
      for (var i = 0; i < this.trafficconfigs.length; i++) {
        var filtermarker = this.trafficconfigs[i];
        if(filtermarker.configevent == this.props.filterevent){
          filtermarker.setVisible(true);
        }
        else{
          filtermarker.setVisible(false);
        }
      }
    }
   
    if(this.props.filterevent === "All"){
      for (var i = 0; i < this.trafficconfigs.length; i++) {
        var filtermarker = this.trafficconfigs[i];
          filtermarker.setVisible(true);
      }
    }

    if(this.props.filterevent === "Active"){

      for (var i = 0; i < this.trafficconfigs.length; i++) {
        var filtermarker = this.trafficconfigs[i];
        if(this.trafficconfigs[i].active){
          filtermarker.setVisible(true);
        }
        else{
          filtermarker.setVisible(false);
        }
      }

    }

    if(this.props.filterevent === "Inactive"){
      for (var i = 0; i < this.trafficconfigs.length; i++) {
        var filtermarker = this.trafficconfigs[i];
        if(!this.trafficconfigs[i].active){
          filtermarker.setVisible(true);
        }
        else{
          filtermarker.setVisible(false);
        }

      }
    }

    
  },
  
  shouldComponentUpdate: function(nextprops, nextstate) {
    return (this.props.trafficconfigs != nextprops.trafficconfigs)
  },

  componentWillUnmount: function() {
    ReactBootstrap.Dispatcher.removeAllListeners("Trafficconfiglist.select");
    if (typeof this.source == 'object') {
      console.log("Closing EventSource...");
      this.source.close();
    };
  },

  render: function () {
    var that = this;

    var ToggleTrafficMonitor = React.createClass({
      toggleTrafficMonitor: function() {
        if (this.refs.toggleTrafficMonitor.getDOMNode().checked) {
          if (!!window.EventSource) {
            if (typeof this.source == 'object') {
              that.source.close();
              console.log('EventSource closed.');
            };
            var url = NSN.mqttURL + '/' + NSN.customerID + '/' + NSN.siteID + '/+/TrafficDetectionEvent';
            that.source = new EventSource(url, {withCredentials : true});
            console.log('EventSource requested: ' + url);

            that.source.addEventListener('message', function(e) {
              var msg = JSON.parse(e.data);
              var idx = that.trafficconfiglookup[msg.trafficdetectioneventid];
              if (typeof idx == "undefined") {
                console.log("Traffic Event received " + msg.trafficdetectioneventid + "; not in this site.");
                return;
              }
              if (typeof msg.count != "undefined") {
                var currentcount = msg.count;
                console.log(idx + " : " + currentcount);
                if (that.trafficconfigs[idx].count != currentcount) {
                  ReactBootstrap.Dispatcher.emit('Trafficconfigmap.countupdate', currentcount, idx);
                  that.trafficconfigs[idx].count = currentcount;
                  that.props.trafficconfigs[idx].count = currentcount;
                }
              }
            });
           
            that.source.onopen = function (e) {
              console.log('EventSource opened: ' + JSON.stringify(e));
            };
            that.source.onerror = function (e) {
              console.log('EventSource failed: ' + JSON.stringify(e));
            };  
          } else {
            alert('The Monitor uses Server-Sent Events (SSE). a feature which is not supported in Microsoft Internet Explorer or Microsoft Edge.');
          }
        } else {
          if (typeof that.source == 'object') {
            that.source.close();
            console.log('EventSource closed.');
          }
        }
      },
      render: function(){
        return (
          <div style={{position:"absolute",zIndex:"999999",top:"29px",right:"20px",fontSize:"14px",padding:"5px 8px",borderRadius:"2px",border:"1px solid #CCC",boxShadow:"0px 1px 4px -1px rgba(0, 0, 0, 0.3)",color:"#000",backgroundColor:"#FFF"}}>
            Monitor? &nbsp;<input title="Toggle real-time display of traffic config count changes" ref="toggleTrafficMonitor" type="checkbox" style={{height:"16px",width:"16px"}} onChange={this.toggleTrafficMonitor} />
          </div>
          );
      }
    });

    var glyph = "icon-fontello-resize-horizontal";
    var handler = this.togglemap;
    var Minmax = (
        <div style={{position:"absolute",cursor:"pointer",top:"-20px",right:"16px",zIndex:"101",color:"#000000",height:"20px",fontSize:"36px"}}
              onClick={handler}>
          <Icon bundle={this.props.bundle} glyph={glyph} />
        </div> );

    return (
      <div style={{height:"100%"}}>
        {Minmax}
        <div id="trafficconfigmap" className='mapCanvas trafficconfigmapCanvas'></div> 
        <ToggleTrafficMonitor />
      </div>
    );
  }
});

module.exports = Trafficconfigmap;