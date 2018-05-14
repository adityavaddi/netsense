import classNames from 'classnames';
import { State, Navigation } from 'react-router';
import helpers from 'global/utils/helpers';

var Nodemap = React.createClass({
  markers: [],
  locked: true,
  maxZIndex: 100000,
  startposition: null,
  unmappednodes: [],
  nodeidlookup: {},
  propTypes: {
    nodes: React.PropTypes.array.isRequired,
    site: React.PropTypes.object.isRequired,
    center: React.PropTypes.object.isRequired,
    zoom: React.PropTypes.number.isRequired,
    detail_state: React.PropTypes.string.isRequired
  },

  getDefaultProps: function () {
    return {
      initialZoom: 8,
      mapCenterLat: 37.3807927,  // Sensity HQ
      mapCenterLng: -121.9928375
    };
  },

  maximize: function () {
    $("#node-map-panel").data("state", "open").css({ left: "0px", width: "100%", zIndex: "400" });
    $(window).trigger('resize');
    google.maps.event.trigger(this.map, "resize");
  },

  minimize: function () {
    $("#node-map-panel").data("state", "closed").css({ left: "33%", width: "67%", zIndex: "100" });
    $(window).trigger('resize');
    google.maps.event.trigger(this.map, "resize");
  },

  togglemap: function () {
    if ($("#node-map-panel").data("state") == "closed") {
      this.maximize();
    } else {
      this.minimize();
    }
  },
  /*
    maximize: function() {
      ReactBootstrap.Dispatcher.emit("Nodemap.maximize", this.map.getCenter(), this.map.getZoom());
    },
  
    minimize: function() {
      ReactBootstrap.Dispatcher.emit("Nodemap.minimize", this.map.getCenter(), this.map.getZoom());
    },
  */
  // build the content for the tooltip
  buildInfoWindow: function (marker) {
    var that = this;
    var i = 0, found = false;
    while (!found && i < this.props.nodes.length) {
      found = (marker.nodeid == this.props.nodes[i].nodeid);
      i++;
    };
    var node = this.props.nodes[--i];

    var s = ['<table style="font-size:14px"><tr style="font-size:18px">'
      ,'<td style="text-align:right"><b>'
      , "<span style='cursor:pointer' onclick='javascript:ReactBootstrap.Dispatcher.emit("+'"Nodemap.streetview"' +")'>ID:</span></b></td><td><b>"
      , "<span style='cursor:pointer' title='Center and Zoom' onclick='javascript:ReactBootstrap.Dispatcher.emit("+'"Nodemap.zoomin"' +")'>"+node.nodeid+"</span></b>"
      , '</td></tr>'
      , (node.name && node.name.length > 0) ? ('<tr><td style="text-align:right"><b>Name:</b></td><td>' + node.name + '</td></tr>') : ""
      , '<tr><td style="text-align:right"><b>Model:</b></td><td><nobr>', node.model, '</nobr></td></tr>'
      ].join("");

    if (helpers.modelType(node.model) == "Lighting") {
      s += [
        , '<tr><td style="text-align:right"><b>Fixture:</b></td><td><nobr>'
        , node.fixturename ? node.fixturename : '<span style="color:#F90;font-weight:bold">Not Set</span>'
        , '</nobr></td></tr>'
        , '<tr><td style="text-align:right"><b>Schedule:</b></td>'
        ].join("");
      s += '<td>';
      if (node.schedulename) {
        s += "<span style='cursor:pointer' title='View' ";
        s += "onclick='javascript:ReactBootstrap.Dispatcher.emit("+'"Nodemap.schedule"' +")'>"+node.schedulename+"</span>"
       } else {
        s += "n/a";
       }
       s += '</td></tr>';
    } else {
      if (helpers.modelType(node.model) == "Video") {
        s += '<tr><td colspan="2" style="text-align:center">';
        s += "<button id='viewImage' className='btn btn-info' style='border-color:#333;border-radius:10px' ";
        s += "onclick='javascript:ReactBootstrap.Dispatcher.emit("+'"Nodeform.image", "open", {nodeid:"'+node.nodeid+'",model:"'+node.model+'"})' + "'><b>View Image</b></button>";
        s += '</td></tr>';
      };
    };
    s += '</table>';

    return s;
  },

  deselectMarkers: function () {
    while (this.selected_markers.length > 0) {
      var selected_marker = this.markers[this.selected_markers[0]];
      var arg = helpers.keytoarg(selected_marker.icon.key);
      arg.selected = false;
      selected_marker.selected = false;
      selected_marker.setIcon(helpers.genMarker(arg));
      this.selected_markers.shift();
    }
  },

  showTooltip: function (marker) {
    if (marker.level != this.currentLevel && this.currentLevel != "all") {
      this.changeLevel(marker.level);
      $("#levelselect").val(this.currentLevel);
    }

    var arg = helpers.keytoarg(marker.icon.key);
    arg.selected = true;
    var icon = helpers.genMarker(arg);
    this.markers[marker.idx].setIcon(icon);
    this.selected_markers.push(marker.idx);

    var content = this.buildInfoWindow(marker);
    this.infoWindow.setPosition(marker.getPosition());
    this.infoWindow.setContent(content);
    this.infoWindow.open(this.map, marker);
  },

  updateMarker: function (nodeid, arg) {
    var idx = this.nodeidlookup[nodeid];
    var currentArg = helpers.keytoarg(this.markers[idx].icon.key);
    if (typeof currentArg !== 'undefined') {
      $.extend(currentArg, arg);
      this.markers[idx].setIcon(helpers.genMarker(currentArg));
    }
  },

  showStreetViewAtMarker: function(markerIndex) {
    showStreetViewAtLatLng(getLatLngForMarker(markerIndex));
  },

  showStreetViewAtLatLng: function(latlng) {
    var streetViewService = new google.maps.StreetViewService();
    //var SVpanorama = map.getStreetView(); // we already have a variable object for the streetView!
    var STREETVIEW_MAX_DISTANCE = 100;

    streetViewService.getPanoramaByLocation(latlng, STREETVIEW_MAX_DISTANCE, function (streetViewPanoramaData, status) {
      if (status === google.maps.StreetViewStatus.OK) {
        //var SVheading = google.maps.geometry.spherical.computeHeading(streetViewPanoramaData.location.latLng,showWhere);
        //alert('heading='+SVheading);
        streetView.setPosition(latlng);
        //streetView.setPov({heading: SVheading, pitch: 0});
        streetView.setVisible(true);
      } else {
        alert('Sorry... Street View is not available within '+STREETVIEW_MAX_DISTANCE+'m of that location.');
      }
    });
  },

  loadStreetView: function(map, marker) {
    if (marker == undefined || marker == null) return;    
    // Find street view within 200 meters, as street view may not always be available on any latlng
    var service = new google.maps.StreetViewService();
    service.getPanoramaByLocation(marker.position, 200, function(result, status) {
          if (status == google.maps.StreetViewStatus.OK) {
              var panorama = map.getStreetView();
          panorama.setPosition(result.location.latLng);
        panorama.setVisible(true);
          }
          else {
            alert("No street view is available within " + 200 + " meters");
            return;
          }
      });
  },


  componentDidMount: function () {

    // default lat/lng for site (Sensity HQ)
    if (typeof this.props.site.latitude == "undefined" || this.props.site.latitude == "") {
      this.props.site.latitude = this.mapCenterLat;
      this.props.site.longitude = this.mapCenterLng;
    };

    // one infoWindow object that will be re-used for multiple purposes
    this.infoWindow = new google.maps.InfoWindow();

    // determine if there are multiple building levels and generate ordered array of levels
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
      this.multiLevel = true;;
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
      mapTypeId: localStorage.getItem('nodeMapType') === null
        ? google.maps.MapTypeId.ROADMAP
        : localStorage.getItem('nodeMapType'),
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
      zoomControl: true,
      fullscreenControl: false
    };
    this.map = new google.maps.Map(this.getDOMNode().childNodes[1], mapOptions);
    this.map.mapTypes.set('night', nightMap);

//    var oms = new OverlappingMarkerSpiderfier(this.map);
    var that = this;

    google.maps.event.addListener(this.map, 'maptypeid_changed', function () {
      localStorage.setItem('nodeMapType', that.map.getMapTypeId());
    });

    var bounds = new google.maps.LatLngBounds();
    if (this.props.nodes.length == 0) {
      bounds.extend(this.mapCenterLatLng());
    }
    this.markers = [];
    this.selected_markers = [];

    // lots of logic for nodes without lat/lng
    this.unmappednodes = [];
    var latlng;
    for (var i = 0; i < this.props.nodes.length; i++) {
      var thisnode = this.props.nodes[i];
      if (typeof thisnode.latitude == "undefined"
        || thisnode.latitude == ""
        || !$.isNumeric(thisnode.latitude)
        || thisnode.latitude < -90
        || thisnode.latitude > 90
        || typeof thisnode.longitude == "undefined"
        || thisnode.longitude == ""
        || !$.isNumeric(thisnode.longitude)
        || thisnode.longitude < -180
        || thisnode.longitude > 180) {
        var n = this.unmappednodes.push(thisnode.nodeid);
        if (typeof thisnode.latitude_gps != "undefined"
          && thisnode.latitude_gps != ""
          && $.isNumeric(thisnode.latitude_gps)) {
          latlng = new google.maps.LatLng(
            parseFloat(thisnode.latitude_gps),
            parseFloat(thisnode.longitude_gps)
          );
        } else {
          latlng = new google.maps.LatLng(
            parseFloat(this.props.site.latitude), // + Math.sin(0.4 * n) * 0.000018 * n,
            parseFloat(this.props.site.longitude) // + Math.cos(0.4 * n) * 0.00002 * n
          );
        }
      } else {
        latlng = new google.maps.LatLng(
          parseFloat(thisnode.latitude),
          parseFloat(thisnode.longitude)
        )
      };
/*
      var enc = 1;
      for (var j=thisnode.nodeid.length - 1; j>=0; j--) {
        enc *= thisnode.nodeid.charCodeAt(j);
      };
      enc = enc.toExponential();
      enc = enc.substr(0, enc.indexOf("e") - 1);
      latlng = new google.maps.LatLng(
                  latlng.lat() + (enc.charAt(3)<5?-1:1)*(enc/500000), 
                  latlng.lng() + (enc.charAt(enc.length-1)<5?1:-1)*(enc/400000)
                );
*/
      // determine initial marker icon
      var icon = helpers.genMarker(helpers.setNodeMarker(this.props.nodes[i]));

      // create the marker
      var marker = new google.maps.Marker({
        idx: i,
        position: latlng,
        map: this.map,
        icon: icon,
        prevIcon: icon,
        draggable: true,
        nodeid: this.props.nodes[i].nodeid,
        name: this.props.nodes[i].name,
        level: this.props.nodes[i].level,
        building: this.props.nodes[i].building,
        type: helpers.modelType(this.props.nodes[i].model),
        lightLevel: "",
        visible: true, // (!this.multiLevel || this.props.nodes[i].level == this.levels[0]),
        selected: false
      });

      this.markers.push(marker);
      this.nodeidlookup[this.props.nodes[i].nodeid] = i;
      bounds.extend(latlng);

      // On click marker
      google.maps.event.addListener(marker, 'mousedown', function () {
        //            this.setIcon("/imgs/markers/teal-dot.png");

        var content = that.buildInfoWindow(this);
        that.infoWindow.setContent(content);
        that.infoWindow.open(that.map, this);

        that.deselectMarkers();

        var arg = helpers.keytoarg(this.icon.key);
        arg.selected = true;
        var icon = helpers.genMarker(arg);
        that.markers[this.idx].setIcon(icon);
        that.markers[this.idx].setZIndex(++that.maxZIndex);
        that.selected_markers.push(this.idx);

        ReactBootstrap.Dispatcher.emit('Nodemap.selectNode', this.nodeid)
      });

      google.maps.event.addListener(marker, 'dragstart', function () {
        if (that.locked) {
          that.startposition = this.getPosition();
          var s = '<p style="text-align:center;color:#C00;font-size:18px">Markers are locked and cannot be repositioned.'
            + '<br />  Click on the green lock icon to unlock.</p>';
          that.infoWindow.setContent(s);
        } else {
          that.startposition = this.getPosition();
          if (that.props.detail_state == "hidden") {
            s = '<p style="text-align:center;color:#C00;font-size:18px">Cannot reposition markers if the Node Details form is hidden.'
              + '<br />(You will need to see the Save button.)</p>';
            that.infoWindow.setContent(s);
            }
          }
        });

      google.maps.event.addListener(marker, 'dragend', function (marker) {
        if (that.locked || that.props.detail_state == "hidden") {
          this.setPosition(that.startposition);
          var content = that.buildInfoWindow(this);
          that.infoWindow.setContent(content);
        } else {
          //            this.setIcon(this.prevIcon);
          ReactBootstrap.Dispatcher.emit('Nodemap.moveNode', this.nodeid, this.getPosition())
        };
      });

    };

    if (!this.props.center || this.props.zoom < 0) {
      this.map.fitBounds(bounds);
    } else {
      if (this.props.center) {
        this.map.setCenter(this.props.center);
      };
      if (this.props.zoom >= 0) {
        this.map.setZoom(this.props.zoom);
      };
    }


    this.markerCluster = new MarkerClusterer(this.map, this.markers,
      {
        ignoreHidden: true,
        imagePath: '/imgs/markers/m',
        minimumClusterSize: 20,
        maxZoom: 17,
        styles: [{
          url: "/imgs/markers/cluster1.png",
          height: 50,
          width: 50,
          textSize: 18,
          textColor: "#FFFFFF"
        }, {
          url: "/imgs/markers/cluster2.png",
          height: 60,
          width: 60,
          textSize: 18,
          textColor: "#FFFFFF"
        }, {
          url: "/imgs/markers/cluster4.png",
          height: 80,
          width: 80,
          textSize: 20,
          textColor: "#FFFFFF"
        }, {
          url: "/imgs/markers/cluster6.png",
          height: 110,
          width: 110,
          textSize: 24,
          textColor: "#FFFFFF"
        }, {
          url: "/imgs/markers/cluster7.png",
          height: 150,
          width: 150,
          textSize: 28,
          textColor: "#FFFFFF"
        }]
      });


    if (this.unmappednodes.length > 0) {
      console.log("***** Nodes with missing lat or lng: " + this.unmappednodes);
      noty({
        type: "warning", timeout: 10000, text: this.unmappednodes.length + ' node(s) found with missing or invalid latitude/longitude.<br />'
          + 'These nodes are temporarily displayed near the Site location<br />'
          + '(or at their GPS locations, if available).<br />'
          + 'Please correct these locations as soon as possible.'
      })
    }

    var that = this;
    var listener = google.maps.event.addListener(this.map, "idle", function () {
      if (that.map.getZoom() > 20) that.map.setZoom(20);
      if (!that.multilevel) {
        //        $("#levelselect").parent().hide();
      }
      that.map.mapTypes.roadmap.name = 'Day';
      that.map.setOptions({ 'mapTypeControl': true });
      google.maps.event.removeListener(listener);
    });

    if (this.multiLevel) {
      this.currentLevel = "all";
    };
    if (this.multiType) {
      this.currentType = "all";
    }

    // select marker when user selects node from grid
    ReactBootstrap.Dispatcher.on('Nodelist.select', function (nodeid) {
      that.deselectMarkers();
      var idx = that.nodeidlookup[nodeid];
      //      for (var i=0, idx=-1; idx<0 && i<that.markers.length; i++) {
      //        if (that.markers[i].nodeid == nodeid) {
      //          idx = i;
      //        };
      //      };
      //      if (idx >= 0) {
      //map.fitBounds(bounds);
      //        that.map.panTo(that.markers[idx].position);
      //        var listener = google.maps.event.addListener(that.map, "idle", function() {
      //          if (that.map.getZoom() < 16) that.map.setZoom(17);
      //          google.maps.event.removeListener(listener);
      that.markers[idx].setZIndex(++that.maxZIndex);
      that.showTooltip(that.markers[idx]);
      //        });
      //      };
    });

    ReactBootstrap.Dispatcher.on('Nodelist.multiSelect', function (nodes) {
      that.infoWindow.close();
      that.deselectMarkers();
      for (var i = 0; i < that.markers.length; i++) {
        if (nodes.indexOf(that.markers[i].nodeid) >= 0) {
          var arg = helpers.keytoarg(that.markers[i].icon.key);
          arg.selected = true;
          var icon = helpers.genMarker(arg);
          that.markers[i].setIcon(icon);
          that.selected_markers.push(i);
        }
      }
    });

    ReactBootstrap.Dispatcher.on('Nodelist.nodeStatusUpdated', function (updatednodes) {
      for (var i = 0; i < updatednodes.length; i++) {
        that.updateMarker(updatednodes[i].nodeid,
          {
            status: ["good", "warn", "error", "none"][updatednodes[i].net_stats],
            lightLevel: updatednodes[i].sen_stat,
          });

      };
    });

    ReactBootstrap.Dispatcher.on('Nodemap.schedule', function () {
      ReactBootstrap.Dispatcher.emit("Nodeform.schedule", "open", 
        Object.assign({}, that.props.nodes[that.nodeidlookup[NSN.nodeID]]));
    });

    ReactBootstrap.Dispatcher.on('Nodemap.streetview', function () {
      that.loadStreetView(that.map, that.markers[that.nodeidlookup[NSN.nodeID]]);
    });

    ReactBootstrap.Dispatcher.on('Nodemap.zoomin', function () {
      var m = that.markers[that.nodeidlookup[NSN.nodeID]];
      that.map.panTo(m.position);
      that.map.setZoom(20); 
    });

    ReactBootstrap.Dispatcher.on('Nodepanel.resizeMap', function () {
        google.maps.event.trigger(that.map, "resize");
    });

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

  componentWillUnmount: function () {
    for (var i = 0; i < this.markers.length; i++) {
      google.maps.event.clearInstanceListeners(this.markers[i]);
      this.markers[i].setMap(null);
    };
    this.markers = [];
    this.unmappednodes = [];
    if (typeof this.source == 'object') {
      console.log("Closing EventSource...");
      this.source.close();
    };
    ReactBootstrap.Dispatcher.removeAllListeners("Nodelist.select");
    ReactBootstrap.Dispatcher.removeAllListeners("Nodelist.multiSelect");
    ReactBootstrap.Dispatcher.removeAllListeners("Nodelist.nodeStatusUpdated");
    ReactBootstrap.Dispatcher.removeAllListeners("Nodepanel.resizeMap");
    ReactBootstrap.Dispatcher.removeAllListeners("Nodemap.schedule");
    ReactBootstrap.Dispatcher.removeAllListeners("Nodemap.streetview");
    ReactBootstrap.Dispatcher.removeAllListeners("Nodemap.zoomin");
  },

  shouldComponentUpdate: function (nextprops, nextstate) {
    return (this.props.nodes != nextprops.nodes)
  },

  changeLevel: function (level) {
    if (this.currentLevel == level) {
      return;
    }
    this.infoWindow.close();
    for (var i = 0; i < this.markers.length; i++) {
      var m = this.markers[i];
      m.setVisible((level == "all" || m.level == level) && (this.currentType == "all" || m.type == this.currentType));
    };
    this.markerCluster.repaint();
    this.currentLevel = level;
//    ReactBootstrap.Dispatcher.emit("Nodemap.filter", [{columnId:"level",value:level}]);
  },

  changeType: function (type) {
    if (this.currentType == type) {
      return;
    }
    this.infoWindow.close();
    for (var i = 0; i < this.markers.length; i++) {
      var m = this.markers[i];
      m.setVisible((type == "all" || m.type == type) && (this.currentLevel == "all" || m.level == this.currentLevel));
    };
    this.markerCluster.repaint();
    this.currentType = type;
//    ReactBootstrap.Dispatcher.emit("Nodemap.filter", [{columnId:"type",value:type}]);
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
            Type: &nbsp;<select ref='typeselect' id='typeselect' style={{ fontSize: "11px" }} onChange={this.handleChangeType}>
            </select>
          </div>
        )
      }
    });

    var ToggleLock = React.createClass({
      getInitialState: function () {
        return { locked: that.locked }
      },
      toggleLock: function () {
        that.locked = !that.locked;
        this.setState({ locked: !this.state.locked });
      },
      render: function () {
        if (this.state.locked) {
          return (
            <div title="Unlock markers" id="lockmarkers"
              style={{ position: "absolute", zIndex: "999999", cursor: "pointer", bottom: "210px", right: "10px", fontSize: "18px", padding: "0px 8px", borderRadius: "2px", border: "1px solid #CCC", boxShadow: "0px 1px 4px -1px rgba(0, 0, 0, 0.3)", color: "#090", backgroundColor: "#FFF" }}>
              <Icon glyph="icon-fontello-lock" onClick={this.toggleLock} />
            </div>
          )
        } else {
          return (
            <div title="Markers are unlocked - click to lock"
              style={{ position: "absolute", zIndex: "999999", cursor: "pointer", bottom: "210px", right: "10px", fontSize: "18px", padding: "0px 8px", borderRadius: "2px", border: "1px solid #CCC", boxShadow: "0px 1px 4px -1px rgba(0, 0, 0, 0.3)", color: "rgb(254, 77, 8)", backgroundColor: "#FF8" }}>
              <Icon glyph="icon-fontello-lock-open-alt" onClick={this.toggleLock} />
            </div>
          )
        }
      }
    });

    var ToggleMonitor = React.createClass({
      toggleMonitor: function () {
        if (this.refs.toggleMonitor.getDOMNode().checked) {
          if (!!window.EventSource) {
            var url = NSN.mqttURL + '/' + NSN.customerID + '/' + NSN.siteID + '/+/+';
            that.source = new EventSource(url, { withCredentials: true });
            console.log('EventSource requested: ' + url);

            that.source.addEventListener('message', function (e) {
              //                console.log('EventSource event:' + JSON.stringify(e.data));
              var data = JSON.parse(e.data);
              var found = false, j = 0;

              if (typeof data.sensor != "undefined" && data.sensor === "DeviceAlarm") {
                data.sensor.category = helpers.getAlarmCategory(data.sensor.type);
                ReactBootstrap.Dispatcher.emit('Nodemap.Alert', data.nodeid, data.sensor);
              } else {
                switch (data.name) {
                  case "SensorSample":
                    if (typeof data.sensor != "undefined" && data.sensor == "lt") {
                      console.log("lt sensor msg: " + JSON.stringify(data));
                      console.log("Node " + data.nodeid + " reports light level is now: " + data.value + "%");
                      that.updateMarker(data.nodeid, {lightLevel: data.value});
                      ReactBootstrap.Dispatcher.emit('Nodemap.Status', data.nodeid,
                        {Sensor:true, Light:data.value, Connection:true});
                    } else {
                      // update connection status if currently disconnected
                      if (typeof data.nodeid == "undefined") {
                        console.log("SensorSample event received with undefined node: " + JSON.stringify(data));
                      } else {
                        if (typeof that.nodeidlookup[data.nodeid] == "undefined") {
                          console.log("SensorSample event received for unknown node: " + data.nodeid);
                        } else {
                          if (typeof that.markers[that.nodeidlookup[data.nodeid]] == "undefined") {
                            console.log("Map marker not found for node: " + data.nodeid);
                          } else {
                            if (helpers.keytoarg(that.markers[that.nodeidlookup[data.nodeid]].icon.key).status == "none") {
                              // find a way to set net_stat to false and reconsider the alerts before setting marker and icons
                              // use _setalerts from nodelist
                              that.updateMarker(data.nodeid, { status: "good" })
                              ReactBootstrap.Dispatcher.emit('Nodemap.Status', data.nodeid,
                                {Sensor:true, Connection: true });
                            };
                          };
                        };
                      };
                    };
                    break;
                  case "ConnectionStatus":
                    if (typeof data.status != 'undefined') {
                      if (data.status == 'connected') {
                        console.log("Node " + data.nodeid + " is now connected.");
                        that.updateMarker(data.nodeid, { status: "good" })
                        ReactBootstrap.Dispatcher.emit('Nodemap.Status', data.nodeid,
                          { Connection: true });
                      } else {
                        if (data.status == 'disconnected') {
                          console.log("Node " + data.nodeid + " has disconnected.");
                          that.updateMarker(data.nodeid, { status: "error" })
                          ReactBootstrap.Dispatcher.emit('Nodemap.Status', data.nodeid,
                            { Connection: false, Light: null, Sensor: null });
                        }
                      }
                    };
                    break;
                };
              };
            });
            that.source.onopen = function (e) {
              console.log('EventSource opened: ' + JSON.stringify(e));
              ReactBootstrap.Dispatcher.emit("Nodemap.UpdateNodeStatus");
            }
            that.source.onerror = function (e) {
              console.log('EventSource failed: ' + JSON.stringify(e));
              //                that.toggleMonitor();
            }
          } else {
            // Resort to xhr polling :(
            alert('The Monitor uses Server-Sent Events (SSE). a feature which is not supported in Microsoft Internet Explorer or Microsoft Edge.')
          }
        } else {
          if (typeof that.source == 'object') {
            that.source.close();
            console.log('EventSource closed.');
          }
        }
      },
      render: function () {
        return (
          <div style={{ position: "absolute", zIndex: "999999", top: "30px", right: "20px", fontSize: "14px", padding: "5px 8px", borderRadius: "2px", border: "1px solid #CCC", boxShadow: "0px 1px 4px -1px rgba(0, 0, 0, 0.3)", color: "#000", backgroundColor: "#FFF" }}>
            Monitor? &nbsp;<input title="Toggle real-time display of node status changes" id="monitor-toggle" ref="toggleMonitor" type="checkbox" style={{ height: "16px", width: "16px", position: "relative !important" }} onChange={this.toggleMonitor} />
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
      <div style={{ height: "100%" }}>
        {Minmax}
        <div className='mapCanvas' style={{ width: "100%" }}></div>
        <ToggleMonitor />
        <Level />
        <ToggleLock />
      </div>
    );
  }
});

module.exports = Nodemap;