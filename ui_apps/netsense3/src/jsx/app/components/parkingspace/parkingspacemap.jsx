import classNames from 'classnames';
import { State, Navigation } from 'react-router';
import auth from 'global/utils/auth';
import helpers from 'global/utils/helpers';

var Parkingspacemap = React.createClass({

    getInitialState: function() {
        // return this.getparkingSpaces(this.props.parkingspaces, this.props.parkingspacesmetadata)
        return{
            parkingspaces:this.props.parkingspaces,
            renameSpacesButton:false
        }
    },

    propTypes: {
        nodes: React.PropTypes.array.isRequired,
        parkingspaces: React.PropTypes.array.isRequired,
        parkingzones: React.PropTypes.array.isRequired,
        site: React.PropTypes.object.isRequired,
        parkingspacesmetadata: React.PropTypes.array.isRequired,
        selected_spaces:React.PropTypes.array.isRequired,
        nodes:React.PropTypes.array.isRequired
    },

    getDefaultProps: function () {
        return {
            initialZoom: 22,
            readonly: false,
            map:null,
            mapCenterLat: 37.3807927,  // Sensity HQ
            mapCenterLng: -121.9928375,
            spaces:[],
            renameSpacesButton:false
        };
    },
    getparkingSpaces: function(parkingSpaces, metadata) {
        for (var i=0; i<parkingSpaces.length; i++) {
            for(var p = 0; p<metadata.length;p++){
                if (parkingSpaces[i].parkingspaceid == metadata[p].parkingspaceid){
                    return parkingSpaces[i].metadata = metadata[p];
                }
            }
        }
        return null;

    },

    currentActive: "all",

    maximize: function() {
        $("#parkingspace-map-panel").data("state","open").css({left:"0px",width:"100%",zIndex:"400"});
        $(window).trigger('resize');
        google.maps.event.trigger(this.map, "resize");
    },

    minimize: function() {
        $("#parkingspace-map-panel").data("state","closed").css({left:"33%",width:"67%",zIndex:"100"});
        $(window).trigger('resize');
        google.maps.event.trigger(this.map, "resize");
    },

    togglemap: function() {
        if ($("#parkingspace-map-panel").data("state") == "closed") {
            this.maximize();
        } else {
            this.minimize();
        }
    },

    bearing: function (n, z) {
        var y = Math.sin(z.lng-n.lng) * Math.cos(z.lat);
        var x = Math.cos(n.lat)*Math.sin(z.lat) -
            Math.sin(n.lat)*Math.cos(z.lat)*Math.cos(z.lng-n.lng);
        var b = Math.atan2(y, x)*(180/Math.PI);
        return b<0?(360+b):b;
    },

    handleSpaceNameChange: function (key) {
        return function (e) {
            var state = this.state;
            switch (key) {
                case 'name':
                    state.metadata[key] = e.target.value;
                    state[key] = e.target.value;

                    break;
                default:;
            };
            this.setState({ state: state });
        }.bind(this);
    },

    detectNameEdited:function(){
        var state = this.state;

        $('#parkingSpotName').keypress(function(e){
            if(e.which == 13){
                var nameEdited = $("#parkingSpotName"). val();
                state.name = nameEdited;
                state.metadata.name = nameEdited;
                ReactBootstrap.Dispatcher.emit("Parkingspaceform.save", Object.assign({}, state));
            }
        });
    },

    spaces: [] ,
    zones: [],
    count:0,
    currentExcludedMarker :[],

    buildSpaceInfoWindow: function(spot){
        var i = 0, found = false;
        for (var i=0, match=0;  i<this.props.parkingspaces.length; i++) {
            if (spot.parkingspaceid == this.props.parkingspaces[i].parkingspaceid){
                match = i;
            }
            this.spaces[i].setOptions({fillOpacity:0.35});
        };

        var parkingspace = this.props.parkingspaces[match];
        if(parkingspace.metadata){
            parkingspace.name = parkingspace.metadata.name;
        }

        var value = parkingspace.name
        return ['<table style="margin-bottom:12px;font-size:14px">' +
                '<tr>' +
                '<tr style="font-size:16px"><td style="text-align:right"><b>Space ID:</b></td>' +
                '<td><b>'
                , "<span title='Center and Zoom' onclick='javascript:ReactBootstrap.Dispatcher.emit("+'"Parkingspacemap.zoomin","'+_.escape(parkingspace.parkingspaceid)+'"' +")'>"+_.escape(parkingspace.parkingspaceid)+"</span></b>"
                , '</td>' +
                '</tr>',
                (parkingspace.name != "undefined" && parkingspace.name.length>0)?
                ('<tr>' +
                    '<td style="text-align:right">' +
                        '<b>Name:</b>' +
                    '</td>' +
                    '<td>'  + parkingspace.name + '</td>' +
                    // '<td>' + <input type="text" id="parkingSpotName" ref="parkingspacename" /> + parkingspace.name + '</td>' +
                '</tr>'):
                " No Name Specified"
        ].join("");

    },

    buildRenameInfoWindow:function(show){
        console.log("show", show);
        return [
        '<div class="parking-space-map-rename-instruction">'+
            '<p class="firstHeading">Hover over each space in order to rename </p>'+
        '</div>'
        ].join("")
    },

    showSpaceTooltip: function(spot) {
        var content = this.buildSpaceInfoWindow(spot);
        this.infoWindow.setContent(content);
        this.infoWindow.setPosition(spot.center);
        this.infoWindow.open(this.map, spot);
    },

    getCenter: function (box) {
        var minLat = Math.min.apply(null, box.lat),
            minLng = Math.min.apply(null, box.lon),
            maxLat = Math.max.apply(null, box.lat),
            maxLng = Math.max.apply(null, box.lon);
        return ({lat:minLat + (Math.abs(maxLat - minLat) / 2),lng:minLng + (Math.abs(maxLng - minLng) / 2)});
    },

    handleEventType:function(key){
        return function (e) {
            switch (key){
                case 'hover':
                    console.log("hovered", e.target.value);
                break
            }
        }
    },

    // clearZones: function() {
    //     for (var i=0; i<this.zones.length; i++) {
    //         this.zones[i].setOptions({fillOpacity:0.35});
    //         this.zones[i].highlighted = false;
    //     }
    // },

    // count = 0,
    markerAction: function (marker) {

        switch ($("#action").val()) {
            case "SelectMultiple":
                break;
            case "Toggle":
                marker.highlighted = !marker.highlighted;
                if (!marker.hovered) {
                    marker.hovered = true;
                    marker.setIcon(helpers.genSpaceMarker(true, marker.parkingspaceid));
                    var countvalue = this.count++;
                    marker.tempHoveredidx = Number(countvalue);

                } else {
                    marker.hovered = false;
                    marker.setIcon(helpers.genSpaceMarker(false, marker.parkingspaceid));
                    this.currentExcludedMarker.push(marker.tempHoveredidx);
                    this.count--;
                    delete marker.tempHoveredidx;
                };
                break;
            case "Include":
                if(marker.hovered == true){
                }else{
                    marker.highlighted = true;
                    marker.hovered = true;
                    marker.setIcon(helpers.genSpaceMarker(true, marker.parkingspaceid));
                    var countvalue = this.count++;
                    marker.tempHoveredidx = Number(countvalue);
                }
                    break;
            case "Exclude":
                if(marker.hovered == true){
                    marker.highlighted = false;
                    marker.hovered = false;
                    marker.setIcon(helpers.genSpaceMarker(false, marker.parkingspaceid));
                    this.currentExcludedMarker.push(marker.tempHoveredidx);
                    this.count--;
                    delete marker.tempHoveredidx;

                }
                break;
        }
    },

    displayNewMarkerName: function(that, marker, event){

        this.infoWindow.close();
        var name = that.spotlookup[marker.parkingspaceid].markers.nameMarkers[0];
        name.label= {
            text: marker.label.text,
            color: 'black',
            fontSize: '12px'
        };
        // that.map.panTo(marker.position);
        name.setVisible();
        name.setMap(that.map)

    },

    handleSubmit: function(e) {
        e.stopPropagation();
        e.preventDefault();

        var spacesToBeRenamed = [];
        var allParkingSpaces = this.state.parkingspaces;
        var ids = this.state.spacestobeRenamed;
        var hoveredSpaces = this.state.hoveredMarkers;
        for(var j  in allParkingSpaces){
            for(var i in ids)
            if(allParkingSpaces[j].parkingspaceid == ids[i]){
                spacesToBeRenamed.push(allParkingSpaces[j])
            }
        }

        for(var k  in spacesToBeRenamed){
            for(var l in hoveredSpaces)
                if(spacesToBeRenamed[k].parkingspaceid == hoveredSpaces[l].parkingspaceid){
                    // spacesToBeRenamed.push(allParkingSpaces[j])
                    var labelName = hoveredSpaces[l].label.text;
                    spacesToBeRenamed[k].name = labelName;
                    spacesToBeRenamed[k].metadata.name = labelName;
                    spacesToBeRenamed[k].metadata.parkingSpaceType ="surface-lot";
                }
        }
        this.map.setOptions({draggableCursor:'url(maps.gstatic.com/mapfiles/openhand_8_8.cur),default'});

        ReactBootstrap.Dispatcher.emit("Parkingspacemap.renameSpaces", spacesToBeRenamed );
        this.setState({renameSpacesButton:false, hoveredMarkers:{}})
        for(var p = 0 ;p< this.markers.iconMarkers.length; p++){
                if(this.markers.iconMarkers[p].hovered){
                   this.markers.iconMarkers[p].hovered = false;
                   this.markers.iconMarkers[p].highlighted = false;
                   this.markers.iconMarkers[p].label.text = " ";
                    this.markers.iconMarkers[p].setIcon(helpers.genSpaceMarker(false,this.markers.iconMarkers[p].parkingspaceid )); // @TODO: add type at the end of it
                    delete this.markers.iconMarkers[p].hoveredIndex
                }
            }
        this.count = 0;
        this.infoWindow.close();
    },

    handleCancel:function(){

        this.setState({
              renameSpacesButton:false,
              hoveredMarkers:[],
              newParkingSpaceNames:[],
              spacestobeRenamed:[],
              spaceRenamesGenerated:false,
        });
        this.map.setOptions({draggableCursor:'url(maps.gstatic.com/mapfiles/openhand_8_8.cur),default'});

        for(var p = 0 ;p< this.markers.iconMarkers.length; p++){
            if(this.markers.iconMarkers[p].hovered){
                this.markers.iconMarkers[p].hovered = false;
                this.markers.iconMarkers[p].highlighted = false;
                this.markers.iconMarkers[p].label.text = " ";
                this.markers.iconMarkers[p].setIcon(helpers.genSpaceMarker(false, this.markers.iconMarkers[p].parkingspaceid )); // @TODO: add type at the end of it
                delete this.markers.iconMarkers[p].hoveredIndex

            }
        }
        this.count = 0;
        this.infoWindow.close();

        //emit an event to cancel the rename spaces entirely,
        // pass an empty for the selectedSpaces
        // expand the list section and minimize the map basically
        ReactBootstrap.Dispatcher.emit("Parkingspacemap.cancelRenameSpaces", [] );
        this.togglemap();

    },

    mapCenterLatLng: function () {
        return new google.maps.LatLng(
            this.props.site.latitude,
            this.props.site.longitude );
    },

    //Toggle the Parking Zones
    toggleZone:function(){
        if(this.refs.toggleZone.getDOMNode().checked){
            var bounds = new google.maps.LatLngBounds();
            if (this.props.parkingzones.length == 0) {
                bounds.extend(this.mapCenterLatLng());
            }
            this.zonelookup = {};
            for (var i=0; i<this.props.parkingzones.length; i++) {

                if(this.props.parkingzones[i].type === "Demarcated"){
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

                    this.zones.push(zone);
                    bounds.extend(zoneCoords[0]);
                    this.map.fitBounds(bounds);
                    bounds.extend(zoneCoords[2]);
                    this.map.fitBounds(bounds);
                }
            };
        }else {
            for (var i=0; i<this.zones.length; i++) {
                this.zones[i].setMap(null);
            }
        }
    },

    toggleVideoNodes:function(){

        if(this.refs.toggleVideoNodes.getDOMNode().checked){
            // Video Node Markers
            var bounds = new google.maps.LatLngBounds();
            if (this.props.nodes.length == 0) {
                bounds.extend(this.mapCenterLatLng());
            }
            this.nodeMarkers = [];
            this.unmappednodes = [];
            var latlng;
            for (var i = 0; i < this.props.nodes.length; i++) {

                // if()
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

                // determine initial marker icon
                var nodeicon = helpers.genMarker(helpers.setNodeMarker(this.props.nodes[i]));

                // create the marker
                var nodemarker = new google.maps.Marker({
                    idx: i,
                    position: latlng,
                    map: this.map,
                    icon: nodeicon,
                    prevIcon: nodeicon,
                    draggable: false,
                    nodeid: this.props.nodes[i].nodeid,
                    name: this.props.nodes[i].name,
                    level: this.props.nodes[i].level,
                    building: this.props.nodes[i].building,
                    type: helpers.modelType(this.props.nodes[i].model),
                    lightLevel: "",
                    visible: true, // (!this.multiLevel || this.props.nodes[i].level == this.levels[0]),
                    selected: false
                });

                this.nodeMarkers.push(nodemarker);
                // this.nodeidlookup[this.props.nodes[i].nodeid] = i;
                bounds.extend(latlng);

            };
        }else{
            for (var i=0; i< this.nodeMarkers.length; i++) {
                this.nodeMarkers[i].setMap(null);
            }
        }

    },


    handleChange: function (key) {
        return function (e) {
            console.log(key);
            switch(key){
                case 'action':
                    if(e.target.value == "Include"  || e.target.value == "Exclude"  ){
                        this.map.setOptions({draggableCursor:'url(/imgs/paintbrush_cursor.png),default'});
                    }else{
                        this.map.setOptions({draggableCursor:'url(maps.gstatic.com/mapfiles/openhand_8_8.cur),default'});
                    }
                    break;
                case 'event':
                    if(e.target.value == "Hover" ||  e.target.value == "Click" ){
                        this.map.setOptions({draggableCursor:'url(/imgs/paintbrush_cursor.png),default'});
                    }else{
                        this.map.setOptions({draggableCursor:'url(maps.gstatic.com/mapfiles/openhand_8_8.cur),default'});
                    }
                    break;
                default:
                    this.map.setOptions({draggableCursor:'url(maps.gstatic.com/mapfiles/openhand_8_8.cur),default'});
                    break;
            }
        }.bind(this);
    },

    componentDidMount: function () {

        this.spotStyles = {
            highlighted: {strokeWeight: 6},
            normal: {strokeWeight: 2}
        };
        // one infoWindow object that will be re-used for multiple purposes
        this.infoWindow = new google.maps.InfoWindow();
        var nightMap = new google.maps.StyledMapType(helpers.getNightMap(), {name: "Night"});
        var mapOptions = {
            center: this.mapCenterLatLng(),
            zoom: this.props.initialZoom,
            mapTypeControlOptions: {
                mapTypeIds: ['roadmap', 'night', 'satellite']
            },
            mapTypeId: localStorage.getItem('parkingspaceMapType') === null
                ?google.maps.MapTypeId.ROADMAP
                :localStorage.getItem('parkingspaceMapType'),
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
        this.map = new google.maps.Map($("#parkingspacemap").get(0), mapOptions);
        this.map.mapTypes.set('night', nightMap);

        var that = this;
        google.maps.event.addListener( this.map, 'maptypeid_changed', function() {
            localStorage.setItem('parkingspaceMapType', that.map.getMapTypeId());
        });

        this.spotlookup = {}; // to dispaly the demarcated spaces
        this.zonelookup = {}; // to display the non-dematrcated spaces
        this.markerlookup = {};
        this.markers = {
            iconMarkers:[],
            nameMarkers:[]
        };
        // Parking Space Markers
        for (var i=0; i<this.props.parkingspaces.length; i++) {
            var parkingspace = this.props.parkingspaces[i];

            var marker = {};
            var nameMarker = {};
            var spotmarker = {};
            this.spotlookup[parkingspace.parkingspaceid] = {spots : [], markers:{iconMarkers:[], nameMarkers:[]}};

            var spot = parkingspace.parkingspaceid;

            if (parkingspace.demarcated) {

                // #### Demarcated Spaces to be visible
                var box = {
                    "lat":[
                        parkingspace.world.lat[0],
                        parkingspace.world.lat[1],
                        parkingspace.world.lat[2],
                        parkingspace.world.lat[3],
                    ],
                    "lon":[
                        parkingspace.world.lon[0],
                        parkingspace.world.lon[1],
                        parkingspace.world.lon[2],
                        parkingspace.world.lon[3]
                    ]
                };


                    spotmarker = new google.maps.Circle({
                        idx: i,
                        strokeWeight: 2,
                        coords:box,
                        strokeColor: "#0088CE",
                        strokeOpacity: 0.8,
                        fillColor: "#0088CE",
                        fillOpacity: 0.10,
                        nodeid: parkingspace.nodeid,
                        parkingspaceid: parkingspace.parkingspaceid,
                        spotid: spot,
                        center: this.getCenter(box),
                        radius:1.5,
                        highlighted: false,
                        type:"spot"
                    });

                    marker = new google.maps.Marker({
                        idx: i,
                        box:box,
                        position: this.getCenter(box),
                        map: this.map,
                        icon: helpers.genSpaceMarker(false, parkingspace.parkingspaceid, "spot"),
                        draggable:false,
                        label:{
                          text:" "
                        },
                        optimized: false,
                        highlighted: false,
                        name: parkingspace.name,
                        parkingspaceid: parkingspace.parkingspaceid,
                        nodeid: parkingspace.nodeid,
                        title: parkingspace.name,
                        hovered:false,
                        type:"spot"
                    });
                    nameMarker = new google.maps.Marker({
                        idx: i,
                        position: this.getCenter(box),
                        icon: helpers.genNameMarker(),
                        map: this.map,
                        visible: false,
                        label: {
                            text: parkingspace.name,
                            color: 'black',
                            fontSize: '12px'
                        },
                        draggable:false,
                        optimized: false,
                        highlighted: false,
                        name: parkingspace.name,
                        parkingspaceid: parkingspace.parkingspaceid,
                        nodeid: parkingspace.nodeid,
                        title: parkingspace.parkingspaceid,
                        type:"spot"
                    });
                    this.spotlookup[parkingspace.parkingspaceid].spots.push(spotmarker);

                    this.spotlookup[parkingspace.parkingspaceid].markers.iconMarkers.push(marker);
                    this.spotlookup[parkingspace.parkingspaceid].markers.nameMarkers.push(nameMarker);
                    spotmarker.setMap(this.map);
            }else{

                // #### Non Demarcated Spaces

                if ($.isArray(parkingspace.world_bounding_box)) {
                    var wbb = {lon: [], lat: []};
                    for (var j=0; j<4; j++) {
                        wbb.lon[j] = parkingspace.world_bounding_box[j].longitude;
                        wbb.lat[j] = parkingspace.world_bounding_box[j].latitude;
                    };
                    // delete parkingspace.world_bounding_box;
                    // deep copy
                    parkingspace.world_bounding_box = JSON.parse(JSON.stringify(wbb));
                };

                // var parkingzone = parkingspace[i];

                this.zonelookup[parkingspace.parkingzoneid] = i;

                // Define the LatLng coordinates for the polygon's path.
                var zoneCoords = [];
                for (var j=0; j<4; j++) {
                    zoneCoords.push({
                        lat: parkingspace.world_bounding_box.lat[j],
                        lng: parkingspace.world_bounding_box.lon[j]
                    });
                };

                // Construct the polygon.
                // var pzColor = helpers.parkingColor(100*(parkingspace.occupied_spaces/parkingspace.max_spaces));
                spotmarker = new google.maps.Polygon({
                    idx: i,
                    paths: zoneCoords,
                    strokeColor: "#0088CE",
                    strokeOpacity: 0.8,
                    strokeWeight: 2,
                    fillColor: "#0088CE",
                    fillOpacity: 0.10,
                    spotid: spot,
                    nodeid: parkingspace.nodeid,
                    parkingzoneid: parkingspace.parkingzoneid,
                    parkingspaceid:parkingspace.parkingspaceid,
                    occupied_spaces: parkingspace.occupied_spaces,
                    max_spaces: parkingspace.max_spaces,
                    center: this.getCenter(parkingspace.world_bounding_box),
                    active: parkingspace.active,
                    highlighted: false,
                    type:"zone"
                });
                marker = new google.maps.Marker({
                    idx: i,
                    paths: zoneCoords,
                    box:parkingspace.world_bounding_box,
                    position: this.getCenter(parkingspace.world_bounding_box),
                    map: this.map,
                    icon: helpers.genSpaceMarker(false, parkingspace.parkingspaceid,"zone"),
                    draggable:false,
                    label:{
                        text:" "
                    },
                    optimized: false,
                    highlighted: false,
                    name: parkingspace.name,
                    parkingspaceid: parkingspace.parkingspaceid,
                    nodeid: parkingspace.nodeid,
                    title: parkingspace.name,
                    hovered:false,
                    type:"zone"
                });
                nameMarker = new google.maps.Marker({
                    idx: i,
                    paths: zoneCoords,
                    position: this.getCenter(parkingspace.world_bounding_box),
                    icon: helpers.genNameMarker(),
                    map: this.map,
                    visible: false,
                    label: {
                        text: parkingspace.name,
                        color: 'black',
                        fontSize: '12px'
                    },
                    draggable:false,
                    optimized: false,
                    highlighted: false,
                    name: parkingspace.name,
                    parkingspaceid: parkingspace.parkingspaceid,
                    nodeid: parkingspace.nodeid,
                    title: parkingspace.parkingspaceid,
                    type:"zone"
                });
                this.spotlookup[parkingspace.parkingspaceid].spots.push(spotmarker);

                this.spotlookup[parkingspace.parkingspaceid].markers.iconMarkers.push(marker);
                this.spotlookup[parkingspace.parkingspaceid].markers.nameMarkers.push(nameMarker);
                spotmarker.setMap(this.map);

            }


            that.spaces.push(spotmarker);
            that.markers.iconMarkers.push(marker);
            that.markers.nameMarkers.push(nameMarker);


            google.maps.event.addListener(spotmarker,'click', function(event){

                console.log("this onclick of the spot marker", this);
                if(that.props.selected_spaces.length >1){
                    if(this.type=="zone"){
                        this.setOptions(that.spotStyles.highlighted);
                        for (var i=0; i< that.spaces.length; i++) {
                            if (that.spaces[i].spotid == that.parkingspaceid) {
                                that.showSpaceTooltip(that.spaces[i]);
                            }
                        }
                        that.props.selected_spaces.push(this.parkingspaceid);
                        ReactBootstrap.Dispatcher.emit('Parkingspacemap.multiSelect', that.props.selected_spaces)
                        console.log("that.props.selected_spaces", that.props.selected_spaces)

                    }else if(this.type=="spot"){
                        this.setOptions(that.spotStyles.highlighted);
                        for (var i=0; i< that.spaces.length; i++) {
                            if (that.spaces[i].spotid == that.parkingspaceid) {
                                that.showSpaceTooltip(that.spaces[i]);
                            }
                        }
                        that.props.selected_spaces.push(this.parkingspaceid);
                        ReactBootstrap.Dispatcher.emit('Parkingspacemap.multiSelect', that.props.selected_spaces)
                        console.log("that.props.selected_spaces", that.props.selected_spaces)

                    }
                }else{

                    if(this.type=="zone"){

                        for (var i=0; i<that.spaces.length; i++) {
                            if (that.spaces[i].highlighted) {
                                that.spaces[i].highlighted = false;
                                that.spaces[i].setOptions(that.spotStyles.normal);
                            }
                        }
                        this.setOptions(that.spotStyles.highlighted);
                        for (var i=0; i< that.spaces.length; i++) {
                            if (that.spaces[i].spotid == this.parkingspaceid) {
                                that.showSpaceTooltip(that.spaces[i]);
                            }
                        }
                        ReactBootstrap.Dispatcher.emit('Parkingspacemap.selectSpace', this.parkingspaceid)

                    }else if(this.type=="spot"){
                        for (var i=0; i<that.spaces.length; i++) {
                            if (that.spaces[i].highlighted) {
                                that.spaces[i].highlighted = false;
                                that.spaces[i].setOptions(that.spotStyles.normal);
                            }
                        }
                        this.setOptions(that.spotStyles.highlighted);
                        for (var i=0; i< that.spaces.length; i++) {
                            if (that.spaces[i].spotid == this.parkingspaceid) {
                                that.showSpaceTooltip(that.spaces[i]);
                            }
                        }
                        ReactBootstrap.Dispatcher.emit('Parkingspacemap.selectSpace', this.parkingspaceid)
                    }
                }

            });

            google.maps.event.addListener(marker, 'mouseover', function(event) {
                if ($("#event").val() == "Hover") {
                    console.log(" getCenter inside the hover marker " ,this);
                    that.markerAction(this);
                }
            });

            google.maps.event.addListener(marker, 'click', function() {
                if ($("#event").val() == "Click") {
                    that.markerAction(this);
                }
            });

            google.maps.event.addListener(that.map, 'click', function() {
                    var temp = [], hoveredMarkers=[], hoveredIndices=[], missingIndices=[];

                    // find all the hovered spaces
                    for(var p = 0 ;p< that.markers.iconMarkers.length; p++){
                        if(that.markers.iconMarkers[p].hovered){
                           hoveredMarkers.push(that.markers.iconMarkers[p]);
                            hoveredIndices.push(that.markers.iconMarkers[p].tempHoveredidx);
                            temp.push(that.markers.iconMarkers[p].parkingspaceid)
                        }
                    }

                    // Sort the hovered markers to update if there was any excluding space
                    hoveredMarkers.sort(function(a, b){
                        return a.tempHoveredidx - b.tempHoveredidx
                    });

                    // Sort just the indices to find which is missing indice in between
                    hoveredIndices = hoveredIndices.sort();
                    function absent (hoveredIndices){
                        var mia= [], min= Math.min.apply('',hoveredIndices), max= Math.max.apply('',hoveredIndices);
                        while(min<max){
                            if(hoveredIndices.indexOf(++min)== -1) mia.push(min);
                        }
                        return mia;
                    }
                    missingIndices = absent(hoveredIndices); // find the missing indice if any inbetween spot was excluded


                    // Check how many spaces were excluded and update the name sequence
                    if(that.currentExcludedMarker.length == 1){ // Only One was Excluded
                        var value = Number(that.currentExcludedMarker[0]);
                        if (value == 0) {
                            for (var p = 0; p < hoveredMarkers.length; p++) {
                                hoveredMarkers[p].tempHoveredidx = Number(value);
                                value++;
                            }
                        } else {
                            var missingIndex = missingIndices[0]
                            for (var p = 0; p < hoveredMarkers.length; p++) {
                                if (hoveredMarkers[p].tempHoveredidx >= missingIndex + 1) {
                                    hoveredMarkers[p].tempHoveredidx = missingIndex
                                    missingIndex++;
                                }
                            }
                        }
                    }else if(that.currentExcludedMarker.length > 1){ // More than one were excluded
                        // find the smallest and then update the rest with the same
                        var smallestIndice = Math.min.apply(null, missingIndices);
                        for (var p = 0; p < hoveredMarkers.length; p++) {
                            if (hoveredMarkers[p].tempHoveredidx >= smallestIndice + 1) {
                                hoveredMarkers[p].tempHoveredidx = smallestIndice
                                smallestIndice++;
                            }
                        }
                    }
                    that.setState({hoveredMarkers})
                    that.map.setOptions({draggableCursor:'url(maps.gstatic.com/mapfiles/openhand_8_8.cur),default'});
                ReactBootstrap.Dispatcher.emit('Parkingspacemap.multiSelect', temp)
            });

            google.maps.event.addListener(marker, 'mouseover', function(event) {
                if(that.state.spaceRenamesGenerated){
                    for(var j in that.state.hoveredMarkers){
                        if(this.parkingspaceid == that.state.hoveredMarkers[j].parkingspaceid){
                            that.displayNewMarkerName(that ,this, event)
                        }
                    }
                }
            });

        };


        var that = this;
        var listener = google.maps.event.addListener(this.map, "idle", function() {
            if (that.map.getZoom() > 20) that.map.setZoom(20);
            google.maps.event.removeListener(listener);
        });

        ReactBootstrap.Dispatcher.on('Parkingspacelist.select', function(parkingspaceid) {
            for (var i=0; i< that.spaces.length; i++) {
                if (that.spaces[i].spotid == parkingspaceid) {
                    that.showSpaceTooltip(that.spaces[i]);
                    that.spaces[i].setOptions(that.spotStyles.highlighted);
                    that.spaces[i].highlighted = true;
                    var name = that.spotlookup[parkingspaceid].markers[0];
                   ReactBootstrap.Dispatcher.emit('Parkingspacemap.zoomin',parkingspaceid);
                } else {
                    if (that.spaces[i].highlighted) {
                        that.spaces[i].highlighted = false;
                        that.spaces[i].setOptions(that.spotStyles.normal);
                    }
                }
            }
        });

        ReactBootstrap.Dispatcher.on("Parkingspacelist.multiSelect", function (parkingspaceids) {
        // If all get Deselected from the list
            if(parkingspaceids.length == 0){
                for (var i=0; i< that.spaces.length; i++) {
                        that.spaces[i].highlighted = false;
                        that.spaces[i].setOptions(that.spotStyles.normal);
                    ReactBootstrap.Dispatcher.emit("Parkingspacemap.zoomout","none")
                }

        // If only a few get selected - hight those and deselect the others
            }else if(parkingspaceids.length >= 1 && parkingspaceids.length < that.spaces.length){
                for (var p=0; p< that.spaces.length; p++) {
                        that.spaces[p].highlighted = false;
                        that.spaces[p].setOptions(that.spotStyles.normal);
                }

                for (var i=0; i< that.spaces.length; i++) {
                    for(var j = 0; j< parkingspaceids.length; j++){
                        if(that.spaces[i].parkingspaceid == parkingspaceids[j] ){
                            that.spaces[i].highlighted = true;
                            that.spaces[i].setOptions(that.spotStyles.highlighted);
                        }
                    }
                }
                ReactBootstrap.Dispatcher.emit("Parkingspacemap.zoomout","multipleSelect")

        //  if all are selected then select all on the map
            }else if(parkingspaceids.length == that.spaces.length){
                for (var i=0; i< that.spaces.length; i++) {
                     if (that.spaces[i].highlighted) {
                         that.spaces[i].highlighted = false;
                         that.spaces[i].setOptions(that.spotStyles.normal);
                     }
                 }
                for (var i=0; i< that.spaces.length; i++) {
                    if (!that.spaces[i].highlighted) {
                        that.spaces[i].highlighted = true;
                        that.spaces[i].setOptions(that.spotStyles.highlighted);
                    }
                 }
                ReactBootstrap.Dispatcher.emit("Parkingspacemap.zoomout","multipleSelect")
            }
        });

        ReactBootstrap.Dispatcher.on('Parkingspacemap.zoomout', function (type) {

            if(type === "multipleSelect"){
                for (var i=0; i< that.markers.nameMarkers.length; i++) {
                    if (that.markers.nameMarkers[i].visible || that.markers.nameMarkers[i].visible == undefined){
                        that.markers.nameMarkers[i].setVisible(false)
                    }
                }
            }else if(type === "none"){
                for (var i=0; i< that.spaces.length; i++) {
                    if (that.spaces[i].highlighted) {
                        that.spaces[i].highlighted = false;
                        that.spaces[i].setOptions(that.spotStyles.normal);
                    }
                }
                for (var i=0; i< that.markers.nameMarkers.length; i++) {
                    if (that.markers.nameMarkers[i].visible || that.markers.nameMarkers[i].visible == undefined){
                        that.markers.nameMarkers[i].setVisible(false)
                    }
	            }
            }
            that.infoWindow.close();
            that.map.setZoom(20);
        });

        ReactBootstrap.Dispatcher.on('Parkingspacemap.zoomin', function (parkingspaceid) {
            var z = that.spotlookup[parkingspaceid].spots[0];
            var name = that.spotlookup[parkingspaceid].markers.nameMarkers[0];

            for(var j in that.props.parkingspaces){
                if(that.props.parkingspaces[j].parkingspaceid == parkingspaceid){
                    var newName = that.props.parkingspaces[j].name;
                }else{
                    // display names of other spaces around the space that was selected
                    var otherName = that.spotlookup[that.props.parkingspaces[j].parkingspaceid].markers.nameMarkers[0];
                    otherName.label= {
                        text: that.props.parkingspaces[j].name,
                        color: 'black',
                        fontSize: '12px'
                    };
                    otherName.setVisible();
                }
            }
            name.label= {
                text: newName,
                    color: 'black',
                    fontSize: '12px'
            };
            // that.map.panTo(z.center);
            name.setVisible();
            name.setMap(that.map)
            that.map.setZoom(21);
        });

        ReactBootstrap.Dispatcher.on('Parkingspacemultirenameform.multispacerename', function(newParkingSpaceNames, selectedSpaces, spaceRenamesGenerated){
            that.togglemap();
            var newNames = newParkingSpaceNames.slice(0, selectedSpaces.length);
            var newParkingSpaceNames = that.state.newParkingSpaceNames;
            var hoveredMarkers = that.state.hoveredMarkers;
            for(var x in newNames){
                for(var y in hoveredMarkers){
                    if(Number(x) === hoveredMarkers[y].tempHoveredidx ){
                        var label= {
                            text: newNames[x],
                            color: 'white',
                            fontSize: '12px'
                        };
                        hoveredMarkers[y].label = label;
                    }
                }
            }
            var content = that.buildRenameInfoWindow(true);
            var position = that.getCenter(hoveredMarkers[0].box);
            that.infoWindow.setContent(content);
            that.infoWindow.setPosition(position);
            that.infoWindow.open(that.map); // open the info window to instruct the user of the next steps

            that.map.setOptions({draggableCursor:'url(/imgs/paintbrush_cursor.png),default'});
            that.setState({renameSpacesButton:true,
                            newParkingSpaceNames:newNames ,
                            spacestobeRenamed:selectedSpaces,
                            spaceRenamesGenerated,
                            hoveredMarkers
            })

        });

        ReactBootstrap.Dispatcher.on('Parkingspacemultirenameform.cancelRenameSpaces', function(selectedSpaces){
            that.setState({
                renameSpacesButton:false,
                hoveredMarkers:[],
                newParkingSpaceNames:[],
                spacestobeRenamed:[],
                spaceRenamesGenerated:false,
            });
            that.map.setOptions({draggableCursor:'url(maps.gstatic.com/mapfiles/openhand_8_8.cur),default'});

            for(var p = 0 ;p< that.markers.iconMarkers.length; p++){
                if(that.markers.iconMarkers[p].hovered){
                    that.markers.iconMarkers[p].hovered = false;
                    that.markers.iconMarkers[p].hovered = false;
                    that.markers.iconMarkers[p].highlighted = false;
                    that.markers.iconMarkers[p].label.text = " ";
                    console.log("that.markers.iconMarkers[p].parkingspaceid", that.markers.iconMarkers[p].parkingspaceid)
                    that.markers.iconMarkers[p].setIcon(helpers.genSpaceMarker(false, that.markers.iconMarkers[p].parkingspaceid)); // @TODO: add type at the end of it
                    delete that.markers.iconMarkers[p].hoveredIndex
                }
            }
            this.count = 0;
        });
    },

    changeActive: function (active) {
        if (this.currentActive == active) {
            return;
        }
        this.infoWindow.close();
        // for (var i = 0; i < this.zones.length; i++) {
        //     var m = this.zones[i];
        //     m.setVisible(active == "all" || m.active == active);
        //     var spots = this.spotlookup[m.parkingzoneid].spots;
        //     for (var j = 0; j < spots.length; j++) {
        //         spots[j].setVisible(active == "all" || m.active == active);
        //     };
        // };
        // this.markerCluster.repaint();
        // this.currentActive = active;
        // ReactBootstrap.Dispatcher.emit("Parkingzonemap.filter", [{columnId:"active",value:active}]);
    },

    componentWillUnmount: function() {
        // ReactBootstrap.Dispatcher.removeAllListeners("Parkingzoneform.showNodes");
        ReactBootstrap.Dispatcher.removeAllListeners("Parkingspacelist.select");
        ReactBootstrap.Dispatcher.removeAllListeners("Parkingspacelist.selectSpace");
        ReactBootstrap.Dispatcher.removeAllListeners("Parkingspacemap.zoomin");
        ReactBootstrap.Dispatcher.removeAllListeners("Parkingspacemap.zoomout");
        ReactBootstrap.Dispatcher.removeAllListeners("Parkingspacelist.multiSelect");
        ReactBootstrap.Dispatcher.removeAllListeners("Parkingspacemap.multiSelect");
        ReactBootstrap.Dispatcher.removeAllListeners("Parkingspacemultirenameform.multispacerename");
        if (typeof NSN.source == 'object') {
            NSN.source.close();
            NSN.source = null;
        };

    },

    render: function () {
        var that = this;
        var Control = React.createClass({
            render: function(){
                return (
                    <div id="markercontrol" style={{position:"absolute",zIndex:"999999",top:"10px",right:"60px",
                        padding:"4px 8px",fontSize:"16px",backgroundColor:"rgba(255,255,255,0.85)"}}>
                        <span style={{fontSize:"18px"}}>Selection:</span> &nbsp;
                        <select ref="action" id="action" onChange={that.handleChange('action')}>
                            <option>Toggle</option>
                            <option>Include</option>
                            <option>Exclude</option>
                        </select>
                        &nbsp; <span style={{fontSize:"18px"}}>on</span> &nbsp;
                        <select ref="event" id="event" onChange={that.handleChange('event')}>
                            <option>Click</option>
                            <option>Hover</option>
                        </select>
                        <p style={{fontSize: "15psx",margin: "0px"}}> End the Selection with a click on the map</p>
                    </div>
                )
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
                <div id="parkingspacemap" className='mapCanvas parkingspacemapCanvas'></div>
                <Control />
                <div style={{position:"absolute",zIndex:"999999",top:"11px",left:"170px", padding:"4px 8px",fontSize:"14px",backgroundColor:"rgba(255,255,255,0.85)"}}>
                    <input title="Toggle Zones" ref="toggleZone" type="checkbox" style={{height:"16px",width:"16px", top:"2px",marginLeft:"11px"}} onChange={this.toggleZone} /> Show Zones
                    <input title="Toggle Video Nodes" ref="toggleVideoNodes" type="checkbox" style={{height:"16px",width:"16px",top:"2px", marginLeft:"12px"}} onChange={this.toggleVideoNodes} /> Show Video Nodes
                </div>
                    {this.state.renameSpacesButton ? (
                        <div className="col-sm-12"style={{position:"absolute",zIndex:"9999999",top:"102px",right:"12px",
                            padding:"4px 8px",fontSize:"16px",width:"230px"}}>
                            <button type="button" className="ns-delete-btn" id="cancelMapRenameSpace" onClick={this.handleCancel} style={{float:"right"}}>
                                Cancel </button>
                            <button type="button" className="ns-save-btn" id="saveMapRenameSpace" onClick={this.handleSubmit} style={{width: "90px !important"}}>
                                Save</button>
                        </div>
                    ):""}

            </div>
        );
    }
});

module.exports = Parkingspacemap;


