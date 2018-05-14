var React = require('react');
var Nodemap = React.createClass({

  propTypes: {
    nodes: React.PropTypes.array.isRequired
  },

  getDefaultProps: function () {
    return {
      initialZoom: 8,
      mapCenterLat: 3.6425569,
      mapCenterLng: -129.4073126,
    };
  },
  componentDidMount: function () {
    var mapOptions = {
      center: this.mapCenterLatLng(),
      zoom: this.props.initialZoom      
    },
    map = new google.maps.Map(this.getDOMNode(), mapOptions);

    $.get(this.props.data, function (data) {
      var bounds = new google.maps.LatLngBounds();
      for (var i=0;i<this.props.nodes.length;i++){
        var latlng = new google.maps.LatLng(this.props.nodes[i].Latitude, this.props.nodes[i].Longitude); 
        var marker = new google.maps.Marker({
          position: latlng, 
          map: map
        });
        bounds.extend(latlng);

        // Window Content
        var content = '<span>' + '<b> ID:</b>' + this.props.nodes[i].ID + '</span>' + "<br />" + '<span>' + '<b> Hardware Model:</b>' + this.props.nodes[i]["Hardware Model"] + '</span>' + "<br />" + '<span>' + '<b> Local IP :</b>' + this.props.nodes[i]["Local IP"] + '</span>';

        var infowindow = new google.maps.InfoWindow;

        google.maps.event.addListener(marker,'click', (function(marker,content){ 
          return function() {
            infowindow.setContent(content);
            infowindow.open(map,marker);
          };
        })(marker,content,infowindow)); 
      }
      map.fitBounds(bounds);
    }.bind(this));
    this.setState({map: map});
  },
  mapCenterLatLng: function () {
    var props = this.props;
    return new google.maps.LatLng(props.mapCenterLat, props.mapCenterLng);
  },
  render: function () {
    return (
      <div className='mapCanvas'></div> 
    );
  }
});

module.exports = Nodemap;





 