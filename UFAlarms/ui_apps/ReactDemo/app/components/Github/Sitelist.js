var React = require('react');

var Sitelist = React.createClass({
  propTypes: {
    customerID: React.PropTypes.string.isRequired,
    sites: React.PropTypes.array.isRequired
  },
  render: function(){
    var Sitelist = this.props.sites.map(function(site, index){
      return (
        <li className="list-group-item" key={index}>
          {site.name && <h4>{site.name}</h4>}
          {site.city && <p> {site.city}, {site.state}</p>}
        </li>
      );
    });
    return (
      <div className="panel panel-primary">
        <div className="panel-heading"> Site List </div>
        <div className="panel-body" style={{padding:"0px"}}>
          <ul className="list-group" style={{marginBottom:"0px"}}>
            {Sitelist}
          </ul>
        </div>
      </div>
    )
  }
});

module.exports = Sitelist;