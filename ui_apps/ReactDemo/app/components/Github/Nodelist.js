var React = require('react');

var Nodelist = React.createClass({
  propTypes: {
    nodes: React.PropTypes.array.isRequired
  },

  render: function(){
    var Nodelist = this.props.nodes.map(function(node, index){
      return (
        <tr key={index}>
          {node.ID && <td>{node.ID}</td>}
          {node["Hardware Model"] && <td>{node["Hardware Model"]}</td>}
          {node.Latitude && <td> {node.Latitude}</td>}
          {node.Longitude && <td>{node.Longitude}</td>}
          {node["Local IP"] && <td>{node["Local IP"]}</td>}
        </tr>
      );
    });
    return (
      <div className="panel panel-primary">      
        <div className="panel-body" style={{padding:"0px"}}>
          <table>
            <tbody>
              <tr>
                <td> ID </td>
                <td> Hardware Model </td>
                <td> Latitude </td>
                <td> Longitude </td>
                <td> Local IP </td>
              </tr>
              {Nodelist}
            </tbody>
          </table>
        </div>
      </div>
    )
  }
});

module.exports = Nodelist;