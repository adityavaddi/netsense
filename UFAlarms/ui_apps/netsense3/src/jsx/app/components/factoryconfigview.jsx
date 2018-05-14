import classNames from 'classnames';
import helpers from 'global/utils/helpers';
import { State, Navigation } from 'react-router';

var FactoryConfigView = React.createClass({

  propTypes: {
    show: React.PropTypes.bool.isRequired,
    context: React.PropTypes.string.isRequired,
    entity: React.PropTypes.object.isRequired
  },

  handleClose: function() {
    ReactBootstrap.Dispatcher.emit(helpers.ucfirst(this.props.context) + 'form.viewfactorysettings', 'close');
  },

  getFactoryConfig: function() {
    var that = this, nodeID = "";
  
    if ((this.props.context == "node") && typeof this.props.entity != "undefined") {
      nodeID = this.props.entity.nodeid;

      //$('#factoryconfig-heading').empty();
      $('.factoryconfigAssigned').empty(); 
      if (nodeID!== "") {
        $.ajax({
          "url" : NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/nodes/' + nodeID + '/config/' + 'default',
          "type" : "GET",
          "xhrFields": {
             withCredentials: true
          },
          "dataType" : "json",
          "contentType" : "application/json",
          "processData" : false,
          "success" : function(data) {
            var configData = '';
            $.map(data, function(item, key) {
              configData += '<tr><td style="text-align:left;padding-left:35px">' + key + '</td><td style="text-align:left;padding-left:35px">' + item + '</td></tr>';
            });
            $('.factoryconfigAssigned').append(configData); 
          }.bind(that),
          "error" : function(jqXHR, textStatus, errorThrown) {
            var configData = '';
            configData = '<h3 style="margin: 20px 35px;">Could not retrieve factory configurations data.</h3>';
            $('.factoryconfigAssigned').append(configData); 
            noty({type:"error", text: "Could not retrieve factory configurations data: " + textStatus});
          }
        }); 
      };
    };
  },

  componentDidUpdate: function(){
    if (this.props.show) {
        $(".factoryconfig-display").fadeIn('fast');
        this.getFactoryConfig();

    } else {
        $(".factoryconfig-display").fadeOut('fast');
    }
  },

  componentWillUnmount: function(){
  },

  render() {
   
    return (
      <div className="factoryconfig-display">
        <div id="factoryconfig-heading">Factory Configurations: </div>
        <div id="factoryconfig" style={{marginBottom:"60px"}}>
          <div className="infoMessage" style={{marginLeft:"20px",width:"100%",padding:"0",margin:"0",overflow:"auto",maxHeight:"400px"}}>
            <table className="factoryconfigAssigned table table-striped" style={{marginBottom:"0px"}}>
            </table>
          </div>
        </div>

        <div style={{position:"absolute",bottom:"10px",right:"10px"}}>
          <button type="button" className="ns-form-btn" onClick={this.handleClose}>
          <b> Close </b> </button>
        </div>
      </div>
    );
  }
});



module.exports = FactoryConfigView;
