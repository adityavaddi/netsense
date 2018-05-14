import classNames from 'classnames';
import helpers from 'global/utils/helpers';
import { State, Navigation } from 'react-router';

var LocalConfigView = React.createClass({

  propTypes: {
    show: React.PropTypes.bool.isRequired,
    context: React.PropTypes.string.isRequired,
    entity: React.PropTypes.object.isRequired
  },

  handleClose: function() {
    ReactBootstrap.Dispatcher.emit(helpers.ucfirst(this.props.context) + 'form.viewlocalsettings', 'close');
  },

  getLocalConfig: function() {
    var that = this, nodeID = "";
  
    if ((this.props.context == "node") && typeof this.props.entity != "undefined") {
      nodeID = this.props.entity.nodeid;

      //$('#localconfig-heading').empty();
      $('.localconfigAssigned').empty(); 
      if (nodeID!== "") {
        $.ajax({
          "url" : NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/nodes/' + nodeID + '/config/' + 'local',
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

            $('.localconfigAssigned').append(configData); 

          }.bind(that),
          "error" : function(jqXHR, textStatus, errorThrown) {
            var configData = '';
            configData = '<h3 style="margin: 20px 35px;">Could not retrieve local configurations data.</h3>';
            $('.localconfigAssigned').append(configData); 
            noty({type:"error", text: "Could not retrieve local configurations data: " + textStatus});
          }
        }); 
      };
    };
  },
  componentDidUpdate: function(){
    if (this.props.show) {
        $(".localconfig-display").fadeIn('fast');
        this.getLocalConfig();

    } else {
        $(".localconfig-display").fadeOut('fast');
    }
  },

  componentWillUnmount: function(){
  },

  render() {
    return (
      <div className="localconfig-display">
        <div id="localconfig-heading">Local Configurations: </div>
        <div id="localconfig" style={{marginBottom:"60px"}}>
          <div className="infoMessage" style={{marginLeft:"20px",width:"100%",padding:"0",margin:"0",overflow:"auto",maxHeight:"400px"}}>
            <table className="localconfigAssigned table table-striped" style={{marginBottom:"0px"}}>
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


module.exports = LocalConfigView;
