import classNames from 'classnames';
import helpers from 'global/utils/helpers';
import { State, Navigation } from 'react-router';

var ProvConfigView = React.createClass({

  propTypes: {
    show: React.PropTypes.bool.isRequired,
    context: React.PropTypes.string.isRequired,
    entity: React.PropTypes.object.isRequired
  },

  handleClose: function() {
    ReactBootstrap.Dispatcher.emit(helpers.ucfirst(this.props.context) + 'form.viewprovsettings', 'close');
  },

  getProvConfig: function() {
    var that = this, nodeID = "";
  
    if ((this.props.context == "node") && typeof this.props.entity != "undefined") {
      nodeID = this.props.entity.nodeid;

      //$('#provconfig-heading').empty();
      $('.provconfigAssigned').empty(); 
      if (nodeID!== "") {
        $.ajax({
          "url" : NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/nodes/' + nodeID + '/config/' + 'prov',
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
            $('.provconfigAssigned').append(configData); 
          }.bind(that),
          "error" : function(jqXHR, textStatus, errorThrown) {
            var configData = '';
            configData = '<h3 style="margin: 20px 35px;">Could not retrieve provisional configurations data.</h3>';
            $('.provconfigAssigned').append(configData); 
            noty({type:"error", text: "Could not retrieve provisional configurations data: " + textStatus});
          }
        }); 
      };
    };
  },

  componentDidUpdate: function(){
    if (this.props.show) {
        $(".provconfig-display").fadeIn('fast');
        this.getProvConfig();

    } else {
        $(".provconfig-display").fadeOut('fast');
    }
  },

  componentWillUnmount: function(){
  },

  render() {
   
    return (
      <div className="provconfig-display">
        <div id="provconfig-heading">Provisional Configurations: </div>
        <div id="provconfig" style={{marginBottom:"60px"}}>
          <div className="infoMessage" style={{marginLeft:"20px",width:"100%",padding:"0",margin:"0",overflow:"auto",maxHeight:"400px"}}>
            <table className="provconfigAssigned table table-striped" style={{marginBottom:"0px"}}>
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



module.exports = ProvConfigView;
