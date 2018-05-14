import classNames from 'classnames';
import helpers from 'global/utils/helpers';
import { State, Navigation } from 'react-router';

var ConfigView = React.createClass({

  propTypes: {
    show: React.PropTypes.bool.isRequired,
    context: React.PropTypes.string.isRequired,
    entity: React.PropTypes.object.isRequired
  },

  handleClose: function() {
    ReactBootstrap.Dispatcher.emit(helpers.ucfirst(this.props.context) + 'form.assignedconfig', 'close');
  },

  getAssignedConfig: function() {
    var that = this, configID = "";
  
    if (this.props.context == "node") {
      configID = this.props.entity.configid;

      $('#config-heading').empty();
      $('.configAssigned').empty(); 

      if (configID!== "") {
        $.ajax({
          "url" : NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/configs/' + configID,
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
              console.log(key,item);

              if(key == "nodes"){
                item = JSON.stringify(item);
              }

              if((key == "networkYPasskey") || (key == "networkXPasskey")){
                
                item = item.replace(/[a-zA-Z0-9!@#$_^&*%]/g, "*");

                configData += '<tr><td style="text-align:center">' + key + '</td><td style="text-align:center">' + item + '</td></tr>';
              }
              else{
                configData += '<tr><td style="text-align:center">' + key + '</td><td style="text-align:center">' + item + '</td></tr>';
              }
            });

            $('#config-heading').append("Assigned configuration: " + data.name);
            $('.configAssigned').append(configData); 

          }.bind(that),
          "error" : function(jqXHR, textStatus, errorThrown) {
            noty({type:"error", text: "Could not retrieve assigned config: " + textStatus});
          }
        });
      }
    };
  },
  componentDidUpdate: function(){
      console.log(this.props.entity);
    if (this.props.show) {
        $(".config-display").fadeIn('fast');
        this.getAssignedConfig();

    } else {
        $(".config-display").fadeOut('fast');
    }
  },

  componentWillUnmount: function(){
  },

  render() {
   
    return (
      <div className="config-display">
        <div id="config-heading">Assigned Configuration: </div>
        <div id="config" style={{marginBottom:"60px"}}>
          <div className="infoMessage" style={{marginLeft:"20px",width:"100%",padding:"0",margin:"0",overflow:"auto",maxHeight:"400px"}}>
            <table className="configAssigned table table-striped" style={{marginBottom:"0px"}}>
            </table>
          </div>
        </div>

        <div style={{position:"absolute",bottom:"10px",right:"10px"}}>
          <button type="button" className="ns-close-btn" onClick={this.handleClose}>Close </button>
        </div>
      </div>
    );
  }
});



module.exports = ConfigView;
