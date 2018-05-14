import classNames from 'classnames';
import { State, Navigation } from 'react-router';
import Otaform from 'components/ota/otaform';
import helpers from 'global/utils/helpers';
import { Modal } from 'react-bootstrap';

var Otadetail = React.createClass({

  getInitialState: function() {
      return null;
  },

  propTypes: {
    otas: React.PropTypes.array.isRequired,
    firmwares: React.PropTypes.array.isRequired,
    otaID: React.PropTypes.string.isRequired,
  },

  getOta: function(otaID) {
    var that = this;
    if (otaID == "-1") {
      return {
          otaid: "",
          success: "",
          target_type: "",
          firmwareid: "",
          nodeid: "",
          target_id: "",
          status: "",
          when: "",
          siteid: "",
          jobid: "",
          description: "",
          target_name: "",
          orgid: "",
          idx: -1
      };
    };

    $.ajax({
        url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/ota_status/' + otaID,
        data: '',
        method: 'GET',
        "xhrFields": {
            withCredentials: true
        },
        dataType: 'json',
        success: function(ota) {
          $("#loadingmsg")
              .html("Generating display.");
          console.log("Retrieving ota info: " + JSON.stringify(ota));

          if ('job_status' in ota){
            var jobInfo = ota.job_info;
            $.each(jobInfo, function(key,value){
                console.log("Video OTA Job Id",jobInfo.jobid);
                that.props.otaID = jobInfo.jobid;
            });
          }
          else{
            that.props.otaID = ota.jobid;
          }
          
          /*if(otaID == "d5301534-dae1-42dc-987e-a6b2c162b373"){
            var otadata = helpers.getFakeCoreOtaListData();
          }
          else{
            var otadata = helpers.getFakeVideoOtaListData();
          }

          this.setState([otadata]);*/

          this.setState([ota]);
        }.bind(that),
        error: function(jqXHR, status, error) {
          console.log("ajax failure (otas): " + status + " - " + error);
          $("#loadingmsg").html("Cannot retrieve OTA Details.  API reported error: " + error);
        }
    });
    
  },

    componentDidMount: function() {
        this.getOta(this.props.otaID);

        var that = this;
        ReactBootstrap.Dispatcher.on("Otaform.refresh",function(otaID){
          that.getOta(otaID);
        });
    },

    componentDidUpdate: function() {
        
    },

    componentWillReceiveProps: function(nextProps) {
      if (this.props.otaID != nextProps.otaID) {
          this.setState(this.getOta(nextProps.otaID));
      };
    },

    render: function () {
      if (this.props.show) {
        return (
          <div className="otaForm" >
            <Modal.Dialog style={{ zIndex: 100, marginTop: 80 }}>

              <Modal.Body>
              <a className="" id="otaoverlayClose" onClick={() => { this.props.hide() }}>   
                <img src='/imgs/Close1.svg' width='30' height='30'  style={{marginTop:'60px',marginRight:'8px', zIndex: 100} }/>
              </a>
                <Otaform firmwares={this.props.firmwares} ota={this.state} />
              </Modal.Body>

            </Modal.Dialog>
          </div>
        )
      }
      return null;
    }

});

module.exports = Otadetail;

