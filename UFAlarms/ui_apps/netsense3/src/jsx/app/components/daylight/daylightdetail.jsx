import classNames from 'classnames';
import { State, Navigation } from 'react-router';
import Daylightform from 'components/daylight/daylightform';
import helpers from 'global/utils/helpers';
import { Modal } from 'react-bootstrap';

var Daylightdetail = React.createClass({

  getInitialState: function() {
    return this.getDaylight(this.props.daylightID,this.props.daylights);
  },

  componentDidMount: function() {
    var that = this;
    $('body').on('keydown', function (e) {
      if($('.modal-body').is(":visible") && e.which === 27){
        that.props.hide();
      }
    });
  },

  propTypes: {
    daylights: React.PropTypes.array.isRequired,
    daylightID: React.PropTypes.string.isRequired
  },

  getDaylight: function(daylightID, daylights) {
    var that = this;
    if (daylightID == "0" || daylightID == "-1") {
      return {
          etdhprofileid: "0",
          name: "",
          highLux: 0,
          lowLux: 0,
          lowDriver: 0,
          minDriver: 0,
          highDriver: 0,
          minLux: 10,
          fastPoll: 30,
          slowPoll: 600,
          nodes:[],
          groups:[],
          triggers:[],
          scheduled:[{beginTime:"sunrise-90",endTime:"sunset+90"}],
          idx: -1
      };
    };
    for (var i=0; i<daylights.length; i++) {
      if (daylights[i].etdhprofileid == daylightID){
        $.ajax({
          url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID
              + '/daylightharvesting/' + daylights[i].etdhprofileid + '/triggers',
          data : '',
          xhrFields: {
             withCredentials: true
          },
          method : 'GET',
          dataType : 'json',
          context: {
            daylights: daylights[i]
          },
          success: function(data) {
            this.daylights.triggers = data;
            that.setState(this.daylights);
          },
          error: function() {
            console.log("error:cannot retrieve trigger data");
          }
        });
      }
    }
    return null;
  },

  componentWillReceiveProps: function(nextProps){
    this.setState(this.getDaylight(nextProps.daylightID, nextProps.daylights));
  },

  render: function() {
    if(this.state.triggers){
       if (this.props.show) {
        return (
          <div className="daylightForm">
            <Modal.Dialog style={{ zIndex: 100, marginTop: 80 }}>

              <Modal.Body>
              <a className=" " id="daylightoverlayClose" onClick={() => { this.props.hide() }}>
                <img src='/imgs/Close1.svg' width='30' height='30'  style={{marginTop:'60px',marginRight:'8px', zIndex: 100} }/>
              </a>
                <Daylightform daylight={this.state} groups={this.props.groups} />
              </Modal.Body>

            </Modal.Dialog>
          </div>
        )
      }
    }
    return null;

    /*if (this.props.daylights.length == 0 && this.props.daylightID != "0") {
      return (
        <div style={{padding:"20px 0px 0px 0px"}}>
          <div style={{textAlign:"center",fontSize:"54px",opacity:"0.5"}}>
              <Icon glyph="icon-fontello-info-circled" />
          </div>
          <h3 style={{textAlign:"center",lineHeight:"140%"}}>Authorized users can create Daylight Harvesting
          profiles<br />and assign them to Sites or Groups on this page.</h3>
        </div>
        );
    };
    if (this.props.daylightID == "-1") {
      return (
        <h2 style={{textAlign:"center",padding:"100px 0px"}}>Select a Profile.</h2>
        );
    };
    return (
      <Daylightform daylight={this.state} groups={this.props.groups} />
    ); */
  }

});

module.exports = Daylightdetail;

