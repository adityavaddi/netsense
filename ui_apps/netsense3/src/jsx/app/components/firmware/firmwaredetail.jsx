import classNames from 'classnames';
import { State, Navigation } from 'react-router';
import Firmwareform from 'components/firmware/firmwareform';
import helpers from 'global/utils/helpers';
import { Modal } from 'react-bootstrap';

var Firmwaredetail = React.createClass({

  getInitialState: function() {
    return this.getFirmware(this.props.firmwareID,this.props.firmwares);
  },

  propTypes: {
    firmwares: React.PropTypes.array.isRequired,
    firmwareID: React.PropTypes.string.isRequired,
    groups: React.PropTypes.array.isRequired,
    otas: React.PropTypes.array.isRequired,
    allFirmwares : React.PropTypes.array.isRequired

  },

  componentDidMount: function() {
    var that = this;
    $('body').on('keydown', function (e) {
      if($('.modal-body').is(":visible") && e.which === 27){
        that.props.hide();
      }
    });
},

  getFirmware: function(firmwareID, firmwares) {
    if (firmwareID == "0" || firmwareID == "-1") {
      return {
          firmwareid: "",
          name: "",
          release: "",
          released: false,
          commit: "",
          deprecated: false,
          checksum: "",
          builder: "",
          build_date: "",
          image_size: 0,
          groups: [],
          sites: [],
          nodes: [],
          assign: "unassigned",
          assigngroups: [],
          idx: -1
      };
    };
    for (var i=0; i<firmwares.length; i++) {
      if (firmwares[i].firmwareid == firmwareID){
        firmwares[i].assigngroups = firmwares[i].groups.map(function(group, index) {return group.groupid;});
        return (firmwares[i]);
      }
    }
    return null;
  },

  componentWillReceiveProps: function(nextProps){
      this.setState(this.getFirmware(nextProps.firmwareID, nextProps.firmwares));
  },

  render: function() {
    if (this.props.show) {
      return (
        <div className="firmwareForm">
          <Modal.Dialog style={{ zIndex: 100, marginTop: 80 }}>
            <Modal.Body>
            <a className=" " id="firmwareoverlayClose" onClick={() => { this.props.hide() }}>       
              <img src='/imgs/Close1.svg' width='30' height='30'  style={{marginTop:'60px',marginRight:'8px', zIndex: 100} }/>
            </a>
              <Firmwareform firmware={this.state} otas={this.props.otas} groups={this.props.groups} allFirmwares={this.props.allFirmwares} />
            </Modal.Body>
          </Modal.Dialog>
        </div>
      )
    }
    return null;
    /*if (this.props.firmwares.length == 0 && this.props.firmwareID != "0") {
      return (
        <div style={{padding:"20px 0px 0px 0px"}}>
          <div style={{textAlign:"center",fontSize:"54px",opacity:"0.5"}}>
              <Icon glyph="icon-fontello-info-circled" />
          </div>
          <h3 style={{textAlign:"center",lineHeight:"140%"}}>Authorized users can create Firmware versions
          <br />and assign them to Sites or Groups on this page.</h3>
        </div>
        );
    };

    if (this.props.firmwareID == "-1") {
      return (
        <h2 style={{textAlign:"center",padding:"100px 0px"}}>Select a Firmware version.</h2>
        );
    };
    return (
      <Firmwareform firmware={this.state} otas={this.props.otas} groups={this.props.groups} />
    ); */
  }

});

module.exports = Firmwaredetail;

