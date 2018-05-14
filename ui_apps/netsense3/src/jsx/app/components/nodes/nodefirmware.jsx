import classNames from 'classnames';
import auth from 'global/utils/auth';
import DisabledTextField from 'components/disabledtextfield'

import { State, Navigation } from 'react-router';

var Nodefirmware = React.createClass({

  
  propTypes: {
    firmwares:React.PropTypes.array.isRequired,
    otas:React.PropTypes.array.isRequired,
    nodeinfo: React.PropTypes.array.isRequired,
  },

   handleChange: function (key) {
    return function (e) {
      var state = {};
      state[key] = e.target.value;
      this.setState(state);
    }.bind(this);
  },
 

  handleViewJob: function(e){
    e.stopPropagation();
    e.preventDefault();
    this.props.router.push("/app/firmwareupdatepanel");
  },

  handleSetFirmware: function (e) {
    e.stopPropagation();
    e.preventDefault();
    ReactBootstrap.Dispatcher.emit("Nodeform.setfirmware", "open", Object.assign({}, this.state));
  },

  render: function() {
    if(this.props.nodeinfo != null){
      var visibilityModemFirmware = ["unode-v6"].indexOf(this.props.nodeinfo.model)>=0 ? {} : { display: "none" };

      if(this.props.otas != null){

        var count = 0;
        for(var i=0;i<this.props.otas.length;i++){
          if(this.props.otas[i].firmwareid === this.props.nodeinfo.firmwareid){
            count ++;
          }
        }
        var viewBtnStyle = ((this.props.nodeinfo.firmwareid=="")||(count==0))?{display:"none"}:{};
      }

      var that = this;
      if (this.props.firmwares != null){
        var firmwareOptions = this.props.firmwares.filter(function(firmware, index){

          if (["falcon-q","merlin","vdkmaster","cnext"].indexOf(that.props.nodeinfo.model) >=0){
            if(typeof firmware.type == "undefined") {
              return false;
            }
            else{

              return that.props.nodeinfo.model == firmware.type;
            } 
          }
   
          if ((that.props.nodeinfo.model == "unode-v2") || (that.props.nodeinfo.model== "unode-v3") || (that.props.nodeinfo.model =="unode-v4") || (that.props.nodeinfo.model == "unode-v5") || (that.props.nodeinfo.model == "unode-v6")){
            if(typeof firmware.type == "undefined") {
              return firmware;
            }
          }

        }).map(function(firmware, index) {
          if(typeof firmware.type != "undefined"){
            var firmwareList = firmware.name + "_" + firmware.when.substring(0,10);

            return (
              <option key={index} value={firmware.firmwareid}>{firmwareList}</option>
            );
          }
          else{
            return (
              <option key={index} value={firmware.firmwareid}>{firmware.name}</option>
            );
          }
          
        });

        var firmwareDisabled = false;
        if (firmwareOptions.length == 0) {
          firmwareOptions = ["<option>Model has no defined Firmwares</option>"];
          firmwareDisabled = true;
        }; 

      };
    };

   
    return(
      <div>

      { this.props.firmwares === null &&
              (
              <div style={{textAlign:"center",padding:"20px 0px"}}>
                <img src="/imgs/loading.gif" alt="Loading" />
              </div>
              )
      }

      { this.props.firmwares !== null &&
              (
             
            
                <form role="form" className="form-horizontal" >
                  <div className="form-group">
                    <label htmlFor="model" className="control-label col-sm-3">Model:</label>
                    <div className="col-sm-6">
                      <input type="text" className="form-control" id="model" ref="model" disabled="disabled" value={this.props.nodeinfo.model} />
                    </div>
                  </div>

                  <div className="form-group" style={visibilityModemFirmware}>
                    <label htmlFor="modemRevEd" className="control-label col-sm-3">Modem Firmware:</label>
                    <div className="col-sm-6">
                      <input type="text" className="form-control" id="modemRevEd" ref="modemRevEd" disabled="disabled" value={this.props.nodeinfo.modemRevEd} />
                    </div>
                  </div>
                  
                 <DisabledTextField cols={[3, 6]} label="Current Firmware" fieldid="softwareVersion" value={this.props.nodeinfo.softwareVersion} />

                  <div className="form-group">
                    <label htmlFor="firmwareid" className="control-label col-sm-3" style={{lineHeight:"18px",paddingTop:"4px"}}>Assigned Firmware:</label>
                    <div className="col-sm-4">
                      <input type="text" className="form-control" id="firmwareid" ref="firmwareid" disabled="disabled" value={this.props.nodeinfo.firmwareid} />
                    </div>
                    <div style={{display:"inline-block",float:"left"}}>
                      <button type="button" id="viewbutton" className="ns-form-btn" onClick={this.handleViewJob} style={viewBtnStyle}>
                        <Icon glyph="icon-fontello-eye" /> <b>View Job</b>
                      </button>
                    </div>
                  </div>
                  <div className="form-group">
                    <label htmlFor="firmwareLastUpdated" className="control-label col-sm-3" style={{lineHeight:"18px",paddingTop:"4px"}}>Last Updated:</label>
                    <div className="col-sm-6">
                      <input type="text" className="form-control" id="firmwareLastUpdated" ref="firmwareLastUpdated" disabled="disabled" value={this.props.nodeinfo.firmwareLastUpdated} />
                    </div>
                  </div>
                  <div className="form-group">
                    <label htmlFor="changefirmware" className="control-label col-sm-3" style={{lineHeight:"18px",paddingTop:"4px"}}>Apply Firmware:</label>
                    <div className="col-sm-9">
                      <select className="form-control" style={{float:"left"}} id="newfirmwareid" ref="newfirmwareid" value={this.props.nodeinfo.newfirmwareid} onChange={this.handleChange('newfirmwareid')} >
                        {firmwareOptions}
                      </select><br/><br/>
                      <div className="text-right col-sm-12">
                        <button className="ns-form-btn" id="setNodeFirmware" disabled= {firmwareDisabled} onClick={this.handleSetFirmware}> <b> Set Firmware </b> </button>
                      </div>
                    </div>
                  </div>  
                </form>
             
          )
        }
      </div>
    );
  }
});

  module.exports = Nodefirmware;
