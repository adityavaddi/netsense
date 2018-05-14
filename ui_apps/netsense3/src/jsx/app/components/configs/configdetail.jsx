import classNames from 'classnames';
import { State, Navigation } from 'react-router';
import Configform from 'components/configs/configform';
import helpers from 'global/utils/helpers';
import { Modal } from 'react-bootstrap';

var Configdetail = React.createClass({

  getInitialState: function () {
    return null;
  },

  propTypes: {
    configs: React.PropTypes.array.isRequired,
    groups: React.PropTypes.array.isRequired,
    configID: React.PropTypes.string.isRequired,
    show: React.PropTypes.bool.isRequired
  },

  componentDidMount: function() {
    var that = this;
    $('body').on('keydown', function (e) {
      if($('.modal-body').is(":visible") && e.which === 27){
        that.props.hide();
      }
    });
},

  getConfig: function (configID) {

    var that = this;

    if ((configID != undefined) && (configID != "0") && (configID != "-1")) {

      $.ajax({
        url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/configs/' + configID,
        data: '',
        method: 'GET',
        "xhrFields": {
          withCredentials: true
        },
        dataType: 'json',
        success: function (config) {
          $("#loadingmsg")
            .html("Generating display.");
          console.log("Retrieving config info: " + JSON.stringify(config));
          that.props.configID = config.configid;
          var nodesConfig = config.nodes;

          var $listSelector = $(".nodesAssigned");
          $listSelector.empty();
          if ((typeof config.nodes != "undefined") && (config.nodes.length != 0)) {
            $("#assignmentWrapper").css("display", "none");
            $.each(nodesConfig, function (i, obj) {
              $listSelector.append((i==0?"":", ") + obj.nodeid);
            });
          }
          else {
            $("#assignmentWrapper").css("display", "block");
            $listSelector.append("<span>No nodes have been assigned to this configuration.</span>");
          }

          // Modal:
          var nodesAssignedModal = $(".nodesAssignedModal");
          nodesAssignedModal.empty();
          if ((typeof config.nodes != "undefined") && (config.nodes.length != 0)) {
            $.each(nodesConfig, function (i, obj) {
              var checkbox = $(document.createElement("input")).css({ "width": "30px", "height": "30px" }).attr({
                id: obj.nodeid
                , name: obj.name
                , value: obj.name
                , text: obj.name
                , type: 'checkbox'
                , checked: true
              })

              var checkboxlabel = "<span class=checkboxlabel>" + obj.nodeid + "</span><br>";

              nodesAssignedModal.append(checkbox);
              nodesAssignedModal.append(checkboxlabel);

            });
          }
          else {
            nodesAssignedModal.append("<span>No nodes have been assigned to the selected config</span>");
          }

          config.configid = config.configid || "";
          config.name = config.name || "";
          config.model = config.model || "";
          config.modelName = helpers.modelName(config.model);

          if ((config.model != "falcon-q") && (config.model != "merlin") && (config.model != "vdkmaster")){

            config.networkXPasskey = config.networkXPasskey;
            config.networkXSSID = config.networkXSSID || "";
            config.networkXSecurity = config.networkXSecurity || "";
            config.networkYPasskey = config.networkYPasskey;
            config.networkYSSID = config.networkYSSID || "";
            config.networkYSecurity = config.networkYSecurity || "";
            config.network_region = config.network_region || "US";
            config.debugmode = config.debugmode;
            config.telnet = config.telnet;
            config.vpn_on_demand = config.vpn_on_demand;
            config.aux_power = config.aux_power;
            config.podbus_disable = config.podbus_disable;
            config.sensor_rf_pint = config.sensor_rf_pint / 1000 || 0;
            config.sensor_rf_dint = config.sensor_rf_dint / 1000 || 0;
            config.sensor_rf_mode = config.sensor_rf_mode || 0;
            config.sensor_v_pint = config.sensor_v_pint / 1000 || 0;
            config.sensor_v_dint = config.sensor_v_dint / 1000 || 0;
            config.sensor_v_mode = config.sensor_v_mode || 0;
            config.sensor_aw_pint = config.sensor_aw_pint / 1000 || 0;
            config.sensor_aw_dint = config.sensor_aw_dint / 1000 || 0;
            config.sensor_aw_mode = config.sensor_aw_mode || 0;
            config.sensor_aPF_pint = config.sensor_aPF_pint / 1000 || 0;
            config.sensor_aPF_dint = config.sensor_aPF_dint / 1000 || 0;
            config.sensor_aPF_mode = config.sensor_aPF_mode || 0;
            config.sensor_aP_pint = config.sensor_aP_pint / 1000 || 0;
            config.sensor_aP_dint = config.sensor_aP_dint / 1000 || 0;
            config.sensor_aP_mode = config.sensor_aP_mode || 0;
            config.sensor_mP_pint = config.sensor_mP_pint / 1000 || 0;
            config.sensor_mP_dint = config.sensor_mP_dint / 1000 || 0;
            config.sensor_mP_mode = config.sensor_mP_mode || 0;
            config.sensor_mip_pint = config.sensor_mip_pint / 1000 || 0;
            config.sensor_mip_dint = config.sensor_mip_dint / 1000 || 0;
            config.sensor_mip_mode = config.sensor_mip_mode || 0;
            config.sensor_ai_pint = config.sensor_ai_pint / 1000 || 0;
            config.sensor_ai_dint = config.sensor_ai_dint / 1000 || 0;
            config.sensor_ai_mode = config.sensor_ai_mode || 0;
            config.sensor_aip_pint = config.sensor_aip_pint / 1000 || 0;
            config.sensor_aip_dint = config.sensor_aip_dint / 1000 || 0;
            config.sensor_aip_mode = config.sensor_aip_mode || 0;
            config.sensor_mi_pint = config.sensor_mi_pint / 1000 || 0;
            config.sensor_mi_dint = config.sensor_mi_dint / 1000 || 0;
            config.sensor_mi_mode = config.sensor_mi_mode || 0;
            config.sensor_mw_pint = config.sensor_mw_pint / 1000 || 0;
            config.sensor_mw_dint = config.sensor_mw_dint / 1000 || 0;
            config.sensor_mw_mode = config.sensor_mw_mode || 0;
            config.sensor_mPF_pint = config.sensor_mPF_pint / 1000 || 0;
            config.sensor_mPF_dint = config.sensor_mPF_dint / 1000 || 0;
            config.sensor_mPF_mode = config.sensor_mPF_mode || 0;
            config.sensor_lIR_pint = config.sensor_lIR_pint / 1000 || 0;
            config.sensor_lIR_dint = config.sensor_lIR_dint / 1000 || 0;
            config.sensor_lIR_mode = config.sensor_lIR_mode || 0;
            config.sensor_l_pint = config.sensor_l_pint / 1000 || 0;
            config.sensor_l_dint = config.sensor_l_dint / 1000 || 0;
            config.sensor_l_mode = config.sensor_l_mode || 0;
            config.sensor_l_i_pint = config.sensor_l_i_pint / 1000 || 0;
            config.sensor_l_i_dint = config.sensor_l_i_dint / 1000 || 0;
            config.sensor_l_i_mode = config.sensor_l_i_mode || 0;
            config.sensor_lIR_i_pint = config.sensor_lIR_i_pint / 1000 || 0;
            config.sensor_lIR_i_dint = config.sensor_lIR_i_dint / 1000 || 0;
            config.sensor_lIR_i_mode = config.sensor_lIR_i_mode || 0;
            config.sensor_p_pint = config.sensor_p_pint / 1000 || 0;
            config.sensor_p_dint = config.sensor_p_dint / 1000 || 0;
            config.sensor_p_mode = config.sensor_p_mode || 0;
            config.sensor_pc_pint = config.sensor_pc_pint / 1000 || 0;
            config.sensor_pc_dint = config.sensor_pc_dint / 1000 || 0;
            config.sensor_pc_mode = config.sensor_pc_mode || 0;
            config.sensor_t_pint = config.sensor_t_pint / 1000 || 0;
            config.sensor_t_dint = config.sensor_t_dint / 1000 || 0;
            config.sensor_t_mode = config.sensor_t_mode || 0;
            config.sensor_T_pint = config.sensor_T_pint / 1000 || 0;
            config.sensor_T_dint = config.sensor_T_dint / 1000 || 0;
            config.sensor_T_mode = config.sensor_T_mode || 0;
            config.sensor_mt_pint = config.sensor_mt_pint / 1000 || 0;
            config.sensor_mt_dint = config.sensor_mt_dint / 1000 || 0;
            config.sensor_mt_mode = config.sensor_mt_mode || 0;
            config.sensor_pdc_pint = config.sensor_pdc_pint / 1000 || 0;
            config.sensor_pdc_dint = config.sensor_pdc_dint / 1000 || 0;
            config.sensor_pdc_mode = config.sensor_pdc_mode || 0;
            config.sensor_ppr_pint = config.sensor_ppr_pint / 1000 || 0;
            config.sensor_ppr_dint = config.sensor_ppr_dint / 1000 || 0;
            config.sensor_ppr_mode = config.sensor_ppr_mode || 0;
            config.sensor_pnd_pint = config.sensor_pnd_pint / 1000 || 0;
            config.sensor_pnd_dint = config.sensor_pnd_dint / 1000 || 0;
            config.sensor_pnd_mode = config.sensor_pnd_mode || 0;
            config.sensor_pdt_pint = config.sensor_pdt_pint / 1000 || 0;
            config.sensor_pdt_dint = config.sensor_pdt_dint / 1000 || 0;
            config.sensor_pdt_mode = config.sensor_pdt_mode || 0;
            config.sensor_podm_pint = config.sensor_podm_pint / 1000 || 0;
            config.sensor_podm_dint = config.sensor_podm_dint / 1000 || 0;
            config.sensor_podm_mode = config.sensor_podm_mode || 0;
            config.sensor_podme_pint = config.sensor_podme_pint / 1000 || 0;
            config.sensor_podme_dint = config.sensor_podme_dint / 1000 || 0;
            config.sensor_podme_mode = config.sensor_podme_mode || 0;
            config.sensor_rlym_pint = config.sensor_rlym_pint / 1000 || 0;
            config.sensor_rlym_dint = config.sensor_rlym_dint / 1000 || 0;
            config.sensor_rlym_mode = config.sensor_rlym_mode || 0;
            config.lctrl_trigger_ont = config.lctrl_trigger_ont / 1000 || 0;
            config.lctrl_trigger_offt = config.lctrl_trigger_offt / 1000 || 0;
            config.lctrl_trigger_nonett = config.lctrl_trigger_nonett / 1000 || 0;
            config.lctrl_trigger_off_dint = config.lctrl_trigger_off_dint / 1000 || 0;
            config.lctrl_trigger_on_dint = config.lctrl_trigger_on_dint / 1000 || 0;
          }

          config.assign = "unassigned";
          config.nodes = config.nodes || [];
          config.sites = config.sites || [];
          config.groups = config.groups || [];
          config.assigngroups = config.groups.map(function (group, index) { return group.groupid; });

          this.setState(config);
        }.bind(that),

        error: function (jqXHR, status, error) {
          console.log("ajax failure (configs): " + status + " - " + error);
          $("#loadingmsg").html("Cannot retrieve configs Details.  API reported error: " + error);
        }
      });
    }
  },

  handleChange: function (key) {
    return function (e) {
      var state = {};
      state[key] = e.target.value;
      this.setState(state);
    }.bind(this);
  },

  handleModelSubmit: function (e) {
    e.stopPropagation();
    e.preventDefault();
    var selected_model = $("select#model option").filter(":selected").val();
    ReactBootstrap.Dispatcher.emit("Configform.modelSubmit", selected_model);
  },

  componentWillReceiveProps: function (nextProps) {
    if (this.props.configID != nextProps.configID) {
      this.setState(this.getConfig(nextProps.configID));
    };
  },

  getContent: function () {

    var heading;
    for(var i=0;i<this.props.configs.length;i++){
      if(this.props.configs[i].configid == this.props.configID){
        heading = this.props.configs[i].name;
      }
    }
    if ((this.props.configID == "0" || this.props.configID == "-1") && (this.props.defaultmodel != null)) {
      return (
        <div>
          <div className="netsense__form__header">
            <h2 style={{marginTop:"0",padding:"7px 0px 1px 19px"}}>Add Configuration</h2>
          </div>
          <Configform config={this.props.defaultmodel} groups={this.props.groups} allConfigs={this.props.configs} />
        </div>

      );
    }

    if (this.props.configs.length == 0 && this.props.configID != "0") {
      return (
        <div style={{ padding: "20px 0px 0px 0px" }}>
          <div style={{ textAlign: "center", fontSize: "54px", opacity: "0.5" }}>
            <Icon glyph="icon-fontello-info-circled" />
          </div>
          <h3 style={{ textAlign: "center", lineHeight: "140%" }}>Authorized users can create a Config
          <br />and assign them to a site, group or configs on this page.</h3>
        </div>
      );
    };

    if ((this.props.configID == "0")) {
      return (
      <div className="configForm">
        <Modal.Dialog style={{ zIndex: 100, marginTop: 80 }}>
          <Modal.Body>
          <a className=" " id="configoverlayClose" onClick={() => { this.props.hide() }}>
            <img src='/imgs/Close1.svg' width='30' height='30'  style={{marginTop:'60px',marginRight:'8px', zIndex: 100} }/>
          </a>
            <div className="netsense__form__header">
              <h2 style={{ textAlign: "center", padding: "10px 0px 50px 0px", margin: "0" }}>Please select a config model</h2>
            </div>

            <div className="netsense__form__body" style={{ padding: "30px" }}>
              <form role="form" className="form-horizontal">
                <div className="form-group">
                  <label htmlFor="model" className="control-label col-sm-3"> Config Model:</label>
                  <div className="col-sm-6">
                    <select className="form-control" id="model" ref="model" value={this.state.model} onChange={this.handleChange('model')}>
                      <option value="unode-v2"> Core Node</option>
                      <option value="unode-v3"> Internal Core Node</option>
                      <option value="unode-v4"> Core Node EX Wifi</option>
                      <option value="unode-v5"> Core Node EX Cellular</option>
                      <option value="unode-v6"> Core Node EX LTE</option>
                      <option value="falcon-q"> Video Node</option>
                      <option value="merlin"> Video Node 4k</option>
                      <option value="vdkmaster"> Verizon Digital Kiosk</option>
                      <option value="cnext"> Smart City Hub</option>
                    </select>
                  </div>
                </div>
              </form>
              <button id="applyConfig" type="button" className="ns-save-btn" onClick={this.handleModelSubmit}>
                <b>Apply </b></button>

            </div>
          </Modal.Body>

        </Modal.Dialog>
      </div>

     );
    };


    if (this.props.configID == "-1") {
      return (
        <div></div>
      );
    }
    return (
      <div>
        <div>
          <div className="netsense__form__header">
            <h2 style={{marginTop:"0",padding:"7px 0px 1px 19px"}}>{heading}</h2>
          </div>
          <div style={{maxHeight:"80px",overflow:"auto",margin:"5px 30px", border:" 1px solid black", padding:" 11px"}}>
            <b>Nodes Assigned: &nbsp;</b>
            <span className="nodesAssigned"></span>
          </div>
          <Configform config={this.state} groups={this.props.groups}  allConfigs={this.props.configs}/>
        </div>
      </div>
    );
  },

  render: function () {
    if (this.props.show) {
      return (
        <div className="configForm">
          <Modal.Dialog style={{ zIndex: 100, marginTop: 80 }}>
            <Modal.Body>
             <a className="" id="configoverlayClose" onClick={() => { this.props.hide() }}>
              <img src='/imgs/Close1.svg' width='30' height='30'  style={{marginTop:'60px',marginRight:'8px', zIndex: 101} }/>
            </a>
              {this.getContent()}
            </Modal.Body>
          </Modal.Dialog>
        </div>
      )
    }
    return null
  }
});

module.exports = Configdetail;
