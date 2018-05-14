import classNames from 'classnames';
import helpers from 'global/utils/helpers';
import auth from 'global/utils/auth';
import { State, Navigation } from 'react-router';
var Configform = React.createClass({

  getInitialState: function(){
    return this.props.config
  },

  propTypes: {
    config: React.PropTypes.object.isRequired,
    defaultmodel: React.PropTypes.object.isRequired,
    groups: React.PropTypes.array.isRequired,
    errors: React.PropTypes.object,
    allConfigs: React.PropTypes.array.isRequired
  },

  getDefaultProps: function() {
    return {
      errors: {}
    };
  },


handleChange: function (key) {
    return function (e) {
      var state = {};
      switch (key) {
        case 'debugmode':
        case 'telnet':
        case 'vpn_on_demand':
        case 'medianode.ap_supported':
        case 'aux_power':
        case 'ota_disable':
        case 'podbus.disable':
        case 'evtgrab.periodic_only':
        case 'poe.enabled':
        case 'camera.0.autoexposure':
        case 'camera.0.binning':
        case 'camera.0.autogain':
        case 'camera.0.wdrmode':
        case 'camera.0.indoormode':        
        case 'camera.1.autoexposure':
        case 'camera.1.autogain':
        case 'camera.1.indoormode':        
        case 'camera.1.wdrmode':
        case 'camera.0.enabled':
        case 'camera.1.enabled':
        case 'mediaserver.nomve':
        case 'mediaserver.wmm_qos':
        case 'medianode.commissioned':
        case 'network.vpn_on_demand':
        case 'network.nowifi':
        case 'network.nocell':
          state[key] = e.target.checked;
          break;
        case 'podbus_disable':
          state[key] = (e.target.checked ? 1:0);
          break;
        case 'assigngroups':
          state.assigngroups = $.makeArray($(e.target).val());
          break;
        default:
          state[key] = e.target.value;
      };
      this.setState(state);
    }.bind(this);
  },

  handleRadio: function (key) {
    return function (e) {
      var state = {};
      switch (e.target.value) {
        case "unassigned":
          state.sites = [];
          state.assign = "unassigned";
        break;
        case "sitewide":
          state.sites = [{
            siteid: NSN.siteID,
            name: NSN.siteName,
          }];
          state.assign="sitewide";
        break;
        case "groups":
          state.sites = [];
          state.assign = "groups";
          state.assigngroups = $.makeArray($(React.findDOMNode(this.refs.assigngroups)).val());
        break;
      };
      this.setState(state);
    }.bind(this);
  },

  isValid: function() {
    this.props.errors = {};
    var rules = {
        sensor_rf_pint: {
            digits: true,
            min: 30,
            max: 36000
        },
        sensor_rf_dint: {
            digits: true,
            min: 30,
            max: 36000
        },
        sensor_rf_mode: {
            digits: true,
            min: 1,
            max: 2
        },
        sensor_v_pint: {
            digits: true,
            min: 30,
            max: 36000
        },
        sensor_v_dint: {
            digits: true,
            min: 30,
            max: 36000
        },
        sensor_v_mode: {
            digits: true,
            min: 1,
            max: 2
        },
        sensor_aw_pint: {
            digits: true,
            min: 30,
            max: 36000
        },
        sensor_aw_dint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_aw_mode: {
            digits: true,
            min: 1,
            max: 2
        },
        sensor_aPF_pint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_aPF_dint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_aPF_mode: {
            digits: true,
            min: 1,
            max: 2
        },
        sensor_mi_pint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_mi_dint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_mi_mode: {
            digits: true,
            min: 1,
            max: 2
        },
        sensor_mw_pint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_mw_dint: {
            digits:true,
            min: 30,
            max: 36000
        },
         sensor_mw_mode: {
            digits: true,
            min: 1,
            max: 2
        },
        sensor_mPF_pint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_mPF_dint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_mPF_mode: {
            digits: true,
            min: 1,
            max: 2
        },
        sensor_lIR_pint: {
            digits:true,
            min: 30,
            max: 172800
        },
        sensor_lIR_dint: {
            digits:true,
            min: 30,
            max: 172800
        },
        sensor_lIR_mode: {
            digits: true,
            min: 1,
            max: 2
        },
        sensor_l_pint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_l_dint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_l_mode: {
            digits: true,
            min: 1,
            max: 2
        },
        sensor_l_i_pint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_l_i_dint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_l_i_mode: {
            digits: true,
            min: 1,
            max: 2
        },
        sensor_lIR_i_pint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_lIR_i_dint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_lIR_i_mode: {
            digits: true,
            min: 1,
            max: 2
        },
        sensor_p_pint: {
            digits:true,
            min: 1,
            max: 3600
        },
        sensor_p_dint: {
            digits:true,
            min: 1,
            max: 3600
        },
        sensor_p_mode: {
            digits: true,
            min: 1,
            max: 2
        },
        sensor_pc_pint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_pc_dint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_pc_mode: {
            digits: true,
            min: 1,
            max: 2
        },
        sensor_t_pint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_t_dint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_t_mode: {
            digits: true,
            min: 1,
            max: 2
        },
        sensor_T_pint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_T_dint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_T_mode: {
            digits: true,
            min: 1,
            max: 2
        },
        sensor_mt_pint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_mt_dint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_mt_mode: {
            digits: true,
            min: 1,
            max: 2
        },
        sensor_podm_pint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_podm_dint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_podm_mode: {
            digits: true,
            min: 1,
            max: 2
        },
        sensor_podme_pint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_podme_dint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_podme_mode: {
            digits: true,
            min: 1,
            max: 2
        },
        sensor_rlym_pint: {
            digits:true,
            min: 30,
            max: 86400
        },
        sensor_aP_pint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_aP_dint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_aP_mode: {
            digits: true,
            min: 1,
            max: 2
        },
        sensor_mP_pint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_mP_dint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_mP_mode: {
            digits: true,
            min: 1,
            max: 2
        },
        sensor_mip_pint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_mip_dint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_mip_mode: {
            digits: true,
            min: 1,
            max: 2
        },
        sensor_ai_pint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_ai_dint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_ai_mode: {
            digits: true,
            min: 1,
            max: 2
        },
        sensor_aip_pint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_aip_dint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_aip_mode: {
            digits: true,
            min: 1,
            max: 2
        },
        lctrl_trigger_ont: {
            digits:true,
            min: 0,
            max: 2147483646
        },
        lctrl_trigger_offt: {
            digits:true,
            min: 1,
            max: 2147483647
        },
        lctrl_trigger_nonett: {
            digits:true,
            min: 0,
            max: 2147483647
        },
         lctrl_trigger_off_dint: {
            digits:true,
            min: 0,
            max: 2147483647
        },
         lctrl_trigger_on_dint: {
            digits:true,
            min: 0,
            max: 2147483647
        }
    };

    var rules_unodev2 = {
        networkXPasskey:{
            required:true
        },
        networkYPasskey:{
            required:true
        },
        sensor_rf_pint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_rf_dint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_rf_mode: {
            digits: true,
            min: 1,
            max: 2
        },
        sensor_v_pint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_v_dint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_v_mode: {
            digits: true,
            min: 1,
            max: 2
        },
        sensor_lIR_pint: {
            digits:true,
            min: 30,
            max: 172800
        },
        sensor_lIR_dint: {
            digits:true,
            min: 30,
            max: 172800
        },
        sensor_lIR_mode: {
            digits: true,
            min: 1,
            max: 2
        },
        sensor_l_pint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_l_dint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_l_mode: {
            digits: true,
            min: 1,
            max: 2
        },
        sensor_p_pint: {
            digits:true,
            min: 1,
            max: 3600
        },
        sensor_p_dint: {
            digits:true,
            min: 1,
            max: 3600
        },
        sensor_p_mode: {
            digits: true,
            min: 1,
            max: 2
        },
        sensor_t_pint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_t_dint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_t_mode: {
            digits: true,
            min: 1,
            max: 2
        },
        sensor_T_pint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_T_dint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_T_mode: {
            digits: true,
            min: 1,
            max: 2
        },
        sensor_pdc_pint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_pdc_dint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_pdc_mode: {
            digits: true,
            min: 1,
            max: 2
        },
        sensor_ppr_pint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_ppr_dint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_ppr_mode: {
            digits: true,
            min: 1,
            max: 2
        },
        sensor_pnd_pint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_pnd_dint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_pnd_mode: {
            digits: true,
            min: 1,
            max: 2
        },
        sensor_pdt_pint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_pdt_dint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_pdt_mode: {
            digits: true,
            min: 1,
            max: 2
        },
    };

    var rules_unodev3 = {
        networkXPasskey:{
            required:true
        },
        networkYPasskey:{
            required:true
        },
        sensor_rf_pint: {
            digits: true,
            min: 30,
            max: 36000
        },
        sensor_rf_dint: {
            digits: true,
            min: 30,
            max: 36000
        },
        sensor_rf_mode: {
            digits: true,
            min: 1,
            max: 2
        },
        sensor_v_pint: {
            digits: true,
            min: 30,
            max: 36000
        },
        sensor_v_dint: {
            digits: true,
            min: 30,
            max: 36000
        },
        sensor_v_mode: {
            digits: true,
            min: 1,
            max: 2
        },
        sensor_mi_pint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_mi_dint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_mi_mode: {
            digits: true,
            min: 1,
            max: 2
        },
        sensor_lIR_pint: {
            digits:true,
            min: 30,
            max: 172800
        },
        sensor_lIR_dint: {
            digits:true,
            min: 30,
            max: 172800
        },
        sensor_lIR_mode: {
            digits: true,
            min: 1,
            max: 2
        },
        sensor_l_pint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_l_dint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_l_mode: {
            digits: true,
            min: 1,
            max: 2
        },
        sensor_p_pint: {
            digits:true,
            min: 1,
            max: 3600
        },
        sensor_p_dint: {
            digits:true,
            min: 1,
            max: 3600
        },
        sensor_p_mode: {
            digits: true,
            min: 1,
            max: 2
        },
        sensor_a_jdelta: {
            digits: true,
            min: 32,
            max: 65000
        },
        sensor_t_pint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_t_dint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_t_mode: {
            digits: true,
            min: 1,
            max: 2
        },
        sensor_T_pint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_T_dint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_T_mode: {
            digits: true,
            min: 1,
            max: 2
        },
        sensor_pdc_pint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_pdc_dint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_pdc_mode: {
            digits: true,
            min: 1,
            max: 2
        },
        sensor_ppr_pint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_ppr_dint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_ppr_mode: {
            digits: true,
            min: 1,
            max: 2
        },
        sensor_pnd_pint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_pnd_dint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_pnd_mode: {
            digits: true,
            min: 1,
            max: 2
        },
        sensor_pdt_pint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_pdt_dint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_pdt_mode: {
            digits: true,
            min: 1,
            max: 2
        },
    };

    var rules_unodev4 = {
        networkXPasskey:{
            required:true
        },
        networkYPasskey:{
            required:true
        },
        sensor_rf_pint: {
            digits: true,
            min: 30,
            max: 36000
        },
        sensor_rf_dint: {
            digits: true,
            min: 30,
            max: 36000
        },
        sensor_rf_mode: {
            digits: true,
            min: 1,
            max: 2
        },
        sensor_v_pint: {
            digits: true,
            min: 30,
            max: 36000
        },
        sensor_v_dint: {
            digits: true,
            min: 30,
            max: 36000
        },
        sensor_v_mode: {
            digits: true,
            min: 1,
            max: 2
        },
        sensor_aw_pint: {
            digits: true,
            min: 30,
            max: 36000
        },
        sensor_aw_dint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_aw_mode: {
            digits: true,
            min: 1,
            max: 2
        },
        sensor_aPF_pint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_aPF_dint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_aPF_mode: {
            digits: true,
            min: 1,
            max: 2
        },
        sensor_mi_pint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_mi_dint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_mi_mode: {
            digits: true,
            min: 1,
            max: 2
        },
        sensor_mw_pint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_mw_dint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_mw_mode: {
            digits: true,
            min: 1,
            max: 2
        },
        sensor_mPF_pint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_mPF_dint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_mPF_mode: {
            digits: true,
            min: 1,
            max: 2
        },
        sensor_lIR_pint: {
            digits:true,
            min: 30,
            max: 172800
        },
        sensor_lIR_dint: {
            digits:true,
            min: 30,
            max: 172800
        },
        sensor_lIR_mode: {
            digits: true,
            min: 1,
            max: 2
        },
        sensor_l_pint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_l_dint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_l_mode: {
            digits: true,
            min: 1,
            max: 2
        },
        sensor_p_pint: {
            digits:true,
            min: 1,
            max: 3600
        },
        sensor_p_dint: {
            digits:true,
            min: 1,
            max: 3600
        },
        sensor_p_mode: {
            digits: true,
            min: 1,
            max: 2
        },
        sensor_a_jdelta: {
            digits: true,
            min: 32,
            max: 65000
        },
        sensor_pc_pint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_pc_dint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_pc_mode: {
            digits: true,
            min: 1,
            max: 2
        },
        sensor_t_pint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_t_dint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_t_mode: {
            digits: true,
            min: 1,
            max: 2
        },
        sensor_T_pint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_T_dint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_T_mode: {
            digits: true,
            min: 1,
            max: 2
        },
        sensor_mt_pint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_mt_dint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_mt_mode: {
            digits: true,
            min: 1,
            max: 2
        },
        sensor_podm_pint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_podm_dint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_podm_mode: {
            digits: true,
            min: 1,
            max: 2
        },
        sensor_podme_pint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_podme_dint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_podme_mode: {
            digits: true,
            min: 1,
            max: 2
        },
        sensor_rlym_pint: {
            digits:true,
            min: 30,
            max: 86400
        },
        sensor_aP_pint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_aP_dint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_aP_mode: {
            digits: true,
            min: 1,
            max: 2
        },
        sensor_mP_pint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_mP_dint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_mP_mode: {
            digits: true,
            min: 1,
            max: 2
        },
        sensor_mip_pint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_mip_dint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_mip_mode: {
            digits: true,
            min: 1,
            max: 2
        },
        sensor_ai_pint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_ai_dint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_ai_mode: {
            digits: true,
            min: 1,
            max: 2
        },
        sensor_aip_pint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_aip_dint: {
            digits:true,
            min: 30,
            max: 36000
        },
        sensor_aip_mode: {
            digits: true,
            min: 1,
            max: 2
        },
        lctrl_trigger_ont: {
            digits:true,
            min: 0,
            max: 2147483646
        },
        lctrl_trigger_offt: {
            digits:true,
            min: 1,
            max: 2147483647
        },
        lctrl_trigger_nonett: {
            digits:true,
            min: 0,
            max: 2147483647
        },
         lctrl_trigger_off_dint: {
            digits:true,
            min: 0,
            max: 2147483647
        },
        lctrl_trigger_on_dint: {
            digits:true,
            min: 0,
            max: 2147483647
        }

    };

    var rules_falconq = {
        "camera.0.colortemp":{
            digits:true
        },
        "camera.0.exposure":{
            digits:true
        },
        "camera.0.gain":{
            digits:true
        },
        "camera.0.master":{
            digits:true
        },
        "camera.1.colortemp":{
            digits:true
        },
        "camera.1.exposure":{
            digits:true
        },
        "camera.1.gain":{
            digits:true
        },
        "camera.0.streamH.bitrate":{
            digits:true
        },
        "camera.0.streamH.framerate":{
            digits:true
        },
        "camera.0.streamH.gopsize":{
            digits:true
        },
        "camera.0.streamH.storage.diskquota":{
            digits:true
        },
        "camera.0.streamH.storage.recordmode":{
            digits:true
        },
        "camera.0.streamH.storage.recordpostamble":{
            digits:true
        },
        "camera.0.streamH.storage.recordpreamble":{
            digits:true
        },
        "camera.0.streamH.storage.vaqa.0.qty":{
            digits:true
        },
        "camera.0.streamH.storage.vaqa.1.qty":{
            digits:true
        },
        "camera.0.streamL.bitrate":{
            digits:true
        },
        "camera.0.streamL.framerate":{
            digits:true
        },
        "camera.0.streamL.gopsize":{
            digits:true
        },
        "camera.0.streamL.storage.diskquota":{
            digits:true
        },
        "camera.0.streamL.storage.recordmode":{
            digits:true
        },
        "camera.0.streamL.storage.recordpostamble":{
            digits:true
        },
        "camera.0.streamL.storage.recordpreamble":{
            digits:true
        },
        "camera.1.streamH.bitrate":{
            digits:true
        },
        "camera.1.streamH.framerate":{
            digits:true
        },
        "camera.1.streamH.gopsize":{
            digits:true
        },
        "camera.1.streamH.storage.diskquota":{
            digits:true
        },
        "camera.1.streamH.storage.recordmode":{
            digits:true
        },
        "camera.1.streamH.storage.recordpostamble":{
            digits:true
        },
        "camera.1.streamH.storage.recordpreamble":{
            digits:true
        },
        "camera.1.streamH.storage.vaqa.0.qty":{
            digits:true
        },
        "camera.1.streamH.storage.vaqa.1.qty":{
            digits:true
        },
        "camera.1.streamL.bitrate":{
            digits:true
        },
        "camera.1.streamL.framerate":{
            digits:true
        },
        "camera.1.streamL.gopsize":{
            digits:true
        },
        "camera.1.streamL.storage.diskquota":{
            digits:true,
            falconVal :"validate",
            fCamera0streamH: "camera.0.streamH.storage.diskquota",
            fCamera0streamL: "camera.0.streamL.storage.diskquota",
            fCamera1streamH: "camera.1.streamH.storage.diskquota",
            total: 1
        },
        "camera.1.streamL.storage.recordmode":{
            digits:true
        },
        "camera.1.streamL.storage.recordpostamble":{
            digits:true
        },
        "camera.1.streamL.storage.recordpreamble":{
            digits:true
        },
        "evtgrab.capint":{
            digits:true
        },
        "evtgrab.dint":{
            digits:true
        },
        "evtgrab.nfiles":{
            digits:true
        },
        "rtsp.service":{
            digits:true
        },
        "storage.maxsessions":{
            digits:true
        },
        "storageserver.cache_size":{
            digits:true
        },
        "storageserver.max_clip_size":{
            digits:true
        },
        "alarm.redis.memory_threshold":{
            digits:true
        },
        "application_server_port":{
            digits:true
        },
        "network.wlan-x.security.bgscan-long-interval":{
            digits:true
        },
        "network.wlan-x.security.bgscan-short-interval":{
            digits:true
        },
        "network.wlan-x.security.bgscan-signal-threshold":{
            digits:true
        },
        "network.wlan-y.security.bgscan-long-interval":{
            digits:true
        },
        "network.wlan-y.security.bgscan-short-interval":{
            digits:true
        },
        "network.wlan-y.security.bgscan-signal-threshold":{
            digits:true
        },
        "network.eth-x.netmask":{
            digits:true
        },
    }; 

    var rules_vdkmaster = {
        "alarm.redis.memory_threshold":{
            digits:true
        },
        "application_server_port":{
            digits:true
        },
        "al.left.setbrightness":{
            digits:true,
            min:0,
            max:100
        },
        "al.right.setbrightness":{
            digits:true,
            min:0,
            max:100
        },
        "al.left.setmode":{
            regex:"^[AMS]$"
        },
        "al.right.setmode":{
            regex:"^[AMS]$"
        },
        "glassl.display.blue.gain":{
            digits:true
        },
        "glassl.display.blue.offset":{
            digits:true
        },
        "glassl.display.brightness":{
            digits:true
        },
        "glassl.display.color-temp":{
            digits:true
        },
        "glassl.display.contrast":{
            digits:true
        },
        "glassl.display.green.gain":{
            digits:true
        },
        "glassl.display.green.offset":{
            digits:true
        },
        "glassl.display.hue":{
            digits:true
        },
        "glassl.display.mpeg-nr":{
            digits:true
        },
        "glassl.display.red.gain":{
            digits:true
        },
        "glassl.display.red.offset":{
            digits:true
        },
        "glassl.display.saturation":{
            digits:true
        },
        "glassl.display.sharpness":{
            digits:true
        },
        "glassl.display.temp.cold":{
            digits:true
        },
        "glassl.display.temp.hot":{
            digits:true
        },
        "glassl.display.temp.warm":{
            digits:true
        },
        "glassr.display.blue.gain":{
            digits:true
        },
        "glassr.display.blue.offset":{
            digits:true
        },
        "glassr.display.brightness":{
            digits:true
        },
        "glassr.display.color-temp":{
            digits:true
        },
        "glassr.display.contrast":{
            digits:true
        },
        "glassr.display.green.gain":{
            digits:true
        },
        "glassr.display.green.offset":{
            digits:true
        },
        "glassr.display.hue":{
            digits:true
        },
        "glassr.display.mpeg-nr":{
            digits:true
        },
        "glassr.display.red.gain":{
            digits:true
        },
        "glassr.display.red.offset":{
            digits:true
        },
        "glassr.display.saturation":{
            digits:true
        },
        "glassr.display.sharpness":{
            digits:true
        },
        "glassr.display.temp.cold":{
            digits:true
        },
        "glassr.display.temp.hot":{
            digits:true
        },
        "glassr.display.temp.warm":{
            digits:true
        },
        "network.eth-x.netmask":{
            digits:true
        },
        "pebble.display.blue.gain":{
            digits:true
        },
        "pebble.display.blue.offset":{
            digits:true
        },
        "pebble.display.brightness":{
            digits:true
        },
        "pebble.display.color-temp":{
            digits:true
        },
        "pebble.display.contrast":{
            digits:true
        },
        "pebble.display.green.gain":{
            digits:true
        },
        "pebble.display.green.offset":{
            digits:true
        },
        "pebble.display.hue":{
            digits:true
        },
        "pebble.display.mpeg-nr":{
            digits:true
        },
        "pebble.display.red.gain":{
            digits:true
        },
        "pebble.display.red.offset":{
            digits:true
        },
        "pebble.display.saturation":{
            digits:true
        },
        "pebble.display.sharpness":{
            digits:true
        },
        "pebble.display.temp.cold":{
            digits:true
        },
        "pebble.display.temp.hot":{
            digits:true
        },
        "pebble.display.temp.warm":{
            digits:true
        },
        
    }; 

    var rules_merlin = {
        "camera.0.exposure":{
            digits:true
        },
        "camera.0.gain":{
            digits:true
        },
        "camera.0.master":{
            digits:true
        },
        "camera.0.streamH.bitrate":{
            digits:true
        },
        "camera.0.streamH.framerate":{
            digits:true
        },
        "camera.0.streamH.gopsize":{
            digits:true
        },
        "camera.0.streamH.storage.diskquota":{
            digits:true
        },
        "camera.0.streamH.storage.recordmode":{
            digits:true
        },
        "camera.0.streamH.storage.recordpostamble":{
            digits:true
        },
        "camera.0.streamH.storage.recordpreamble":{
            digits:true
        },
        "camera.0.streamH.storage.vaqa.0.qty":{
            digits:true
        },
        "camera.0.streamH.storage.vaqa.1.qty":{
            digits:true
        },
        "camera.0.streamL.bitrate":{
            digits:true
        },
        "camera.0.streamL.framerate":{
            digits:true
        },
        "camera.0.streamL.gopsize":{
            digits:true
        },
        "camera.0.streamL.storage.diskquota":{
            digits:true
        },
        "camera.0.streamL.storage.recordmode":{
            digits:true
        },
        "camera.0.streamL.storage.recordpostamble":{
            digits:true
        },
        "camera.0.streamL.storage.recordpreamble":{
            digits:true
        },
        "camera.1.streamH.bitrate":{
            digits:true
        },
        "camera.1.streamH.framerate":{
            digits:true
        },
        "camera.1.streamH.gopsize":{
            digits:true
        },
        "camera.1.streamH.storage.recordpostamble":{
            digits:true
        },
        "camera.1.streamH.storage.recordpreamble":{
            digits:true
        },
        "camera.1.streamH.storage.vaqa.0.qty":{
            digits:true
        },
        "camera.1.streamH.storage.vaqa.1.qty":{
            digits:true
        },
        "camera.1.streamL.bitrate":{
            digits:true
        },
        "camera.1.streamL.framerate":{
            digits:true
        },
        "camera.1.streamL.gopsize":{
            digits:true
        },
        "camera.1.streamL.storage.recordpostamble":{
            digits:true
        },
        "camera.1.streamL.storage.recordpreamble":{
            digits:true
        },
        "evtgrab.capint":{
            digits:true
        },
        "evtgrab.dint":{
            digits:true
        },
        "evtgrab.nfiles":{
            digits:true
        },
        "rtsp.service":{
            digits:true
        },
        "storage.maxsessions":{
            digits:true
        },
        "storageserver.cache_size":{
            digits:true
        },
        "storageserver.max_clip_size":{
            digits:true
        },
        "alarm.redis.memory_threshold":{
            digits:true
        },
        "application_server_port":{
            digits:true
        },
        "network.eth-x.netmask":{
            digits:true
        },

    }; 

    var rules_cnext = {
        
        "alarm.redis.memory_threshold":{
            digits:true
        },
        "application_server_port":{
            digits:true
        },
        "network.wlan-x.security.bgscan-long-interval":{
            digits:true
        },
        "network.wlan-x.security.bgscan-short-interval":{
            digits:true
        },
        "network.wlan-x.security.bgscan-signal-threshold":{
            digits:true
        },
        "network.wlan-y.security.bgscan-long-interval":{
            digits:true
        },
        "network.wlan-y.security.bgscan-short-interval":{
            digits:true
        },
        "network.wlan-y.security.bgscan-signal-threshold":{
            digits:true
        },
        "network.eth-x.netmask":{
            digits:true
        },

        "lctrl.dimmer.vmax":{
            digits:true
        },
        "lctrl.ltype":{
            digits:true
        },
        "lctrl.lvlrate.fast":{
            digits:true
        },
        "lctrl.lvlrate.light":{
            digits:true
        },
        "lctrl.lvlrate.motion":{
            digits:true
        },
        "lctrl.lvlrate.norm":{
            digits:true
        },
        "lctrl.trigger.nonett":{
            digits:true
        },
        "lctrl.trigger.off.dint":{
            digits:true
        },
        "lctrl.trigger.offt":{
            digits:true
        },
        "lctrl.trigger.on.dint":{
            digits:true
        },
        "lctrl.trigger.ont":{
            digits:true
        },
        "pwr.noload.i":{
            digits:true
        },
        "pwr.noload.mode":{
            digits:true
        },

    }; 

    if(this.state.model == "unode-v2"){
      this.props.errors = helpers.checkValidity(this.state, rules_unodev2);
    }
    else if(this.state.model == "unode-v3"){
      this.props.errors = helpers.checkValidity(this.state, rules_unodev3);
    }
    else if(this.state.model == "unode-v4"){
      this.props.errors = helpers.checkValidity(this.state, rules_unodev4);
    }
    else if ((this.state.model == "unode-v5") || (this.state.model == "unode-v6")){
      this.props.errors = helpers.checkValidity(this.state, rules);
    }
    else if(this.state.model == "falcon-q"){
        this.props.errors = helpers.checkValidity(this.state, rules_falconq);
    } 
    else if(this.state.model == "vdkmaster"){
        this.props.errors = helpers.checkValidity(this.state, rules_vdkmaster);
    }
    else if(this.state.model == "merlin"){
        this.props.errors = helpers.checkValidity(this.state, rules_merlin);
    } 
    else{
        this.props.errors = helpers.checkValidity(this.state, rules_cnext);
    }

    console.log(this.props.errors);
    return (Object.keys(this.props.errors).length == 0);
  },
    closemodal:function(){
        $('#configWrapper').animate({'top':'-1097px'},500,function(){
            $('#configForm').fadeOut('fast');
            $('#rubix-nav-header').removeClass('newBackDrop');
            $('#body').removeClass('bodyBackdrop');
            $('#logoutButton').addClass('overlayGrey');
        });
    },


    handleSubmit: function(e) {
    e.stopPropagation();
    e.preventDefault();

    if (this.isValid()) {
      this.props.errors={};
      if(this.state.nodes.length>0){
        console.log("Updating assigned nodes");
        ReactBootstrap.Dispatcher.emit("Configform.update", "open", Object.assign({},this.state));
          $('.configForm').css("display","none");
      }
      else{
        console.log("calling configform.save");
        ReactBootstrap.Dispatcher.emit("Configform.save", Object.assign({},this.state));
          $('.configForm').css("display","none");
      }

    } else {
      alert("Some fields are invalid");
      this.forceUpdate();
    }
    return false;
  },

  handleDelete: function(e) {
    e.preventDefault();
    console.log(this.state.nodes.length);
    if(this.state.nodes.length>0){
      ReactBootstrap.Dispatcher.emit("Configform.deleteconfigwithnodes", "opendelete", this.state.nodes.length);
        this.closemodal();
        $('.newConfigForm').css("display","none");

    }
    else{
      if (confirm("Are you sure you want to Delete this Config?")) {
        ReactBootstrap.Dispatcher.emit("Configform.delete", Object.assign({},this.state));
          this.closemodal();
          $('.newConfigForm').css("display","none");
      };
    }
  },

    callPreviousItem:function(allItems, currentItem){
        $('.config-previous').keyup();
        $("#Config-grid").data("gridInstance");
        console.log($("#Config-gridd").data("gridInstance"));
        var currentRow = $("#Config-grid").data("gridInstance").getSelectedRows();
        var nextRow =  currentRow[0] -1
        $("#Config-grid").data("gridInstance").setSelectedRows([nextRow])
    },

    callNextItem:function(allItems, currentItem){
        $('.config-next').keydown();
        $("#Config-grid").data("gridInstance");
        console.log($("#Config-grid").data("gridInstance"));
        var currentRow = $("#Config-grid").data("gridInstance").getSelectedRows();
        var nextRow =  currentRow[0]+ 1;
        $("#Config-grid").data("gridInstance").setSelectedRows([nextRow])
    },


    handleDuplicate: function(e) {
    e.preventDefault();
    if (confirm("Are you sure you want to Duplicate this Config?")) {
      ReactBootstrap.Dispatcher.emit("Configform.duplicateconfig", Object.assign({},this.state));
    };
  },

  componentWillReceiveProps: function(nextProps){

    if (this.props.config.configid != nextProps.config.configid){
      this.setState(nextProps.config);
    };

    nextProps.errors = {};

  },

  componentDidMount: function() {
    this.props.errors = {};
  },

  render: function() {

      var allItemsLength = this.props.allConfigs.length;
      var firstItem = 0;
      var lastItem = allItemsLength - 1;
      for( var a=0; a < allItemsLength; a++){
          if(this.props.allConfigs[a].configid === this.props.config.configid){
              var noNextItem = lastItem === a ? {display:"none"}:{};
              var noPreviousItem = firstItem === a ? {display:"none"}:{};
          }
      }
      var previousNextButtons = this.props.config.configid === "" || this.props.config.configid === "default" ? {display:"none"}:{};

      var hstyle = {overflowY:"auto",overflowX:"hidden",marginBottom:"16px",

                  maxHeight:helpers.calcHeight(100, -320)+"px !important"};
    var config = this.props.config;
    var heading = (config.configid=="")?"Add Config":(<span> {config.name} </span>);
    var wifiVisibility = ((config.model == "unode-v5") || (config.model == "unode-v6"))?{display:"none"}:{};
    var auxPowerVisibility = ((config.model == "unode-v2") || (config.model == "unode-v3"))?{display:"none"}:{};
    var notValid = (config.model == "unode-v2")?{display:"none"}:{};
    var zmotionsensors = ((config.model == "unode-v2")||(config.model == "unode-v3"))?{}:{display:"none"};
    var podbussensors = ((config.model == "unode-v4")||(config.model == "unode-v5")||(config.model == "unode-v6"))?{}:{display:"none"};
    var podbusDisableVisibility = ((config.model == "unode-v4")||(config.model == "unode-v5")||(config.model == "unode-v6"))?{}:{display:"none"};
    var mainRelayCyclesSensorPollVisibility = ((config.model == "unode-v3")||(config.model == "unode-v4")||(config.model == "unode-v5")||(config.model == "unode-v6"))?{}:{display:"none"};
    var cellularVisibility = ((config.model == "unode-v5") || (config.model == "unode-v6"))?{}:{display:"none"};
    console.log("configid" + config.configid);
    var deleteduplicateButtonVisibility = ((typeof config.configid == 'undefined')||(config.configid=="default"))?{display:"none"}:{};
    var isDisabled = !auth.allowed('CAN_UPDATE', 'ConfigModel');
    var isDisabledButtons = auth.allowed('CAN_UPDATE', 'ConfigModel')?{}:{display:"none"};

    if(Object.keys(config).length > 0){
    
    var selectedModel = config.model;
    var that = this;

    // Fields:
    var modelData = {
     
      "unode-v2" : [  {id: "name",type:"text",label: "Name:"},
                      {id: "model",type:"text",disabled:"disabled",label: "Model:"},
                      {id: "networkXPasskey",type:"password",label: "Network X Passkey:"},
                      {id: "networkXSSID",type:"text",label: "Network X SSID:"},
                      {id: "networkXSecurity",type:"select",options:[{id:"wpa2p",value:"WPA2 personal"}],label: "Network X Security:"},
                      {id: "networkYPasskey",type:"password",label: "Network Y Passkey:"},
                      {id: "networkYSSID",type:"text",label: "Network Y SSID:"},
                      {id: "networkYSecurity",type:"select",options:[{id:"wpa2p",value:"WPA2 personal"}],label: "Network Y Security:"},
                      {id: "network_region",type:"text",label: "Network Region:"},
                      {id: "debugmode",type:"checkbox",label: "Debug Mode:"},
                      {id: "telnet",type:"checkbox",label: "Telnet:"},
                      {id: "sensor_rf_pint",type:"text",label: "Received Signal Strength Poll Interval (sec):"},
                      {id: "sensor_rf_dint",type:"text",label: "Received Signal Strength Debounce Interval (sec):"},
                      {id: "sensor_rf_mode",type:"text",label: "Received Signal Strength Mode:"},
                      {id: "sensor_v_pint",type:"text",label: "Voltage Poll Interval (sec):"},
                      {id: "sensor_v_dint",type:"text",label: "Voltage Debounce Interval (sec):"},
                      {id: "sensor_v_mode",type:"text",label: "Voltage Mode:"},
                      {id: "sensor_lIR_pint",type:"text",label: "Infrared Sensor Poll Interval (sec):"},
                      {id: "sensor_lIR_dint",type:"text",label: "Infrared Sensor Debounce Interval (sec):"},
                      {id: "sensor_lIR_mode",type:"text",label: "Infrared Sensor Mode:"},
                      {id: "sensor_l_pint",type:"text",label: "Light Level Poll Interval (sec):"},
                      {id: "sensor_l_dint",type:"text",label: "Light Level Debounce Interval (sec):"},
                      {id: "sensor_l_mode",type:"text",label: "Light Level Mode:"},
                      {id: "sensor_p_pint",type:"text",label: "Presence (Motion Detector) Poll Interval (sec):"},
                      {id: "sensor_p_dint",type:"text",label: "Presence (Motion Detector) Debounce Interval (sec):"},
                      {id: "sensor_p_mode",type:"text",label: "Presence (Motion Detector) Mode:"},
                      {id: "sensor_t_pint",type:"text",label: "Ambient Temperature Poll Interval (sec):"},
                      {id: "sensor_t_dint",type:"text",label: "Ambient Temperature Debounce Interval (sec):"},
                      {id: "sensor_t_mode",type:"text",label: "Ambient Temperature Mode:"},
                      {id: "sensor_T_pint",type:"text",label: "Node Temperature Poll Interval (sec):"},
                      {id: "sensor_T_dint",type:"text",label: "Node Temperature Debounce Interval (sec):"},
                      {id: "sensor_T_mode",type:"text",label: "Node Temperature Mode:"},
                      {id: "sensor_pdc_pint",type:"text",label: "Zmotion PIR DC Value Poll Interval (sec):"},
                      {id: "sensor_pdc_dint",type:"text",label: "Zmotion PIR DC Value Debounce Interval (sec):"},
                      {id: "sensor_pdc_mode",type:"text",label: "Zmotion PIR DC Value Mode:"},
                      {id: "sensor_ppr_pint",type:"text",label: "Zmotion PIR Process Rate Poll Interval (sec):"},
                      {id: "sensor_ppr_dint",type:"text",label: "Zmotion PIR Process Rate Debounce Interval (sec):"},
                      {id: "sensor_ppr_mode",type:"text",label: "Zmotion PIR Process Rate Mode:"},
                      {id: "sensor_pnd_pint",type:"text",label: "Zmotion EM Noise Detected Poll Interval (sec):"},
                      {id: "sensor_pnd_dint",type:"text",label: "Zmotion EM Noise Detected Debounce Interval (sec):"},
                      {id: "sensor_pnd_mode",type:"text",label: "Zmotion EM Noise Detected Mode:"},
                      {id: "sensor_pdt_pint",type:"text",label: "Zmotion EM Transient Detected Poll Interval (sec):"},
                      {id: "sensor_pdt_dint",type:"text",label: "Zmotion EM Transient Detected Debounce Interval (sec):"},
                      {id: "sensor_pdt_mode",type:"text",label: "Zmotion EM Transient Detected Mode:"},
                      {id: "sensor_ppr_mode",type:"text",label: "Zmotion PIR Process Rate Mode:"},
                      {id: "lctrl_dimmer_vmax",type:"select",options:[{id:"10000",value:"0-10V LED driver"},{id:"8000",value:"0-8V LED driver"}],label: "Maximum Dimming Control Output Voltage (millivolts):"},
                    ],
      "unode-v3" : [  {id: "name",type:"text",label: "Name:"},
                      {id: "model",type:"text",disabled:"disabled",label: "Model:"},
                      {id: "networkXPasskey",type:"password",label: "Network X Passkey:"},
                      {id: "networkXSSID",type:"text",label: "Network X SSID:"},
                      {id: "networkXSecurity",type:"select",options:[{id:"wpa2p",value:"WPA2 personal"}],label: "Network X Security:"},
                      {id: "networkYPasskey",type:"password",label: "Network Y Passkey:"},
                      {id: "networkYSSID",type:"text",label: "Network Y SSID:"},
                      {id: "networkYSecurity",type:"select",options:[{id:"wpa2p",value:"WPA2 personal"}],label: " Network Y Security:"},
                      {id: "network_region",type:"text",label: "Network Region:"},
                      {id: "debugmode",type:"checkbox",label: "Debug Mode:"},
                      {id: "telnet",type:"checkbox",label: "Telnet:"},
                      {id: "sensor_rf_pint",type:"text",label: "Received Signal Strength Poll Interval (sec):"},
                      {id: "sensor_rf_dint",type:"text",label: "Received Signal Strength Debounce Interval (sec):"},
                      {id: "sensor_rf_mode",type:"text",label: "Received Signal Strength Mode:"},
                      {id: "sensor_v_pint",type:"text",label: "Voltage Poll Interval (sec):"},
                      {id: "sensor_v_dint",type:"text",label: "Voltage Debounce Interval (sec):"},
                      {id: "sensor_v_mode",type:"text",label: "Voltage Mode:"},
                      {id: "sensor_mi_pint",type:"text",label: "Main Current Poll Interval (sec):"},
                      {id: "sensor_mi_dint",type:"text",label: "Main Current Debounce Interval (sec):"},
                      {id: "sensor_mi_mode",type:"text",label: "Main Current Mode:"},
                      {id: "sensor_lIR_pint",type:"text",label: "Infrared Sensor Poll Interval (sec):"},
                      {id: "sensor_lIR_dint",type:"text",label: "Infrared Sensor Debounce Interval (sec):"},
                      {id: "sensor_lIR_mode",type:"text",label: "Infrared Sensor Mode:"},
                      {id: "sensor_l_pint",type:"text",label: "Light Level Poll Interval (sec):"},
                      {id: "sensor_l_dint",type:"text",label: "Light Level Debounce Interval (sec):"},
                      {id: "sensor_l_mode",type:"text",label: "Light Level Mode:"},
                      {id: "sensor_p_pint",type:"text",label: "Presence (Motion Detector) Poll Interval (sec):"},
                      {id: "sensor_p_dint",type:"text",label: "Presence (Motion Detector) Debounce Interval (sec):"},
                      {id: "sensor_p_mode",type:"text",label: "Presence (Motion Detector) Mode:"},
                      {id: "sensor_a_jdelta",type:"text",label: "Jolt Threshold:"},
                      {id: "sensor_t_pint",type:"text",label: "Ambient Temperature Poll Interval (sec):"},
                      {id: "sensor_t_dint",type:"text",label: "Ambient Temperature Debounce Interval (sec):"},
                      {id: "sensor_t_mode",type:"text",label: "Ambient Temperature Mode:"},
                      {id: "sensor_T_pint",type:"text",label: "Node Temperature Poll Interval (sec):"},
                      {id: "sensor_T_dint",type:"text",label: "Node Temperature Debounce Interval (sec):"},
                      {id: "sensor_T_mode",type:"text",label: "Node Temperature Mode:"},
                      {id: "sensor_pdc_pint",type:"text",label: "Zmotion PIR DC Value Poll Interval (sec):"},
                      {id: "sensor_pdc_dint",type:"text",label: "Zmotion PIR DC Value Debounce Interval (sec):"},
                      {id: "sensor_pdc_mode",type:"text",label: "Zmotion PIR DC Value Mode:"},
                      {id: "sensor_ppr_pint",type:"text",label: "Zmotion PIR Process Rate Poll Interval (sec):"},
                      {id: "sensor_ppr_dint",type:"text",label: "Zmotion PIR Process Rate Debounce Interval (sec):"},
                      {id: "sensor_ppr_mode",type:"text",label: "Zmotion PIR Process Rate Mode:"},
                      {id: "sensor_pnd_pint",type:"text",label: "Zmotion EM Noise Detected Poll Interval (sec):"},
                      {id: "sensor_pnd_dint",type:"text",label: "Zmotion EM Noise Detected Debounce Interval (sec):"},
                      {id: "sensor_pnd_mode",type:"text",label: "Zmotion EM Noise Detected Mode:"},
                      {id: "sensor_pdt_pint",type:"text",label: "Zmotion EM Transient Detected Poll Interval (sec):"},
                      {id: "sensor_pdt_dint",type:"text",label: "Zmotion EM Transient Detected Debounce Interval (sec):"},
                      {id: "sensor_pdt_mode",type:"text",label: "Zmotion EM Transient Detected Mode:"},
                      {id: "sensor_ppr_mode",type:"text",label: "Zmotion PIR Process Rate Mode:"},
                      {id: "lctrl_dimmer_vmax",type:"select",options:[{id:"10000",value:"0-10V LED driver"},{id:"8000",value:"0-8V LED driver"}],label: "Maximum Dimming Control Output Voltage (millivolts):"},
                    ],
      "unode-v4" : [  {id: "name",type:"text",label: "Name:"},
                      {id: "model",type:"text",disabled:"disabled",label: "Model:"},
                      {id: "networkXPasskey",type:"password",label: "Network X Passkey:"},
                      {id: "networkXSSID",type:"text",label: "Network X SSID:"},
                      {id: "networkXSecurity",type:"select",options:[{id:"wpa2p",value:"WPA2 personal"}],label: "Network X Security:"},
                      {id: "networkYPasskey",type:"password",label: "Network Y Passkey:"},
                      {id: "networkYSSID",type:"text",label: "Network Y SSID:"},
                      {id: "networkYSecurity",type:"select",options:[{id:"wpa2p",value:"WPA2 personal"}],label: " Network Y Security:"},
                      {id: "network_region",type:"text",label: "Network Region:"},
                      {id: "debugmode",type:"checkbox",label: "Debug Mode:"},
                      {id: "telnet",type:"checkbox",label: "Telnet:"},
                      {id: "aux_power",type:"checkbox",label: "AUX Power:"},
                      {id: "podbus_disable",type:"checkbox",label: "Podbus Disable:"},
                      {id: "sensor_rf_pint",type:"text",label: "Received Signal Strength Poll Interval (sec):"},
                      {id: "sensor_rf_dint",type:"text",label: "Received Signal Strength Debounce Interval (sec):"},
                      {id: "sensor_rf_mode",type:"text",label: "Received Signal Strength Mode:"},
                      {id: "sensor_v_pint",type:"text",label: "Voltage Poll Interval (sec):"},
                      {id: "sensor_v_dint",type:"text",label: "Voltage Debounce Interval (sec):"},
                      {id: "sensor_v_mode",type:"text",label: "Voltage Mode:"},
                      {id: "sensor_aw_pint",type:"text",label: "Auxiliary Energy Use Poll Interval (sec):"},
                      {id: "sensor_aw_dint",type:"text",label: "Auxiliary Energy Use Debounce Interval (sec):"},
                      {id: "sensor_aw_mode",type:"text",label: "Auxiliary Energy Use Mode:"},
                      {id: "sensor_aPF_pint",type:"text",label: "Auxiliary Power Factor Poll Interval (sec):"},
                      {id: "sensor_aPF_dint",type:"text",label: "Auxiliary Power Factor Debounce Interval (sec):"},
                      {id: "sensor_aPF_mode",type:"text",label: "Auxiliary Power Factor Mode:"},
                      {id: "sensor_aP_pint",type:"text",label: "Auxiliary Power Poll Interval (sec):"},
                      {id: "sensor_aP_dint",type:"text",label: "Auxiliary Power Debounce Interval (sec):"},
                      {id: "sensor_aP_mode",type:"text",label: "Auxiliary Power Mode:"},
                      {id: "sensor_mP_pint",type:"text",label: "Main Power Poll Interval (sec):"},
                      {id: "sensor_mP_dint",type:"text",label: "Main Power Debounce Interval (sec):"},
                      {id: "sensor_mP_mode",type:"text",label: "Main Power Mode:"},
                      {id: "sensor_mip_pint",type:"text",label: "Main Current Spike Poll Interval (sec):"},
                      {id: "sensor_mip_dint",type:"text",label: "Main Current Spike Debounce Interval (sec):"},
                      {id: "sensor_mip_mode",type:"text",label: "Main Current Spike Mode:"},
                      {id: "sensor_ai_pint",type:"text",label: "Auxiliary Current Poll Interval (sec):"},
                      {id: "sensor_ai_dint",type:"text",label: "Auxiliary Current Debounce Interval (sec):"},
                      {id: "sensor_ai_mode",type:"text",label: "Auxiliary Current Mode:"},
                      {id: "sensor_aip_pint",type:"text",label: "Auxiliary Current Spike Poll Interval (sec):"},
                      {id: "sensor_aip_dint",type:"text",label: "Auxiliary Current Spike Debounce Interval (sec):"},
                      {id: "sensor_aip_mode",type:"text",label: "Auxiliary Current Spike Mode:"},
                      {id: "sensor_mi_pint",type:"text",label: "Main Current Poll Interval (sec):"},
                      {id: "sensor_mi_dint",type:"text",label: "Main Current Debounce Interval (sec):"},
                      {id: "sensor_mi_mode",type:"text",label: "Main Current Mode:"},
                      {id: "sensor_mw_pint",type:"text",label: "Main Energy Use Poll Interval (sec):"},
                      {id: "sensor_mw_dint",type:"text",label: "Main Energy Use Debounce Interval (sec):"},
                      {id: "sensor_mw_mode",type:"text",label: "Main Energy Use Mode:"},
                      {id: "sensor_mPF_pint",type:"text",label: "Main Power Factor Poll Interval (sec):"},
                      {id: "sensor_mPF_dint",type:"text",label: "Main Power Factor Debounce Interval (sec):"},
                      {id: "sensor_mPF_mode",type:"text",label: "Main Power Factor Mode:"},
                      {id: "sensor_lIR_pint",type:"text",label: "Infrared Sensor Poll Interval (sec):"},
                      {id: "sensor_lIR_dint",type:"text",label: "Infrared Sensor Debounce Interval (sec):"},
                      {id: "sensor_lIR_mode",type:"text",label: "Infrared Sensor Mode:"},
                      {id: "sensor_l_pint",type:"text",label: "Light Level Poll Interval (sec):"},
                      {id: "sensor_l_dint",type:"text",label: "Light Level Debounce Interval (sec):"},
                      {id: "sensor_l_mode",type:"text",label: "Light Level Mode:"},
                      {id: "sensor_p_pint",type:"text",label: "Presence (Motion Detector) Poll Interval (sec):"},
                      {id: "sensor_p_dint",type:"text",label: "Presence (Motion Detector) Debounce Interval (sec):"},
                      {id: "sensor_p_mode",type:"text",label: "Presence (Motion Detector) Mode:"},
                      {id: "sensor_a_jdelta",type:"text",label: "Jolt Threshold:"},
                      {id: "sensor_pc_pint",type:"text",label: "Presence Count Poll Interval (sec):"},
                      {id: "sensor_pc_pint",type:"text",label: "Presence Count Debounce Interval (sec):"},
                      {id: "sensor_pc_mode",type:"text",label: "Presence Count Mode:"},
                      {id: "sensor_t_pint",type:"text",label: "Ambient Temperature Poll Interval (sec):"},
                      {id: "sensor_t_dint",type:"text",label: "Ambient Temperature Debounce Interval (sec):"},
                      {id: "sensor_t_mode",type:"text",label: "Ambient Temperature Mode:"},
                      {id: "sensor_T_pint",type:"text",label: "Node Temperature Poll Interval (sec):"},
                      {id: "sensor_T_dint",type:"text",label: "Node Temperature Debounce Interval (sec):"},
                      {id: "sensor_T_mode",type:"text",label: "Node Temperature Mode:"},
                      {id: "sensor_mt_pint",type:"text",label: "MCU Temperature Poll Interval (sec):"},
                      {id: "sensor_mt_dint",type:"text",label: "MCU Temperature Debounce Interval (sec):"},
                      {id: "sensor_mt_mode",type:"text",label: "MCU Temperature Mode:"},
                      {id: "sensor_podm_pint",type:"text",label: "Total Message And Error Count Poll Interval (sec):"},
                      {id: "sensor_podm_dint",type:"text",label: "Total Message And Error Count Debounce Interval (sec):"},
                      {id: "sensor_podm_mode",type:"text",label: "Total Message And Error Count Mode:"},
                      {id: "sensor_podme_pint",type:"text",label: "Response Error Count Poll Interval (sec):"},
                      {id: "sensor_podme_dint",type:"text",label: "Response Error Count Debounce Interval (sec):"},
                      {id: "sensor_podme_mode",type:"text",label: "Response Error Count Mode:"},
                      {id: "sensor_rlym_pint",type:"text",label: "Main Relay Cycle Poll Interval (sec):"},
                      {id: "lctrl_dimmer_vmax",type:"select",options:[{id:"10000",value:"0-10V LED driver"},{id:"8000",value:"0-8V LED driver"}],label: "Maximum Dimming Control Output Voltage (millivolts):"},
                      {id: "lctrl_trigger_off_dint",type:"text",label: "Lctrl Trigger Off Dint (sec):"},
                      {id: "lctrl_trigger_on_dint",type:"text",label: "Lctrl Trigger On Dint (sec):"},
                      {id: "lctrl_trigger_ont",type:"text",label: "Lctrl Trigger Ont (lux):"},
                      {id: "lctrl_trigger_offt",type:"text",label: "Lctrl Trigger Offt (lux):"},
                      {id: "lctrl_trigger_nonett",type:"text",label: "Lctrl Trigger Nonett (sec):"},
                    ],
      "unode-v5" : [ {id: "name",type:"text",label: "Name:"},
                      {id: "model",type:"text",disabled:"disabled",label: "Model:"},
                      {id: "network_region",type:"text",label: "Network Region:"},
                      {id: "debugmode",type:"checkbox",label: "Debug Mode:"},
                      {id: "telnet",type:"checkbox",label: "Telnet:"},
                      {id: "aux_power",type:"checkbox",label: "AUX Power:"},
                      {id: "podbus_disable",type:"checkbox",label: "Podbus Disable:"},
                      {id: "sensor_rf_pint",type:"text",label: "Received Signal Strength Poll Interval (sec):"},
                      {id: "sensor_rf_dint",type:"text",label: "Received Signal Strength Debounce Interval (sec):"},
                      {id: "sensor_rf_mode",type:"text",label: "Received Signal Strength Mode:"},
                      {id: "sensor_v_pint",type:"text",label: "Voltage Poll Interval (sec):"},
                      {id: "sensor_v_dint",type:"text",label: "Voltage Debounce Interval (sec):"},
                      {id: "sensor_v_mode",type:"text",label: "Voltage Mode:"},
                      {id: "sensor_aw_pint",type:"text",label: "Auxiliary Energy Use Poll Interval (sec):"},
                      {id: "sensor_aw_dint",type:"text",label: "Auxiliary Energy Use Debounce Interval (sec):"},
                      {id: "sensor_aw_mode",type:"text",label: "Auxiliary Energy Use Mode:"},
                      {id: "sensor_aPF_pint",type:"text",label: "Auxiliary Power Factor Poll Interval (sec):"},
                      {id: "sensor_aPF_dint",type:"text",label: "Auxiliary Power Factor Debounce Interval (sec):"},
                      {id: "sensor_aPF_mode",type:"text",label: "Auxiliary Power Factor Mode:"},
                      {id: "sensor_aP_pint",type:"text",label: "Auxiliary Power Poll Interval (sec):"},
                      {id: "sensor_aP_dint",type:"text",label: "Auxiliary Power Debounce Interval (sec):"},
                      {id: "sensor_aP_mode",type:"text",label: "Auxiliary Power Mode:"},
                      {id: "sensor_mP_pint",type:"text",label: "Main Power Poll Interval (sec):"},
                      {id: "sensor_mP_dint",type:"text",label: "Main Power Debounce Interval (sec):"},
                      {id: "sensor_mP_mode",type:"text",label: "Main Power Mode:"},
                      {id: "sensor_mip_pint",type:"text",label: "Main Current Spike Poll Interval (sec):"},
                      {id: "sensor_mip_dint",type:"text",label: "Main Current Spike Debounce Interval (sec):"},
                      {id: "sensor_mip_mode",type:"text",label: "Main Current Spike Mode:"},
                      {id: "sensor_ai_pint",type:"text",label: "Auxiliary Current Poll Interval (sec):"},
                      {id: "sensor_ai_dint",type:"text",label: "Auxiliary Current Debounce Interval (sec):"},
                      {id: "sensor_ai_mode",type:"text",label: "Auxiliary Current Mode:"},
                      {id: "sensor_aip_pint",type:"text",label: "Auxiliary Current Spike Poll Interval (sec):"},
                      {id: "sensor_aip_dint",type:"text",label: "Auxiliary Current Spike Debounce Interval (sec):"},
                      {id: "sensor_aip_mode",type:"text",label: "Auxiliary Current Spike Mode:"},
                      {id: "sensor_mi_pint",type:"text",label: "Main Current Poll Interval (sec):"},
                      {id: "sensor_mi_dint",type:"text",label: "Main Current Debounce Interval (sec):"},
                      {id: "sensor_mi_mode",type:"text",label: "Main Current Mode:"},
                      {id: "sensor_mw_pint",type:"text",label: "Main Energy Use Poll Interval (sec):"},
                      {id: "sensor_mw_dint",type:"text",label: "Main Energy Use Debounce Interval (sec):"},
                      {id: "sensor_mw_mode",type:"text",label: "Main Energy Use Mode:"},
                      {id: "sensor_mPF_pint",type:"text",label: "Main Power Factor Poll Interval (sec):"},
                      {id: "sensor_mPF_dint",type:"text",label: "Main Power Factor Debounce Interval (sec):"},
                      {id: "sensor_mPF_mode",type:"text",label: "Main Power Factor Mode:"},
                      {id: "sensor_lIR_pint",type:"text",label: "Infrared Sensor Poll Interval (sec):"},
                      {id: "sensor_lIR_dint",type:"text",label: "Infrared Sensor Debounce Interval (sec):"},
                      {id: "sensor_lIR_mode",type:"text",label: "Infrared Sensor Mode:"},
                      {id: "sensor_l_pint",type:"text",label: "Light Level Poll Interval (sec):"},
                      {id: "sensor_l_dint",type:"text",label: "Light Level Debounce Interval (sec):"},
                      {id: "sensor_l_mode",type:"text",label: "Light Level Mode:"},
                      {id: "sensor_l_i_pint",type:"text",label: "Internal Light Level Poll Interval (sec):"},
                      {id: "sensor_l_i_dint",type:"text",label: "Internal Light Level Debounce Interval (sec):"},
                      {id: "sensor_l_i_mode",type:"text",label: "Internal Light Level Mode:"},
                      {id: "sensor_lIR_i_pint",type:"text",label: "Internal Infrared Sensor Poll Interval (sec):"},
                      {id: "sensor_lIR_i_dint",type:"text",label: "Internal Infrared Sensor Debounce Interval (sec):"},
                      {id: "sensor_lIR_i_mode",type:"text",label: "Internal Infrared Sensor Mode:"},
                      {id: "sensor_p_pint",type:"text",label: "Presence (Motion Detector) Poll Interval (sec):"},
                      {id: "sensor_p_dint",type:"text",label: "Presence (Motion Detector) Debounce Interval (sec):"},
                      {id: "sensor_p_mode",type:"text",label: "Presence (Motion Detector) Mode:"},
                      {id: "sensor_pc_pint",type:"text",label: "Presence Count Poll Interval (sec):"},
                      {id: "sensor_pc_pint",type:"text",label: "Presence Count Debounce Interval (sec):"},
                      {id: "sensor_pc_mode",type:"text",label: "Presence Count Mode:"},
                      {id: "sensor_t_pint",type:"text",label: "Ambient Temperature Poll Interval (sec):"},
                      {id: "sensor_t_dint",type:"text",label: "Ambient Temperature Debounce Interval (sec):"},
                      {id: "sensor_t_mode",type:"text",label: "Ambient Temperature Mode:"},
                      {id: "sensor_T_pint",type:"text",label: "Node Temperature Poll Interval (sec):"},
                      {id: "sensor_T_dint",type:"text",label: "Node Temperature Debounce Interval (sec):"},
                      {id: "sensor_T_mode",type:"text",label: "Node Temperature Mode:"},
                      {id: "sensor_mt_pint",type:"text",label: "MCU Temperature Poll Interval (sec):"},
                      {id: "sensor_mt_dint",type:"text",label: "MCU Temperature Debounce Interval (sec):"},
                      {id: "sensor_mt_mode",type:"text",label: "MCU Temperature Mode:"},
                      {id: "sensor_podm_pint",type:"text",label: "Total Message And Error Count Poll Interval (sec):"},
                      {id: "sensor_podm_dint",type:"text",label: "Total Message And Error Count Debounce Interval (sec):"},
                      {id: "sensor_podm_mode",type:"text",label: "Total Message And Error Count Mode:"},
                      {id: "sensor_podme_pint",type:"text",label: "Response Error Count Poll Interval (sec):"},
                      {id: "sensor_podme_dint",type:"text",label: "Response Error Count Debounce Interval (sec):"},
                      {id: "sensor_podme_mode",type:"text",label: "Response Error Count Mode:"},
                      {id: "sensor_rlym_pint",type:"text",label: "Main Relay Cycle Poll Interval (sec):"},
                      {id: "lctrl_dimmer_vmax",type:"select",options:[{id:"10000",value:"0-10V LED driver"},{id:"8000",value:"0-8V LED driver"}],label: "Maximum Dimming Control Output Voltage (millivolts):"},
                      {id: "lctrl_trigger_off_dint",type:"text",label: "Lctrl Trigger Off Dint (sec):"},
                      {id: "lctrl_trigger_on_dint",type:"text",label: "Lctrl Trigger On Dint (sec):"},
                      {id: "lctrl_trigger_ont",type:"text",label: "Lctrl Trigger Ont (lux):"},
                      {id: "lctrl_trigger_offt",type:"text",label: "Lctrl Trigger Offt (lux):"},
                      {id: "lctrl_trigger_nonett",type:"text",label: "Lctrl Trigger Nonett (sec):"},
                    ],
      "unode-v6" : [  {id: "name",type:"text",label: "Name:"},
                      {id: "model",type:"text",disabled:"disabled",label: "Model:"},
                      {id: "network_region",type:"text",label: "Network Region:"},
                      {id: "debugmode",type:"checkbox",label: "Debug Mode:"},
                      {id: "telnet",type:"checkbox",label: "Telnet:"},
                      {id: "aux_power",type:"checkbox",label: "AUX Power:"},
                      {id: "podbus_disable",type:"checkbox",label: "Podbus Disable:"},
                      {id: "sensor_rf_pint",type:"text",label: "Received Signal Strength Poll Interval (sec):"},
                      {id: "sensor_rf_dint",type:"text",label: "Received Signal Strength Debounce Interval (sec):"},
                      {id: "sensor_rf_mode",type:"text",label: "Received Signal Strength Mode:"},
                      {id: "sensor_v_pint",type:"text",label: "Voltage Poll Interval (sec):"},
                      {id: "sensor_v_dint",type:"text",label: "Voltage Debounce Interval (sec):"},
                      {id: "sensor_v_mode",type:"text",label: "Voltage Mode:"},
                      {id: "sensor_aw_pint",type:"text",label: "Auxiliary Energy Use Poll Interval (sec):"},
                      {id: "sensor_aw_dint",type:"text",label: "Auxiliary Energy Use Debounce Interval (sec):"},
                      {id: "sensor_aw_mode",type:"text",label: "Auxiliary Energy Use Mode:"},
                      {id: "sensor_aPF_pint",type:"text",label: "Auxiliary Power Factor Poll Interval (sec):"},
                      {id: "sensor_aPF_dint",type:"text",label: "Auxiliary Power Factor Debounce Interval (sec):"},
                      {id: "sensor_aPF_mode",type:"text",label: "Auxiliary Power Factor Mode:"},
                      {id: "sensor_aP_pint",type:"text",label: "Auxiliary Power Poll Interval (sec):"},
                      {id: "sensor_aP_dint",type:"text",label: "Auxiliary Power Debounce Interval (sec):"},
                      {id: "sensor_aP_mode",type:"text",label: "Auxiliary Power Mode:"},
                      {id: "sensor_mP_pint",type:"text",label: "Main Power Poll Interval (sec):"},
                      {id: "sensor_mP_dint",type:"text",label: "Main Power Debounce Interval (sec):"},
                      {id: "sensor_mP_mode",type:"text",label: "Main Power Mode:"},
                      {id: "sensor_mip_pint",type:"text",label: "Main Current Spike Poll Interval (sec):"},
                      {id: "sensor_mip_dint",type:"text",label: "Main Current Spike Debounce Interval (sec):"},
                      {id: "sensor_mip_mode",type:"text",label: "Main Current Spike Mode:"},
                      {id: "sensor_ai_pint",type:"text",label: "Auxiliary Current Poll Interval (sec):"},
                      {id: "sensor_ai_dint",type:"text",label: "Auxiliary Current Debounce Interval (sec):"},
                      {id: "sensor_ai_mode",type:"text",label: "Auxiliary Current Mode:"},
                      {id: "sensor_aip_pint",type:"text",label: "Auxiliary Current Spike Poll Interval (sec):"},
                      {id: "sensor_aip_dint",type:"text",label: "Auxiliary Current Spike Debounce Interval (sec):"},
                      {id: "sensor_aip_mode",type:"text",label: "Auxiliary Current Spike Mode:"},
                      {id: "sensor_mi_pint",type:"text",label: "Main Current Poll Interval (sec):"},
                      {id: "sensor_mi_dint",type:"text",label: "Main Current Debounce Interval (sec):"},
                      {id: "sensor_mi_mode",type:"text",label: "Main Current Mode:"},
                      {id: "sensor_mw_pint",type:"text",label: "Main Energy Use Poll Interval (sec):"},
                      {id: "sensor_mw_dint",type:"text",label: "Main Energy Use Debounce Interval (sec):"},
                      {id: "sensor_mw_mode",type:"text",label: "Main Energy Use Mode:"},
                      {id: "sensor_mPF_pint",type:"text",label: "Main Power Factor Poll Interval (sec):"},
                      {id: "sensor_mPF_dint",type:"text",label: "Main Power Factor Debounce Interval (sec):"},
                      {id: "sensor_mPF_mode",type:"text",label: "Main Power Factor Mode:"},
                      {id: "sensor_lIR_pint",type:"text",label: "Infrared Sensor Poll Interval (sec):"},
                      {id: "sensor_lIR_dint",type:"text",label: "Infrared Sensor Debounce Interval (sec):"},
                      {id: "sensor_lIR_mode",type:"text",label: "Infrared Sensor Mode:"},
                      {id: "sensor_l_pint",type:"text",label: "Light Level Poll Interval (sec):"},
                      {id: "sensor_l_dint",type:"text",label: "Light Level Debounce Interval (sec):"},
                      {id: "sensor_l_mode",type:"text",label: "Light Level Mode:"},
                      {id: "sensor_l_i_pint",type:"text",label: "Internal Light Level Poll Interval (sec):"},
                      {id: "sensor_l_i_dint",type:"text",label: "Internal Light Level Debounce Interval (sec):"},
                      {id: "sensor_l_i_mode",type:"text",label: "Internal Light Level Mode:"},
                      {id: "sensor_lIR_i_pint",type:"text",label: "Internal Infrared Sensor Poll Interval (sec):"},
                      {id: "sensor_lIR_i_dint",type:"text",label: "Internal Infrared Sensor Debounce Interval (sec):"},
                      {id: "sensor_lIR_i_mode",type:"text",label: "Internal Infrared Sensor Mode:"},
                      {id: "sensor_p_pint",type:"text",label: "Presence (Motion Detector) Poll Interval (sec):"},
                      {id: "sensor_p_dint",type:"text",label: "Presence (Motion Detector) Debounce Interval (sec):"},
                      {id: "sensor_p_mode",type:"text",label: "Presence (Motion Detector) Mode:"},
                      {id: "sensor_pc_pint",type:"text",label: "Presence Count Poll Interval (sec):"},
                      {id: "sensor_pc_pint",type:"text",label: "Presence Count Debounce Interval (sec):"},
                      {id: "sensor_pc_mode",type:"text",label: "Presence Count Mode:"},
                      {id: "sensor_t_pint",type:"text",label: "Ambient Temperature Poll Interval (sec):"},
                      {id: "sensor_t_dint",type:"text",label: "Ambient Temperature Debounce Interval (sec):"},
                      {id: "sensor_t_mode",type:"text",label: "Ambient Temperature Mode:"},
                      {id: "sensor_T_pint",type:"text",label: "Node Temperature Poll Interval (sec):"},
                      {id: "sensor_T_dint",type:"text",label: "Node Temperature Debounce Interval (sec):"},
                      {id: "sensor_T_mode",type:"text",label: "Node Temperature Mode:"},
                      {id: "sensor_mt_pint",type:"text",label: "MCU Temperature Poll Interval (sec):"},
                      {id: "sensor_mt_dint",type:"text",label: "MCU Temperature Debounce Interval (sec):"},
                      {id: "sensor_mt_mode",type:"text",label: "MCU Temperature Mode:"},
                      {id: "sensor_podm_pint",type:"text",label: "Total Message And Error Count Poll Interval (sec):"},
                      {id: "sensor_podm_dint",type:"text",label: "Total Message And Error Count Debounce Interval (sec):"},
                      {id: "sensor_podm_mode",type:"text",label: "Total Message And Error Count Mode:"},
                      {id: "sensor_podme_pint",type:"text",label: "Response Error Count Poll Interval (sec):"},
                      {id: "sensor_podme_dint",type:"text",label: "Response Error Count Debounce Interval (sec):"},
                      {id: "sensor_podme_mode",type:"text",label: "Response Error Count Mode:"},
                      {id: "sensor_rlym_pint",type:"text",label: "Main Relay Cycle Poll Interval (sec):"},
                      {id: "lctrl_dimmer_vmax",type:"select",options:[{id:"10000",value:"0-10V LED driver"},{id:"8000",value:"0-8V LED driver"}],label: "Maximum Dimming Control Output Voltage (millivolts):"},
                      {id: "lctrl_trigger_off_dint",type:"text",label: "Lctrl Trigger Off Dint (sec):"},
                      {id: "lctrl_trigger_on_dint",type:"text",label: "Lctrl Trigger On Dint (sec):"},
                      {id: "lctrl_trigger_ont",type:"text",label: "Lctrl Trigger Ont (lux):"},
                      {id: "lctrl_trigger_offt",type:"text",label: "Lctrl Trigger Offt (lux):"},
                      {id: "lctrl_trigger_nonett",type:"text",label: "Lctrl Trigger Nonett (sec):"},
                    ],

        "falcon-q" :[ {id: "name",type:"text",label: "Name:"},
                      {id: "model",type:"text",disabled:"disabled",label: "Model:"},
                      /*{id: "Genetec.Network.Services.Http.Port",disabled:"disabled",type:"text",label: "Genetec Network Services Http Port:"},*/
                      {id: "Genetec.custom_events",type:"text",label: "Genetec Custom Events:"},
                      {id: "alarm.redis.memory_threshold",type:"text",label: "Alarm Redis Memory Threshold:"},
                      {id: "application_server",type:"text",label: "Application Server:"},
                      {id: "application_server_port",type:"text",label: "Application Server Port:"},
                      {id: "camera.0.autoexposure",type:"checkbox",label: "Camera 0 Auto Exposure:"},
                      {id: "camera.0.autogain",type:"checkbox",label: "Camera 0 Auto Gain:"},
                      {id: "camera.0.colortemp",type:"text",label: "Camera 0 Color Temp:"},
                      {id: "camera.0.enabled",type:"checkbox",label: "Camera 0 Enabled:"},
                      {id: "camera.0.exposure",type:"text",label: "Camera 0 Exposure:"},
                      {id: "camera.0.flickermode",type:"text",label: "Camera 0 Flicker Mode:"},
                      {id: "camera.0.gain",type:"text",label: "Camera 0 Gain:"},
                      {id: "camera.0.indoormode",type:"checkbox",label: "Camera 0 Indoor Mode:"},
                      {id: "camera.0.master",type:"text",label: "Camera 0 Master:"},
                      {id: "camera.0.scanmode",type:"select",options:[{id:"none",value:"none"},{id:"normal",value:"normal"},{id:"flip",value:"flip"},{id:"both",value:"both"},{id:"mirror",value:"mirror"}],label: "Camera 0 Scan Mode:"},
                      {id: "camera.0.streamH.bitrate",type:"text",label: "Camera 0 StreamH Bitrate:"},
                      {id: "camera.0.streamH.format",type:"text",label: "Camera 0 StreamH Format:"},
                      {id: "camera.0.streamH.framerate",type:"text",label: "Camera 0 StreamH framerate:"},
                      {id: "camera.0.streamH.gopsize",type:"text",label: "Camera 0 StreamH gopsize:"},
                      {id: "camera.0.streamH.storage.diskquota",type:"text",label: "Camera 0 StreamH Storage Diskquota:"},
                      {id: "camera.0.streamH.storage.recordevents",type:"text",label: "Camera 0 StreamH Storage Recordevents:"},
                      {id: "camera.0.streamH.storage.recordmode",type:"select",options:[{id:"0",value:"Off"},{id:"1",value:"Continuous"},{id:"2",value:"Event Triggered"}],label: "Camera 0 StreamH Storage Recordmode:"},
                      {id: "camera.0.streamH.storage.recordpostamble",type:"text",label: "Camera 0 StreamH Storage Recordpostamble:"},
                      {id: "camera.0.streamH.storage.recordpreamble",type:"text",label: "Camera 0 StreamH.storage.recordpreamble:"},
                      {id: "camera.0.streamH.storage.vaqa.0.duration",type:"text",label: "Camera 0 streamH.storage.vaqa.0.duration:"},
                      {id: "camera.0.streamH.storage.vaqa.0.interval",type:"text",label: "Camera 0 streamH.storage.vaqa.0.interval:"},
                      {id: "camera.0.streamH.storage.vaqa.0.qty",type:"text",label: "Camera 0 streamH.storage.vaqa.0.qty:"},
                      /*{id: "camera.0.streamH.storage.vaqa.status",type:"text",disabled:"disabled",label: "Camera 0 streamH.storage.vaqa.status:"},*/
                      {id: "camera.0.streamH.storage.vaqa.1.duration",type:"text",label: "Camera 0 streamH.storage.vaqa.1.duration:"},
                      {id: "camera.0.streamH.storage.vaqa.1.interval",type:"text",label: "Camera 0 streamH.storage.vaqa.1.interval:"},
                      {id: "camera.0.streamH.storage.vaqa.1.qty",type:"text",label: "Camera 0 streamH.storage.vaqa.1.qty:"},
                      {id: "camera.0.streamL.bitrate",type:"text",label: "Camera 0 streamL.bitrate:"},
                      {id: "camera.0.streamL.format",type:"text",label: "Camera 0 streamL.format:"},
                      {id: "camera.0.streamL.framerate",type:"text",label: "Camera 0 streamL.framerate:"},
                      {id: "camera.0.streamL.gopsize",type:"text",label: "Camera 0 streamL.gopsize:"},
                      {id: "camera.0.streamL.storage.diskquota",type:"text",label: "Camera 0 streamL.storage.diskquota:"},
                      {id: "camera.0.streamL.storage.recordevents",type:"text",label: "Camera 0 streamL.storage.recordevents:"},
                      {id: "camera.0.streamL.storage.recordmode",type:"select",options:[{id:"0",value:"Off"},{id:"1",value:"Continuous"},{id:"2",value:"Event Triggered"}],label: "Camera 0 streamL.storage.recordmode:"},
                      {id: "camera.0.streamL.storage.recordpostamble",type:"text",label: "Camera 0 streamL.storage.recordpostamble:"},
                      {id: "camera.0.streamL.storage.recordpreamble",type:"text",label: "Camera 0 streamL.storage.recordpreamble:"},
                      {id: "camera.0.wdrmode",type:"checkbox",label: "Camera 0 WDR Mode:"},
                      {id: "camera.1.autoexposure",type:"checkbox",label: "Camera 1 Auto Exposure:"},
                      {id: "camera.1.autogain",type:"checkbox",label: "Camera 1 Auto Gain:"},
                      {id: "camera.1.colortemp",type:"text",label: "Camera 1 Color Temp:"},
                      {id: "camera.1.enabled",type:"checkbox",label: "Camera 1 Enabled:"},
                      {id: "camera.1.exposure",type:"text",label: "Camera 1 Exposure:"},
                      {id: "camera.1.flickermode",type:"text",label: "Camera 1 Flicker Mode:"},
                      {id: "camera.1.gain",type:"text",label: "Camera 1 Gain:"},
                      {id: "camera.1.indoormode",type:"checkbox",label: "Camera 1 Indoor Mode:"},
                      {id: "camera.1.scanmode",type:"select",options:[{id:"none",value:"none"},{id:"normal",value:"normal"},{id:"flip",value:"flip"},{id:"both",value:"both"},{id:"mirror",value:"mirror"}],label: "Camera 1 Scan Mode:"},
                      {id: "camera.1.streamH.bitrate",type:"text",label: "Camera 1 StreamH Bitrate:"},
                      {id: "camera.1.streamH.format",type:"text",label: "Camera 1 StreamH Format:"},
                      {id: "camera.1.streamH.framerate",type:"text",label: "Camera 1 StreamH framerate:"},
                      {id: "camera.1.streamH.gopsize",type:"text",label: "Camera 1 StreamH gopsize:"},
                      {id: "camera.1.streamH.storage.diskquota",type:"text",label: "Camera 1 StreamH Storage Diskquota:"},
                      {id: "camera.1.streamH.storage.recordevents",type:"text",label: "Camera 1 StreamH Storage Recordevents:"},
                      {id: "camera.1.streamH.storage.recordmode",type:"select",options:[{id:"0",value:"Off"},{id:"1",value:"Continuous"},{id:"2",value:"Event Triggered"}],label: "Camera 1 StreamH Storage Recordmode:"},
                      {id: "camera.1.streamH.storage.recordpostamble",type:"text",label: "Camera 1 StreamH Storage Recordpostamble:"},
                      {id: "camera.1.streamH.storage.recordpreamble",type:"text",label: "Camera 1 StreamH.storage.recordpreamble:"},
                      {id: "camera.1.streamH.storage.vaqa.0.duration",type:"text",label: "Camera 1 streamH.storage.vaqa.0.duration:"},
                      {id: "camera.1.streamH.storage.vaqa.0.interval",type:"text",label: "Camera 1 streamH.storage.vaqa.0.interval:"},
                      {id: "camera.1.streamH.storage.vaqa.0.qty",type:"text",label: "Camera 1 streamH.storage.vaqa.0.qty:"},
                      {id: "camera.1.streamH.storage.vaqa.1.duration",type:"text",label: "Camera 1 streamH.storage.vaqa.1.duration:"},
                      {id: "camera.1.streamH.storage.vaqa.1.interval",type:"text",label: "Camera 1 streamH.storage.vaqa.1.interval:"},
                      {id: "camera.1.streamH.storage.vaqa.1.qty",type:"text",label: "Camera 1 streamH.storage.vaqa.1.qty:"},
                      /*{id: "camera.1.streamH.storage.vaqa.status",type:"text",disabled:"disabled",label: "Camera 1 streamH.storage.vaqa.1.status:"},*/
                      {id: "camera.1.streamL.bitrate",type:"text",label: "Camera 1 streamL.bitrate:"},
                      {id: "camera.1.streamL.format",type:"text",label: "Camera 1 streamL.format:"},
                      {id: "camera.1.streamL.framerate",type:"text",label: "Camera 1 streamL.framerate:"},
                      {id: "camera.1.streamL.gopsize",type:"text",label: "Camera 1 streamL.gopsize:"},
                      {id: "camera.1.streamL.storage.diskquota",type:"text",label: "Camera 1 streamL.storage.diskquota:"},
                      {id: "camera.1.streamL.storage.recordevents",type:"text",label: "Camera 1 streamL.storage.recordevents:"},
                      {id: "camera.1.streamL.storage.recordmode",type:"select",options:[{id:"0",value:"Off"},{id:"1",value:"Continuous"},{id:"2",value:"Event Triggered"}],label: "Camera 1 streamL.storage.recordmode:"},
                      {id: "camera.1.streamL.storage.recordpostamble",type:"text",label: "Camera 1 streamL.storage.recordpostamble:"},
                      {id: "camera.1.streamL.storage.recordpreamble",type:"text",label: "Camera 1 streamL.storage.recordpreamble:"},
                      {id: "camera.1.wdrmode",type:"checkbox",label: "Camera 1 WDR Mode:"},
                      {id: "evtgrab.capint",type:"text",label: "Evtgrab Capint:"},
                      {id: "evtgrab.dint",type:"text",label: "Evtgrab dint:"},
                      {id: "evtgrab.nfiles",type:"text",label: "Evtgrab Nfiles:"},
                      {id: "evtgrab.periodic_only",type:"checkbox",label: "Evtgrab Periodic Only:"},
                      /*{id: "medianode.commissioned",type:"checkbox",disabled:"disabled",label: "Medianode Commissioned:"},*/
                      {id: "mediaserver.nomve",type:"checkbox",label: "Mediaserver Nomve:"},
                      {id: "mediaserver.video_codec",type:"text",label: "Mediaserver Video Codec:"},
                      {id: "mediaserver.wmm_qos",type:"checkbox",label: "Mediaserver Wmm Qos:"},
                      {id: "network.eth-x.ip",type:"text",label: "Network Eth-x IP:"},
                      /*{id: "network.eth-x.ipv6.method",type:"text",disabled:"disabled",label: "Network Eth-x IPV6 Method:"},*/
                      {id: "network.eth-x.leasetime",type:"text",label: "Network Eth-x Leasetime:"},
                      {id: "network.eth-x.method",type:"text",label: "Network Eth-x Method:"},
                      {id: "network.eth-x.netmask",type:"text",label: "Network Eth-x Netmask:"},
                      {id: "network.eth-x.port-forwarding.genetec",type:"text",label: "Network Eth-x Port Forwarding Genetec:"},
                      {id: "network.eth-x.port-forwarding.https",type:"text",label: "Network Eth-x Port-Forwarding https:"},
                      {id: "network.eth-x.port-forwarding.rtsp",type:"text",label: "Network Eth-x Port-Forwarding rtsp:"},
                      {id: "network.eth-x.port-forwarding.ssh",type:"text",label: "Network Eth-x Port Forwarding ssh:"},
                      {id: "network.eth-x.whitelist",type:"text",label: "Network Eth-x Whitelist:"},
                      {id: "network.firewall.ports",type:"text",label: "Network Firewall Ports:"},
                      {id: "network.firewall.protocols",type:"text",label: "Network Firewall Protocols:"},
                      {id: "network.region",type:"text",label: "Network Region:"},
                      {id: "network.server.mqtt",type:"text",label: "Network Server Mqtt:"},
                      {id: "network.server.vpn",type:"text",label: "Network Server VPN:"},
                      {id: "network.vpn_on_demand",type:"checkbox",label: "Network VPN On Demand:"},
                      /*{id: "network.wlan-x.ipv6.method",type:"text",disabled:"disabled",label: "Network Wlan-x Ipv6 Method:"},*/
                      {id: "network.wlan-x.security",type:"text",label: "Network Wlan-x Security:"},
                      {id: "network.wlan-x.security.bgscan-long-interval",type:"text",label: "Network Wlan-x Security Bgscan Long Interval:"},
                      {id: "network.wlan-x.security.bgscan-short-interval",type:"text",label: "Network Wlan-x Security Bgscan Short Interval:"},
                      {id: "network.wlan-x.security.bgscan-signal-threshold",type:"text",label: "Network wlan-x Security Bgscan Signal Threshold:"},
                      {id: "network.wlan-x.security.psk",type:"text",label: "Network Wlan-x Security Psk:"},
                      {id: "network.wlan-x.ssid",type:"text",label: "Network Wlan-x Ssid:"},
                      /*{id: "network.wlan-y.ipv6.method",type:"text",disabled:"disabled",label: "Network Wlan-y Ipv6 Method:"},*/
                      {id: "network.wlan-y.security",type:"text",label: "Network Wlan-y Security:"},
                      {id: "network.wlan-y.security.bgscan-long-interval",type:"text",label: "Network Wlan-y Security Bgscan Long Interval:"},
                      {id: "network.wlan-y.security.bgscan-short-interval",type:"text",label: "Network Wlan-y Security Bgscan Short Interval:"},
                      {id: "network.wlan-y.security.bgscan-signal-threshold",type:"text",label: "Network wlan-y Security Bgscan Signal Threshold:"},
                      {id: "network.wlan-y.security.psk",type:"text",label: "Network Wlan-y Security Psk:"},
                      {id: "network.wlan-y.ssid",type:"text",label: "Network Wlan-y Ssid:"},
                      {id: "rtsp.service",type:"text",label: "RTSP Service:"},
                      {id: "server",type:"text",label: "Server:"},
                      {id: "storage.maxsessions",type:"text",label: "Storage Maxsessions:"},
                      {id: "storageserver.cache_size",type:"text",label: "Storageserver Cache Size:"},
                      {id: "storageserver.max_clip_size",type:"text",label: "Storageserver Max Clip Size:"},
                    ],
         "merlin" :[  {id: "name",type:"text",label: "Name:"},
                      {id: "model",type:"text",disabled:"disabled",label: "Model:"},
                      /*{id: "Genetec.Network.Services.Http.Port",type:"text",disabled:"disabled",label: "Genetec Network Services Http Port:"},*/
                      {id: "Genetec.custom_events",type:"text",label: "Genetec Custom Events:"},
                      {id: "alarm.redis.memory_threshold",type:"text",label: "Alarm Redis Memory Threshold:"},
                      {id: "application_server",type:"text",label: "Application Server:"},
                      {id: "application_server_port",type:"text",label: "Application Server Port:"},
                      {id: "camera.0.binning",type:"checkbox",label: "Camera 0 Binning:"},
                      {id: "camera.0.enabled",type:"checkbox",label: "Camera 0 Enabled:"},
                      {id: "camera.0.exposure",type:"text",label: "Camera 0 Exposure:"},
                      {id: "camera.0.gain",type:"text",label: "Camera 0 Gain:"},
                      {id: "camera.0.master",type:"text",label: "Camera 0 Master:"},
                      {id: "camera.0.streamH.bitrate",type:"text",label: "Camera 0 StreamH Bitrate:"},
                      {id: "camera.0.streamH.format",type:"text",label: "Camera 0 StreamH Format:"},
                      {id: "camera.0.streamH.framerate",type:"text",label: "Camera 0 StreamH framerate:"},
                      {id: "camera.0.streamH.gopsize",type:"text",label: "Camera 0 StreamH gopsize:"},
                      {id: "camera.0.streamH.storage.diskquota",type:"text",label: "Camera 0 StreamH Storage Diskquota:"},
                      {id: "camera.0.streamH.storage.recordevents",type:"text",label: "Camera 0 StreamH Storage Recordevents:"},
                      {id: "camera.0.streamH.storage.recordmode",type:"select",options:[{id:"0",value:"Off"},{id:"1",value:"Continuous"},{id:"2",value:"Event Triggered"}],label: "Camera 0 StreamH Storage Recordmode:"},
                      {id: "camera.0.streamH.storage.recordpostamble",type:"text",label: "Camera 0 StreamH Storage Recordpostamble:"},
                      {id: "camera.0.streamH.storage.recordpreamble",type:"text",label: "Camera 0 StreamH.storage.recordpreamble:"},
                      {id: "camera.0.streamH.storage.vaqa.0.duration",type:"text",label: "Camera 0 streamH.storage.vaqa.0.duration:"},
                      {id: "camera.0.streamH.storage.vaqa.0.interval",type:"text",label: "Camera 0 streamH.storage.vaqa.0.interval:"},
                      {id: "camera.0.streamH.storage.vaqa.0.qty",type:"text",label: "Camera 0 streamH.storage.vaqa.0.qty:"},
                      {id: "camera.0.streamH.storage.vaqa.1.duration",type:"text",label: "Camera 0 streamH.storage.vaqa.1.duration:"},
                      {id: "camera.0.streamH.storage.vaqa.1.interval",type:"text",label: "Camera 0 streamH.storage.vaqa.1.interval:"},
                      {id: "camera.0.streamH.storage.vaqa.1.qty",type:"text",label: "Camera 0 streamH.storage.vaqa.1.qty:"},
                      /*{id: "camera.0.streamH.storage.vaqa.status",type:"text",disabled:"disabled",label: "Camera 0 streamH.storage.vaqa.status:"},*/
                      {id: "camera.0.streamL.bitrate",type:"text",label: "Camera 0 streamL.bitrate:"},
                      {id: "camera.0.streamL.format",type:"text",label: "Camera 0 streamL.format:"},
                      {id: "camera.0.streamL.framerate",type:"text",label: "Camera 0 streamL.framerate:"},
                      {id: "camera.0.streamL.gopsize",type:"text",label: "Camera 0 streamL.gopsize:"},
                      {id: "camera.0.streamL.storage.diskquota",type:"text",label: "Camera 0 streamL.storage.diskquota:"},
                      {id: "camera.0.streamL.storage.recordevents",type:"text",label: "Camera 0 streamL.storage.recordevents:"},
                      {id: "camera.0.streamL.storage.recordmode",type:"select",options:[{id:"0",value:"Off"},{id:"1",value:"Continuous"},{id:"2",value:"Event Triggered"}],label: "Camera 0 streamL.storage.recordmode:"},
                      {id: "camera.0.streamL.storage.recordpostamble",type:"text",label: "Camera 0 streamL.storage.recordpostamble:"},
                      {id: "camera.0.streamL.storage.recordpreamble",type:"text",label: "Camera 0 streamL.storage.recordpreamble:"},
                      {id: "camera.1.enabled",type:"checkbox",label: "Camera 1 Enabled:"},
                      {id: "camera.1.streamH.bitrate",type:"text",label: "Camera 1 StreamH Bitrate:"},
                      {id: "camera.1.streamH.format",type:"text",label: "Camera 1 StreamH Format:"},
                      {id: "camera.1.streamH.framerate",type:"text",label: "Camera 1 StreamH framerate:"},
                      {id: "camera.1.streamH.gopsize",type:"text",label: "Camera 1 StreamH gopsize:"},
                      {id: "camera.1.streamH.storage.recordevents",type:"text",label: "Camera 1 StreamH Storage Recordevents:"},
                      {id: "camera.1.streamH.storage.recordpostamble",type:"text",label: "Camera 1 StreamH Storage Recordpostamble:"},
                      {id: "camera.1.streamH.storage.recordpreamble",type:"text",label: "Camera 1 StreamH.storage.recordpreamble:"},
                      {id: "camera.1.streamH.storage.vaqa.0.duration",type:"text",label: "Camera 1 streamH.storage.vaqa.0.duration:"},
                      {id: "camera.1.streamH.storage.vaqa.0.interval",type:"text",label: "Camera 1 streamH.storage.vaqa.0.interval:"},
                      {id: "camera.1.streamH.storage.vaqa.0.qty",type:"text",label: "Camera 1 streamH.storage.vaqa.0.qty:"},
                      {id: "camera.1.streamH.storage.vaqa.1.duration",type:"text",label: "Camera 1 streamH.storage.vaqa.1.duration:"},
                      {id: "camera.1.streamH.storage.vaqa.1.interval",type:"text",label: "Camera 1 streamH.storage.vaqa.1.interval:"},
                      {id: "camera.1.streamH.storage.vaqa.1.qty",type:"text",label: "Camera 1 streamH.storage.vaqa.1.qty:"},
                      /*{id: "camera.1.streamH.storage.vaqa.status",type:"text",disabled:"disabled",label: "Camera 1 streamH.storage.vaqa.1.status:"},*/
                      {id: "camera.1.streamL.bitrate",type:"text",label: "Camera 1 streamL.bitrate:"},
                      {id: "camera.1.streamL.format",type:"text",label: "Camera 1 streamL.format:"},
                      {id: "camera.1.streamL.framerate",type:"text",label: "Camera 1 streamL.framerate:"},
                      {id: "camera.1.streamL.gopsize",type:"text",label: "Camera 1 streamL.gopsize:"},
                      {id: "camera.1.streamL.storage.recordevents",type:"text",label: "Camera 1 streamL.storage.recordevents:"},
                      {id: "camera.1.streamL.storage.recordpostamble",type:"text",label: "Camera 1 streamL.storage.recordpostamble:"},
                      {id: "camera.1.streamL.storage.recordpreamble",type:"text",label: "Camera 1 streamL.storage.recordpreamble:"},
                      {id: "evtgrab.capint",type:"text",label: "Evtgrab Capint:"},
                      {id: "evtgrab.dint",type:"text",label: "Evtgrab dint:"},
                      {id: "evtgrab.nfiles",type:"text",label: "Evtgrab Nfiles:"},
                      {id: "evtgrab.periodic_only",type:"checkbox",label: "Evtgrab Periodic Only:"},
                      /*{id: "medianode.commissioned",type:"checkbox",disabled:"disabled",label: "Medianode Commissioned:"},*/
                      {id: "mediaserver.nomve",type:"checkbox",label: "Mediaserver Nomve:"},
                      {id: "mediaserver.rtspsrc",type:"text",label: "Mediaserver RTSP Src:"},
                      {id: "mediaserver.video_codec",type:"text",label: "Mediaserver Video Codec:"},
                      {id: "mediaserver.wmm_qos",type:"checkbox",label: "Mediaserver Wmm Qos:"},
                      {id: "network.eth-x.ip",type:"text",label: "Network Eth-x IP:"},
                      /*{id: "network.eth-x.ipv6.method",type:"text",disabled:"disabled",label: "Network Eth-x IPV6 Method:"},*/
                      {id: "network.eth-x.method",type:"text",label: "Network Eth-x Method:"},
                      {id: "network.eth-x.netmask",type:"text",label: "Network Eth-x Netmask:"},
                      {id: "network.firewall.ports",type:"text",label: "Network Firewall Ports:"},
                      {id: "network.firewall.protocols",type:"text",label: "Network Firewall Protocols:"},
                      {id: "network.server.mqtt",type:"text",label: "Network Server Mqtt:"},
                      {id: "network.server.vpn",type:"text",label: "Network Server VPN:"},
                      {id: "network.vpn_on_demand",type:"checkbox",label: "Network VPN On Demand:"},
                      {id: "rtsp.service",type:"text",label: "RTSP Service:"},
                      {id: "server",type:"text",label: "Server:"},
                      {id: "storage.maxsessions",type:"text",label: "Storage Maxsessions:"},
                      {id: "storageserver.cache_size",type:"text",label: "Storageserver Cache Size:"},
                      {id: "storageserver.max_clip_size",type:"text",label: "Storageserver Max Clip Size:"},
                    ],

         "vdkmaster":[{id: "name",type:"text",label: "Name:"},
                      {id: "model",type:"text",disabled:"disabled",label: "Model:"},
                      {id: "siteid",type:"text",label: "Site ID:"},
                      {id: "al.left.setbrightness",type:"text",label: "Al Left Set Brightness:"},
                      {id: "al.left.setmode",type:"text",label: "Al Left Set Mode:"},
                      {id: "al.right.setbrightness",type:"text",label: "Al Right Set Brightness:"},
                      {id: "al.right.setmode",type:"text",label: "Al Right Set Mode:"},
                      {id: "alarm.redis.memory_threshold",type:"text",label: "Alarm Redis Memory Threshold:"},
                      {id: "application_server",type:"text",label: "Application Server:"},
                      {id: "application_server_port",type:"text",label: "Application Server Port:"},
                      {id: "appos.timezone",type:"text",label: "Appos Timezone:"},
                      {id: "glassl.display.blue.gain",type:"text",label: "Glassl Display Blue Gain:"},
                      {id: "glassl.display.blue.offset",type:"text",label: "Glassl Display Blue Offset:"},
                      {id: "glassl.display.brightness",type:"text",label: "Glassl Display Brightness:"},
                      {id: "glassl.display.color-temp",type:"text",label: "Glassl Display Color Temp:"},
                      {id: "glassl.display.contrast",type:"text",label: "Glassl Display Contrast:"},
                      {id: "glassl.display.green.gain",type:"text",label: "Glassl Display Green Gain:"},
                      {id: "glassl.display.green.offset",type:"text",label: "Glassl Display Green Offset:"},
                      {id: "glassl.display.hue",type:"text",label: "Glassl Display Hue:"},
                      {id: "glassl.display.mode.ccs",type:"text",label: "Glassl Display Mode CCS:"},
                      {id: "glassl.display.mode.dynamic-nr",type:"text",label: "Glassl Display Mode Dyamic-NR:"},
                      {id: "glassl.display.mode.main-dcdi",type:"text",label: "Glassl Display Mode Main-DCDI:"},
                      {id: "glassl.display.mode.main-madi",type:"text",label: "Glassl Display Mode Main-MADI:"},
                      {id: "glassl.display.mode.mpeg-nr",type:"text",label: "Glassl Display Mode MPEG-NR:"},
                      {id: "glassl.display.mode.selection-1080p",type:"text",label: "Glassl Display Mode Selection 1080p:"},
                      {id: "glassl.display.mode.selection-720p",type:"text",label: "Glassl Display Mode Selection 720p:"},
                      {id: "glassl.display.mpeg-nr",type:"text",label: "Glassl Display MPEG-NR:"},
                      {id: "glassl.display.power",type:"text",label: "Glassl Display Power:"},
                      {id: "glassl.display.red.gain",type:"text",label: "Glassl Display Red Gain:"},
                      {id: "glassl.display.red.offset",type:"text",label: "Glassl Display Red Offset:"},
                      {id: "glassl.display.saturation",type:"text",label: "Glassl Display Saturation:"},
                      {id: "glassl.display.sharpness",type:"text",label: "Glassl Display Sharpness:"},
                      {id:"glassl.display.temp.cold",type:"text",label: "Glassl Display Temp Cold:"},
                      {id: "glassl.display.temp.hot",type:"text",label: "Glassl Display Temp Hot:"},
                      {id: "glassl.display.temp.warm",type:"text",label: "Glassl Display Temp Warm:"},
                      {id: "glassr.display.blue.gain",type:"text",label: "Glassr Display Blue Gain:"},
                      {id: "glassr.display.blue.offset",type:"text",label: "Glassr Display Blue Offset:"},
                      {id: "glassr.display.brightness",type:"text",label: "Glassr Display Brightness:"},
                      {id: "glassr.display.color-temp",type:"text",label: "Glassr Display Color Temp:"},
                      {id: "glassr.display.contrast",type:"text",label: "Glassr Display Contrast:"},
                      {id: "glassr.display.green.gain",type:"text",label: "Glassr Display Green Gain:"},
                      {id: "glassr.display.green.offset",type:"text",label: "Glassr Display Green Offset:"},
                      {id:"glassr.display.hue",type:"text",label: "Glassr Display Hue:"},
                      {id: "glassr.display.mode.ccs",type:"text",label: "Glassr Display Mode CCS:"},
                      {id: "glassr.display.mode.dynamic-nr",type:"text",label: "GlassR Display Mode Dynamic NR:"},
                      {id: "glassr.display.mode.main-dcdi",type:"text",label: "GlassR Display Mode Main DCDI:"},
                      {id: "glassr.display.mode.main-madi",type:"text",label: "GlassR Display Mode Main MADI:"},
                      {id: "glassr.display.mode.mpeg-nr",type:"text",label: "GlassR Display Mode MPEG NR:"},
                      {id: "glassr.display.mode.selection-1080p",type:"text",label: "GlassR Display Mode Selection 1080p:"},
                      {id: "glassr.display.mode.selection-720p",type:"text",label: "GlassR Display Mode Selection 720p:"},
                      {id: "glassr.display.mpeg-nr",type:"text",label: "GlassR Display MPEG-NR:"},
                      {id: "glassr.display.power",type:"text",label: "GlassR Display Power:"},
                      {id: "glassr.display.red.gain",type:"text",label: "GlassR Display Red Gain:"},
                      {id: "glassr.display.red.offset",type:"text",label: "GlassR Display Red Offset:"},
                      {id: "glassr.display.saturation",type:"text",label: "GlassR Display Saturation:"},
                      {id: "glassr.display.sharpness",type:"text",label: "GlassR Display Sharpness:"},
                      {id: "glassr.display.temp.cold",type:"text",label: "GlassR Display Temp Cold:"},
                      {id: "glassr.display.temp.hot",type:"text",label: "GlassR Display Temp Hot:"},
                      {id: "glassr.display.temp.warm",type:"text",label: "GlassR Display Temp Warm:"},
                      {id: "lr.setcolor",type:"text",label: "LR Set Color:"},
                      /*{id: "medianode.commissioned",type:"checkbox",disabled:"disabled",label: "Medianode Commissioned:"},*/
                      {id: "network.eth-x.ip",type:"text",label: "Network Eth-x IP:"},
                      /*{id: "network.eth-x.ipv6.method",type:"text",disabled:"disabled",label: "Network Eth-x IPV6 Method:"},*/
                      {id: "network.eth-x.method",type:"text",label: "Network Eth-x Method:"},
                      {id: "network.eth-x.netmask",type:"text",label: "Network Eth-x Netmask:"},
                      {id: "network.eth-x.port-forwarding.genetec",type:"text",label: "Network Eth-x Port Forwarding Genetec:"},
                      {id: "network.eth-x.port-forwarding.https",type:"text",label: "Network Eth-x Port-Forwarding https:"},
                      {id: "network.eth-x.port-forwarding.rtsp",type:"text",label: "Network Eth-x Port-Forwarding rtsp:"},
                      {id: "network.eth-x.port-forwarding.ssh",type:"text",label: "Network Eth-x Port Forwarding ssh:"},
                      {id: "network.eth-x.whitelist",type:"text",label: "Network Eth-x Whitelist:"},
                      {id: "network.firewall.ports",type:"text",label: "Network Firewall Ports:"},
                      {id: "network.firewall.protocols",type:"text",label: "Network Firewall Protocols:"},
                      {id: "network.server.mqtt",type:"text",label: "Network Server Mqtt:"},
                      {id: "network.server.vpn",type:"text",label: "Network Server VPN:"},
                      {id: "network.vpn_on_demand",type:"checkbox",label: "Network VPN On Demand:"},
                      {id: "pebble.display.blue.gain",type:"text",label: "Pebble Display Blue Gain:"},
                      {id: "pebble.display.blue.offset",type:"text",label: "Pebble Display Blue Offset:"},
                      {id: "pebble.display.brightness",type:"text",label: "Pebble Display Brightness:"},
                      {id: "pebble.display.color-temp",type:"text",label: "Pebble Display Color Temp:"},
                      {id: "pebble.display.contrast",type:"text",label: "Pebble Display Contrast:"},
                      {id: "pebble.display.green.gain",type:"text",label: "Pebble Display Green Gain:"},
                      {id: "pebble.display.green.offset",type:"text",label: "Pebble Display Green Offset:"},
                      {id: "pebble.display.hue",type:"text",label: "Pebble Display Hue:"},
                      {id: "pebble.display.mode.ccs",type:"text",label: "Pebble Display Mode CCS:"},
                      {id: "pebble.display.mode.dynamic-nr",type:"text",label: "Pebble Display Mode Dynamic NR:"},
                      {id: "pebble.display.mode.main-dcdi",type:"text",label: "Pebble Display Mode Main DCDI:"},
                      {id: "pebble.display.mode.main-madi",type:"text",label: "Pebble Display Mode Main MADI:"},
                      {id: "pebble.display.mode.mpeg-nr",type:"text",label: "Pebble Display Mode MPEG NR:"},
                      {id: "pebble.display.mode.selection-1080p",type:"text",label: "Pebble Display Mode Selection 1080p:"},
                      {id: "pebble.display.mode.selection-720p",type:"text",label: "Pebble Display Mode Selection 720p:"},
                      {id: "pebble.display.mpeg-nr",type:"text",label: "Pebble Display MPEG NR:"},
                      {id: "pebble.display.power",type:"text",label: "Pebble Display Power:"},
                      {id: "pebble.display.red.gain",type:"text",label: "Pebble Display Red Gain:"},
                      {id: "pebble.display.red.offset",type:"text",label: "Pebble Display Red Offset:"},
                      {id: "pebble.display.saturation",type:"text",label: "Pebble Display Saturation:"},
                      {id: "pebble.display.sharpness",type:"text",label: "Pebble Display Sharpness:"},
                      {id: "pebble.display.temp.cold",type:"text",label: "Pebble Display Temp Cold:"},
                      {id: "pebble.display.temp.hot",type:"text",label: "Pebble Display Temp Hot:"},
                      {id: "pebble.display.temp.warm",type:"text",label: "Pebble Display Temp Warm:"},
                      {id: "server",type:"text",label: "Server:"},
                    ],

            "cnext"  :[ {id: "name",type:"text",label: "Name:"},
                      {id: "model",type:"text",disabled:"disabled",label: "Model:"},
                      {id: "alarm.redis.memory_threshold",type:"text",label: "Alarm Redis Memory Threshold:"},
                      {id: "application_server",type:"text",label: "Application Server:"},
                      {id: "application_server_port",type:"text",label: "Application Server Port:"},
                      {id: "lctrl.dimmer.vmax",type:"text",label: "Lctrl Dimmer Vmax:"},
                      {id: "lctrl.ltype",type:"text",label: "Lctrl Ltype:"},
                      {id: "lctrl.lvlrate.fast",type:"text",label: "Lctrl Lvlrate Fast:"},
                      {id: "lctrl.lvlrate.light",type:"text",label: "Lctrl Lvlrate Light:"},
                      {id: "lctrl.lvlrate.motion",type:"text",label: "Lctrl Lvlrate Motion:"},
                      {id: "lctrl.lvlrate.norm",type:"text",label: "Lctrl Lvlrate Norm:"},
                      {id: "lctrl.trigger.nonett",type:"text",label: "Lctrl Trigger Nonett:"},
                      {id: "lctrl.trigger.off.dint",type:"text",label: "Lctrl Trigger Off Dint:"},
                      {id: "lctrl.trigger.offt",type:"text",label: "Lctrl Trigger Offt:"},
                      {id: "lctrl.trigger.on.dint",type:"text",label: "Lctrl Trigger On Dint:"},
                      {id: "lctrl.trigger.ont",type:"text",label: "Lctrl Trigger Ont:"},
                      /*{id: "medianode.commissioned",type:"checkbox",disabled:"disabled",label: "Medianode Commissioned:"},*/
                      {id: "network.eth-x.ip",type:"text",label: "Network Eth-x IP:"},
                      /*{id: "network.eth-x.ipv6.method",type:"text",disabled:"disabled",label: "Network Eth-x IPV6 Method:"},*/
                      {id: "network.eth-x.leasetime",type:"text",label: "Network Eth-x Leasetime:"},
                      {id: "network.eth-x.method",type:"text",label: "Network Eth-x Method:"},
                      {id: "network.eth-x.netmask",type:"text",label: "Network Eth-x Netmask:"},
                      /*{id: "network.eth-x.port-forwarding.genetec",type:"text",label: "Network Eth-x Port Forwarding Genetec:"},
                      {id: "network.eth-x.port-forwarding.https",type:"text",label: "Network Eth-x Port-Forwarding https:"},
                      {id: "network.eth-x.port-forwarding.rtsp",type:"text",label: "Network Eth-x Port-Forwarding rtsp:"},
                      {id: "network.eth-x.port-forwarding.ssh",type:"text",label: "Network Eth-x Port Forwarding ssh:"},
                      {id: "network.eth-x.whitelist",type:"text",label: "Network Eth-x Whitelist:"}, */
                      {id: "network.firewall.ports",type:"text",label: "Network Firewall Ports:"},
                      {id: "network.firewall.protocols",type:"text",label: "Network Firewall Protocols:"},
                      {id: "network.ppp-x.apn",type:"text",label: "Network Region:"},
                      {id: "network.server.mqtt",type:"text",label: "Network Server Mqtt:"},
                      {id: "network.server.vpn",type:"text",label: "Network Server VPN:"},
                      {id: "network.vpn_on_demand",type:"checkbox",label: "Network VPN On Demand:"},
                      /*{id: "network.wlan-x.ipv6.method",type:"text",disabled:"disabled",label: "Network Wlan-x Ipv6 Method:"},*/
                      {id: "network.wlan-x.security",type:"text",label: "Network Wlan-x Security:"},
                      {id: "network.wlan-x.security.bgscan-long-interval",type:"text",label: "Network Wlan-x Security Bgscan Long Interval:"},
                      {id: "network.wlan-x.security.bgscan-short-interval",type:"text",label: "Network Wlan-x Security Bgscan Short Interval:"},
                      {id: "network.wlan-x.security.bgscan-signal-threshold",type:"text",label: "Network wlan-x Security Bgscan Signal Threshold:"},
                      {id: "network.wlan-x.security.psk",type:"text",label: "Network Wlan-x Security Psk:"},
                      {id: "network.wlan-x.ssid",type:"text",label: "Network Wlan-x Ssid:"},
                      /*{id: "network.wlan-y.ipv6.method",type:"text",disabled:"disabled",label: "Network Wlan-y Ipv6 Method:"},*/
                      {id: "network.wlan-y.security",type:"text",label: "Network Wlan-y Security:"},
                      {id: "network.wlan-y.security.bgscan-long-interval",type:"text",label: "Network Wlan-y Security Bgscan Long Interval:"},
                      {id: "network.wlan-y.security.bgscan-short-interval",type:"text",label: "Network Wlan-y Security Bgscan Short Interval:"},
                      {id: "network.wlan-y.security.bgscan-signal-threshold",type:"text",label: "Network wlan-y Security Bgscan Signal Threshold:"},
                      {id: "network.wlan-y.security.psk",type:"text",label: "Network Wlan-y Security Psk:"},
                      {id: "network.wlan-y.ssid",type:"text",label: "Network Wlan-y Ssid:"},
                      {id: "podbus.disable",type:"checkbox",label: "Podbus Disable:"},
                      {id: "poe.enabled",type:"checkbox",label: "Poe Enabled:"},
                      {id: "pwr.noload.i",type:"text",label: "Pwr Noload i:"},
                      {id: "pwr.noload.mode",type:"text",label: "Pwr Noload Mode:"},
                      {id: "server",type:"text",label: "Server:"},
                    ],
    };

    
    
    return (
      <div>
          <div className="netsense__form__body">
              {/* <span style={previousNextButtons}>
                 <span style={noPreviousItem} className="rubix-icon icon-simple-line-icons-arrow-left config-previous" onClick={ () => this.callPreviousItem(this.props.allConfigs,this.state.config)}></span>
              </span>

              <span style={previousNextButtons}>
                  <span style={noNextItem} className="rubix-icon icon-simple-line-icons-arrow-right config-next" onClick={ () => this.callNextItem(this.props.allConfigs,this.state.config)}></span>
              </span> */}


              <form role="form" className="form-horizontal" data-configid={config.configid} >
                  <div style={hstyle}>
                        {modelData[selectedModel].map(function(obj, index) {
                            var objType = obj.type
                            var objId = obj.id;
                            var objLabel = obj.label;
                            var objDisabled = obj.disabled;
                            
                            if(objType == "select"){
                                var objOptions = obj.options;

                                var defaultvalue;
                                var options = objOptions.map(function (option, i) {
                                  if (option.selected === true || option.selected === 'selected') {
                                    if (this.props.multiple) {
                                      if (defaultvalue === undefined) {
                                        defaultvalue = [];
                                      }
                                      defaultvalue.push(option.value);
                                    } else {
                                      defaultvalue = option.value;
                                    }
                                  }
                                  return <option key={i} value={option.id}>{option.value}</option>;
                                },that);


                                return (
                                    <div className="form-group">
                                      <label htmlFor={objId} className="control-label col-sm-3"> {objLabel} </label>
                                      <div className="col-sm-6">
                                        <select className="form-control" id={objId} ref={objId} value={that.state[objId]} multiple={that.props.multiple} defaultvalue={defaultvalue} onChange={that.handleChange(objId)} >
                                            {options}
                                        </select>
                                      </div>
                                    </div>
                                );
                                
                            }
                            else if (objType == "checkbox"){
                                if(objDisabled == "disabled"){
                                    return (
                                        <div className="form-group">
                                          <label htmlFor={objId} className="control-label col-sm-3"> {objLabel} </label>
                                          <div className="col-sm-1">
                                              <input type={objType} disabled={objDisabled} className="form-control" id={objId} ref={objId} checked={that.state[objId]} onChange={that.handleChange(objId)} />
                                          </div>
                                        </div>
                                    ); 
                                }
                                else{
                                    return (
                                        <div className="form-group">
                                          <label htmlFor={objId} className="control-label col-sm-3"> {objLabel} </label>
                                          <div className="col-sm-1">
                                              <input type={objType} className="form-control" id={objId} ref={objId} checked={that.state[objId]} onChange={that.handleChange(objId)} />
                                          </div>
                                        </div>
                                    ); 
                                }
                                
                            }
                            else{
                                if(objDisabled == "disabled"){
                                    return (
                                        <div className="form-group">
                                          <label htmlFor={objId} className="control-label col-sm-3"> {objLabel} </label>
                                          <div className="col-sm-6">
                                              <input type={objType} disabled={objDisabled} className="form-control" id={objId} ref={objId} value={that.state[objId]} onChange={that.handleChange(objId)} />
                                          </div>
                                        </div>
                                    );
                                }
                                else{
                                    return (
                                        <div className="form-group">
                                          <label htmlFor={objId} className="control-label col-sm-3"> {objLabel} </label>
                                          <div className="col-sm-6">
                                              <input type={objType} className={(that.props.errors[objId])? "form-control orange":"form-control"} id={objId} ref={objId} value={that.state[objId]} onChange={that.handleChange(objId)} />
                                              <div className="form-error">
                                                {that.props.errors[objId] || ""}
                                              </div>
                                          </div>
                                        </div>
                                    );
                                }
                            }

                          })}

                    

                                           { /* <div className="form-group">
                          <label htmlFor="name" className="control-label col-sm-3"> Name:</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="name" ref="name" value={this.state.name} onChange={this.handleChange('name')}/>
                          </div>
                      </div>

                      <div className="form-group">
                          <label htmlFor="model" className="control-label col-sm-3"> Model:</label>
                          <div className="col-sm-6">
                              <input type="text" className="form-control" id="model" ref="model" value={this.state.model} disabled="disabled" onChange={this.handleChange('model')}/>
                          </div>
                      </div>

                      <div className="form-group" style={wifiVisibility}>
                          <label htmlFor="networkXPasskey" className="control-label col-sm-3"> Network X Passkey:</label>
                          <div className="col-sm-6">
                              <input type="password" disabled={isDisabled} style={{border:"0 none",borderBottom:"2px solid #ccc"}} className="form-control" id="networkXPasskey" ref="networkXPasskey" value={this.state.networkXPasskey} onChange={this.handleChange('networkXPasskey')}/>
                              <div className="form-error">
                                  {this.props.errors.networkXPasskey || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group" style={wifiVisibility}>
                          <label htmlFor="networkXSSID" className="control-label col-sm-3"> Network X SSID:</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="networkXSSID" ref="networkXSSID" value={this.state.networkXSSID} onChange={this.handleChange('networkXSSID')}/>
                          </div>
                      </div>
                      <div className="form-group" style={wifiVisibility}>
                          <label htmlFor="networkXSecurity" className="control-label col-sm-3"> Network X Security:</label>
                          <div className="col-sm-6">
                              <select disabled={isDisabled} className="form-control" id="networkXSecurity" ref="networkXSecurity" value={this.state.networkXSecurity} onChange={this.handleChange('networkXSecurity')}>
                                  <option value="wpa2p">WPA2 personal</option>
                              </select>
                          </div>
                      </div>
                      <div className="form-group" style={wifiVisibility}>
                          <label htmlFor="networkYPasskey" className="control-label col-sm-3"> Network Y Passkey:</label>
                          <div className="col-sm-6">
                              <input type="password" disabled={isDisabled} className="form-control" style={{border:"0 none",borderBottom:"2px solid #ccc"}} id="networkYPasskey" ref="networkYPasskey" value={this.state.networkYPasskey} onChange={this.handleChange('networkYPasskey')}/>
                              <div className="form-error">
                                  {this.props.errors.networkYPasskey || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group" style={wifiVisibility}>
                          <label htmlFor="networkYSSID" className="control-label col-sm-3"> Network Y SSID:</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="networkYSSID" ref="networkYSSID" value={this.state.networkYSSID} onChange={this.handleChange('networkYSSID')}/>
                          </div>
                      </div>
                      <div className="form-group" style={wifiVisibility}>
                          <label htmlFor="networkYSecurity" className="control-label col-sm-3"> Network Y Security:</label>
                          <div className="col-sm-6">
                              <select disabled={isDisabled} className="form-control" id="networkYSecurity" ref="networkYSecurity" value={this.state.networkYSecurity} onChange={this.handleChange('networkYSecurity')}>
                                  <option value="wpa2p">WPA2 personal</option>
                              </select>
                          </div>
                      </div>
                      <div className="form-group">
                          <label htmlFor="network_region" className="control-label col-sm-3">Network Region:</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="network_region" ref="network_region" value={this.state.network_region} onChange={this.handleChange('network_region')} />
                          </div>
                      </div>
                      <div className="form-group">
                          <label htmlFor="debugmode" className="control-label col-sm-3">Debug Mode:</label>
                          <div className="col-sm-1">
                              <input type="checkbox" disabled={isDisabled} className="form-control" id="debugmode" ref="debugmode" style={{outline:"0 none"}} checked={this.state.debugmode} onChange={this.handleChange('debugmode')} />
                          </div>
                      </div>
                      <div className="form-group" style={wifiVisibility}>
                          <label htmlFor="telnet" className="control-label col-sm-3">Telnet:</label>
                          <div className="col-sm-1">
                              <input type="checkbox" disabled={isDisabled} className="form-control" id="telnet" ref="telnet" style={{outline:"0 none"}} checked={this.state.telnet} onChange={this.handleChange('telnet')} />
                          </div>
                      </div>
                      <div className="form-group">
                          <label htmlFor="vpn_on_demand" className="control-label col-sm-3">VPN on demand:</label>
                          <div className="col-sm-1">
                              <input type="checkbox" disabled={isDisabled} className="form-control" id="vpn_on_demand" ref="vpn_on_demand"  style={{outline:"0 none"}} checked={this.state.vpn_on_demand} onChange={this.handleChange('vpn_on_demand')} />
                          </div>
                      </div>
                      <div className="form-group" style={auxPowerVisibility}>
                          <label htmlFor="aux_power" className="control-label col-sm-3">AUX Power:</label>
                          <div className="col-sm-1">
                              <input type="checkbox" disabled={isDisabled} className="form-control" id="aux_power" ref="aux_power" style={{outline:"0 none"}} checked={this.state.aux_power} onChange={this.handleChange('aux_power')} />
                          </div>
                      </div>
                      <div className="form-group" style={podbusDisableVisibility}>
                          <label htmlFor="podbus_disable" className="control-label col-sm-3">Podbus Disable:</label>
                          <div className="col-sm-1">
                              <input type="checkbox" disabled={isDisabled} className="form-control" id="podbus_disable" ref="podbus_disable" style={{outline:"0 none"}} checked={this.state.podbus_disable} onChange={this.handleChange('podbus_disable')} />
                          </div>
                      </div>
                      <div className="form-group">
                          <label htmlFor="sensor_rf_pint" className="control-label col-sm-3">Received Signal Strength Poll Interval (sec):</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_rf_pint" ref="sensor_rf_pint" value={this.state.sensor_rf_pint} onChange={this.handleChange('sensor_rf_pint')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_rf_pint || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group">
                          <label htmlFor="sensor_rf_dint" className="control-label col-sm-3">Received Signal Strength Debounce Interval (sec):</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_rf_dint" ref="sensor_rf_dint" value={this.state.sensor_rf_dint} onChange={this.handleChange('sensor_rf_dint')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_rf_dint || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group">
                          <label htmlFor="sensor_rf_mode" className="control-label col-sm-3">Received Signal Strength Mode:</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_rf_mode" ref="sensor_rf_mode" value={this.state.sensor_rf_mode} onChange={this.handleChange('sensor_rf_mode')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_rf_mode || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group">
                          <label htmlFor="sensor_v_pint" className="control-label col-sm-3">Voltage Poll Interval (sec):</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_v_pint" ref="sensor_v_pint" value={this.state.sensor_v_pint} onChange={this.handleChange('sensor_v_pint')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_v_pint || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group">
                          <label htmlFor="sensor_v_dint" className="control-label col-sm-3">Voltage Debounce Interval (sec):</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_v_dint" ref="sensor_v_dint" value={this.state.sensor_v_dint} onChange={this.handleChange('sensor_v_dint')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_v_dint || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group">
                          <label htmlFor="sensor_v_mode" className="control-label col-sm-3">Voltage Mode:</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_v_mode" ref="sensor_v_mode" value={this.state.sensor_v_mode} onChange={this.handleChange('sensor_v_mode')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_v_mode || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group" style={notValid}>
                          <label htmlFor="sensor_aw_pint" className="control-label col-sm-3">Auxiliary Energy Use Poll Interval (sec):</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_aw_pint" ref="sensor_aw_pint" value={this.state.sensor_aw_pint} onChange={this.handleChange('sensor_aw_pint')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_aw_pint || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group" style={notValid}>
                          <label htmlFor="sensor_aw_dint" className="control-label col-sm-3">Auxiliary Energy Use Debounce Interval (sec):</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_aw_dint" ref="sensor_aw_dint" value={this.state.sensor_aw_dint} onChange={this.handleChange('sensor_aw_dint')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_aw_dint || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group" style={notValid}>
                          <label htmlFor="sensor_aw_mode" className="control-label col-sm-3">Auxiliary Energy Use Mode:</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_aw_mode" ref="sensor_aw_mode" value={this.state.sensor_aw_mode} onChange={this.handleChange('sensor_aw_mode')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_aw_mode || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group" style={notValid}>
                          <label htmlFor="sensor_aPF_pint" className="control-label col-sm-3">Auxiliary Power Factor Poll Interval (sec):</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_aPF_pint" ref="sensor_aPF_pint" value={this.state.sensor_aPF_pint} onChange={this.handleChange('sensor_aPF_pint')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_aPF_pint || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group" style={notValid}>
                          <label htmlFor="sensor_aPF_dint" className="control-label col-sm-3">Auxiliary Power Factor Debounce Interval (sec):</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_aPF_dint" ref="sensor_aPF_dint" value={this.state.sensor_aPF_dint} onChange={this.handleChange('sensor_aPF_dint')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_aPF_dint || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group" style={notValid}>
                          <label htmlFor="sensor_aPF_mode" className="control-label col-sm-3">Auxiliary Power Factor Mode:</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_aPF_mode" ref="sensor_aPF_mode" value={this.state.sensor_aPF_mode} onChange={this.handleChange('sensor_aPF_mode')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_aPF_mode || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group" style={notValid}>
                          <label htmlFor="sensor_aP_pint" className="control-label col-sm-3">Auxiliary Power Poll Interval (sec):</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_aP_pint" ref="sensor_aP_pint" value={this.state.sensor_aP_pint} onChange={this.handleChange('sensor_aP_pint')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_aP_pint || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group" style={notValid}>
                          <label htmlFor="sensor_aP_dint" className="control-label col-sm-3">Auxiliary Power Debounce Interval (sec):</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_aP_dint" ref="sensor_aP_dint" value={this.state.sensor_aP_dint} onChange={this.handleChange('sensor_aP_dint')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_aP_dint || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group" style={notValid}>
                          <label htmlFor="sensor_aP_mode" className="control-label col-sm-3">Auxiliary Power Mode:</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_aP_mode" ref="sensor_aP_mode" value={this.state.sensor_aP_mode} onChange={this.handleChange('sensor_aP_mode')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_aP_mode || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group" style={notValid}>
                          <label htmlFor="sensor_mP_pint" className="control-label col-sm-3">Main Power Poll Interval (sec):</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_mP_pint" ref="sensor_mP_pint" value={this.state.sensor_mP_pint} onChange={this.handleChange('sensor_mP_pint')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_mP_pint || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group" style={notValid}>
                          <label htmlFor="sensor_mP_dint" className="control-label col-sm-3">Main Power Debounce Interval (sec):</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_mP_dint" ref="sensor_mP_dint" value={this.state.sensor_mP_dint} onChange={this.handleChange('sensor_mP_dint')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_mP_dint || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group" style={notValid}>
                          <label htmlFor="sensor_mP_mode" className="control-label col-sm-3">Main Power Mode:</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_mP_mode" ref="sensor_mP_mode" value={this.state.sensor_mP_mode} onChange={this.handleChange('sensor_mP_mode')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_mP_mode || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group" style={notValid}>
                          <label htmlFor="sensor_mip_pint" className="control-label col-sm-3">Main Current Spike Poll Interval (sec):</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_mip_pint" ref="sensor_mip_pint" value={this.state.sensor_mip_pint} onChange={this.handleChange('sensor_mip_pint')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_mip_pint || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group" style={notValid}>
                          <label htmlFor="sensor_mip_dint" className="control-label col-sm-3">Main Current Spike Debounce Interval (sec):</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_mip_dint" ref="sensor_mip_dint" value={this.state.sensor_mip_dint} onChange={this.handleChange('sensor_mip_dint')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_mip_dint || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group" style={notValid}>
                          <label htmlFor="sensor_mip_mode" className="control-label col-sm-3">Main Current Spike Mode:</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_mip_mode" ref="sensor_mip_mode" value={this.state.sensor_mip_mode} onChange={this.handleChange('sensor_mip_mode')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_mip_mode || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group" style={notValid}>
                          <label htmlFor="sensor_ai_pint" className="control-label col-sm-3">Auxiliary Current Poll Interval (sec):</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_ai_pint" ref="sensor_ai_pint" value={this.state.sensor_ai_pint} onChange={this.handleChange('sensor_ai_pint')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_ai_pint || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group" style={notValid}>
                          <label htmlFor="sensor_ai_dint" className="control-label col-sm-3">Auxiliary Current Debounce Interval (sec):</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_ai_dint" ref="sensor_ai_dint" value={this.state.sensor_ai_dint} onChange={this.handleChange('sensor_ai_dint')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_ai_dint || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group" style={notValid}>
                          <label htmlFor="sensor_ai_mode" className="control-label col-sm-3">Auxiliary Current Mode:</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_ai_mode" ref="sensor_ai_mode" value={this.state.sensor_ai_mode} onChange={this.handleChange('sensor_ai_mode')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_ai_mode || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group" style={notValid}>
                          <label htmlFor="sensor_aip_pint" className="control-label col-sm-3">Auxiliary Current Spike Poll Interval (sec):</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_aip_pint" ref="sensor_aip_pint" value={this.state.sensor_aip_pint} onChange={this.handleChange('sensor_aip_pint')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_aip_pint || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group" style={notValid}>
                          <label htmlFor="sensor_aip_dint" className="control-label col-sm-3">Auxiliary Current Spike Debounce Interval (sec):</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_aip_dint" ref="sensor_aip_dint" value={this.state.sensor_aip_dint} onChange={this.handleChange('sensor_aip_dint')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_aip_dint || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group" style={notValid}>
                          <label htmlFor="sensor_aip_mode" className="control-label col-sm-3">Auxiliary Current Spike Mode:</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_aip_mode" ref="sensor_aip_mode" value={this.state.sensor_aip_mode} onChange={this.handleChange('sensor_aip_mode')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_aip_mode || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group" style={notValid}>
                          <label htmlFor="sensor_mi_pint" className="control-label col-sm-3">Main Current Poll Interval (sec):</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_mi_pint" ref="sensor_mi_pint" value={this.state.sensor_mi_pint} onChange={this.handleChange('sensor_mi_pint')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_mi_pint || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group" style={notValid}>
                          <label htmlFor="sensor_mi_dint" className="control-label col-sm-3">Main Current Debounce Interval (sec):</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_mi_dint" ref="sensor_mi_dint" value={this.state.sensor_mi_dint} onChange={this.handleChange('sensor_mi_dint')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_mi_dint || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group" style={notValid}>
                          <label htmlFor="sensor_mi_mode" className="control-label col-sm-3">Main Current Mode:</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_mi_mode" ref="sensor_mi_mode" value={this.state.sensor_mi_mode} onChange={this.handleChange('sensor_mi_mode')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_mi_mode || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group" style={notValid}>
                          <label htmlFor="sensor_mw_pint" className="control-label col-sm-3">Main Energy Use Poll Interval (sec):</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_mw_pint" ref="sensor_mw_pint" value={this.state.sensor_mw_pint} onChange={this.handleChange('sensor_mw_pint')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_mw_pint || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group" style={notValid}>
                          <label htmlFor="sensor_mw_dint" className="control-label col-sm-3">Main Energy Use Debounce Interval (sec):</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_mw_dint" ref="sensor_mw_dint" value={this.state.sensor_mw_dint} onChange={this.handleChange('sensor_mw_dint')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_mw_dint || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group" style={notValid}>
                          <label htmlFor="sensor_mw_mode" className="control-label col-sm-3">Main Energy Use Mode:</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_mw_mode" ref="sensor_mw_mode" value={this.state.sensor_mw_mode} onChange={this.handleChange('sensor_mw_mode')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_mw_mode || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group" style={notValid}>
                          <label htmlFor="sensor_mPF_pint" className="control-label col-sm-3">Main Power Factor Poll Interval (sec):</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_mPF_pint" ref="sensor_mPF_pint" value={this.state.sensor_mPF_pint} onChange={this.handleChange('sensor_mPF_pint')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_mPF_pint || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group" style={notValid}>
                          <label htmlFor="sensor_mPF_dint" className="control-label col-sm-3">Main Power Factor Debounce Interval (sec):</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_mPF_dint" ref="sensor_mPF_dint" value={this.state.sensor_mPF_dint} onChange={this.handleChange('sensor_mPF_dint')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_mPF_dint || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group" style={notValid}>
                          <label htmlFor="sensor_mPF_mode" className="control-label col-sm-3">Main Power Factor Mode:</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_mPF_mode" ref="sensor_mPF_mode" value={this.state.sensor_mPF_mode} onChange={this.handleChange('sensor_mPF_mode')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_mPF_mode || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group">
                          <label htmlFor="sensor_lIR_pint" className="control-label col-sm-3">Infrared Sensor Poll Interval (sec):</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_lIR_pint" ref="sensor_lIR_pint" value={this.state.sensor_lIR_pint} onChange={this.handleChange('sensor_lIR_pint')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_lIR_pint || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group">
                          <label htmlFor="sensor_lIR_dint" className="control-label col-sm-3">Infrared Sensor Debounce Interval (sec):</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_lIR_dint" ref="sensor_lIR_dint" value={this.state.sensor_lIR_dint} onChange={this.handleChange('sensor_lIR_dint')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_lIR_dint || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group">
                          <label htmlFor="sensor_lIR_mode" className="control-label col-sm-3">Infrared Sensor Mode:</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_lIR_mode" ref="sensor_lIR_mode" value={this.state.sensor_lIR_mode} onChange={this.handleChange('sensor_lIR_mode')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_lIR_mode || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group">
                          <label htmlFor="sensor_l_pint" className="control-label col-sm-3">Light Level Poll Interval (sec):</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_l_pint" ref="sensor_l_pint" value={this.state.sensor_l_pint} onChange={this.handleChange('sensor_l_pint')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_l_pint || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group">
                          <label htmlFor="sensor_l_dint" className="control-label col-sm-3">Light Level Debounce Interval (sec):</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_l_dint" ref="sensor_l_dint" value={this.state.sensor_l_dint} onChange={this.handleChange('sensor_l_dint')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_l_dint || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group">
                          <label htmlFor="sensor_l_mode" className="control-label col-sm-3">Light Level Mode:</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_l_mode" ref="sensor_l_mode" value={this.state.sensor_l_mode} onChange={this.handleChange('sensor_l_mode')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_l_mode || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group" style={cellularVisibility}>
                          <label htmlFor="sensor_l_i_pint" className="control-label col-sm-3">Internal Light Level Poll Interval (sec):</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_l_i_pint" ref="sensor_l_i_pint" value={this.state.sensor_l_i_pint} onChange={this.handleChange('sensor_l_i_pint')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_l_i_pint || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group" style={cellularVisibility}>
                          <label htmlFor="sensor_l_i_dint" className="control-label col-sm-3">Internal Light Level Debounce Interval (sec):</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_l_i_dint" ref="sensor_l_i_dint" value={this.state.sensor_l_i_dint} onChange={this.handleChange('sensor_l_i_dint')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_l_i_dint || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group" style={cellularVisibility}>
                          <label htmlFor="sensor_l_i_mode" className="control-label col-sm-3">Internal Light Level Mode:</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_l_i_mode" ref="sensor_l_i_mode" value={this.state.sensor_l_i_mode} onChange={this.handleChange('sensor_l_i_mode')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_l_i_mode || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group" style={cellularVisibility}>
                          <label htmlFor="sensor_lIR_i_pint" className="control-label col-sm-3">Internal Infrared Sensor Poll Interval (sec):</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_lIR_i_pint" ref="sensor_lIR_i_pint" value={this.state.sensor_lIR_i_pint} onChange={this.handleChange('sensor_lIR_i_pint')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_lIR_i_pint || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group" style={cellularVisibility}>
                          <label htmlFor="sensor_lIR_i_dint" className="control-label col-sm-3">Internal Infrared Sensor Debounce Interval (sec):</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_lIR_i_dint" ref="sensor_lIR_i_dint" value={this.state.sensor_lIR_i_dint} onChange={this.handleChange('sensor_lIR_i_dint')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_lIR_i_dint || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group" style={cellularVisibility}>
                          <label htmlFor="sensor_lIR_i_mode" className="control-label col-sm-3">Internal Infrared Sensor Mode:</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_lIR_i_mode" ref="sensor_lIR_i_mode" value={this.state.sensor_lIR_i_mode} onChange={this.handleChange('sensor_lIR_i_mode')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_lIR_i_mode || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group">
                          <label htmlFor="sensor_p_pint" className="control-label col-sm-3">Presence (Motion Detector) Poll Interval (sec):</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_p_pint" ref="sensor_p_pint" value={this.state.sensor_p_pint} onChange={this.handleChange('sensor_p_pint')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_p_pint || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group">
                          <label htmlFor="sensor_p_dint" className="control-label col-sm-3">Presence (Motion Detector) Debounce Interval (sec):</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_p_dint" ref="sensor_p_dint" value={this.state.sensor_p_dint} onChange={this.handleChange('sensor_p_dint')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_p_dint || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group">
                          <label htmlFor="sensor_p_mode" className="control-label col-sm-3">Presence (Motion Detector) Mode:</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_p_mode" ref="sensor_p_mode" value={this.state.sensor_p_mode} onChange={this.handleChange('sensor_p_mode')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_p_mode || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group" style={notValid}>
                          <label htmlFor="sensor_pc_pint" className="control-label col-sm-3">Presence Count Poll Interval (sec):</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_pc_pint" ref="sensor_pc_pint" value={this.state.sensor_pc_pint} onChange={this.handleChange('sensor_pc_pint')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_pc_pint || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group" style={notValid}>
                          <label htmlFor="sensor_pc_dint" className="control-label col-sm-3">Presence Count Debounce Interval (sec):</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_pc_dint" ref="sensor_pc_dint" value={this.state.sensor_pc_dint} onChange={this.handleChange('sensor_pc_dint')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_pc_dint || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group" style={notValid}>
                          <label htmlFor="sensor_pc_mode" className="control-label col-sm-3">Presence Count Mode:</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_pc_mode" ref="sensor_pc_mode" value={this.state.sensor_pc_mode} onChange={this.handleChange('sensor_pc_mode')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_pc_mode || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group">
                          <label htmlFor="sensor_t_pint" className="control-label col-sm-3">Ambient Temperature Poll Interval (sec):</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_t_pint" ref="sensor_t_pint" value={this.state.sensor_t_pint} onChange={this.handleChange('sensor_t_pint')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_t_pint || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group">
                          <label htmlFor="sensor_t_dint" className="control-label col-sm-3">Ambient Temperature Debounce Interval (sec):</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_t_dint" ref="sensor_t_dint" value={this.state.sensor_t_dint} onChange={this.handleChange('sensor_t_dint')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_t_dint || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group">
                          <label htmlFor="sensor_t_mode" className="control-label col-sm-3">Ambient Temperature Mode:</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_t_mode" ref="sensor_t_mode" value={this.state.sensor_t_mode} onChange={this.handleChange('sensor_t_mode')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_t_mode || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group">
                          <label htmlFor="sensor_T_pint" className="control-label col-sm-3">Node Temperature Poll Interval (sec):</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_T_pint" ref="sensor_T_pint" value={this.state.sensor_T_pint} onChange={this.handleChange('sensor_T_pint')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_T_pint || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group">
                          <label htmlFor="sensor_T_dint" className="control-label col-sm-3">Node Temperature Debounce Interval (sec):</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_T_dint" ref="sensor_T_dint" value={this.state.sensor_T_dint} onChange={this.handleChange('sensor_T_dint')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_T_dint || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group">
                          <label htmlFor="sensor_T_mode" className="control-label col-sm-3">Node Temperature Mode:</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_T_mode" ref="sensor_T_mode" value={this.state.sensor_T_mode} onChange={this.handleChange('sensor_T_mode')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_T_mode || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group" style={notValid}>
                          <label htmlFor="sensor_mt_pint" className="control-label col-sm-3">MCU Temperature Poll Interval (sec):</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_mt_pint" ref="sensor_mt_pint" value={this.state.sensor_mt_pint} onChange={this.handleChange('sensor_mt_pint')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_mt_pint || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group" style={notValid}>
                          <label htmlFor="sensor_mt_dint" className="control-label col-sm-3">MCU Temperature Debounce Interval (sec):</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_mt_dint" ref="sensor_mt_dint" value={this.state.sensor_mt_dint} onChange={this.handleChange('sensor_mt_dint')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_mt_dint || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group" style={notValid}>
                          <label htmlFor="sensor_mt_mode" className="control-label col-sm-3">MCU Temperature Mode:</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_mt_mode" ref="sensor_mt_mode" value={this.state.sensor_mt_mode} onChange={this.handleChange('sensor_mt_mode')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_mt_mode || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group" style={zmotionsensors}>
                          <label htmlFor="sensor_pdc_pint" className="control-label col-sm-3">Zmotion PIR DC Value Poll Interval (sec):</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_pdc_pint" ref="sensor_pdc_pint" value={this.state.sensor_pdc_pint} onChange={this.handleChange('sensor_pdc_pint')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_pdc_pint || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group" style={zmotionsensors}>
                          <label htmlFor="sensor_pdc_dint" className="control-label col-sm-3">Zmotion PIR DC Value Debounce Interval (sec):</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_pdc_dint" ref="sensor_pdc_dint" value={this.state.sensor_pdc_dint} onChange={this.handleChange('sensor_pdc_dint')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_pdc_dint || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group" style={zmotionsensors}>
                          <label htmlFor="sensor_pdc_mode" className="control-label col-sm-3">Zmotion PIR DC Value Mode:</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_pdc_mode" ref="sensor_pdc_mode" value={this.state.sensor_pdc_mode} onChange={this.handleChange('sensor_pdc_mode')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_pdc_mode || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group" style={zmotionsensors}>
                          <label htmlFor="sensor_ppr_pint" className="control-label col-sm-3">Zmotion PIR Process Rate Poll Interval (sec):</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_ppr_pint" ref="sensor_ppr_pint" value={this.state.sensor_ppr_pint} onChange={this.handleChange('sensor_ppr_pint')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_ppr_pint || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group" style={zmotionsensors}>
                          <label htmlFor="sensor_ppr_dint" className="control-label col-sm-3">Zmotion PIR Process Rate Debounce Interval (sec):</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_ppr_dint" ref="sensor_ppr_dint" value={this.state.sensor_ppr_dint} onChange={this.handleChange('sensor_ppr_dint')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_ppr_dint || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group" style={zmotionsensors}>
                          <label htmlFor="sensor_ppr_mode" className="control-label col-sm-3">Zmotion PIR Process Rate Mode:</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_ppr_mode" ref="sensor_ppr_mode" value={this.state.sensor_ppr_mode} onChange={this.handleChange('sensor_ppr_mode')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_ppr_mode || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group" style={zmotionsensors}>
                          <label htmlFor="sensor_pnd_pint" className="control-label col-sm-3">Zmotion EM Noise Detected Poll Interval (sec):</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_pnd_pint" ref="sensor_pnd_pint" value={this.state.sensor_pnd_pint} onChange={this.handleChange('sensor_pnd_pint')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_pnd_pint || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group" style={zmotionsensors}>
                          <label htmlFor="sensor_pnd_dint" className="control-label col-sm-3">Zmotion EM Noise Detected Debounce Interval (sec):</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_pnd_dint" ref="sensor_pnd_dint" value={this.state.sensor_pnd_dint} onChange={this.handleChange('sensor_pnd_dint')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_pnd_dint || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group" style={zmotionsensors}>
                          <label htmlFor="sensor_pnd_mode" className="control-label col-sm-3">Zmotion EM Noise Detected Mode:</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_pnd_mode" ref="sensor_pnd_mode" value={this.state.sensor_pnd_mode} onChange={this.handleChange('sensor_pnd_mode')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_pnd_mode || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group" style={zmotionsensors}>
                          <label htmlFor="sensor_pdt_pint" className="control-label col-sm-3">Zmotion EM Transient Detected Poll Interval (sec):</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_pdt_pint" ref="sensor_pdt_pint" value={this.state.sensor_pdt_pint} onChange={this.handleChange('sensor_pdt_pint')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_pdt_pint || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group" style={zmotionsensors}>
                          <label htmlFor="sensor_pdt_dint" className="control-label col-sm-3">Zmotion EM Transient Detected Debounce Interval (sec):</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_pdt_dint" ref="sensor_pdt_dint" value={this.state.sensor_pdt_dint} onChange={this.handleChange('sensor_pdt_dint')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_pdt_dint || ""}
                              </div>
                          </div>
                      </div>

                      <div className="form-group" style={zmotionsensors}>
                          <label htmlFor="sensor_pdt_mode" className="control-label col-sm-3">Zmotion EM Transient Detected Mode:</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_pdt_mode" ref="sensor_pdt_mode" value={this.state.sensor_pdt_mode} onChange={this.handleChange('sensor_pdt_mode')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_pdt_mode || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group" style={podbussensors}>
                          <label htmlFor="sensor_podm_pint" className="control-label col-sm-3">Total Message And Error Count Poll Interval (sec):</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_podm_pint" ref="sensor_podm_pint" value={this.state.sensor_podm_pint} onChange={this.handleChange('sensor_podm_pint')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_podm_pint || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group" style={podbussensors}>
                          <label htmlFor="sensor_podm_dint" className="control-label col-sm-3">Total Message And Error Count Debounce Interval (sec):</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_podm_dint" ref="sensor_podm_dint" value={this.state.sensor_podm_dint} onChange={this.handleChange('sensor_podm_dint')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_podm_dint || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group" style={podbussensors}>
                          <label htmlFor="sensor_podm_mode" className="control-label col-sm-3">Total Message And Error Count Mode:</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_podm_mode" ref="sensor_podm_mode" value={this.state.sensor_podm_mode} onChange={this.handleChange('sensor_podm_mode')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_podm_mode || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group" style={podbussensors}>
                          <label htmlFor="sensor_podme_pint" className="control-label col-sm-3">Response Error Count Poll Interval (sec):</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_podme_pint" ref="sensor_podme_pint" value={this.state.sensor_podme_pint} onChange={this.handleChange('sensor_podme_pint')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_podme_pint || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group" style={podbussensors}>
                          <label htmlFor="sensor_podme_dint" className="control-label col-sm-3">Response Error Count Debounce Interval (sec):</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_podme_dint" ref="sensor_podme_dint" value={this.state.sensor_podme_dint} onChange={this.handleChange('sensor_podme_dint')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_podme_dint || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group" style={podbussensors}>
                          <label htmlFor="sensor_podme_mode" className="control-label col-sm-3">Response Error Count Mode:</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_podme_mode" ref="sensor_podme_mode" value={this.state.sensor_podme_mode} onChange={this.handleChange('sensor_podme_mode')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_podme_mode || ""}
                              </div>
                          </div>
                      </div>
                      <div className="form-group" style={mainRelayCyclesSensorPollVisibility}>
                          <label htmlFor="sensor_rlym_pint" className="control-label col-sm-3">Main Relay Cycle Poll Interval (sec):</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="sensor_rlym_pint" ref="sensor_rlym_pint" value={this.state.sensor_rlym_pint} onChange={this.handleChange('sensor_rlym_pint')} />
                              <div className="form-error">
                                  {this.props.errors.sensor_rlym_pint || ""}
                              </div>
                          </div>
                      </div>

                      <div className="form-group">
                          <label htmlFor="lctrl_dimmer_vmax" className="control-label col-sm-3">Maximum Dimming Control Output Voltage (millivolts):</label>
                          <div className="col-sm-6">
                              <select disabled={isDisabled} className="form-control" id="lctrl_dimmer_vmax" ref="lctrl_dimmer_vmax" value={this.state.lctrl_dimmer_vmax} onChange={this.handleChange('lctrl_dimmer_vmax')}>
                                  <option value="10000">0-10V LED driver</option>
                                  <option value="8000">0-8V LED driver</option>
                              </select>
                          </div>
                      </div>

                      <div className="form-group" style={notValid}>
                          <label htmlFor="lctrl_trigger_off_dint" className="control-label col-sm-3">Lctrl Trigger Off Dint (sec):</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="lctrl_trigger_off_dint" ref="lctrl_trigger_off_dint" value={this.state.lctrl_trigger_off_dint} onChange={this.handleChange('lctrl_trigger_off_dint')} />
                              <div className="form-error">
                                  {this.props.errors.lctrl_trigger_off_dint || ""}
                              </div>
                          </div>
                      </div>

                      <div className="form-group" style={notValid}>
                          <label htmlFor="lctrl_trigger_on_dint" className="control-label col-sm-3">Lctrl Trigger On Dint (sec):</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="lctrl_trigger_on_dint" ref="lctrl_trigger_on_dint" value={this.state.lctrl_trigger_on_dint} onChange={this.handleChange('lctrl_trigger_on_dint')} />
                              <div className="form-error">
                                  {this.props.errors.lctrl_trigger_on_dint || ""}
                              </div>
                          </div>
                      </div>

                      <div className="form-group" style={notValid}>
                          <label htmlFor="lctrl_trigger_ont" className="control-label col-sm-3">Lctrl Trigger Ont (sec):</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="lctrl_trigger_ont" ref="lctrl_trigger_ont" value={this.state.lctrl_trigger_ont} onChange={this.handleChange('lctrl_trigger_ont')} />
                              <div className="form-error">
                                  {this.props.errors.lctrl_trigger_ont || ""}
                              </div>
                          </div>
                      </div>

                      <div className="form-group" style={notValid}>
                          <label htmlFor="lctrl_trigger_offt" className="control-label col-sm-3">Lctrl Trigger Offt (sec):</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="lctrl_trigger_offt" ref="lctrl_trigger_offt" value={this.state.lctrl_trigger_offt} onChange={this.handleChange('lctrl_trigger_offt')} />
                              <div className="form-error">
                                  {this.props.errors.lctrl_trigger_offt || ""}
                              </div>
                          </div>
                      </div>

                      <div className="form-group" style={notValid}>
                          <label htmlFor="lctrl_trigger_nonett" className="control-label col-sm-3">Lctrl Trigger Nonett (sec):</label>
                          <div className="col-sm-6">
                              <input type="text" disabled={isDisabled} className="form-control" id="lctrl_trigger_nonett" ref="lctrl_trigger_nonett" value={this.state.lctrl_trigger_nonett} onChange={this.handleChange('lctrl_trigger_nonett')} />
                              <div className="form-error">
                                  {this.props.errors.lctrl_trigger_nonett || ""}
                              </div>
                          </div>
                      </div>
    */ }
                      <div style={isDisabledButtons}>
                          <div className="form-group" style={{margin:"0px 24px"}} id="assignmentWrapper">
                              <h3>Assign this configuration to a Site / Group:</h3>

                              <div className="form-group">
                                  <label htmlFor="unassigned" className="control-label col-sm-3"> Do not assign:</label>
                                  <div className="col-sm-6">
                                      <input type="radio" name="fx-assign" value="unassigned" style={{position:"relative",marginTop:"12px"}} checked={this.state.assign=="unassigned"} onChange={this.handleRadio('fx-assign')} />
                                  </div>
                              </div>
                              <div className="form-group">
                                  <label htmlFor="sitewide" className="control-label col-sm-3"> Assign to a Site :</label>
                                  <div className="col-sm-6">
                                      <input type="radio" name="fx-assign" value="sitewide" style={{position:"relative",marginTop:"12px"}} checked={this.state.assign=="sitewide"} onChange={this.handleRadio('fx-assign')} />
                                  </div>
                              </div>
                              <div className="form-group">
                                  <label htmlFor="groups" className="control-label col-sm-3"> Assign to a Group :</label>
                                  <div className="col-sm-6">
                                      <input type="radio" name="fx-assign" value="groups" style={{verticalAlign:"top",position:"relative",marginTop:"12px"}} checked={this.state.assign=="groups"} onChange={this.handleRadio('fx-assign')} />
                                      <select style={{marginLeft:"32px",position:"relative",marginTop:"15px"}} name="assigngroups" ref="assigngroups" value={this.state.assigngroups} onChange={this.handleChange('assigngroups')} >
                                          {
                                              this.props.groups.map(function(group, index) {
                                                  return <option key={index} value={group.groupid}>{group.name}</option>;
                                              })
                                          }
                                      </select>
                                  </div>
                              </div>
                          </div>
                      </div>

                  </div>
                  <div style={isDisabledButtons}>
                      <div style={{margin:"20px",padding:"20px"}}>
                          <div className="col-sm-3" style={deleteduplicateButtonVisibility}>
                              <button type="button" className="ns-delete-btn" onClick={this.handleDelete}>
                                  <b>Delete</b></button>
                          </div>
                          <div className="col-sm-9 text-right">
                              <button type="button" className="ns-delete-btn" onClick={this.handleDuplicate} style={deleteduplicateButtonVisibility}>
                                  <b>Duplicate</b></button>
                              &nbsp; &nbsp;
                              <button id="saveConfig"  type="button" className="ns-save-btn" onClick={this.handleSubmit}>
                                  <b>Save</b></button>
                          </div>
                      </div>
                  </div>
              </form>
          </div>
      </div>
      );
    }
    else{
        return null;
    }

  }
});

  module.exports = Configform;
