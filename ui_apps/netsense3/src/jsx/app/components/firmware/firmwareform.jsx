import classNames from 'classnames';
import auth from 'global/utils/auth';
import helpers from 'global/utils/helpers';
import { State, Navigation } from 'react-router';
import { Link, withRouter } from 'react-router';

var Firmwareform = React.createClass({

  getInitialState: function(){
    return this.props.firmware
  },

  getDefaultProps: function() {
    return {
      errors: {}
    };
  },

  propTypes: {
    firmware: React.PropTypes.object.isRequired,
    errors: React.PropTypes.object,
    groups: React.PropTypes.array.isRequired,
    otas: React.PropTypes.array.isRequired,
    allFirmwares : React.PropTypes.array.isRequired
  },

  handleChange: function (key) {
    return function (e) {
      var state = {};
      switch (key) {
        case 'assigngroups':
          state.assigngroups = $.makeArray($(e.target).val());
          break;
        case 'released':
        case 'deprecated':
          state[key] = e.target.checked;
          break;
        default:
          state[key] = e.target.value;
      };
      this.setState(state);
    }.bind(this);
  },

  isValid: function() {
    this.props.errors = {};
    var rules = {
        commit: {
            required: true
        },
    };
    this.props.errors = helpers.checkValidity(this.state, rules);
    return (Object.keys(this.props.errors).length == 0);
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
          state.assigngroups = [];
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
 
  handleSubmit: function(e) {

   
    //if (this.isValid()) {
      //this.props.errors={};

      var firmware = Object.assign({}, this.state);
      firmware.image_size = parseInt(firmware.image_size);

      if(firmware.assign == "sitewide"){
        if (confirm("Are you sure you want to assign this firmware to the site?")) {
          ReactBootstrap.Dispatcher.emit("Firmwareform.save", firmware);
        }
      }
      else if(firmware.assign == "groups"){
        if (confirm("Are you sure you want to assign this firmware to the selected group?")) {
          ReactBootstrap.Dispatcher.emit("Firmwareform.save", firmware);
        }
      }
      else{
        if (confirm("No changes will be made to this firmware?")) {
          return false;
          //ReactBootstrap.Dispatcher.emit("Firmwareform.save", firmware);
        }
      }
      
   /* }
    else {
      this.forceUpdate();
    }*/
    //return false;

    e.stopPropagation();
    e.preventDefault();


  },

  handleReset: function(e) {
    e.stopPropagation();
    e.preventDefault();
    ReactBootstrap.Dispatcher.emit("Firmwareform.reset", Object.assign({},this.state));
  },

  handleDelete: function(e) {
    e.stopPropagation();
    e.preventDefault();
    if (confirm("Are you sure you want to Delete this Firmware version?")) {
      ReactBootstrap.Dispatcher.emit("Firmwareform.delete", Object.assign({},this.state));
    };
  },

  handleViewJob: function(e){
    e.stopPropagation();
    e.preventDefault();
    this.props.router.push("/app/firmwareupdatepanel");
  },

  callPreviousItem: function (allItems, currentItem) {
    $('.firmware-previous').keyup();
    $("#Firmware-grid").data("gridInstance");
    console.log($("#Firmware-grid").data("gridInstance"));
    var currentRow = $("#Firmware-grid").data("gridInstance").getSelectedRows();
    var nextRow = currentRow[0] - 1
    $("#Firmware-grid").data("gridInstance").setSelectedRows([nextRow])
  },

  callNextItem: function (allItems, currentItem) {
    $('.firmware-next').keydown();
    $("#Firmware-grid").data("gridInstance");
    console.log($("#Firmware-grid").data("gridInstance"));
    var currentRow = $("#Firmware-grid").data("gridInstance").getSelectedRows();
    var nextRow = currentRow[0] + 1;
    $("#Firmware-grid").data("gridInstance").setSelectedRows([nextRow])
  },

  componentWillReceiveProps: function(nextProps){
//    if (this.props.firmware.firmwareid != nextProps.firmware.firmwareid){
      this.setState(nextProps.firmware);
//    };
  },

  render: function() {
    var hstyle = {overflowY:"auto",overflowX:"hidden",marginBottom:"16px",
                  maxHeight:helpers.calcHeight(100, -400)+"px !important"};
    var firmware = this.state;
    console.log("Firmware Form", JSON.stringify(this.state));
    var heading = (firmware.name=="")?"Add Firmware":(<span><Icon glyph="icon-fontello-right-dir"/> {firmware.name}</span>);
    var allItemsLength = this.props.allFirmwares.length;
    var firstItem = 0;
    var lastItem = allItemsLength - 1;
      for( var a=0; a < allItemsLength; a++){
          if(this.props.allFirmwares[a].firmwareid === this.props.firmware.firmwareid){
              var noNextItem = lastItem === a ? {display:"none"}:{};
              var noPreviousItem = firstItem === a ? {display:"none"}:{};
          }
      }
    var previousNextButtons = this.props.firmware.firmwareid === "" || this.props.firmware.firmwareid === "default" ? {display:"none"}:{};
    var count = 0;
    for(var i=0;i<this.props.otas.length;i++){
      if(this.props.otas[i].firmwareid === firmware.firmwareid){
        count ++;
      }
    }

    var viewBtnStyle = ((firmware.firmwareid=="")||(count==0))?{display:"none"}:{};
 
    var deleteBtnStyle = (firmware.firmwareid=="")?{display:"none"}:{};
    var firmwareidform = (firmware.firmwareid != "")?(
      <div className="form-group">
        <label htmlFor="firmwareid" className="control-label col-sm-3">FirmwareID:</label>
        <div className="col-sm-6" style={{lineHeight:"40px"}}>
          <span style={{fontSize:"16px",color:"#000000"}}>{firmware.firmwareid}</span>
        </div>
        <input type="hidden" name="commit" value={firmware.commit} />
        <input type="hidden" name="firmwareid" value={firmware.firmwareid} />
      </div>
      ):(
      <div>
     
      <div className="form-group">
        <label htmlFor="commit" className="control-label col-sm-3">Commit:</label>
        <div className="col-sm-6">
          <input type="text" className={(this.props.errors.commit)? "form-control orange":"form-control"} id="commit" ref="commit" value={this.state.commit} onChange={this.handleChange('commit')} />
          <div className="form-error">
              {this.props.errors.commit || ""}
          </div>
        </div>
      </div>
      </div>
      );
    return (
      <div>
        <div className="netsense__form__header">
          <h3> {heading}</h3>
        </div>
        <div className="netsense__form__body">
          <span style={previousNextButtons}>
                 <span style={noPreviousItem} className="rubix-icon icon-simple-line-icons-arrow-left firmware-previous generic-previous" onClick={ () => this.callPreviousItem(this.props.allFirmwares,this.state.firmware)}></span>
              </span>

              <span style={previousNextButtons}>
                  <span style={noNextItem} className="rubix-icon icon-simple-line-icons-arrow-right firmware-next generic-next" onClick={ () => this.callNextItem(this.props.allFirmwares,this.state.firmware)}></span>
              </span>

        {/*<div>
          <div style={{display:"inline-block",float:"right"}}>
            <button type="button" id="viewbutton" className="ns-form-btn" onClick={this.handleViewJob} style={viewBtnStyle}>
              View Job 
            </button>
          </div>
        </div> */
        }
        &nbsp; &nbsp;

        <form role="form" className="form-horizontal" data-firmwareid={firmware.firmwareid} >
<div style={hstyle}>
          <div className="form-group">
            <label htmlFor="name" className="control-label col-sm-3">Name:</label>
            <div className="col-sm-6">
              <input type="text" className="form-control" id="name" ref="name" disabled="disabled" value={this.state.name} onChange={this.handleChange('name')} />
            </div>
          </div>
          {firmwareidform}
          <div className="form-group">
            <label htmlFor="release" className="control-label col-sm-3">Release:</label>
            <div className="col-sm-6">
              <input type="text" className="form-control" id="release" ref="release" disabled="disabled" value={this.state.release} onChange={this.handleChange('release')} />
            </div>
          </div>
          <div className="form-group">
            <label htmlFor="released" className="control-label col-sm-3">Released:</label>
            <div className="col-sm-1">
              <input type="checkbox" className="form-control" style={{marginTop:"18px"}} id="released" ref="released" disabled="disabled" checked={this.state.released} onChange={this.handleChange('released')} />
            </div>
          </div>
          <div className="form-group">
            <label htmlFor="deprecated" className="control-label col-sm-3">Deprecated:</label>
            <div className="col-sm-1">
              <input type="checkbox" className="form-control" style={{marginTop:"18px"}} id="deprecated" ref="deprecated" disabled="disabled" checked={this.state.deprecated} onChange={this.handleChange('deprecated')} />
            </div>
          </div>
          <div className="form-group">
            <label htmlFor="checksum" className="control-label col-sm-3">Checksum:</label>
            <div className="col-sm-6">
              <input type="text" className="form-control" id="checksum" ref="checksum" disabled="disabled" value={this.state.checksum} onChange={this.handleChange('checksum')} />
            </div>
          </div>
          <div className="form-group">
            <label htmlFor="build_date" className="control-label col-sm-3">Build date:</label>
            <div className="col-sm-6">
              <input type="text" className="form-control" id="build_date" ref="build_date" disabled="disabled" value={this.state.build_date} onChange={this.handleChange('build_date')} />
            </div>
          </div>
          <div className="form-group">
            <label htmlFor="image_size" className="control-label col-sm-3">Image size:</label>
            <div className="col-sm-6">
              <input type="text" className="form-control" id="image_size" ref="image_size" disabled="disabled" value={this.state.image_size} onChange={this.handleChange('image_size')} />
            </div>
          </div>

          <h3> Send Firmware Update Request:</h3>
          
          { /*<div className="form-group">
            <label htmlFor="unassigned" className="control-label col-sm-3"> Unassigned :</label>
            <div className="col-sm-6">
              <input type="radio" name="fx-assign" value="unassigned" style={{position:"relative",marginTop:"14px"}} checked={this.state.assign=="unassigned"} onChange={this.handleRadio('fx-assign')} />
            </div>
          </div> */}
          <div className="form-group">
            <label htmlFor="sitewide" className="control-label col-sm-3"> Assign to a site :</label>
            <div className="col-sm-6">
              <input type="radio" name="fx-assign" value="sitewide" style={{position:"relative",marginTop:"14px"}} checked={this.state.assign=="sitewide"} onChange={this.handleRadio('fx-assign')} />
            </div>
          </div>
          <div className="form-group">
            <label htmlFor="groups" className="control-label col-sm-3"> Assign to a group :</label>
            <div className="col-sm-6">
              <input type="radio" name="fx-assign" value="groups" style={{verticalAlign:"top",float:"left",position:"relative",marginTop:"12px"}} checked={this.state.assign=="groups"} onChange={this.handleRadio('fx-assign')} />
              <select className="form-control" style={{marginLeft:"32px",position:"relative",float:"left",width:"300px",top:"5px"}} name="assigngroups" ref="assigngroups" value={this.state.assigngroups} onChange={this.handleChange('assigngroups')} >
              {
                this.props.groups.map(function(group, index) {
                  return <option key={index} value={group.groupid}>{group.name}</option>;
                })
              }
            </select>
            </div>
          </div>

          <div className="form-group">
            <label htmlFor="description" className="control-label col-sm-3"> OTA job description:</label>
            <div className="col-sm-6">
              <input type="text" className="form-control" id="description" ref="description" value={this.state.description} onChange={this.handleChange('description')} />
            </div>
          </div>

</div>
          <div style={{margin:"20px 0px"}}>
            { /*<div className="col-sm-3 text-right">
              <button type="button" className="ns-delete-btn" style={deleteBtnStyle} onClick={this.handleDelete}>
                <b>Delete</b></button>
            </div> */ }
            <div className="col-sm-6 text-right">
              { /*<button type="button" className="ns-reset-btn" onClick={this.handleReset}>
                <b>Reset</b></button>
            &nbsp; &nbsp; */}
              <button type="button" className="ns-big-btn" onClick={this.handleSubmit}>
                <b>Apply</b></button>
            </div>
          </div>
        </form>
        </div>
      </div>
      );
 
  }
});

module.exports = withRouter(Firmwareform);
