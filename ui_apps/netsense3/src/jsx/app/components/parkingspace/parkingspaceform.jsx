
import classNames from 'classnames';
import auth from 'global/utils/auth';
import helpers from 'global/utils/helpers';
import { State, Navigation } from 'react-router';
import Dropdown from 'components/dropdown';

var Parkingspaceform = React.createClass({

  getInitialState: function(){
      return this.props.parkingspace
  },

  propTypes: {
    parkingspace: React.PropTypes.object.isRequired,
    defaultMetadataObject: React.PropTypes.object.isRequired,
    errors: React.PropTypes.object
  },

  getDefaultProps: function() {
    return {
      errors: {},
      defaultMetadata:{
          typeOfVehicle:["unrestricted"],
          handicap:false,
          reservation:false,
          businessUse:"general",
          howMetered:"not-metered",
          active:true,
          // PPV:false,
          areaType:["mixed"],
          paystationid:"not-metered"

      }
    };
  },

    typeOfVehicle: function (state, key) {
        var found = state.metadata.typeOfVehicle.indexOf("unrestricted");
        while (found !== -1) {
            state.metadata.typeOfVehicle.splice(found, 1);
            found = state.metadata.typeOfVehicle.indexOf("unrestricted");
        }
        state.metadata.typeOfVehicle.push(key);
    },

    uncheck: function(state, key){
        var found = state.metadata.typeOfVehicle.indexOf(key);
        console.log(found);
        if(found !== -1 ){
            state.metadata.typeOfVehicle.splice(found, 1);
        }
    },

    handleChange: function (key) {
        return function (e) {
            var state = this.state;
            switch (key) {
                case 'bicycle':
                    state[key] = e.target.checked;
                    if(e.target.checked){
                        this.typeOfVehicle(state, key);
                    }else{
                        this.uncheck(state,key);
                    }
                    break;
                case 'car':
                    state[key] = e.target.checked;
                    if(e.target.checked){
                        this.typeOfVehicle(state, key);
                    }else{
                        this.uncheck(state,key);
                    }
                    break;
                case 'motorcycle':
                    state[key] = e.target.checked;
                    if(e.target.checked){
                        this.typeOfVehicle(state, key);
                    }else{
                        this.uncheck(state,key);
                    }
                    break;
                case 'truck':
                    state[key] = e.target.checked;
                    if(e.target.checked){
                        this.typeOfVehicle(state, key);
                    }else{
                        this.uncheck(state,key);
                    }
                    break;
                case 'unrestricted':
                    state[key] = e.target.checked;
                    if(e.target.checked){
                        state.metadata.typeOfVehicle = [];
                        state.metadata.typeOfVehicle.push(key);
                    }
                    break;
                case "businessUse":
                    state.metadata.businessUse =  e.value;
                    break;
                case "areaType":
                    state.metadata.areaType[0] =  e.value;
                    break;
                case "parkingSpaceType":
                    state.metadata.parkingSpaceType =  e.value;
                    break;
                case "howMetered":
                    state.metadata.howMetered =  e.value;
                    break;

                default:' ';
            };

            console.log("state before setting to new one", state);
            this.setState({state: state});
        }.bind(this);
    },

    handleSpaceNameChange: function (key) {
         return function (e) {
                var state = this.state;
                switch (key) {
                    case 'name':
                    state.metadata[key] = e.target.value;
                    state[key] = e.target.value;
                    break;
                default:;
             };
             this.setState({ state });
         }.bind(this);
    },


    updateName:function(keyCode){
        if(keyCode == 13){
            var nameEdited = $("#parkingSpotName"). val().trim();

            var state = this.state;
            state.name = nameEdited;
            state.metadata.name = nameEdited;

            if (this.isValid()) {
                this.props.errors={};
                $("#parkingSpotName").blur();
                $(".invalidAlert").css("display","none");
                ReactBootstrap.Dispatcher.emit("Parkingspaceform.save", Object.assign({}, state));
            }else {
                $(".invalidAlert").css("display","block");
            }   // Escape case - not to save the new name
        }

    },

    handleRadio: function (key) {
        return function (e) {
            var state = this.state;
            switch (e.target.value) {
                case "reservation-false":
                    state.metadata.reservation = false;
                    break;
                case "reservation-true":
                    state.metadata.reservation = true;
                    break;
                case "handicap-true":
                    state.metadata.handicap = true;
                    break;
                case "handicap-false":
                    state.metadata.handicap = false;
                    break;
                // case "PPV-true":
                //     state.metadata.PPV = true;
                //     break;
                // case "PPV-false":
                //     state.metadata.PPV = false;
                //     break;
            };
            this.setState({state});
        }.bind(this);
    },

    isValid: function() {
        this.props.errors = {};
        var rules = {
            name: {
                required: true
            }
        };
        this.props.errors = helpers.checkValidity(this.state, rules);
        return (Object.keys(this.props.errors).length == 0);
    },

  handleSubmit: function(e) {
    e.stopPropagation();
    e.preventDefault();

	  var state = this.state;
	  var nameEdited = $("#parkingSpotName").val().trim();
	  state.name = nameEdited;
	  state.metadata.name = nameEdited;
	  if (this.isValid()) {
      this.props.errors={};
        $(".invalidAlert").css("display","none");
      ReactBootstrap.Dispatcher.emit("Parkingspaceform.save", Object.assign({}, state));
    }else {
        $(".invalidAlert").css("display","block");
    }
      return false;
  },

    handleCancel:function(){
        if ($("#parkingspace-list-panel").data("state") == "open") {
            $("#parkingspace-list-panel").data("state","closed").css({width:"33%", zIndex: "300"});
            $(window).trigger('resize');
            ReactBootstrap.Dispatcher.emit("Parkingspaceform.toggleDetail");
            $(".invalidAlert").css("display","none");
        } else {
            ReactBootstrap.Dispatcher.emit("Parkingspaceform.toggleDetail");
            $(".invalidAlert").css("display","none");
        }
    },

  handleReset: function(e) {
    e.stopPropagation();
    e.preventDefault();
    ReactBootstrap.Dispatcher.emit("Parkingspaceform.reset", Object.assign({},this.state));
  },

    checkValues:function(parkingspace, defaultMetadataObject){
        var state = parkingspace
        state.oldname = parkingspace.name;
        if(state.metadata.businessUse == ""){
            state.metadata.businessUse = defaultMetadataObject.businessUse
        }
        if(state.metadata.howMetered == ""){
            state.metadata.howMetered = defaultMetadataObject.howMetered
        }
        if(state.metadata.areaType == ""){
            state.metadata.areaType = defaultMetadataObject.areaType
        }
        if(state.metadata.parkingSpaceType == ""){
            state.metadata.parkingSpaceType = defaultMetadataObject.parkingSpaceType
        }
        if(state.metadata.typeOfVehicle == undefined){
            state.metadata.typeOfVehicle = defaultMetadataObject.typeOfVehicle
        }
        // if(state.metadata.PPV == undefined){
        //     state.metadata.PPV = defaultMetadataObject.PPV
        // }
        if(state.metadata.handicap == undefined){
            state.metadata.handicap = defaultMetadataObject.handicap
        }
        if(state.metadata.reservation == undefined){
            state.metadata.reservation = defaultMetadataObject.reservation
        }
        if(state.metadata.paystationid == undefined){
            state.metadata.paystationid = defaultMetadataObject.paystationid
        }

        return state;

    },

    componentDidMount:function(){
        if(this.props.parkingspace.metadata == undefined){
            this.setState({metadataPresent: false});
        }else{
            var state = this.checkValues(this.props.parkingspace,this.props.defaultMetadataObject);
            state.metadataPresent = true
            this.setState({state});
        }

    },

  componentWillReceiveProps: function(nextProps){
//    if (this.props.parkingzone.parkingzoneid != nextProps.parkingzone.parkingzoneid){
      this.setState(nextProps.parkingspace);
	  nextProps.parkingspace.oldname = nextProps.parkingspace.name;
//    };
  },

  render: function() {

      var parkingspace = this.state;
      console.log(parkingspace.parkinggroupname);
    var hstyle = {overflowY:"auto",overflowX:"hidden",marginLeft:"8px",maxHeight:helpers.calcHeight(100, -180)+"px !important"};


    var businessUseOptions = [{"loading":"loading"}, {"general":"general"}, {"emergency":"emergency"}];
    var parkingSpaceTypeOptions = [{"curb-side":"curb-side"}, {"surface-lot":"surface-lot"}, {"multi-level":"multi-level"}];
    var areaTypeOptions = [{"industrial":"industrial"},{"mixed":"mixed"}, {"school":"school"}, {"commercial":"commercial"},{"residential":"residential"}, {"cbd":"cbd"}];
    var meterTypeOptions = [{"individually-metered":"individually-metered"}, {"paystation-metered":"paystation-metered"},{"not-metered":"not-metered"}, {"cloud-app":"cloud-app"}];
    
    var heading = (
        <span>
             <input type="text" id="parkingSpotName" data-spotid={parkingspace.metadata.parkingspaceid} ref="parkingspacename" onKeyDown={(e)=>{this.updateName(e.keyCode)}}  onChange={this.handleSpaceNameChange('name')}
                    value={parkingspace.name} />
            <p className="parking-space-type">{parkingspace.metadata.active ? "Active" :"Inactive"}</p>
        </span>);
    return (
      <div>
        <h2 className="parking-space-detail-header" >{heading}
            <div className="invalidAlert" style={{display:'none', position:"relative", float:"none !important"}}>
                <div className="form-error" style={{margin:"0px",padding:"0px"}}>
                    Name is required
                </div>
            </div>
        </h2>


        <form role="form"className="form-horizontal " data-parkingspaceid={parkingspace.metadata.parkingspaceid} >
          <div style={hstyle}>
              <div>
                  {/*Set 1*/}
                  <div>
                      <div className="col-sm-12 parking-space-detail">
                          <div className="col-sm-6">
                              <label className="parking-space-label">Space ID</label>
                              <span className="parking-space-value"> {parkingspace.metadata.parkingspaceid}</span>
                          </div>
                          <div className="col-sm-6" style={{paddingLeft:"23px !important"}}>
                              <label className="parking-space-label">Group</label>
                              <span className="parking-space-value"> {parkingspace.parkinggroupname}</span>
                          </div>

                      </div>
                  </div>

                  {/*Set 2*/}
                  <div className="col-sm-12 parking-space-detail">
                      <div>
                          <label className="parking-space-label">Vehicle type(s)</label>
                              <span className="parking-space-value">
                                  <ul className="parking-space-checkbox-list">
                                  <li><input type="checkbox" name="Bicycle" value="bicycle" checked={parkingspace.metadata.typeOfVehicle.includes('bicycle')} onChange={this.handleChange('bicycle')} />
                                        <span>Bicycle</span></li>
                                        <li><input type="checkbox" style={{top:"2px"}} name="Car" value="car" checked={parkingspace.metadata.typeOfVehicle.includes('car')} onChange={this.handleChange('car')}/>
                                        <span>Car</span>
                                        </li>
                                        <li><input type="checkbox" style={{top:"2px"}} name="Motorcycle" value="motorcycle" checked={parkingspace.metadata.typeOfVehicle.includes('motorcycle') } onChange={this.handleChange('motorcycle')}/>
                                        <span>Motorcycle</span>
                                        </li>
                                        <li><input type="checkbox" style={{top:"2px"}} name="Truck" value="truck" checked={parkingspace.metadata.typeOfVehicle.includes('truck')} onChange={this.handleChange('truck')}/>
                                        <span>Truck</span>
                                        </li>
                                        {/*</ul>*/}
                                        <ul className="parking-space-checkbox-list one">
                                        <li><input type="checkbox" style={{top:"2px"}} name="Unrestricted" value="unrestricted" checked={parkingspace.metadata.typeOfVehicle.includes('unrestricted')} onChange={this.handleChange('unrestricted')}/>
                                        <span>Unrestricted</span>
                                        </li>
                                        </ul>
                                    </ul>
                          </span>

                      </div>

                                <div className="parking-space-sets">
                                    <div className="col-sm-6">
                                        <label className="parking-space-label">Reservable</label>
                                        <ul className="parking-space-checkbox-list">
                                            <li>
                                                <input type="radio" style={{width:"20px"}} name="reservablespace" value="reservation-true" checked={parkingspace.metadata.reservation == true} onChange={this.handleRadio('reservation-true')} /> Yes
                                            </li>
                                            <li>
                                                <input type="radio" style={{width:"20px"}} name="reservablespace" value="reservation-false" checked={parkingspace.metadata.reservation == false} onChange={this.handleRadio('reservation-false')}/> No
                                            </li>
                                        </ul>

                                    </div>
                                    <div className="col-sm-6"  style={{paddingLeft:"23px !important"}}>
                                        <label className="parking-space-label">Handicap</label>
                                        <ul className="parking-space-checkbox-list">
                                            <li>
                                                <input type="radio" name="handicapparking" value="handicap-true" checked={parkingspace.metadata.handicap == true} onChange={this.handleRadio('handicap-true') }/> Yes
                                            </li>
                                            <li>
                                                <input type="radio" name="handicapparking" value="handicap-false"  checked={parkingspace.metadata.handicap == false} onChange={this.handleRadio('handicap-false')}/> No
                                            </li>
                                        </ul>

                                    </div>
                                </div>

                                <div  className="parking-space-sets">
                                    {/*<div className="col-sm-6">*/}
                                        {/*<label className="parking-space-label">Enforce poorly parked violation</label>*/}
                                        {/*<ul className="parking-space-checkbox-list">*/}
                                            {/*<li>*/}
                                                {/*<input type="radio" name="ppa" value="PPV-true" checked={parkingspace.metadata.PPV == true} onChange={this.handleRadio('PPV-true')}/> Yes*/}
                                            {/*</li>*/}
                                            {/*<li>*/}
                                                {/*<input type="radio" name="ppa" value="PPV-false" checked={parkingspace.metadata.PPV == false} onChange={this.handleRadio('PPV-false')}/> No*/}
                                            {/*</li>*/}
                                        {/*</ul>*/}

                                    {/*</div>*/}
                                    <div className="col-sm-6">
                                        <label className="parking-space-label">Sensor ID</label>
                                        <span className="parking-space-value"> {parkingspace.nodeid}</span>
                                    </div>
                                </div>

                                <div className="col-sm-12" style={{padding:"20px 0px"}}>
                                    <label className="parking-space-label">Business use</label>
                                    <Dropdown options={businessUseOptions}
                                              className="form-control"
                                              id="businessuse"
                                              value={parkingspace.metadata.businessUse}
                                              onChange={this.handleChange('businessUse')}/>
                                </div>

                                <div className="col-sm-12"  style={{padding:"20px 0px"}}>
                                    <label className="parking-space-label">Location Type</label>
                                    <Dropdown options={areaTypeOptions}
                                              className="form-control"
                                              id="locationType"
                                              value={parkingspace.metadata.areaType}
                                              onChange={this.handleChange('areaType')}/>
                                </div>

                                <div className="col-sm-12"  style={{padding:"20px 0px"}}>
                                      <label className="parking-space-label">Parking Space Type</label>
                                      <Dropdown options={parkingSpaceTypeOptions}
                                                className="form-control"
                                                id="parkingSpaceType"
                                                value={parkingspace.metadata.parkingSpaceType}
                                                onChange={this.handleChange('parkingSpaceType')}/>
                                </div>
                            </div>

                            {/* Set 3*/}
                            <div className="col-sm-12 parking-space-detail">
                                {/*<div className="parking-space-sets">*/}
                                <div className="col-sm-12"  style={{padding:"20px 0px"}}>
                                    <label className="parking-space-label">Meter type</label>
                                    <Dropdown options={meterTypeOptions}
                                              className="form-control"
                                              id="howMetered"
                                              value={parkingspace.metadata.howMetered}
                                              onChange={this.handleChange('howMetered')}/>
                                </div>

                                {/* Commenting out the code as per story: 7408*/}
                                {/*<div className="col-sm-12"  style={{padding:"20px 0px"}}>*/}
                                    {/*<label className="parking-space-label">Meter or paystation ID</label>*/}
                                    {/*<input style={{width:"90%"}} type="text" className="" value={parkingspace.metadata.paystationid?parkingspace.metadata.paystationid: ''}*/}
                                           {/*id="payStationId" ref="payStationId" placeholder='Enter meter or paystation ID' onChange={this.handleChange('payStationId')} />*/}
                                {/*</div>*/}
                                <div  className="parking-space-sets">
                                    <div className="col-sm-6">
                                        <label className="parking-space-label">Demarcated</label>

                                        <span className="parking-space-value">{parkingspace.demarcated ? "Yes":"No"}</span>
                                    </div>

                                    {/* Dispalyed only when the space is a demarcated space*/}
                                    {parkingspace.demarcated ? (""):<div className="col-sm-6">
                                        <label className="parking-space-label">Max number of vehicles</label>
                                        <span className="parking-space-value">{parkingspace.max_spaces} </span>
                                    </div>}

                                </div>
                            </div>
                        </div>
                    </div>

                    <div>
                        <div className="col-sm-3">
                            <button type="button" id="cancelspace" className="ns-delete-btn" onClick={this.handleCancel}>
                                {/*<Icon glyph="icon-fontello-trash" /> */}
                                Cancel </button>
                        </div>
                        <div className="col-sm-8 text-right">
                            <button type="button" id="saveSpace" className="ns-save-btn" onClick={this.handleSubmit}>
                                <Icon glyph="icon-fontello-ok" /> Save</button>
                        </div>
                    </div>
                </form>
            </div>
        );

  }
});

  module.exports = Parkingspaceform;
