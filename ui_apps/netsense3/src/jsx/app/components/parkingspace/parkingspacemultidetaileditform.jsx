import classNames from 'classnames';
import helpers from '../../../../global/utils/helpers';
import auth from '../../../../global/utils/auth';
import { State, Navigation } from 'react-router';
import DataUtil from '../../service/datautil';
import Dropdown from 'components/dropdown';

var Parkingspacemultidetaileditform = React.createClass({

  getInitialState: function () {
    return this.props.defaultMetadataObject
  },

  propTypes: {
      selectedSpaces: React.PropTypes.array.isRequired,
      parkingspaces: React.PropTypes.array.isRequired,
      vehicleTypes:React.PropTypes.array.isRequired,
      reservation:React.PropTypes.array.isRequired,
      handicap:React.PropTypes.array.isRequired,
      // PPV:React.PropTypes.array.isRequired,
      businessUse:React.PropTypes.array.isRequired,
      locationType:React.PropTypes.array.isRequired,
      meterType:React.PropTypes.array.isRequired,
      areaType:React.PropTypes.array.isRequired,
      sensorids:React.PropTypes.array.isRequired,
      groupText:React.PropTypes.string.isRequired,
      demarcatedV:React.PropTypes.string.isRequired,
      defaultMetadataObject:React.PropTypes.object.isRequired,
	  parkingSpaceType:React.PropTypes.array.isRequired,
  },

  componentDidMount: function () {
  },

    typeOfVehicle: function (state, key) {
        var found = state.typeOfVehicle.indexOf("unrestricted");
        while (found !== -1) {
            state.typeOfVehicle.splice(found, 1);
            found = state.typeOfVehicle.indexOf("unrestricted");
        }
        state.typeOfVehicle.push(key);
    },
    uncheck: function(state, key){
        var found = state.typeOfVehicle.indexOf(key);
        console.log(found);
        if(found !== -1 ){
            state.typeOfVehicle.splice(found, 1);
        }
    },


    handleChange: function(key){
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
                        state.typeOfVehicle = [];
                        state.typeOfVehicle.push(key);
                    }
                    break;
                case "businessUse":
                    state.businessUse =  e.value;
                    break;
                case "areaType":
                    state.areaType[0] =  e.value;
                    break;
                case "parkingSpaceType":
                    state.parkingSpaceType=  e.value;
                    break;
                case "howMetered":
                    state.howMetered =  e.value;
                    break;


                default:' ';
            };
            this.setState({state: state});
        }.bind(this);
    },

    handleRadio: function (key) {
        return function (e) {
            var state = this.state;
            switch (e.target.value) {
                case "reservation-false":
                    state.reservation = false;
                    break;
                case "reservation-true":
                    state.reservation = true;
                    break;
                case "handicap-true":
                    state.handicap = true;
                    break;
                case "handicap-false":
                    state.handicap = false;
                    break;
                // case "PPV-true":
                //     state.PPV = true;
                //     break;
                // case "PPV-false":
                //     state.PPV = false;
                //     break;
            };
            this.setState({state});
        }.bind(this);
    },

    handleSubmit: function(e) {
        e.stopPropagation();
        e.preventDefault();
        delete this.state.state;
        var updatedMetadata = this.state;
        var selectedSpaces = this.props.selected_spaces;
        for(var j=0; j < selectedSpaces.length; j++){
            selectedSpaces[j].metadata.typeOfVehicle = updatedMetadata.typeOfVehicle;
            selectedSpaces[j].metadata.handicap = updatedMetadata.handicap;
            selectedSpaces[j].metadata.reservation = updatedMetadata.reservation;
            selectedSpaces[j].metadata.businessUse = updatedMetadata.businessUse;
            selectedSpaces[j].metadata.howMetered = updatedMetadata.howMetered;
            // selectedSpaces[j].metadata.PPV = updatedMetadata.PPV;
            selectedSpaces[j].metadata.areaType = updatedMetadata.areaType;
            selectedSpaces[j].metadata.parkingSpaceType = updatedMetadata.parkingSpaceType;
            selectedSpaces[j].metadata.paystationid = updatedMetadata.paystationid;
            delete selectedSpaces[j].metadata.createdOn;
            delete selectedSpaces[j].metadata.lastUpdated;
        }
        ReactBootstrap.Dispatcher.emit("Parkingspacemultidetaileditform.save", selectedSpaces);
        this.props.hide("editAttributes");
    },

    handleCancel:function(e){
        e.stopPropagation();
        e.preventDefault();
        this.props.hide("editAttributes");
    },


    componentWillReceiveProps: function (nextProps) {
  },

  render: function () {

      var aggregatedSpaces = this.state;

      var hstyle = {
          overflowY: "auto", overflowX: "hidden", marginBottom: "16px",
          maxHeight: helpers.calcHeight(100, -240) + "px !important"
      };
      var businessUseOptions = [{"loading":"loading"}, {"general":"general"}, {"emergency":"emergency"}];
      var areaTypeOptions = [{"industrial":"industrial"},{"mixed":"mixed"}, {"school":"school"}, {"commercial":"commercial"},{"residential":"residential"}, {"cbd":"cbd"}];
      var meterTypeOptions = [{"individually-metered":"individually-metered"}, {"paystation-metered":"paystation-metered"},{"not-metered":"not-metered"}, {"cloud-app":"cloud-app"}];
      var parkingSpaceTypeOptions = [{"curb-side":"curb-side"}, {"surface-lot":"surface-lot"}, {"multi-level":"multi-level"}];

      return (
      <div>
          <form role="form"className="form-horizontal " >

              <div className="netsense__form__header" style={{borderBottom:"1px solid rgba(0, 0, 0, 0.2784313725490196)", marginBottom:"17px"}}>
                  <h3> Edit Attributes</h3>
              </div>

              <div style={hstyle}>

                  <div>
                      <h5> Aggregated spaces <span className="type"> Active </span></h5>
                      <div className="group-name">{this.props.groupText} </div>
                  </div>
                  <div className="col-sm-12" style={{paddingLeft: "20px  !important"}}>
                      <div>
                          <label className="parking-space-label">Vehicle type(s)</label>
                          {/* Default: unrestricted*/}
                          <span className="parking-space-value">
                                  <ul className="parking-space-checkbox-list" style={{marginBottom:"20px"}}>
                                  <li><input type="checkbox" name="Bicycle" value="bicycle" style={{top:"2px"}} checked={aggregatedSpaces.typeOfVehicle.includes('bicycle')} onChange={this.handleChange('bicycle')} />
                                        <span>Bicycle</span></li>
                                        <li><input type="checkbox" name="Car" value="car" style={{top:"2px"}} checked={aggregatedSpaces.typeOfVehicle.includes('car')} onChange={this.handleChange('car')}/>
                                        <span>Car</span>
                                        </li>
                                        <li><input type="checkbox" name="Motorcycle" style={{top:"2px"}} value="motorcycle" checked={aggregatedSpaces.typeOfVehicle.includes('motorcycle')} onChange={this.handleChange('motorcycle')}/>
                                        <span>Motorcycle</span>
                                        </li>
                                        <li><input type="checkbox" name="Truck" style={{top:"2px"}} value="truck" checked={aggregatedSpaces.typeOfVehicle.includes('truck')} onChange={this.handleChange('truck')}/>
                                        <span>Truck</span>
                                        </li>
                                      {/*</ul>*/}
                                      <ul className="parking-space-checkbox-list">
                                        <li><input type="checkbox" name="Unrestricted" style={{top:"2px"}} value="unrestricted" checked={aggregatedSpaces.typeOfVehicle.includes('unrestricted')} onChange={this.handleChange('unrestricted')}/>
                                        <span>Unrestricted</span>
                                        </li>
                                        </ul>
                                    </ul>
                          </span>

                      </div>

                      <div className="parking-space-sets">
                          <div className="col-sm-6" style={{paddingLeft:"0px !important"}}>
                              <label className="parking-space-label">Reservable</label>
                              <ul className="parking-space-checkbox-list">
                                  <li>
                                      <input type="radio" style={{width:"20px"}} name="reservablespace" value="reservation-true" checked={aggregatedSpaces.reservation == true} onChange={this.handleRadio('reservation-true')} /> Yes
                                  </li>
                                  <li>
                                      <input type="radio" style={{width:"20px"}} name="reservablespace" value="reservation-false" checked={aggregatedSpaces.reservation == false} onChange={this.handleRadio('reservation-false')}/> No
                                  </li>
                              </ul>

                          </div>
                          <div className="col-sm-6" style={{paddingLeft:"23px !important"}}>
                              <label className="parking-space-label">Handicap</label>
                              <ul className="parking-space-checkbox-list">
                                  <li>
                                      <input type="radio" name="handicapparking" value="handicap-true" checked={aggregatedSpaces.handicap == true} onChange={this.handleRadio('handicap-true') }/> Yes
                                  </li>
                                  <li>
                                      <input type="radio" name="handicapparking" value="handicap-false"  checked={aggregatedSpaces.handicap == false} onChange={this.handleRadio('handicap-false')}/> No
                                  </li>
                              </ul>

                          </div>
                      </div>

                      <div  className="parking-space-sets">
                          {/*<div className="col-sm-6">*/}
                              {/*<label className="parking-space-label">Enforce poorly parked violation</label>*/}
                              {/*<ul className="parking-space-checkbox-list">*/}
                                  {/*<li>*/}
                                      {/*<input type="radio" name="ppa" value="PPV-true" checked={aggregatedSpaces.PPV == true} onChange={this.handleRadio('PPV-true')}/> Yes*/}
                                  {/*</li>*/}
                                  {/*<li>*/}
                                      {/*<input type="radio" name="ppa" value="PPV-false" checked={aggregatedSpaces.PPV == false} onChange={this.handleRadio('PPV-false')}/> No*/}
                                  {/*</li>*/}
                              {/*</ul>*/}
                          {/*</div>*/}
                          {/*<div className="col-sm-6" style={{paddingLeft:"0px !important"}}>*/}
                              {/*<label className="parking-space-label">Sensor ID</label>*/}
                              {/*<span className="parking-space-value"> </span>*/}
                          {/*</div>*/}
                      </div>

                      <div className="col-sm-12" style={{padding:"20px 0px", marginTop:"23px"}}>
                          <label className="parking-space-label">Business use</label>
                          <Dropdown options={businessUseOptions}
                                    className="form-control"
                                    id="businessuse"
                                    value={aggregatedSpaces.businessUse}
                                    onChange={this.handleChange('businessUse')}/>
                      </div>

                      <div className="col-sm-12"  style={{padding:"20px 0px"}}>
                          <label className="parking-space-label">Location Type</label>
                          <Dropdown options={areaTypeOptions}
                                    className="form-control"
                                    id="locationType"
                                    value={aggregatedSpaces.areaType}
                                    onChange={this.handleChange('areaType')}/>
                      </div>

	                  <div className="col-sm-12"  style={{padding:"20px 0px"}}>
		                  <label className="parking-space-label">Parking Space Type</label>
		                  <Dropdown options={parkingSpaceTypeOptions}
		                            className="form-control"
		                            id="parkingSpaceType"
		                            value={aggregatedSpaces.parkingSpaceType}
		                            onChange={this.handleChange('parkingSpaceType')}/>
	                  </div>
                  </div>

                  <div className="col-sm-12 " style={{marginBottom: "20px", paddingLeft:"20px !important"}}>
                      {/*<div className="parking-space-sets">*/}
                      <div className="col-sm-12"  style={{padding:"20px 0px"}}>
                          <label className="parking-space-label">Meter type</label>
                          <Dropdown options={meterTypeOptions}
                                    className="form-control"
                                    id="howMetered"
                                    value={aggregatedSpaces.howMetered}
                                    onChange={this.handleChange('howMetered')}/>
                      </div>

                      {/*<div className="col-sm-12"  style={{padding:"20px 0px"}}>*/}
                          {/*<label className="parking-space-label">Meter or paystation ID</label>*/}
                          {/*<input style={{width:"90%"}} type="text" className=""*/}
                                 {/*id="payStationId" ref="payStationId" placeholder='Enter meter or paystation ID' />*/}
                      {/*</div>*/}
                      <div  className="parking-space-sets">
                          {/*<div className="col-sm-6">*/}
                              {/*<label className="parking-space-label">Demarcated</label>*/}
                              {/*<span className="parking-space-value"></span>*/}
                          {/*</div>*/}
                          {/*<div className="col-sm-6">*/}
                              {/*<label className="parking-space-label">Max number of vehicles</label>*/}
                              {/*<span className="parking-space-value"> </span>*/}
                          {/*</div>*/}
                      </div>
                  </div>


                  <div className="parking-space-buttons-section">
                          <button type="button" className="ns-delete-btn" id="cancelMultiSpace" onClick={this.handleCancel} style={{ position: "relative",left: "66%"}}>
                              {/*<Icon glyph="icon-fontello-trash" /> */}
                              Cancel </button>
                          <button type="button" className="ns-save-btn" id="saveMultiSpace" onClick={this.handleSubmit} style={{float:"right"}}>
                             Save</button>
                  </div>
              </div>


          </form>
      </div>
    );
  }
});

module.exports = Parkingspacemultidetaileditform;