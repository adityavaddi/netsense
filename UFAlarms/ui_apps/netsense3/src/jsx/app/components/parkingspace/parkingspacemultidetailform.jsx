
import classNames from 'classnames';
import auth from 'global/utils/auth';
import helpers from 'global/utils/helpers';
import { State, Navigation } from 'react-router';
import Dropdown from 'components/dropdown';
import Parkingspacemultirenameform from 'components/parkingspace/parkingspacemultirenameform';
import Parkingspacemultidetaileditform from 'components/parkingspace/parkingspacemultidetaileditform';
import { Modal } from 'react-bootstrap';


var Parkingspacemultidetailform = React.createClass({

  getInitialState: function(){
      return {
          selected_spaces: this.props.selected_spaces,
          vehicleTypes:[],
          reservation:[],
          handicap:[],
          // PPV:[],
          businessUse:[],
          locationType:[],
          meterType:[],
          areaType:[],
          sensorids:[],
          reservationText:[],
          handicapText:[],
          // PPVText:[],
          demarcatedV:[],
          displayRenameSpaces:false,
	      parkingSpaceType:[]
      }
  },

  propTypes: {
      selected_spaces: React.PropTypes.array.isRequired,
      parkingspaces: React.PropTypes.array.isRequired,
      defaultMetadataObject:React.PropTypes.object.isRequired
  },

  getDefaultProps: function() {
    return {
      errors: {}
    };
  },
    handleEditAttributes:function(){
        this.setState({
            showEditAttributes:true
        })
        $('#parkingspace-list-panel').css('z-index', '0');
    },

    handleSpacesRename:function(){
        this.setState({
            showSpacesRename:true
        })
        $('#parkingspace-list-panel').css('z-index', '0');

    },

    hide:function(modal){
      if(modal === "editAttributes"){
          this.setState({
              showEditAttributes:false
          });
          $('#parkingspace-list-panel').css('z-index', '300');

      }else if(modal === "renameSpaces"){
          this.setState({
              showSpacesRename:false
          });
          $('#parkingspace-list-panel').css('z-index', '300');

      }

    },

    getSpaces: function(spaces){
        var selectedSpaces = [];
        for(var i in this.props.parkingspaces){
            for(var j in spaces){
                if(this.props.parkingspaces[i].parkingspaceid == spaces[j]){
                    selectedSpaces.push(this.props.parkingspaces[i]);
                }
            }
        }
        return selectedSpaces;

    },

    getText: function (type, values) {
        if (type.includes(false) && (type.includes(true))) {
            values.push("Yes");
            values.push("No");
        } else if (type.includes(false)) {
            values.push("No");
        } else if (type.includes(true)) {
            values.push("Yes");
        }
        return values;
    },

	getType:function(arrayType, text){
        for(var p in arrayType){
		    var value = arrayType.toString();
	        text.indexOf(value) === -1 ?text.push(value):"";
		}
		return text;
    },
    getAttributeValues: function (selectedSpaces) {
        var vehicleTypes=[];
        var reservation=[];
        var handicap = [];
        // var PPV = [];
        var businessUse = [];
        var locationType = [];
        var meterType = [];
        var meterPaystationId = [];
        var areaType = [];
        var sensorids = [];
        var groups=[];
        var demarcatedValue = [];
        var parkingSpaceType = []

        for(var x in selectedSpaces){

            if(selectedSpaces[x].parkinggroupname){
                groups.indexOf(selectedSpaces[x].parkinggroupname) === -1 ?
                    groups.push(selectedSpaces[x].parkinggroupname)
                    :"";
            }

            if(selectedSpaces[x].metadata){
                if(selectedSpaces[x].metadata.businessUse !== undefined){
                    businessUse.indexOf(selectedSpaces[x].metadata.businessUse) === -1 ?
                        businessUse.push(selectedSpaces[x].metadata.businessUse)
                        :"";
                }

                if(selectedSpaces[x].metadata.handicap !== undefined){
                    handicap.indexOf(selectedSpaces[x].metadata.handicap) === -1 ?
                        handicap.push(selectedSpaces[x].metadata.handicap)
                        :"";
                }

                if(selectedSpaces[x].metadata.reservation !== undefined){
                    reservation.indexOf(selectedSpaces[x].metadata.reservation) === -1 ?
                        reservation.push(selectedSpaces[x].metadata.reservation)
                        :"";
                }
                // if(selectedSpaces[x].metadata.PPV !== undefined){
                //     PPV.indexOf(selectedSpaces[x].metadata.PPV) === -1 ?
                //         PPV.push(selectedSpaces[x].metadata.PPV)
                //         :"";
                // }

                if(selectedSpaces[x].metadata.areaType !== undefined){
	                areaType = this.getType(selectedSpaces[x].metadata.areaType, areaType);
                }
                if(selectedSpaces[x].metadata.parkingSpaceType !== undefined){
                    parkingSpaceType = this.getType(selectedSpaces[x].metadata.parkingSpaceType,parkingSpaceType );
                    console.log("parkingSpaceType", parkingSpaceType)
                }

                if(selectedSpaces[x].metadata.typeOfVehicle !== undefined){
	                vehicleTypes = this.getType(selectedSpaces[x].metadata.typeOfVehicle, vehicleTypes);
                }
                if(selectedSpaces[x].metadata.howMetered !== undefined){
                    meterType.indexOf(selectedSpaces[x].metadata.howMetered) === -1 ?
                        meterType.push(selectedSpaces[x].metadata.howMetered)
                        :"";
                }
                if(selectedSpaces[x].nodeid !== undefined){
                    sensorids.indexOf(selectedSpaces[x].nodeid) === -1 ?
                        sensorids.push(selectedSpaces[x].nodeid)
                        :"";
                }
                if(selectedSpaces[x].demarcated !== undefined){
                    demarcatedValue.indexOf(selectedSpaces[x].demarcated) === -1 ?
                        demarcatedValue.push(selectedSpaces[x].demarcated)
                        :"";
                }
            }
        }

        var count = 0;
        var groupText = "";
        for(var j in groups){
            var oneElement = groups[j];
            if(groups[0] === groups[j]){
                count++;
            }
        }
            if(count < groups.length){
                groupText = "Multiple Groups"
            }else if(count == groups.length){
                groupText = groups[0];
            }
        var reservationText = this.getText(reservation, []);
        var handicapText = this.getText(handicap, []);
        // var PPVText = this.getText(PPV, []);
        var demarcatedV = this.getText(demarcatedValue, []);

        this.setState({
            businessUse,
            reservation,
            handicap,
            // PPV,
            areaType,
            sensorids,
            meterType,
            vehicleTypes,
            groupText,
            selectedSpaces,
            reservationText,
            handicapText,
            // PPVText,
            demarcatedV,
	        parkingSpaceType
        });

    },

    componentDidMount: function(){
        var selectedSpaces = this.getSpaces(this.props.selected_spaces);
        this.getAttributeValues(selectedSpaces);

        var that = this;
        ReactBootstrap.Dispatcher.on('Parkingspacemap.multiSelect', function(temp){
            if(temp.length >0){
                that.setState({displayRenameSpaces:true});
            }else{
                that.setState({displayRenameSpaces:false});
            }
        });

    },

    componentWillReceiveProps: function(nextProps){
        this.setState(nextProps.selected_spaces);
        var newSpaces = this.getSpaces(nextProps.selected_spaces);
        this.getAttributeValues(newSpaces);

    },

  render: function() {
      var hstyle = {overflowY:"auto",overflowX:"hidden",marginBottom:"16px",
          maxHeight:helpers.calcHeight(100, -200)+"px !important"};
      var heading = (<span><Icon glyph="icon-fontello-right-dir"/></span>);
                var editAttributesModal = this.state.showEditAttributes ?
                  <div className="customerForm" >
                      <Modal.Dialog style={{ marginTop: 100 }} id="parkingspace-modal">
                          <Modal.Body >
                              <a className="" id="customeroverlayClose" onClick={() => {this.hide("editAttributes")}}>
                                  <img src='/imgs/Close1.svg' width='30' height='30'  style={{marginTop:'60px',marginRight:'8px', zIndex: 100, width:"20px",height:"20px"}}/>
                              </a>
                              <Parkingspacemultidetaileditform
                                  show={this.state.showEditAttributes}
                                  selected_spaces={this.state.selectedSpaces}
                                  parkingspaces={this.props.parkingspaces}
                                  vehicleTypes={this.state.vehicleTypes}
                                  locationType={this.state.locationType}
                                  reservation={this.state.reservation}
                                  handicap={this.state.handicap}
                                  businessUse={this.state.businessUse}
                                  meterType={this.state.meterType}
                                  parkingSpaceType={this.state.parkingSpaceType}
                                  areaType={this.state.areaType}
                                  sensorids={this.state.sensorids}
                                  groupText={this.state.groupText}
                                  defaultMetadataObject={this.props.defaultMetadataObject}
                                  hide={()=>{this.hide("editAttributes")}}/>
                          </Modal.Body>
                      </Modal.Dialog>
                  </div>
                :"";

                var renameSpacesModal = this.state.showSpacesRename ?
                   <div className="customerForm" >
                       <Modal.Dialog style={{ marginTop: 100 }} id="parkingspace-modal">
                           <Modal.Body >
                               <a className="" id="customeroverlayClose" onClick={() => {this.hide("renameSpaces")}}>
                                   <img src='/imgs/Close1.svg' width='30' height='30'  style={{marginTop:'60px',marginRight:'8px', zIndex: 100, width:"20px",height:"20px"}}/>
                               </a>
                               <Parkingspacemultirenameform selected_spaces={this.props.selected_spaces} hide={()=>{this.hide("renameSpaces")}}/>
                           </Modal.Body>
                       </Modal.Dialog>
                   </div>
                   :"";

      return (
      <div>
        <h2 className="parking-space-detail-header" > Aggregated Spaces </h2>
        <form role="form"className="form-horizontal " >
              <div style={hstyle}>
                  <div>


                      {/*Set 1*/}
                      <div>
                          <div className="col-sm-12 parking-space-detail">
                              <div className="col-sm-6">
                                  <label className="parking-space-label">Space ID</label>
                                  <span className="parking-space-value"> Multiple</span>
                              </div>
                              <div className="col-sm-6" style={{paddingLeft:"23px !important"}}>
                                  <label className="parking-space-label">Group</label>
                                  <span className="parking-space-value"> {this.state.groupText} </span>
                              </div>

                          </div>
                      </div>

                      {/*Set 2*/}
                      <div className="col-sm-12 parking-space-detail">
                          <div>
                              <label className="parking-space-label">Vehicle type(s)</label>
                                  <span className="parking-space-value">
                                      {this.state.vehicleTypes.join(", ")}
                              </span>

                          </div>
                                    <div className="parking-space-sets">
                                        <div className="col-sm-6">
                                            <label className="parking-space-label">Reservable</label>
                                            <ul className="parking-space-checkbox-list">
                                                {this.state.reservationText.join(", ")}
                                            </ul>

                                        </div>
                                        <div className="col-sm-6" style={{paddingLeft:"23px !important"}}>
                                            <label className="parking-space-label">Handicap</label>
                                            <ul className="parking-space-checkbox-list">
                                                {this.state.handicapText.join(", ")}
                                            </ul>

                                        </div>
                                    </div>

                                    <div  className="parking-space-sets">
                                        {/*<div className="col-sm-6">*/}
                                            {/*<label className="parking-space-label">Enforce poorly parked violation</label>*/}
                                            {/*<ul className="parking-space-checkbox-list">*/}
                                                {/*{this.state.PPVText.join(", ")}*/}
                                            {/*</ul>*/}

                                        {/*</div>*/}
                                        <div className="col-sm-6">
                                            <label className="parking-space-label">Sensor ID</label>
                                            <span className="parking-space-value">{this.state.sensorids.length>1?"Multiple":"Single Sensor ID present "}  </span>
                                        </div>
                                    </div>

                                    <div className="col-sm-12" style={{padding:"20px 0px"}}>
                                        <label className="parking-space-label">Business use</label>
                                        {this.state.businessUse.join(", ")}
                                    </div>

                                    <div className="col-sm-12"  style={{padding:"20px 0px"}}>
                                        <label className="parking-space-label">Location Type</label>
                                        {this.state.areaType.join(", ")}
                                    </div>

                                      <div className="col-sm-12" style={{padding:"20px 0px"}}>
                                          <label className="parking-space-label">Parking Space Type</label>
                                          {this.state.parkingSpaceType.join(", ")}
                                      </div>
                                    </div>

                      {/* Set 3*/}
                            <div className="col-sm-12 parking-space-detail">
                                    <div className="col-sm-12"  style={{padding:"20px 0px"}}>
                                        <label className="parking-space-label">Meter type</label>

                                        {this.state.meterType.join(", ")}
                                    </div>
                                 <div  className="parking-space-sets">
                                        <div className="col-sm-6">
                                            <label className="parking-space-label">Demarcated</label>
                                            <span className="parking-space-value">{this.state.demarcatedV.join(", ")}</span>
                                        </div>
                                        {/*<div className="col-sm-6">*/}
                                            {/*<label className="parking-space-label">Max number of vehicles</label>*/}
                                            {/*<span className="parking-space-value"> </span>*/}
                                        {/*</div>*/}
                                    </div>
                                </div>
                  </div>
              </div>
        </form>

          <div className="col-sm-12">
              <div className="col-sm-6">
                  <button type="button" className="ns-delete-btn" onClick={this.handleEditAttributes}>
                      Edit Attributes </button>
              </div>

              {this.state.displayRenameSpaces?
                  <div className="col-sm-6" >
                      <button type="button" className="ns-delete-btn" onClick={this.handleSpacesRename}>
                          Rename Spaces </button>
                  </div>
                  :""
              }
          </div>

          {editAttributesModal}
          {renameSpacesModal}
      </div>
    );

  }
});

  module.exports = Parkingspacemultidetailform;
