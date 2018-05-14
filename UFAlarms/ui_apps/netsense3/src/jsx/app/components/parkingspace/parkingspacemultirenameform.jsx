import classNames from 'classnames';
import helpers from '../../../../global/utils/helpers';
import auth from '../../../../global/utils/auth';
import { State, Navigation } from 'react-router';
import DataUtil from '../../service/datautil';
import Dropdown from 'components/dropdown';

var Parkingspacemultirenameform = React.createClass({

  getInitialState: function () {
    return {
      selectedParkingSpaces: this.props.selectedParkingSpaces,
      spaceNamePattern:[],
      type:"",
      placement:"",
      count:0,
      numbers: "0123456789",
      newNameCount:0,
      name:""

    }
  },

  init: function () {
  },

  propTypes: {
      selected_spaces: React.PropTypes.array.isRequired,
      parkingspaces: React.PropTypes.array.isRequired,
  },

    handleChange: function (key) {
        return function (e) {
            var state = this.state;
            switch (key) {
                case 'name':
                    state[key] = e.target.value.trim();
                    state.count= e.target.value.length;
                    state.spaceNamePattern = this.genNewNameSequence('name', e.target.value.trim());

                    break;
                case 'startNumber':
                    state[key] = e.target.value;
                    state.numberToStartWith = e.target.value;
                    state.spaceNamePattern = this.getNewNumberSequence("startNumber", e.target.value);

                    break;
                default:' ';
            };
            this.setState({state});
        }.bind(this);
    },

    appendNameToPattern:function(spaceNamePattern, name){
        for(var p=0; p<spaceNamePattern.length; p++){
            var afterAddingName = name + spaceNamePattern[p];
            spaceNamePattern[p] = afterAddingName;
        }

    },

    genNewNameSequence: function(key, value){
        var name= this.state.name;
        var newSequenceArray = [];

        if(this.state.placement == "before"){
            newSequenceArray= helpers.genSpaceNameSequence([], 100);
            for(var a in newSequenceArray){
                newSequenceArray[a] = newSequenceArray[a] + name  ;
            }
        }else if(this.state.placement == "after"){
            newSequenceArray= helpers.genSpaceNameSequence([], 100);
            for(var a in newSequenceArray){
                newSequenceArray[a] = name + newSequenceArray[a];
            }
        }else{
            newSequenceArray= helpers.genSpaceNameSequence([], 100);
            for(var a in newSequenceArray){
                newSequenceArray[a] = name + newSequenceArray[a];
            }
        }
        return newSequenceArray;

    },

    // If the start number has been changed  in the input value
    getNewNumberSequence:function(key, value){
      var name= this.state.name;
        var newSequenceArray=[]
        if(this.state.placement == "before"){
            newSequenceArray= helpers.genNumberSequence([], 100, "none", value );
          for(var a in newSequenceArray){
              newSequenceArray[a] = newSequenceArray[a] + name  ;
          }
      }else if(this.state.placement == "after"){
          newSequenceArray = helpers.genNumberSequence([], 100, "none", value );
          for(var a in newSequenceArray){
                  newSequenceArray[a] = name + newSequenceArray[a];
          }
      }
      return newSequenceArray;
    },

    alpha_IncrementBefore: function (spaceNamePattern, name) {
        for (var a = 0; a < spaceNamePattern.length; a++) {
            // 24 -- as the letters start repeating after z to AA Ab and so on
            if (a < 24) {
                var lastItem = spaceNamePattern[a].slice(-1);
                lastItem += name;
                spaceNamePattern[a] = lastItem;
            } else if (a > 24) {
                var lastItems = spaceNamePattern[a].slice(spaceNamePattern[a].length - 2);
                var currentName = name;
                lastItems += currentName;
                spaceNamePattern[a] = lastItems;
            }
        }
    }, 
    
    alpha_IncrementAfter: function (spaceNamePattern, name) {
        for (var a = 0; a < spaceNamePattern.length; a++) {
            if (a < 24) {
                var firstItem = spaceNamePattern[a].slice(0, 1);
                var currentName = name;
                currentName += firstItem
                spaceNamePattern[a] = currentName;
            } else if (a > 24) {
                var firstItem = spaceNamePattern[a].slice(0, 2)
                var currentName = name;
                currentName += firstItem
                spaceNamePattern[a] = currentName;
            }
        }
    }, 
    
    numeric_IncrementBefore: function (spaceNamePattern, name) {
	      for (var a = 0; a < spaceNamePattern.length; a++) {
		      if (a <= 9) {
			      var lastItem = spaceNamePattern[a].slice(-1);
			      lastItem += name;
			      spaceNamePattern[a] = lastItem;
		      } else if (a > 9) {
			      var lastItems = spaceNamePattern[a].slice(spaceNamePattern[a].length - 2);
			      lastItems += name;
			      spaceNamePattern[a] = lastItems;
		      }
	      }
    },

    numeric_IncrementBeforeWithNumber: function (number, spaceNamePattern, name) {
	    if(Number(number.charAt(0)) === 0){
		    if(number.length === 1){
			    // Regular append
			    for (var a = 0; a < spaceNamePattern.length; a++) {
				    if (a <= 9) {
					    var lastItem = spaceNamePattern[a].slice(-1);
					    lastItem += name;
					    spaceNamePattern[a] = lastItem;
				    } else if (a > 9) {
					    var lastItems = spaceNamePattern[a].slice(spaceNamePattern[a].length - 2);
					    lastItems += name;
					    spaceNamePattern[a] = lastItems;
				    }
			    }
		    }else{
			    //   More than 1 is the length as in 2 digits with a zero in front of it
			    var secondChar = Number(number.charAt(1)); //
			    var changeUntil = 9-secondChar;
			    var changeAfter = changeUntil+1
			    for(var k = 0; k<spaceNamePattern.length; k++){
				    //     slice the last 2 and replace them -
				    if(k <= changeUntil){ // until the second char needs to replace the last 2 of them
					    var lastItems = spaceNamePattern[k].slice(spaceNamePattern[k].length - 2);
					    lastItems += name;
					    spaceNamePattern[k] = lastItems;
				    }else if(k >= changeAfter){
					    var lastItems = spaceNamePattern[k].slice(spaceNamePattern[k].length - 2);
					    lastItems += name;
					    spaceNamePattern[k] = lastItems;
				    }
			    }
		    }
        }else{ // if the first element is not a zero
		    var singleDigitValues = 9 - number; // Anything Less then 9 need to be in a separate loop
		    for (var j = 0; j < spaceNamePattern.length; j++) {
			    if (j <= singleDigitValues) {
				    var lastItem = spaceNamePattern[j].slice(-1);
				    lastItem += name;
				    spaceNamePattern[j] = lastItem;
			    } else {
				    var lastItems = spaceNamePattern[j].slice(spaceNamePattern[j].length - 2);
				    lastItems += name;
				    spaceNamePattern[j] = lastItems;
			    }
		    }
	    }
    },

    numeric_IncrementAfter: function (spaceNamePattern, name) {
        for (var a = 0; a < spaceNamePattern.length; a++) {
            if (a <= 9) {
                var firstItem = spaceNamePattern[a].slice(0, 1);
                var currentName = name;
                currentName += firstItem
                spaceNamePattern[a] = currentName;
            } else if (a > 9) {
                var firstItem = spaceNamePattern[a].slice(0, 2)
                var currentName = name;
                currentName += firstItem
                spaceNamePattern[a] = currentName;
            }
        }
    },

    numeric_IncrementAfterWithNumber: function (number, spaceNamePattern, name) {
	    if(Number(number.charAt(0)) === 0){
		    if(number.length === 1){
			    // Regular append
			    for (var j = 0; j < spaceNamePattern.length; j++) {
				    if (j <= 9) {
					    var firstItem = spaceNamePattern[j].slice(0, 1);
					    var currentName = name;
					    currentName += firstItem
					    spaceNamePattern[j] = currentName;
				    } else if (j> 9) {
					    var firstItem = spaceNamePattern[j].slice(0, 2)
					    var currentName = name;
					    currentName += firstItem
					    spaceNamePattern[j] = currentName;
				    }
			    }
		    }else{
			    //More than 1 is the length as in 2 digits with a zero in front of it
			    var secondChar = Number(number.charAt(1)); //
			    var changeUntil = 9-secondChar;
			    var changeAfter = changeUntil+1
			    for(var k = 0; k<spaceNamePattern.length; k++){
				    //     slice the last 2 and replace them -
				    if(k <= changeUntil){ // until the second char needs to replace the last 2 of them
					    var firstItem = spaceNamePattern[k].slice(0, 2);
					    var currentName = name;
					    currentName += firstItem
					    spaceNamePattern[k] = currentName;
				    }else if(k >= changeAfter){
					    var firstItem = spaceNamePattern[k].slice(0, 2);
					    var currentName = name;
					    currentName += firstItem
					    spaceNamePattern[k] = currentName;
				    }
			    }
		    }
        }else{
		    var singleDigitValues = 9 - number; // Anything Less then 9 need to be in a separate for loop
		    for (var j = 0; j < spaceNamePattern.length; j++) {
			    if (j < singleDigitValues) {
				    var firstItem = spaceNamePattern[j].slice(0, 1);
				    var currentName = name;
				    currentName += firstItem
				    spaceNamePattern[j] = currentName;
			    } else {
				    var firstItem = spaceNamePattern[j].slice(0, 2)
				    var currentName = name;
				    currentName += firstItem
				    spaceNamePattern[j] = currentName;
			    }
		    }
	    }
    },

    handleRadio: function (key) {
        return function (e) {
            // var state = this.state;
            var name = this.state.name;
            var spaceNamePattern = this.state.spaceNamePattern;
            var type = this.state.type;
            var placement = this.state.placement;
            var number = this.state.numberToStartWith;
            var newNameCount = this.state.newNameCount;

            switch (key) {
                case "alpha-increments":
                        // Setting the array to empty as the user may switch between numerical values and alphabets
                        spaceNamePattern=[];
                        // first argument returns the newly generated array of strings
                        // second argument is basically the number of space names that need to be generated - customizable
                        spaceNamePattern = helpers.genSpaceNameSequence(spaceNamePattern, 100);
                        this.appendNameToPattern(spaceNamePattern, name);
                        type = "alpha";
                        placement="after"
                        break;
                case "alpha-numeric-increments":
                        spaceNamePattern=[];
                        helpers.genNumberSequence(spaceNamePattern, 100, this.state.numbers, "none");
                        this.appendNameToPattern(spaceNamePattern, name);
                        type = "numeric";
                        placement="after"
                        break;
                case "increment-before":
                    if(placement=="after"){
                        // if you want to place it before but it is sitting after,
                        // then pull the value out of the back and place in front
                        if(type== "alpha"){
                            this.alpha_IncrementBefore(spaceNamePattern, name);
                        }else if(type=="numeric"){
                                 if(number == undefined){
                                     this.numeric_IncrementBefore(spaceNamePattern, name);
                                 }else{
                                     this.numeric_IncrementBeforeWithNumber(number, spaceNamePattern, name);
                                 }
                        }
                        placement="before";
                    }
                    break;
                case "increment-after":
                    if(placement=="before"){
                        // if you want to place it after but it is sitting before,
                        // then pull the value out of the front and place it at the back

                        if(type=="alpha"){
                            this.alpha_IncrementAfter(spaceNamePattern, name);
                        }
                        else if(type == "numeric"){
                            if(number == undefined){
                                this.numeric_IncrementAfter(spaceNamePattern, name);
                            }else{
                                this.numeric_IncrementAfterWithNumber(number, spaceNamePattern, name);
                            }
                        }
                        placement="after";
                    }
                    break;
            };
            this.setState({ name, spaceNamePattern, type, placement});
        }.bind(this);
    },

	isValid: function() {
		this.props.errors = {};
		var rules = {
			name: {
				required: true
			}
		};

		console.log("state", this.state);
		this.props.errors = helpers.checkValidity(this.state, rules);
		console.log("this.props.errors ",this.props.errors );
		return (Object.keys(this.props.errors).length == 0);
	},

    handleSubmit: function(e) {
        e.stopPropagation();
        e.preventDefault();
	    if (this.isValid()) {
		    this.props.errors={};
		    $(".invalidAlert").css("display","none");
		    var spaceRenamesGenerated = true;
		    ReactBootstrap.Dispatcher.emit("Parkingspacemultirenameform.multispacerename",  this.state.spaceNamePattern, this.props.selected_spaces, spaceRenamesGenerated);
		    this.props.hide("renameSpaces");
	    }else {
		    $(".invalidAlert").css("display","block");

	    }
    },

	handleBlur :function(e) {
      if(e.target.value !== ""){
	      $(".invalidAlert").css("display","none");
      }
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
			}
		}

	},



	handleCancel: function(e) {

      // console.log(this.state)
        e.stopPropagation();
        e.preventDefault();
        var spaceRenamesGenerated = false;
        ReactBootstrap.Dispatcher.emit('Parkingspacemultirenameform.cancelRenameSpaces', [])
        this.props.hide("renameSpaces");
    },

    componentDidMount: function () {
      this.setState({newNameCount : this.props.selected_spaces.length})
  },

  componentWillReceiveProps: function (nextProps) {
  },

  render: function () {
      var hstyle = {
          overflowY: "auto", overflowX: "hidden", marginBottom: "16px",
          maxHeight: helpers.calcHeight(100, -240) + "px !important"
      };
      var businessUseOptions = [{"loading":"loading"}, {"general":"general"}, {"emergency":"emergency"},{"school":"school"}];
      var areaTypeOptions = [{"Industrial Type":"Industrial Type"},{"industrial":"industrial"},{"mixed":"mixed"}, {"school":"school"}, {"general":"general"}, {"emergency":"emergency"},{"loading":"loading"}, {"cbd":"cbd"},{"commercial":"commercial"},{"residential":"residential"}];
      var meterTypeOptions = [{"paystation-metered":"paystation-metered"}, {"individually-metered":"individual-metered"},{"individual-meter":"individual-meter"}, {"not-metered":"not-metered"}];

      return (
      <div>
          <form role="form"className="form-horizontal " >

              <div className="netsense__form__header" style={{borderBottom:"1px solid rgba(0, 0, 0, 0.2784313725490196)", marginBottom:"17px"}}>
                  <h3> Renaming sequence</h3>

              </div>

              <div style={hstyle}>

                  <div className="col-sm-12 parking-space-set">
                      <label className="parking-space-label">Parking spaces name </label>
                      <input type="text" className="parking-space-input" value={this.state.name} onChange={this.handleChange('name')}
                             id="name" ref="name" placeholder='Enter one or two letters'
                             onKeyDown={(e)=>{this.updateName(e.keyCode)}} onBlur={(e)=>this.handleBlur(e)} />
	                  <div className="invalidAlert" style={{display:'none', position:"relative", float:"none !important"}}>
		                  <div className="form-error" style={{margin:"0px",padding:"0px", fontSize:"14px"}}>
			                  Field is required
		                  </div>
	                  </div>

                  </div>


                      <div className="col-sm-12 parking-space-set">
                          <label className="parking-space-label">Name increments</label>
                          <ul className="parking-space-checkbox-list">
                              <li>
                                  <input type="radio" style={{width:"20px"}} disabled={!this.state.name} name="alpha-increments" onChange={this.handleRadio('alpha-increments')} />
                                  <span className="parking-space-radio-label">Alphabetical (AA, AB, AC...)</span>
                              </li>
                              <li style={{marginLeft:"30px"}}>
                                  <input type="radio" style={{width:"20px"}} disabled={!this.state.name} name="alpha-increments" onChange={this.handleRadio('alpha-numeric-increments')}/>
                                  <span className="parking-space-radio-label">Numerical (A1, A2, A3...)</span>
                              </li>
                          </ul>

                      </div>

                      {this.state.type == "numeric" ? (
                          <div id="number" className="col-sm-12 parking-space-set">
                              <label className="parking-space-label">Start number  </label>
                              <input  type="text" className="parking-space-input"  value={this.state.startNumber} onChange={this.handleChange('startNumber')}
                                      id="startNumber" ref="startNumber" placeholder='Enter start number' />
                          </div>
                      ):""}

                      <div className="col-sm-12 parking-space-set">
                          <label className="parking-space-label">Increment placement</label>
                          <ul className="parking-space-checkbox-list">
                              <li>
                                  <input type="radio" name="increment" disabled={!this.state.name}  onChange={this.handleRadio('increment-before') }/>
                                  <span className="parking-space-radio-label">Before (1A, 2A ,3A...)</span>
                              </li>
                              <li style={{marginLeft:"30px"}}>
                                  <input type="radio" name="increment" disabled={!this.state.name}  onChange={this.handleRadio('increment-after')}/>
                                  <span className="parking-space-radio-label">After (A1, A2 ,A3...)</span>
                              </li>
                          </ul>
                      </div>

                      <div className="col-sm-12 parking-space-set">

                          <label className="parking-space-label">Example</label>

                          <ul className="parking-space-rename-list">
                               {this.state.spaceNamePattern.map(p => <li>{p}</li>)}
                          </ul>
                      </div>

                      <div className="col-sm-12">
                          <button type="button" className="ns-delete-btn" id="cancelSpaceRename" onClick={this.handleCancel} style={{left:"343px"}}>
                              Cancel </button>
                          <button type="button" className="ns-save-btn" id="saveSpaceRename" onClick={this.handleSubmit} style={{float:"right", width: "90px !important"}}>
                              Continue</button>
                      </div>
              </div>
          </form>
      </div>
    );
  }
});

module.exports = Parkingspacemultirenameform;