import classNames from 'classnames';
import auth from 'global/utils/auth';
import helpers from 'global/utils/helpers';
import { State, Navigation } from 'react-router';

var UFAlarmform = React.createClass({

  getInitialState: function () {
    return this.props.ufalarm
  },

  propTypes: {
    ufalarm: React.PropTypes.object.isRequired,
    allUFalarms: React.PropTypes.array.isRequired,
    errors: React.PropTypes.object,
    //alarmTypeList: React.PropTypes.array.isRequired
  },

  getDefaultProps: function () {
    return {
      errors: {}
    };
  },

  handleChange: function (key) {
    return function (e) {
      var state = {};
        if (typeof e == "string") {
          state[key] = document.getElementById(e).value
        } else {
          state[key] = e.target.value;
        }
      this.setState(state);
    }.bind(this);
  },

  handleNodeModel: function (key) {
    
  },

  handleSubmit: function (e) {
    console.log("state in handle submit", this.state);
    e.stopPropagation();
    e.preventDefault(); 
   var snode = $("#selected_nodemodel").val();

      this.props.errors = {};
      var ufalarm = Object.assign({}, this.state);
      var snode = $("#selected_nodemodel").val();
      if(snode.length > 0){
        ufalarm.nodemodels = snode;
      }
      if(document.getElementById('other_nodemodel').checked) {

        var otherNodes = $('#text_nodemodel').val();
        ufalarm.nodemodels.push(otherNodes);

      }
      ReactBootstrap.Dispatcher.emit("ufalarmform.save", ufalarm);
  },

  handleReset: function (e) {
    e.stopPropagation();
    e.preventDefault();
      ReactBootstrap.Dispatcher.emit("ufalarmform.reset", Object.assign({}, this.state));
  },

  getNodeModels: function(){
    return ["unode","falcon-q","merlin","vdkmaster","cnext"]
  },


  componentDidMount() {
    var options_alarmtype = '';
    var currentCategory = '';
    var totalOptions;
    var nodemodelOptions;



    for(var key in this.getNodeModels()){
        var option = this.getNodeModels()[key];
        nodemodelOptions += '<option value="' + option + '">' + option + '</option>';
      }
     // nodemodelOptions += '<option  class="otherOption"> Other </option>';
      $("#selected_nodemodel").html(nodemodelOptions);
      
    var ufalarmplaceholder;
    ufalarmplaceholder = "Select one or more node model";

    $('#selected_nodemodel').multipleSelect({
      placeholder: ufalarmplaceholder,
    });


    $("#other_nodemodel").change(function getChanged(){
      console.log(" inside getChanged", this.checked);
      if(this.checked){
        $('#text_nodemodel').show();
      }
      else{
        $('#text_nodemodel').hide();
      }

    })
  
    
  },

  // Need it for reset UF alarm
  componentWillReceiveProps: function (nextProps) {    
    this.setState(nextProps.ufalarm);    
  },
  
//   callPreviousItem:function(allItems, currentItem){
//     $('.proximity-previous').keyup();
//     $("#Proximity-grid").data("gridInstance");
//     console.log($("#Proximity-grid").data("gridInstance"));
//     var currentRow = $("#Proximity-grid").data("gridInstance").getSelectedRows();
//     var nextRow =  currentRow[0] -1
//     $("#Proximity-grid").data("gridInstance").setSelectedRows([nextRow])
//   },

//   callNextItem:function(allItems, currentItem){

//     $('.proximity-next').keydown();
//     $("#Proximity-grid").data("gridInstance");
//     console.log($("#Proximity-grid").data("gridInstance"));
//     var currentRow = $("#Proximity-grid").data("gridInstance").getSelectedRows();
//     var nextRow =  currentRow[0]+ 1;
//     $("#Proximity-grid").data("gridInstance").setSelectedRows([nextRow])
//   },


  render: function () {
    var hstyle = {
      overflowY: "auto", overflowX: "hidden", marginBottom: "16px",
      maxHeight: helpers.calcHeight(100, -280) + "px !important"
    };
    var nodemodels = helpers.modelName();
    var ufalarm = this.state;
    var heading = (ufalarm.ufname == "") ? "Add User Friendly Alarm" : (<span><Icon glyph="icon-fontello-right-dir" /> {ufalarm.ufname}</span>);
    var deleteBtnStyle = (ufalarm.mappingid == "") ? { display: "none" } : {};
    var nodemodeltextStyle = { display: "none" };
    console.log('ufalarmform', this);
    var alarmTypeHTML  = this.props.ufalarm.idx == -1 ? (<div >
      <input style={deleteBtnStyle} type="text" className="form-control"  id="alarmtype" ref="alarmtype" onChange={this.handleChange('alarmtype')} />
    </div>) : (<div>
    <input style={deleteBtnStyle} type="text" className="form-control" disabled="disabled" id="selected_ufalarmtype"  ref="selected_ufalarmtype" value={this.state.alarmtype} />
  </div>)
	//   var currentLength = $("#Proximity-grid").data("gridInstance").getData().getLength();
	//   var allItemsLength = this.props.allProximities.length;
	//   var currentRow = $("#Proximity-grid").data("gridInstance").getSelectedRows();
	//   var noNextItem = {}; var noPreviousItem = {};var displayArrows = {};
	//   if(currentLength === allItemsLength ){
	// 	  var firstItem = 0; var lastItem = allItemsLength - 1;
	// 	  for (var a = 0; a < allItemsLength; a++) {
	// 		  if (this.props.allProximities[a]!=null && this.props.proximity!=null && this.props.allProximities[a].pdprofileid === this.props.proximity.pdprofileid){
	// 			  noNextItem = this.props.proximity.idx === lastItem ? {display:"none"}:{};
	// 			  noPreviousItem = this.props.proximity.idx === firstItem ? {display:"none"}:{};
	// 		  }
	// 	  }
	// 	  displayArrows = (this.props.proximity!=null && this.props.proximity.pdprofileid === "" ) ? { display: "none" } : {};
	//   }else{
	// 	  if(this.props.proximity!=null && this.props.proximity.pdprofileid != null){
	// 		  var firstElement = 0;
	// 		  var lastElement = currentLength-1;
	// 		  if(currentRow[0] === firstElement){
	// 			  //display only the next arrow and not the previous arrow
	// 			  if(firstElement+1 === currentLength){ displayArrows={display:"none"};
	// 			  }else{
	// 				  noNextItem={};noPreviousItem={display:"none"}; displayArrows={};
	// 			  }
	// 		  }else if(currentRow[0] === lastElement){
	// 			  //display only the previous arrow and not the next arrow
	// 			  noNextItem={display:"none"}; noPreviousItem={}; displayArrows={};
	// 		  }else{
	// 			  //display both arrows
	// 			  noNextItem={};noPreviousItem={}; displayArrows={};
	// 		  }
	// 	  }
	//   }
	//   if(proximity.pdprofileid === "0"){
	// 	  displayArrows={display:"none"};
    //   }

    return (
      <div>

        <div className="netsense__form__header">
          <h3>{heading}</h3>
        </div>
        <div className="netsense__form__body">

            {/* <span style={displayArrows}>
              <span style={noPreviousItem} className="rubix-icon icon-simple-line-icons-arrow-left proximity-previous" onClick={ () => this.callPreviousItem(this.props.allUsers,this.state.user)}></span>
           </span>

          <span style={displayArrows}>
             <span style={noNextItem} className="rubix-icon icon-simple-line-icons-arrow-right proximity-next" onClick={ () => this.callNextItem(this.props.allUsers,this.state.user)}></span>
          </span> */}

          <form role="form" className="form-horizontal" data-ufalarmid={ufalarm.mappingid} >
            <div style={hstyle}>
              <div className="form-group">
                <label htmlFor="name" className="control-label col-sm-3">Name:</label>
                <div className="col-sm-6">
                  <input type="text" className={(this.props.errors.name)? "form-control orange":"form-control"} id="name" ref="name" value={this.state.ufname} onChange={this.handleChange('ufname')} />
                  <div className="form-error">
                    {this.props.errors.name || ""}
                  </div>
                </div>
              </div>
              <div className="form-group">
                <label htmlFor="description" className="control-label col-sm-3">Description:</label>
                <div className="col-sm-6">
                  <input type="text" className="form-control" id="description" ref="description" value={this.state.description} onChange={this.handleChange('description')} />
                </div>
              </div>

              </div>
              <div className="form-group">
              <label htmlFor="alarmtype" className="control-label col-sm-3">Alarm Type:</label>
              <div className="col-sm-6">
              {alarmTypeHTML}
              </div>
            </div> 
            <div className="form-group">
              <label htmlFor="nodemodel" className="control-label col-sm-3">Node Model:</label>
              <div className="col-sm-6">
              <input style={deleteBtnStyle} type="text" className="form-control" disabled="disabled" id="nodemodels" ref="nodemodels"  value={this.state.nodemodels} onChange={this.handleChange('nodemodels')}/>
              <select multiple="multiple" className="form-control"  id="selected_nodemodel"  ref="selected_nodemodel" value={this.state.selected_nodemodel} onChange={this.handleChange('selected_nodemodel')}>               
              </select>
              </div>
              <div className="col-sm-6" style= {{marginLeft: "290px"}}>
              <label> Other </label>
              <input style={deleteBtnStyle} type="checkbox" className="form-control" id="other_nodemodel" ref="other_nodemodel" />
              <input style={nodemodeltextStyle} type="text" className="form-control" id="text_nodemodel" ref="text_nodemodel"/>
              </div>
            </div>
            <div className="form-group">
              <label htmlFor="displaytopartner" className="control-label col-sm-3">Display to Partner:</label>
              <div className="col-sm-6">
              <select  className="form-control" id="displaytopartner" ref="displaytopartner" value={this.state.displaytopartner} onChange={this.handleChange('displaytopartner')}>  
              <option value="Yes"> Yes </option>
                <option value="No"> No </option>             
              </select>
              </div>
            </div>
            <div className="form-group">
              <label htmlFor="displaytocustomer" className="control-label col-sm-3">Display to Customer:</label>
              <div className="col-sm-6">
              <select  className="form-control" id="displaytocustomer" ref="displaytocustomer" value={this.state.displaytocustomer} onChange={this.handleChange('displaytocustomer')}>  
              <option value="Yes"> Yes </option>
                <option value="No"> No </option>             
              </select>
              </div>
            </div>           
            <div style={{ margin: "20px 0px" }}>
              <div className="col-sm-3 text-right">
                <button type="button" id="pddelete"className="ns-delete-btn" style={deleteBtnStyle} onClick={this.handleReset}>
                  <b>Reset</b></button>
              </div>
              <div className="col-sm-6 text-right">
              <button type="button" id="submit" className="ns-save-btn" onClick={this.handleSubmit}>
                  <b>Save</b></button>
              </div>
            </div>
          </form>
        </div>
      </div>
    );
  }
});

module.exports = UFAlarmform;
