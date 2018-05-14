import classNames from 'classnames';
import { State, Navigation } from 'react-router';
import DataGrid from 'components/datagrid';
import Listeditor  from 'components/listeditor';

var Auditlist = React.createClass({
  getInitialState: function(){
      var component = "Audit";
      var userRole = NSN.userInfo.name;
      var componentColumns = "AuditColumnWidths_"+ userRole;
      var auditStoredWidths = [];
      if(localStorage.getItem(componentColumns)){
          auditStoredWidths = JSON.parse(localStorage.getItem(componentColumns));
      }else{
          // default widths
          auditStoredWidths = [
              {name:"When", field:"when", id:"when",sortable:true, width:150, checked:true, required:true},
              {name:"Target Type", field:"targettype", id:"targettype",sortable:true, width:150, checked:true, required:false},
              {name:"Target ID", field:"targetid", id:"targetid",sortable:true, width:150, checked:true, required:false},
              {name:"User ID", field:"userid", id:"userid",sortable:true, width:150, checked:true, required:false},
              {name:"Activity", field:"activity", id:"activity",sortable:true, width:150, checked:true, required:false},
              {name:"Message", field:"message", id:"message",sortable:true, width:800, checked:true, required:true}
          ]
      }

      return {
          datemin:null,
          datemax:null,
          showListEditor:false,
          auditStoredWidths:auditStoredWidths,
          componentColumns:componentColumns,
          component:component
      }
  },

  propTypes: {
    audits: React.PropTypes.array.isRequired,
    users: React.PropTypes.array.isRequired
  },

  componentDidMount: function(){
    $('#datemin').datetimepicker({
      format : "YYYY-MM-DDTHH:mm:ssZZ",
    });
    $('#datemax').datetimepicker({
      format : "YYYY-MM-DDTHH:mm:ssZZ",
    });
  },
  
 handleChange: function (key) {
    return function (e) {
      var state = {};
      state[key] = e.target.value;
      this.setState(state);
    }.bind(this);
  },
 
  handleSubmit: function(e) {
    e.stopPropagation();
    e.preventDefault();
    ReactBootstrap.Dispatcher.emit("Auditlist.save", Object.assign({},this.state));
  },

    toggleListEditor : function(){
        this.setState({showListEditor:!this.state.showListEditor});
    },

    shouldComponentUpdate: function (nextProps, nextState) {
        return true;
    },


  render: function() {
    var auditsData = this.props.audits;
    var usersData = this.props.users;
    console.log((usersData.length));
    // Convert who to readable format:
    for(var i=0;i<auditsData.length;i++){

      if(usersData.length > 0){
        for(var j=0;j<usersData.length;j++){
          if(auditsData[i].userid == usersData[j].userid){
            console.log(usersData[j].email);
            auditsData[i].userid = usersData[j].email;
          }
        }
      }
      else if (auditsData[i].userid = "507ef3cc-cd2d-46d8-ae6d-7ccf430c1110"){
        console.log("it is sensity user");
        auditsData[i].userid = "sensity_user@sensity.com"
      }
      else if (auditsData[i].userid = "114ad560-a046-11e5-a57f-ef24ae600576"){
        console.log("it is sensity admin ");
        auditsData[i].userid = "sensity_admin@sensity.com"
      }
      else{
        auditsData[i].userid = "sensity_read_only@sensity.com"
      }
    }

    // Convert targetname to readable format:
    for(var k=0;k<auditsData.length;k++){
      var messageData = auditsData[k].message;
      /*if (messageData == "") {
        auditsData[k].targetid = "(n/a)";
      } else { */
        if(messageData != ""){
          var messageObject = JSON.parse(messageData);
          $.each(messageObject, function(key,messageName){
            if(key == "name"){
              auditsData[k].targetid = messageName;
            }
          });
        }
       
      //};
    } 

    return (
      <div id="audit-table-container">
        <h2 className="netsense__table__title"  style={{marginTop:"-4px"}}>Audits</h2>
        <div>
          <form role="form" className="form-horizontal" style={{marginLeft:"30px"}}>
            <div style={{display:"inline-block",paddingRight:"10px"}}>
                <div className="form-group" style={{paddingRight:"20px"}}>
                    <div className='input-group date'>
                        <span style={{paddingRight:"10px",float:"left",lineHeight:"40px"}}> From: </span>
                        <input type='text' className="form-control" style={{width:"230px"}} id="datemin" ref="datemin" 
                            placeholder={NSN.datemin} value={this.state.datemin} onChange={this.handleChange('datemin')} />
                    </div>
                </div>
            </div>
            <div style={{display:"inline-block"}}>
                <div className="form-group">
                    <div className='input-group date'>
                        <span style={{paddingRight:"10px",float:"left",lineHeight:"40px"}}> To: </span>
                        <input type='text' className="form-control" id='datemax' style={{width:"230px"}} ref="datemax" 
                            placeholder={NSN.datemax} value={this.state.datemax} onChange={this.handleChange('datemax')} />
                    </div>
                </div>
            </div>
            <div style={{display:"inline-block", position:"relative", bottom:"30px", left:"10px"}}>
            <button className="ns-save-btn" onClick={this.handleSubmit}> Submit </button>
            </div>
            <div style={{float:"right", position:"relative", bottom:"30px", left:"10px"}}>
              <span onClick={()=>this.toggleListEditor()} className="ns-filter-icon"></span>
            </div>

          </form>
        </div>
          {
              this.state.showListEditor ?
                  <div className="ns-list-editor" style={{top:"80px !important"}}>
                      <Listeditor show={this.state.showListEditor}
                                  component={this.state.component}
                                  componentColumns ={this.state.componentColumns}
                                  handleToggle = {this.toggleListEditor}
                                  columns={this.state.auditStoredWidths}/>
                  </div>:
                  null
          }

          <DataGrid component="Audit"
                  dataArray={this.props.audits}
                  dataID="n/a" 
                  dataIdField="auditid"
                  componentColumns ={this.state.componentColumns}
                  columns={this.state.auditStoredWidths} />
      </div>
      );    
    }
  }
);

module.exports = Auditlist;

