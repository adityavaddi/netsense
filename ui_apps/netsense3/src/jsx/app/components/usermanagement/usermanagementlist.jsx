import classNames from 'classnames';
import auth from 'global/utils/auth';
import { Link, withRouter } from 'react-router';
import DataGrid from 'components/datagrid';
import Listeditor  from 'components/listeditor';

var Usermanagementlist = React.createClass({
    getInitialState: function(){
        var component = "Usermanagement";
        var userRole = NSN.userInfo.name;
        var componentColumns = "UsermanagementColumnWidths_"+ userRole;

        var userManagementStoredWidths = [];
        if(localStorage.getItem(componentColumns)){
            userManagementStoredWidths = JSON.parse(localStorage.getItem(componentColumns));
        }else{
            // default widths
            userManagementStoredWidths = [
                {name:"Name", field:"name", id:"name", sortable:true, width:100, checked:true, required:true},
                {name:"Title", field:"title", id:"title", sortable:true, width:150, checked:true, required:false},
                {name:"Roles", field:"roles", id:"roles", sortable:true, width:150, checked:true, required:true},
                {name:"Email", field:"email", id:"email", sortable:true, width:150, checked:true, required:false},
                {name:"Phone", field:"phone", id:"phone", sortable:true, width:150, checked:true, required:false}
            ]
        }
        return {
            showListEditor:false,
            userManagementStoredWidths:userManagementStoredWidths,
            componentColumns:componentColumns,
            component:component
        }
    },
  propTypes: {
    users: React.PropTypes.array.isRequired,
    userID: React.PropTypes.string,
  },

  handleAdd: function() {
    ReactBootstrap.Dispatcher.emit("Usermanagementlist.add");
  },

  handleSuspendedUsers: function(){
    this.props.router.push("/app/suspendeduserspanel")
  },

    toggleListEditor : function(){
        this.setState({showListEditor:!this.state.showListEditor});
    },

  shouldComponentUpdate: function(nextProps,nextState){
    // var changed = this.props.users !== nextProps.users;
    // console.log("userlist.jsx shouldComponentUpdate ",changed);
    // return changed;
      return true;
  },

  render() {
    var Addbutton = (<span></span>);
    if (auth.allowed('CAN_CREATE_ORG_USER','UserModel')) {
       Addbutton = (
        <button id="addUser" title="Add User" className="ns-big-btn" onClick={this.handleAdd}><b>Add user</b></button>
        );
    };

    return (

      <div id="user-table-container">
          {auth.allowed('CAN_CHANGE_ORG_USER', 'UserModel') &&
            (

            <div>
              <div>
                <div style={{float:"left"}}>
                  <h2 className="netsense__table__title" style={{marginTop:"-4px"}}>Active Users 
                  </h2>
                </div>
                <div style={{float:"right"}}>
                  {Addbutton}
                </div>
                <div style={{clear:"both"}}></div>
              </div>

              <div style={{paddingTop:"10px"}}>
                <div style={{float:"right"}}>
                  <button id="suspendedUsers" style={{float:"right",marginRight:"24px",marginLeft:"15px"}} className="ns-form-btn" onClick={this.handleSuspendedUsers} title="View Suspended Users">
                    View Suspended Users
                  </button>
                  <span onClick={()=>this.toggleListEditor()} className="ns-filter-icon"
                      style={{display:"inline-block", cursor:"pointer", float:"right",position:"relative",top:"7px"}}></span>
                </div>
                <div style={{clear:"both"}}></div>
              </div>
            </div>
            )
          }

         

          {
              this.state.showListEditor ?
                  <div className="ns-list-editor" style={{top:"126px !important"}}>
                      <Listeditor show={this.state.showListEditor}
                                  component={this.state.component}
                                  handleToggle = {this.toggleListEditor}
                                  componentColumns ={this.state.componentColumns}
                                  columns={this.state.userManagementStoredWidths}/>
                  </div>:null
          }
          <DataGrid component={this.state.component} componentName="User" roles={this.props.roles}
            dataArray={this.props.users}
            dataID={this.props.userID}
            dataIdField="userid"
            componentColumns ={this.state.componentColumns}
            columns={this.state.userManagementStoredWidths} />

      </div>
      );
  }
});

module.exports = withRouter(Usermanagementlist);
