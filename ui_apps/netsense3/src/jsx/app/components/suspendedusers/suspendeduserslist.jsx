import classNames from 'classnames';
import auth from 'global/utils/auth';
import { withRouter } from 'react-router';
import DataGrid from 'components/datagrid';
import Listeditor  from 'components/listeditor';

var Suspendeduserslist = React.createClass({
    getInitialState: function(){
        var component = "Suspendedusers";
        var userRole = NSN.userInfo.name;
        var componentColumns = "SuspendedusersColumnWidths_"+ userRole;

        var suspendedUsersStoredWidths = [];
        if(localStorage.getItem(componentColumns)){
            suspendedUsersStoredWidths = JSON.parse(localStorage.getItem(componentColumns));
        }else{
            // default widths
            suspendedUsersStoredWidths = [
                {name:"Name", field:"name", id:"name", sortable:true, width:100,checked:true, required:true},
                    {name:"Title", field:"title", id:"title",sortable:true, width:150,checked:true, required:true},
                    {name:"Roles", field:"roles", id:"roles", sortable:true, width:150,checked:true, required:true},
                    {name:"Email", field:"email", id:"email",sortable:true, width:150,checked:true, required:true},
                    {name:"Phone", field:"phone", id:"phone", sortable:true, width:150,checked:true, required:true}

            ]
        }
        return {
            showListEditor:false,
            suspendedUsersStoredWidths:suspendedUsersStoredWidths,
            componentColumns:componentColumns,
            component:component
        }
    },
  propTypes: {
    suspendedusers: React.PropTypes.array.isRequired,
    userID: React.PropTypes.string,
  },

    toggleListEditor : function(){
        this.setState({showListEditor:!this.state.showListEditor});

    },

  handleActiveUsers: function(){
    this.props.router.push("/app/usermanagementpanel")
  },

  shouldComponentUpdate: function(nextProps,nextState){
    // This component does not need to be rerendered unless users have been added or changed
    // var changed = this.props.suspendedusers !== nextProps.suspendedusers;
    // console.log("suspeneduserlist.jsx shouldComponentUpdate ",changed);
    // return changed;
      return true;
  },

  render() {

      return (

        <div id="user-table-container">

          <div>
            <div style={{float:"left"}}>
              <h2 className="netsense__table__title" style={{marginTop:"-4px"}}>Suspended Users
              </h2>
            </div>

            <div style={{float:"right"}}>
              <button className="ns-form-btn" style={{float:"right",marginRight:"24px",marginLeft:"15px"}} onClick={this.handleActiveUsers} title="View Active Users">
                View Active Users
              </button>
              <span onClick={()=>this.toggleListEditor()} className="ns-filter-icon"
                    style={{display:"inline-block", cursor:"pointer", float:"right",position:"relative",top:"7px"}}></span>
            </div>
            <div style={{clear:"both"}}></div>
          </div>

            {
                this.state.showListEditor ?
                    <div className="ns-list-editor">
                        <Listeditor show={this.state.showListEditor}
                                    component={this.state.component}
                                    handleToggle = {this.toggleListEditor}
                                    componentColumns ={this.state.componentColumns}
                                    columns={this.state.suspendedUsersStoredWidths}/>
                    </div>:null
            }

          <DataGrid component={this.state.component} roles={this.props.roles}
            dataArray={this.props.suspendedusers}
            dataID={this.props.userID}
            dataIdField="userid"
            componentColumns ={this.state.componentColumns}
            columns={this.state.suspendedUsersStoredWidths} />
        </div>

      );
  }
});

module.exports = withRouter(Suspendeduserslist);
