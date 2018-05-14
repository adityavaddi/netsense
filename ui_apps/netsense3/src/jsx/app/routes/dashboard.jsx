import classNames from 'classnames';
import Notificationlist from 'components/notifications/notificationlist';
import Sitelist from 'components/sites/sitelist';
import Auditlist from 'components/audits/auditlist';
import Userpersonalist from 'components/userPersona/userPersonalist';

import { IonTabContainer,
         IonTabHead,
         IonTabBody,
         IonTab,
         IonTabItem } from 'global/jsx/ion';
import LoremIpsum from 'global/jsx/loremipsum';
import helpers from 'global/utils/helpers';
import Header from 'common/headernew';

var Body = React.createClass({
  getInitialState: function(){
    return {
      notifications: null,
      notificationID: NSN.notificationID,
      users:null,
      audits: null,
      datemin:NSN.datemin,
      datemax:NSN.datemax,
      sites: null,
      siteID: NSN.siteID,
    }
  },

  calcHeight: function(pct, extra) {
    var h = (window&&window.document)?(($(window).height() - 75) * (pct/100) + extra):500;
    return h;
  },

  init() {
    var that = this;

    if (NSN.customerID=="-1" && NSN.siteID=="-1") {
      $("#loadingmsg").html("Please select an Account and a Site first.")
      return;
    } else {
      if (NSN.customerID=="-1") {
        $("#loadingmsg").html("Please select an Account first.")
        return;
      } else {
        if (NSN.siteID=="-1") {
          $("#loadingmsg").html("Please select a Site first.")
          return;
        }
      }
    };
    

    /*******  Ajax call for Dashboard:  *******/

    // Ajax call for Sites:
    $.ajax({
      url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites', 
      data : '',
      method : 'GET',
      xhrFields: {
         withCredentials: true
      },
      dataType : 'json',
      success : function(data){
        if (data.errors) {
          console.log("ajax error");
          $("#loadingmsg").html("Cannot retrieve site list.  API returned: " + JSON.stringify(data));             
        } else {
          console.log("ajax success: " + JSON.stringify(data));
          that.setState({
            sites: data.map(function(site, index) {
              site.idx = index;
              site.name = site.name.replace(/([a-z])([A-Z])/g, '$1 $2');
              return site;
            })
          })
        };
      },
      error : function(){
        console.log("ajax failure");
        $("#loadingmsg").html("Set customerID and siteID to view dashboard.");
      }
    }); 


    // Ajax call for notifications:
    $.ajax({
        url: NSN.apiURL + 'customers/' + NSN.customerID + '/notifications',
      xhrFields: {
        withCredentials: true
      },
      data : '',
      method : 'GET',
      dataType : 'json',
      success : function(data){
        if (data.errors) {
          console.log("ajax error");
          that.setState({ 
            notifications: helpers.getNotificationList().map(function(notification, index) {
              notification.idx = index;
              return notification;
          })});             
        } else {
          console.log("ajax success: " + JSON.stringify(data));
              
          that.setState({
            notifications: data.map(function(notification, index) {
              notification.idx = index;
              notification.name = notification.name.replace(/([a-z])([A-Z])/g, '$1 $2');
              return notification;
            })
          })
        };
      },
      error : function(){
        console.log("ajax failure");
        that.setState({
          notifications: helpers.getNotificationList().map(function(notification, index) {
            notification.idx = index;
            return notification;            
        })});
      }
    });

      //Get userlist:

    $.ajax({
      url: NSN.apiURL + 'customers/' + NSN.customerID + '/users', 
      data : '',
      method : 'GET',
      xhrFields: {
         withCredentials: true
      },
      dataType : 'json',
      success : function(data){
        
          console.log("ajax success: " + JSON.stringify(data));
          NSN.users = data;
          that.setState({
            users: data.map(function(user, index) {
              user.idx = index;
              return user;
              })
          });
      },
      error : function(jqXHR, status, error){
        console.log("ajax failure (users): " + status + " - " + error);
        $("#loadingmsg").html("Cannot retrieve user list.  API call failed.");
      }
    });

        
    // Ajax calling for Activity Logs:
    $.ajax({
      url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/audits/'  + 'from/' + that.state.datemin + '/to/' + that.state.datemax,
      xhrFields: {
        withCredentials: true
      },
      data :'',
      method : 'GET',
      dataType : 'json',
      success : function(data){
        console.log("ajax success: " + JSON.stringify(data));
        var sortedAudit = [];
        $.each(data, function(i, obj) {
          sortedAudit.push({targetid:obj.targetid,when:obj.when,targettype:obj.targettype,userid:obj.userid,activity:obj.activity,message:obj.message});
        }); 

        that.setState({
          audits: sortedAudit.map(function(sortedAudit, index) {
            sortedAudit.idx = index;
            return sortedAudit;
          })
        });         
      },
      error : function(jqXHR, status, error){
        console.log("ajax failure (audits): " + status + " - " + error);
        $("#loadingmsg").html("Cannot retrieve audit list.  API reported error.");
      }
    });
  },
 
  componentDidMount() {
    var that = this;

     /* Set the from and to date of audits with current date and last 24hrs date */
    var timeZone = new Date().toString().substr(28,5);
    console.log(timeZone);
    var newdate = new Date().toISOString().substr(0, 19) + timeZone;
    var Yesterday = new Date(new Date().getTime() - (24 * 60 * 60 * 1000));
    var olddate = Yesterday.toISOString().substr(0,19) + timeZone;
    console.log(newdate);
    console.log(olddate);
    NSN.datemin = olddate;
    NSN.datemax = newdate;
    that.state.datemin = NSN.datemin;
    that.state.datemax = NSN.datemax;

    that.init(that.state.datemin,that.state.datemax);

  },

  render() {
    var hstyle = {height:helpers.calcHeight(90, 0)+"px !important"};
    if (this.state.notifications && this.state.audits && this.state.users && this.state.sites && NSN.userInfo) {
      var Subpanels = (
            <div className="dashboardWrapper">
              <Col sm={4}>
                <PanelContainer>
                  <Panel>
                    <PanelBody>
                      <Userpersonalist concise={true} />
                    </PanelBody>
                  </Panel>
                </PanelContainer>
              </Col>
              <Col sm={4}>
                <PanelContainer>
                  <Panel>
                    <PanelBody>
                      <Notificationlist notifications={this.state.notifications} notificationID={this.state.notificationID} concise={true} /> 
                    </PanelBody>
                  </Panel>
                </PanelContainer>
              </Col>
              <Col sm={4}>
                <PanelContainer>
                  <Panel>
                    <PanelBody>
                     <Sitelist sites={this.state.sites} siteID={this.state.siteID} concise={true} /> 
                    </PanelBody>
                  </Panel>
                </PanelContainer>
              </Col>
              <Col sm={12}>
                <PanelContainer>
                  <Panel>
                    <PanelBody>
                       <Auditlist audits={this.state.audits} users={this.state.users} concise={true} />
                    </PanelBody>
                  </Panel>
                </PanelContainer>
              </Col>
            </div>
            );
      return (
        <Container id='body'>
          <Grid>
            <Row>
              <Col sm={12}>
                <Row>
                  {Subpanels}
                </Row>
              </Col>
            </Row>
          </Grid>
        </Container>
      );
    };
      return (
        <Container id='body'>
          <Grid>
            <Row>
              <Col sm={12}>
                <PanelContainer>
                  <Panel>
                    <PanelBody style={hstyle}>
                        <h2 id="loadingmsg" style={{paddingTop:"16%",textAlign:"center"}}>Loading...</h2>
                    </PanelBody>
                  </Panel>
                </PanelContainer>
              </Col>
            </Row>
          </Grid>
        </Container>
      );
  }
});

export default class extends React.Component {
  render() {
    var classes = classNames({
      'container-open': this.props.open
    });

    return (
      <Container id='container' className={classes}>
        <Header />
        <Body />
      </Container>
    );
  }
}
