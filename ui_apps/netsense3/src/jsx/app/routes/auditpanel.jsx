import classNames from 'classnames';
import helpers from 'global/utils/helpers';
import Auditlist from 'components/audits/auditlist';
import DataUtil from '../service/datautil';
import Header from 'common/headernew';

import { IonTabContainer,
         IonTabHead,
         IonTabBody,
         IonTab,
         IonTabItem } from 'global/jsx/ion';
import LoremIpsum from 'global/jsx/loremipsum';

var Body = React.createClass({
  getInitialState: function(){
    return {
      audits: null,
      users:null,
      datemin:NSN.datemin,
      datemax:NSN.datemax
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


    //Get userlist:
    DataUtil.getAll("users",this.processUserObject);
    // $.ajax({
    //   url: NSN.apiURL + 'customers/' + NSN.customerID + '/users',
    //   data : '',
    //   method : 'GET',
    //   xhrFields: {
    //      withCredentials: true
    //   },
    //   dataType : 'json',
    //   success : function(data){
    //
    //       console.log("ajax success: " + JSON.stringify(data));
    //       NSN.users = data;
    //       that.setState({
    //         users: data.map(function(user, index) {
    //           user.idx = index;
    //           return user;
    //           })
    //       });
    //   },
    //   error : function(jqXHR, status, error){
    //     console.log("ajax failure (users): " + status + " - " + error);
    //     $("#loadingmsg").html("Cannot retrieve user list.  API call failed.");
    //   }
    // });

    // Get Audits:
    DataUtil.getOne('audits', this.processAuditData);
    // $.ajax({
    //   url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/audits/'  + 'from/' + this.state.datemin + '/to/' + this.state.datemax,
    //   headers: { 'X-Request-Id': Math.round(Math.random() * 100000000000) },
    //   xhrFields: {
    //     withCredentials: true
    //   },
    //   data :'',
    //   method : 'GET',
    //   dataType : 'json',
    //   success : function(data){
    //     if (data.errors) {
    //       console.log("/audits API returned error: " + JSON.stringify(data));
    //       $("#loadingmsg").html("Cannot retrieve audits list. " + "/audits API returned error: " + JSON.stringify(data));
    //     } else {
    //       console.log("ajax success: " + JSON.stringify(data));
    //       var sortedAudit = [];
    //       $.each(data, function(i, obj) {
    //         var l = sortedAudit.length;
    //         sortedAudit.push({ auditid: l, targetid:obj.targetid,when:obj.when,targettype:obj.targettype,userid:obj.userid,activity:obj.activity,message:obj.message});
    //       });
    //
    //
    //       that.setState({audits: sortedAudit});
    //
    //       /*var temp = '';
    //       var count = 0;
    //       for(var i=0;i<sortedAudit.length;i++){
    //
    //         if (sortedAudit[i].targetid === temp) {
    //           console.log("duplicate value");
    //         }
    //         else{
    //           temp = sortedAudit[i].targetid;
    //           count++;
    //           console.log("new value");
    //         }
    //       }
    //       var $appendElem = $("<span style=font-size:25px>" + "<b>" + "-" + "</b>" + temp + "</span>");
    //       if(count == 1){
    //         console.log(temp);
    //         $('#contentWrapper').find('span:contains("' + temp + '")').remove();
    //         $appendElem.appendTo('#contentWrapper');
    //         console.log("There is only one targettype");
    //         $(".dataTables_scrollBody #audit-table tbody tr td:nth-child(3),.dataTables_scrollHeadInner table thead tr th:nth-child(3),.dataTables_scrollHeadInner table thead tr#filterRow th:nth-child(3)").hide();
    //         var table = $('#audit-table').DataTable();
    //         table.columns.adjust().draw();
    //       }
    //       else{
    //         console.log("There are more than one targettype");
    //         $('#contentWrapper').find('span:contains("' + temp + '")').remove();
    //         $(".dataTables_scrollBody #audit-table tbody tr td:nth-child(3),.dataTables_scrollHeadInner table thead tr th:nth-child(3),.dataTables_scrollHeadInner table thead tr#filterRow th:nth-child(3)").show();
    //         var table = $('#audit-table').DataTable();
    //         table.columns.adjust().draw();
    //       } */
    //     };
    //   }.bind(that),
    //   error : function(jqXHR, status, error){
    //     console.log("ajax failure (audits): " + status + " - " + error);
    //     $("#loadingmsg").html("Cannot retrieve Audits.  API reported error: " + error);
    //   }
    // });
  },
    processAuditData: function (data) {

        if (data.errors) {
            console.log("/audits API returned error: " + JSON.stringify(data));
            $("#loadingmsg").html("Cannot retrieve audits list. " + "/audits API returned error: " + JSON.stringify(data));
        } else {
            console.log("ajax success: " + JSON.stringify(data));
            var sortedAudit = [];
            $.each(data, function(i, obj) {
                var l = sortedAudit.length;
                var whenDate = new Date (obj.when).toISOString().substr(0, 19) + new Date().toString().substr(28,5);
                sortedAudit.push({ auditid: l, targetid:obj.targetid,when:whenDate,targettype:obj.targettype,userid:obj.userid,activity:obj.activity,message:obj.message});
            });

            this.setState({audits: sortedAudit});
    }},

    processAuditList: function(data){
        if (data.errors) {
            console.log("/audits API returned error: " + JSON.stringify(data));
            $("#loadingmsg").html("Cannot retrieve audits list. " + "/audits API returned error: " + JSON.stringify(data));
        } else {
            console.log("ajax success: " + JSON.stringify(data));
            var sortedAudit = [];
            $.each(data, function(i, obj) {
                var l = sortedAudit.length;
                if(obj.message){
                  var messageData = obj.message;
                  var messageObject = JSON.parse(messageData);
                   $.each(messageObject, function(key,messageName){
                    if(key == "name"){
                      obj.targetid = messageName;
                    }
                  });
                }
                var whenDate = new Date (obj.when).toISOString().substr(0, 19) + new Date().toString().substr(28,5);
                sortedAudit.push({ auditid: l, targetid:obj.targetid,when:whenDate,targettype:obj.targettype,userid:obj.userid,activity:obj.activity,message:obj.message});
            });
            ReactBootstrap.Dispatcher.emit("Auditlist.update.success", sortedAudit);
            that.setState({audits: sortedAudit});
        };
    },
    processUserObject: function (data) {
      console.log("data in processUserObj###", data)

        NSN.users = data;
        this.setState(DataUtil.assignState("users",data,this,this.makeUsersObj));
    },

     makeUsersObj: function(user, index) {
             user.idx = index;
             return user;
     },
     formatWhenDate: function(date){
      return new Date (date).toISOString().substr(0, 19) + timeZone;
     },
  componentDidMount: function() {
    var that = this;

    /* Set the from and to date of audits with current date and last 24hrs date */
    console.log("###newdate###",new Date().toString());
    var timeZone = new Date().toString().substr(28,5);
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

    ReactBootstrap.Dispatcher.on("Auditlist.save", function(){
      var newDatemin = $('#datemin').val();
      var newDatemax = $('#datemax').val();
      console.log("New Datemin is :" + newDatemin);
      console.log("New Datemax is :" + newDatemax);
      that.state.datemin = newDatemin;
      that.state.datemax = newDatemax;
      //DataUtil.getOne('audits', this.processAuditList);
         if(newDatemin){
            if(newDatemax){
              if(newDatemax>newDatemin){
                  $.ajax({
                  url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/audits/'  + 'from/' + that.state.datemin + '/to/' + that.state.datemax,
                  headers: { 'X-Request-Id': Math.round(Math.random() * 100000000000) },
                  xhrFields: {
                    withCredentials: true
                  },
                  data :'',
                  method : 'GET',
                  dataType : 'json',
                  success : function(data){
                    if (data.errors) {
                      console.log("/audits API returned error: " + JSON.stringify(data));
                      $("#loadingmsg").html("Cannot retrieve audits list. " + "/audits API returned error: " + JSON.stringify(data));
                    } else {
                      console.log("ajax success: " + JSON.stringify(data));
                      var sortedAudit = [];
                      $.each(data, function(i, obj) {
                        var l = sortedAudit.length;

                        if(obj.message){
                          var messageData = obj.message;
                          var messageObject = JSON.parse(messageData);
                           $.each(messageObject, function(key,messageName){
                            if(key == "name"){
                              obj.targetid = messageName;
                            }
                          });
                        }
                        
                        sortedAudit.push({ auditid: l, targetid:obj.targetid,when:obj.when,targettype:obj.targettype,userid:obj.userid,activity:obj.activity,message:obj.message});
                      });
                      ReactBootstrap.Dispatcher.emit("Auditlist.update.success", sortedAudit);
                      that.setState({audits: sortedAudit});
                    };
                  }.bind(that),
                  error : function(jqXHR, status, error){
                    console.log("ajax failure (audits): " + status + " - " + error);
                    $("#loadingmsg").html("Cannot retrieve Audits.  API reported error: " + error);
                  }
                });
              }else{
                noty({ type: "error", text: 'To date cannot be less than From date' });
              }
            }else{
              noty({ type: "error", text: 'To date is missing' });
            } 
      }else{
           noty({ type: "error", text: 'From date is missing' });
      }
    });
  },

  componentWillUnmount: function() {
    ReactBootstrap.Dispatcher.removeAllListeners("Auditlist.save");
  },

  render() {
    var hstyle = {height:helpers.calcHeight(90, 0)+"px !important"};
    if (this.state.audits && this.state.users) {
      var Subpanels = (
        <Col sm={12}>
          <Auditlist audits={this.state.audits} users={this.state.users} />
        </Col>
      );
      return (
        <Container id='body'>
          <Grid>
            <Row>
              <div className="netsense-center-panel">
              <Col sm={12} md={12} lg={12}>
                <PanelContainer>
                  <Panel>
                    <PanelBody>
                      <Row>
                        {Subpanels}
                      </Row>
                    </PanelBody>
                  </Panel>
                </PanelContainer>
              </Col>
              </div>
            </Row>
          </Grid>
        </Container>
      );
    }
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
        <Body/>
      </Container>
    );
  }
}