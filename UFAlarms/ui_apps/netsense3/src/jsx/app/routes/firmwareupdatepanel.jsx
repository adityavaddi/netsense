import classNames from 'classnames';
import {State, Navigation} from 'react-router';

import helpers from 'global/utils/helpers';
import Otalist from 'components/ota/otalist';
import OtaDetail from 'components/ota/otadetail';

import Header from 'common/headernew';


var Body = React.createClass({
  mixins: [State, Navigation],
  getInitialState: function(){
    return {
      otas: null,
      firmwares:null,
      showOtaDetail: false,
      selectedOta: null,
      otaID: "-1"
    }
  },

  calcHeight: function(pct, extra) {
    var h = (window&&window.document)?(($(window).height() - 75) * (pct/100) + extra):500;
    return h;
  },

  init: function() {
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

    $.ajax({
      url: NSN.apiURL + 'firmwares',  
      data : '',
      xhrFields: {
         withCredentials: true
      },
      method : 'GET',
      dataType : 'json',
      success : function(data){
        console.log("ajax success firmware list: " + JSON.stringify(data));
        if (data == "") {
          that.setState({firmwares:[]});
        } else {
          that.setState({
            firmwares: data.map(function(firmware, index) {
              firmware.idx = index;
              return firmware;
            })
          })
        }
      },
      error : function(jqXHR, status, error){
        console.log("ajax failure (firmwares): " + status + " - " + error);
        $("#loadingmsg").html("Cannot retrieve Firmware list.  API reported error: " + error);
      }
    });

    $.ajax({
          url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/ota_status', 
          data : '',
          xhrFields: {
             withCredentials: true
          },
          method : 'GET',
          dataType : 'json',
          success : function(data){
            console.log("ajax success: " + JSON.stringify(data));
            if (data == "") {
              /*var fakedata = helpers.getFakeOtaList();
              
              that.setState({
                otas: fakedata.map(function(ota, index) {
                  ota.otaid = ota.jobid;
                  ota.when = new Date(ota.when).toString();
                  ota.model = helpers.modelName(ota.model);
                  ota.idx = index;
                  return ota;
                })
              }) */
             
              that.setState({otas:[]});
            } else {
              that.setState({
                otas: data.map(function(ota, index) {
                  ota.otaid = ota.jobid;
                  ota.when = new Date(ota.when).toString();
                  ota.model = helpers.modelName(ota.model);
                  ota.idx = index;
                  return ota;
                })
              })
            }
          },
          error : function(jqXHR, status, error){
            console.log("ajax failure (otas): " + status + " - " + error);
            $("#loadingmsg").html("Cannot retrieve Ota status.  API reported error: " + error);
          }
        });
  },

  componentDidMount: function() {
    this.init();
    var that = this;

    ReactBootstrap.Dispatcher.on("Otalist.select",function(otaID){
      //if(otaID != NSN.otaID){
          console.log(otaID);
          NSN.otaID = otaID;
          sessionStorage.setItem("otaID", NSN.otaID);
          that.setState({"otaID":otaID});
          that.setState({
              "otaID": otaID,
              showOtaDetail: true
          });
          console.log('selected otaID', otaID);
      //}
    });


    ReactBootstrap.Dispatcher.on("Otaform.fast",function(job){
      var jobData = job[0];
      if ('job_info' in jobData){
        var currentjobid = job[0].job_info.jobid;
      }
      else{
        var currentjobid = job[0][0].jobid;
      }
      $.ajax({
        url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/ota_status/'  + currentjobid + '/faster',
        type: 'POST',
        xhrFields: {
            withCredentials: true
        },
        data: '',
        dataType: 'json',
        contentType: 'application/json',
        success: function (data) {
            console.log('Response from faster the job process: ' + JSON.stringify(data));
            noty({
                type: 'success',
                text: 'Job "' + currentjobid + '" speed has been updated to fast.'
            })

        },
        error: function () {
            noty({
                type: 'error',
                text: 'Could not update the job "' + currentjobid + '" speed to fast'
            });
        }
      }) 
    });    

    ReactBootstrap.Dispatcher.on("Otaform.slow",function(job){
      var jobData = job[0];
      if ('job_info' in jobData){
        var currentjobid = job[0].job_info.jobid;
      }
      else{
        var currentjobid = job[0][0].jobid;
      }
      $.ajax({
        url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/ota_status/'  + currentjobid + '/slower',
        type: 'POST',
        xhrFields: {
            withCredentials: true
        },
        data: '',
        dataType: 'json',
        contentType: 'application/json',
        success: function (data) {
            console.log('Response from slower the job process: ' + JSON.stringify(data));
            noty({
                type: 'success',
                text: 'Job "' + currentjobid + '" speed has been updated to slow.'
            })

        },
        error: function () {
            noty({
                type: 'error',
                text: 'Could not update the job "' + currentjobid + '" speed to slow'
            });
        }
      }) 
    });    

    ReactBootstrap.Dispatcher.on("Otaform.stop",function(job){
      var jobData = job[0];
      if ('job_info' in jobData){
        var currentjobid = job[0].job_info.jobid;
      }
      else{
        var currentjobid = job[0][0].jobid;
      }
      $.ajax({
        url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/ota_status/'  + currentjobid + '/stop',
        type: 'POST',
        xhrFields: {
            withCredentials: true
        },
        data: '',
        dataType: 'json',
        contentType: 'application/json',
        success: function (data) {
            console.log('Response from stop the job process: ' + JSON.stringify(data));
            noty({
                type: 'success',
                text: 'Job "' + currentjobid + '" has been stopped successfully'
            })

        },
        error: function () {
            noty({
                type: 'error',
                text: 'Could not stop the job "' + currentjobid + '" successfully'
            });
        }
      }) 
    });    

  },

  componentWillUnmount: function() {
    ReactBootstrap.Dispatcher.removeAllListeners("Otalist.select");
    ReactBootstrap.Dispatcher.removeAllListeners("Otaform.stop");
    ReactBootstrap.Dispatcher.removeAllListeners("Otaform.slow");
    ReactBootstrap.Dispatcher.removeAllListeners("Otaform.fast");
  },

  hideOtaDetail() {
    this.setState({
      showOtaDetail: false
    });
  },

  render() {
    var hstyle = {height:helpers.calcHeight(90, 0)+"px !important"};
    if (this.state.otas) {
      var Subpanels = (
        <div>
          <div className="netsense-center-panel">
            <Col sm={12} md={12} lg={12}>
              <PanelContainer>
                <Panel>
                  <PanelBody style={hstyle} >
                    <OtaDetail firmwares={this.state.firmwares} otas={this.state.otas} otaID={this.state.otaID} hide={this.hideOtaDetail} show={this.state.showOtaDetail}  />
                    <Otalist otas={this.state.otas} otaID={this.state.otaID} />
                  </PanelBody>
                </Panel>
              </PanelContainer>
            </Col>
          </div>
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
                    <h2 id="loadingmsg" style={{paddingTop:"16%",textAlign:"center"}}>Loading..</h2>
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
