import classNames from 'classnames';
import helpers from 'global/utils/helpers';

import UFAlarmDetail from 'components/ufalarms/ufalarmdetail';
import UFAlarmList from 'components/ufalarms/ufalarmlist';
import DataUtil from '../service/datautil';

import Header from 'common/headernew';


var Body = React.createClass({
  getInitialState: function () {
    return {
      ufalarm: null,
      ufalarm_ID: null,
      showUFAlarmDetail: false
    }
  },

  init: function () {
    var that = this;


    $.ajax({
          url: NSN.apiURL + 'manage/alarms',
          data : '',
          xhrFields: {
             withCredentials: true
          },
          method : 'GET',
          dataType : 'json',
          success : function(data){
              console.log("ajax success: " + JSON.stringify(data));
              if (data == "") {
                that.setState({ufalarm:[]});
              } else {
                that.setState({
                    ufalarm: data.map(function(ufalarm, index) {
                    ufalarm.idx = index;
                    ufalarm.displaytopartner = ufalarm.displaytopartner ? "Yes" : "No";
                    ufalarm.displaytocustomer = ufalarm.displaytocustomer ? "Yes" : "No";
                    return ufalarm;
                  })
              })
              }
          },
          error : function(jqXHR, status, error){
            console.log("ajax failure (User Friendly): " + status + " - " + error);
            $("#loadingmsg").html("Cannot retrieve User Friendly.  API reported error: " + error);
          }
        });


  },


  componentDidMount: function () {
    this.init();
    var that = this;

    ReactBootstrap.Dispatcher.on("ufalarmlist.select", function (ufalarmID, sortedFlag) {
        that.setState({
            ufalarm_ID: ufalarmID,
            showUFAlarmDetail: true
        });
      
    });

    ReactBootstrap.Dispatcher.on("ufalarmlist.add", function () {
      that.setState({
        "ufalarm_ID": "0",
        showUFAlarmDetail: true
      });
    });



    ReactBootstrap.Dispatcher.on("ufalarmform.reset", function (ufalarm_info) {
      var idx = helpers.get_idx(that.state.ufalarm, ufalarm_info, 'mappingid');
      $.ajax({
        url: NSN.apiURL + 'manage/alarms/' + ufalarm_info.mappingid +'/reset',
        "type" : "PUT",
        "xhrFields": {
           withCredentials: true
        },        
        "dataType" : "json",
        "contentType" : "application/json",
        "processData" : false,
        "success" : function(data) {
          data.displaytopartner = data.displaytopartner ? "Yes" : "No";
          data.displaytocustomer = data.displaytocustomer ? "Yes" : "No";
          if(!data.description) data.description = "";
          noty({type:"success", text:'User Friendly Alarm reset.'})
          ReactBootstrap.Dispatcher.emit('ufalarmform.update.success', data);
          var newState = React.addons.update(this.state, { ufalarm: { [idx]: { $set: data } }});
          this.setState(newState);
        }.bind(that),
        "error" : function() {
          noty({type:"error", text:'Could not reset User Friendly Alarm'});
        }
      })
    });

    ReactBootstrap.Dispatcher.on("ufalarmform.save", function (ufalarm_info) {
      var newState = {};
      if(ufalarm_info.displaytopartner == 'Yes') ufalarm_info.displaytopartner = true;
      else ufalarm_info.displaytopartner = false;
      if(ufalarm_info.displaytocustomer == 'Yes') ufalarm_info.displaytocustomer = true;
      else ufalarm_info.displaytocustomer = false;

      if (ufalarm_info.idx == -1 && ufalarm_info.ufalarm_ID == "0") {
          delete ufalarm_info.mappingid;
          delete ufalarm_info.created;
          delete ufalarm_info.updated;
          delete ufalarm_info.idx;
          delete ufalarm_info.ufalarm_ID;
          $.ajax({
            url: NSN.apiURL + 'manage/alarms',
            "type" : "POST",
            "data" : JSON.stringify(ufalarm_info),
            "xhrFields": {
               withCredentials: true
            },
            "dataType" : "json",
            "contentType": "application/json",
            "processData": false,
            "success" : function(data) {
              console.log("Response from Add ufalarm_info: " + JSON.stringify(data));
              noty({type:"success", text:'User Friendly Alarm added.'});
              data.idx = this.state.ufalarm.length;
              data.displaytopartner = data.displaytopartner ? "Yes" : "No";
              data.displaytocustomer = data.displaytocustomer ? "Yes" : "No";
              ReactBootstrap.Dispatcher.emit('ufalarmform.add.success', data);
              var newState = React.addons.update(this.state, { ufalarm: { $push: [data] }, ufalarm_ID: {$set: data.ufalarmid } });
              this.setState(newState);
              this.setState({
                showUFAlarmDetail: false
            });
            }.bind(that),
            "error" : function() {
              noty({type:"error", text:"Could not add User Friendly Alarm."});
            }
          })
        
      } else{
        var idx = helpers.get_idx(that.state.ufalarm, ufalarm_info, 'mappingid');
        delete ufalarm_info.idx;
        delete ufalarm_info.updated;
        delete ufalarm_info.created;
        $.ajax({
          url: NSN.apiURL + 'manage/alarms/' + ufalarm_info.mappingid,
          "type" : "PUT",
          "xhrFields": {
             withCredentials: true
          },
          "data" : JSON.stringify(ufalarm_info),
          "dataType" : "json",
          "contentType" : "application/json",
          "processData" : false,
          "success" : function(data) {
            noty({type:"success", text:'User Friendly Alarm updated.'})
            if(ufalarm_info.description == ""){
              data.description = "";
            }
            data.displaytopartner = data.displaytopartner ? "Yes" : "No";
            data.displaytocustomer = data.displaytocustomer ? "Yes" : "No";
            ReactBootstrap.Dispatcher.emit('ufalarmform.update.success', data);
            newState = React.addons.update(this.state, { ufalarm: { [idx]: { $set: data } }});
            this.setState(newState);
          }.bind(that),
          "error" : function() {
            noty({type:"error", text:'Could not update User Friendly Alarm'});
          }
        })
      }
    })

  },

  componentWillUnmount: function () {
    ReactBootstrap.Dispatcher.removeAllListeners("ufalarmlist.select");
    ReactBootstrap.Dispatcher.removeAllListeners("ufalarmlist.add");
    ReactBootstrap.Dispatcher.removeAllListeners("ufalarmform.save");
    ReactBootstrap.Dispatcher.removeAllListeners("ufalarmform.reset");
  },

  hideufalarmDetail() {
    this.setState({
      showUFAlarmDetail: false
    });
  },

  render() {
    var hstyle = { height: helpers.calcHeight(90, 0) + "px !important" };
    if (this.state.ufalarm) {
      var Subpanels = (
        <div>
          <div className="netsense-center-panel">
            <Col sm={12} md={12} lg={12}>
              <PanelContainer>
                <Panel>
                  <PanelBody style={hstyle} >
                    <UFAlarmDetail show={this.state.showUFAlarmDetail} ufalarm={this.state.ufalarm} hide={this.hideufalarmDetail} ufalarm_ID={this.state.ufalarm_ID}/>
                    <UFAlarmList ufalarm={this.state.ufalarm} ufalarm_ID={this.state.ufalarm_ID} />
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
                  <PanelBody style={hstyle} >
                    <h2 id="loadingmsg" style={{ paddingTop: "16%", textAlign: "center" }}>Loading...</h2>
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