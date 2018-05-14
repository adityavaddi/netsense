import classNames from 'classnames';
import SidebarMixin from 'global/jsx/sidebar_component';
import helpers from 'global/utils/helpers';
import Sitelist from 'components/sites/sitelist';
import SiteDetail from 'components/sites/sitedetail';
import DataUtil from '../service/datautil';

import Header from 'common/header';
import Sidebar from 'common/sidebar';
import Footer from 'common/footer';


var Body = React.createClass({
  // siteID 0 implies adding a site
  // siteID -1 implies no site has been selected
  getInitialState: function () {
    return {
      sites: null,
      siteID: NSN.siteID
    }
  },

  calcHeight: function (pct, extra) {
    var h = (window && window.document) ? (($(window).height() - 75) * (pct / 100) + extra) : 500;
    return h;
  },

  init: function () {
    if (NSN.customerID == "-1") {
      $("#loadingmsg").html("Please select an Account first.");
      return;
    }
    var that = this;
    this.hstyle = { height: helpers.calcHeight(90, 0) + "px !important" };

    DataUtil.getAll('sites', that.processGetSites);
    // $.ajax({
    //       url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites', 
    //       data : '',
    //       method : 'GET',
    //       xhrFields: {
    //          withCredentials: true
    //       },
    //       dataType : 'json',
    //       success : function(data){
    //         if (data.errors) {
    //           console.log("/customers/" + NSN.customerID + "/sites API returned error: " + JSON.stringify(data));
    //           $("#loadingmsg").html("Cannot retrieve site list.  API returned: " + JSON.stringify(data));             
    //         } else {
    //           console.log("ajax success: " + JSON.stringify(data));
    //           that.setState({
    //             sites: data.map(function(site, index) {
    //               site.idx = index;
    //               site.name = site.name.replace(/([a-z])([A-Z])/g, '$1 $2');
    //               site.street1 = site.street1 || "";
    //               site.street2 = site.street2 || "";
    //               site.city = site.city || "";
    //               site.state = site.state || "";
    //               site.postal_code = site.postal_code || "";
    //               site.country = site.country || "";
    //               site.latitude = site.latitude || "";
    //               site.longitude = site.longitude || "";
    //               return site;
    //               })
    //           })
    //         }
    //       },
    //       error : function(jqXHR, status, error){
    //         console.log("ajax failure (sites): " + status + " - " + error);
    //         $("#loadingmsg").html("Cannot retrieve site list.  API call failed.");
    //       }
    //     });
  },

  getSite: function (siteID) {
    for (var i = 0; i < this.state.sites.length; i++) {
      if (this.state.sites[i].siteid == siteID) {
        return (this.state.sites[i]);
      }
    }
    return null;
  },
  ////////////////////Callback function to get sites/////////////
  processGetSites: function (data) {
    if (data.errors) {
      console.log("/customers/" + NSN.customerID + "/sites API returned error: " + JSON.stringify(data));
      $("#loadingmsg").html("Cannot retrieve site list.  API returned: " + JSON.stringify(data));
    } else {
      console.log("ajax success: " + JSON.stringify(data));
      this.setState(DataUtil.assignState('sites', data, this, this.makeSiteObj))
    }
  },
  makeSiteObj: function (site, index) {
    site.idx = index;
    site.name = site.name.replace(/([a-z])([A-Z])/g, '$1 $2');
    site.street1 = site.street1 || "";
    site.street2 = site.street2 || "";
    site.city = site.city || "";
    site.state = site.state || "";
    site.postal_code = site.postal_code || "";
    site.country = site.country || "";
    site.latitude = site.latitude || "";
    site.longitude = site.longitude || "";
    return site;
  },
  /////////////////Callback function to add sites//////////
  processAddSites: function (data) {
    console.log("Response from Add Site: " + JSON.stringify(data));
    NSN.siteID = data.siteid;
    data.idx = this.state.sites.length;

    var newState = React.addons.update(that.state, { sites: { $push: [data] }, siteID: { $set: data.siteid } });
    noty({ type: "success", text: 'Site "' + data.name + '" added.' });
    this.setState(newState);
  },
  ////////////////Callback function to update sites/////////////
  processUpdateSites: function (data, site_idx) {
    var newState = React.addons.update(that.state, { sites: { [site_idx]: { $set: data } } });
    data.idx = site_idx;
    noty({ type: "success", text: 'Site "' + data.name + '" updated.' });
    this.setState(newState);
  },

  componentDidMount: function () {
    this.init();
    var that = this;
    ReactBootstrap.Dispatcher.on("Sitelist.select", function (siteID) {
      NSN.siteID = siteID;
      NSN.site = that.getSite(siteID);
      that.setState({ "siteID": siteID });
      //      siteID=100;
    });
    ReactBootstrap.Dispatcher.on("Sitelist.add", function () {
      that.setState({ "siteID": "0" });
    });
    ReactBootstrap.Dispatcher.on("Siteform.save", function (site_info) {
      var newState = {};
      if (site_info.siteid == "") {
        delete site_info.idx;
        delete site_info.siteid;

        DataUtil.addEntity('sites', site_info, that.processAddSites, '');
        // $.ajax({
        //   "url" : NSN.apiURL + 'customers/' + NSN.customerID + '/sites',
        //   "type" : "POST",
        //   "xhrFields": {
        //      withCredentials: true
        //   },
        //   "data" : JSON.stringify(site_info),
        //   "dataType" : "json",
        //   "contentType": "application/json",
        //   "success" : function(data) {
        //     console.log("Response from Add Site: " + JSON.stringify(data));
        //     NSN.siteID = data.siteid;
        //     data.idx = this.state.sites.length;

        //     var newState = React.addons.update(that.state, { sites: { $push : [data] }, siteID: { $set : data.siteid } });
        //     noty({type:"success", text:'Site "' + data.name + '" added.'});
        //     this.setState(newState);
        //   }.bind(that),
        //   "error" : function() {
        //     noty({type:"error", text:"Could not add site."});
        //   }
        // })
      } else {
        var site_idx = site_info.idx;
        delete site_info.idx;

        DataUtil.updateEntity('site-update', site_info, that.processUpdateSites, site_idx);
        // $.ajax({
        //   "url" : NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + site_info.siteid,
        //   "type" : "POST",
        //   "xhrFields": {
        //      withCredentials: true
        //   },
        //   "data" : JSON.stringify(site_info),
        //   "dataType" : "json",
        //   "contentType" : "application/json",
        //   "success" : function(data) {
        //     var newState = React.addons.update(that.state, { sites: { [site_idx]: { $set: data } }});
        //     data.idx = site_idx;
        //     noty({type:"success", text:'Site "' + data.name + '" updated.'});
        //     this.setState(newState);
        //   }.bind(that),
        //   "error" : function() {
        //     noty({type:"error", text:'Could not update site.'});
        //   }
        // })
      }
    })

  },

  componentWillUnmount: function () {
    ReactBootstrap.Dispatcher.removeAllListeners("Sitelist.select");
    ReactBootstrap.Dispatcher.removeAllListeners("Sitelist.add");
    ReactBootstrap.Dispatcher.removeAllListeners("Siteform.save");
  },

  render() {
    var hstyle = { height: helpers.calcHeight(90, 0) + "px !important" };
    if (this.state.sites) {
      return (
        <Container id='body'>
          <Grid>
            <Row>
              <Col sm={12}>
                <Row>
                  <div>
                    <Col sm={5}>
                      <PanelContainer>
                        <Panel>
                          <PanelBody>
                            <Sitelist sites={this.state.sites} siteID={this.state.siteID} />
                          </PanelBody>
                        </Panel>
                      </PanelContainer>
                    </Col>
                    <Col sm={7}>
                      <PanelContainer>
                        <Panel>
                          <PanelBody style={this.hstyle}>
                            <SiteDetail sites={this.state.sites} siteID={this.state.siteID} />
                          </PanelBody>
                        </Panel>
                      </PanelContainer>
                    </Col>
                  </div>
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

@SidebarMixin
export default class extends React.Component {
  render() {
    var classes = classNames({
      'container-open': this.props.open
    });

    return (
      <Container id='container' className={classes}>
        <Sidebar />
        <Header />
        <Body />
        <Footer />
      </Container>
    );
  }
}