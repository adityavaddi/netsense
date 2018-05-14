import classNames from 'classnames';
import helpers from 'global/utils/helpers';
import Sitetiles from 'components/sites/sitetiles';
import SiteSingleTile from 'components/sites/siteSingleTile';
import SiteDetail from 'components/sites/sitedetail';
import DataUtil from '../service/datautil';
import Header from 'common/headernew';


var Body = React.createClass({
  // siteID 0 implies adding a site
  // siteID -1 implies no site has been selected
  getInitialState: function () {
    return {
      sites: null,
      siteID: NSN.siteID,
      editing: false,
      submitStatus: false
    }
  },

  calcHeight: function (pct, extra) {
    var h = (window && window.document) ? (($(window).height() - 75) * (pct / 100) + extra) : 500;
    return h;
  },

  init: function () {
    if (NSN.customerID == "-1") {
      $("#loadingmsg").html("Please select an Account first.")
      return;
    };
    var that = this;

    this.hstyle = { height: helpers.calcHeight(90, 0) + "px !important" };
    DataUtil.getAll('sites', this.processSitesData);
  },

  processSitesData: function (data) {
    this.setState(DataUtil.assignState('sites', data, this, this.makeSiteObj));
  },


  ///////////////////// Callback Function to get all sites/////////////////

  makeSiteObj: function (site, index) {
    site.idx = index;
    site.street1 = site.street1 || "";
    site.street2 = site.street2 || "";
    site.city = site.city || "No City";
    site.state = site.state || "No State";
    site.postal_code = site.postal_code || "";
    site.country = site.country || "";
    site.latitude = site.latitude || "";
    site.longitude = site.longitude || "";
    return site;
  },

  /////////////////////Callback function to add site//////////////////////////////
  processAddSites: function (data) {
    console.log("Response from Add Site: " + JSON.stringify(data));
    NSN.siteID = data.siteid;
    sessionStorage.setItem("siteID", NSN.siteID);
    helpers.clearSiteContext();
    data.idx = this.state.sites.length;
    var newState = React.addons.update(this.state, {
      sites: { $push: [data] },
      siteID: { $set: data.siteid },
      editing: { $set: false },
      submitStatus: {$set: true }
    });    
    noty({ type: "success", text: 'Site "' + data.name + '" added.' })    
    this.setState(newState);
  },
  /////////////////Callback function to update site///////////////////
  processUpdateSite: function (data, idx) {
    var newState = React.addons.update(this.state, {
      sites: { [idx]: { $set: data } },
      editing: { $set: false },
      submitStatus: {$set: true }
    });    
    noty({ type: "success", text: 'Site "' + data.name + '" updated.' })    
    this.setState(newState);
  },


  getSite: function (siteID) {
    for (var i = 0; i < this.state.sites.length; i++) {
      if (this.state.sites[i].siteid == siteID) {
        return (this.state.sites[i]);
      }
    }
    return null;
  },

  checkDuplicate: function(site_info){
        var duplicate = false;
        for (var i = 0, len = this.state.sites.length; i < len; i++) {
          if ((this.state.sites[i].name === site_info.name) && !(this.state.sites[i].siteid === site_info.siteid))
            duplicate = true;
        }
        if (duplicate) {
          noty({ type: "error", text: 'Site "' + site_info.name + '" already exists.' })
          this.setState({
          submitStatus: true
       });
        } 

        return duplicate;
  },

  componentDidMount: function () {
    this.init();
    var that = this;

    ReactBootstrap.Dispatcher.on("SiteSingleTile.select", function (siteID,siteName) {
      console.log("SiteSingleTile.select - react .bootstrap emit");
      NSN.siteID = siteID;
      sessionStorage.setItem("siteID", NSN.siteID);
      NSN.site = that.getSite(siteID);
      sessionStorage.setItem("site", JSON.stringify(NSN.site));
      NSN.siteName = siteName;
      sessionStorage.setItem("siteName", NSN.siteName);
      helpers.clearSiteContext();
      that.setState({ "siteID": siteID, "siteName":siteName });
    });

    ReactBootstrap.Dispatcher.on("Sitetiles.add", function () {
      that.setState({ "siteID": "0", "editing": true, "submitStatus": true });
    });

    ReactBootstrap.Dispatcher.on("Sitetiles.edit", function (siteID) {
      NSN.siteID = siteID;
      sessionStorage.setItem("siteID", NSN.siteID);
      NSN.site = that.getSite(siteID);
      sessionStorage.setItem("site", JSON.stringify(NSN.site));
      //      React.render(<Header />, $("#navbar").get(0));
      that.setState({ "siteID": siteID, "editing": true, "submitStatus": true });
    });

    ReactBootstrap.Dispatcher.on("Siteform.cancel", function () {
      that.setState({ "editing": false });
    });

    ReactBootstrap.Dispatcher.on("Siteform.reset", function () {
      that.forceUpdate();
    });

    ReactBootstrap.Dispatcher.on("Siteform.lightLevel", function (siteid, level, timeout) {
      $.ajax({
        url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/lightcontrol',
        data: JSON.stringify({ level: parseInt(level), timeout: parseInt(timeout) }),
        contentType: 'application/json',
        method: 'POST',
        "xhrFields": {
          withCredentials: true
        },
        dataType: 'json',
        success: function (data) {
          if (data && data.errors) {
            console.log('lightcontrol error:' + JSON.stringify(data.errors))
          }
        },
        error: function () {
          console.log("lightcontrol ajax failure");
        }
      })
    });


    ReactBootstrap.Dispatcher.on("Siteform.save", function (site_info) {
      site_info.latitude = _.toString(site_info.latitude);
      site_info.longitude = _.toString(site_info.longitude);
       that.setState({
          submitStatus: false
       });
      // false
      var newState = {};

       if ((typeof site_info.siteid == "undefined") || site_info.siteid == "") {
         if(!that.checkDuplicate(site_info)){
        delete site_info.idx;
        delete site_info.siteid;

          DataUtil.addEntity('sites', site_info, that.processAddSites, '');
         }
      } else if(!that.checkDuplicate(site_info)) {
        var idx = helpers.get_idx(that.state.sites, site_info, 'siteid');
        delete site_info.idx;

        DataUtil.updateEntity('site-update', site_info, that.processUpdateSite, idx);

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
        //     var newState = React.addons.update(this.state, { sites: { [idx]: { $set: data } },
        //                                                       editing: { $set : false }});
        //     noty({type:"success", text:'Site "' + data.name + '" updated.'})
        //     this.setState(newState);
        //   }.bind(that),
        //   "error" : function() {
        //     noty({type:"error", text:'Could not update site.'});
        //   }
        // });
      }
    });

    function find_add_comp(key, address_components) {
      var found = _.find(address_components, function (comp) {
        var typeIndex = _.findIndex(comp.types, function (val) {
          return key === val;
        });

        if (typeIndex === -1) {
          return false;
        } else {
          return true;
        }
      });

      if (found) {
        return found.long_name;
      } else {
        return undefined;
      }
    }

    function populate_address(data) {
      var self = this,
        location = data.geometry.location,
        comp = data.address_components;

      var street_num = find_add_comp('street_number', comp),
        route = find_add_comp('route', comp),
        street1 = '';

      if (street_num) {
        if (route) {
          street1 = street_num + ' ' + route;
        } else {
          street1 = street_num;
        }
      } else {
        if (route) {
          street1 = route;
        }
      }

      self.state.street1 = street1;
      self.state.street2 = find_add_comp('subpremise', comp);
      self.state.city = find_add_comp('locality', comp);
      self.state.state = find_add_comp('administrative_area_level_1', comp);
      self.state.postal_code = find_add_comp('postal_code', comp);
      self.state.country = find_add_comp('country', comp);
      self.state.latitude = location.lat();
      self.state.longitude = location.lng();

      self.setState(self.state);
    }

    ReactBootstrap.Dispatcher.on('Siteform.checkAddress', function (self) {
      var geocoder = new google.maps.Geocoder(),
        address = _.join(_.values(_.pick(self.state, ['street1', 'street2', 'city', 'state', 'postal_code', 'country'])), ' ');

      self.addresses.options = [];

      geocoder.geocode({
        address: address
      }, function (results) {
        if (results && results.length > 0) {
          var result = results[0];

          noty({
            type: "success",
            text: 'Found: ' + result.formatted_address
          });

          self.addresses.data = results;
          _.forEach(results, function (o, i) {
            self.addresses.options.push(
              <option key={i} value={i}>{o.formatted_address}</option>
            );
          });

          populate_address.apply(self, [result])
        } else {
          noty({
            type: "error",
            text: 'No match found for: ' + address
          });
          self.setState({});
        }
      });
    });
    
 
    ReactBootstrap.Dispatcher.on("Siteform.suspend", function(site_info){
      var idx = helpers.get_idx(that.state.sites, site_info, 'siteid');
      console.log("Deleting site (idx:" + idx + "; id:" + site_info.siteid + ")");
      delete site_info.idx;
      $.ajax({
        "url" : NSN.apiURL + 'customers/' + NSN.customerID + '/suspended-sites/' + site_info.siteid,
        "type" : "PUT",
        "xhrFields": {
           withCredentials: true
        },
        "data" : JSON.stringify(site_info),
        "dataType" : "json",
        "contentType" : "application/json",
        "processData" : false,
        "success" : function(data) {
          var newState = React.addons.update(this.state, { sites: { $splice: [[idx, 1]] }, siteID: { $set : "-1"}});
          noty({type:"success", text:'Site "' + site_info.name + '" has been suspended.'})
          this.setState({
             editing : false
         });
          NSN.siteID = "-1";
          sessionStorage.setItem("siteID", NSN.siteID);
          helpers.clearSiteContext();
          ReactBootstrap.Dispatcher.emit("Siteform.delete.success", site_info.siteid);
          this.setState(newState);
        }.bind(that),
        "error" : function() {
          noty({type:"error", text:'Could not suspend site.'});
        }
      });
    });

    ReactBootstrap.Dispatcher.on('Siteform.selectAddress', function (self, val) {
      var data = self.addresses.data[val];

      populate_address.apply(self, [data]);
    });

  },

  componentWillUnmount: function () {
    ReactBootstrap.Dispatcher.removeAllListeners("Sitetiles.select");
    ReactBootstrap.Dispatcher.removeAllListeners("Siteform.suspend");
    ReactBootstrap.Dispatcher.removeAllListeners("Sitetiles.add");
    ReactBootstrap.Dispatcher.removeAllListeners("Siteform.save");
    ReactBootstrap.Dispatcher.removeAllListeners("Sitetiles.edit");
    ReactBootstrap.Dispatcher.removeAllListeners("Siteform.cancel");
    ReactBootstrap.Dispatcher.removeAllListeners("Siteform.checkAddress");
    ReactBootstrap.Dispatcher.removeAllListeners("Siteform.selectAddress");
  },
 hide(){
         this.setState({
             editing : false
         })
     },
  render() {
    var hstyle = {height:helpers.calcHeight(90, 0)+"px !important"};
    if (this.state.sites) {
      var panels = (!this.state.editing)?
      (
        <Row>
            <Col>
            <PanelContainer>
              <Panel>
                <PanelBody style={hstyle}>
                   <Sitetiles sites={this.state.sites} siteID={this.state.siteID} />
                </PanelBody>
              </Panel>
            </PanelContainer>
          </Col>
        </Row>
        )
      :(
      <Row>
           <Col>
              <PanelContainer>
            <Panel>
              <PanelBody style={hstyle}>
              <SiteDetail  show={this.state.editing} hide={this.hide} sites={this.state.sites} siteID={this.state.siteID} submitStatus={this.state.submitStatus} />
              <Sitetiles sites={this.state.sites} siteID={this.state.siteID} />
              </PanelBody>
            </Panel>
          </PanelContainer>
           </Col>
      </Row>
      );
      return (
        <Container id='body'>
            <Grid>
            <Row>
              <Col lg={12}>
                {panels}
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
              <Col sm={12} lg={12}>
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