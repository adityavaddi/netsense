import classNames from 'classnames';
import auth from 'global/utils/auth';
import helpers from 'global/utils/helpers';
import SiteSingleTile from './newSiteSingleTile'

import { withRouter } from 'react-router';


var Sitetiles = React.createClass({
  propTypes: {
    sites: React.PropTypes.array.isRequired,
    siteID: React.PropTypes.string.isRequired,
    concise: React.PropTypes.bool
  },

  getDefaultProps: function () {
    return {
      concise: false,
    };
  },

  setSiteContext: function (siteid) {
    var i = 0, found = false;
    while (!found && i < this.props.sites.length) {
      found = (siteid == this.props.sites[i].siteid);
      i++;
    }
    if (found) {
      var site = this.props.sites[--i];
    } else {
      console.log("Could not find site in props.");
      return;
    }
    var s = JSON.stringify(site);
    NSN.site = JSON.parse(s);
    sessionStorage.setItem("site", s);
  },

  handleAdd: function () {
    ReactBootstrap.Dispatcher.emit("Sitetiles.add");
  },

  handleEdit: function (e) {
    e.stopPropagation();
    var tile = $(e.currentTarget).closest(".site-tile");
    NSN.siteID = tile.attr("data-siteid");
    sessionStorage.setItem("siteID", NSN.siteID);
    NSN.siteName = tile.find('h3').html();
    sessionStorage.setItem("siteName", NSN.siteName);
    this.setSiteContext(NSN.siteID);
    ReactBootstrap.Dispatcher.emit("Sitetiles.edit", NSN.siteID, NSN.siteName);
  },

  shouldComponentUpdate: function (nextProps, nextState) {
    // This component does not need to be rerendered unless sites have been added
    return (!helpers.compareArrays(this.props.sites, nextProps.sites));
  },

  transTo: function (e) {
    e.stopPropagation();
    var tile = $(e.currentTarget).closest(".netsense__site__single");

    console.log('Inside the Trans TO ')

    console.log(this.props);
    // var tile = $(e.currentTarget).closest(".site-tile");
    NSN.siteID = tile.attr("data-siteid");
    sessionStorage.setItem("siteID", NSN.siteID);
    NSN.siteName = tile.find('h3').html();
    sessionStorage.setItem("siteName", NSN.siteName);
    helpers.clearSiteContext();
    this.setSiteContext(NSN.siteID);
    this.props.router.push(e.currentTarget.getAttribute("data-target"));
  },

  handleSelect: function (e) {
    e.stopPropagation();
    var tile = $(e.currentTarget).closest(".site-tile");
    NSN.siteID = tile.attr("data-siteid");
    sessionStorage.setItem("siteID", NSN.siteID);
    NSN.siteName = tile.find('h3').html();
    sessionStorage.setItem("siteName", NSN.siteName);
    helpers.clearSiteContext();
    this.setSiteContext(NSN.siteID);
    ReactBootstrap.Dispatcher.emit("Sitetiles.select", NSN.siteID, NSN.siteName)
  },


  xcomponentDidUpdate: function () {
    var that = this;
    $(".site-tile-map").each(function () {
      if ($(this).children().length == 0) {
        new google.maps.Map(this, {
          center: {
            lat: parseFloat($(this).attr("data-lat")),
            lng: parseFloat($(this).attr("data-lng"))
          },
          disableDefaultUI: true,
          zoom: 10,
          scrollwheel: false,
          draggable: false
        });
      };
    });
  },

  handleResize: function () {
     $("#site-tiles-container").css("maxHeight", helpers.calcSitePanelHeight(100, -140) + "px");
  },

  componentWillUnmount: function () {
    $(window).off('resize', this.handleResize);
  },

  xcomponentDidMount: function () {
    var that = this;
    $(".site-tile-map").each(function () {
      new google.maps.Map(this, {
        center: {
          lat: parseFloat($(this).attr("data-lat")),
          lng: parseFloat($(this).attr("data-lng"))
        },
        disableDefaultUI: true,
        zoom: 10,
        scrollwheel: false,
        draggable: false
      });
    });
    $("#site-tiles-container").css("maxHeight", helpers.calcSitePanelHeight(100, -140) + "px");
    $(window).on('resize', this.handleResize);
  },

  render() {

    console.log("this.props.sites", this.props);
    var Addbutton = (<span></span>);
    var AddMessage = (<span></span>);

    var NodeVisibility = (NSN.userInfo.authorization[0].type != "sensity_admin") ? {} : { display: "none" };
    var GroupVisibility = auth.allowed('CAN_READ', 'GroupModel') ? {} : { display: "none" };
    var ScheduleVisibility = auth.allowed('CAN_READ', 'ScheduleModel') ? {} : { display: "none" };

    if (auth.allowed('CAN_CREATE', 'SiteModel')) {
      Addbutton = (
        <button id="add-Site" className="ns-med-btn" onClick={this.handleAdd}>Add site</button>
      );
      if (this.props.siteID != "0") {
        AddMessage = (
          <h3>Use the Add site button to create or add one.</h3>
        );
      }
    };
    var that = this;
    var sites;
    if (this.props.sites.length == 0) {
      sites = (
        <div style={{ padding: "30px 0px 0px 0px" }}>
          <div style={{ textAlign: "center", fontSize: "54px", opacity: "0.5" }}>
            <Icon glyph="icon-fontello-info-circled" />
          </div>
          <div style={{ textAlign: "center" }}>
            <h3>No Sites have been defined for this Account.</h3>
            {AddMessage}
          </div>
        </div>
      )
    } else {
      var sitesList = this.props.sites;
      sites = this.props.sites.map(function (site, index) {
        return <SiteSingleTile site={site} sitesList={sitesList} key={index} />
      })
    };
    if (this.props.concise) {
      return (
        <div>
          {sites}
        </div>
      );
    } else {
      var hstyle = {
        overflowY: "auto", overflowX: "hidden", marginBottom: "16px",
      };
      return (
        <div className="rubix-site-panel-body">
          <div className="netsense__site__header">
          <h2 className="netsense__table__title" style={{ marginTop: "0px"}}><span style={{color:"#333333"}}>Sites</span> {Addbutton}</h2>
          </div>
          <div id="site-tiles-container">
            {sites}
          </div>
        </div>
      );
    }

  }
});

module.exports = withRouter(Sitetiles);