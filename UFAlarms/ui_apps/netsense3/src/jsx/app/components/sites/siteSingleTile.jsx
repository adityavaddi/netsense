import classNames from 'classnames';
import auth from 'global/utils/auth';
import helpers from 'global/utils/helpers';
import { State, Navigation } from 'react-router';
import { withRouter } from 'react-router';


var SiteSingleTile = React.createClass({
  getInitialState() {
    return {
      open: false,
      hovered: null,
      insubnav: false
    };
  },

  propTypes: {
    sitesList: React.PropTypes.array.isRequired,
    siteID: React.PropTypes.string.isRequired,
    concise: React.PropTypes.bool
  },


  transTo: function (e) {
    e.stopPropagation();
    var tile = $(e.currentTarget).closest(".netsense__site__single");
    // console.log('Inside the Trans TO ')
    console.log(this.props);
    NSN.siteID = tile.attr("data-siteid");
    sessionStorage.setItem("siteID", NSN.siteID);
    NSN.siteName = tile.find('h2').html();

    sessionStorage.setItem("siteName", NSN.siteName);


    helpers.clearSiteContext();
    this.setSiteContext(NSN.siteID);
    this.props.router.push(e.currentTarget.getAttribute("data-target"));
  },
  handleSelect: function (e) {
    e.stopPropagation();
    var tile = $(e.currentTarget).closest(".netsense__site__single");
    NSN.siteID = tile.attr("data-siteid");
    sessionStorage.setItem("siteID", NSN.siteID);
    NSN.siteName = tile.find('h2').html();
    sessionStorage.setItem("siteName", NSN.siteName);
    helpers.clearSiteContext();
    this.setSiteContext(NSN.siteID);
    ReactBootstrap.Dispatcher.emit("SiteSingleTile.select", NSN.siteID, NSN.siteName)
  },

  setSiteContext: function (siteid) {
    var i = 0, found = false;
    while (!found && i < this.props.sitesList.length) {
      found = (siteid == this.props.sitesList[i].siteid);
      i++;
    }
    if (found) {
      var site = this.props.sitesList[--i];
    } else {
      console.log("Could not find site in props.");
      return;
    }
    var s = JSON.stringify(site);
    NSN.site = JSON.parse(s);
    sessionStorage.setItem("site", s);
  },
  handleEdit: function (e) {
    e.stopPropagation();
    var tile = $(e.currentTarget).closest(".netsense__site__single");
    NSN.siteID = tile.attr("data-siteid");
    sessionStorage.setItem("siteID", NSN.siteID);
    NSN.siteName = tile.find('h3').html();
    sessionStorage.setItem("siteName", NSN.siteName);
    this.setSiteContext(NSN.siteID);
    ReactBootstrap.Dispatcher.emit("Sitetiles.edit", NSN.siteID, NSN.siteName);
  },

  handleSiteGoToMouseEnter: function (e) {
    console.log("on enter", e.target.nodeName);
    let targetDiv = $(e.target);
    if (e.target.nodeName == 'SPAN' || e.target.nodeName == 'H5') {
      targetDiv = $(e.target).parent().closest('div');
    }
    console.log('targetDiv.data("menu")', targetDiv.data("menu"));
    const hoveredValue = targetDiv.data("menu");
    $('.sitenav').css('display', 'none');
    targetDiv.siblings('.sitenav').css('display', 'block');
  },

  handleSiteGoToMouseLeave: function (e) {
    var selector = $('.sitenav');
    console.log('on leave', $(':hover').filter(selector).length);
    if ($(':hover').filter(selector).length == 0) {
      $('.sitenav').css('display', 'none');
    }
  },

  componentDidMount() {
    let that = this;

    $("div.sitenav").on("mouseenter", function (e) {
    });

    $("div.sitenav").on("mouseleave", function (e) {
      e.preventDefault();
      $('.sitenav').css('display', 'none');
    });
  },

  render() {
    var that = this;
    var NodeVisibility = (NSN.userInfo.authorization[0].type != "sensity_admin") ? {} : { display: "none" };
    var MoreVisibility = (NSN.userInfo.authorization[0].type != "sensity_admin") ? {} : { display: "none" };
    var GroupVisibility = auth.allowed('CAN_READ', 'GroupModel') ? {} : { display: "none" };
    var ScheduleVisibility = auth.allowed('CAN_READ', 'ScheduleModel') ? {} : { display: "none" };
    var CommissioningVisibility = (NSN.userInfo.authorization[0].type != "partner_read_only" && NSN.userInfo.authorization[0].type != "end_user_read_only") ? {} : { display: "none" };
    var navigationClass = "sitenav" + (this.state.hovered == 'navigateTo' ? ' hovered' : '');

    return (
      <div className="netsense__site__single" data-siteid={this.props.site.siteid} key={"index"}>
        <div className="site-tile">
          <div className={navigationClass}>
            <ul className="sitesubnavlist">
              <li onClick={this.transTo} data-target="/app/nodepanel" style={NodeVisibility}><b> Nodes </b></li>
              <li onClick={this.transTo} data-target="/app/grouppanel" style={GroupVisibility}><b> Groups </b></li>
              <li onClick={this.transTo} data-target="/app/schedulepanel" style={ScheduleVisibility}><b> Schedules </b></li>
              <li onClick={this.transTo} data-target="/app/parkinggrouppanel"><b> Parking Groups </b></li>
              <li onClick={this.transTo} data-target="/app/parkingzonepanel"><b> Parking Zones </b></li>
              <li onClick={this.transTo} data-target="/app/trafficconfigpanel"><b> Traffic Configurations </b></li>
            </ul>
          </div>
          <div className="netsense__site--description--name">
            <h2 onClick={this.handleSelect}>{this.props.site.name}</h2>
          </div>
          <div className="netsense__site--description">
            <span className="netsense__site--description-location">{this.props.site.city}, {this.props.site.state}</span>
            <div className="netsense__site--description--edit" >
              <span className="ns-edit-icon" onClick={this.handleEdit} ></span>
            </div>
          </div>
          <div className="netsense__site--description--body">
            <ul className="netsense__form--list">
              <li className="netsense__form--list--item netsense__form--list--sites" onClick={this.transTo} data-target="/app/schedulepanel" style={ScheduleVisibility}> Schedules </li>
              <li className="netsense__form--list--item netsense__form--list--sites" onClick={this.transTo} data-target="/app/grouppanel" style={GroupVisibility}> Groups </li>
              <li className="netsense__form--list--item netsense__form--list--sites" onClick={this.transTo} data-target="/app/nodepanel" style={NodeVisibility}> Nodes </li>
            </ul>

            <div className="netsense__site--description--more collapsed" style={MoreVisibility} onClick={() => this.setState({ open: !this.state.open })}> More
              {this.state.open ?
                  <span className="more-icon rubix-icon icon-fontello-down-dir-1"></span>
                  :<span className="more-icon rubix-icon icon-fontello-right-dir-1"></span>
              }
            </div>
            {
              this.state.open ?
                <div>
                  {/*Management*/}
                  <div className="netsense__site--content">
                    <ul className="netsense__form--list">
                      <li className="netsense__form--list--item netsense__form--list--sites" onClick={this.transTo} data-target="/app/auditpanel" >Audits</li>
                      <li className="netsense__form--list--item netsense__form--list--sites" onClick={this.transTo} data-target="/app/reportingpanel">Reporting</li>
                      <li className="netsense__form--list--item netsense__form--list--sites" onClick={this.transTo} data-target="/app/notificationpanel">Notifications</li>
                      <li className="netsense__form--list--item netsense__form--list--sites" onClick={this.transTo} data-target="/app/fixturepanel">Fixtures</li>
                      {helpers.isInternalUser() &&
                        <li className="netsense__form--list--item netsense__form--list--sites" onClick={this.transTo} data-target="/app/daylightpanel"> Daylight Harvesting </li>
                      }
                    </ul>
                    <ul className="netsense__form--list ">
                      <li className="netsense__form--list--item netsense__form--list--sites" onClick={this.transTo} data-target="/app/proximitypanel"> Proximity Dimming </li>
                      <li className="netsense__form--list--item netsense__form--list--sites" onClick={this.transTo} data-target="/app/firmwarepanel"> Firmware </li>
                      <li className="netsense__form--list--item netsense__form--list--sites" onClick={this.transTo} data-target="/app/commissioningpanel" style={CommissioningVisibility}> Commissioning </li>
                      <li className="netsense__form--list--item netsense__form--list--sites" onClick={this.transTo} data-target="/app/configpanel"> Configurations </li>
                    </ul>
                    <ul className="netsense__form--list">
                      <li className="netsense__form--list--item netsense__form--list--sites" onClick={this.transTo} data-target="/app/parkinggrouppanel"> Parking Groups </li>
                      <li className="netsense__form--list--item netsense__form--list--sites" onClick={this.transTo} data-target="/app/parkingzonepanel"> Parking Zones </li>
                      <li className="netsense__form--list--item netsense__form--list--sites" onClick={this.transTo} data-target="/app/trafficconfigpanel"> Traffic Configurations </li>
                    </ul>
                    <ul className="netsense__form--list">
                      <li className="netsense__form--list--item netsense__form--list--sites" onClick={this.transTo} data-target="/app/parkingdashboardpanel"> Parking Dashboard </li>
                      <li className="netsense__form--list--item netsense__form--list--sites" onClick={this.transTo} data-target="/app/energylookerpanel"> Energy </li>
                      <li className="netsense__form--list--item netsense__form--list--sites"><a href="/parking" target="_blank" title="Opens in new tab/window">Parking Optimization</a></li>
                    </ul>


                  </div>
                </div> : null
            }
          </div>
        </div>
      </div>
    )
  }
});
module.exports = withRouter(SiteSingleTile);