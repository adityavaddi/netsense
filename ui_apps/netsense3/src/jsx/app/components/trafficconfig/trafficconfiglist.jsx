import classNames from 'classnames';
import auth from 'global/utils/auth';
import { State, Navigation } from 'react-router';
import DataGrid from 'components/datagrid';
import Listeditor from 'components/listeditor';

var Trafficconfiglist = React.createClass({
  getInitialState: function () {
    var component = "Trafficconfig";
    var userRole = NSN.userInfo.name;
    var componentColumns = "TrafficconfigColumnWidths_" + userRole;
    var trafficConfigStoredWidths = [];

    if (localStorage.getItem(componentColumns)) {
      trafficConfigStoredWidths = JSON.parse(localStorage.getItem(componentColumns));
    } else {
      // default widths
      trafficConfigStoredWidths = [
        { name: "Name", field: "roiname", id: "name", sortable: true, width: 100, required: false, checked: true },
        { name: "Active", field: "active", id: "active", sortable: true, width: 100, required: true, checked: true },
        { name: "Nodeid", field: "nodeid", id: "nodeid", sortable: true, width: 100, required: false, checked: true },
        { name: "Eventid", field: "eventid", id: "eventid", sortable: true, width: 100, required: false, checked: true }
      ]
    }
    return {
      showListEditor: false,
      trafficConfigStoredWidths: trafficConfigStoredWidths,
      componentColumns: componentColumns,
      component: component,
      isActiveFilter: 'Yes',
      filterevent: 'All'
    }
  },

  propTypes: {
    trafficconfigs: React.PropTypes.array.isRequired,
    trafficconfigID: React.PropTypes.string,
    minmax: React.PropTypes.string,
    detail_state: React.PropTypes.string,
    ui_state: React.PropTypes.object
  },

  maximize: function () {
        if (this.props.ui_state.detail == "pinned") {
            ReactBootstrap.Dispatcher.emit('Trafficconfigdetail.togglePin');
        }
        $("#trafficconfig-list-panel").data("state", "open").css({ width: "100%" });
        $("#trafficconfig-table-wide").css({ width: "98%" });
        $(window).trigger('resize');
  },

  minimize: function () {
    $("#trafficconfig-list-panel").data("state", "closed").css({ width: "33%" });
    $("#trafficconfig-table-wide").css({ width: "1600px" });
    $(window).trigger('resize');
  },

  togglegrid: function () {
    if ($("#trafficconfig-list-panel").data("state") == "closed") {
      this.maximize();
    } else {
      this.minimize();
    }
  },

  shouldComponentUpdate: function (nextProps, nextState) {
    // This component does not need to be rerendered unless customers have been added or changed
    // return this.props.trafficconfigs !== nextProps.trafficconfigs;
    return true;
  },

  handleChange: function (e, key) {
    this.setState({
      [key]: e.target.value
    }, () => this.handleSubmit())
  },
  
  toggleListEditor: function () {
    this.setState({ showListEditor: !this.state.showListEditor });
  },

  toggleDetail: function () {
        if ($("#trafficconfig-list-panel").data("state") == "open") {
            this.minimize();
            ReactBootstrap.Dispatcher.emit("Trafficconfiglist.toggleDetail");
        } else {
            ReactBootstrap.Dispatcher.emit("Trafficconfiglist.toggleDetail");
        }
  },


  handleSubmit: function () {
    console.log(this.state);
    ReactBootstrap.Dispatcher.emit("Trafficconfiglist.filter", Object.assign({}, this.state));
  },

  handleOptionChange: function (changeEvent) {
    this.setState({
      isActiveFilter: changeEvent.target.value
    }, () => {
      this.handleSubmit();
    });

  },
  render() {

    if (this.props.minmax) {
      var glyph = (this.props.minmax === "expand") ? "icon-fontello-step-backward" : "icon-fontello-resize-horizontal";
      var handler = this.togglegrid;
      var Minmaxlist = (
        <div style={{ position: "absolute", cursor: "pointer", top: "-15px", right: "33px", height: "20px", width: "20px", fontSize: "28px" }}
          onClick={handler}>
          <Icon bundle={this.props.bundle} glyph={glyph} />
        </div>);
    };
    return (
      <div id="group-table-container" style={{ height: "100%" }}>
        {Minmaxlist}
        <h2 className="netsense__map__table__title">Traffic Configurations
            <span onClick={() => this.toggleListEditor()}
            className="ns-map-filter-icon" style={{top:"8px !important"}}></span>
        </h2>
        <div className="traffic-filter">
            <div className="row">
              <div className="col-sm-6">
                <form role="form" className="form-vertical filter-form">
                  <div className="form-group">
                    <label className="control-label active-label">Active :</label>
                    <ul>
                      <li><input id="filter-active-yes" type="radio" value="Yes" checked={this.state.isActiveFilter === 'Yes'} onChange={this.handleOptionChange} /><label>Yes</label></li>
                      <li><input id="filter-active-no" type="radio" value="No" checked={this.state.isActiveFilter === 'No'} onChange={this.handleOptionChange} /><label>No</label></li>
                      <li><input id="filter-active-all" type="radio" value="All" checked={this.state.isActiveFilter === 'All'} onChange={this.handleOptionChange} /><label>All</label></li>
                    </ul>
                  </div>
                </form>
              </div>
              <div className="col-sm-6">
                <form role="form" className="form-vertical filter-form">
                  <div className="form-group">
                    <label htmlFor="filterevent" className="control-label filter-type-label">Configuration type:</label>
                      <select className="form-control" style={{ height: "30px" }} name="filterevent" id="filterevent" onChange={(e) => { this.handleChange(e, 'filterevent') }}>
                        <option id="All" value="All" > All </option>
                        <option id="ObjectDwellConfig" value="ObjectDwellConfig"> Object Dwell Config </option>
                        <option id="ObjectEnteringConfig" value="ObjectEnteringConfig"> Object Entering Config </option>
                        <option id="ObjectLeavingConfig" value="ObjectLeavingConfig"> Object Leaving Config </option>
                        <option id="LineCrossingConfig" value="LineCrossingConfig"> Line Crossing Config </option>
                      </select>
                  </div>
                </form>
              </div>
           </div>
        </div>

        {
          this.state.showListEditor ?
            <div className="ns-list-editor" style={{top:"62px !important"}}>
              <Listeditor show={this.state.showListEditor}
                component={this.state.component}
                handleToggle={this.toggleListEditor}
                componentColumns={this.state.componentColumns}
                columns={this.state.trafficConfigStoredWidths} />
            </div> :
            null
        }
        <DataGrid component="Trafficconfig"
          componentName="Configuration"
          dataArray={this.props.trafficconfigs}
          dataID={this.props.trafficconfigID}
          dataIdField="trafficconfigid"
          options={{gridHeight: { windowPct: 100, offset: -330 }}}
          componentColumns={this.state.componentColumns}
          columns={this.state.trafficConfigStoredWidths} />
        <div style={{textAlign:"center"}} >
            <button className="ns-big-btn" style={{float:"none",width:"300px !important"}} onClick={this.toggleDetail}>{
                this.props.detail_state=="hidden"
                ?<b>Show Details</b>
                :<b>Hide Details</b>
              }</button>
        </div>
      </div>
    );
  }
});

module.exports = Trafficconfiglist;

