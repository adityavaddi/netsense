import classNames from 'classnames';
import auth from 'global/utils/auth';
import helpers from 'global/utils/helpers';
import Dropdown from 'components/dropdown';
import { State, Navigation } from 'react-router';

var Fixtureform = React.createClass({

  getInitialState: function () {
    return this.props.fixture
  },

  propTypes: {
    fixture: React.PropTypes.object.isRequired,
    fixturetypes: React.PropTypes.array.isRequired,
    groups: React.PropTypes.array.isRequired,
    allFixtures:React.PropTypes.array.isRequired
  },

  getDefaultProps: function () {
    return {
      multiple: false
    }
  },

  handleChange: function (key) {
    if (typeof key == "object") {
      var state = {};
      state[key.name] = key.value;
      this.setState(state);
    } else {
      return function (e) {
        var state = {};
        if (key == 'assigngroups') {
          state.assigngroups = $.makeArray($(e.target).val());
        } else {
          state[key] = e.target.value;
        }
        this.setState(state);
      }.bind(this);
    };
  },

  handleRadio: function (key) {
    return function (e) {
      var state = {};
      switch (e.target.value) {
        case "unassigned":
          state.sites = [];
          state.assign = "unassigned";
          break;
        case "sitewide":
          state.sites = [{
            siteid: NSN.siteID,
            name: NSN.siteName,
          }];
          state.assign = "sitewide";
          break;
        case "groups":
          state.sites = [];
          state.assign = "groups";
          state.assigngroups = $.makeArray($(React.findDOMNode(this.refs.assigngroups)).val());
          break;
      };
      this.setState(state);
    }.bind(this);
  },

  handleSubmit: function (e) {
    e.stopPropagation();
    e.preventDefault();
    ReactBootstrap.Dispatcher.emit("Fixtureform.save", Object.assign({}, this.state));
  },

  handleDelete: function (e) {
    e.preventDefault();
    if (confirm("Are you sure you want to Delete this Fixture?")) {
      ReactBootstrap.Dispatcher.emit("Fixtureform.delete", Object.assign({}, this.state));
    };
  },

  handleReset: function (e) {
    e.stopPropagation();
    e.preventDefault();
    ReactBootstrap.Dispatcher.emit("Fixtureform.reset", Object.assign({}, this.state));
  },

  callPreviousItem:function(allItems, currentItem){

    $('.fixture-previous').keyup();
    $("#Fixture-grid").data("gridInstance");
    console.log($("#Fixture-grid").data("gridInstance"));
    var currentRow = $("#Fixture-grid").data("gridInstance").getSelectedRows();
    var nextRow =  currentRow[0] -1
    $("#Fixture-grid").data("gridInstance").setSelectedRows([nextRow])
  },

  callNextItem:function(allItems, currentItem){

    $('.fixture-next').keydown();
    $("#Fixture-grid").data("gridInstance");
    console.log($("#CFixture-grid").data("gridInstance"));
    var currentRow = $("#Fixture-grid").data("gridInstance").getSelectedRows();
    var nextRow =  currentRow[0]+ 1;
    $("#Fixture-grid").data("gridInstance").setSelectedRows([nextRow])
  },


  /*handleFixtureType: function(e){
    ReactBootstrap.Dispatcher.emit("Fixtureform.fixturetype", this.state);
    e.preventDefault();
  }, */

  componentWillReceiveProps: function (nextProps) {
    //    if (this.props.fixture.fixtureid != nextProps.fixture.fixtureid){
    this.setState(nextProps.fixture);
    //    };
  },

  render: function () {
    var allItemsLength = this.props.allFixtures.length;
    var firstItem = 0;
    var lastItem = allItemsLength - 1;
    for( var a=0; a < allItemsLength; a++){
      if(this.props.allFixtures[a].fixtureid === this.props.fixture.fixtureid){
        var noNextItem = lastItem === a ? {display:"none"}:{};
        var noPreviousItem = firstItem === a ? {display:"none"}:{};
      }
    }
    var previousNextButtons = this.props.fixture.fixtureid === "" ? {display:"none"}:{};

    var hstyle = {
      overflowY: "auto", overflowX: "hidden", marginBottom: "30px", padding: "29px",
      maxHeight: helpers.calcHeight(100, -226) + "px !important"
    };
    var fixture = this.props.fixture;
    var defaultvalue;
/*    var options = this.props.fixturetypes.map(function (option, i) {
      if (option.selected === true || option.selected === 'selected') {
        if (this.props.multiple) {
          if (defaultvalue === undefined) {
            defaultvalue = [];
          }
          defaultvalue.push(option.value);
        } else {
          defaultvalue = option.value;
        }
      }
      return <option key={i} value={option.fixtureTypeId}>{option.fixtureType}</option>;
    }, this);
*/
    var ftoptions = this.props.fixturetypes.map(function(fixturetype, index) {
      var obj = {};
      obj[fixturetype.fixtureType] = fixturetype.fixtureType;
      return obj;
    });


    var heading = (fixture.name == "") ? "Add Fixture" : (<span><Icon glyph="icon-fontello-right-dir" /> {fixture.name}</span>);

    return (
      <div>
        <div className="netsense__form__header">
          <h3> {this.state.name} </h3>
        </div>
        <div className="netsense__form__body">
           <span style={this.props.displayArrows}>
              <span style={this.props.noPreviousItem} className="rubix-icon icon-simple-line-icons-arrow-left fixture-previous generic-previous" onClick={ () => this.callPreviousItem(this.props.allFixtures,this.state.fixture)}></span>
           </span>
          <span style={this.props.displayArrows}>
              <span style={this.props.noNextItem} className="rubix-icon icon-simple-line-icons-arrow-right fixture-next generic-next" onClick={ () => this.callNextItem(this.props.allFixtures,this.state.fixture)}></span>
          </span>

          <form role="form" className="form-horizontal" data-fixtureid={fixture.fixtureid} >

            <div style={hstyle}>
              <div className="form-group">
                <label htmlFor="name" className="control-label col-sm-3"> Name:</label>
                <div className="col-sm-6">
                  <input type="text" className="form-control" id="name" readonly ref="name" value={this.state.name} onChange={this.handleChange('name')} />
                </div>
              </div>
              <div className="form-group">
                <label htmlFor="description" className="control-label col-sm-3"> Description:</label>
                <div className="col-sm-6">
                  <input type="text" className="form-control" id="description" ref="description" value={this.state.description} onChange={this.handleChange('description')} />
                </div>
              </div>
              <div className="form-group">
                <label htmlFor="manufacturer" className="control-label col-sm-3"> Manufacturer:</label>
                <div className="col-sm-6">
                  <input type="text" className="form-control" id="manufacturer" ref="manufacturer" value={this.state.manufacturer} onChange={this.handleChange('manufacturer')} />
                </div>
              </div>
              <div className="form-group">
                <label htmlFor="manufacturersku" className="control-label col-sm-3"> Manufacturer SKU:</label>
                <div className="col-sm-6">
                  <input type="text" className="form-control" id="manufacturersku" ref="manufacturersku" value={this.state.manufacturersku} onChange={this.handleChange('manufacturersku')} />
                </div>
              </div>
              <div className="form-group">
                <label htmlFor="fixtureType" className="control-label col-sm-3">Luminaire type:</label>
                <div className="col-sm-6">
                  <Dropdown options={ftoptions}
                            className="form-control" 
                            id="fixtureType" 
                            value={this.state.fixtureType} 
                            onChange={this.handleChange} />
                </div>
              </div>
              <div className="form-group">
                <label htmlFor="PowerDraw" className="control-label col-sm-3">Power draw:</label>
                <div className="col-sm-6">
                  <input type="text" className="form-control" id="PowerDraw" ref="PowerDraw" value={this.state.PowerDraw} onChange={this.handleChange('PowerDraw')} />
                </div>
              </div>
              <div className="form-group">
                <label htmlFor="nemasocket" className="control-label col-sm-3"> NEMA socket:</label>
                <div className="col-sm-6">
                  <Dropdown options={[{"Yes":"Yes"},{"No":"No"}]}
                            className="form-control" 
                            id="nemasocket" 
                            value={this.state.nemasocket} 
                            onChange={this.handleChange} />
                </div>
              </div>
              <div className="form-group">
                <label htmlFor="MaxPower0" className="control-label col-sm-3"> Max power 0%:</label>
                <div className="col-sm-6">
                  <input type="text" className="form-control" id="MaxPower0" ref="MaxPower0" value={this.state.MaxPower0} onChange={this.handleChange('MaxPower0')} />
                </div>
              </div>
              <div className="form-group">
                <label htmlFor="MaxPower10" className="control-label col-sm-3"> Max power 10%:</label>
                <div className="col-sm-6">
                  <input type="text" className="form-control" id="MaxPower10" ref="MaxPower10" value={this.state.MaxPower10} onChange={this.handleChange('MaxPower10')} />
                </div>
              </div>
              <div className="form-group">
                <label htmlFor="MaxPower50" className="control-label col-sm-3"> Max power 50%:</label>
                <div className="col-sm-6">
                  <input type="text" className="form-control" id="MaxPower50" ref="MaxPower50" value={this.state.MaxPower50} onChange={this.handleChange('MaxPower50')} />
                </div>
              </div>
              <div className="form-group">
                <label htmlFor="MaxPower100" className="control-label col-sm-3"> Max power 100%:</label>
                <div className="col-sm-6">
                  <input type="text" className="form-control" id="MaxPower100" ref="MaxPower100" value={this.state.MaxPower100} onChange={this.handleChange('MaxPower100')} />
                </div>
              </div>
              <div className="form-group">
                <label htmlFor="MinPower0" className="control-label col-sm-3"> Min power 0%:</label>
                <div className="col-sm-6">
                  <input type="text" className="form-control" id="MinPower0" ref="MinPower0" value={this.state.MinPower0} onChange={this.handleChange('MinPower0')} />
                </div>
              </div>
              <div className="form-group">
                <label htmlFor="MinPower10" className="control-label col-sm-3"> Min power 10%:</label>
                <div className="col-sm-6">
                  <input type="text" className="form-control" id="MinPower10" ref="MinPower10" value={this.state.MinPower10} onChange={this.handleChange('MinPower10')} />
                </div>
              </div>
              <div className="form-group">
                <label htmlFor="MinPower50" className="control-label col-sm-3"> Min power 50%:</label>
                <div className="col-sm-6">
                  <input type="text" className="form-control" id="MinPower50" ref="MinPower50" value={this.state.MinPower50} onChange={this.handleChange('MinPower50')} />
                </div>
              </div>
              <div className="form-group">
                <label htmlFor="MinPower100" className="control-label col-sm-3"> Min power 100%:</label>
                <div className="col-sm-6">
                  <input type="text" className="form-control" id="MinPower100" ref="MinPower100" value={this.state.MinPower100} onChange={this.handleChange('MinPower100')} />
                </div>
              </div>
              <div className="form-group">
                <label htmlFor="MinimumLightLevelForFailureDetection" className="control-label col-sm-3">Minumum light level for failure detection:</label>
                <div className="col-sm-6">
                  <input type="text" className="form-control" id="MinimumLightLevelForFailureDetection" ref="MinimumLightLevelForFailureDetection" value={this.state.MinimumLightLevelForFailureDetection} onChange={this.handleChange('MinimumLightLevelForFailureDetection')} />
                </div>
              </div>

              <div className="form-group">
                <label htmlFor="BallastCost" className="control-label col-sm-3">Ballast cost:</label>
                <div className="col-sm-6">
                  <input type="text" className="form-control" id="BallastCost" ref="BallastCost" value={this.state.BallastCost} onChange={this.handleChange('BallastCost')} />
                </div>
              </div>
              <div className="form-group">
                <label htmlFor="BulbCost" className="control-label col-sm-3">Bulb cost:</label>
                <div className="col-sm-6">
                  <input type="text" className="form-control" id="BulbCost" ref="BulbCost" value={this.state.BulbCost} onChange={this.handleChange('BulbCost')} />
                </div>
              </div>
              <div className="form-group">
                <label htmlFor="LegacyPowerDraw" className="control-label col-sm-3">Legacy power draw:</label>
                <div className="col-sm-6">
                  <input type="text" className="form-control" id="LegacyPowerDraw" ref="LegacyPowerDraw" value={this.state.LegacyPowerDraw} onChange={this.handleChange('LegacyPowerDraw')} />
                </div>
              </div>
              <div className="form-group">
                <label htmlFor="DailyOperatingTime" className="control-label col-sm-3">Daily operating time:</label>
                <div className="col-sm-6">
                  <input type="text" className="form-control" id="DailyOperatingTime" ref="DailyOperatingTime" value={this.state.DailyOperatingTime} onChange={this.handleChange('DailyOperatingTime')} />
                </div>
              </div>

              <div className="form-group" style={{ margin: "0px 24px" }}>
                <h3>&bull; Assign this profile as follows:</h3>
                <div className="col-sm-3 text-center">
                  <input type="radio" name="fx-assign" value="unassigned" checked={this.state.assign == "unassigned"} onChange={this.handleRadio('fx-assign')} /> 
                  <span style={{position: "relative", bottom:"6px"}}> Unassigned</span>
                </div>
                <div className="col-sm-3 text-center">
                  <input type="radio" name="fx-assign" value="sitewide" checked={this.state.assign == "sitewide"} onChange={this.handleRadio('fx-assign')} /> 
                  <span style={{position: "relative", bottom:"6px"}}> Site-wide </span>
                </div>
                <div className="col-sm-3">
                <span>
                  <input type="radio" name="fx-assign" value="groups" style={{ verticalAlign: "top" }} checked={this.state.assign == "groups"} onChange={this.handleRadio('fx-assign')} /> Assign to groups
              </span>
                  <br />
                  <select multiple={true} style={{ marginLeft: "32px" }} name="assigngroups" ref="assigngroups" value={this.state.assigngroups} onChange={this.handleChange('assigngroups')} >
                    {
                      this.props.groups.map(function (group, index) {
                        return <option key={index} value={group.groupid}>{group.name}</option>;
                      })
                    }
                  </select>
                </div>
              </div>

            </div>
            <div style={{ margin: "-24px", padding: "14px" }}>
              <div className="col-sm-3">
                <button type="button" className="ns-delete-btn" onClick={this.handleDelete}>
                  <b>Delete</b></button>
              </div>
              <div className="col-sm-9 text-right">
                <button type="button" className="ns-reset-btn" onClick={this.handleReset}>
                  <b>Reset</b></button>
                &nbsp; &nbsp;
                <button type="button" className="ns-save-btn" id="saveFixture" onClick={this.handleSubmit}>
                  <b> Save</b></button>
              </div>
            </div>
          </form>
        </div>

      </div>
    );
  }
});

module.exports = Fixtureform;