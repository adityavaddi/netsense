import classNames from 'classnames';
import { State, Navigation } from 'react-router';
import Fixtureform from 'components/fixtures/fixtureform';
import helpers from 'global/utils/helpers';
import { Modal } from 'react-bootstrap';

var Fixturedetail = React.createClass({

  getInitialState: function () {
    // return this.getFixture(this.props.fixtureID, this.props.fixtures, this.props.fixturetypes);
    return this.props.fixture || null;
  },

  propTypes: {
    fixture: React.PropTypes.array.isRequired,
    fixturetypes: React.PropTypes.array.isRequired,
    fixtureID: React.PropTypes.string.isRequired,
    groups: React.PropTypes.array.isRequired,
    fixtures: React.PropTypes.array.isRequired
  },

  componentDidMount: function () {
    var that = this;
    $('body').on('keydown', function (e) {
      if ($('.modal-body').is(":visible") && e.which === 27) {
        that.props.hide();
      }
    });
  },

  // getFixture(fixtureID, fixtures) {
  //   if (fixtureID == "0" || fixtureID == "-1") {
  //     return {
  //       name: "",
  //       description: "",
  //       manufacturer: "",
  //       manufacturersku: "",
  //       fixtureid: "",
  //       fixtureType: this.props.fixturetypes,
  //       nemasocket: "",
  //       MaxPower0: "",
  //       MaxPower10: "",
  //       MaxPower50: "",
  //       MaxPower100: "",
  //       MinPower0: "",
  //       MinPower10: "",
  //       MinPower50: "",
  //       MinPower100: "",
  //       PowerDraw: "",
  //       MinimumLightLevelForFailureDetection: "",
  //       BallastCost: "",
  //       BulbCost: "",
  //       LegacyPowerDraw: "",
  //       DailyOperatingTime: "",
  //       groups: [],
  //       sites: [],
  //       nodes: [],
  //       assign: "unassigned",
  //       assigngroups: [],
  //       idx: -1
  //     };
  //   };

  //   for (var i = 0; i < fixtures.length; i++) {
  //     if (fixtures[i].fixtureid == fixtureID) {
  //       fixtures[i].assigngroups = fixtures[i].groups.map(function (group, index) { return group.groupid; });
  //       if (!fixtures[i].BallastCost) {
  //         fixtures[i].BallastCost = "";
  //       }
  //       if (!fixtures[i].BulbCost) {
  //         fixtures[i].BulbCost = "";
  //       }
  //       if (!fixtures[i].LegacyPowerDraw) {
  //         fixtures[i].LegacyPowerDraw = "";
  //       }
  //       if (!fixtures[i].DailyOperatingTime) {
  //         fixtures[i].DailyOperatingTime = "";
  //       }
  //       return (fixtures[i]);
  //     }
  //   }
  //   return null;
  // },

  componentWillReceiveProps: function (nextProps) {
    // this.setState(this.getFixture(nextProps.fixtureID, nextProps.fixtures));

	  var currentLength = $("#Fixture-grid").data("gridInstance").getData().getLength();
	  var allItemsLength = this.props.fixtures.length;
	  var currentRow = $("#Fixture-grid").data("gridInstance").getSelectedRows();
	  var noNextItem = {}; var noPreviousItem = {};var displayArrows = {};
	  if(currentLength === allItemsLength ){
		  var firstItem = 0; var lastItem = allItemsLength - 1;
		  for (var a = 0; a < allItemsLength; a++) {
			  if (this.props.fixtures[a]!=null && this.props.fixture!=null && this.props.fixtures[a].fixtureid === this.props.fixture.fixtureid){
				  noNextItem = this.props.fixture.idx === lastItem ? {display:"none"}:{};
				  noPreviousItem = this.props.fixture.idx === firstItem ? {display:"none"}:{};
			  }
		  }
		  displayArrows = (this.props.fixture!=null && this.props.fixture.fixtureid === "" ) ? { display: "none" } : {};
		  this.setState({displayArrows,  noNextItem,  noPreviousItem })
	  }else{
		  if (this.props.fixture!=null && this.props.fixture.fixtureid != null){
			  var firstElement = 0;
			  var lastElement = currentLength-1;
			  if(currentRow[0] === firstElement){
				  if(firstElement+1 === currentLength){ displayArrows={display:"none"};
				  }else{
					  noNextItem={};noPreviousItem={display:"none"}; displayArrows={};
				  }
			  }else if(currentRow[0] === lastElement){
				  noNextItem={display:"none"}; noPreviousItem={}; displayArrows={};
			  }else{
				  noNextItem={};noPreviousItem={}; displayArrows={};
			  }
			  this.setState({displayArrows , noNextItem,  noPreviousItem })
		  }
	  }


  },

  render: function () {
    console.log("props in fixture detail", this.props);
    console.log("this in fixture detail", this);
    if (this.props.show) {
      return (
        <div className="fixtureForm" >
          <Modal.Dialog style={{ zIndex: 100, marginTop: 80 }}>
            <Modal.Body>
            <a className="" id="fixtureoverlayClose" onClick={() => { this.props.hide() }}>    
              <img src='/imgs/Close1.svg' width='30' height='30'  style={{marginTop:'60px',marginRight:'8px', zIndex: 100} }/>
            </a>
              <Fixtureform fixture={this.props.fixture} fixturetypes={this.props.fixturetypes} groups={this.props.groups} allFixtures={this.props.fixtures}
                           displayArrows={this.state.displayArrows} noNextItem={this.state.noNextItem} noPreviousItem={this.state.noPreviousItem}/>
            </Modal.Body>

          </Modal.Dialog>
        </div>
      )
    }
    return null;
  }
});

module.exports = Fixturedetail;