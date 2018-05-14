import classNames from 'classnames';
import { State, Navigation } from 'react-router';
import Scheduleform from 'components/schedules/scheduleform';
import { Modal } from 'react-bootstrap';

var Scheduledetail = React.createClass({

  getInitialState: function() {
    return this.getSchedule(this.props.scheduleID, this.props.schedules);
  },

  propTypes: {
    schedules: React.PropTypes.array.isRequired,
    scheduleID: React.PropTypes.string.isRequired,
    groups: React.PropTypes.array.isRequired,
    submitStatus: React.PropTypes.bool
  },

  componentDidMount: function() {
    var that = this;
    $('body').on('keydown', function (e) {
      if($('.modal-body').is(":visible") && e.which === 27){
        that.props.hide();
      }
    });
  },

  getSchedule: function(scheduleID, schedules) {
    if (scheduleID == "0" || scheduleID == "-1") {
      return {
          name: "",
          description: "",
          network: {photocell_enabled:true,
                    photocell_highLevel:100,
                    photocell_lowLevel:0,
                    highTime:"18:00:00",
                    highLevel:60
                  },
          events:[{photocell_enabled:false,
                   photocell_highLevel:100,
                   photocell_lowLevel:0,
                   days:["mon","tue","wed","thu","fri","sat","sun"],
                   actions:[]
                 }],
          scheduleid: scheduleID,
          sites:[],
          groups:[],
          nodes:[],
          idx: -1
      };
    };
    for (var i=0; i<schedules.length; i++) {
      if (schedules[i].scheduleid == scheduleID){
        return (schedules[i]);
      }
    }
  },

  componentWillReceiveProps: function(nextProps){
//    if (this.props.scheduleID != nextProps.scheduleID){
      this.setState(this.getSchedule(nextProps.scheduleID, nextProps.schedules));
//    };
  },

  render: function() {
    if (this.props.schedules.length == 0 && this.props.scheduleID != "0") {
      return (
        <div style={{padding:"20px 0px 0px 0px"}}>
          <div style={{textAlign:"center",fontSize:"54px",opacity:"0.5"}}>
              <Icon glyph="icon-fontello-info-circled" />
          </div>
          <h3 style={{textAlign:"center",lineHeight:"140%"}}>Authorized users can create Lighting Schedules
          <br />and assign them to Sites or Groups on this page.</h3>
        </div>
        );
    };

    // if (this.props.scheduleID == "-1") {
    //   return (
    //     <h2 style={{textAlign:"center",padding:"100px 0px"}}>Select a schedule</h2>
    //     );
    // };
  //   return (
  //     <Scheduleform schedule={this.state} groups={this.props.groups} />
  //   );

  // New 
  if (this.props.show) {
      return (
        <div className="customerForm" >
          <Modal.Dialog style={{ zIndex: 100, marginTop: "80px" }}>
            <Modal.Body style={{ paddingBottom:"20px"}}>
              <a className=" " id="scheduleoverlayClose" onClick={() => { this.props.hide() }}>   
                <img src='/imgs/Close1.svg' width='30' height='30'  style={{marginTop:'60px',marginRight:'8px', zIndex: 100} }/>
              </a>
              <Scheduleform schedule={this.state} groups={this.props.groups} allSchedules ={this.props.schedules} submitStatus={this.props.submitStatus}/>
            </Modal.Body>
          </Modal.Dialog>
        </div>
      )
    }
    return null;
  }

});

module.exports = Scheduledetail;