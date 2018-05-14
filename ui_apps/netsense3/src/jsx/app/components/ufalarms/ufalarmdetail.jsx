import classNames from 'classnames';
import { State, Navigation } from 'react-router';
import UFAlarmForm from 'components/ufalarms/ufalarmform';
import { Modal } from 'react-bootstrap';

var UFalarmdetail = React.createClass({

  getInitialState: function () {
    return this.getUFalarm(this.props.ufalarm_ID, this.props.ufalarm);
  },

  propTypes: {
    ufalarm: React.PropTypes.array.isRequired,
    ufalarm_ID: React.PropTypes.string.isRequired
  },

  componentDidMount: function() {
    var that = this;
    $('body').on('keydown', function (e) {
      if($('.modal-body').is(":visible") && e.which === 27){
        that.props.hide();
      }
    });
  },

  getUFalarm: function (ufalarmID, ufalarms) {
      console.log("in getUFalarm 1", ufalarmID);
    if (ufalarmID == "0" || ufalarmID == "-1") {
      return {
        ufname: "",
        description: "",
        alarmtype: "",
        nodemodels: [],
        displaytocustomer: 'No',
        displaytopartner: 'No',
        ufalarm_ID: "0",
        idx: -1
      };
    };

    for (var i = 0; i < ufalarms.length; i++) {
      if (ufalarms[i].mappingid == ufalarmID) {
        console.log("in getUFalarm in forloop", ufalarms[i]);
        return (ufalarms[i]);
      }
    }
    return null;
  },

  componentWillReceiveProps: function (nextProps) {
      console.log("nextProps in ufdetail", nextProps);
    this.setState(this.getUFalarm(nextProps.ufalarm_ID, nextProps.ufalarm));
  },

  render: function () {
    console.log("this in ufalarmDetail", this)
    if (this.props.show) {
      return (
        <div className="ufalarmForm" >
          <Modal.Dialog style={{ zIndex: 100, marginTop: 80 }}>

            <Modal.Body>
               <a className=" " id="proximityoverlayClose" onClick={() => { this.props.hide() }}>   
                <img src='/imgs/Close1.svg' width='30' height='30'  style={{marginTop:'60px',marginRight:'8px', zIndex: 100} }/>
              </a>
              <UFAlarmForm ufalarm={this.state} allUFalarms={this.props.ufalarm}  />
            </Modal.Body>

          </Modal.Dialog>
        </div>
      )
    }
    return null;
  }
});

module.exports = UFalarmdetail;