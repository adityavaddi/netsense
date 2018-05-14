import classNames from 'classnames';
import { State, Navigation } from 'react-router';
import Proximityform from 'components/proximity/proximityform';
import { Modal } from 'react-bootstrap';

var Proximitydetail = React.createClass({

  getInitialState: function () {
    return this.getProximity(this.props.proximityID, this.props.proximitys);
  },

  propTypes: {
    proximitys: React.PropTypes.array.isRequired,
    proximityID: React.PropTypes.string.isRequired
  },

  componentDidMount: function() {
    var that = this;
    $('body').on('keydown', function (e) {
      if($('.modal-body').is(":visible") && e.which === 27){
        that.props.hide();
      }
    });
  },

  getProximity: function (proximityID, proximitys) {
    if (proximityID == "0" || proximityID == "-1") {
      return {
        pdprofileid: "0",
        name: "",
        description: "",
        minLevel: 0,
        maxLevel: 100,
        enableRadius: false,
        enableRadiusDisabled: false,
        //          radius: 10,
        beginTime: "18:00:00",
        endTime: "06:00:00",
        detection_duration: 300,
        idx: -1
      };
    };

    for (var i = 0; i < proximitys.length; i++) {
      if (proximitys[i].pdprofileid == proximityID) {
        return (proximitys[i]);
      }
    }
    return null;
  },

  componentWillReceiveProps: function (nextProps) {
    this.setState(this.getProximity(nextProps.proximityID, nextProps.proximitys));
  },

  render: function () {
    console.log("this in proximityDetail", this.state)
    if (this.props.show) {
      return (
        <div className="proximityForm" >
          <Modal.Dialog style={{ zIndex: 100, marginTop: 80 }}>

            <Modal.Body>
               <a className=" " id="proximityoverlayClose" onClick={() => { this.props.hide() }}>   
                <img src='/imgs/Close1.svg' width='30' height='30'  style={{marginTop:'60px',marginRight:'8px', zIndex: 100} }/>
              </a>
              <Proximityform proximity={this.state} allProximities={this.props.proximitys} />
            </Modal.Body>

          </Modal.Dialog>
        </div>
      )
    }
    return null;
  }
});

module.exports = Proximitydetail;