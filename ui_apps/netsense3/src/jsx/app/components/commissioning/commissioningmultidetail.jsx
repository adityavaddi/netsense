import classNames from 'classnames';
import Commissioningform from 'components/commissioning/commissioningform';
import { Modal } from 'react-bootstrap';
var commissioningmultidetail = React.createClass({

  render: function () {
    if (this.props.show) {
      return (
        <div className="commissioningForm" >
        <Modal.Dialog style={{ zIndex: 124, marginTop: 184 }}>
          <Modal.Body className="commissioning-modal-body">
            <Commissioningform sites={this.props.sites} isMultiSelected={true} selected_nodes={this.props.selected_nodes} nodes={this.props.nodes} toggleModal={this.props.toggleModal} />
          </Modal.Body>
        </Modal.Dialog>
        </div>
      )
    } return null
  }
});

module.exports = commissioningmultidetail;

