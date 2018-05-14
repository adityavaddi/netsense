import classNames from 'classnames';
import { State, Navigation } from 'react-router';
import Commissioningform from 'components/commissioning/commissioningform';

import { Modal } from 'react-bootstrap';

var Commissioningdetail = React.createClass({

  getInitialState: function () {
    return this.getNode(this.props.nodeID, this.props.nodes);
  },

  propTypes: {
    sites: React.PropTypes.array.isRequired,
    nodeID: React.PropTypes.string.isRequired,
    nodes: React.PropTypes.array.isRequired
  },

  getNode: function (nodeID, nodes) {
    if (nodeID == "0" || nodeID == "-1") {
      return {
        nodeid: "",
        name: "",
        model: ""

      };
    };
    for (var i = 0; i < nodes.length; i++) {
      if (nodes[i].nodeid == nodeID) {
        return (nodes[i]);
      }
    }
    return null;
  },

  componentWillReceiveProps: function (nextProps) {
    if (this.props.nodeID != nextProps.nodeID) {
      this.setState(this.getNode(nextProps.nodeID, nextProps.nodes));
    };
  },

  render: function () {
    if (this.props.show) {
      return (
        <div className="commissioningForm">
          <Modal.Dialog  style={{ zIndex:124, marginTop:184 }}>
            <Modal.Body className="commissioning-modal-body">
              <Commissioningform node={this.state} sites={this.props.sites} isMultiSelected={false} toggleModal={this.props.toggleModal} />
            </Modal.Body>
          </Modal.Dialog>
        </div>
      )
    } return null

  }

});
module.exports = Commissioningdetail;