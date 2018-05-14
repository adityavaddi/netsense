import classNames from 'classnames';
import helpers from 'global/utils/helpers';
import { State, Navigation } from 'react-router';

var ConfigUpdateModal = React.createClass({

  propTypes: {
    show: React.PropTypes.bool.isRequired,
    context: React.PropTypes.string.isRequired,
    entity: React.PropTypes.object.isRequired
  },

  handleClose: function() {
    ReactBootstrap.Dispatcher.emit(helpers.ucfirst(this.props.context) + 'form.update', 'close');
  },

  handleSaveandApply: function(e){
    e.stopPropagation();
    e.preventDefault();
    var selectedNodes = [];
    $('.nodesAssignedModal input:checked').each(function() {
        selectedNodes.push($(this).attr('id'));
    });
    //console.log(selectedNodes);

    var entity = this.props.entity;
    var entityNodes = [];
     $.each(entity.nodes, function(i, obj) {
        for(var j=0;j<selectedNodes.length;j++){
          if(obj.nodeid == selectedNodes[j]){
            console.log(obj);
            entityNodes.push(obj);
          }
        }

      });
     console.log(entityNodes);
     entity.nodes = entityNodes;
     console.log(this.props.entity);

    ReactBootstrap.Dispatcher.emit(helpers.ucfirst(this.props.context) + 'form.saveandapply', this.props.entity);
  },

  componentDidUpdate: function(){
    
    if (this.props.show) {
        $(".config-display").fadeIn('fast');
    } else {
        $(".config-display").fadeOut('fast');
    }
  },

  componentWillUnmount: function(){
  },

  render() {

     var hStyle = {overflowY:"auto",overflowX:"hidden",marginLeft:"20px",
                  maxHeight:helpers.calcHeight(100, -250)+"px !important"};

    return (
      <div className="config-display">
        <div id="config-heading"> The updated config will be applied to the below nodes:</div>
        <div id="config" style={{marginBottom:"60px"}}>
          <div className="infoMessage">
            <div className="nodesAssignedModal" style={hStyle}></div>
          </div>
        </div>
        <div style={{position:"absolute",bottom:"10px",right:"10px"}}>
          <button type="button" className="ns-save-btn" onClick={this.handleSaveandApply}>
          <b> Update & Apply </b></button>
          &nbsp; &nbsp;
          <button type="button" className="ns-form-btn" onClick={this.handleClose}>
          <b> Cancel </b></button>
        </div>
      </div>
    );
  }
});

module.exports = ConfigUpdateModal;
