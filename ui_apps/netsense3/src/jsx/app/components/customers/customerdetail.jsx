import classNames from 'classnames';
import { State, Navigation } from 'react-router';
import Customerform from 'components/customers/customerform';
import helpers from 'global/utils/helpers';
// import Customers from '../../common/customeredit';
import { Modal } from 'react-bootstrap';

var Customerdetail = React.createClass({

  getInitialState: function () {
    return {
        displayArrows:true
    }
  },

  propTypes: {
    customers: React.PropTypes.array.isRequired,
    customerID: React.PropTypes.string.isRequired,
    allCustomers: React.PropTypes.array.isRequired,
    sites: React.PropTypes.array.isRequired
  },

componentDidMount: function() {  
    var that = this;
    $('body').on('keydown', function (e) {;
      if($('.modal-body').is(":visible") && e.which === 27){
        that.props.hide();
      }
    });
},

  componentWillReceiveProps: function (nextProps) {

	  var currentLength = $("#Customer-grid").data("gridInstance").getData().getLength();
	  var allItemsLength = this.props.allCustomers.length;
	  var currentRow = $("#Customer-grid").data("gridInstance").getSelectedRows();
	  var noNextItem = {}; var noPreviousItem = {};var displayArrows = {};
	  if(currentLength === allItemsLength ){
		  var firstItem = 0; var lastItem = allItemsLength - 1;
		  for (var a = 0; a < allItemsLength; a++) {
			  if (this.props.allCustomers[a]!=null && this.props.customer!=null && this.props.allCustomers[a].orgid === this.props.customer.orgid){
				  noNextItem = this.props.customer.idx === lastItem ? {display:"none"}:{};
				  noPreviousItem = this.props.customer.idx === firstItem ? {display:"none"}:{};
			  }
		  }
		  displayArrows = (this.props.customer!=null && this.props.customer.orgid === "" ) ? { display: "none" } : {};
		  this.setState({displayArrows,  noNextItem,  noPreviousItem })
	  }else{
		  if (this.props.customer!=null && this.props.customer.orgid != null){
			  var firstElement = 0;
			  var lastElement = currentLength-1;
			  if(currentRow[0] === firstElement){
				  //display only the next arrow and not the previous arrow
				  if(firstElement+1 === currentLength){ displayArrows={display:"none"};
				  }else{
					  noNextItem={};noPreviousItem={display:"none"}; displayArrows={};
				  }
			  }else if(currentRow[0] === lastElement){
				  //display only the previous arrow and not the next arrow
				  noNextItem={display:"none"}; noPreviousItem={}; displayArrows={};
			  }else{
				  //display both arrows
				  noNextItem={};noPreviousItem={}; displayArrows={};
			  }
			  this.setState({displayArrows , noNextItem,  noPreviousItem })
		  }else{
			  displayArrows={display:"none"};noNextItem={display:"none"};noPreviousItem={display:"none"};
			  this.setState({displayArrows , noNextItem,  noPreviousItem })
		  }
	  }
  },

  render: function () {
    if (this.props.show) {
      return (
        <div className="customerForm" >
          <Modal.Dialog style={{ marginTop: 100 }}>         
            <Modal.Body>
              <a className="" id="customeroverlayClose" onClick={() => { this.props.hide() }}>
             <img src='/imgs/Close1.svg' width='30' height='30'  style={{marginTop:'60px',marginRight:'8px', zIndex: 100, width:"20px",height:"20px"}}/>
            </a>
              <Customerform customer={this.props.customer} sites={this.props.sites} allCustomers={this.props.allCustomers}
                            displayArrows={this.state.displayArrows} noNextItem={this.state.noNextItem} noPreviousItem={this.state.noPreviousItem}/>
            </Modal.Body>

          </Modal.Dialog>
        </div>
      )
    }
    return null;
  }


});

module.exports = Customerdetail;