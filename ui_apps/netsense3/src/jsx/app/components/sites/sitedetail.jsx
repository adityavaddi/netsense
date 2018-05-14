import classNames from 'classnames';
import { State, Navigation } from 'react-router';
import Siteform from 'components/sites/siteform';
import { Modal } from 'react-bootstrap';

var SiteDetail = React.createClass({

  getInitialState: function() {
    return this.getSite(this.props.siteID, this.props.sites);
  },

  propTypes: {
    sites: React.PropTypes.array.isRequired,
    siteID: React.PropTypes.string.isRequired,
    submitStatus: React.PropTypes.bool
  },

  componentDidMount: function () {
    var that = this;
    $('body').on('keydown', function (e) {
      if ($('.modal-body').is(":visible") && e.which === 27) {
        that.props.hide();
      }
    });
  },

  getSite: function(siteID, sites) {
    if (siteID == "0" || siteID == "-1") {
      return {
          name: "",
          street1: "",
          street2: "",
          city: "",
          state: "",
          country: "",
          postal_code: "",
          latitude: "",
          longitude: "",
          siteid: "",
          idx: -1
      };
    };
    for (var i=0; i<sites.length; i++) {
      if (sites[i].siteid == siteID){
        return (sites[i]);
      }
    }
    return null;
  },

  componentWillReceiveProps: function(nextProps){
 //   if (this.props.siteID != nextProps.siteID){
      this.setState(this.getSite(nextProps.siteID, nextProps.sites));
 //   };
  },


  render: function() {
    if(this.props.show){
        if (this.props.siteID == "-1") {
      return null;
    }
       return (
         <div className="sitesForm" >
         <Modal.Dialog style={{zIndex:100,marginTop:100}}>

             <Modal.Body>
              <a className=" " id="sitesoverlayClose" onClick={()=>{this.props.hide()}}>   
                <img src='/imgs/Close1.svg' width='30' height='30'  style={{marginTop:'60px',marginRight:'8px', zIndex: 100} }/>
              </a>
              <Siteform site={this.state} submitStatus={this.props.submitStatus} />
             </Modal.Body>
         </Modal.Dialog>
         </div>
       )
     }
     return null;
  }
});

module.exports = SiteDetail;