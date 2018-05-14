import classNames from 'classnames';
import auth from 'global/utils/auth';
import helpers from 'global/utils/helpers';

import { State, Navigation } from 'react-router';
import Sitedatatable from 'components/sites/sitedatatable';

var Sitelist = React.createClass({
  propTypes: {
    sites: React.PropTypes.array.isRequired,
    siteID: React.PropTypes.string.isRequired,
    concise:React.PropTypes.bool
  },

  getDefaultProps: function () { 
    return {
      concise:false,
    };
  },

  handleAdd: function() {
    ReactBootstrap.Dispatcher.emit("Sitelist.add");
  },

  shouldComponentUpdate: function(nextProps,nextState){
    // This component does not need to be rerendered unless sites have been added
    return (!helpers.compareArrays(this.props.sites, nextProps.sites));
  },
 
  render() {      
    var Addbutton = (<span></span>);
    if (auth.allowed('CAN_CREATE','SiteModel')) {
       Addbutton = (
        <span style={{position:"relative",fontSize:"30px",color:"#3c3",cursor:"pointer",zIndex:"99999"}} title="Add Customer">
          <Icon glyph="icon-fontello-plus-circle" onClick={this.handleAdd} />
        </span>
        );
    };
    if(this.props.concise){
      return (
        <div id="site-table-container">
          <h2 style={{position:"absolute",top:"-10px",left:"12px",color:"black"}}>Sites</h2>
          <br/> <br/>
          <Sitedatatable sites={this.props.sites} siteID={this.props.siteID} concise={true} />
        </div>
      );
    }
    else{
      return (
        <div id="site-table-container">
          <h2 style={{position:"absolute",top:"-10px",left:"12px", color:"black"}}>Sites {Addbutton}</h2>
          <Sitedatatable sites={this.props.sites} siteID={this.props.siteID} />
        </div>
      ); 
    }
     
  }
});

module.exports = Sitelist;

