import { withRouter } from 'react-router';
import classNames from 'classnames';
import { Link } from 'react-router';

import DataUtil from '../service/datautil';
import DataGrid from '../components/datagrid';
import SearchSites from './searchsiteslist';

import Customerdetail from '../components/customers/customerdetail';
import Fixturedetail from '../components/fixtures/fixturedetail';
import Proximitydetail from '../components/proximity/proximitydetail';

import Configdetail from '../components/configs/configdetail';
import auth from 'global/utils/auth';
var SearchOverlay = React.createClass({

  propTypes: {
    customers: React.PropTypes.array,
    customerID: React.PropTypes.string,
    proximitys: React.PropTypes.array,
    proximityID: React.PropTypes.string,
    overlayType: React.PropTypes.string,
    fixtures: React.PropTypes.array,
    fixturetypes: React.PropTypes.array,
    fixtureID: React.PropTypes.string,
    configs: React.PropTypes.array,
    // defaultmodel: null,
    groups: React.PropTypes.array,
    configID: React.PropTypes.string,
    siteSelectState: React.PropTypes.string,
    siteSelectCustomersLoaded: React.PropTypes.string
  },
  init: function () {
    console.log("init")
    // DataUtil.getAll('customers', this.processCustomerObject);
  },
  render() {
    // console.log('props', this.props);
    return (

      <div >
        {
          this.props.overlayType == 'customer' ?
            <Customerdetail customers={this.props.customers}
              customerID={this.props.customerID} /> : null
        }
        {
          this.props.overlayType == 'config' ?
            <Configdetail configs={this.props.configs}
              groups={this.props.groups}
              defaultmodel={this.props.defaultmodel}
              configID={this.props.configID} /> : null

        }
        {
          this.props.overlayType == 'search' ?
            <SearchSites siteSelectState={this.props.siteSelectState} siteSelectCustomersLoaded={this.props.siteSelectCustomersLoaded} /> : null
        }
        {
          this.props.overlayType == 'fixture' ?
            <Fixturedetail fixtures={this.props.fixtures} groups={this.props.groups} fixturetypes={this.props.fixturetypes}
              fixtureID={this.props.fixtureID} /> : null
        }
        {
          this.props.overlayType == 'proximity' ?
            <Proximitydetail proximitys={this.props.proximitys} proximityID={this.props.proximityID} /> : null

        }
      </div>
    );
  }
});
module.exports = withRouter(SearchOverlay);