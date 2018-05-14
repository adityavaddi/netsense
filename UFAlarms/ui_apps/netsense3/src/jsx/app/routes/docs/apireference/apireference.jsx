import { Link } from 'react-router';

import classNames from 'classnames';
import SidebarMixin from 'global/jsx/sidebar_component';
import helpers from 'global/utils/helpers';

import Header from 'common/header';
import Sidebar from 'common/sidebar';
import Footer from 'common/footer';

import package_snippet from '../../../../../../package.json';

class Body extends React.Component {
  calcHeight(pct, extra) {
    var h = (window&&window.document)?(($(window).height() - 75) * (pct/100) + extra):500;
    return h;
  }
  render() {
    var hstyle = {height:helpers.calcHeight(90, 0)+"px !important"};
    return (
        <Container id='body'>
          <Grid>
            <Row>
              <Col sm={12}>
                <PanelContainer>
                  <Panel>
                    <PanelBody style={hstyle}>
      <div style={{overflow:"scroll",height:"98%"}}>
        <div className="container">
          <h1>NetSense 3.0 API Reference</h1>
          <p />
          <div id="sw-default-consumes" className="sw-default-value">
            <span className="sw-default-value-header">Default request content-types:</span>
            application/json
          </div>
          <div id="sw-default-produces" className="sw-default-value">
            <span className="sw-default-value-header">Default response content-types:</span>
            application/json
          </div>
          <div id="sw-schemes" className="sw-default-value">
            <span className="sw-default-value-header">Schemes:</span>
            http, https
          </div>
          <h2 id="swagger--summary-no-tags">Summary</h2>
          <table className="table table-bordered table-condensed swagger--summary">
            <thead>
              <tr>
                <th>Path</th>
                <th>Operation</th>
                <th>Description</th>
              </tr>
            </thead>
            <tbody>
              <tr>
                <td className="swagger--summary-path" rowSpan={1}>
                  <a href="#path--audits-from--datemin--to--datemax--">/audits/from/{'{'}datemin{'}'}/to/{'{'}datemax{'}'}/</a>
                </td>
                <td>
                  <a href="#operation--audits-from--datemin--to--datemax---get">GET</a>
                </td>
                <td>
                </td>
              </tr>
              <tr>
                <td className="swagger--summary-path" rowSpan={2}>
                  <a href="#path--customers">/customers</a>
                </td>
                <td>
                  <a href="#operation--customers-get">GET</a>
                </td>
                <td>
                </td>
              </tr>
              <tr>
                <td>
                  <a href="#operation--customers-post">POST</a>
                </td>
                <td>
                </td>
              </tr>
              <tr>
                <td className="swagger--summary-path" rowSpan={3}>
                  <a href="#path--customers--cid-">/customers/{'{'}cid{'}'}</a>
                </td>
                <td>
                  <a href="#operation--customers--cid--delete">DELETE</a>
                </td>
                <td>
                </td>
              </tr>
              <tr>
                <td>
                  <a href="#operation--customers--cid--get">GET</a>
                </td>
                <td>
                </td>
              </tr>
              <tr>
                <td>
                  <a href="#operation--customers--cid--post">POST</a>
                </td>
                <td>
                </td>
              </tr>
              <tr>
                <td className="swagger--summary-path" rowSpan={2}>
                  <a href="#path--customers--cid--sites">/customers/{'{'}cid{'}'}/sites</a>
                </td>
                <td>
                  <a href="#operation--customers--cid--sites-get">GET</a>
                </td>
                <td>
                </td>
              </tr>
              <tr>
                <td>
                  <a href="#operation--customers--cid--sites-post">POST</a>
                </td>
                <td>
                </td>
              </tr>
              <tr>
                <td className="swagger--summary-path" rowSpan={3}>
                  <a href="#path--customers--cid--sites--sid-">/customers/{'{'}cid{'}'}/sites/{'{'}sid{'}'}</a>
                </td>
                <td>
                  <a href="#operation--customers--cid--sites--sid--delete">DELETE</a>
                </td>
                <td>
                </td>
              </tr>
              <tr>
                <td>
                  <a href="#operation--customers--cid--sites--sid--get">GET</a>
                </td>
                <td>
                </td>
              </tr>
              <tr>
                <td>
                  <a href="#operation--customers--cid--sites--sid--put">PUT</a>
                </td>
                <td>
                </td>
              </tr>
              <tr>
                <td className="swagger--summary-path" rowSpan={2}>
                  <a href="#path--customers--cid--sites--sid--nodes">/customers/{'{'}cid{'}'}/sites/{'{'}sid{'}'}/nodes</a>
                </td>
                <td>
                  <a href="#operation--customers--cid--sites--sid--nodes-get">GET</a>
                </td>
                <td>
                </td>
              </tr>
              <tr>
                <td>
                  <a href="#operation--customers--cid--sites--sid--nodes-post">POST</a>
                </td>
                <td>
                </td>
              </tr>
              <tr>
                <td className="swagger--summary-path" rowSpan={3}>
                  <a href="#path--customers--cid--sites--sid--nodes--nid-">/customers/{'{'}cid{'}'}/sites/{'{'}sid{'}'}/nodes/{'{'}nid{'}'}</a>
                </td>
                <td>
                  <a href="#operation--customers--cid--sites--sid--nodes--nid--delete">DELETE</a>
                </td>
                <td>
                </td>
              </tr>
              <tr>
                <td>
                  <a href="#operation--customers--cid--sites--sid--nodes--nid--get">GET</a>
                </td>
                <td>
                </td>
              </tr>
              <tr>
                <td>
                  <a href="#operation--customers--cid--sites--sid--nodes--nid--put">PUT</a>
                </td>
                <td>
                </td>
              </tr>
              <tr>
                <td className="swagger--summary-path" rowSpan={2}>
                  <a href="#path--customers--cid--sites--siteid--notifications">/customers/{'{'}cid{'}'}/sites/{'{'}siteid{'}'}/notifications</a>
                </td>
                <td>
                  <a href="#operation--customers--cid--sites--siteid--notifications-get">GET</a>
                </td>
                <td>
                </td>
              </tr>
              <tr>
                <td>
                  <a href="#operation--customers--cid--sites--siteid--notifications-post">POST</a>
                </td>
                <td>
                </td>
              </tr>
              <tr>
                <td className="swagger--summary-path" rowSpan={3}>
                  <a href="#path--customers--cid--sites--siteid--notifications--notificationid-">/customers/{'{'}cid{'}'}/sites/{'{'}siteid{'}'}/notifications/{'{'}notificationid{'}'}</a>
                </td>
                <td>
                  <a href="#operation--customers--cid--sites--siteid--notifications--notificationid--delete">DELETE</a>
                </td>
                <td>
                </td>
              </tr>
              <tr>
                <td>
                  <a href="#operation--customers--cid--sites--siteid--notifications--notificationid--get">GET</a>
                </td>
                <td>
                </td>
              </tr>
              <tr>
                <td>
                  <a href="#operation--customers--cid--sites--siteid--notifications--notificationid--post">POST</a>
                </td>
                <td>
                </td>
              </tr>
              <tr>
                <td className="swagger--summary-path" rowSpan={2}>
                  <a href="#path--customers--cid--sites--siteid--overlays">/customers/{'{'}cid{'}'}/sites/{'{'}siteid{'}'}/overlays</a>
                </td>
                <td>
                  <a href="#operation--customers--cid--sites--siteid--overlays-get">GET</a>
                </td>
                <td>
                </td>
              </tr>
              <tr>
                <td>
                  <a href="#operation--customers--cid--sites--siteid--overlays-post">POST</a>
                </td>
                <td>
                </td>
              </tr>
              <tr>
                <td className="swagger--summary-path" rowSpan={3}>
                  <a href="#path--customers--cid--sites--siteid--overlays--oid-">/customers/{'{'}cid{'}'}/sites/{'{'}siteid{'}'}/overlays/{'{'}oid{'}'}</a>
                </td>
                <td>
                  <a href="#operation--customers--cid--sites--siteid--overlays--oid--delete">DELETE</a>
                </td>
                <td>
                </td>
              </tr>
              <tr>
                <td>
                  <a href="#operation--customers--cid--sites--siteid--overlays--oid--get">GET</a>
                </td>
                <td>
                </td>
              </tr>
              <tr>
                <td>
                  <a href="#operation--customers--cid--sites--siteid--overlays--oid--post">POST</a>
                </td>
                <td>
                </td>
              </tr>
              <tr>
                <td className="swagger--summary-path" rowSpan={1}>
                  <a href="#path--data--which--of--type--from--from--id--id--since--since--to--to-">/data/{'{'}which{'}'}/of/{'{'}type{'}'}/from/{'{'}from{'}'}/id/{'{'}id{'}'}/since/{'{'}since{'}'}/to/{'{'}to{'}'}</a>
                </td>
                <td>
                  <a href="#operation--data--which--of--type--from--from--id--id--since--since--to--to--get">GET</a>
                </td>
                <td>
                </td>
              </tr>
              <tr>
                <td className="swagger--summary-path" rowSpan={2}>
                  <a href="#path--firmwares">/firmwares</a>
                </td>
                <td>
                  <a href="#operation--firmwares-get">GET</a>
                </td>
                <td>
                </td>
              </tr>
              <tr>
                <td>
                  <a href="#operation--firmwares-post">POST</a>
                </td>
                <td>
                </td>
              </tr>
              <tr>
                <td className="swagger--summary-path" rowSpan={1}>
                  <a href="#path--firmwares-assign--id-">/firmwares/assign/{'{'}id{'}'}</a>
                </td>
                <td>
                  <a href="#operation--firmwares-assign--id--put">PUT</a>
                </td>
                <td>
                </td>
              </tr>
              <tr>
                <td className="swagger--summary-path" rowSpan={1}>
                  <a href="#path--firmwares-download--id-">/firmwares/download/{'{'}id{'}'}</a>
                </td>
                <td>
                  <a href="#operation--firmwares-download--id--get">GET</a>
                </td>
                <td>
                </td>
              </tr>
              <tr>
                <td className="swagger--summary-path" rowSpan={1}>
                  <a href="#path--firmwares-revert--id-">/firmwares/revert/{'{'}id{'}'}</a>
                </td>
                <td>
                  <a href="#operation--firmwares-revert--id--put">PUT</a>
                </td>
                <td>
                </td>
              </tr>
              <tr>
                <td className="swagger--summary-path" rowSpan={3}>
                  <a href="#path--firmwares--id-">/firmwares/{'{'}id{'}'}</a>
                </td>
                <td>
                  <a href="#operation--firmwares--id--delete">DELETE</a>
                </td>
                <td>
                </td>
              </tr>
              <tr>
                <td>
                  <a href="#operation--firmwares--id--get">GET</a>
                </td>
                <td>
                </td>
              </tr>
              <tr>
                <td>
                  <a href="#operation--firmwares--id--put">PUT</a>
                </td>
                <td>
                </td>
              </tr>
              <tr>
                <td className="swagger--summary-path" rowSpan={1}>
                  <a href="#path--lightcontrol-group--gid--level--lvl-">/lightcontrol/groups/{'{'}gid{'}'}/level/{'{'}lvl{'}'}</a>
                </td>
                <td>
                  <a href="#operation--lightcontrol-group--gid--level--lvl--post">POST</a>
                </td>
                <td>
                </td>
              </tr>
              <tr>
                <td className="swagger--summary-path" rowSpan={1}>
                  <a href="#path--lightcontrol-node--nid--level--lvl-">/lightcontrol/node/{'{'}nid{'}'}/level/{'{'}lvl{'}'}</a>
                </td>
                <td>
                  <a href="#operation--lightcontrol-node--nid--level--lvl--post">POST</a>
                </td>
                <td>
                </td>
              </tr>
              <tr>
                <td className="swagger--summary-path" rowSpan={1}>
                  <a href="#path--lightcontrol-nodes-level--lvl-">/lightcontrol/nodes/level/{'{'}lvl{'}'}</a>
                </td>
                <td>
                  <a href="#operation--lightcontrol-nodes-level--lvl--post">POST</a>
                </td>
                <td>
                </td>
              </tr>
              <tr>
                <td className="swagger--summary-path" rowSpan={1}>
                  <a href="#path--lightcontrol-site--sid--level--lvl-">/lightcontrol/site/{'{'}sid{'}'}/level/{'{'}lvl{'}'}</a>
                </td>
                <td>
                  <a href="#operation--lightcontrol-site--sid--level--lvl--post">POST</a>
                </td>
                <td>
                </td>
              </tr>
              <tr>
                <td className="swagger--summary-path" rowSpan={4}>
                  <a href="#path--lightingpolicy--lpid-">/lightingpolicy/{'{'}lpid{'}'}</a>
                </td>
                <td>
                  <a href="#operation--lightingpolicy--lpid--delete">DELETE</a>
                </td>
                <td>
                </td>
              </tr>
              <tr>
                <td>
                  <a href="#operation--lightingpolicy--lpid--get">GET</a>
                </td>
                <td>
                </td>
              </tr>
              <tr>
                <td>
                  <a href="#operation--lightingpolicy--lpid--post">POST</a>
                </td>
                <td>
                </td>
              </tr>
              <tr>
                <td>
                  <a href="#operation--lightingpolicy--lpid--put">PUT</a>
                </td>
                <td>
                </td>
              </tr>
              <tr>
                <td className="swagger--summary-path" rowSpan={2}>
                  <a href="#path--login">/login</a>
                </td>
                <td>
                  <a href="#operation--login-get">GET</a>
                </td>
                <td>
                </td>
              </tr>
              <tr>
                <td>
                  <a href="#operation--login-post">POST</a>
                </td>
                <td>
                </td>
              </tr>
              <tr>
                <td className="swagger--summary-path" rowSpan={1}>
                  <a href="#path--logout">/logout</a>
                </td>
                <td>
                  <a href="#operation--logout-get">GET</a>
                </td>
                <td>
                </td>
              </tr>
              <tr>
                <td className="swagger--summary-path" rowSpan={4}>
                  <a href="#path--schedules--sid-">/schedules/{'{'}sid{'}'}</a>
                </td>
                <td>
                  <a href="#operation--schedules--sid--delete">DELETE</a>
                </td>
                <td>
                </td>
              </tr>
              <tr>
                <td>
                  <a href="#operation--schedules--sid--get">GET</a>
                </td>
                <td>
                </td>
              </tr>
              <tr>
                <td>
                  <a href="#operation--schedules--sid--post">POST</a>
                </td>
                <td>
                </td>
              </tr>
              <tr>
                <td>
                  <a href="#operation--schedules--sid--put">PUT</a>
                </td>
                <td>
                </td>
              </tr>
              <tr>
                <td className="swagger--summary-path" rowSpan={1}>
                  <a href="#path--user">/user</a>
                </td>
                <td>
                  <a href="#operation--user-post">POST</a>
                </td>
                <td>
                </td>
              </tr>
              <tr>
                <td className="swagger--summary-path" rowSpan={3}>
                  <a href="#path--user--uid-">/user/{'{'}uid{'}'}</a>
                </td>
                <td>
                  <a href="#operation--user--uid--delete">DELETE</a>
                </td>
                <td>
                </td>
              </tr>
              <tr>
                <td>
                  <a href="#operation--user--uid--get">GET</a>
                </td>
                <td>
                </td>
              </tr>
              <tr>
                <td>
                  <a href="#operation--user--uid--put">PUT</a>
                </td>
                <td>
                </td>
              </tr>
            </tbody>
          </table>
          <h2>Paths</h2>
          <span id="path--audits-from--datemin--to--datemax--" />
          <div id="operation--audits-from--datemin--to--datemax---get" className="swagger--panel-operation-get panel">
            <div className="panel-heading">
              <div className="operation-summary" />
              <h3 className="panel-title"><span className="operation-name">GET</span> <strong>/audits/from/{'{'}datemin{'}'}/to/{'{'}datemax{'}'}/</strong></h3>
            </div>
            <div className="panel-body">
              <section className="sw-operation-description">
                <p>Returns all audits information for a site</p>
              </section>
              <section className="sw-request-params">
                <table className="table">
                  <thead>
                    <tr>
                      <th className="sw-param-name" />
                      <th className="sw-param-description" />
                      <th className="sw-param-type" />
                      <th className="sw-param-data-type" />
                      <th className="sw-param-annotation" />
                    </tr>
                  </thead>
                  <tbody>
                    <tr>
                      <td>
                        datemin
                      </td><td />
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                    <tr>
                      <td>
                        datemax
                      </td><td />
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                  </tbody>
                </table>
              </section>
              <section className="sw-responses">
                <p><a href="#sw-default-produces">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <dl>
                  <dt className="sw-response-200">
                    200 OK
                  </dt>
                  <dd className="sw-response-200">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Success</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <section className="json-schema-array-items">
                              <span className="json-property-type">    <a className="json-schema-ref" href="#/definitions/Audit">Audit</a>
                              </span>
                              <span className="json-property-range" title="Value limits" />
                              <div className="json-inner-schema">
                              </div>
                            </section>  </div>
                        </div></div>
                    </div>              </dd>
                  <dt className="sw-response-default">
                    default 
                  </dt>
                  <dd className="sw-response-default">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Error</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                </dl>
              </section>
            </div>
          </div>    
          <span id="path--customers" />
          <div id="operation--customers-get" className="swagger--panel-operation-get panel">
            <div className="panel-heading">
              <div className="operation-summary" />
              <h3 className="panel-title"><span className="operation-name">GET</span> <strong>/customers</strong></h3>
            </div>
            <div className="panel-body">
              <section className="sw-operation-description">
                <p>Returns all customer information to the caller</p>
              </section>
              <section className="sw-responses">
                <p><a href="#sw-default-produces">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <dl>
                  <dt className="sw-response-200">
                    200 OK
                  </dt>
                  <dd className="sw-response-200">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Success</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <section className="json-schema-array-items">
                              <span className="json-property-type">    <a className="json-schema-ref" href="#/definitions/Customer">Customer</a>
                              </span>
                              <span className="json-property-range" title="Value limits" />
                              <div className="json-inner-schema">
                              </div>
                            </section>  </div>
                        </div></div>
                    </div>              </dd>
                  <dt className="sw-response-default">
                    default 
                  </dt>
                  <dd className="sw-response-default">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Error</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                </dl>
              </section>
            </div>
          </div>    <div id="operation--customers-post" className="swagger--panel-operation-post panel">
            <div className="panel-heading">
              <div className="operation-summary" />
              <h3 className="panel-title"><span className="operation-name">POST</span> <strong>/customers</strong></h3>
            </div>
            <div className="panel-body">
              <section className="sw-operation-description">
                <p>Add new customer</p>
              </section>
              <section className="sw-request-body">
                <p><a href="#sw-default-consumes">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <div className="row">
                  <div className="col-md-6">
                    <p /><p>New customer info</p>
                    <p />
                  </div>
                  <div className="col-md-6 sw-request-model">
                    <div className="panel panel-definition">
                      <div className="panel-body">
                        <a className="json-schema-ref" href="#/definitions/NewCustomer">NewCustomer</a>
                      </div>
                    </div></div>
                </div>
              </section>        
              <section className="sw-responses">
                <p><a href="#sw-default-produces">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <dl>
                  <dt className="sw-response-200">
                    200 OK
                  </dt>
                  <dd className="sw-response-200">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Success</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/Customer">Customer</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                  <dt className="sw-response-default">
                    default 
                  </dt>
                  <dd className="sw-response-default">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Error</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                </dl>
              </section>
            </div>
          </div>    
          <span id="path--customers--cid-" />
          <div id="operation--customers--cid--delete" className="swagger--panel-operation-delete panel">
            <div className="panel-heading">
              <div className="operation-summary" />
              <h3 className="panel-title"><span className="operation-name">DELETE</span> <strong>/customers/{'{'}cid{'}'}</strong></h3>
            </div>
            <div className="panel-body">
              <section className="sw-operation-description">
                <p>Delete a customer based on ID</p>
              </section>
              <section className="sw-request-params">
                <table className="table">
                  <thead>
                    <tr>
                      <th className="sw-param-name" />
                      <th className="sw-param-description" />
                      <th className="sw-param-type" />
                      <th className="sw-param-data-type" />
                      <th className="sw-param-annotation" />
                    </tr>
                  </thead>
                  <tbody>
                    <tr>
                      <td>
                        cid
                      </td><td><p>The customer ID</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                  </tbody>
                </table>
              </section>
              <section className="sw-responses">
                <p><a href="#sw-default-produces">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <dl>
                  <dt className="sw-response-204">
                    204 No Content
                  </dt>
                  <dd className="sw-response-204">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Success</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                      </div>
                    </div>              </dd>
                  <dt className="sw-response-default">
                    default 
                  </dt>
                  <dd className="sw-response-default">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Error</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                </dl>
              </section>
            </div>
          </div>    <div id="operation--customers--cid--get" className="swagger--panel-operation-get panel">
            <div className="panel-heading">
              <div className="operation-summary" />
              <h3 className="panel-title"><span className="operation-name">GET</span> <strong>/customers/{'{'}cid{'}'}</strong></h3>
            </div>
            <div className="panel-body">
              <section className="sw-operation-description">
                <p>Returns customer information to the caller</p>
              </section>
              <section className="sw-request-params">
                <table className="table">
                  <thead>
                    <tr>
                      <th className="sw-param-name" />
                      <th className="sw-param-description" />
                      <th className="sw-param-type" />
                      <th className="sw-param-data-type" />
                      <th className="sw-param-annotation" />
                    </tr>
                  </thead>
                  <tbody>
                    <tr>
                      <td>
                        cid
                      </td><td><p>The customer ID</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                  </tbody>
                </table>
              </section>
              <section className="sw-responses">
                <p><a href="#sw-default-produces">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <dl>
                  <dt className="sw-response-200">
                    200 OK
                  </dt>
                  <dd className="sw-response-200">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Success</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/Customer">Customer</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                  <dt className="sw-response-404">
                    404 Not Found
                  </dt>
                  <dd className="sw-response-404">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Customer Not Found</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                  <dt className="sw-response-default">
                    default 
                  </dt>
                  <dd className="sw-response-default">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Error</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                </dl>
              </section>
            </div>
          </div>    <div id="operation--customers--cid--post" className="swagger--panel-operation-post panel">
            <div className="panel-heading">
              <div className="operation-summary" />
              <h3 className="panel-title"><span className="operation-name">POST</span> <strong>/customers/{'{'}cid{'}'}</strong></h3>
            </div>
            <div className="panel-body">
              <section className="sw-operation-description">
                <p>Update a customer</p>
              </section>
              <section className="sw-request-body">
                <p><a href="#sw-default-consumes">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <div className="row">
                  <div className="col-md-6">
                    <p /><p>Updated customer info</p>
                    <p />
                  </div>
                  <div className="col-md-6 sw-request-model">
                    <div className="panel panel-definition">
                      <div className="panel-body">
                        <a className="json-schema-ref" href="#/definitions/NewCustomer">NewCustomer</a>
                      </div>
                    </div></div>
                </div>
              </section>        
              <section className="sw-request-params">
                <table className="table">
                  <thead>
                    <tr>
                      <th className="sw-param-name" />
                      <th className="sw-param-description" />
                      <th className="sw-param-type" />
                      <th className="sw-param-data-type" />
                      <th className="sw-param-annotation" />
                    </tr>
                  </thead>
                  <tbody>
                    <tr>
                      <td>
                        cid
                      </td><td><p>The customer ID</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                  </tbody>
                </table>
              </section>
              <section className="sw-responses">
                <p><a href="#sw-default-produces">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <dl>
                  <dt className="sw-response-200">
                    200 OK
                  </dt>
                  <dd className="sw-response-200">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Success</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/Customer">Customer</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                  <dt className="sw-response-default">
                    default 
                  </dt>
                  <dd className="sw-response-default">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Error</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                </dl>
              </section>
            </div>
          </div>    
          <span id="path--customers--cid--sites" />
          <div id="operation--customers--cid--sites-get" className="swagger--panel-operation-get panel">
            <div className="panel-heading">
              <div className="operation-summary" />
              <h3 className="panel-title"><span className="operation-name">GET</span> <strong>/customers/{'{'}cid{'}'}/sites</strong></h3>
            </div>
            <div className="panel-body">
              <section className="sw-operation-description">
                <p>Returns all site information for a customer</p>
              </section>
              <section className="sw-request-params">
                <table className="table">
                  <thead>
                    <tr>
                      <th className="sw-param-name" />
                      <th className="sw-param-description" />
                      <th className="sw-param-type" />
                      <th className="sw-param-data-type" />
                      <th className="sw-param-annotation" />
                    </tr>
                  </thead>
                  <tbody>
                    <tr>
                      <td>
                        cid
                      </td><td><p>The customer ID</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                  </tbody>
                </table>
              </section>
              <section className="sw-responses">
                <p><a href="#sw-default-produces">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <dl>
                  <dt className="sw-response-200">
                    200 OK
                  </dt>
                  <dd className="sw-response-200">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Success</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <section className="json-schema-array-items">
                              <span className="json-property-type">    <a className="json-schema-ref" href="#/definitions/Site">Site</a>
                              </span>
                              <span className="json-property-range" title="Value limits" />
                              <div className="json-inner-schema">
                              </div>
                            </section>  </div>
                        </div></div>
                    </div>              </dd>
                  <dt className="sw-response-default">
                    default 
                  </dt>
                  <dd className="sw-response-default">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Error</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                </dl>
              </section>
            </div>
          </div>    <div id="operation--customers--cid--sites-post" className="swagger--panel-operation-post panel">
            <div className="panel-heading">
              <div className="operation-summary" />
              <h3 className="panel-title"><span className="operation-name">POST</span> <strong>/customers/{'{'}cid{'}'}/sites</strong></h3>
            </div>
            <div className="panel-body">
              <section className="sw-operation-description">
                <p>Add new Site</p>
              </section>
              <section className="sw-request-body">
                <p><a href="#sw-default-consumes">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <div className="row">
                  <div className="col-md-6">
                    <p /><p>New site info</p>
                    <p />
                  </div>
                  <div className="col-md-6 sw-request-model">
                    <div className="panel panel-definition">
                      <div className="panel-body">
                        <a className="json-schema-ref" href="#/definitions/NewSite">NewSite</a>
                      </div>
                    </div></div>
                </div>
              </section>        
              <section className="sw-request-params">
                <table className="table">
                  <thead>
                    <tr>
                      <th className="sw-param-name" />
                      <th className="sw-param-description" />
                      <th className="sw-param-type" />
                      <th className="sw-param-data-type" />
                      <th className="sw-param-annotation" />
                    </tr>
                  </thead>
                  <tbody>
                    <tr>
                      <td>
                        cid
                      </td><td><p>The customer ID</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                  </tbody>
                </table>
              </section>
              <section className="sw-responses">
                <p><a href="#sw-default-produces">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <dl>
                  <dt className="sw-response-200">
                    200 OK
                  </dt>
                  <dd className="sw-response-200">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Success</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/Site">Site</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                  <dt className="sw-response-default">
                    default 
                  </dt>
                  <dd className="sw-response-default">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Error</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                </dl>
              </section>
            </div>
          </div>    
          <span id="path--customers--cid--sites--sid-" />
          <div id="operation--customers--cid--sites--sid--delete" className="swagger--panel-operation-delete panel">
            <div className="panel-heading">
              <div className="operation-summary" />
              <h3 className="panel-title"><span className="operation-name">DELETE</span> <strong>/customers/{'{'}cid{'}'}/sites/{'{'}sid{'}'}</strong></h3>
            </div>
            <div className="panel-body">
              <section className="sw-operation-description">
                <p>Delete a site based on ID</p>
              </section>
              <section className="sw-request-params">
                <table className="table">
                  <thead>
                    <tr>
                      <th className="sw-param-name" />
                      <th className="sw-param-description" />
                      <th className="sw-param-type" />
                      <th className="sw-param-data-type" />
                      <th className="sw-param-annotation" />
                    </tr>
                  </thead>
                  <tbody>
                    <tr>
                      <td>
                        cid
                      </td><td><p>The customer ID</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                    <tr>
                      <td>
                        sid
                      </td><td><p>The site ID</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                  </tbody>
                </table>
              </section>
              <section className="sw-responses">
                <p><a href="#sw-default-produces">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <dl>
                  <dt className="sw-response-204">
                    204 No Content
                  </dt>
                  <dd className="sw-response-204">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Success</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                      </div>
                    </div>              </dd>
                  <dt className="sw-response-default">
                    default 
                  </dt>
                  <dd className="sw-response-default">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Error</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                </dl>
              </section>
            </div>
          </div>    <div id="operation--customers--cid--sites--sid--get" className="swagger--panel-operation-get panel">
            <div className="panel-heading">
              <div className="operation-summary" />
              <h3 className="panel-title"><span className="operation-name">GET</span> <strong>/customers/{'{'}cid{'}'}/sites/{'{'}sid{'}'}</strong></h3>
            </div>
            <div className="panel-body">
              <section className="sw-operation-description">
                <p>Returns site information to the caller</p>
              </section>
              <section className="sw-request-params">
                <table className="table">
                  <thead>
                    <tr>
                      <th className="sw-param-name" />
                      <th className="sw-param-description" />
                      <th className="sw-param-type" />
                      <th className="sw-param-data-type" />
                      <th className="sw-param-annotation" />
                    </tr>
                  </thead>
                  <tbody>
                    <tr>
                      <td>
                        cid
                      </td><td><p>The customer ID</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                    <tr>
                      <td>
                        sid
                      </td><td><p>The site ID</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                  </tbody>
                </table>
              </section>
              <section className="sw-responses">
                <p><a href="#sw-default-produces">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <dl>
                  <dt className="sw-response-200">
                    200 OK
                  </dt>
                  <dd className="sw-response-200">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Success</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/Site">Site</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                  <dt className="sw-response-404">
                    404 Not Found
                  </dt>
                  <dd className="sw-response-404">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Site Not Found</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                  <dt className="sw-response-default">
                    default 
                  </dt>
                  <dd className="sw-response-default">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Error</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                </dl>
              </section>
            </div>
          </div>    <div id="operation--customers--cid--sites--sid--put" className="swagger--panel-operation-put panel">
            <div className="panel-heading">
              <div className="operation-summary" />
              <h3 className="panel-title"><span className="operation-name">PUT</span> <strong>/customers/{'{'}cid{'}'}/sites/{'{'}sid{'}'}</strong></h3>
            </div>
            <div className="panel-body">
              <section className="sw-operation-description">
                <p>Update a site</p>
              </section>
              <section className="sw-request-body">
                <p><a href="#sw-default-consumes">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <div className="row">
                  <div className="col-md-6">
                    <p /><p>Updated site info</p>
                    <p />
                  </div>
                  <div className="col-md-6 sw-request-model">
                    <div className="panel panel-definition">
                      <div className="panel-body">
                        <a className="json-schema-ref" href="#/definitions/NewSite">NewSite</a>
                      </div>
                    </div></div>
                </div>
              </section>        
              <section className="sw-request-params">
                <table className="table">
                  <thead>
                    <tr>
                      <th className="sw-param-name" />
                      <th className="sw-param-description" />
                      <th className="sw-param-type" />
                      <th className="sw-param-data-type" />
                      <th className="sw-param-annotation" />
                    </tr>
                  </thead>
                  <tbody>
                    <tr>
                      <td>
                        cid
                      </td><td><p>The customer ID</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                    <tr>
                      <td>
                        sid
                      </td><td><p>The site ID</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                  </tbody>
                </table>
              </section>
              <section className="sw-responses">
                <p><a href="#sw-default-produces">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <dl>
                  <dt className="sw-response-200">
                    200 OK
                  </dt>
                  <dd className="sw-response-200">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Success</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/Site">Site</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                  <dt className="sw-response-default">
                    default 
                  </dt>
                  <dd className="sw-response-default">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Error</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                </dl>
              </section>
            </div>
          </div>    
          <span id="path--customers--cid--sites--sid--nodes" />
          <div id="operation--customers--cid--sites--sid--nodes-get" className="swagger--panel-operation-get panel">
            <div className="panel-heading">
              <div className="operation-summary" />
              <h3 className="panel-title"><span className="operation-name">GET</span> <strong>/customers/{'{'}cid{'}'}/sites/{'{'}sid{'}'}/nodes</strong></h3>
            </div>
            <div className="panel-body">
              <section className="sw-operation-description">
                <p>Returns all node information for a site</p>
              </section>
              <section className="sw-request-params">
                <table className="table">
                  <thead>
                    <tr>
                      <th className="sw-param-name" />
                      <th className="sw-param-description" />
                      <th className="sw-param-type" />
                      <th className="sw-param-data-type" />
                      <th className="sw-param-annotation" />
                    </tr>
                  </thead>
                  <tbody>
                    <tr>
                      <td>
                        cid
                      </td><td><p>The customer ID</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                    <tr>
                      <td>
                        sid
                      </td><td><p>The site ID</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                  </tbody>
                </table>
              </section>
              <section className="sw-responses">
                <p><a href="#sw-default-produces">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <dl>
                  <dt className="sw-response-200">
                    200 OK
                  </dt>
                  <dd className="sw-response-200">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Success</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <section className="json-schema-array-items">
                              <span className="json-property-type">    <a className="json-schema-ref" href="#/definitions/Node">Node</a>
                              </span>
                              <span className="json-property-range" title="Value limits" />
                              <div className="json-inner-schema">
                              </div>
                            </section>  </div>
                        </div></div>
                    </div>              </dd>
                  <dt className="sw-response-default">
                    default 
                  </dt>
                  <dd className="sw-response-default">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Error</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                </dl>
              </section>
            </div>
          </div>    <div id="operation--customers--cid--sites--sid--nodes-post" className="swagger--panel-operation-post panel">
            <div className="panel-heading">
              <div className="operation-summary" />
              <h3 className="panel-title"><span className="operation-name">POST</span> <strong>/customers/{'{'}cid{'}'}/sites/{'{'}sid{'}'}/nodes</strong></h3>
            </div>
            <div className="panel-body">
              <section className="sw-operation-description">
                <p>Add new Node</p>
              </section>
              <section className="sw-request-body">
                <p><a href="#sw-default-consumes">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <div className="row">
                  <div className="col-md-6">
                    <p /><p>New node info</p>
                    <p />
                  </div>
                  <div className="col-md-6 sw-request-model">
                    <div className="panel panel-definition">
                      <div className="panel-body">
                        <a className="json-schema-ref" href="#/definitions/Node">Node</a>
                      </div>
                    </div></div>
                </div>
              </section>        
              <section className="sw-request-params">
                <table className="table">
                  <thead>
                    <tr>
                      <th className="sw-param-name" />
                      <th className="sw-param-description" />
                      <th className="sw-param-type" />
                      <th className="sw-param-data-type" />
                      <th className="sw-param-annotation" />
                    </tr>
                  </thead>
                  <tbody>
                    <tr>
                      <td>
                        cid
                      </td><td><p>The customer ID</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                    <tr>
                      <td>
                        sid
                      </td><td><p>The site ID</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                  </tbody>
                </table>
              </section>
              <section className="sw-responses">
                <p><a href="#sw-default-produces">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <dl>
                  <dt className="sw-response-200">
                    200 OK
                  </dt>
                  <dd className="sw-response-200">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Success</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/Node">Node</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                  <dt className="sw-response-default">
                    default 
                  </dt>
                  <dd className="sw-response-default">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Error</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                </dl>
              </section>
            </div>
          </div>    
          <span id="path--customers--cid--sites--sid--nodes--nid-" />
          <div id="operation--customers--cid--sites--sid--nodes--nid--delete" className="swagger--panel-operation-delete panel">
            <div className="panel-heading">
              <div className="operation-summary" />
              <h3 className="panel-title"><span className="operation-name">DELETE</span> <strong>/customers/{'{'}cid{'}'}/sites/{'{'}sid{'}'}/nodes/{'{'}nid{'}'}</strong></h3>
            </div>
            <div className="panel-body">
              <section className="sw-operation-description">
                <p>Delete a node based on ID</p>
              </section>
              <section className="sw-request-params">
                <table className="table">
                  <thead>
                    <tr>
                      <th className="sw-param-name" />
                      <th className="sw-param-description" />
                      <th className="sw-param-type" />
                      <th className="sw-param-data-type" />
                      <th className="sw-param-annotation" />
                    </tr>
                  </thead>
                  <tbody>
                    <tr>
                      <td>
                        cid
                      </td><td><p>The customer ID</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                    <tr>
                      <td>
                        sid
                      </td><td><p>The site ID</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                    <tr>
                      <td>
                        nid
                      </td><td><p>The node ID</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                  </tbody>
                </table>
              </section>
              <section className="sw-responses">
                <p><a href="#sw-default-produces">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <dl>
                  <dt className="sw-response-204">
                    204 No Content
                  </dt>
                  <dd className="sw-response-204">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Success</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                      </div>
                    </div>              </dd>
                  <dt className="sw-response-default">
                    default 
                  </dt>
                  <dd className="sw-response-default">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Error</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                </dl>
              </section>
            </div>
          </div>    <div id="operation--customers--cid--sites--sid--nodes--nid--get" className="swagger--panel-operation-get panel">
            <div className="panel-heading">
              <div className="operation-summary" />
              <h3 className="panel-title"><span className="operation-name">GET</span> <strong>/customers/{'{'}cid{'}'}/sites/{'{'}sid{'}'}/nodes/{'{'}nid{'}'}</strong></h3>
            </div>
            <div className="panel-body">
              <section className="sw-operation-description">
                <p>Returns node information to the caller</p>
              </section>
              <section className="sw-request-params">
                <table className="table">
                  <thead>
                    <tr>
                      <th className="sw-param-name" />
                      <th className="sw-param-description" />
                      <th className="sw-param-type" />
                      <th className="sw-param-data-type" />
                      <th className="sw-param-annotation" />
                    </tr>
                  </thead>
                  <tbody>
                    <tr>
                      <td>
                        cid
                      </td><td><p>The customer ID</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                    <tr>
                      <td>
                        sid
                      </td><td><p>The site ID</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                    <tr>
                      <td>
                        nid
                      </td><td><p>The node ID</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                  </tbody>
                </table>
              </section>
              <section className="sw-responses">
                <p><a href="#sw-default-produces">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <dl>
                  <dt className="sw-response-200">
                    200 OK
                  </dt>
                  <dd className="sw-response-200">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Success</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/Node">Node</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                  <dt className="sw-response-404">
                    404 Not Found
                  </dt>
                  <dd className="sw-response-404">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Node Not Found</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                  <dt className="sw-response-default">
                    default 
                  </dt>
                  <dd className="sw-response-default">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Error</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                </dl>
              </section>
            </div>
          </div>    <div id="operation--customers--cid--sites--sid--nodes--nid--put" className="swagger--panel-operation-put panel">
            <div className="panel-heading">
              <div className="operation-summary" />
              <h3 className="panel-title"><span className="operation-name">PUT</span> <strong>/customers/{'{'}cid{'}'}/sites/{'{'}sid{'}'}/nodes/{'{'}nid{'}'}</strong></h3>
            </div>
            <div className="panel-body">
              <section className="sw-operation-description">
                <p>Update a node</p>
              </section>
              <section className="sw-request-body">
                <p><a href="#sw-default-consumes">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <div className="row">
                  <div className="col-md-6">
                    <p /><p>Updated node info</p>
                    <p />
                  </div>
                  <div className="col-md-6 sw-request-model">
                    <div className="panel panel-definition">
                      <div className="panel-body">
                        <a className="json-schema-ref" href="#/definitions/Node">Node</a>
                      </div>
                    </div></div>
                </div>
              </section>        
              <section className="sw-request-params">
                <table className="table">
                  <thead>
                    <tr>
                      <th className="sw-param-name" />
                      <th className="sw-param-description" />
                      <th className="sw-param-type" />
                      <th className="sw-param-data-type" />
                      <th className="sw-param-annotation" />
                    </tr>
                  </thead>
                  <tbody>
                    <tr>
                      <td>
                        cid
                      </td><td><p>The customer ID</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                    <tr>
                      <td>
                        sid
                      </td><td><p>The site ID</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                    <tr>
                      <td>
                        nid
                      </td><td><p>The node ID</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                  </tbody>
                </table>
              </section>
              <section className="sw-responses">
                <p><a href="#sw-default-produces">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <dl>
                  <dt className="sw-response-200">
                    200 OK
                  </dt>
                  <dd className="sw-response-200">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Success</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/Node">Node</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                  <dt className="sw-response-default">
                    default 
                  </dt>
                  <dd className="sw-response-default">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Error</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                </dl>
              </section>
            </div>
          </div>    
          <span id="path--customers--cid--sites--siteid--notifications" />
          <div id="operation--customers--cid--sites--siteid--notifications-get" className="swagger--panel-operation-get panel">
            <div className="panel-heading">
              <div className="operation-summary" />
              <h3 className="panel-title"><span className="operation-name">GET</span> <strong>/customers/{'{'}cid{'}'}/sites/{'{'}siteid{'}'}/notifications</strong></h3>
            </div>
            <div className="panel-body">
              <section className="sw-operation-description">
                <p>Returns all notifications information for a site</p>
              </section>
              <section className="sw-request-params">
                <table className="table">
                  <thead>
                    <tr>
                      <th className="sw-param-name" />
                      <th className="sw-param-description" />
                      <th className="sw-param-type" />
                      <th className="sw-param-data-type" />
                      <th className="sw-param-annotation" />
                    </tr>
                  </thead>
                  <tbody>
                    <tr>
                      <td>
                        cid
                      </td><td><p>The customer ID</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                    <tr>
                      <td>
                        siteid
                      </td><td><p>The site ID</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                  </tbody>
                </table>
              </section>
              <section className="sw-responses">
                <p><a href="#sw-default-produces">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <dl>
                  <dt className="sw-response-200">
                    200 OK
                  </dt>
                  <dd className="sw-response-200">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Success</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <section className="json-schema-array-items">
                              <span className="json-property-type">    <a className="json-schema-ref" href="#/definitions/Notification">Notification</a>
                              </span>
                              <span className="json-property-range" title="Value limits" />
                              <div className="json-inner-schema">
                              </div>
                            </section>  </div>
                        </div></div>
                    </div>              </dd>
                  <dt className="sw-response-default">
                    default 
                  </dt>
                  <dd className="sw-response-default">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Error</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                </dl>
              </section>
            </div>
          </div>    <div id="operation--customers--cid--sites--siteid--notifications-post" className="swagger--panel-operation-post panel">
            <div className="panel-heading">
              <div className="operation-summary" />
              <h3 className="panel-title"><span className="operation-name">POST</span> <strong>/customers/{'{'}cid{'}'}/sites/{'{'}siteid{'}'}/notifications</strong></h3>
            </div>
            <div className="panel-body">
              <section className="sw-operation-description">
                <p>Add new Notification</p>
              </section>
              <section className="sw-request-body">
                <p><a href="#sw-default-consumes">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <div className="row">
                  <div className="col-md-6">
                    <p /><p>New Notification info</p>
                    <p />
                  </div>
                  <div className="col-md-6 sw-request-model">
                    <div className="panel panel-definition">
                      <div className="panel-body">
                        <a className="json-schema-ref" href="#/definitions/NewNotification">NewNotification</a>
                      </div>
                    </div></div>
                </div>
              </section>        
              <section className="sw-request-params">
                <table className="table">
                  <thead>
                    <tr>
                      <th className="sw-param-name" />
                      <th className="sw-param-description" />
                      <th className="sw-param-type" />
                      <th className="sw-param-data-type" />
                      <th className="sw-param-annotation" />
                    </tr>
                  </thead>
                  <tbody>
                    <tr>
                      <td>
                        cid
                      </td><td><p>The customer ID</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                    <tr>
                      <td>
                        siteid
                      </td><td><p>The site ID</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                  </tbody>
                </table>
              </section>
              <section className="sw-responses">
                <p><a href="#sw-default-produces">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <dl>
                  <dt className="sw-response-200">
                    200 OK
                  </dt>
                  <dd className="sw-response-200">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Success</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/Notification">Notification</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                  <dt className="sw-response-default">
                    default 
                  </dt>
                  <dd className="sw-response-default">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Error</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                </dl>
              </section>
            </div>
          </div>    
          <span id="path--customers--cid--sites--siteid--notifications--notificationid-" />
          <div id="operation--customers--cid--sites--siteid--notifications--notificationid--delete" className="swagger--panel-operation-delete panel">
            <div className="panel-heading">
              <div className="operation-summary" />
              <h3 className="panel-title"><span className="operation-name">DELETE</span> <strong>/customers/{'{'}cid{'}'}/sites/{'{'}siteid{'}'}/notifications/{'{'}notificationid{'}'}</strong></h3>
            </div>
            <div className="panel-body">
              <section className="sw-operation-description">
                <p>Delete an notification based on ID</p>
              </section>
              <section className="sw-request-params">
                <table className="table">
                  <thead>
                    <tr>
                      <th className="sw-param-name" />
                      <th className="sw-param-description" />
                      <th className="sw-param-type" />
                      <th className="sw-param-data-type" />
                      <th className="sw-param-annotation" />
                    </tr>
                  </thead>
                  <tbody>
                    <tr>
                      <td>
                        cid
                      </td><td><p>The customer ID</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                    <tr>
                      <td>
                        siteid
                      </td><td><p>The site ID</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                    <tr>
                      <td>
                        notificationid
                      </td><td><p>The notification ID</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                  </tbody>
                </table>
              </section>
              <section className="sw-responses">
                <p><a href="#sw-default-produces">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <dl>
                  <dt className="sw-response-204">
                    204 No Content
                  </dt>
                  <dd className="sw-response-204">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Success</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                      </div>
                    </div>              </dd>
                  <dt className="sw-response-default">
                    default 
                  </dt>
                  <dd className="sw-response-default">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Error</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                </dl>
              </section>
            </div>
          </div>    <div id="operation--customers--cid--sites--siteid--notifications--notificationid--get" className="swagger--panel-operation-get panel">
            <div className="panel-heading">
              <div className="operation-summary" />
              <h3 className="panel-title"><span className="operation-name">GET</span> <strong>/customers/{'{'}cid{'}'}/sites/{'{'}siteid{'}'}/notifications/{'{'}notificationid{'}'}</strong></h3>
            </div>
            <div className="panel-body">
              <section className="sw-operation-description">
                <p>Returns notification information to the caller</p>
              </section>
              <section className="sw-request-params">
                <table className="table">
                  <thead>
                    <tr>
                      <th className="sw-param-name" />
                      <th className="sw-param-description" />
                      <th className="sw-param-type" />
                      <th className="sw-param-data-type" />
                      <th className="sw-param-annotation" />
                    </tr>
                  </thead>
                  <tbody>
                    <tr>
                      <td>
                        cid
                      </td><td><p>The customer ID</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                    <tr>
                      <td>
                        siteid
                      </td><td><p>The site ID</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                    <tr>
                      <td>
                        notificationid
                      </td><td><p>The notification ID</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                  </tbody>
                </table>
              </section>
              <section className="sw-responses">
                <p><a href="#sw-default-produces">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <dl>
                  <dt className="sw-response-200">
                    200 OK
                  </dt>
                  <dd className="sw-response-200">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Success</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/Notification">Notification</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                  <dt className="sw-response-404">
                    404 Not Found
                  </dt>
                  <dd className="sw-response-404">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Notification Not Found</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                  <dt className="sw-response-default">
                    default 
                  </dt>
                  <dd className="sw-response-default">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Error</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                </dl>
              </section>
            </div>
          </div>    <div id="operation--customers--cid--sites--siteid--notifications--notificationid--post" className="swagger--panel-operation-post panel">
            <div className="panel-heading">
              <div className="operation-summary" />
              <h3 className="panel-title"><span className="operation-name">POST</span> <strong>/customers/{'{'}cid{'}'}/sites/{'{'}siteid{'}'}/notifications/{'{'}notificationid{'}'}</strong></h3>
            </div>
            <div className="panel-body">
              <section className="sw-operation-description">
                <p>Update a notification</p>
              </section>
              <section className="sw-request-body">
                <p><a href="#sw-default-consumes">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <div className="row">
                  <div className="col-md-6">
                    <p /><p>Updated notification info</p>
                    <p />
                  </div>
                  <div className="col-md-6 sw-request-model">
                    <div className="panel panel-definition">
                      <div className="panel-body">
                        <a className="json-schema-ref" href="#/definitions/NewNotification">NewNotification</a>
                      </div>
                    </div></div>
                </div>
              </section>        
              <section className="sw-request-params">
                <table className="table">
                  <thead>
                    <tr>
                      <th className="sw-param-name" />
                      <th className="sw-param-description" />
                      <th className="sw-param-type" />
                      <th className="sw-param-data-type" />
                      <th className="sw-param-annotation" />
                    </tr>
                  </thead>
                  <tbody>
                    <tr>
                      <td>
                        cid
                      </td><td><p>The customer ID</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                    <tr>
                      <td>
                        siteid
                      </td><td><p>The site ID</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                    <tr>
                      <td>
                        notificationid
                      </td><td><p>The notification ID</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                  </tbody>
                </table>
              </section>
              <section className="sw-responses">
                <p><a href="#sw-default-produces">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <dl>
                  <dt className="sw-response-200">
                    200 OK
                  </dt>
                  <dd className="sw-response-200">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Success</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/Notification">Notification</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                  <dt className="sw-response-default">
                    default 
                  </dt>
                  <dd className="sw-response-default">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Error</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                </dl>
              </section>
            </div>
          </div>    
          <span id="path--customers--cid--sites--siteid--overlays" />
          <div id="operation--customers--cid--sites--siteid--overlays-get" className="swagger--panel-operation-get panel">
            <div className="panel-heading">
              <div className="operation-summary" />
              <h3 className="panel-title"><span className="operation-name">GET</span> <strong>/customers/{'{'}cid{'}'}/sites/{'{'}siteid{'}'}/overlays</strong></h3>
            </div>
            <div className="panel-body">
              <section className="sw-operation-description">
                <p>Returns all overlay information for a site</p>
              </section>
              <section className="sw-request-params">
                <table className="table">
                  <thead>
                    <tr>
                      <th className="sw-param-name" />
                      <th className="sw-param-description" />
                      <th className="sw-param-type" />
                      <th className="sw-param-data-type" />
                      <th className="sw-param-annotation" />
                    </tr>
                  </thead>
                  <tbody>
                    <tr>
                      <td>
                        cid
                      </td><td><p>The customer ID</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                    <tr>
                      <td>
                        siteid
                      </td><td><p>The site ID</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                  </tbody>
                </table>
              </section>
              <section className="sw-responses">
                <p><a href="#sw-default-produces">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <dl>
                  <dt className="sw-response-200">
                    200 OK
                  </dt>
                  <dd className="sw-response-200">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Success</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <section className="json-schema-array-items">
                              <span className="json-property-type">    <a className="json-schema-ref" href="#/definitions/Overlay">Overlay</a>
                              </span>
                              <span className="json-property-range" title="Value limits" />
                              <div className="json-inner-schema">
                              </div>
                            </section>  </div>
                        </div></div>
                    </div>              </dd>
                  <dt className="sw-response-default">
                    default 
                  </dt>
                  <dd className="sw-response-default">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Error</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                </dl>
              </section>
            </div>
          </div>    <div id="operation--customers--cid--sites--siteid--overlays-post" className="swagger--panel-operation-post panel">
            <div className="panel-heading">
              <div className="operation-summary" />
              <h3 className="panel-title"><span className="operation-name">POST</span> <strong>/customers/{'{'}cid{'}'}/sites/{'{'}siteid{'}'}/overlays</strong></h3>
            </div>
            <div className="panel-body">
              <section className="sw-operation-description">
                <p>Add new Overlay</p>
              </section>
              <section className="sw-request-body">
                <p><a href="#sw-default-consumes">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <div className="row">
                  <div className="col-md-6">
                    <p /><p>New overlay info</p>
                    <p />
                  </div>
                  <div className="col-md-6 sw-request-model">
                    <div className="panel panel-definition">
                      <div className="panel-body">
                        <a className="json-schema-ref" href="#/definitions/Overlay">Overlay</a>
                      </div>
                    </div></div>
                </div>
              </section>        
              <section className="sw-request-params">
                <table className="table">
                  <thead>
                    <tr>
                      <th className="sw-param-name" />
                      <th className="sw-param-description" />
                      <th className="sw-param-type" />
                      <th className="sw-param-data-type" />
                      <th className="sw-param-annotation" />
                    </tr>
                  </thead>
                  <tbody>
                    <tr>
                      <td>
                        cid
                      </td><td><p>The customer ID</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                    <tr>
                      <td>
                        siteid
                      </td><td><p>The site ID</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                  </tbody>
                </table>
              </section>
              <section className="sw-responses">
                <p><a href="#sw-default-produces">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <dl>
                  <dt className="sw-response-200">
                    200 OK
                  </dt>
                  <dd className="sw-response-200">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Success</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/Overlay">Overlay</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                  <dt className="sw-response-default">
                    default 
                  </dt>
                  <dd className="sw-response-default">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Error</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                </dl>
              </section>
            </div>
          </div>    
          <span id="path--customers--cid--sites--siteid--overlays--oid-" />
          <div id="operation--customers--cid--sites--siteid--overlays--oid--delete" className="swagger--panel-operation-delete panel">
            <div className="panel-heading">
              <div className="operation-summary" />
              <h3 className="panel-title"><span className="operation-name">DELETE</span> <strong>/customers/{'{'}cid{'}'}/sites/{'{'}siteid{'}'}/overlays/{'{'}oid{'}'}</strong></h3>
            </div>
            <div className="panel-body">
              <section className="sw-operation-description">
                <p>Delete an overlay based on ID</p>
              </section>
              <section className="sw-request-params">
                <table className="table">
                  <thead>
                    <tr>
                      <th className="sw-param-name" />
                      <th className="sw-param-description" />
                      <th className="sw-param-type" />
                      <th className="sw-param-data-type" />
                      <th className="sw-param-annotation" />
                    </tr>
                  </thead>
                  <tbody>
                    <tr>
                      <td>
                        cid
                      </td><td><p>The customer ID</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                    <tr>
                      <td>
                        siteid
                      </td><td><p>The site ID</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                    <tr>
                      <td>
                        oid
                      </td><td><p>The overlay ID</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                  </tbody>
                </table>
              </section>
              <section className="sw-responses">
                <p><a href="#sw-default-produces">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <dl>
                  <dt className="sw-response-204">
                    204 No Content
                  </dt>
                  <dd className="sw-response-204">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Success</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                      </div>
                    </div>              </dd>
                  <dt className="sw-response-default">
                    default 
                  </dt>
                  <dd className="sw-response-default">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Error</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                </dl>
              </section>
            </div>
          </div>    <div id="operation--customers--cid--sites--siteid--overlays--oid--get" className="swagger--panel-operation-get panel">
            <div className="panel-heading">
              <div className="operation-summary" />
              <h3 className="panel-title"><span className="operation-name">GET</span> <strong>/customers/{'{'}cid{'}'}/sites/{'{'}siteid{'}'}/overlays/{'{'}oid{'}'}</strong></h3>
            </div>
            <div className="panel-body">
              <section className="sw-operation-description">
                <p>Returns overlay information to the caller</p>
              </section>
              <section className="sw-request-params">
                <table className="table">
                  <thead>
                    <tr>
                      <th className="sw-param-name" />
                      <th className="sw-param-description" />
                      <th className="sw-param-type" />
                      <th className="sw-param-data-type" />
                      <th className="sw-param-annotation" />
                    </tr>
                  </thead>
                  <tbody>
                    <tr>
                      <td>
                        cid
                      </td><td><p>The customer ID</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                    <tr>
                      <td>
                        siteid
                      </td><td><p>The site ID</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                    <tr>
                      <td>
                        oid
                      </td><td><p>The overlay ID</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                  </tbody>
                </table>
              </section>
              <section className="sw-responses">
                <p><a href="#sw-default-produces">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <dl>
                  <dt className="sw-response-200">
                    200 OK
                  </dt>
                  <dd className="sw-response-200">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Success</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/Overlay">Overlay</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                  <dt className="sw-response-404">
                    404 Not Found
                  </dt>
                  <dd className="sw-response-404">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Overlay Not Found</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                  <dt className="sw-response-default">
                    default 
                  </dt>
                  <dd className="sw-response-default">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Error</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                </dl>
              </section>
            </div>
          </div>    <div id="operation--customers--cid--sites--siteid--overlays--oid--post" className="swagger--panel-operation-post panel">
            <div className="panel-heading">
              <div className="operation-summary" />
              <h3 className="panel-title"><span className="operation-name">POST</span> <strong>/customers/{'{'}cid{'}'}/sites/{'{'}siteid{'}'}/overlays/{'{'}oid{'}'}</strong></h3>
            </div>
            <div className="panel-body">
              <section className="sw-operation-description">
                <p>Update an overlay</p>
              </section>
              <section className="sw-request-body">
                <p><a href="#sw-default-consumes">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <div className="row">
                  <div className="col-md-6">
                    <p /><p>Updated overlay info</p>
                    <p />
                  </div>
                  <div className="col-md-6 sw-request-model">
                    <div className="panel panel-definition">
                      <div className="panel-body">
                        <a className="json-schema-ref" href="#/definitions/Overlay">Overlay</a>
                      </div>
                    </div></div>
                </div>
              </section>        
              <section className="sw-request-params">
                <table className="table">
                  <thead>
                    <tr>
                      <th className="sw-param-name" />
                      <th className="sw-param-description" />
                      <th className="sw-param-type" />
                      <th className="sw-param-data-type" />
                      <th className="sw-param-annotation" />
                    </tr>
                  </thead>
                  <tbody>
                    <tr>
                      <td>
                        cid
                      </td><td><p>The customer ID</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                    <tr>
                      <td>
                        siteid
                      </td><td><p>The site ID</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                    <tr>
                      <td>
                        oid
                      </td><td><p>The overlay ID</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                  </tbody>
                </table>
              </section>
              <section className="sw-responses">
                <p><a href="#sw-default-produces">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <dl>
                  <dt className="sw-response-200">
                    200 OK
                  </dt>
                  <dd className="sw-response-200">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Success</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/Overlay">Overlay</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                  <dt className="sw-response-default">
                    default 
                  </dt>
                  <dd className="sw-response-default">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Error</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                </dl>
              </section>
            </div>
          </div>    
          <span id="path--data--which--of--type--from--from--id--id--since--since--to--to-" />
          <div id="operation--data--which--of--type--from--from--id--id--since--since--to--to--get" className="swagger--panel-operation-get panel">
            <div className="panel-heading">
              <div className="operation-summary" />
              <h3 className="panel-title"><span className="operation-name">GET</span> <strong>/data/{'{'}which{'}'}/of/{'{'}type{'}'}/from/{'{'}from{'}'}/id/{'{'}id{'}'}/since/{'{'}since{'}'}/to/{'{'}to{'}'}</strong></h3>
            </div>
            <div className="panel-body">
              <section className="sw-operation-description">
                <p>Returns all audits information for a site</p>
              </section>
              <section className="sw-request-params">
                <table className="table">
                  <thead>
                    <tr>
                      <th className="sw-param-name" />
                      <th className="sw-param-description" />
                      <th className="sw-param-type" />
                      <th className="sw-param-data-type" />
                      <th className="sw-param-annotation" />
                    </tr>
                  </thead>
                  <tbody>
                    <tr>
                      <td>
                        which
                      </td><td />
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                    <tr>
                      <td>
                        type
                      </td><td />
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                    <tr>
                      <td>
                        from
                      </td><td />
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                    <tr>
                      <td>
                        id
                      </td><td />
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                    <tr>
                      <td>
                        since
                      </td><td />
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                    <tr>
                      <td>
                        to
                      </td><td />
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                  </tbody>
                </table>
              </section>
              <section className="sw-responses">
                <p><a href="#sw-default-produces">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <dl>
                  <dt className="sw-response-200">
                    200 OK
                  </dt>
                  <dd className="sw-response-200">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Success</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/hdata">hdata</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                  <dt className="sw-response-default">
                    default 
                  </dt>
                  <dd className="sw-response-default">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Error</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                </dl>
              </section>
            </div>
          </div>    
          <span id="path--firmwares" />
          <div id="operation--firmwares-get" className="swagger--panel-operation-get panel">
            <div className="panel-heading">
              <div className="operation-summary" />
              <h3 className="panel-title"><span className="operation-name">GET</span> <strong>/firmwares</strong></h3>
            </div>
            <div className="panel-body">
              <section className="sw-operation-description">
                <p>Returns all firmware information to the caller</p>
              </section>
              <section className="sw-responses">
                <p><a href="#sw-default-produces">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <dl>
                  <dt className="sw-response-200">
                    200 OK
                  </dt>
                  <dd className="sw-response-200">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Success</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <section className="json-schema-array-items">
                              <span className="json-property-type">    <a className="json-schema-ref" href="#/definitions/Firmware">Firmware</a>
                              </span>
                              <span className="json-property-range" title="Value limits" />
                              <div className="json-inner-schema">
                              </div>
                            </section>  </div>
                        </div></div>
                    </div>              </dd>
                  <dt className="sw-response-default">
                    default 
                  </dt>
                  <dd className="sw-response-default">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Error</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                </dl>
              </section>
            </div>
          </div>    <div id="operation--firmwares-post" className="swagger--panel-operation-post panel">
            <div className="panel-heading">
              <div className="operation-summary" />
              <h3 className="panel-title"><span className="operation-name">POST</span> <strong>/firmwares</strong></h3>
            </div>
            <div className="panel-body">
              <section className="sw-operation-description">
                <p>Add new firmware</p>
              </section>
              <section className="sw-request-body">
                <p><a href="#sw-default-consumes">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <div className="row">
                  <div className="col-md-6">
                    <p /><p>New firmware info</p>
                    <p />
                  </div>
                  <div className="col-md-6 sw-request-model">
                    <div className="panel panel-definition">
                      <div className="panel-body">
                        <a className="json-schema-ref" href="#/definitions/NewFirmware">NewFirmware</a>
                      </div>
                    </div></div>
                </div>
              </section>        
              <section className="sw-responses">
                <p><a href="#sw-default-produces">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <dl>
                  <dt className="sw-response-200">
                    200 OK
                  </dt>
                  <dd className="sw-response-200">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Success</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/Firmware">Firmware</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                  <dt className="sw-response-default">
                    default 
                  </dt>
                  <dd className="sw-response-default">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Error</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                </dl>
              </section>
            </div>
          </div>    
          <span id="path--firmwares-assign--id-" />
          <div id="operation--firmwares-assign--id--put" className="swagger--panel-operation-put panel">
            <div className="panel-heading">
              <div className="operation-summary" />
              <h3 className="panel-title"><span className="operation-name">PUT</span> <strong>/firmwares/assign/{'{'}id{'}'}</strong></h3>
            </div>
            <div className="panel-body">
              <section className="sw-operation-description">
                <p>Update firmware</p>
              </section>
              <section className="sw-request-body">
                <p><a href="#sw-default-consumes">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <div className="row">
                  <div className="col-md-6">
                    <p /><p>The list of node IDs</p>
                    <p />
                  </div>
                  <div className="col-md-6 sw-request-model">
                    <div className="panel panel-definition">
                      <div className="panel-body">
                        <section className="json-schema-array-items">
                          <span className="json-property-type">string</span>
                          <span className="json-property-range" title="Value limits" />
                          <div className="json-inner-schema">
                          </div>
                        </section>  </div>
                    </div></div>
                </div>
              </section>        
              <section className="sw-request-params">
                <table className="table">
                  <thead>
                    <tr>
                      <th className="sw-param-name" />
                      <th className="sw-param-description" />
                      <th className="sw-param-type" />
                      <th className="sw-param-data-type" />
                      <th className="sw-param-annotation" />
                    </tr>
                  </thead>
                  <tbody>
                    <tr>
                      <td>
                        id
                      </td><td><p>The firmware id</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                  </tbody>
                </table>
              </section>
              <section className="sw-responses">
                <p><a href="#sw-default-produces">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <dl>
                  <dt className="sw-response-200">
                    200 OK
                  </dt>
                  <dd className="sw-response-200">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Success</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                  <dt className="sw-response-404">
                    404 Not Found
                  </dt>
                  <dd className="sw-response-404">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Firmware Not Found</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                  <dt className="sw-response-default">
                    default 
                  </dt>
                  <dd className="sw-response-default">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Error</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                </dl>
              </section>
            </div>
          </div>    
          <span id="path--firmwares-download--id-" />
          <div id="operation--firmwares-download--id--get" className="swagger--panel-operation-get panel">
            <div className="panel-heading">
              <div className="operation-summary" />
              <h3 className="panel-title"><span className="operation-name">GET</span> <strong>/firmwares/download/{'{'}id{'}'}</strong></h3>
            </div>
            <div className="panel-body">
              <section className="sw-operation-description">
                <p>Download firmware</p>
              </section>
              <section className="sw-request-params">
                <table className="table">
                  <thead>
                    <tr>
                      <th className="sw-param-name" />
                      <th className="sw-param-description" />
                      <th className="sw-param-type" />
                      <th className="sw-param-data-type" />
                      <th className="sw-param-annotation" />
                    </tr>
                  </thead>
                  <tbody>
                    <tr>
                      <td>
                        id
                      </td><td><p>The firmware id</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                  </tbody>
                </table>
              </section>
              <section className="sw-responses">
                <p><a href="#sw-default-produces">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <dl>
                  <dt className="sw-response-200">
                    200 OK
                  </dt>
                  <dd className="sw-response-200">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Success</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                          </div>
                        </div></div>
                    </div>              </dd>
                  <dt className="sw-response-404">
                    404 Not Found
                  </dt>
                  <dd className="sw-response-404">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Firmware Not Found</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                  <dt className="sw-response-default">
                    default 
                  </dt>
                  <dd className="sw-response-default">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Error</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                </dl>
              </section>
            </div>
          </div>    
          <span id="path--firmwares-revert--id-" />
          <div id="operation--firmwares-revert--id--put" className="swagger--panel-operation-put panel">
            <div className="panel-heading">
              <div className="operation-summary" />
              <h3 className="panel-title"><span className="operation-name">PUT</span> <strong>/firmwares/revert/{'{'}id{'}'}</strong></h3>
            </div>
            <div className="panel-body">
              <section className="sw-operation-description">
                <p>Revert firmware</p>
              </section>
              <section className="sw-request-params">
                <table className="table">
                  <thead>
                    <tr>
                      <th className="sw-param-name" />
                      <th className="sw-param-description" />
                      <th className="sw-param-type" />
                      <th className="sw-param-data-type" />
                      <th className="sw-param-annotation" />
                    </tr>
                  </thead>
                  <tbody>
                    <tr>
                      <td>
                        id
                      </td><td><p>The firmware id</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                  </tbody>
                </table>
              </section>
              <section className="sw-responses">
                <p><a href="#sw-default-produces">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <dl>
                  <dt className="sw-response-200">
                    200 OK
                  </dt>
                  <dd className="sw-response-200">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Success</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                  <dt className="sw-response-404">
                    404 Not Found
                  </dt>
                  <dd className="sw-response-404">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Firmware Not Found</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                  <dt className="sw-response-default">
                    default 
                  </dt>
                  <dd className="sw-response-default">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Error</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                </dl>
              </section>
            </div>
          </div>    
          <span id="path--firmwares--id-" />
          <div id="operation--firmwares--id--delete" className="swagger--panel-operation-delete panel">
            <div className="panel-heading">
              <div className="operation-summary" />
              <h3 className="panel-title"><span className="operation-name">DELETE</span> <strong>/firmwares/{'{'}id{'}'}</strong></h3>
            </div>
            <div className="panel-body">
              <section className="sw-operation-description">
                <p>Delete a firmware based on ID</p>
              </section>
              <section className="sw-request-params">
                <table className="table">
                  <thead>
                    <tr>
                      <th className="sw-param-name" />
                      <th className="sw-param-description" />
                      <th className="sw-param-type" />
                      <th className="sw-param-data-type" />
                      <th className="sw-param-annotation" />
                    </tr>
                  </thead>
                  <tbody>
                    <tr>
                      <td>
                        id
                      </td><td><p>The firmware ID</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                  </tbody>
                </table>
              </section>
              <section className="sw-responses">
                <p><a href="#sw-default-produces">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <dl>
                  <dt className="sw-response-200">
                    200 OK
                  </dt>
                  <dd className="sw-response-200">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Success</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                  <dt className="sw-response-204">
                    204 No Content
                  </dt>
                  <dd className="sw-response-204">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Success</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                      </div>
                    </div>              </dd>
                  <dt className="sw-response-default">
                    default 
                  </dt>
                  <dd className="sw-response-default">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Error</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                </dl>
              </section>
            </div>
          </div>    <div id="operation--firmwares--id--get" className="swagger--panel-operation-get panel">
            <div className="panel-heading">
              <div className="operation-summary" />
              <h3 className="panel-title"><span className="operation-name">GET</span> <strong>/firmwares/{'{'}id{'}'}</strong></h3>
            </div>
            <div className="panel-body">
              <section className="sw-operation-description">
                <p>Returns firmware information to the caller</p>
              </section>
              <section className="sw-request-params">
                <table className="table">
                  <thead>
                    <tr>
                      <th className="sw-param-name" />
                      <th className="sw-param-description" />
                      <th className="sw-param-type" />
                      <th className="sw-param-data-type" />
                      <th className="sw-param-annotation" />
                    </tr>
                  </thead>
                  <tbody>
                    <tr>
                      <td>
                        id
                      </td><td><p>The firmware ID</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                  </tbody>
                </table>
              </section>
              <section className="sw-responses">
                <p><a href="#sw-default-produces">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <dl>
                  <dt className="sw-response-200">
                    200 OK
                  </dt>
                  <dd className="sw-response-200">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Success</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/Firmware">Firmware</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                  <dt className="sw-response-404">
                    404 Not Found
                  </dt>
                  <dd className="sw-response-404">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Firmware Not Found</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                  <dt className="sw-response-default">
                    default 
                  </dt>
                  <dd className="sw-response-default">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Error</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                </dl>
              </section>
            </div>
          </div>    <div id="operation--firmwares--id--put" className="swagger--panel-operation-put panel">
            <div className="panel-heading">
              <div className="operation-summary" />
              <h3 className="panel-title"><span className="operation-name">PUT</span> <strong>/firmwares/{'{'}id{'}'}</strong></h3>
            </div>
            <div className="panel-body">
              <section className="sw-operation-description">
                <p>Update a firmware</p>
              </section>
              <section className="sw-request-body">
                <p><a href="#sw-default-consumes">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <div className="row">
                  <div className="col-md-6">
                    <p /><p>Updated firmware info</p>
                    <p />
                  </div>
                  <div className="col-md-6 sw-request-model">
                    <div className="panel panel-definition">
                      <div className="panel-body">
                        <a className="json-schema-ref" href="#/definitions/Firmware">Firmware</a>
                      </div>
                    </div></div>
                </div>
              </section>        
              <section className="sw-request-params">
                <table className="table">
                  <thead>
                    <tr>
                      <th className="sw-param-name" />
                      <th className="sw-param-description" />
                      <th className="sw-param-type" />
                      <th className="sw-param-data-type" />
                      <th className="sw-param-annotation" />
                    </tr>
                  </thead>
                  <tbody>
                    <tr>
                      <td>
                        id
                      </td><td><p>The firmware ID</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                  </tbody>
                </table>
              </section>
              <section className="sw-responses">
                <p><a href="#sw-default-produces">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <dl>
                  <dt className="sw-response-200">
                    200 OK
                  </dt>
                  <dd className="sw-response-200">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Success</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                  <dt className="sw-response-204">
                    204 No Content
                  </dt>
                  <dd className="sw-response-204">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Success</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                      </div>
                    </div>              </dd>
                  <dt className="sw-response-default">
                    default 
                  </dt>
                  <dd className="sw-response-default">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Error</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                </dl>
              </section>
            </div>
          </div>    
          <span id="path--lightcontrol-group--gid--level--lvl-" />
          <div id="operation--lightcontrol-group--gid--level--lvl--post" className="swagger--panel-operation-post panel">
            <div className="panel-heading">
              <div className="operation-summary" />
              <h3 className="panel-title"><span className="operation-name">POST</span> <strong>/lightcontrol/groups/{'{'}gid{'}'}/level/{'{'}lvl{'}'}</strong></h3>
            </div>
            <div className="panel-body">
              <section className="sw-operation-description">
                <p>Override light level</p>
              </section>
              <section className="sw-request-params">
                <table className="table">
                  <thead>
                    <tr>
                      <th className="sw-param-name" />
                      <th className="sw-param-description" />
                      <th className="sw-param-type" />
                      <th className="sw-param-data-type" />
                      <th className="sw-param-annotation" />
                    </tr>
                  </thead>
                  <tbody>
                    <tr>
                      <td>
                        gid
                      </td><td><p>The group ID</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                    <tr>
                      <td>
                        lvl
                      </td><td><p>The light level</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                  </tbody>
                </table>
              </section>
              <section className="sw-responses">
                <p><a href="#sw-default-produces">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <dl>
                  <dt className="sw-response-204">
                    204 No Content
                  </dt>
                  <dd className="sw-response-204">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Success</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                      </div>
                    </div>              </dd>
                  <dt className="sw-response-404">
                    404 Not Found
                  </dt>
                  <dd className="sw-response-404">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Group Not Found</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                  <dt className="sw-response-default">
                    default 
                  </dt>
                  <dd className="sw-response-default">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Error</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                </dl>
              </section>
            </div>
          </div>    
          <span id="path--lightcontrol-node--nid--level--lvl-" />
          <div id="operation--lightcontrol-node--nid--level--lvl--post" className="swagger--panel-operation-post panel">
            <div className="panel-heading">
              <div className="operation-summary" />
              <h3 className="panel-title"><span className="operation-name">POST</span> <strong>/lightcontrol/node/{'{'}nid{'}'}/level/{'{'}lvl{'}'}</strong></h3>
            </div>
            <div className="panel-body">
              <section className="sw-operation-description">
                <p>Override light level</p>
              </section>
              <section className="sw-request-params">
                <table className="table">
                  <thead>
                    <tr>
                      <th className="sw-param-name" />
                      <th className="sw-param-description" />
                      <th className="sw-param-type" />
                      <th className="sw-param-data-type" />
                      <th className="sw-param-annotation" />
                    </tr>
                  </thead>
                  <tbody>
                    <tr>
                      <td>
                        nid
                      </td><td><p>The node ID</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                    <tr>
                      <td>
                        lvl
                      </td><td><p>The light level</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                  </tbody>
                </table>
              </section>
              <section className="sw-responses">
                <p><a href="#sw-default-produces">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <dl>
                  <dt className="sw-response-204">
                    204 No Content
                  </dt>
                  <dd className="sw-response-204">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Success</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                      </div>
                    </div>              </dd>
                  <dt className="sw-response-404">
                    404 Not Found
                  </dt>
                  <dd className="sw-response-404">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Node Not Found</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                  <dt className="sw-response-default">
                    default 
                  </dt>
                  <dd className="sw-response-default">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Error</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                </dl>
              </section>
            </div>
          </div>    
          <span id="path--lightcontrol-nodes-level--lvl-" />
          <div id="operation--lightcontrol-nodes-level--lvl--post" className="swagger--panel-operation-post panel">
            <div className="panel-heading">
              <div className="operation-summary" />
              <h3 className="panel-title"><span className="operation-name">POST</span> <strong>/lightcontrol/nodes/level/{'{'}lvl{'}'}</strong></h3>
            </div>
            <div className="panel-body">
              <section className="sw-operation-description">
                <p>Override light level</p>
              </section>
              <section className="sw-request-body">
                <p><a href="#sw-default-consumes">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <div className="row">
                  <div className="col-md-6">
                    <p /><p>The list of node IDs</p>
                    <p />
                  </div>
                  <div className="col-md-6 sw-request-model">
                    <div className="panel panel-definition">
                      <div className="panel-body">
                        <section className="json-schema-array-items">
                          <span className="json-property-type">string</span>
                          <span className="json-property-range" title="Value limits" />
                          <div className="json-inner-schema">
                          </div>
                        </section>  </div>
                    </div></div>
                </div>
              </section>        
              <section className="sw-request-params">
                <table className="table">
                  <thead>
                    <tr>
                      <th className="sw-param-name" />
                      <th className="sw-param-description" />
                      <th className="sw-param-type" />
                      <th className="sw-param-data-type" />
                      <th className="sw-param-annotation" />
                    </tr>
                  </thead>
                  <tbody>
                    <tr>
                      <td>
                        lvl
                      </td><td><p>The light level</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                  </tbody>
                </table>
              </section>
              <section className="sw-responses">
                <p><a href="#sw-default-produces">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <dl>
                  <dt className="sw-response-204">
                    204 No Content
                  </dt>
                  <dd className="sw-response-204">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Success</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                      </div>
                    </div>              </dd>
                  <dt className="sw-response-default">
                    default 
                  </dt>
                  <dd className="sw-response-default">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Error</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                </dl>
              </section>
            </div>
          </div>    
          <span id="path--lightcontrol-site--sid--level--lvl-" />
          <div id="operation--lightcontrol-site--sid--level--lvl--post" className="swagger--panel-operation-post panel">
            <div className="panel-heading">
              <div className="operation-summary" />
              <h3 className="panel-title"><span className="operation-name">POST</span> <strong>/lightcontrol/site/{'{'}sid{'}'}/level/{'{'}lvl{'}'}</strong></h3>
            </div>
            <div className="panel-body">
              <section className="sw-operation-description">
                <p>Override light level</p>
              </section>
              <section className="sw-request-params">
                <table className="table">
                  <thead>
                    <tr>
                      <th className="sw-param-name" />
                      <th className="sw-param-description" />
                      <th className="sw-param-type" />
                      <th className="sw-param-data-type" />
                      <th className="sw-param-annotation" />
                    </tr>
                  </thead>
                  <tbody>
                    <tr>
                      <td>
                        sid
                      </td><td><p>The site ID</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                    <tr>
                      <td>
                        lvl
                      </td><td><p>The light level</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                  </tbody>
                </table>
              </section>
              <section className="sw-responses">
                <p><a href="#sw-default-produces">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <dl>
                  <dt className="sw-response-204">
                    204 No Content
                  </dt>
                  <dd className="sw-response-204">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Success</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                      </div>
                    </div>              </dd>
                  <dt className="sw-response-404">
                    404 Not Found
                  </dt>
                  <dd className="sw-response-404">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Site Not Found</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                  <dt className="sw-response-default">
                    default 
                  </dt>
                  <dd className="sw-response-default">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Error</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                </dl>
              </section>
            </div>
          </div>    
          <span id="path--lightingpolicy--lpid-" />
          <div id="operation--lightingpolicy--lpid--delete" className="swagger--panel-operation-delete panel">
            <div className="panel-heading">
              <div className="operation-summary" />
              <h3 className="panel-title"><span className="operation-name">DELETE</span> <strong>/lightingpolicy/{'{'}lpid{'}'}</strong></h3>
            </div>
            <div className="panel-body">
              <section className="sw-operation-description">
                <p>Delete a lighting policy based on ID</p>
              </section>
              <section className="sw-request-params">
                <table className="table">
                  <thead>
                    <tr>
                      <th className="sw-param-name" />
                      <th className="sw-param-description" />
                      <th className="sw-param-type" />
                      <th className="sw-param-data-type" />
                      <th className="sw-param-annotation" />
                    </tr>
                  </thead>
                  <tbody>
                    <tr>
                      <td>
                        lpid
                      </td><td><p>The lighting policy ID</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                  </tbody>
                </table>
              </section>
              <section className="sw-responses">
                <p><a href="#sw-default-produces">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <dl>
                  <dt className="sw-response-204">
                    204 No Content
                  </dt>
                  <dd className="sw-response-204">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Success</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                      </div>
                    </div>              </dd>
                  <dt className="sw-response-default">
                    default 
                  </dt>
                  <dd className="sw-response-default">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Error</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                </dl>
              </section>
            </div>
          </div>    <div id="operation--lightingpolicy--lpid--get" className="swagger--panel-operation-get panel">
            <div className="panel-heading">
              <div className="operation-summary" />
              <h3 className="panel-title"><span className="operation-name">GET</span> <strong>/lightingpolicy/{'{'}lpid{'}'}</strong></h3>
            </div>
            <div className="panel-body">
              <section className="sw-operation-description">
                <p>Returns lighting policy information to the caller</p>
              </section>
              <section className="sw-request-params">
                <table className="table">
                  <thead>
                    <tr>
                      <th className="sw-param-name" />
                      <th className="sw-param-description" />
                      <th className="sw-param-type" />
                      <th className="sw-param-data-type" />
                      <th className="sw-param-annotation" />
                    </tr>
                  </thead>
                  <tbody>
                    <tr>
                      <td>
                        lpid
                      </td><td><p>The lighting policy ID</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                  </tbody>
                </table>
              </section>
              <section className="sw-responses">
                <p><a href="#sw-default-produces">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <dl>
                  <dt className="sw-response-200">
                    200 OK
                  </dt>
                  <dd className="sw-response-200">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Success</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/LightingPolicy">LightingPolicy</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                  <dt className="sw-response-404">
                    404 Not Found
                  </dt>
                  <dd className="sw-response-404">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Lighting Policy Not Found</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                  <dt className="sw-response-default">
                    default 
                  </dt>
                  <dd className="sw-response-default">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Error</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                </dl>
              </section>
            </div>
          </div>    <div id="operation--lightingpolicy--lpid--post" className="swagger--panel-operation-post panel">
            <div className="panel-heading">
              <div className="operation-summary" />
              <h3 className="panel-title"><span className="operation-name">POST</span> <strong>/lightingpolicy/{'{'}lpid{'}'}</strong></h3>
            </div>
            <div className="panel-body">
              <section className="sw-operation-description">
                <p>create a lighting policy</p>
              </section>
              <section className="sw-request-body">
                <p><a href="#sw-default-consumes">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <div className="row">
                  <div className="col-md-6">
                    <p /><p>Updated overlay info</p>
                    <p />
                  </div>
                  <div className="col-md-6 sw-request-model">
                    <div className="panel panel-definition">
                      <div className="panel-body">
                        <a className="json-schema-ref" href="#/definitions/LightingPolicy">LightingPolicy</a>
                      </div>
                    </div></div>
                </div>
              </section>        
              <section className="sw-request-params">
                <table className="table">
                  <thead>
                    <tr>
                      <th className="sw-param-name" />
                      <th className="sw-param-description" />
                      <th className="sw-param-type" />
                      <th className="sw-param-data-type" />
                      <th className="sw-param-annotation" />
                    </tr>
                  </thead>
                  <tbody>
                    <tr>
                      <td>
                        lpid
                      </td><td><p>The lighting policy ID</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                  </tbody>
                </table>
              </section>
              <section className="sw-responses">
                <p><a href="#sw-default-produces">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <dl>
                  <dt className="sw-response-200">
                    200 OK
                  </dt>
                  <dd className="sw-response-200">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Success</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/LightingPolicy">LightingPolicy</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                  <dt className="sw-response-default">
                    default 
                  </dt>
                  <dd className="sw-response-default">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Error</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                </dl>
              </section>
            </div>
          </div>    <div id="operation--lightingpolicy--lpid--put" className="swagger--panel-operation-put panel">
            <div className="panel-heading">
              <div className="operation-summary" />
              <h3 className="panel-title"><span className="operation-name">PUT</span> <strong>/lightingpolicy/{'{'}lpid{'}'}</strong></h3>
            </div>
            <div className="panel-body">
              <section className="sw-operation-description">
                <p>Update a lighting policy</p>
              </section>
              <section className="sw-request-body">
                <p><a href="#sw-default-consumes">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <div className="row">
                  <div className="col-md-6">
                    <p /><p>Updated lighting policy info</p>
                    <p />
                  </div>
                  <div className="col-md-6 sw-request-model">
                    <div className="panel panel-definition">
                      <div className="panel-body">
                        <a className="json-schema-ref" href="#/definitions/LightingPolicy">LightingPolicy</a>
                      </div>
                    </div></div>
                </div>
              </section>        
              <section className="sw-request-params">
                <table className="table">
                  <thead>
                    <tr>
                      <th className="sw-param-name" />
                      <th className="sw-param-description" />
                      <th className="sw-param-type" />
                      <th className="sw-param-data-type" />
                      <th className="sw-param-annotation" />
                    </tr>
                  </thead>
                  <tbody>
                    <tr>
                      <td>
                        lpid
                      </td><td><p>The lighting policy ID</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                  </tbody>
                </table>
              </section>
              <section className="sw-responses">
                <p><a href="#sw-default-produces">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <dl>
                  <dt className="sw-response-200">
                    200 OK
                  </dt>
                  <dd className="sw-response-200">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Success</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/LightingPolicy">LightingPolicy</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                  <dt className="sw-response-default">
                    default 
                  </dt>
                  <dd className="sw-response-default">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Error</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                </dl>
              </section>
            </div>
          </div>    
          <span id="path--login" />
          <div id="operation--login-get" className="swagger--panel-operation-get panel">
            <div className="panel-heading">
              <div className="operation-summary" />
              <h3 className="panel-title"><span className="operation-name">GET</span> <strong>/login</strong></h3>
            </div>
            <div className="panel-body">
              <section className="sw-operation-description">
                <p>Returns all customer information to the caller</p>
              </section>
              <section className="sw-responses">
                <p><a href="#sw-default-produces">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <dl>
                  <dt className="sw-response-200">
                    200 OK
                  </dt>
                  <dd className="sw-response-200">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Success</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/Account">Account</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                  <dt className="sw-response-default">
                    default 
                  </dt>
                  <dd className="sw-response-default">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Error</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                </dl>
              </section>
            </div>
          </div>    <div id="operation--login-post" className="swagger--panel-operation-post panel">
            <div className="panel-heading">
              <div className="operation-summary" />
              <h3 className="panel-title"><span className="operation-name">POST</span> <strong>/login</strong></h3>
            </div>
            <div className="panel-body">
              <section className="sw-operation-description">
                <p>Login user</p>
              </section>
              <section className="sw-request-body">
                <p><a href="#sw-default-consumes">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <div className="row">
                  <div className="col-md-6">
                    <p /><p>Login data</p>
                    <p />
                  </div>
                  <div className="col-md-6 sw-request-model">
                    <div className="panel panel-definition">
                      <div className="panel-body">
                        <a className="json-schema-ref" href="#/definitions/LoginData">LoginData</a>
                      </div>
                    </div></div>
                </div>
              </section>        
              <section className="sw-responses">
                <p><a href="#sw-default-produces">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <dl>
                  <dt className="sw-response-200">
                    200 OK
                  </dt>
                  <dd className="sw-response-200">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Success</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/Account">Account</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                  <dt className="sw-response-404">
                    404 Not Found
                  </dt>
                  <dd className="sw-response-404">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Account Not Found</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                  <dt className="sw-response-default">
                    default 
                  </dt>
                  <dd className="sw-response-default">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Error</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                </dl>
              </section>
            </div>
          </div>    
          <span id="path--logout" />
          <div id="operation--logout-get" className="swagger--panel-operation-get panel">
            <div className="panel-heading">
              <div className="operation-summary" />
              <h3 className="panel-title"><span className="operation-name">GET</span> <strong>/logout</strong></h3>
            </div>
            <div className="panel-body">
              <section className="sw-operation-description">
                <p>Logout user</p>
              </section>
              <section className="sw-responses">
                <p><a href="#sw-default-produces">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <dl>
                  <dt className="sw-response-200">
                    200 OK
                  </dt>
                  <dd className="sw-response-200">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Success</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                  <dt className="sw-response-default">
                    default 
                  </dt>
                  <dd className="sw-response-default">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Error</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                </dl>
              </section>
            </div>
          </div>    
          <span id="path--schedules--sid-" />
          <div id="operation--schedules--sid--delete" className="swagger--panel-operation-delete panel">
            <div className="panel-heading">
              <div className="operation-summary" />
              <h3 className="panel-title"><span className="operation-name">DELETE</span> <strong>/schedules/{'{'}sid{'}'}</strong></h3>
            </div>
            <div className="panel-body">
              <section className="sw-operation-description">
                <p>Delete a schedule based on ID</p>
              </section>
              <section className="sw-request-params">
                <table className="table">
                  <thead>
                    <tr>
                      <th className="sw-param-name" />
                      <th className="sw-param-description" />
                      <th className="sw-param-type" />
                      <th className="sw-param-data-type" />
                      <th className="sw-param-annotation" />
                    </tr>
                  </thead>
                  <tbody>
                    <tr>
                      <td>
                        sid
                      </td><td><p>The schedule ID</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                  </tbody>
                </table>
              </section>
              <section className="sw-responses">
                <p><a href="#sw-default-produces">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <dl>
                  <dt className="sw-response-204">
                    204 No Content
                  </dt>
                  <dd className="sw-response-204">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Success</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                      </div>
                    </div>              </dd>
                  <dt className="sw-response-default">
                    default 
                  </dt>
                  <dd className="sw-response-default">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Error</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                </dl>
              </section>
            </div>
          </div>    <div id="operation--schedules--sid--get" className="swagger--panel-operation-get panel">
            <div className="panel-heading">
              <div className="operation-summary" />
              <h3 className="panel-title"><span className="operation-name">GET</span> <strong>/schedules/{'{'}sid{'}'}</strong></h3>
            </div>
            <div className="panel-body">
              <section className="sw-operation-description">
                <p>Returns schedule information to the caller</p>
              </section>
              <section className="sw-request-params">
                <table className="table">
                  <thead>
                    <tr>
                      <th className="sw-param-name" />
                      <th className="sw-param-description" />
                      <th className="sw-param-type" />
                      <th className="sw-param-data-type" />
                      <th className="sw-param-annotation" />
                    </tr>
                  </thead>
                  <tbody>
                    <tr>
                      <td>
                        sid
                      </td><td><p>The schedule ID</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                  </tbody>
                </table>
              </section>
              <section className="sw-responses">
                <p><a href="#sw-default-produces">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <dl>
                  <dt className="sw-response-200">
                    200 OK
                  </dt>
                  <dd className="sw-response-200">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Success</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/Schedule">Schedule</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                  <dt className="sw-response-404">
                    404 Not Found
                  </dt>
                  <dd className="sw-response-404">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Overlay Not Found</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                  <dt className="sw-response-default">
                    default 
                  </dt>
                  <dd className="sw-response-default">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Error</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                </dl>
              </section>
            </div>
          </div>    <div id="operation--schedules--sid--post" className="swagger--panel-operation-post panel">
            <div className="panel-heading">
              <div className="operation-summary" />
              <h3 className="panel-title"><span className="operation-name">POST</span> <strong>/schedules/{'{'}sid{'}'}</strong></h3>
            </div>
            <div className="panel-body">
              <section className="sw-operation-description">
                <p>create a schedule</p>
              </section>
              <section className="sw-request-body">
                <p><a href="#sw-default-consumes">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <div className="row">
                  <div className="col-md-6">
                    <p /><p>Updated overlay info</p>
                    <p />
                  </div>
                  <div className="col-md-6 sw-request-model">
                    <div className="panel panel-definition">
                      <div className="panel-body">
                        <a className="json-schema-ref" href="#/definitions/Schedule">Schedule</a>
                      </div>
                    </div></div>
                </div>
              </section>        
              <section className="sw-request-params">
                <table className="table">
                  <thead>
                    <tr>
                      <th className="sw-param-name" />
                      <th className="sw-param-description" />
                      <th className="sw-param-type" />
                      <th className="sw-param-data-type" />
                      <th className="sw-param-annotation" />
                    </tr>
                  </thead>
                  <tbody>
                    <tr>
                      <td>
                        sid
                      </td><td><p>The schedule ID</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                  </tbody>
                </table>
              </section>
              <section className="sw-responses">
                <p><a href="#sw-default-produces">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <dl>
                  <dt className="sw-response-200">
                    200 OK
                  </dt>
                  <dd className="sw-response-200">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Success</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/Schedule">Schedule</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                  <dt className="sw-response-default">
                    default 
                  </dt>
                  <dd className="sw-response-default">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Error</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                </dl>
              </section>
            </div>
          </div>    <div id="operation--schedules--sid--put" className="swagger--panel-operation-put panel">
            <div className="panel-heading">
              <div className="operation-summary" />
              <h3 className="panel-title"><span className="operation-name">PUT</span> <strong>/schedules/{'{'}sid{'}'}</strong></h3>
            </div>
            <div className="panel-body">
              <section className="sw-operation-description">
                <p>Update a schedule</p>
              </section>
              <section className="sw-request-body">
                <p><a href="#sw-default-consumes">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <div className="row">
                  <div className="col-md-6">
                    <p /><p>Updated overlay info</p>
                    <p />
                  </div>
                  <div className="col-md-6 sw-request-model">
                    <div className="panel panel-definition">
                      <div className="panel-body">
                        <a className="json-schema-ref" href="#/definitions/Schedule">Schedule</a>
                      </div>
                    </div></div>
                </div>
              </section>        
              <section className="sw-request-params">
                <table className="table">
                  <thead>
                    <tr>
                      <th className="sw-param-name" />
                      <th className="sw-param-description" />
                      <th className="sw-param-type" />
                      <th className="sw-param-data-type" />
                      <th className="sw-param-annotation" />
                    </tr>
                  </thead>
                  <tbody>
                    <tr>
                      <td>
                        sid
                      </td><td><p>The schedule ID</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                  </tbody>
                </table>
              </section>
              <section className="sw-responses">
                <p><a href="#sw-default-produces">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <dl>
                  <dt className="sw-response-200">
                    200 OK
                  </dt>
                  <dd className="sw-response-200">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Success</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/Schedule">Schedule</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                  <dt className="sw-response-default">
                    default 
                  </dt>
                  <dd className="sw-response-default">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Error</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                </dl>
              </section>
            </div>
          </div>    
          <span id="path--user" />
          <div id="operation--user-post" className="swagger--panel-operation-post panel">
            <div className="panel-heading">
              <div className="operation-summary" />
              <h3 className="panel-title"><span className="operation-name">POST</span> <strong>/user</strong></h3>
            </div>
            <div className="panel-body">
              <section className="sw-operation-description">
                <p>Add a new user</p>
              </section>
              <section className="sw-request-body">
                <p><a href="#sw-default-consumes">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <div className="row">
                  <div className="col-md-6">
                    <p /><p>user</p>
                    <p />
                  </div>
                  <div className="col-md-6 sw-request-model">
                    <div className="panel panel-definition">
                      <div className="panel-body">
                        <a className="json-schema-ref" href="#/definitions/User">User</a>
                      </div>
                    </div></div>
                </div>
              </section>        
              <section className="sw-responses">
                <p><a href="#sw-default-produces">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <dl>
                  <dt className="sw-response-200">
                    200 OK
                  </dt>
                  <dd className="sw-response-200">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Success</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/User">User</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                  <dt className="sw-response-default">
                    default 
                  </dt>
                  <dd className="sw-response-default">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Error</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                </dl>
              </section>
            </div>
          </div>    
          <span id="path--user--uid-" />
          <div id="operation--user--uid--delete" className="swagger--panel-operation-delete panel">
            <div className="panel-heading">
              <div className="operation-summary" />
              <h3 className="panel-title"><span className="operation-name">DELETE</span> <strong>/user/{'{'}uid{'}'}</strong></h3>
            </div>
            <div className="panel-body">
              <section className="sw-operation-description">
                <p>Deletes a user</p>
              </section>
              <section className="sw-request-params">
                <table className="table">
                  <thead>
                    <tr>
                      <th className="sw-param-name" />
                      <th className="sw-param-description" />
                      <th className="sw-param-type" />
                      <th className="sw-param-data-type" />
                      <th className="sw-param-annotation" />
                    </tr>
                  </thead>
                  <tbody>
                    <tr>
                      <td>
                        uid
                      </td><td><p>The user ID</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                  </tbody>
                </table>
              </section>
              <section className="sw-responses">
                <p><a href="#sw-default-produces">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <dl>
                  <dt className="sw-response-200">
                    200 OK
                  </dt>
                  <dd className="sw-response-200">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Success</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                      </div>
                    </div>              </dd>
                  <dt className="sw-response-default">
                    default 
                  </dt>
                  <dd className="sw-response-default">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Error</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                </dl>
              </section>
            </div>
          </div>    <div id="operation--user--uid--get" className="swagger--panel-operation-get panel">
            <div className="panel-heading">
              <div className="operation-summary" />
              <h3 className="panel-title"><span className="operation-name">GET</span> <strong>/user/{'{'}uid{'}'}</strong></h3>
            </div>
            <div className="panel-body">
              <section className="sw-operation-description">
                <p>Gets a user</p>
              </section>
              <section className="sw-request-params">
                <table className="table">
                  <thead>
                    <tr>
                      <th className="sw-param-name" />
                      <th className="sw-param-description" />
                      <th className="sw-param-type" />
                      <th className="sw-param-data-type" />
                      <th className="sw-param-annotation" />
                    </tr>
                  </thead>
                  <tbody>
                    <tr>
                      <td>
                        uid
                      </td><td><p>The user ID</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                  </tbody>
                </table>
              </section>
              <section className="sw-responses">
                <p><a href="#sw-default-produces">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <dl>
                  <dt className="sw-response-200">
                    200 OK
                  </dt>
                  <dd className="sw-response-200">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Success</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                          </div>
                        </div></div>
                    </div>              </dd>
                  <dt className="sw-response-default">
                    default 
                  </dt>
                  <dd className="sw-response-default">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Error</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                </dl>
              </section>
            </div>
          </div>    <div id="operation--user--uid--put" className="swagger--panel-operation-put panel">
            <div className="panel-heading">
              <div className="operation-summary" />
              <h3 className="panel-title"><span className="operation-name">PUT</span> <strong>/user/{'{'}uid{'}'}</strong></h3>
            </div>
            <div className="panel-body">
              <section className="sw-operation-description">
                <p>Update a user</p>
              </section>
              <section className="sw-request-body">
                <p><a href="#sw-default-consumes">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <div className="row">
                  <div className="col-md-6">
                    <p /><p>user</p>
                    <p />
                  </div>
                  <div className="col-md-6 sw-request-model">
                    <div className="panel panel-definition">
                      <div className="panel-body">
                        <a className="json-schema-ref" href="#/definitions/User">User</a>
                      </div>
                    </div></div>
                </div>
              </section>        
              <section className="sw-request-params">
                <table className="table">
                  <thead>
                    <tr>
                      <th className="sw-param-name" />
                      <th className="sw-param-description" />
                      <th className="sw-param-type" />
                      <th className="sw-param-data-type" />
                      <th className="sw-param-annotation" />
                    </tr>
                  </thead>
                  <tbody>
                    <tr>
                      <td>
                        uid
                      </td><td><p>ID of the user</p>
                      </td>
                      <td>path</td>
                      <td><span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                      </td>
                      <td>
                        <span className="json-property-required" />
                      </td>
                    </tr>
                  </tbody>
                </table>
              </section>
              <section className="sw-responses">
                <p><a href="#sw-default-produces">Uses default content-types: </a>
                  <span className="label label-default">application/json</span> 
                </p>
                <dl>
                  <dt className="sw-response-200">
                    200 OK
                  </dt>
                  <dd className="sw-response-200">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Success</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/User">User</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                  <dt className="sw-response-default">
                    default 
                  </dt>
                  <dd className="sw-response-default">
                    <div className="row">
                      <div className="col-md-12">
                        <p>Error</p>
                      </div>
                    </div>
                    <div className="row">
                      <div className="col-md-6 sw-response-model">
                        <div className="panel panel-definition">
                          <div className="panel-body">
                            <a className="json-schema-ref" href="#/definitions/ErrorResponse">ErrorResponse</a>
                          </div>
                        </div></div>
                    </div>              </dd>
                </dl>
              </section>
            </div>
          </div>
          <h2>Schema definitions</h2>
          <div id="definition-Account" className="panel panel-definition">
            <div className="panel-heading">
              <h3 className="panel-title"><a name="/definitions/Account" />Account:
                <span className="json-property-type"><span className="json-property-type" />
                  <span className="json-property-range" title="Value limits" />
                </span>
              </h3>
            </div>
            <div className="panel-body">
              <section className="json-schema-allOf">
                <section className="json-schema-allOf-inherited">
                  <ul>
                    <a className="json-schema-ref" href="#/definitions/NewAccount">NewAccount</a>
                  </ul>
                </section>
                <section className="json-schema-allOf-additional">
                  <section className="json-schema-properties">
                    <dl>
                      <dt data-property-name="id">
                        <span className="json-property-name">id:</span>
                        <span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                        <span className="json-property-required" />
                      </dt>
                      <dd>
                        <div className="json-inner-schema">
                        </div>
                      </dd>
                    </dl>
                  </section>
                </section>
              </section>
            </div>
          </div>        
          <div id="definition-Audit" className="panel panel-definition">
            <div className="panel-heading">
              <h3 className="panel-title"><a name="/definitions/Audit" />Audit:
                <span className="json-property-type"><span className="json-property-type">object</span>
                  <span className="json-property-range" title="Value limits" />
                </span>
              </h3>
            </div>
            <div className="panel-body">
              <section className="json-schema-properties">
                <dl>
                  <dt data-property-name="targetid">
                    <span className="json-property-name">targetid:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="when">
                    <span className="json-property-name">when:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="activity">
                    <span className="json-property-name">activity:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="message">
                    <span className="json-property-name">message:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                    <span className="json-property-required" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="targettype">
                    <span className="json-property-name">targettype:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="userid">
                    <span className="json-property-name">userid:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                </dl>
              </section>
            </div>
          </div>        
          <div id="definition-Customer" className="panel panel-definition">
            <div className="panel-heading">
              <h3 className="panel-title"><a name="/definitions/Customer" />Customer:
                <span className="json-property-type"><span className="json-property-type" />
                  <span className="json-property-range" title="Value limits" />
                </span>
              </h3>
            </div>
            <div className="panel-body">
              <section className="json-schema-allOf">
                <section className="json-schema-allOf-inherited">
                  <ul>
                    <a className="json-schema-ref" href="#/definitions/NewCustomer">NewCustomer</a>
                  </ul>
                </section>
                <section className="json-schema-allOf-additional">
                  <section className="json-schema-properties">
                    <dl>
                      <dt data-property-name="cid">
                        <span className="json-property-name">cid:</span>
                        <span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                        <span className="json-property-required" />
                      </dt>
                      <dd>
                        <div className="json-inner-schema">
                        </div>
                      </dd>
                    </dl>
                  </section>
                </section>
              </section>
            </div>
          </div>        
          <div id="definition-ErrorResponse" className="panel panel-definition">
            <div className="panel-heading">
              <h3 className="panel-title"><a name="/definitions/ErrorResponse" />ErrorResponse:
                <span className="json-property-type"><span className="json-property-type">object</span>
                  <span className="json-property-range" title="Value limits" />
                </span>
              </h3>
            </div>
            <div className="panel-body">
              <section className="json-schema-properties">
                <dl>
                  <dt data-property-name="message">
                    <span className="json-property-name">message:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                    <span className="json-property-required" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                </dl>
              </section>
            </div>
          </div>        
          <div id="definition-Firmware" className="panel panel-definition">
            <div className="panel-heading">
              <h3 className="panel-title"><a name="/definitions/Firmware" />Firmware:
                <span className="json-property-type"><span className="json-property-type" />
                  <span className="json-property-range" title="Value limits" />
                </span>
              </h3>
            </div>
            <div className="panel-body">
              <section className="json-schema-allOf">
                <section className="json-schema-allOf-inherited">
                  <ul>
                    <a className="json-schema-ref" href="#/definitions/NewFirmware">NewFirmware</a>
                  </ul>
                </section>
                <section className="json-schema-allOf-additional">
                  <section className="json-schema-properties">
                    <dl>
                      <dt data-property-name="id">
                        <span className="json-property-name">id:</span>
                        <span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                        <span className="json-property-required" />
                      </dt>
                      <dd>
                        <div className="json-inner-schema">
                        </div>
                      </dd>
                    </dl>
                  </section>
                </section>
              </section>
            </div>
          </div>        
          <div id="definition-hdata" className="panel panel-definition">
            <div className="panel-heading">
              <h3 className="panel-title"><a name="/definitions/hdata" />hdata:
                <span className="json-property-type"><span className="json-property-type">object</span>
                  <span className="json-property-range" title="Value limits" />
                </span>
              </h3>
            </div>
            <div className="panel-body">
              <section className="json-schema-properties">
                <dl>
                  <dt data-property-name="message">
                    <span className="json-property-name">message:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                    <span className="json-property-required" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="what">
                    <span className="json-property-name">what:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="units">
                    <span className="json-property-name">units:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="since">
                    <span className="json-property-name">since:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="to">
                    <span className="json-property-name">to:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="datapoints">
                    <span className="json-property-name">datapoints:</span>
                    <span className="json-property-type">object[]</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                      <section className="json-schema-array-items">
                        <span className="json-property-type">object</span>
                        <span className="json-property-range" title="Value limits" />
                        <div className="json-inner-schema">
                          <section className="json-schema-properties">
                            <dl>
                              <dt data-property-name="when">
                                <span className="json-property-name">when:</span>
                                <span className="json-property-type">string</span>
                                <span className="json-property-range" title="Value limits" />
                                <span className="json-property-required" />
                              </dt>
                              <dd>
                                <div className="json-inner-schema">
                                </div>
                              </dd>
                              <dt data-property-name="value">
                                <span className="json-property-name">value:</span>
                                <span className="json-property-type">string</span>
                                <span className="json-property-range" title="Value limits" />
                                <span className="json-property-required" />
                              </dt>
                              <dd>
                                <div className="json-inner-schema">
                                </div>
                              </dd>
                            </dl>
                          </section>
                        </div>
                      </section>              </div>
                  </dd>
                </dl>
              </section>
            </div>
          </div>        
          <div id="definition-LightingPolicy" className="panel panel-definition">
            <div className="panel-heading">
              <h3 className="panel-title"><a name="/definitions/LightingPolicy" />LightingPolicy:
                <span className="json-property-type"><span className="json-property-type">object</span>
                  <span className="json-property-range" title="Value limits" />
                </span>
              </h3>
            </div>
            <div className="panel-body">
              <section className="json-schema-properties">
                <dl>
                  <dt data-property-name="weekday">
                    <span className="json-property-name">weekday:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                    <span className="json-property-required" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="weekend">
                    <span className="json-property-name">weekend:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="days">
                    <span className="json-property-name">days:</span>
                    <span className="json-property-type">object[]</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                      <section className="json-schema-array-items">
                        <span className="json-property-type">object</span>
                        <span className="json-property-range" title="Value limits" />
                        <div className="json-inner-schema">
                          <section className="json-schema-properties">
                            <dl>
                              <dt data-property-name="which">
                                <span className="json-property-name">which:</span>
                                <span className="json-property-type">string</span>
                                <span className="json-property-range" title="Value limits" />
                                <span className="json-property-required" />
                              </dt>
                              <dd>
                                <div className="json-inner-schema">
                                </div>
                              </dd>
                              <dt data-property-name="schedule">
                                <span className="json-property-name">schedule:</span>
                                <span className="json-property-type">string</span>
                                <span className="json-property-range" title="Value limits" />
                                <span className="json-property-required" />
                              </dt>
                              <dd>
                                <div className="json-inner-schema">
                                </div>
                              </dd>
                            </dl>
                          </section>
                        </div>
                      </section>              </div>
                  </dd>
                  <dt data-property-name="special_dates">
                    <span className="json-property-name">special_dates:</span>
                    <span className="json-property-type">object[]</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                      <section className="json-schema-array-items">
                        <span className="json-property-type">object</span>
                        <span className="json-property-range" title="Value limits" />
                        <div className="json-inner-schema">
                          <section className="json-schema-properties">
                            <dl>
                              <dt data-property-name="date">
                                <span className="json-property-name">date:</span>
                                <span className="json-property-type">string</span>
                                <span className="json-property-range" title="Value limits" />
                                <span className="json-property-required" />
                              </dt>
                              <dd>
                                <div className="json-inner-schema">
                                </div>
                              </dd>
                              <dt data-property-name="schedule">
                                <span className="json-property-name">schedule:</span>
                                <span className="json-property-type">string</span>
                                <span className="json-property-range" title="Value limits" />
                                <span className="json-property-required" />
                              </dt>
                              <dd>
                                <div className="json-inner-schema">
                                </div>
                              </dd>
                            </dl>
                          </section>
                        </div>
                      </section>              </div>
                  </dd>
                  <dt data-property-name="state">
                    <span className="json-property-name">state:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="override_level_luminosity">
                    <span className="json-property-name">override_level_luminosity:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="override_level">
                    <span className="json-property-name">override_level:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="lightingpolicyid">
                    <span className="json-property-name">lightingpolicyid:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                    <span className="json-property-required" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                </dl>
              </section>
            </div>
          </div>        
          <div id="definition-LoginData" className="panel panel-definition">
            <div className="panel-heading">
              <h3 className="panel-title"><a name="/definitions/LoginData" />LoginData:
                <span className="json-property-type"><span className="json-property-type">object</span>
                  <span className="json-property-range" title="Value limits" />
                </span>
              </h3>
            </div>
            <div className="panel-body">
              <section className="json-schema-properties">
                <dl>
                  <dt data-property-name="username">
                    <span className="json-property-name">username:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                    <span className="json-property-required" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="password">
                    <span className="json-property-name">password:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                </dl>
              </section>
            </div>
          </div>        
          <div id="definition-NewAccount" className="panel panel-definition">
            <div className="panel-heading">
              <h3 className="panel-title"><a name="/definitions/NewAccount" />NewAccount:
                <span className="json-property-type"><span className="json-property-type">object</span>
                  <span className="json-property-range" title="Value limits" />
                </span>
              </h3>
            </div>
            <div className="panel-body">
              <section className="json-schema-properties">
                <dl>
                  <dt data-property-name="id">
                    <span className="json-property-name">id:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                    <span className="json-property-required" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                </dl>
              </section>
            </div>
          </div>        
          <div id="definition-NewCustomer" className="panel panel-definition">
            <div className="panel-heading">
              <h3 className="panel-title"><a name="/definitions/NewCustomer" />NewCustomer:
                <span className="json-property-type"><span className="json-property-type">object</span>
                  <span className="json-property-range" title="Value limits" />
                </span>
              </h3>
            </div>
            <div className="panel-body">
              <section className="json-schema-properties">
                <dl>
                  <dt data-property-name="name">
                    <span className="json-property-name">name:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                    <span className="json-property-required" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="street1">
                    <span className="json-property-name">street1:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="street2">
                    <span className="json-property-name">street2:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="city">
                    <span className="json-property-name">city:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="state">
                    <span className="json-property-name">state:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="postal_code">
                    <span className="json-property-name">postal_code:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="country">
                    <span className="json-property-name">country:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                </dl>
              </section>
            </div>
          </div>        
          <div id="definition-NewFirmware" className="panel panel-definition">
            <div className="panel-heading">
              <h3 className="panel-title"><a name="/definitions/NewFirmware" />NewFirmware:
                <span className="json-property-type"><span className="json-property-type">object</span>
                  <span className="json-property-range" title="Value limits" />
                </span>
              </h3>
            </div>
            <div className="panel-body">
              <section className="json-schema-properties">
                <dl>
                  <dt data-property-name="name">
                    <span className="json-property-name">name:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="release">
                    <span className="json-property-name">release:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="released">
                    <span className="json-property-name">released:</span>
                    <span className="json-property-type">boolean</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="commit">
                    <span className="json-property-name">commit:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="deprecated">
                    <span className="json-property-name">deprecated:</span>
                    <span className="json-property-type">boolean</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="version">
                    <span className="json-property-name">version:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="checksum">
                    <span className="json-property-name">checksum:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="model">
                    <span className="json-property-name">model:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="builder">
                    <span className="json-property-name">builder:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="build_date">
                    <span className="json-property-name">build_date:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="image_size">
                    <span className="json-property-name">image_size:</span>
                    <span className="json-property-type">integer</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                </dl>
              </section>
            </div>
          </div>        
          <div id="definition-NewNotification" className="panel panel-definition">
            <div className="panel-heading">
              <h3 className="panel-title"><a name="/definitions/NewNotification" />NewNotification:
                <span className="json-property-type"><span className="json-property-type">object</span>
                  <span className="json-property-range" title="Value limits" />
                </span>
              </h3>
            </div>
            <div className="panel-body">
              <section className="json-schema-properties">
                <dl>
                  <dt data-property-name="name">
                    <span className="json-property-name">name:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                    <span className="json-property-required" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="active">
                    <span className="json-property-name">active:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="rules">
                    <span className="json-property-name">rules:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="msg">
                    <span className="json-property-name">msg:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="notifyBy">
                    <span className="json-property-name">notifyBy:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="email">
                    <span className="json-property-name">email:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="phone">
                    <span className="json-property-name">phone:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                </dl>
              </section>
            </div>
          </div>        
          <div id="definition-NewSite" className="panel panel-definition">
            <div className="panel-heading">
              <h3 className="panel-title"><a name="/definitions/NewSite" />NewSite:
                <span className="json-property-type"><span className="json-property-type">object</span>
                  <span className="json-property-range" title="Value limits" />
                </span>
              </h3>
            </div>
            <div className="panel-body">
              <section className="json-schema-properties">
                <dl>
                  <dt data-property-name="name">
                    <span className="json-property-name">name:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                    <span className="json-property-required" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="street1">
                    <span className="json-property-name">street1:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="street2">
                    <span className="json-property-name">street2:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="city">
                    <span className="json-property-name">city:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="state">
                    <span className="json-property-name">state:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="postal_code">
                    <span className="json-property-name">postal_code:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="country">
                    <span className="json-property-name">country:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="latitude">
                    <span className="json-property-name">latitude:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="longitude">
                    <span className="json-property-name">longitude:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                </dl>
              </section>
            </div>
          </div>        
          <div id="definition-Node" className="panel panel-definition">
            <div className="panel-heading">
              <h3 className="panel-title"><a name="/definitions/Node" />Node:
                <span className="json-property-type"><span className="json-property-type">object</span>
                  <span className="json-property-range" title="Value limits" />
                </span>
              </h3>
            </div>
            <div className="panel-body">
              <section className="json-schema-properties">
                <dl>
                  <dt data-property-name="nodeid">
                    <span className="json-property-name">nodeid:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                    <span className="json-property-required" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="model">
                    <span className="json-property-name">model:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="latitude">
                    <span className="json-property-name">latitude:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="longitude">
                    <span className="json-property-name">longitude:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="ip">
                    <span className="json-property-name">ip:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="buildingLevel">
                    <span className="json-property-name">buildingLevel:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="lightStatus">
                    <span className="json-property-name">lightStatus:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="networkStatus">
                    <span className="json-property-name">networkStatus:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="sensorStatus">
                    <span className="json-property-name">sensorStatus:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                </dl>
              </section>
            </div>
          </div>        
          <div id="definition-Notification" className="panel panel-definition">
            <div className="panel-heading">
              <h3 className="panel-title"><a name="/definitions/Notification" />Notification:
                <span className="json-property-type"><span className="json-property-type" />
                  <span className="json-property-range" title="Value limits" />
                </span>
              </h3>
            </div>
            <div className="panel-body">
              <section className="json-schema-allOf">
                <section className="json-schema-allOf-inherited">
                  <ul>
                    <a className="json-schema-ref" href="#/definitions/NewNotification">NewNotification</a>
                  </ul>
                </section>
                <section className="json-schema-allOf-additional">
                  <section className="json-schema-properties">
                    <dl>
                      <dt data-property-name="notificationid">
                        <span className="json-property-name">notificationid:</span>
                        <span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                        <span className="json-property-required" />
                      </dt>
                      <dd>
                        <div className="json-inner-schema">
                        </div>
                      </dd>
                    </dl>
                  </section>
                </section>
              </section>
            </div>
          </div>        
          <div id="definition-Overlay" className="panel panel-definition">
            <div className="panel-heading">
              <h3 className="panel-title"><a name="/definitions/Overlay" />Overlay:
                <span className="json-property-type"><span className="json-property-type">object</span>
                  <span className="json-property-range" title="Value limits" />
                </span>
              </h3>
            </div>
            <div className="panel-body">
              <section className="json-schema-properties">
                <dl>
                  <dt data-property-name="fileName">
                    <span className="json-property-name">fileName:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="description">
                    <span className="json-property-name">description:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="users">
                    <span className="json-property-name">users:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="id">
                    <span className="json-property-name">id:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="imageBounds">
                    <span className="json-property-name">imageBounds:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                    <span className="json-property-required" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="imageData">
                    <span className="json-property-name">imageData:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                    <span className="json-property-required" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="imageType">
                    <span className="json-property-name">imageType:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="buildingLevel">
                    <span className="json-property-name">buildingLevel:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                    <span className="json-property-required" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                </dl>
              </section>
            </div>
          </div>        
          <div id="definition-Schedule" className="panel panel-definition">
            <div className="panel-heading">
              <h3 className="panel-title"><a name="/definitions/Schedule" />Schedule:
                <span className="json-property-type"><span className="json-property-type">object</span>
                  <span className="json-property-range" title="Value limits" />
                </span>
              </h3>
            </div>
            <div className="panel-body">
              <section className="json-schema-properties">
                <dl>
                  <dt data-property-name="scheduleid">
                    <span className="json-property-name">scheduleid:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                    <span className="json-property-required" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="is24Hours">
                    <span className="json-property-name">is24Hours:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                    <span className="json-property-required" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="isDaytime">
                    <span className="json-property-name">isDaytime:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                    <span className="json-property-required" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="segments">
                    <span className="json-property-name">segments:</span>
                    <span className="json-property-type">object[]</span>
                    <span className="json-property-range" title="Value limits" />
                    <span className="json-property-required" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                      <section className="json-schema-array-items">
                        <span className="json-property-type">object</span>
                        <span className="json-property-range" title="Value limits" />
                        <div className="json-inner-schema">
                          <section className="json-schema-properties">
                            <dl>
                              <dt data-property-name="time">
                                <span className="json-property-name">time:</span>
                                <span className="json-property-type">string</span>
                                <span className="json-property-range" title="Value limits" />
                                <span className="json-property-required" />
                              </dt>
                              <dd>
                                <div className="json-inner-schema">
                                </div>
                              </dd>
                              <dt data-property-name="driver">
                                <span className="json-property-name">driver:</span>
                                <span className="json-property-type">string</span>
                                <span className="json-property-range" title="Value limits" />
                                <span className="json-property-required" />
                              </dt>
                              <dd>
                                <div className="json-inner-schema">
                                </div>
                              </dd>
                              <dt data-property-name="luminosity">
                                <span className="json-property-name">luminosity:</span>
                                <span className="json-property-type">string</span>
                                <span className="json-property-range" title="Value limits" />
                              </dt>
                              <dd>
                                <div className="json-inner-schema">
                                </div>
                              </dd>
                            </dl>
                          </section>
                        </div>
                      </section>              </div>
                  </dd>
                  <dt data-property-name="harvesting">
                    <span className="json-property-name">harvesting:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="dimming">
                    <span className="json-property-name">dimming:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="presence">
                    <span className="json-property-name">presence:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="presence_duration">
                    <span className="json-property-name">presence_duration:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="presence_driver">
                    <span className="json-property-name">presence_driver:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                </dl>
              </section>
            </div>
          </div>        
          <div id="definition-Site" className="panel panel-definition">
            <div className="panel-heading">
              <h3 className="panel-title"><a name="/definitions/Site" />Site:
                <span className="json-property-type"><span className="json-property-type" />
                  <span className="json-property-range" title="Value limits" />
                </span>
              </h3>
            </div>
            <div className="panel-body">
              <section className="json-schema-allOf">
                <section className="json-schema-allOf-inherited">
                  <ul>
                    <a className="json-schema-ref" href="#/definitions/NewSite">NewSite</a>
                  </ul>
                </section>
                <section className="json-schema-allOf-additional">
                  <section className="json-schema-properties">
                    <dl>
                      <dt data-property-name="sid">
                        <span className="json-property-name">sid:</span>
                        <span className="json-property-type">string</span>
                        <span className="json-property-range" title="Value limits" />
                        <span className="json-property-required" />
                      </dt>
                      <dd>
                        <div className="json-inner-schema">
                        </div>
                      </dd>
                    </dl>
                  </section>
                </section>
              </section>
            </div>
          </div>        
          <div id="definition-User" className="panel panel-definition">
            <div className="panel-heading">
              <h3 className="panel-title"><a name="/definitions/User" />User:
                <span className="json-property-type"><span className="json-property-type">object</span>
                  <span className="json-property-range" title="Value limits" />
                </span>
              </h3>
            </div>
            <div className="panel-body">
              <section className="json-schema-properties">
                <dl>
                  <dt data-property-name="uid">
                    <span className="json-property-name">uid:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                    <span className="json-property-required" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="email">
                    <span className="json-property-name">email:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="name">
                    <span className="json-property-name">name:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="title">
                    <span className="json-property-name">title:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="org">
                    <span className="json-property-name">org:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="password">
                    <span className="json-property-name">password:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="roles">
                    <span className="json-property-name">roles:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                  <dt data-property-name="sites">
                    <span className="json-property-name">sites:</span>
                    <span className="json-property-type">string</span>
                    <span className="json-property-range" title="Value limits" />
                  </dt>
                  <dd>
                    <div className="json-inner-schema">
                    </div>
                  </dd>
                </dl>
              </section>
            </div>
          </div>
        </div>
      </div>
                          </PanelBody>
                  </Panel>
                </PanelContainer>
              </Col>
            </Row>
          </Grid>
        </Container>
    );
};
}

@SidebarMixin
export default class extends React.Component {
  render() {
    var classes = classNames({
      'container-open': this.props.open
    });

    return (
      <Container id='container' className={classes}>
        <Sidebar />
        <Header />
        <Body />
        <Footer />
      </Container>
    );
  }
}
