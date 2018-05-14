import { Link } from 'react-router';

import classNames from 'classnames';
import helpers from 'global/utils/helpers';

import Header from 'common/headernew';

import package_snippet from '../../../../../package.json';

class Body extends React.Component {

  state = {ready: false};

  calcHeight(pct, extra) {
    var h = (window&&window.document)?(($(window).height() - 75) * (pct/100) + extra):500;
    return h;
  }

  componentDidMount() {
    this.setState({ready:true});
  }

  handleChange(key, index) {
    return function (e) {
      var state = [];
      if (key == "type") {
        state.permissions[index][key] = e.target.value;
        this.setState(state);
      };
      if (key == "allowed") {
        state.permissions[index].allowed = e.target.value.split(",");
        this.setState(state);
      };
    }.bind(this);
  }
 
  render() {
    var hstyle = {height:helpers.calcHeight(90, 0)+"px !important"};
    if (!this.state.ready) {
      return (
        <Container id='body'>
          <Grid>
            <Row>
              <Col sm={12}>
                <PanelContainer>
                  <Panel>
                    <PanelBody style={hstyle}>
                        <h2 id="loadingmsg" style={{paddingTop:"16%",textAlign:"center"}}>Cannot view permissions.  User is not logged in.</h2>
                    </PanelBody>
                  </Panel>
                </PanelContainer>
              </Col>
            </Row>
          </Grid>
        </Container>
      );
    }
    var that = this;
    var Permissiontablerows = NSN.userInfo.authorization
      .sort(function(a,b) {return a.model > b.model;})
      .map(function(model, index){
      return (
        <tr key={index}>
          <td>
            <b>{model.model}</b>
          </td>
          <td>
            {model.type}
          </td>
          <td>
            {model.allowed.toString().replace(/,/g, ", ")}
          </td>
        </tr>
      );
    });
    return (
      <Container id='body'>
        <Grid>
          <Row>
            <Col sm={12}>
              <PanelContainer>
                <Panel>
                  <PanelBody>
                    <Grid>
                      <Row>
                        <Col xs={12}>
                          <h2>User Permissions</h2>
                          <table id="permission-table" className="table table-condensed table-hover table-striped">
                            <thead><tr>
                              <th data-column-id="model" width="15%">Model</th>
                              <th data-column-id="type" width="15%">Type</th>
                              <th data-column-id="allowed" width="69%">Allowed</th>
                            </tr></thead>
                            <tbody>
                              {Permissiontablerows}
                            </tbody>
                          </table>
                        </Col>
                      </Row>
                    </Grid>
                  </PanelBody>
                </Panel>
              </PanelContainer>
            </Col>
          </Row>
        </Grid>
      </Container>
    );
  }
}

export default class extends React.Component {
  render() {
    var classes = classNames({
      'container-open': this.props.open
    });

    return (
      <Container id='container' className={classes}>
        <Header />
        <Body />
      </Container>
    );
  }
}
