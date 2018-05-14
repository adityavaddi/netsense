import { Link } from 'react-router';

import classNames from 'classnames';
import helpers from 'global/utils/helpers';
import sensors from 'global/utils/sensors';
import Header from 'common/headernew';

import DataGrid from 'components/datagrid';


class Body extends React.Component {

  render() {
    var hstyle = {height:helpers.calcHeight(90, 0)+"px !important"};
    var jiraURL = "https://xeranet.atlassian.net/secure/CreateIssueDetails!init.jspa?pid=10800&reporter=delliott&assignee=delliott&issuetype=1&components=12301&environment="
        + NSN.apiURL.replace(":443/v3.0/", "")
        + "&summary=UI:%20Update%20Sensor%20Information&description=Sensor:%0A%0DChanges Needed:";

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
                          <h2 style={{marginTop:"0px"}}>Sensors</h2>
<div style={{position:"absolute",top:"-8px",right:"33px",fontSize:"18px"}}>
<a href="https://github.com/Xeralux/medianode-definitions/tree/master/sensors"
target="_blank" style={{color:"#008ABF"}} title="View on GitHub">
<span className="rubix-icon icon-fontello-github-circled-1" style={{fontSize:"24px"}}></span> &nbsp;
<span style={{position:"relative",top:"-3px"}}>sensors/*.yaml</span></a>
&nbsp; | &nbsp;
<a href="https://github.com/Xeralux/Farallones/blob/feature/sbt_multi_project/ui_apps/netsense3/src/global/utils/sensors.js"
target="_blank" style={{color:"#008ABF"}} title="View on GitHub">
<span className="rubix-icon icon-fontello-github-circled-1" style={{fontSize:"24px"}}></span> &nbsp;
<span style={{position:"relative",top:"-3px"}}>sensors.js</span></a>
&nbsp; | &nbsp;
<a href={jiraURL} target="_blank" style={{color:"#008ABF"}} title="Create Jira ticket">
<img src="/imgs/jira.png" style={{position:"relative",top:"-3px",height:"21px"}} /> &nbsp;
<span style={{position:"relative",top:"-3px"}}>create issue</span></a>
</div>
                          <DataGrid component="Sensor"
                            dataArray={sensors.getSensorsList()}
                            dataIdField="id"
                            dataID=""
                            match="contains"
                            columns={[
                { name: "Id", field: "id", id: "id", sortable: true, width: 80, required:true, checked:true},
                { name: "Name", field: "name", id: "name", sortable: true, width: 100, required:true, checked:true},
                { name: "Type", field: "type", id: "type", sortable: true, width: 40, required:true, checked:true},
                { name: "Model", field: "model", id: "model", sortable: true, width: 120, required:true, checked:true},
                { name: "Category", field: "category", id: "category", sortable: true, width: 40, required:true, checked:true},
                { name: "Order", field: "order", id: "order", sortable: true, width: 20, required:true, checked:true},
                { name: "Units", field: "units", id: "units", sortable: true, width: 30, required:true, checked:true},
                        ]
                            } />                   
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
