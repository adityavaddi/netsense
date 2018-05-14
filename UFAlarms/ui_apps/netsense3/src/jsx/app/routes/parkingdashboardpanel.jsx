import classNames from 'classnames';
import helpers from 'global/utils/helpers';
import Header from 'common/headernew';
import DataUtil from '../service/datautil';

var Body = React.createClass({

    getInitialState: function() {
        return {
            url:null
        }
    },
    init: function () {
        var that = this;
        //var siteselected = $("select#siteselector option").filter(":selected").val();
        if (NSN.customerID=="-1" && NSN.siteID=="-1") {
          $("#loadingmsg").html("Please select an Account and a Site first.")
          return;
        } else {
          if (NSN.customerID=="-1") {
            $("#loadingmsg").html("Please select an Account first.")
            return;
          } else {
            if (NSN.siteID=="-1") {
              $("#loadingmsg").html("Please select a Site first.")
              return;
            }
          }
        };

        var payload = {site:NSN.siteID};
        // Get Looker Url:
        $.ajax({
          url: '/getLookerUrl', 
          data : payload,
          method : 'POST',
          xhrFields: {
             withCredentials: true
          },
          success : function(data){
            console.log("ajax success: " + data);
            that.setState({url:data});
          },
          error : function(jqXHR, status, error){
            console.log("ajax failure (looker): " + status + " - " + error);
            $("#loadingmsg").html("Cannot retrieve looker list.  API call failed.");
          }
        });
    },

    /*handleSiteChange: function(e) {
        e.preventDefault();
        var that = this;
        var payload = {site:e.target.value};
        $.ajax({
          url: '/getLookerUrl', 
          data : payload,
          method : 'POST',
          xhrFields: {
             withCredentials: true
          },
          success : function(data){
            console.log("ajax success: " + data);
            that.setState({url:data});
          },
          error : function(jqXHR, status, error){
            console.log("ajax failure (looker): " + status + " - " + error);
            $("#loadingmsg").html("Cannot retrieve looker list.  API call failed.");
          }
        });
    }, */

    componentDidMount: function () {
        this.init();
    },

    render: function() {
        var self = this;
        var css = {
            'text-align': 'center',
            'width': '100%'
        };
        /*var List = (
                <Col  md={12} lg={2}>
                <PanelContainer>
                    <Panel>
                        <PanelBody>
                            <Form>
                                <FormGroup>
                                    <Label>
                                        Site
                                    </Label>
                                    <Select id="siteselector" style={css} onChange={self.handleSiteChange}>
                                        <option value="Great Mall">Great Mall </option>
                                        <option value="Stoneridge">Stoneridge </option>
                                        <option value="ValleyFair">ValleyFair </option>
                                    </Select>
                                </FormGroup>
                               
                            </Form>
                        </PanelBody>
                    </Panel>
                </PanelContainer>
            </Col>), */
                var graphPanel = (
                    <Col md={12} lg={12}>
                        <PanelContainer>
                            <Panel>
                                <PanelBody>
                                    <iframe src={this.state.url} style={{border:"0", position:"absolute",width:"99%", height:"95%"}}></iframe>
                                </PanelBody>
                            </Panel>
                        </PanelContainer>
                    </Col>
                );

            if(NSN.customerID!="-1" && NSN.siteID!="-1"){
                return (
                    <Container id="body">
                        <Grid>
                            <Row>
                                {graphPanel}
                            </Row>
                        </Grid>
                    </Container>
                );
            }

            else{
                return (
                  <Container id='body'>
                    <Grid>
                        <Row>
                            <Col sm={12}>
                                <PanelContainer>
                                    <Panel>
                                        <PanelBody>
                                            <h2 id="loadingmsg" style={{ paddingTop:'16%',textAlign:'center' }}>
                                                No sensors / nodes available
                                            </h2>
                                        </PanelBody>
                                    </Panel>
                                </PanelContainer>
                            </Col>
                        </Row>
                    </Grid>
                </Container>
                );
            }
            
    },

});

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