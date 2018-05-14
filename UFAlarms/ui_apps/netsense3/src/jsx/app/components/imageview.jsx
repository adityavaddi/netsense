import classNames from 'classnames';
import helpers from 'global/utils/helpers';

import { State, Navigation } from 'react-router';

var ImageView = React.createClass({

  getInitialState: function() {
    return {
      image0Ready: false,
      image0: null,
      image0Error: false,
      image0Timestamp: null,
      image1Ready: false,
      image1: null,
      image1Error: false,
      image1Timestamp: null,
      imageCReady: false,
      imageC: null,
      imageCError: false,
      imageCTimestamp: null
     };
  },

  propTypes: {
    context: React.PropTypes.string.isRequired,
    node: React.PropTypes.object.isRequired,
    timestamp: React.PropTypes.string,
    show: React.PropTypes.bool.isRequired
  },

  getImage: function() {
    var that = this;

    if (helpers.modelInternalName(this.props.node.model) == "falcon-q") {

      $.ajax({
        url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/node/' + this.props.node.nodeid
              + '/Media/img/0/L',
        data: JSON.stringify({imgFormat:"jpg"}),
        method: 'POST',
        xhrFields: {
            withCredentials: true
        },
        dataType: 'json',
        contentType: 'application/json',
        timeout: 60000,
        success: function(data) {
            if (data.message) {
              that.setState({image0Ready: true,
                             image0Timestamp: data.timestamp,
                             image0: "data:image/jpg;base64," + data.message});
            }
        },
        error: function(jqXHR, status, error) {
            noty({type:"error", text:'Error retrieving Camera 0 image. '
                + (status==408?"Timeout (408) occurred.":("Status " + jqXHR.status))});
            that.setState({image0Error: true});
        }
      });

      setTimeout(function(){
        $.ajax({
          url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/node/' + that.props.node.nodeid
                + '/Media/img/1/L',
          data: JSON.stringify({imgFormat:"jpg"}),
          method: 'POST',
          xhrFields: {
              withCredentials: true
          },
          dataType: 'json',
          contentType: 'application/json',
          timeout: 60000,
          success: function(data) {
              if (data.message) {
                that.setState({image1Ready: true,
                               image1Timestamp: data.timestamp,
                               image1: "data:image/jpg;base64," + data.message});
              }
          },
          error: function(jqXHR, status, error) {
              noty({type:"error", text:'Error retrieving Camera 1 image. '
                  + (status==408?"Timeout (408) occurred.":("Status " + jqXHR.status))});
              that.setState({image1Error: true});
          }
        })}, 1500);
    };

    if (helpers.modelInternalName(this.props.node.model) == "merlin") {
      $.ajax({
        url: NSN.apiURL + 'customers/' + NSN.customerID + '/sites/' + NSN.siteID + '/node/' + this.props.node.nodeid
              + '/Media/img/C/L',
        data: JSON.stringify({imgFormat:"jpg"}),
        method: 'POST',
        xhrFields: {
            withCredentials: true
        },
        dataType: 'json',
        contentType: 'application/json',
        timeout: 60000,
        success: function(data) {
            if (data.message) {
              that.setState({imageCReady: true,
                             imageCTimestamp: data.timestamp,
                             imageC: "data:image/jpg;base64," + data.message});
            }
        },
        error: function(jqXHR, status, error) {
            noty({type:"error", text:'Error retrieving composite image. '
                + (status==408?"Timeout (408) occurred.":("Status " + jqXHR.status))});
            that.setState({imageCError: true});
        }
      });
    };

  },

  handleRefresh: function() {
    var that = this;
    this.setState({
                image0Ready: false,
                image0: null,
                image0Error: false,
                image0Timestamp: null,
                image1Ready: false,
                image1: null,
                image1Error: false,
                image1Timestamp: null,
                imageCReady: false,
                imageC: null,
                imageCError: false,
                imageCTimestamp: null
      }, function(){that.getImage()})
  },

  handleClose: function() {
    ReactBootstrap.Dispatcher.emit(helpers.ucfirst(this.props.context) + 'form.image', 'close');
  },

  componentWillReceiveProps: function(nextProps, nextState) {
    var that = this;
    if ((nextProps.show && !this.props.show) 
      || (nextProps.node !== null && nextProps.node.nodeid != this.props.node.nodeid)) {
      this.setState({
                image0Ready: false,
                image0: null,
                image0Error: false,
                image0Timestamp: null,
                image1Ready: false,
                image1: null,
                image1Error: false,
                image1Timestamp: null,
                imageCReady: false,
                imageC: null,
                imageCError: false,
                imageCTimestamp: null
        }, function(){that.getImage()})
    } else {
      if (this.props.show && !nextProps.show) {
        $(".image-display").fadeOut('fast');
      }
    }
  },

  componentDidUpdate: function(){
    var that = this;
    if (this.props.show) {
        $(".image-display:hidden").fadeIn('fast', function() {
              if (!$(".image-display").hasClass("ui-draggable")) {
                  $(".image-display").draggable();
              };
/*
              if (!$(".image-display").hasClass("ui-resizable")) {
                $(".image-display").resizable({aspectRatio: true,
                                                           minWidth: helpers.modelInternalName(that.props.node.model) == "falcon-q"
                                                                        ?($(window).width() / 2.5)
                                                                        :($(window).width() / 5),
                                                           handles: "se"});
              };
*/
        });
    } else {
        $(".image-display").fadeOut('fast');
    }
  },

  render() {
    if (this.props.node && helpers.modelInternalName(this.props.node.model) == "falcon-q" ) {
      return (
      <div className="image-display">
        <div style={{padding:"0px 40px 60px"}}>

          <h3>Current Image -&nbsp;
            {this.props.node ? this.props.node.nodeid : ""}
          </h3>
          <div>
            {this.state.image0Ready ? 
              (
              <div style={{display:"inline-block",marginRight:"3%",width:"47%",textAlign:"center"}}>
                <img src={this.state.image0} style={{width:"100%"}}/>
                <h4>Camera 0 image &nbsp;
                <button className="ns-form-btn"><a style={{color:"#000"}} href={this.state.image0} 
                    title={this.props.node.nodeid + "_0L_" + this.state.image0Timestamp.replace(/:/g,".") + ".jpg"}
                    download={this.props.node.nodeid + "_0L_" + this.state.image0Timestamp.replace(/:/g,".") + ".jpg"}>Download</a>
                </button>
                </h4>
              </div>
              )
            : (
              <div style={{display:"inline-block",marginRight:"30px",width:"47%",height:"90%",border:"1px dashed #999",textAlign:"center",paddingTop:"5%"}}>
                {this.state.image0Error ?
                  <h4>Image could not be retrieved <nobr>from Camera 0</nobr></h4>
                :
                  <div>
                    <h4>Retrieving image</h4>
                    <img src="/imgs/loading.gif" />
                    <h5>(may take up to 15 seconds)</h5>
                  </div>
                }
              </div>
              )
            }
            {this.state.image1Ready ? 
              (
              <div style={{display:"inline-block",width:"47%",textAlign:"center"}}>
                <img src={this.state.image1} style={{width:"100%"}}/>
                <h4>Camera 1 image &nbsp; 
                <button className="ns-form-btn"><a style={{color:"#000"}} href={this.state.image1} 
                    title={this.props.node.nodeid + "_1L_" + this.state.image1Timestamp.replace(/:/g,".") + ".jpg"}
                    download={this.props.node.nodeid + "_1L_" + this.state.image1Timestamp.replace(/:/g,".") + ".jpg"}>Download</a>
                </button></h4>
                </div>
              )
            : (
              <div style={{display:"inline-block",width:"47%",height:"90%",border:"1px dashed #999",textAlign:"center",paddingTop:"5%"}}>
                {this.state.image1Error ?
                  <h4>Image could not be retrieved <nobr>from Camera 1</nobr></h4>
                :
                  <div>
                    <h4>Retrieving image</h4>
                    <img src="/imgs/loading.gif" />
                    <h5>(may take up to 15 seconds)</h5>
                  </div>
                }
              </div>
              )
            }
            <div style={{clear:"both"}}></div>
          </div>
          {this.state.image0Ready && this.state.image1Ready && 
            (
            <div style={{position:"absolute",bottom:"10px",right:"20px"}}>
              <button className="ns-form-btn" onClick={this.handleRefresh}> &nbsp; Refresh &nbsp; </button>
            </div>
            )
          }
          <div style={{position:"absolute",top:"16px",right:"20px"}}>
            <button className="ns-panel-btn" onClick={this.handleClose}> &nbsp; Close &nbsp; </button>
          </div>

        </div>
      </div>
      )
    };

    if (this.props.node && helpers.modelInternalName(this.props.node.model) == "merlin") {
      return (
      <div className="image-display" style={{width:"1044px"}}>
        <div style={{padding:"0px 40px 60px"}}>

          <h3>Current Image -&nbsp;
            {this.props.node ? this.props.node.nodeid : ""}
          </h3>
          <div>
            {this.state.imageCReady ? 
              (
              <div>
                <img style={{height:"270px",width:"960px"}} src={this.state.imageC} />
                <h4>Combined (stitched) image &nbsp;
                <button className="ns-form-btn"><a style={{color:"#000"}} href={this.state.imageC} 
                    title={this.props.node.nodeid + "_CL_" + this.state.imageCTimestamp.replace(/:/g,".") + ".jpg"}
                    download={this.props.node.nodeid + "_CL_" + this.state.imageCTimestamp.replace(/:/g,".") + ".jpg"}>Download</a>
                </button>
                </h4>
              </div>
              )
            : (
              <div style={{height:"270px",width:"960px",border:"1px dashed #999",textAlign:"center",paddingTop:"10%"}}>
                {this.state.imageCError ?
                  <h4>Image could not be retrieved</h4>
                :
                  <div>
                    <h4>Retrieving image</h4>
                    <img src="/imgs/loading.gif" />
                    <h5>(may take up to 15 seconds)</h5>
                  </div>
                }
              </div>
              )
            }
          </div>

          {this.state.imageCReady  && 
            (
            <div style={{position:"absolute",bottom:"10px",right:"20px"}}>
              <button className="ns-form-btn" onClick={this.handleRefresh}> &nbsp; Refresh &nbsp; </button>
            </div>
            )
          }
          <div style={{position:"absolute",top:"16px",right:"20px"}}>
            <button className="ns-panel-btn" onClick={this.handleClose}> &nbsp; Close &nbsp; </button>
          </div>

        </div>
      </div>
      )
    };

    return (<div></div>);
  }
});

module.exports = ImageView;
