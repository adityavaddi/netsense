import classNames from 'classnames';
import auth from 'global/utils/auth';
import { State, Navigation } from 'react-router';


var AlertHeader = React.createClass({

    handleDismiss:function(){
        ReactBootstrap.Dispatcher.emit('Nodeform.multiDismissAlert',this.props.alerts)
    },

    render: function(){
        var nodeName = this.props.alerts.length> 1 ? "Nodes": "Node";
        return(
              <div id="collapseFour" className="panel-collapse collapse in">
                    <div className="panel-body" style={{ border:"none"}}>
                        <div id="collapseFournested">
                        <div className="panel panel-default">
                            <div className="panel-heading">
                            <h5 className="panel-title">
                                <a className="collapsed" data-toggle="collapse" data-parent="#collapseFournested" href={"#collapseFournested-"+this.props.type}>
                                   <span style={{fontSize:"18px"}}> {this.props.type} </span> <span style={{fontSize:"13px"}}>occurred : {this.props.alerts.length} times</span>

                                    <button className="dismiss-all" onClick={()=>{this.handleDismiss()}}> Acknowledge All </button>
                                </a>
                            </h5>
                            </div>
                            <div id={"collapseFournested-"+this.props.type} className="panel-collapse collapse">
                                {
                                    this.props.alerts.map((eachAlert)=>{
                                        return <AlertRows alert={eachAlert}/>
                                    })
                                }
                            </div>
                        </div>
                        </div>
                    </div>
              </div>
        )
    }
})

var AlertRows = React.createClass({
    render: function(){
        return (
           <div className="panel-body">
                <dl style={{marginBottom:"5px"}} className="nodeAlertsWrapper">
                    <dt>Name </dt>
                    <dd> {this.props.alert.ufname} </dd>
                    <dt>Alarm Type </dt>
                    <dd> {this.props.alert.type} </dd>
                    <dt>Description </dt>
                    <dd>{this.props.alert.description}</dd>
                    <dt>Severity</dt>
                    <dd>{this.props.alert.severity}</dd>
                    <dt>Date & Time</dt>
                    <dd>{new Date(this.props.alert.updated).toString()}</dd>
                </dl>
           </div>
        )
    }
})

var Nodemultialertform = React.createClass({

  getInitialState: function(){
    return {
    "multi_alerts": [],
    "types" :[]
    }
  },

  propTypes: {
    multi_alerts: React.PropTypes.array.isRequired
  },

  componentWillMount(){
    this.categorizeAlerts(this.props.multi_alerts)
  },

  categorizeAlerts(alerts){
      var types = []
      var categories = []
      for(var i in alerts){
          if(alerts[i].severity != "Clear"){ //Only Show active Alerts
            if(types.indexOf(alerts[i].type) == -1){
                types.push(alerts[i].type)
                categories[alerts[i].type] = []
            }
            categories[alerts[i].type].push(alerts[i])
          } 
      }
      this.setState({
          multi_alerts : categories,
          types
      })
  },

  componentWillReceiveProps: function(nextProps){
     if(JSON.stringify(this.props.multi_alerts) != JSON.stringify(nextProps.multi_alerts)){
        this.categorizeAlerts(nextProps.multi_alerts)
     }
  },

  render: function() {
    return(
        <div className="accordionWrapper" >
                <div className="panel-group">
                    <div className="panel panel-default">
                        <div className="panel-heading">
                            <h4 className="panel-title">
                                <a className="" style={{background:"#b0bed9"}}  data-toggle="collapse" data-parent="#accordion">
                                   Active Alerts
                                </a>
                            </h4>
                        </div>
                        {
                            this.state.types.map((eachType)=>{
                                return <AlertHeader type={eachType} alerts={this.state.multi_alerts[eachType]}/>
                            })
                        }
                    </div>
                </div>
             </div>
    )
  }
});

module.exports = Nodemultialertform;
