import classNames from 'classnames';
import auth from 'global/utils/auth';
import helpers from 'global/utils/helpers';
import { State, Navigation } from 'react-router';

var Userpersonalist = React.createClass({

  propTypes: {
    concise:React.PropTypes.bool,
  },

  getDefaultProps: function () { 
    return {
      concise:false,
    };
  },

  calcHeight(pct, extra) {
    var h = ($(window).height() - 130) * (pct/100) + extra;
    return h;
  },

  componentDidMount(){
    if(this.props.concise){
      $("#userpersona-table").dataTable({
        scrollY: helpers.calcHeight(27,27),
        paging: false,
        bFilter: false,
        info: false
      });
    }
  },

  componentWillReceiveProps(){
    $("#userpersona-table").DataTable().destroy();
  },
  
  componentDidUpdate(){
    if(this.props.concise){
      $("#userpersona-table").dataTable({
        scrollY: helpers.calcHeight(27,27),
        paging: false,
        bFilter: false,
        info: false
      });
    }
  },


  render() {
    if(this.props.concise){
      return (
        <div id="customer-table-container">
          <h2 style={{position:"absolute",top:"-10px",left:"12px"}}>User Persona </h2>
          <br/> <br/>

          <table id="userpersona-table" className="table table-condensed table-hover table-striped">
            <thead>
              <tr>
                <th>Name</th>
                <th>Value</th>
              </tr>
            </thead>
            <tbody>
              <tr style={{cursor:"pointer"}}>
                <td>
                  UserName:
                </td>
                <td>
                  {NSN.userInfo.user.name}
                </td>
              </tr>
              <tr style={{cursor:"pointer"}}>
                <td>
                  User ID:
                </td>
                <td>
                  {NSN.userInfo.user.userid}
                </td>
              </tr>
              <tr style={{cursor:"pointer"}}>
                <td>
                  Type:                             
                </td>
                <td>
                  {NSN.userInfo.authorization[0].type}
                </td>
              </tr>
            </tbody>
          </table>
        </div>
      );
    }
  }
});

module.exports = Userpersonalist;

