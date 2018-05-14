
import classNames from 'classnames';
import { State, Navigation } from 'react-router';
import helpers from 'global/utils/helpers';

var Overlaydatatable = React.createClass({

  propTypes: {
    overlays: React.PropTypes.array.isRequired,
    overlayID: React.PropTypes.string.isRequired
  },

  calcHeight(pct, extra) {
    var h = ($(window).height() - 130) * (pct/100) + extra;
    return h;
  },

  setHandlers() {
    var table = $("#overlay-table").DataTable();
    table.on( 'select', function ( e, dt, type, indexes ) {
      var row = table[ type ]( indexes ).nodes().to$();
      ReactBootstrap.Dispatcher.emit("Overlaylist.select", row.attr("data-overlayid"));
    });

  },

  componentWillReceiveProps(){
    $("#overlay-table").DataTable().destroy();
  },

  
  componentDidUpdate() {
    $("#overlay-table").dataTable({
      scrollY: helpers.calcHeight(60,80),
      paging: false,
      select: "single",
      language: {
        search: "_INPUT_",
        searchPlaceholder: "Search..."
      }
    });

    $("#overlay-table tbody tr[data-overlayid='"+this.props.overlayID+"']").addClass("selected");

    this.setHandlers();
  },

    componentDidMount() {
    $("#overlay-table").dataTable({
      scrollY: helpers.calcHeight(60,80),
      paging: false,
      select: "single",
      language: {
        search: "_INPUT_",
        searchPlaceholder: "Search..."
      }
    });

    $("#overlay-table tbody tr[data-overlayid='"+this.props.overlayID+"']").addClass("selected");

    this.setHandlers();
  },

  render(){

    var that = this;
    
    var Overlaytablerows = this.props.overlays.map(function(overlay, index){
      return (

        <tr key={index} data-idx={index} data-overlayid={that.props.overlays[index].overlayid} style={{cursor:"pointer"}}>
          <td>
            {overlay.fileName}
          </td>
          <td>
            {overlay.description}
          </td>
        </tr>
      );
    });

    return (
      <table id="overlay-table" className="table table-condensed table-hover table-striped">
        <thead><tr>
          <th data-column-id="overlayname">Name</th>
          <th data-column-id="overlaydescription">Description</th>
        </tr></thead>
        <tbody>
          {Overlaytablerows}
        </tbody>
      </table>
    );
  }
});
module.exports = Overlaydatatable;





