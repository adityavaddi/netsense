import classNames from 'classnames';
import { State, Navigation } from 'react-router';
import helpers from 'global/utils/helpers';

var Auditdatatable = React.createClass({

  propTypes: {
    audits: React.PropTypes.array.isRequired,
    users: React.PropTypes.array.isRequired,
    concise:React.PropTypes.bool,
  },

  getDefaultProps: function () { 
    return {
      concise:false,
    };
  },

  calcHeight(pct, extra) {
    var h = (window && window.document)?(($(window).height() - 75) * (pct/100) + extra):500;
    return h;
  },

  
  componentWillReceiveProps(){
    $("#audit-table").DataTable().destroy();
  },
  
  componentDidUpdate() {
    
    if(!this.props.concise){
      $('#audit-table thead tr#filterRow th').each( function () {
        var title = $('#audit-table thead th').eq( $(this).index()).text();
        $(this).html( '<input type="text" placeholder="Search '+title+'" />' );
      });

      // DataTable
      var table = $('#audit-table').DataTable({
        "paging": false,
        "orderCellsTop": true,
        "scrollY": helpers.calcHeight(60,80),
        "oLanguage": {
          "sEmptyTable": "No activities available for the selected dates",
        }
      });

      table.columns().iterator( 'column', function (ctx, idx) {
        $(table.column(idx).header()).find('span').remove();
        console.log("span removed");
        $( table.column(idx).header() ).append('<span class="sort-icon"/>');
      } );

      // Apply the filter
      $(".dataTables_scrollHeadInner table thead tr#filterRow input").on( 'keyup change', function () {
        table
            .column( $(this).parent().index()+':visible' )
            .search( this.value )
            .draw();
      } );

    }
    else{
      $("#audit-table").dataTable({
        "scrollY": helpers.calcHeight(27,27),
        "paging": false,
        "bFilter": false,
        "info": false,
        "oLanguage": {
          "sEmptyTable": "No activities available in the last 24hrs",
        }
      });
    }
  },
  
  componentDidMount() {  

    if(!this.props.concise){
      $('#audit-table thead tr#filterRow th').each( function () {
        var title = $('#audit-table thead th').eq( $(this).index()).text();
        $(this).html( '<input type="text" placeholder="Search '+title+'" />' );
      });

      // DataTable
      var table = $('#audit-table').DataTable({
        "paging": false,
        "scrollY": helpers.calcHeight(60,80),
        "orderCellsTop": true,
        "oLanguage": {
          "sEmptyTable": "No activities available for the last 24hrs",
        }
      });


      table.columns().iterator( 'column', function (ctx, idx) {
        $( table.column(idx).header() ).append('<span class="sort-icon"/>');
      } );


      // Apply the filter
      $(".dataTables_scrollHeadInner table thead tr#filterRow input").on( 'keyup change', function () {
        table
            .column( $(this).parent().index()+':visible' )
            .search( this.value )
            .draw();
      } );
    }
    else{
      $("#audit-table").dataTable({
        "scrollY": helpers.calcHeight(27,27),
        "paging": false,
        "bFilter": false,
        "info": false,
        "oLanguage": {
          "sEmptyTable": "No activities available in the last 24hrs",
        }
      });
    }
  },

  componentWillUnmount() {
//    this.unsetHandlers();
  },

  render(){

    var auditsData = this.props.audits;
    var usersData = this.props.users;
    console.log((usersData.length));
    // Convert who to readable format:
    for(var i=0;i<auditsData.length;i++){

      if(usersData.length > 0){
        for(var j=0;j<usersData.length;j++){
          if(auditsData[i].userid == usersData[j].userid){
            console.log(usersData[j].email);
            auditsData[i].userid = usersData[j].email;
          }
        }
      }
      else if (auditsData[i].userid = "507ef3cc-cd2d-46d8-ae6d-7ccf430c1110"){
        console.log("it is sensity user");
        auditsData[i].userid = "sensity_user@sensity.com"
      }
      else if (auditsData[i].userid = "114ad560-a046-11e5-a57f-ef24ae600576"){
        console.log("it is sensity admin ");
        auditsData[i].userid = "sensity_admin@sensity.com"
      }
      else{
        auditsData[i].userid = "sensity_read_only@sensity.com"
      }
    }

    // Convert targetname to readable format:
    for(var k=0;k<auditsData.length;k++){
      var messageData = auditsData[k].message
      var messageObject = JSON.parse(messageData);
      $.each(messageObject, function(key,messageName){
        if(key == "name"){
         // console.log(key + "/" + messageName );
          auditsData[k].targetid = messageName;
        }
        else if (key == "orgprops"){
          //console.log(key + "/" + messageName.name );
          auditsData[k].targetid = messageName.name;
        }
        else {
          console.log("Not applicable");
        }
      });
    } 

  
    console.log("after change:" + JSON.stringify(this.props.audits));

    var Audittablerows = this.props.audits.map(function(sortedAudit, index){
      return (
        <tr key={index} data-idx={index}>
          <td>
            {sortedAudit.when}
          </td>
          <td>
            {sortedAudit.targettype}
          </td>
          <td>
            {sortedAudit.targetid}
          </td>
          <td>
            {sortedAudit.userid}
          </td>
          <td>
            {sortedAudit.activity}
          </td>      
          <td>
            {sortedAudit.message}
          </td>
        </tr>
      );
    });
    if(this.props.concise){
      return (
      <table id="audit-table" className="display" style={{margin:"0px"}} cellspacing="0" width="100%">
        <thead>
          <tr>
            <th data-column-id="audittime" style={{width:"8%"}} > When </th>
            <th data-column-id="auditsite" style={{width:"8%"}}> Target Type </th>
            <th data-column-id="auditsiteid" style={{width:"15%"}}> Target Name</th>
            <th data-column-id="audituser" style={{width:"7%"}}> Who </th>
            <th data-column-id="audittype" style={{width:"10%"}}> Activity </th>
            <th data-column-id="auditmessage"> Raw Message </th>
          </tr>
        </thead>
        <tbody>
          {Audittablerows}
        </tbody>
      </table>
      );
    }
    else{
    return (
      <table id="audit-table" className="display" cellspacing="0" width="100%">
        <thead>
            <tr>
              <th data-column-id="audittime"> When </th>
              <th data-column-id="auditsite"> Target Type </th>
              <th data-column-id="auditsiteid"> Target Name</th>
              <th data-column-id="audituser"> Who </th>
              <th data-column-id="audittype"> Activity </th>
              <th data-column-id="auditmessage"> Raw Message </th>
            </tr>
            <tr id="filterRow">
              <th>When</th>
              <th>Site</th>
              <th>Site ID</th>
              <th>Who</th>
              <th>Activity</th>
              <th>Message</th>
            </tr>
        </thead>
        <tbody>
          {Audittablerows}
        </tbody>
      </table>
      );
    }
  }

});
module.exports = Auditdatatable;