import classNames from 'classnames';
import auth from 'global/utils/auth';
import helpers from 'global/utils/helpers';
import DataGrid from 'components/datagrid';

import { Modal } from 'react-bootstrap';
import { State, Navigation } from 'react-router';

var Listeditor = React.createClass({

    propTypes: {
        component: React.PropTypes.string.isRequired,
        componentColumns: React.PropTypes.string.isRequired,
        columns: React.PropTypes.array.isRequired,
        height: React.PropTypes.string,
        overflowY: React.PropTypes.string
    },

    shouldComponentUpdate: function(){
      return true
    },

    toggle: function(event, column){
        const items = this.props.columns;
        for(var i = 0; i< items.length; i ++){
            if( event.id === items[i].id ){
                if(items[i].required === false){
                    items[i].checked = !event.checked;
                }
            }
        }
        this.setState({items});
    },

    closeListEditor:function(){
        this.props.handleToggle();
    },

    handleApplyCheckBox: function(){
        var newColumns = [];
        for(var i = 0; i< this.props.columns.length; i ++){
            if(this.props.columns[i].checked === true){
                newColumns.push(this.props.columns[i]);
            }
        }
        var ComponentColumns = this.props.componentColumns;
        if(localStorage.getItem(ComponentColumns)){
            var storedWidths = JSON.parse(localStorage.getItem(ComponentColumns));
            for(var p = 0; p< this.props.columns.length; p ++){
                for(var r = 0; r <storedWidths.length; r++){
                    if(this.props.columns[p].id === storedWidths[r].id){
                        storedWidths[r].checked = this.props.columns[p].checked;
                    }
                }
            }
            localStorage.setItem(ComponentColumns, JSON.stringify(storedWidths));
        }else{
            var newUpdatedColumns = [];
            for(var p = 0; p< this.props.columns.length; p ++){
                newUpdatedColumns.push(this.props.columns[p]);
            }
            localStorage.setItem(ComponentColumns, JSON.stringify(newUpdatedColumns));
        }

        this.props.columns = newColumns;

        if(this.props.component === "Commissioning"){
            ReactBootstrap.Dispatcher.emit("Listeditor.selectedColumnsCommissioning", newColumns, this.props.options);
        }else{
            ReactBootstrap.Dispatcher.emit("Listeditor.selectedColumns", newColumns);
        }

        this.props.handleToggle();
    },

    handleApplyAll: function(){
        var allChecked = this.props.columns;
        for(var i = 0; i< allChecked.length; i ++) {
            if ($("#checkAll").is(':checked')) {
                if(allChecked[i].required === false){
                    allChecked[i].checked = true;
                }

            } else {
                if(allChecked[i].required === false){
                    allChecked[i].checked = false;
                }
            }
        }
        this.setState({allChecked});
    },

    render() {
        if(this.props.show){
            return (
                <div>
                    <h4> List Editor
                        <span className="rubix-icon icon-ikons-close ns-list-editor-close-icon"  onClick={this.closeListEditor}></span>
                    </h4>

                    <p>Choose what to show </p>

                    <form role="form" className="form-horizontal" style={{height: this.props.height, overflowY : this.props.overflowY}}>
                    <ul className="ns-list-editor-list" >
                        {this.props.columns.map((column, i) => (
                            <li className="ns-list-editor-item" data-item={column.id} >
                                <label htmlFor={column.id} className="control-label col-sm-3">
                                    {column.name}
                                </label>
                                <input type="checkbox" name={column.name} value={column.id}  checked={column.checked}
                                       onClick={this.toggle.bind(this, column)}  disabled={column.required} />
                            </li>
                        ))
                        }
                        <button className="ns-apply-btn" type="button" onClick={this.handleApplyCheckBox}><b> Apply </b></button>
                        <a class="checkall" style={{marginLeft:"25px"}} onClick={this.handleApplyAll}>
                            <input type="checkbox" style={{marginRight:"10px"}}name="checkAll" id="checkAll" value="checkedAll" />
                            <span style={{color: "black"}}>All</span>
                        </a>
                    </ul>
                    </form>

                </div>
            )
        }
    }

});

module.exports = Listeditor;
