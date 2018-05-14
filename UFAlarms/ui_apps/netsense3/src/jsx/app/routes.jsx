import { Route, Router, Redirect } from 'react-router';
import { browserHistory } from 'react-router';
import { hashHistory } from 'react-router';

import Permissions from 'routes/docs/permissions';
import Sensors from 'routes/docs/sensors';

import Nodepanel from 'routes/nodepanel';
import Grouppanel from 'routes/grouppanel';
import Trafficconfigpanel from 'routes/trafficconfigpanel';
import Schedulepanel from 'routes/schedulepanel';
import Sitepanel from 'routes/sitepanel';
import Customerpanel from 'routes/customerpanel';
import CommissioningPanel from 'routes/commissioningpanel';
import Audits from 'routes/auditpanel';
import Overlays from 'routes/overlaypanel';
import Notifications from 'routes/notificationpanel';
import Fixtures from 'routes/fixturepanel';
import Firmware from 'routes/firmwarepanel';
import Firmwareupdate from 'routes/firmwareupdatepanel';
import Usermanagement from 'routes/usermanagementpanel';
import Suspendedusers from 'routes/suspendeduserspanel';
import Daylightpanel from 'routes/daylightpanel';
import Proximitypanel from 'routes/proximitypanel';
import Parkinggrouppanel from 'routes/parkinggrouppanel';
import Parkingzonepanel from 'routes/parkingzonepanel';
import Reportingpanel from 'routes/reportinglookerpanel';
import Energypanel from 'routes/energylookerpanel';
import ParkingDashboardpanel from 'routes/parkingdashboardpanel';
import Parkingspacepanel from 'routes/parkingspacepanel';
import UFAlarmpanel from 'routes/ufalarmpanel'

import Configpanel from 'routes/configpanel';

import Notfound from 'routes/notfound';
import Blank from 'routes/blank';

import Login from 'routes/login';

import ResetPasswordPanel from 'routes/resetpasswordpanel';

export default (withHistory, onUpdate, hsty) => {
  const history = withHistory?
                  (Modernizr.history ?
                    browserHistory
                  : hashHistory)
                : (hsty ? hsty : null);
  history.listen(function(ev){ReactBootstrap.Dispatcher.emit("Router.changed", ev.pathname)});
  return (
    <Router history={history} onUpdate={onUpdate}>
      <Route path='/reset-password-request' component={ResetPasswordPanel} />
      <Route path='/' component={Login} />

      <Route path='/app/nodepanel' component={Nodepanel} />
      <Route path='/app/grouppanel' component={Grouppanel} />
      <Route path='/app/trafficconfigpanel' component={Trafficconfigpanel} />
      <Route path='/app/schedulepanel' component={Schedulepanel} />
      <Route path='/app/sitepanel' component={Sitepanel} />
      <Route path='/app/daylightpanel' component={Daylightpanel} />
      <Route path='/app/proximitypanel' component={Proximitypanel} />
      <Route path='/app/parkinggrouppanel' component={Parkinggrouppanel} />
      <Route path='/app/parkingzonepanel' component={Parkingzonepanel} />
      <Route path='/app/parkingspacepanel' component={Parkingspacepanel} />
      <Route path='/app/auditpanel' component={Audits} />
      <Route path='/app/overlaypanel' component={Overlays} />
      <Route path='/app/notificationpanel' component={Notifications} />
      <Route path='/app/ufalarmpanel' component={UFAlarmpanel} />
      <Route path='/app/fixturepanel' component={Fixtures} />
      <Route path='/app/firmwarepanel' component={Firmware} />
      <Route path='/app/firmwareupdatepanel' component={Firmwareupdate} />
      <Route path='/app/usermanagementpanel' component={Usermanagement} />
      <Route path='/app/suspendeduserspanel' component={Suspendedusers} />
      <Route path='/app/login' component={Login} />

      <Route path='/app/customerpanel' component={Customerpanel} />
      <Route path='/app/commissioningpanel' component={CommissioningPanel} />
      <Route path='/app/reportinglookerpanel' component={Reportingpanel} />
      <Route path='/app/energylookerpanel' component={Energypanel} />
      <Route path='/app/parkingdashboardpanel' component={ParkingDashboardpanel} />
      <Route path='/app/configpanel' component={Configpanel} />

      <Route path='/app/docs/permissions' component={Permissions} />
      <Route path='/app/docs/sensors' component={Sensors} />
      <Route path='/app/blank' component={Blank} />      
      <Route path='*' component={Notfound} />
    </Router>
  );
};
