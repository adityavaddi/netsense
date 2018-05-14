var lm = require('./../../auth/LoginManager.js');

var acl = require('./../../auth/AclManager.js').AclManager;

module.exports = {
    apiKey: function (req, authOrSecDef, scopes, cb) {
        if (req.headers['api_key']) {
            lm.ValidateUserByToken(req, req.headers['api_key'], function (err, fuser) {
                var user = JSON.parse(JSON.stringify(fuser)); // fuser is freezed
                if (err || !user) {
                    req.user = null;
                    req.session.CurrentUser = null;
                    cb();
                } else {
                    var username = user.login.userid;
                    global.log.info('[validateUserByToken]', username, JSON.stringify(user));
                    if (!req.session) {
                        //req.session.regenerate(function(err) {
                        // will have a new session here
                        user.apiUser = true;
                        lm.OnLogin(null, user);
                        cb();
                        //})
                    } else if (!req.session.CurrentUser) {
                        req.session.regenerate(function (err) {
                            // will have a new session here
                            if (user) {
                                user.apiUser = true;
                                req.user = user;
                                req.session.CurrentUser = user;
                                lm.OnLogin(null, user);
                                cb();
                            } else {
                                req.user = null;
                                req.session.CurrentUser = null;
                                cb();
                            }
                        })
                    } else if (req.session.CurrentUser.username != username) {
                        req.session.regenerate(function (err) {
                            // will have a new session here
                            // re-login
                            req.user = user;
                            if (user) {
                                user.apiUser = true;
                                req.user = user;
                                lm.OnLogin(null, user);
                                req.session.CurrentUser = user;
                            } else {
                                req.user = null;
                                req.session.CurrentUser = null;
                            }
                            cb();
                        })
                    } else {
                        //global.log.info(err, user);
                        if (user) {
                            user.apiUser = true;
                            req.user = user;
                            req.session.CurrentUser = user;
                        }
                        cb();
                    }
                }
            });
        } else {
            req.user = null;
            cb();
        }
    }
};
