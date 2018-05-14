'use strict';

var util = require('util');
var passport = require("passport");
var login_mngr = require('./../../auth/LoginManager.js');
var LocalStrategy = require('passport-local').Strategy;
passport.use( new LocalStrategy( {
        usernameField : 'email',
        passwordField : 'password',
        passReqToCallback : true
    },login_mngr.ValidateLocalUser ) );

module.exports = {
    whoAmI: whoAmI,
    auth: auth,
    logout: logout,
};

// pad an integer to make a digits-length string
function pad(number, digits) {
    return Array(Math.max(digits - String(number).length + 1, 0)).join(0) + number;
};

// request module will be used to make the request to neo4j
var request = require('request');
var response = require('./../helpers/response.js');


/**
 * Return current user data
 * @param req
 * @param res
 * @returns {*}
 */
function whoAmI(req, res) {
        //global.log.info('user', req.session);
        var user = req.getCurrentUser();
        var err = null;
        //var user = (req.session && req.session.CurrentUser)? JSON.parse(JSON.stringify(req.session.CurrentUser)):null;
        if(user && user.login && user.login.password){
            delete user.login.password;
            //global.log.info("Result: " + JSON.stringify(user||null));
        }
        if (!user){
            err = {
                error: true,
                message: 'User not found',
                status: 404,
            }
        }

        return response.Done(err, user, res, req);

}

function auth(req, res, next) {
    var cb = function(err, user, info) {
        req.session.CurrentUser = null;
        if (err) {
            return response.Done({error: true, status: message.status || 500, message: err.message}, null, res, req);
        }
        if (!user) {
            //return res.status(404).json({message: 'User not found.'});
            return response.Done({error: true, status: 404, message: 'User not found.'}, null, res, req);
        }
        req.logIn(user, function(err) {
            if (err) {
                return response.Done({error: true, status: message.status || 500, message: err.message}, null, res, req);
            }
            var cuser = JSON.parse(JSON.stringify(user));
            // TODO: Remove these fixes
            if(cuser && !cuser.name)
                cuser.name = cuser.email;
            req.session.CurrentUser = JSON.parse(JSON.stringify(cuser));
            login_mngr.OnLogin(null, cuser);
            // TODO: Remove these fixes
            if(cuser && cuser.login && cuser.login.password)
                delete cuser.login.password;
            //return res.status(200).json(cuser);
            return response.Done(null, cuser, res, req)
        });
    }
    req.body.email = req.body.email.toLowerCase();
    passport.authenticate('local', cb)(req,res,next);
}

function logout(req, res) {
    var user = req.getCurrentUser();
    login_mngr.OnLogout(user, function(err, params) {
        try{
            req.session.CurrentUser = null;
            req.logout();
            req.session.destroy(function (err) {
                if (!err) {
                    res.clearCookie('sess', {path: '/'});
                    return response.Done(null, {"message":"Success"}, res, req);
                } else {
                    // handle error case...
                    global.log.error("Could not destroy session", err.message);
                    return response.Done({error: true, status: 500, message: err.message}, null, res, req);
                }

            });
        } catch(e){
            //res.status(500).json(e);
            return response.Done({error: true, status: 500, message: e.message}, null, res, req);
        }
    }, null);
}
