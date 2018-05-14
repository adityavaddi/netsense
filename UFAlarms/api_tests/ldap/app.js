/**
 * LDAP in-memory server
 * @type {*|exports|module.exports}
 */
"use strict";

var ldap = require('ldapjs');

var configManager = require('kea-config');
configManager.init('./config/main.conf.js');

///--- Shared handlers

function authorize(req, res, next) {
    /* Any user may search after bind, only cn=root has full power */
    var isSearch = (req instanceof ldap.SearchRequest);
    if (!req.connection.ldap.bindDN.equals(configManager.get('ldap.adminDN')) && !isSearch)
        return next(new ldap.InsufficientAccessRightsError());

    return next();
}

var db = {
};

var server = ldap.createServer();

server.bind(configManager.get('ldap.adminDN'), function(req, res, next) {
    if (req.dn.toString().replace(/\s/g,'') !== configManager.get('ldap.adminDN') || req.credentials !== configManager.get('ldap.adminPass'))
        return next(new ldap.InvalidCredentialsError());

    res.end();
    return next();
});

server.add(configManager.get('ldap.suffixUsers'), authorize, function(req, res, next) {
    var dn = req.dn.toString();

    if (db[dn])
        return next(new ldap.EntryAlreadyExistsError(dn));

    db[dn] = req.toObject().attributes;
    res.end();
    return next();
});
server.add(configManager.get('ldap.suffixApiKeys'), authorize, function(req, res, next) {
    var dn = req.dn.toString();

    if (db[dn])
        return next(new ldap.EntryAlreadyExistsError(dn));

    db[dn] = req.toObject().attributes;
    res.end();
    return next();
});

server.bind(configManager.get('ldap.suffixUsers'), function(req, res, next) {
    var dn = req.dn.toString();
    if (!db[dn])
        return next(new ldap.NoSuchObjectError(dn));

    if (!db[dn].userpassword)
        return next(new ldap.NoSuchAttributeError('userPassword'));

    if (db[dn].userpassword.indexOf(req.credentials) === -1)
        return next(new ldap.InvalidCredentialsError());

    res.end();
    return next();
});

server.bind(configManager.get('ldap.suffixApiKeys'), function(req, res, next) {
    var dn = req.dn.toString();
    if (!db[dn])
        return next(new ldap.NoSuchObjectError(dn));

    if (!db[dn].userpassword)
        return next(new ldap.NoSuchAttributeError('userPassword'));

    if (db[dn].userpassword.indexOf(req.credentials) === -1)
        return next(new ldap.InvalidCredentialsError());

    res.end();
    return next();
});

server.compare(configManager.get('ldap.suffixUsers'), authorize, function(req, res, next) {
    var dn = req.dn.toString();
    if (!db[dn])
        return next(new ldap.NoSuchObjectError(dn));

    if (!db[dn][req.attribute])
        return next(new ldap.NoSuchAttributeError(req.attribute));

    var matches = false;
    var vals = db[dn][req.attribute];
    for (var i = 0; i < vals.length; i++) {
        if (vals[i] === req.value) {
            matches = true;
            break;
        }
    }

    res.end(matches);
    return next();
});

server.del(configManager.get('ldap.suffixUsers'), authorize, function(req, res, next) {
    var dn = req.dn.toString();
    if (!db[dn])
        return next(new ldap.NoSuchObjectError(dn));

    delete db[dn];

    res.end();
    return next();
});

server.del(configManager.get('ldap.suffixApiKeys'), authorize, function(req, res, next) {
    var dn = req.dn.toString();
    if (!db[dn])
        return next(new ldap.NoSuchObjectError(dn));

    delete db[dn];

    res.end();
    return next();
});

server.modify(configManager.get('ldap.suffixUsers'), authorize, function(req, res, next) {
    var dn = req.dn.toString();
    if (!req.changes.length)
        return next(new ldap.ProtocolError('changes required'));
    if (!db[dn])
        return next(new ldap.NoSuchObjectError(dn));

    var entry = db[dn];

    for (var i = 0; i < req.changes.length; i++) {
        var mod = req.changes[i].modification;
        switch (req.changes[i].operation) {
            case 'replace':
                if (!entry[mod.type])
                    return next(new ldap.NoSuchAttributeError(mod.type));

                if (!mod.vals || !mod.vals.length) {
                    delete entry[mod.type];
                } else {
                    entry[mod.type] = mod.vals;
                }

                break;

            case 'add':
                if (!entry[mod.type]) {
                    entry[mod.type] = mod.vals;
                } else {
                    mod.vals.forEach(function(v) {
                        if (entry[mod.type].indexOf(v) === -1)
                            entry[mod.type].push(v);
                    });
                }

                break;

            case 'delete':
                if (!entry[mod.type])
                    return next(new ldap.NoSuchAttributeError(mod.type));

                delete entry[mod.type];

                break;
        }
    }

    res.end();
    return next();
});

server.modify(configManager.get('ldap.suffixApiKeys'), authorize, function(req, res, next) {
    var dn = req.dn.toString();
    if (!req.changes.length)
        return next(new ldap.ProtocolError('changes required'));
    if (!db[dn])
        return next(new ldap.NoSuchObjectError(dn));

    var entry = db[dn];

    for (var i = 0; i < req.changes.length; i++) {
        var mod = req.changes[i].modification;
        switch (req.changes[i].operation) {
            case 'replace':
                if (!entry[mod.type])
                    return next(new ldap.NoSuchAttributeError(mod.type));

                if (!mod.vals || !mod.vals.length) {
                    delete entry[mod.type];
                } else {
                    entry[mod.type] = mod.vals;
                }

                break;

            case 'add':
                if (!entry[mod.type]) {
                    entry[mod.type] = mod.vals;
                } else {
                    mod.vals.forEach(function(v) {
                        if (entry[mod.type].indexOf(v) === -1)
                            entry[mod.type].push(v);
                    });
                }

                break;

            case 'delete':
                if (!entry[mod.type])
                    return next(new ldap.NoSuchAttributeError(mod.type));

                delete entry[mod.type];

                break;
        }
    }

    res.end();
    return next();
});

server.search(configManager.get('ldap.suffixUsers'), authorize, function(req, res, next) {
    var dn = req.dn.toString();
//    if (!db[dn])
//        return next(new ldap.NoSuchObjectError(dn));

    var scopeCheck;

    switch (req.scope) {
        case 'base':
            if (!db[dn])
                return next(new ldap.NoSuchObjectError(dn));
            if (req.filter.matches(db[dn])) {
                res.send({
                    dn: dn,
                    attributes: db[dn]
                });
            }

            res.end();
            return next();

        case 'one':
            scopeCheck = function(k) {
                if (req.dn.equals(k))
                    return true;

                var parent = ldap.parseDN(k).parent();
                return (parent ? parent.equals(req.dn) : false);
            };
            break;

        case 'sub':
            scopeCheck = function(k) {
                return (req.dn.equals(k) || req.dn.parentOf(k));
            };

            break;
    }

    Object.keys(db).forEach(function(key) {
        if (!scopeCheck(key))
            return;

        if (req.filter.matches(db[key])) {
            res.send({
                dn: key,
                attributes: db[key]
            });
        }
    });

    res.end();
    return next();
});

server.search(configManager.get('ldap.suffixApiKeys'), authorize, function(req, res, next) {
    var dn = req.dn.toString();
    // Don't use this! It breaks search
    // if (!db[dn])
    //     return next(new ldap.NoSuchObjectError(dn));

    var scopeCheck;

    switch (req.scope) {
        case 'base':
            if (req.filter.matches(db[dn])) {
                res.send({
                    dn: dn,
                    attributes: db[dn]
                });
            }

            res.end();
            return next();

        case 'one':
            scopeCheck = function(k) {
                if (req.dn.equals(k))
                    return true;

                var parent = ldap.parseDN(k).parent();
                return (parent ? parent.equals(req.dn) : false);
            };
            break;

        case 'sub':
            scopeCheck = function(k) {
                return (req.dn.equals(k) || req.dn.parentOf(k));
            };

            break;
    }

    Object.keys(db).forEach(function(key) {
        if (!scopeCheck(key))
            return;

        if (req.filter.matches(db[key])) {
            res.send({
                dn: key,
                attributes: db[key]
            });
        }
    });

    res.end();
    return next();
});



///--- Fire it up
var crypto = require('crypto');


exports.Run = function(){
    server.listen(1389, function() {
        console.log('LDAP server up at: %s', server.url);

        // dict of email -> key
        var dict_email_key = {};
        // dict of email -> keycn
        var dict_email_keycn = {}; // email and key.

        dict_email_key["anon@sensity.com"] = '33JafeNgHGqE4MTsJU9KZn3aaUNraX9gNJ5LeeUwhjbyCm7q';
        dict_email_key["uberuser@sensity.com"] = '33JafeNgHGqE4MTsJU9KZn3aaUNraX9gNJ5LeeUwhjbyCm7w';
        dict_email_key["admin-1@sensity.com"] = '33JafeNgHGqE4MTsJU9KZn3aaUNraX9gNJ5LeeUwhjbyCm7z';

        dict_email_keycn["anon@sensity.com"] = crypto.createHash('sha256').update(dict_email_key["anon@sensity.com"]).digest('hex');
        dict_email_keycn["uberuser@sensity.com"] = crypto.createHash('sha256').update(dict_email_key["uberuser@sensity.com"]).digest('hex');
        dict_email_keycn["admin-1@sensity.com"] = crypto.createHash('sha256').update(dict_email_key["admin-1@sensity.com"]).digest('hex');

        // Insert some data
        var data = [
            {
                dn:'cn=sensity_admin@sensity.com'+','+configManager.get('ldap.suffixUsers'),
                objectClass: "inetOrgPerson",
                uid: "114ad560-a046-11e5-a57f-ef24ae600576",
                sn: "sensity_admin@sensity.com",
                mail: "sensity_admin@sensity.com",
                cn: "sensity_admin",
                userPassword: "admin123"
            },
            {
                dn:'cn=sensity_user@sensity.com'+','+configManager.get('ldap.suffixUsers'),
                objectClass: "inetOrgPerson",
                uid: "507ef3cc-cd2d-46d8-ae6d-7ccf430c1110",
                sn: "sensity_user@sensity.com",
                mail: "sensity_user@sensity.com",
                cn: "sensity_user",
                userPassword: "admin123"
            },
            {
                dn:'cn=sensity_read_only@sensity.com'+','+configManager.get('ldap.suffixUsers'),
                objectClass: "inetOrgPerson",
                uid: "47e3e2f2-b93b-4557-b668-271d43d028ac",
                sn: "sensity_read_only@sensity.com",
                mail: "sensity_read_only@sensity.com",
                cn: "sensity_read_only",
                userPassword: "admin123"
            },
            {
                dn:'cn=ge_admin@ge.com'+','+configManager.get('ldap.suffixUsers'),
                objectClass: "inetOrgPerson",
                uid: "06e5fe40-bbe1-11e5-87f9-f993da16de58",
                sn: "ge_admin@ge.com",
                mail: "ge_admin@ge.com",
                cn: "partner_admin",
                userPassword: "admin123"
            },
            {
                dn:'cn=endcustomer@customer.com'+','+configManager.get('ldap.suffixUsers'),
                objectClass: "inetOrgPerson",
                uid: "20163f00-bbe2-11e5-b034-41a2c9d33321",
                sn: "endcustomer@customer.com",
                mail: "endcustomer@customer.com",
                cn: "end_user_admin",
                userPassword: "admin123"
            },
            {
                dn:'cn=anon@sensity.com'+','+configManager.get('ldap.suffixUsers'),
                //cn: 'cn=anon',
                //email: 'anon@sensity.com',
                //userPassword: 'fish',
                //objectclass: 'Person'
                objectClass: "inetOrgPerson",
                uid: "anon@sensity.com",
                sn: "anon@sensity.com",
                mail: "anon@sensity.com",
                cn: "anon@sensity.com",
                userPassword: "fish",
            },
            {
                dn:'cn=uberuser@sensity.com'+','+configManager.get('ldap.suffixUsers'),
                //cn: 'cn=uberuser',
                //email: 'uber@sensity.com',
                //userPassword: 'ubeR$23',
                //objectclass: 'Person'
                objectClass: "inetOrgPerson",
                uid: "uberuser@sensity.com",
                sn: "uberuser@sensity.com",
                mail: "uberuser@sensity.com",
                cn: "uberuser@sensity.com",
                userPassword: "ubeR$23",
            },
            {
                dn:'cn=admin-1@sensity.com'+','+configManager.get('ldap.suffixUsers'),
                //cn: 'cn=admin-1',
                //email: 'admin1@sensity.com',
                //userPassword: 'admin-1',
                //objectclass: 'Person'
                objectClass: "inetOrgPerson",
                uid: "admin-1@sensity.com",
                sn: "admin-1@sensity.com",
                mail: "admin-1@sensity.com",
                cn: "admin-1@sensity.com",
                userPassword: "admin-1",
            },
            {
                dn:'cn='+ dict_email_keycn['anon@sensity.com'] +','+configManager.get('ldap.suffixApiKeys'),
                //cn: 'cn=33JafeNgHGqE4MTsJU9KZn3aaUNraX9gNJ5LeeUwhjbyCm7q',
                //username: 'anon',
                //userPassword: '33JafeNgHGqE4MTsJU9KZn3aaUNraX9gNJ5LeeUwhjbyCm7q',
                //objectclass: 'ApiKey',
                //expires: '2016-01-01T00:00:00.0000'
                objectClass: ['inetOrgPerson', 'shadowAccount'],
                cn: 'cn='+dict_email_keycn['anon@sensity.com'],
                uid: 'anon@sensity.com',
                sn: 'anon@sensity.com',
                userPassword: dict_email_key['anon@sensity.com'],
                shadowExpire: 18000
            },
            {
                dn:'cn=' + dict_email_keycn['anon@sensity.com'] +','+configManager.get('ldap.suffixApiKeys'),
                //cn: 'cn=33JafeNgHGqE4MTsJU9KZn3aaUNraX9gNJ5LeeUwhjbyCm7w',
                //username: 'uberuser',
                //userPassword: '33JafeNgHGqE4MTsJU9KZn3aaUNraX9gNJ5LeeUwhjbyCm7w',
                //objectclass: 'ApiKey',
                //expires: '2018-01-01T00:00:00.0000'
                objectClass: ['inetOrgPerson', 'shadowAccount'],
                cn: 'cn='+dict_email_keycn['uberuser@sensity.com'],
                uid: 'uberuser@sensity.com',
                sn: 'uberuser@sensity.com',
                userPassword: dict_email_key['uberuser@sensity.com'],
                shadowExpire: 18000
            },
            {
                dn:'cn=' + dict_email_keycn['anon@sensity.com'] +','+configManager.get('ldap.suffixApiKeys'),
                //cn: 'cn=33JafeNgHGqE4MTsJU9KZn3aaUNraX9gNJ5LeeUwhjbyCm7z',
                //username: 'admin-1',
                //userPassword: '33JafeNgHGqE4MTsJU9KZn3aaUNraX9gNJ5LeeUwhjbyCm7z',
                //objectclass: 'ApiKey',
                //expires: '2018-01-01T00:00:00.0000'
                objectClass: ['inetOrgPerson', 'shadowAccount'],
                cn: 'cn='+dict_email_keycn['admin-1@sensity.com'],
                uid: 'admin-1@sensity.com',
                sn: 'admin-1@sensity.com',
                userPassword: dict_email_key['admin-1@sensity.com'],
                shadowExpire: 18000
            }
        ];
        var domain = require('domain');
        var d = domain.create();

        d.on('error', function(err) {
            global.log.error('Error creating ldap client: %s', err.message, JSON.stringify(err));
        });
        // Connect to local ldap server and add demo data
        d.run(function() {
            try{
                var client = ldap.createClient({
                    url: configManager.get('ldap.jsURL')
                }).on('connect', function(){
                    client.bind(configManager.get('ldap.adminDN'), configManager.get('ldap.adminPass'), function(err) {
                        if(err){
                            global.log.error("Error %s binding to ldap", err.message, JSON.stringify(data), JSON.stringify(err));
                        } else {
                            for(var i=0; i<data.length; i++){
                                var dn = data[i].dn;
                                client.add(dn, data[i], function(err) {
                                    if(err){
                                        global.log.error("Error %s adding to ldap as %s", err.message, dn, JSON.stringify(data), JSON.stringify(err));
                                    }
                                });

                            }
                        }
                    });
                });
            } catch(e){
                console.error(e.message)
            }
        });

    });
}
//run();
