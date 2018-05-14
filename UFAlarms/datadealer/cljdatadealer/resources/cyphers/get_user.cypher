MATCH(u:User {userid: {props}.userid})
RETURN {
userid: u.userid,
name: u.name,
email: u.email,
phone: u.phone,
title: u.title,
roles: u.roles,
sites: u.sites,
created: u.created,
updated: u.updated
} AS user
