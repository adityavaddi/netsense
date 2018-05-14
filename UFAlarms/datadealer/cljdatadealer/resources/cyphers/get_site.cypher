MATCH(s:Site {siteid: {props}.siteid})
RETURN {
siteid: s.siteid,
name: s.name,
street1: s.street1,
street2: s.street2,
city: s.city,
state: s.state,
postal_code: s.postal_code,
country: s.country,
latitude: s.latitude,
longitude: s.longitude,
altitude: s.altitude,
country_code: s.country_code,
time_zone: s.time_zone,
contact_name: s.contact_name,
contact_email: s.contact_email,
contact_phone: s.contact_phone
} AS site
