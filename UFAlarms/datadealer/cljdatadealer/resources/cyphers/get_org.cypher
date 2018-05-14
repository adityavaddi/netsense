MATCH(o:Org {orgid: {props}.orgid})
OPTIONAL MATCH (o)-[:IS_CUSTOMER_OF]->(po:Org)
RETURN {
orgid: o.orgid,
name: o.name,
type: o.type,
street1: o.street1,
street2: o.street2,
city: o.city,
state: o.state,
postal_code: o.postal_code,
country: o.country,
contact_email: o.contact_email,
contact_phone: o.contact_phone,
contact_name: o.contact_name,
po: po.orgid,
pname: po.name
} AS org
