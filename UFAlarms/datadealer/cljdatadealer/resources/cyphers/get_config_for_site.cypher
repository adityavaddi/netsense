MATCH (s:Site {siteid:{props}.siteid})
RETURN {
network_region: s.country_code
} AS item
