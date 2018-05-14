MATCH (n:Node)
WHERE n:Unassigned OR n:Inactive
RETURN {
nodeid: n.nodeid,
name: n.name,
model: n.model,
latitude: n.latitude,
longitude: n.longitude,
altitude: n.altitude,
ip: n.ip,
building: n.building,
level: n.level,
meshId: n.meshId,
note: n.note,
baseStation: n.baseStation,
publicKey: n.publicKey,
signature: n.signature,
remoteNetwork: n.remoteNetwork,
bssid: n.bssid,
configToken: n.configToken,
softwareVersion: n.softwareVersion,
mfgDate: n.mfgDate,
circuit: n.circuit,
label: labels(n)
} AS items
