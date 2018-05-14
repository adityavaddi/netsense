var NSN = {
    "customerName":"",
    "customerID":"-1",
    "siteID":"-1",
    "siteName":"",
    "groupID":"-1",
    "nodeID":"-1",
    "overlayID":"-1",
    "notificationID":"-1",
    "alertID":"-1",
    "fixtureID":"-1",
    "userID":"-1",
    "userInfo":{"name":""},
    "apiURL": (production ? config.interface.protocol + "://" + config.interface.host + "/" + config.interface.version + "/" :
        "https://" + config.interface.host + ":" + config.interface.port +  "/" + config.interface.version + "/"),
    "mqttURL": (production ? config.interface.protocol + "://" + config.interface.host + "/streamv1" :
        "https://" + config.interface.host + ":" + config.interface.port + "/streamv1")
};