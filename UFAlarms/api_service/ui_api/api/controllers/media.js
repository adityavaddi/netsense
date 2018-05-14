'use strict';

var uuid = require('uuid'),
    encoder = require('./../../encoder/Encoder.js').JSONEncoder,
    response = require('./../helpers/response.js');

module.exports = {
    imageCaptureFromNode: imageCaptureFromNode,
};

/**
 * Get an image from the node
 * @param req
 * @param res
 */
function imageCaptureFromNode(req, res) {
    var orgid = req.swagger.params.orgid.value,
        siteid = req.swagger.params.siteid.value,
        nodeid = req.swagger.params.nodeid.value,
        channel = req.swagger.params.channel.value,
        resolution = req.swagger.params.resolution.value,
        sicReq = req.body,
        imgDate = sicReq.imgTS;

    global.log.info("imageCaptureFromNode: " + JSON.stringify(sicReq));

    var ts = getTS(req, res);
    var fmt = getFmt(req, res);

    global.log.info("imgCapture: ts : " + ts + "; fmt: " + fmt);

    encoder.imageCaptureFromNode(req.request_id, req.getCurrentUser(), orgid, siteid, nodeid, channel, resolution, ts, fmt, sicReq.description, function (err, msg) {
            response.Done(err, msg, res, req);
        });
}

function getTS(req, res) {
    var sicReq = req.body;
    if(sicReq) {
        if (sicReq.imgTS) {
            try {
              var dt = new Date(sicReq.imgTS);
              var dtISO = dt.toISOString()
              global.log.info("TS ISO: " + dtISO)
            }
            catch(ex) {
              console.log("ex: "+ ex)
              response.Done({status:400, message:"Request contains invalid date(timestamp)"}, null, res, req);
            }
        }
    }
    return "";
}

function getFmt(req, res) {
    var sicReq = req.body;
    if (sicReq) {
        if (sicReq.imgFormat) {
            if (["jpg", "JPG", "png", "PNG"].indexOf(sicReq.imgFormat) > -1) {
                return sicReq.imgFormat.toLowerCase();
            } else  {
                response.Done({status:400, message:"Request contains unsupported image format"}, null, res, req); 
            }
        }
    }
    return "jpg";
}

