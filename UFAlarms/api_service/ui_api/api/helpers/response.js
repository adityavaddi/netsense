'use strict';

module.exports = {
    Done: Done
};

/**
 * Response  based on error and data
 * @param err object Node error JSON object
 * @param data object Data returned as JSON object
 * @param res object Response
 * @param req object Request
 */
function Done(err, data, res, req){
    try{
        if((req && req.timedout) || (res && res.headersSent)) {
            // TODO: Recover from timeout
            global.log.error("Request %s timedout", req.originalUrl);
        } else if(!err && data) {
            global.log.info("Success" /*, (data?JSON.stringify(data):'')*/);
            if(data && data.status && data.status!=204){
                res.setHeader('Content-Type', 'application/json');
                res.status(data.status).json(data);
            }
            else if(!err && data && (data instanceof Array) ) {
                // Send success with whatever data is returned - empty array is still valid data
                res.status(200).json(data);
            } else if(!data || data.status==204 || !(Object.keys(data)) || !(Object.keys(data).length)) {
                // Send success with whatever data is returned - empty array is still valid data
                res.status(204);//.json({});
            } else {
                res.setHeader('Content-Type', 'application/json');
                res.status(200).json(data);
            }
        } else if(!err && !data) {
            global.log.info("Success" /*, (data?JSON.stringify(data):'')*/);
            // Send success with whatever data is returned - empty array is still valid data
            //res.setHeader('Content-Type', 'application/json');
            res.status(204);//.json({});
        } else {
            global.log.info("Error: ", (err?JSON.stringify(err):''));
            res.setHeader('Content-Type', 'application/json');
            // Check if err.status is a number
            var status = isNaN(err.status) ? 500 : err.status;
            if(err.error && isNaN(err.status)){
                err.status = status;
            }
            if(!status)
                status = (err.message &&
                            (err.message.indexOf('not authorized')!=-1 ||
                             err.message.indexOf('not granted')!=-1 ||
                             err.message.indexOf('ccess denied')!=-1))?403:400; // TODO: You can do it better
            res.status(status).json(err);
        }
        res.end();

    } catch(err){
        global.log.error(JSON.stringify(err));
    }

}
