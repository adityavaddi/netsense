"use strict";


var cache = {

};

cache.db={};


exports.LocalCache = cache;
/**
 * Gets the cached value for the given key, or null if none is found.
 * @param key String
 */
cache.get = function(key){
    if(cache.db!=undefined && key!=undefined && cache.db[key])
        return cache.db[key];
    return null;
}

/**
 * Gets the cached value for the given key, or null if none is found.
 * @param key String
 */
cache.getAsync = function(key, on_success, on_failure){
    if(cache.db!=undefined && key!=undefined && cache.db[key])
    {
        return on_success(key, cache.db[key]);
    }
    return on_failure(key);
}

/**
 * Gets the cached value for the given key, or null if none is found.
 * @param key String
 */
cache.removeAsync = function(key, on_success, on_failure){
    if(cache.db!=undefined && key!=undefined && cache.db[key])
    {
        delete cache.db[key];
        return on_success(key);
    }
    return on_failure(key);
}

/**
 * Object	Returns a JavaScript Object containing all key/value pairs found in the cache for an array of keys.
 * @param keys
 */
cache.getAll = function(keys){
    var ret = [];
    for(var i=0;i<keys.length;i++){
        if(cache.db[keys[i]])
            ret.push(cache.db[keys[i]]);
    }
    return cache.db;
}

/**
 * void	Adds a key/value pair to the cache.
 */
cache.put = function(key, value){

    if(cache.db && key!=undefined)
        cache.db[key] = value;
}

/**
 * void	Adds a key/value pair to the cache, with an expiration time (in seconds).
 */
//cache.cacheput = function(key, value, expirationInSeconds){
//
//}

/**
 * void	Adds a set of key/value pairs to the cache.
 */
cache.setAdd = function(key, values)	{

    if(cache.db && key!=undefined){
        if(!cache.db[key] ){
            cache.db[key] = [];
        }
        if(cache.db[key] && cache.db[key].indexOf(value)===-1){
            cache.db[key].push(value);

        }
    }
}

/**
 * void	Adds a set of key/value pairs to the cache, with an expiration time (in seconds)
 * @param values
 * @param expirationInSeconds
 */
//cache.putAll = function(values, expirationInSeconds){
//
//}

/**
 *
 * @param key
 */
cache.remove = function(key){
    if(cache.db[key]!==undefined)
        delete cache.db[key];
}

/**
 *
 * @param keys
 */
cache.removeAll = function(keys){

    for(var i= 0; i<keys.length; i++){
        if(cache.db[i]!==undefined)
            delete cache.db[i];
    }
}
