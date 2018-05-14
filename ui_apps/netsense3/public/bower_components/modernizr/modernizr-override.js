/*!
* modernizr-override v1.0.0
*
* Copyright (c)
* Jon Egerton
* MIT License
*/

/*
* Modernizr-Override
* Modernizr performs feature detection based on the current browser.
* In some circumstances during development/testing it is desirable to override
* these detections with imported values from some other source, such as a mobile device.
* Modernizr-Override facilitates this by allowing an object to be passed containing the overridden properties.
* CSS classes will also be reset
*
* For updates or to feedback, see https://github.com/ja2/modernizr-override
* For details of the Modernizr library, see: https://modernizr.com/
*/

; (function (window, document, override) {

    /**
    * docElement is a convenience wrapper to grab the root element of the document
    *
    * @access private
    * @returns {HTMLElement|SVGElement} The root element of the document
    */
    var docElement = document.documentElement;

    /**
    * A convenience helper to check if the document we are running in is an SVG document
    *
    * @access private
    * @returns {boolean}
    */
    var isSVG = docElement.nodeName.toLowerCase() === 'svg';

    /**
    * docElement is a convenience wrapper to grab the root element of the document
    *
    * @access private
    * @returns Modernizr: The populated instance of Modernizr
    */
    var Modernizr = window.Modernizr;

    /**
    * enableClasses is a convenience wrapper to grab the enableClasses config from Modernizr
    *
    * @access private
    * @returns Modernizr: The populated instance of Modernizr
    */
    var enableClasses = Modernizr._config.enableClasses;

    /**
    * updateModernizr updates the Modernizr object and updates the classes list
    *
    * @access private
    * @function setClasses
    * @param {string[]} classes - Array of class names
    */
    function updateModernizr(classes) {

        var oldClassName = null,
            newClassName = null;

        //Loop on properties of override
        for (var prop in override) {
            if (override.hasOwnProperty(prop)) {

                //Apply override to Modernizr object
                if (Modernizr.hasOwnProperty(prop)) {

                    //Overide existing property
                    if (Modernizr[prop] !== override[prop]) {
                        Modernizr[prop] = override[prop];

                        //Swap out the old class for the new
                        if (enableClasses) {
                            oldClassName = ((override[prop]) ? "no-" : "") + prop.toLowerCase();
                            newClassName = ((override[prop]) ? "" : "no-") + prop.toLowerCase();
                            classes.splice(classes.indexOf(oldClassName), 1, newClassName);
                        }
                    };
                } else {
                    //Add new Property
                    Modernizr[prop] = override[prop];

                    //Add to the classes
                    if (enableClasses) {
                        classes.push(((override[prop]) ? "" : "no-") + prop.toLowerCase());
                    }
                }
            }
        }
    }

    /**
    * setClasses takes an array of class names and adds them to the root element
    * Copied from the Modernizr source and tweaked to suit purpose
    *
    * @access private
    * @function setClasses
    * @param {string[]} classes - Array of class names
    */
    function setClasses(classes) {
        var className = docElement.className;
        var classPrefix = Modernizr._config.classPrefix || '';

        if (!enableClasses) return;

        if (isSVG) {
            className = className.baseVal;
        }

        // Add the new classes
        className = classPrefix + classes.join(' ' + classPrefix);
        isSVG ? docElement.className.baseVal = className : docElement.className = className;
    }

    //Check there's anything to do
    if (override === undefined || override === null) return;
    var classes = (enableClasses) ? docElement.className.split(" ") : [];
    updateModernizr(classes);
    setClasses(classes);

})(window, document, window.ModernizrOverride);
