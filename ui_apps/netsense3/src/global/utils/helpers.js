var helpers = {

    getCookie: function(name) {
        var pattern = RegExp(name + "=.[^;]*");
        var matched = document.cookie.match(pattern);
        if (matched) {
          var cookie = matched[0].split('=');
          return cookie[1];
        }
        return false;
    },

    windowHeight: function() {
        // required for Safari
        return window.innerHeight || $(window).height();
    },

    calcHeight: function(pct, extra) {
        // this should be executed only on the client
        //   -- just in case it executes on the server, it will not fail but will return 500
        var h = (window && window.document) ? ((this.windowHeight() - 75) * (pct / 100) + extra) : 500;
        return h;
    },

    calcSitePanelHeight: function(pct, extra) {
        // this should be executed only on the client
        //   -- just in case it executes on the server, it will not fail but will return 500
        var h = (window && window.document) ? ((this.windowHeight() - 55) * (pct / 100) + extra) : 500;
        return h;
    },

    get_idx: function(arr, elem, field) {
        // given an array, an object and a field name, 
        //   returns the index of the array element that matches the object (field value match)
        for (var i = 0, found = false; !found && i < arr.length; i++) {
            found = arr[i][field] == elem[field];
        };
        return --i;
    },

    ucfirst: function(string) {
        return string.charAt(0).toUpperCase() + string.slice(1).toLowerCase();
    },

    compareArrays: function(a, b) {
        function objectsAreSame(x, y) {
            for (var propertyName in x) {
                if (x[propertyName] !== y[propertyName]) {
                    return false;
                }
            }
            return true;
        };
        if (typeof a == "undefined" && typeof b == "undefined") {
            return true;
        }
        if (typeof a == "undefined" || typeof b == "undefined") {
            return false;
        }
        if (a.length != b.length) {
            return false;
        }
        for (var i = 0; i < a.length; i++) {
            if (!objectsAreSame(a[i], b[i])) {
                return false;
            }
        }
        return true;
    },

    clearSiteContext: function() {
        if (typeof(sessionStorage) != "undefined") {
            NSN.groupID = "-1";
            sessionStorage.setItem("groupID", NSN.groupID);
            NSN.nodeID = "-1";
            sessionStorage.setItem("nodeID", NSN.nodeID);
            NSN.scheduleID = "-1";
            sessionStorage.setItem("scheduleID", NSN.scheduleID);
            NSN.fixtureID = "-1";
            sessionStorage.setItem("fixtureID", NSN.fixtureID);
            NSN.daylightID = "-1";
            sessionStorage.setItem("daylightID", NSN.daylightID);
            NSN.proximityID = "-1";
            sessionStorage.setItem("proximityID", NSN.proximityID);
        };
    },

    checkValidity: function(formcontents, rules) {
        var errors = {},
            req_ok = true,
            type_ok = true,
            inputs = Object.assign({},formcontents);

        for (var field in inputs) {
            if (rules[field]) {
                errors[field] = "";
                req_ok = true;
                type_ok = true;
                var hasMin = typeof rules[field].min != "undefined";
                var hasMax = typeof rules[field].max != "undefined";
                var fragment = "";
                if (hasMin && hasMax) {
                    fragment = " between " + rules[field].min + " and " + rules[field].max;
                } else {
                    if (hasMin) {
                        fragment = " >= " + rules[field].min;
                    };
                    if (hasMax) {
                        fragment = " <= " + rules[field].max;
                    };
                };
                if (rules[field].required && String(inputs[field]).trim().length == 0) {
                    req_ok = false;
                    errors[field] = field + " is required";
                };

                if (rules[field].greaterThan) {
                   var max = parseInt(inputs[field]);
                   var min = parseInt(inputs[rules[field].greaterThan]);
                   
                   if(max <= min){
                    req_ok = false; 
                    errors[field] = field + " should be greater than " + rules[field].greaterThan;
                   } 
                };

                if(rules[field].camera0streamL){
                    var camera0streamHval = parseFloat(inputs[field]);
                    if(camera0streamHval >1){
                        req_ok = false; 
                        errors[field] = field + "value should be less or equal to 1";
                    }
                    var camera0streamLval = parseFloat(inputs[rules[field].camera0streamL]);
                    if(camera0streamLval >1){
                        req_ok = false; 
                        errors[rules[field].camera0streamL] = rules[field].camera0streamL + "value should be less or equal to 1";
                    }
                    var sum = camera0streamHval + camera0streamLval;
                    if(sum > rules[field].total){
                    req_ok = false; 
                    errors[field] = field + " and " + rules[field].camera0streamL +" sum should be less than or equal to 1 " ; 
                    }
                };

                if(rules[field].falconVal){
                    var camera0streamHval = parseFloat(inputs[rules[field].fCamera0streamH]);
                    if(camera0streamHval >1){
                        req_ok = false; 
                        errors[rules[field].fCamera0streamH] = rules[field].fCamera0streamH + "value should be less or equal to 1";
                    }
                    var camera0streamLval = parseFloat(inputs[rules[field].fCamera0streamL]);
                    if(camera0streamLval >1){
                        req_ok = false; 
                        errors[rules[field].fCamera0streamL] = rules[field].fCamera0streamL + "value should be less or equal to 1";
                    }
                    var camera1streamHval = parseFloat(inputs[rules[field].fCamera1streamH]);
                    if(camera1streamHval >1){
                        req_ok = false; 
                        errors[rules[field].fCamera1streamH] = rules[field].fCamera1streamH + "value should be less or equal to 1";
                    }
                    var camera1streamLval = parseFloat(inputs[field]);
                    if(camera1streamLval >1){
                        req_ok = false; 
                        errors[field] = field + "value should be less or equal to 1";
                    }
                    var sum = camera0streamHval + camera0streamLval + camera1streamHval + camera1streamLval;
                    if(sum > rules[field].total){
                    req_ok = false; 
                    errors[field] = rules[field].fCamera0streamH + " , " + rules[field].fCamera0streamL +", "+rules[field].fCamera1streamH+" and "+field+" sum should be less than or equal to 1 " ; 
                    }
                };

                if (rules[field].regex && typeof inputs[field] === 'string' && !inputs[field].match(rules[field].regex)) {
                    type_ok = false;
                    errors[field] += ((errors[field] == "") ? this.ucfirst(field) : " and") + ' must match ' + rules[field].regex;

                }

                if (rules[field].digits && !$.isNumeric(inputs[field])) {
                    type_ok = false;
                    errors[field] += ((errors[field] == "") ? field : " and") + ' must be a number' + fragment;
                }

                if (req_ok && String(inputs[field]) != "" && rules[field]['type']) {
                    switch (rules[field]['type']) {
                        case 'integer':
                            if (!String(inputs[field]).match(/\-?[0-9]*/)
                                || String(inputs[field]) != String(parseInt(inputs[field]))) {
                                type_ok = false;
                                errors[field] += ((errors[field] == "") ? field : " and") + ' must be an integer' + fragment;
                            }
                            break;
                        case 'float':
                            if (!isFinite(String(inputs[field]))) {
                                type_ok = false;
                                errors[field] += ((errors[field] == "") ? field : " and") + ' must be a number' + fragment;
                            }
                            break;
                        case 'email':
                            break;
                    };
                };
                if (req_ok && type_ok && String(inputs[field]) != "") {
                    inputs[field] = parseFloat(inputs[field]);
                    if (hasMin && hasMax && (inputs[field] < rules[field].min) || (inputs[field] > rules[field].max)) {
                        errors[field] = field + " must be" + fragment;
                    } else {
                        if (hasMin && (inputs[field] < rules[field].min)) {
                            errors[field] = field + " must be" + fragment;
                        };
                        if (hasMax && (inputs[field] > rules[field].max)) {
                            errors[field] = field + " must be" + fragment;
                        };
                        if ((rules[field].type === "integer") && (!$.isNumeric(inputs[field]))) {
                            errors[field] = field + " must be an " + rules[field].type;
                        };
                    };
                };

                if (errors[field] == "") {
                    delete errors[field];
                } 
                else if('regex' in rules[field]){
                  errors[field] = errors[field];
                }
                else {
                    errors[field] = this.ucfirst(errors[field]);
                };
            };
        };
        return errors;
    },

    getColumnSettings: function(itemid, current) {
      // This will get a set of DataGrid column width settings from localStorage
      //  - if the stored settings are missing columns from the current default settings,
      //      then the new columns are added before the result is returned
      //  - it does not update the stored values, this will happen when the user
      //.     changes something
      if (localStorage.getItem(itemid)) {
        var stored = JSON.parse(localStorage.getItem(itemid));
        for (var i=0; i<current.length; i++) {
          for (var j=0, found=false; !found && j<stored.length; j++) {
            found = stored[j].id == current[i].id;
          };
          if (!found) {
            stored.push(current[i]);
          };
        };
        return stored;
      } else {
        return current;
      };
    },

    setNodeMarker: function(node) {
        if (this.modelType(node.model) != "Lighting") {
          var nofixture = 0;
        } else {
          nofixture = ((typeof node.fixtureid == "undefined")
                        || node.fixtureid == "")?1:0;
        };
        return {type: this.modelType(node.model),
                status:node.net_stat
                            ?(["good","warn","error","none"][
                             Math.min(2,
                              Math.max(node.net_stats,
                                       node.sen_stats,
                                       node.lig_stats - nofixture
                                       )
                                    )
                              ])
                            :"none",
                lightLevel:Math.round(node.sen_stat),
                selected: false }
    },

    setNodeStatus: function(node) {
      if(!node.net_stat) {
        node.net_stats = 3;
        node.sen_stats = 3;
        node.lig_stats = 3;
      } else {
        node.net_stats = 0;
        node.sen_stats = node.sen_stat?0:2;
        node.lig_stats = 0;
      }
      if(node.alerts && node.net_stat) {
        for(var i=0;i<node.alerts.length;i++) {
          if (node.alerts[i].severity != "Clear") {
            var penalty = 0;
            node.alerts[i].category = helpers.getAlarmCategory(node.alerts[i].type);
            if (node.alerts[i].severity =='Critical') penalty = 2;
            if (node.alerts[i].severity == 'Major') penalty = 1;
            if (this.modelType(node.model) == "Kiosk" && node.alerts[i].severity == "Minor") penalty = 1;

            if(node.alerts[i].category == 'Connection') {
              node.net_stats = Math.min(2, node.net_stats + penalty);
            };
            if(node.alerts[i].category == 'Light') {
              node.lig_stats = Math.min(2, node.lig_stats + penalty);
            };
            if(node.alerts[i].category == 'Sensor') {
              node.sen_stats = Math.min(2, node.sen_stats + penalty);
            };
          };
        };
      }
      var nofixture = this.modelType(node.model) == "Lighting" 
                      && ((typeof node.fixtureid == "undefined") || node.fixtureid == "");
      if (node.net_stat && nofixture) {
        node.lig_stats = 1;
      }

      return node;
    },

    argtokey: function(arg) {
      return arg.type + "-"
            + arg.status + "-"
            + ($.isNumeric(arg.lightLevel)?arg.lightLevel:"X") + "-"
            + (arg.selected?"T":"F");
    },

    keytoarg: function(key) {
      var keyarray = key.split("-");
      return {type:keyarray[0],
            status:keyarray[1],
        lightLevel:(keyarray[2]=="X"?null:keyarray[2]),
          selected:keyarray[3]=="T"
        };
    },

    markerCache: {},

    genMarker: function(arg) {
      var key = this.argtokey(arg);
      if (typeof this.markerCache[key] != "undefined") {
        return this.markerCache[key];
      };
      // not cached; generate new marker
      if (arg.type == "Kiosk") {
        var svgHTML = '<svg id="svg" width="20" height="88" viewPort="0 0 20 88" version="1.1" xmlns="http://www.w3.org/2000/svg">';
        svgHTML += '<rect x="0" y="0" width="20" height="88" rx="4" ry="4" fill="#A4B1A4" stroke-width="1" stroke="#FFF"></rect>'
         + '<polygon points="20,8 0,22 0,72 20,58" fill="#F6F6F6"></polygon>'
         + '<polygon points="18,13 3,23 3,34 18,42" fill="#333"></polygon>'
         + '<polygon points="2,37 17,46 17,57 2,67" fill="#333"></polygon>'
         + '<polygon points="8,21 16,23 16,37 8,35" fill="#FFF"></polygon>'
         + '<polygon points="4,45 12,44 12,59 4,58" fill="#FFF"></polygon>'
         + '<rect x="0" y="0" width="20" height="6" rx="4" ry="4" fill="'
         + (arg.status=="good"?"#00AC3E":(arg.status=="warn"?"#FFBC3D":(arg.status=="error"?"#D52B1E":"#747676"))) 
         + '"></rect>';
        svgHTML += '</svg>';
        var svgIcon = {
          url: 'data:image/svg+xml;charset=UTF-8;base64,' + btoa(svgHTML),
          size: new google.maps.Size(20, 88),
          origin: new google.maps.Point(0,0), // origin
          anchor: new google.maps.Point(10, 68), // anchor
          key: key
        };
      this.markerCache[key] = JSON.parse(JSON.stringify(svgIcon));
      return svgIcon;
      }
      svgHTML = '<svg id="svg" width="40" height="40" viewPort="0 0 40 40" version="1.1" xmlns="http://www.w3.org/2000/svg">';
      if (arg.type == "Video") {
        if (arg.selected) {
          svgHTML +='<circle r="18" cx="20" cy="20" fill="#FFF" stroke-width="4" stroke="' 
            + (arg.status=="good"?"#00AC3E":(arg.status=="warn"?"#FFBC3D":(arg.status=="error"?"#D52B1E":"#747676"))) + '">'
            + '</circle>';
        };
        svgHTML += '<circle r="17" cx="20" cy="20" fill="#666" stroke-width="5" stroke="' 
         + (arg.status=="good"?"#00AC3E":(arg.status=="warn"?"#FFBC3D":(arg.status=="error"?"#D52B1E":"#747676"))) + '"></circle>'
         + '<rect x="8" y="14" width="14" height="12" rx="3" ry="3" fill="#FFF"></rect>'
         + '<polygon points="20,20 30,14 30,26" fill="#FFF" stroke-width="2" stroke="#FFF" stroke-linejoin="round"></polygon>';
      } else {
        var num = $.isNumeric(arg.lightLevel);
        if (num) {
          arg.lightLevel = parseInt(arg.lightLevel);
        };
        switch (arg.status) {
          case "none":
          case "good":
            if (arg.selected) {
              svgHTML +='<circle r="18" cx="20" cy="20" fill="#FFF" stroke-width="4" stroke="' + (arg.status=="good"?"#00AC3E":"#999") + '">'
                      + '</circle>';
            };
            svgHTML += ('<circle r="15" cx="20" cy="20" fill="#FFF" stroke-width="4" stroke="' + (arg.status=="good"?"#00AC3E":"#BBB") + '"></circle>'
             + '<circle r="13" cx="20" cy="20" fill="#000" stroke="none" stroke-width="0" '
             + 'opacity="' +  (num?0.7*(1-(arg.lightLevel/100)):0.7) + '">'
             + '</circle>'
             + '<text class="txt" x="50%" y="50%" text-anchor="middle" dominant-baseline="central" font-weight="bold" font-family="sans-serif" font-size="12" stroke="none" '
             + 'fill="' + ((num&&(arg.lightLevel > 50))?"#747676":"#FFF") + '">'+(num?arg.lightLevel:'?')+'</text>');
             break;
          case "warn":
            if (arg.selected) {
              svgHTML += '<polygon points="20,2 38,38 2,38" fill="#FFF" stroke-width="4" stroke="#FFBC3D" stroke-linejoin="round"></polygon>';
            };
            svgHTML += ('<polygon points="20,5 35,35 5,35" fill="#FFF" stroke-width="4" stroke="#FFBC3D" stroke-linejoin="round"></polygon>'
             + '<polygon points="20,10 32.5,33 8,33" fill="#000" stroke="none" stroke-width="0" '
             + 'opacity="' +  0.7*(1-(arg.lightLevel/100)) + '"></polygon>'
             + '<text class="txt" x="50%" y="68%" text-anchor="middle" dominant-baseline="central" font-weight="bold" font-family="sans-serif" font-size="12" stroke="none" '
             + 'fill="' + ((arg.lightLevel > 50)?"#747676":"#FFF") + '">'+($.isNumeric(arg.lightLevel)?arg.lightLevel:'?')+'</text>');
             break;
          case "error":
            if (arg.selected) {
              svgHTML += '<rect x="4" y="4" width="32" height="32" rx="6" ry="6" fill="#FFF" stroke-width="4" stroke="#D52B1E"></rect>';
            }
            svgHTML += ('<rect x="7" y="7" width="26" height="26" rx="3" ry="3"  fill="#FFF" stroke-width="4" stroke="#D52B1E"></rect>'
             + '<rect x="9" y="9" width="22" height="22" rx="2" ry="2" fill="#000" stroke="none" stroke-width="0" '
             + 'opacity="' +  0.7*(1-(arg.lightLevel/100)) + '"></rect>'
             + '<text class="txt" x="50%" y="50%" text-anchor="middle" dominant-baseline="central" font-weight="bold" font-family="sans-serif" font-size="12" stroke="none" '
             + 'fill="' + ((arg.lightLevel > 50)?"#747676":"#FFF") + '">'+($.isNumeric(arg.lightLevel)?arg.lightLevel:'?')+'</text>');
           };
       };
       svgHTML += '</svg>';
       var svgIcon = {
          url: 'data:image/svg+xml;charset=UTF-8;base64,' + btoa(svgHTML),
          size: new google.maps.Size(40, 40),
          origin: new google.maps.Point(0,0), // origin
          anchor: new google.maps.Point(20, 20), // anchor
          key: key
        };
      this.markerCache[key] = JSON.parse(JSON.stringify(svgIcon));
      return svgIcon;
    },

    genGMarker: function(nodetype, connected, member) {
      var memberstatus = member?'':' opacity="0.4"';
      var connectionstatus = connected?' stroke="#0B0"':' stroke="#777"';
      if (nodetype == "Kiosk") {
        var svgHTML = '<svg id="svg" width="20" height="88" viewPort="0 0 20 88" version="1.1" xmlns="http://www.w3.org/2000/svg">';
        svgHTML += '<g' + memberstatus + '><rect x="0" y="0" width="20" height="88" rx="4" ry="4" fill="#A4B1A4" stroke-width="1" stroke="#FFF"></rect>'
         + '<polygon points="20,8 0,22 0,72 20,58" fill="#EEE"></polygon>'
         + '<polygon points="18,13 3,23 3,34 18,42" fill="#333"></polygon>'
         + '<polygon points="2,37 17,46 17,57 2,67" fill="#333"></polygon>'
         + '<polygon points="8,21 16,23 16,37 8,35" fill="#FFF"></polygon>'
         + '<polygon points="4,45 12,44 12,59 4,58" fill="#FFF"></polygon>'
         + '<rect x="0" y="0" width="20" height="6" rx="4" ry="4" fill="'
         + (connected?"#0B0":"#777")
         + '"></rect></g>';
        svgHTML += '</svg>';
        var svgIcon = {
          url: 'data:image/svg+xml;charset=UTF-8;base64,' + btoa(svgHTML),
          size: new google.maps.Size(20, 88),
          origin: new google.maps.Point(0,0), // origin
          anchor: new google.maps.Point(10, 68) // anchor
        };
        return svgIcon;
      };
      svgHTML = '<svg id="svg" width="40" height="40" viewPort="0 0 40 40" version="1.1" xmlns="http://www.w3.org/2000/svg">';
      var nodeicon = nodetype=="Lighting"?'':('<rect x="8" y="14" width="14" height="12" rx="3" ry="3" fill="#008ABF"></rect>'
           + '<polygon points="20,20 30,14 30,26" fill="#008ABF" stroke-width="2" stroke="#008ABF" stroke-linejoin="round"></polygon>');

      svgHTML += '<g' + memberstatus + '><circle r="15" cx="20" cy="20" fill="#FFF" stroke-width="4" ' + connectionstatus + '></circle>'
           + '<circle r="10" cx="20" cy="20" fill="' + (nodetype=="Lighting"?"#008ABF":"#FFF") + '" fstroke="none" stroke-width="0"></circle>'
           + nodeicon;
      svgHTML += '</g></svg>';       
      return {
        url: 'data:image/svg+xml;charset=UTF-8;base64,' + btoa(svgHTML),
        size: new google.maps.Size(40, 40),
        origin: new google.maps.Point(0, 0), 
        anchor: new google.maps.Point(20, 20)
      };
    },

    spaceMarkerCache: {},
    genSpaceMarker:function(selected, key, type){ // key is basically the parkingspot id
        var spacekey = key;
        var memberstatus = selected?' opacity="10"':' opacity="0.4"';

        if (typeof this.spaceMarkerCache[spacekey] != "undefined") {
            var currentSvg = this.spaceMarkerCache[spacekey];
            var svgHtml;
                if(selected == true){
                    svgHtml = '<svg id="svg" width="10" height="40" viewPort="0 0 10 10" version="1.1" xmlns="http://www.w3.org/2000/svg">'+
                        '<g'+ memberstatus +'>'+
                        '<circle cx="5" cy="5" r="4"  stroke="#D52B1E" stroke-width="3" fill="#D52B1E" ></circle>' +
                        ' </g>'+
                        '</svg>';

                    currentSvg.url = 'data:image/svg+xml;charset=UTF-8;base64,' + btoa(svgHtml);
                    this.spaceMarkerCache[spacekey] = currentSvg;
                    return this.spaceMarkerCache[spacekey];

                }else{
                    svgHtml = '<svg id="svg" width="10" height="40" viewPort="0 0 10 10" version="1.1" xmlns="http://www.w3.org/2000/svg">'+
                            '<g'+ memberstatus +'>'+
                            '<circle cx="5" cy="5" r="4"  stroke="#0088CE" stroke-width="3" fill="#0088CE" ></circle>' +
                            ' </g>'+
                            '</svg>';
                    currentSvg.url = 'data:image/svg+xml;charset=UTF-8;base64,' + btoa(svgHtml);
                    this.spaceMarkerCache[spacekey] = currentSvg;
                    return this.spaceMarkerCache[spacekey];
                }
        }else{
            if(selected == true){
                var svgHtml = '<svg id="svg" width="10" height="40" viewPort="0 0 10 10" version="1.1" xmlns="http://www.w3.org/2000/svg">'+
                    '<g'+ memberstatus +'>'+
                    '<circle cx="5" cy="5" r="4"  stroke="#D52B1E" stroke-width="3" fill="#D52B1E" ></circle>' +
                    ' </g>'+
                    '</svg>';
            }else{
                svgHtml = '<svg id="svg" width="10" height="40" viewPort="0 0 10 10" version="1.1" xmlns="http://www.w3.org/2000/svg">'+
                    '<g'+ memberstatus +'>'+
                    '<circle cx="5" cy="5" r="4"  stroke="#0088CE" stroke-width="3" fill="#0088CE" ></circle>' +
                    ' </g>'+
                    '</svg>';
            }
            var spaceSvg = {
                url: 'data:image/svg+xml;charset=UTF-8;base64,' + btoa(svgHtml),
                size: new google.maps.Size(10, 10),
                origin: new google.maps.Point(0, 0),
                anchor: new google.maps.Point(5, 5),
                labelOrigin: new google.maps.Point(9, 9),
                spacekey:spacekey
            };

            this.spaceMarkerCache[spacekey] = JSON.parse(JSON.stringify(spaceSvg));
            return spaceSvg;
        }

    },

    genNameMarker:function(){
        var svg = '<svg id="svg-name">'+
                    '<g></g>'+
                    '</svg>';

        return {
            url: 'data:image/svg+xml;charset=UTF-8;base64,' + btoa(svg),
            size: new google.maps.Size(10, 10),
            origin: new google.maps.Point(0, 0),
            anchor: new google.maps.Point(5, 5),
            labelOrigin: new google.maps.Point(9, 9)
        };

    },


    genNumberSequence:function(spaceNamePattern, spaceNameCount, numbers, singleNumber){
        if(singleNumber == "none"){
            for(var count=1, end = numbers.length; count < end; count++){
                spaceNamePattern.push(numbers.charAt(count))
            }
            for(var j =1 ; j< spaceNameCount; j++){
                spaceNamePattern.push((Number(spaceNamePattern[spaceNamePattern.length-1]) + 1).toString());
            }
        }else {
            if(Number(singleNumber.charAt(0)) === 0){
                if(singleNumber.length === 1){
	                spaceNamePattern.push(singleNumber);
	                for(var j =0 ; j< spaceNameCount; j++){
		                spaceNamePattern.push((Number(spaceNamePattern[spaceNamePattern.length-1]) + 1).toString());
	                }
                }else{
	                var secondChar = Number(singleNumber.charAt(1)); //
	                var changeUntil = 9-secondChar;
	                var changeAfter = changeUntil+1
                    for(var j = 0 ; j<= changeUntil; j++){
	                    if(j==0 ){
	                        var item = "0"+secondChar;
	                        spaceNamePattern.push(item)
                        }else{
	                        var next = "0"+ (Number(spaceNamePattern[spaceNamePattern.length-1]) + 1);
	                        spaceNamePattern.push(next)
                        }
                    }
	                for(var k = changeAfter ; k<=spaceNameCount; k++){
		                 // find the last element in the list and add 1 and push the item
                         var lastItem = Number(spaceNamePattern[spaceNamePattern.length - 1]);
                         if( lastItem < 9 ){
                             lastItem =  lastItem+1
                             lastItem = ("0"+lastItem).toString();
	                         spaceNamePattern.push(lastItem.toString());
                         }else{
	                         var nextElement = lastItem+1;
	                         nextElement =nextElement.toString();
	                         spaceNamePattern.push(nextElement);
                         }
	                }
                }
            }else{
	            spaceNamePattern.push(singleNumber);
	            for(var j =1 ; j< spaceNameCount; j++){
		            spaceNamePattern.push((Number(spaceNamePattern[spaceNamePattern.length-1]) + 1).toString());
	            }
            }
        }
        return spaceNamePattern;
    },

    genSpaceNameSequence:function(spaceNamePattern, spaceNameCount){
        var alphabets ="ABCDEFGHIJKLMNOPRSTUVWXYZ";
        for(var count=0, end = alphabets.length; count < end; count++){
            var nextChar = alphabets.charAt(count);
            spaceNamePattern.push(nextChar)
        }
        var newItem = "";
        // j is basically how many space names we need to create
        for(var j =0 ; j< spaceNameCount; j++){
            var lastItem = spaceNamePattern[spaceNamePattern.length-1];
            newItem = this.getNextKey(lastItem);
            spaceNamePattern.push(newItem);
        }
        return spaceNamePattern;

    },

    getNextKey : function(key) {
        if (key === 'Z' || key === 'z') {
            return String.fromCharCode(key.charCodeAt() - 25) + String.fromCharCode(key.charCodeAt() - 25); // AA or aa
        } else {
            var lastChar = key.slice(-1);
            var sub = key.slice(0, -1);
            if (lastChar === 'Z' || lastChar === 'z') {
                // If a string of length > 1 ends in Z/z,
                // increment the string (excluding the last Z/z) recursively,
                // and append A/a (depending on casing) to it
                return this.getNextKey(sub) + String.fromCharCode(lastChar.charCodeAt() - 25);
            } else {
                // (take till last char) append with (increment last char)
                return sub + String.fromCharCode(lastChar.charCodeAt() + 1);
            }
        }
        return key;
    },

    modelName: function(model){
        var modelNames = {
                        "unode-v2":"Core Node",
                        "unode-v3":"Internal Core Node",
                        "unode-v4":"Core Node EX Wifi",
                        "unode-v5":"Core Node EX Cellular",
                        "unode-v6":"Core Node EX LTE",
                        "falcon-q":"Video Node",
                        "unode-v7":"Video Node 4k",
                        "unode-v9":"Verizon Digital Kiosk",
                        "merlin":"Video Node 4k",
                        "genericx86-64":"Verizon Digital Kiosk",
                        "vdk":"Verizon Digital Kiosk",
                        "vdkmaster":"Verizon Digital Kiosk",
                        "ngcn":"Smart City Hub",
                        "cnext":"Smart City Hub"};
        return (typeof modelNames[model] == "undefined")?model:modelNames[model];
    },

    modelInternalName: function(model){
        var modelInternalNames = {
                        "Core Node":"unode-v2",
                        "Internal Core Node":"unode-v3",
                        "Core Node EX Wifi":"unode-v4",
                        "Core Node EX Cellular":"unode-v5",
                        "Core Node EX LTE":"unode-v6",
                        "Video Node":"falcon-q",
                        "Video Node 4k":"merlin",
                        "Verizon Digital Kiosk":"vdkmaster",
                        "Smart City Hub":"cnext"};
        return (typeof modelInternalNames[model] == "undefined")?model:modelInternalNames[model];
    },

    modelType: function(model) {
        var modelTypes = {
                        "unode-v2":"Lighting",
                        "unode-v3":"Lighting",
                        "unode-v4":"Lighting",
                        "unode-v5":"Lighting",
                        "unode-v6":"Lighting",
                        "unode-v7":"Video",
                        "unode-v9":"Kiosk",
                        "merlin":"Video",
                        "vdk":"Kiosk",
                        "vdkmaster":"Kiosk",
                        "genericx86-64":"Kiosk",
                        "ngcn":"Lighting",
                        "cnext":"Lighting",
                        "Smart City Hub":"Lighting",
                        "Core Node":"Lighting",
                        "Internal Core Node":"Lighting",
                        "Core Node EX Wifi":"Lighting",
                        "Core Node EX Cellular":"Lighting",
                        "Core Node EX LTE":"Lighting",
                        "Video Node":"Video",
                        "Video Node 4k":"Video",
                        "Verizon Digital Kiosk":"Kiosk",
                        "falcon-q":"Video"};
        return (typeof modelTypes[model] == "undefined")?"Unknown":modelTypes[model];
    },

    parkingColor: function(pct) {
      // converts an occupancy percentage into a color (0% - green; 50% - yellow; 100% - red)
      function toHex(c) {
        var hex = (Math.round(c)).toString(16);
        return hex.length == 1 ? "0" + hex : hex;
      };
      switch (Math.floor(Math.round(pct)/25)) {
        case 0:
          return "#"+toHex(51+(pct*0.04)*(221-51))+toHex(187+(pct*0.04)*(238-187))+toHex(51-(pct*0.04)*51);
        case 1:
          return "#"+toHex(221+((pct-25)*0.04)*(255-221))+"ee00";
        case 2:
          return "#ff"+toHex(238-((pct-50)*0.04)*(238-136))+"00";
        case 3:
          return "#ff"+toHex(136-((pct-75)*0.04)*136)+"00";
        case 4:
          return "#ff0000";
      };
    },

    getNightMap: function() {
      return ([
      { elementType: 'geometry', stylers: [{ color: '#242f3e' }] },
      { elementType: 'labels.text.stroke', stylers: [{ color: '#242f3e' }] },
      { elementType: 'labels.text.fill', stylers: [{ color: '#746855' }] },
      {
        featureType: 'administrative.locality',
        elementType: 'labels.text.fill',
        stylers: [{ color: '#d59563' }]
      },
      {
        featureType: "poi",
        elementType: "labels",
        stylers: [{ visibility: "off" }]
      },
      {
        featureType: 'poi.park',
        elementType: 'geometry',
        stylers: [{ color: '#263c3f' }]
      },
      {
        featureType: 'landscape.man_made',
        elementType: 'geometry',
        stylers: [{ color: '#555555' }]
      },
      {
        featureType: 'poi.park',
        elementType: 'labels.text.fill',
        stylers: [{ color: '#6b9a76' }]
      },
      {
        featureType: 'road',
        elementType: 'geometry',
        stylers: [{ color: '#666666' }]
      },
      {
        featureType: 'road',
        elementType: 'geometry.stroke',
        stylers: [{ color: '#555555' }]
      },
      {
        featureType: 'road',
        elementType: 'labels.text.fill',
        stylers: [{ color: '#BBBBBB' }]
      },
      {
        featureType: 'road.highway',
        elementType: 'geometry',
        stylers: [{ color: '#746855' }]
      },
      {
        featureType: 'road.highway',
        elementType: 'geometry.stroke',
        stylers: [{ color: '#1f2835' }]
      },
      {
        featureType: 'road.highway',
        elementType: 'labels.text.fill',
        stylers: [{ color: '#f3d19c' }]
      },
      {
        featureType: 'transit',
        elementType: 'geometry',
        stylers: [{ color: '#2f3948' }]
      },
      {
        featureType: 'transit.station',
        elementType: 'labels.text.fill',
        stylers: [{ color: '#d59563' }]
      },
      {
        featureType: 'water',
        elementType: 'geometry',
        stylers: [{ color: '#17263c' }]
      },
      {
        featureType: 'water',
        elementType: 'labels.text.fill',
        stylers: [{ color: '#515c6d' }]
      },
      {
        featureType: 'water',
        elementType: 'labels.text.stroke',
        stylers: [{ color: '#17263c' }]
      }
    ]);
  },

  isInternalUser: function() {
    return NSN && NSN.userInfo && NSN.userInfo.authorization
           && NSN.userInfo.authorization.length > 0
           && NSN.userInfo.authorization[0].type.indexOf("sensity") == 0;
  },

    genFakeSiteNodeList: function(n) {
        var blockHeight = 0.0008, blockWidth = 0.0018, vGutter = .00024, hGutter = .00030;
        var nodesPerHeight = 4, nodesPerWidth = 7;
        var originLat = 37, originLng = -121;
        var nodesPerBlock = 2*nodesPerWidth + 2*(nodesPerHeight - 1);
        var numBlocks = Math.floor(n / nodesPerBlock);
        var gridWidth = Math.ceil(Math.sqrt(numBlocks));

        function genModel(){
            var val = Math.floor(Math.random()*100);
            if (val<10) return "unode-v5";
            if (val<20) return "unode-v4";
            if (val<50) return "unode-v3";
            return "unode-v2";
        }

        function genBlock(lat, lng) {
            var list = [], 
                vSpace = blockHeight / (nodesPerHeight - 0.5),
                hSpace = blockWidth / (nodesPerWidth - 0.5);
            // top
            for (var i=0; i<nodesPerWidth; i++) {
                list.push({nodeid:"N"+Math.floor(10000000*Math.random()),model:genModel(),
                    level:"1",latitude:lat,longitude:(lng + (i*hSpace))});
            }
            // bottom
            for (var i=0; i<nodesPerWidth; i++) {
                list.push({nodeid:"N"+Math.floor(10000000*Math.random()),model:genModel(),
                    level:"1",latitude:lat-blockHeight,longitude:((lng+blockWidth) - (i*hSpace))});
            }            
            // left
            for (var i=0; i<(nodesPerHeight-1); i++) {
                list.push({nodeid:"N"+Math.floor(10000000*Math.random()),model:genModel(),
                    level:"1",latitude:lat-(i+1)*vSpace,longitude:lng});
            }                        
            // right
            for (var i=0; i<(nodesPerHeight-1); i++) {
                list.push({nodeid:"N"+Math.floor(10000000*Math.random()),model:genModel(),
                    level:"1",latitude:(lat-blockHeight)+(i+1)*vSpace,longitude:lng+blockWidth});
            }                        
            return list;
        }

        function shuffle (array) {
          var i = 0
            , j = 0
            , temp = null

          for (i = array.length - 1; i > 0; i -= 1) {
            j = Math.floor(Math.random() * (i + 1))
            temp = array[i]
            array[i] = array[j]
            array[j] = temp
          }
        }

        var i = 0, nodeList = [], lat = originLat, lng = originLng;
        while (i < numBlocks) {
            nodeList = nodeList.concat(genBlock(lat, lng));
            lng += (blockWidth + hGutter);
            if ((++i%gridWidth) == 0) {
                lng = originLng;
                lat = lat - (blockHeight + vGutter);
            }
        };
        return nodeList;
    },


    getFakeOtaList: function(){
        console.log("in helpers.getfakeotalist");
        return[{
            "target_type": "group",
            "firmwareid": "6fd8186",
            "target_id": "e2d53100-ff83-11e6-95c1-cd7801a1f13c",
            "when": "2017-03-17T15:28:11.988-07:00",
            "siteid": "1b174f90-ff83-11e6-95c1-cd7801a1f13c",
            "jobid": "0fc93ca3-40b6-4a90-9e8a-bc1aa38e8297",
            "description": "Command B",
            "target_name": "V4 Test",
            "orgid": "efe5bdb3-baac-5d8e-6cae57771c13"
          },
          {
            "target_type": "group",
            "firmwareid": "42352cc",
            "target_id": "e2d53100-ff83-11e6-95c1-cd7801a1f13c",
            "when": "2017-03-30T12:18:00.754-07:00",
            "siteid": "1b174f90-ff83-11e6-95c1-cd7801a1f13c",
            "jobid": "abb0acb3-184e-4280-9023-f44d7bb5bf40",
            "description": "Test for sara",
            "target_name": "V4 Test",
            "orgid": "efe5bdb3-baac-5d8e-6cae57771c13"
          },
          {
            "target_type": "group",
            "firmwareid": "6fd8186",
            "target_id": "e2d53100-ff83-11e6-95c1-cd7801a1f13c",
            "when": "2017-03-30T12:16:57.305-07:00",
            "siteid": "1b174f90-ff83-11e6-95c1-cd7801a1f13c",
            "jobid": "c92b3450-39bc-46cf-86ef-bfac7773e311",
            "description": "Test for sara",
            "target_name": "V4 Test",
            "orgid": "efe5bdb3-baac-5d8e-6cae57771c13"
          },
          {
            "target_type": "group",
            "firmwareid": "42352cc",
            "target_id": "e2d53100-ff83-11e6-95c1-cd7801a1f13c",
            "when": "2017-03-30T12:21:09.018-07:00",
            "siteid": "1b174f90-ff83-11e6-95c1-cd7801a1f13c",
            "jobid": "d5301534-dae1-42dc-987e-a6b2c162b373",
            "description": "Test 2 for sara",
            "target_name": "V4 Test",
            "orgid": "efe5bdb3-baac-5d8e-6cae57771c13"
          },
          {
            "target_type": "group",
            "firmwareid": "6fd8186",
            "target_id": "e2d53100-ff83-11e6-95c1-cd7801a1f13c",
            "when": "2017-03-17T15:28:12.280-07:00",
            "siteid": "1b174f90-ff83-11e6-95c1-cd7801a1f13c",
            "jobid": "dac8a870-b1e8-4895-aedc-2b13b30d0937",
            "description": "Command B",
            "target_name": "V4 Test",
            "orgid": "efe5bdb3-baac-5d8e-6cae57771c13"
        }]
    },

    getFakeOtaListData: function(){
        return [
        {
            "success": true,
            "target_type": "group",
            "firmwareid": "6fd8186",
            "nodeid": null,
            "target_id": "e2d53100-ff83-11e6-95c1-cd7801a1f13c",
            "status": "JOB_SENT",
            "when": "2017-03-30T12:16:57.305-07:00",
            "siteid": "1b174f90-ff83-11e6-95c1-cd7801a1f13c",
            "jobid": "c92b3450-39bc-46cf-86ef-bfac7773e311",
            "description": "Test for sara",
            "target_name": "V4 Test",
            "orgid": "efe5bdb3-baac-5d8e-6cae57771c13"
        },
        {
            "success": true,
            "target_type": "group",
            "firmwareid": "6fd8186",
            "nodeid": null,
            "target_id": "e2d53100-ff83-11e6-95c1-cd7801a1f13c",
            "status": "JOB_RECEIVED",
            "when": "2017-03-30T12:16:57.447-07:00",
            "siteid": "1b174f90-ff83-11e6-95c1-cd7801a1f13c",
            "jobid": "c92b3450-39bc-46cf-86ef-bfac7773e311",
            "description": null,
            "target_name": "V4 Test",
            "orgid": "efe5bdb3-baac-5d8e-6cae57771c13"
        },
        {
            "success": false,
            "target_type": "group",
            "firmwareid": "6fd8186",
            "nodeid": null,
            "target_id": "e2d53100-ff83-11e6-95c1-cd7801a1f13c",
            "status": "FIRMWARE_NOT_FOUND",
            "when": "2017-03-30T12:16:57.996-07:00",
            "siteid": "1b174f90-ff83-11e6-95c1-cd7801a1f13c",
            "jobid": "c92b3450-39bc-46cf-86ef-bfac7773e311",
            "description": null,
            "target_name": "V4 Test",
            "orgid": "efe5bdb3-baac-5d8e-6cae57771c13"
        },
        {
        "success": true,
        "target_type": "group",
        "firmwareid": "42352cc",
        "nodeid": null,
        "target_id": "e2d53100-ff83-11e6-95c1-cd7801a1f13c",
        "status": "JOB_SENT",
        "when": "2017-03-30T12:21:09.018-07:00",
        "siteid": "1b174f90-ff83-11e6-95c1-cd7801a1f13c",
        "jobid": "d5301534-dae1-42dc-987e-a6b2c162b373",
        "description": "Test 2 for sara",
        "target_name": "V4 Test",
        "orgid": "efe5bdb3-baac-5d8e-6cae57771c13"
      },
      {
        "success": false,
        "target_type": "group",
        "firmwareid": "42352cc",
        "nodeid": "JS_2",
        "target_id": "e2d53100-ff83-11e6-95c1-cd7801a1f13c",
        "status": "NODE_OFFLINE",
        "when": "2017-03-30T12:21:09.181-07:00",
        "siteid": "1b174f90-ff83-11e6-95c1-cd7801a1f13c",
        "jobid": "d5301534-dae1-42dc-987e-a6b2c162b373",
        "description": null,
        "target_name": "V4 Test",
        "orgid": "efe5bdb3-baac-5d8e-6cae57771c13"
      },
      {
        "success": true,
        "target_type": "group",
        "firmwareid": "42352cc",
        "nodeid": null,
        "target_id": "e2d53100-ff83-11e6-95c1-cd7801a1f13c",
        "status": "JOB_RECEIVED",
        "when": "2017-03-30T12:21:09.203-07:00",
        "siteid": "1b174f90-ff83-11e6-95c1-cd7801a1f13c",
        "jobid": "d5301534-dae1-42dc-987e-a6b2c162b373",
        "description": null,
        "target_name": "V4 Test",
        "orgid": "efe5bdb3-baac-5d8e-6cae57771c13"
      },
      {
        "success": true,
        "target_type": "group",
        "firmwareid": "42352cc",
        "nodeid": "N01232ea5",
        "target_id": "e2d53100-ff83-11e6-95c1-cd7801a1f13c",
        "status": "NODE_REBOOTING",
        "when": "2017-03-30T12:21:09.209-07:00",
        "siteid": "1b174f90-ff83-11e6-95c1-cd7801a1f13c",
        "jobid": "d5301534-dae1-42dc-987e-a6b2c162b373",
        "description": null,
        "target_name": "V4 Test",
        "orgid": "efe5bdb3-baac-5d8e-6cae57771c13"
      },
      {
        "success": true,
        "target_type": "group",
        "firmwareid": "42352cc",
        "nodeid": "N01232ea5",
        "target_id": "e2d53100-ff83-11e6-95c1-cd7801a1f13c",
        "status": "COMMAND_SENT",
        "when": "2017-03-30T12:21:09.222-07:00",
        "siteid": "1b174f90-ff83-11e6-95c1-cd7801a1f13c",
        "jobid": "d5301534-dae1-42dc-987e-a6b2c162b373",
        "description": null,
        "target_name": "V4 Test",
        "orgid": "efe5bdb3-baac-5d8e-6cae57771c13"
      },
      {
        "success": false,
        "target_type": "group",
        "firmwareid": "42352cc",
        "nodeid": "JS_3",
        "target_id": "e2d53100-ff83-11e6-95c1-cd7801a1f13c",
        "status": "NODE_OFFLINE",
        "when": "2017-03-30T12:21:09.241-07:00",
        "siteid": "1b174f90-ff83-11e6-95c1-cd7801a1f13c",
        "jobid": "d5301534-dae1-42dc-987e-a6b2c162b373",
        "description": null,
        "target_name": "V4 Test",
        "orgid": "efe5bdb3-baac-5d8e-6cae57771c13"
      },
      {
        "success": true,
        "target_type": "group",
        "firmwareid": "42352cc",
        "nodeid": "N01232ea5",
        "target_id": "e2d53100-ff83-11e6-95c1-cd7801a1f13c",
        "status": "START_DOWNLOAD",
        "when": "2017-03-30T12:21:11.116-07:00",
        "siteid": "1b174f90-ff83-11e6-95c1-cd7801a1f13c",
        "jobid": "d5301534-dae1-42dc-987e-a6b2c162b373",
        "description": null,
        "target_name": "V4 Test",
        "orgid": "efe5bdb3-baac-5d8e-6cae57771c13"
      },
      {
        "success": true,
        "target_type": "group",
        "firmwareid": "42352cc",
        "nodeid": "N01232ea5",
        "target_id": "e2d53100-ff83-11e6-95c1-cd7801a1f13c",
        "status": "STOP_DOWNLOAD",
        "when": "2017-03-30T12:21:11.121-07:00",
        "siteid": "1b174f90-ff83-11e6-95c1-cd7801a1f13c",
        "jobid": "d5301534-dae1-42dc-987e-a6b2c162b373",
        "description": null,
        "target_name": "V4 Test",
        "orgid": "efe5bdb3-baac-5d8e-6cae57771c13"
      },
      {
        "success": true,
        "target_type": "group",
        "firmwareid": "42352cc",
        "nodeid": "N01232ea5",
        "target_id": "e2d53100-ff83-11e6-95c1-cd7801a1f13c",
        "status": "START_DOWNLOAD",
        "when": "2017-03-30T12:21:53.476-07:00",
        "siteid": "1b174f90-ff83-11e6-95c1-cd7801a1f13c",
        "jobid": "d5301534-dae1-42dc-987e-a6b2c162b373",
        "description": null,
        "target_name": "V4 Test",
        "orgid": "efe5bdb3-baac-5d8e-6cae57771c13"
      },
      {
        "success": true,
        "target_type": "group",
        "firmwareid": "42352cc",
        "nodeid": "N01232ea5",
        "target_id": "e2d53100-ff83-11e6-95c1-cd7801a1f13c",
        "status": "STOP_DOWNLOAD",
        "when": "2017-03-30T12:21:53.480-07:00",
        "siteid": "1b174f90-ff83-11e6-95c1-cd7801a1f13c",
        "jobid": "d5301534-dae1-42dc-987e-a6b2c162b373",
        "description": null,
        "target_name": "V4 Test",
        "orgid": "efe5bdb3-baac-5d8e-6cae57771c13"
      },
      {
        "success": true,
        "target_type": "group",
        "firmwareid": "42352cc",
        "nodeid": "N01232ea5",
        "target_id": "e2d53100-ff83-11e6-95c1-cd7801a1f13c",
        "status": "UPDATE_SUCCESSFUL",
        "when": "2017-03-30T12:23:20.246-07:00",
        "siteid": "1b174f90-ff83-11e6-95c1-cd7801a1f13c",
        "jobid": "d5301534-dae1-42dc-987e-a6b2c162b373",
        "description": null,
        "target_name": "V4 Test",
        "orgid": "efe5bdb3-baac-5d8e-6cae57771c13"
      },
      {
        "success": true,
        "target_type": "group",
        "firmwareid": "42352cc",
        "nodeid": null,
        "target_id": "e2d53100-ff83-11e6-95c1-cd7801a1f13c",
        "status": "JOB_DONE",
        "when": "2017-03-30T12:23:20.259-07:00",
        "siteid": "1b174f90-ff83-11e6-95c1-cd7801a1f13c",
        "jobid": "d5301534-dae1-42dc-987e-a6b2c162b373",
        "description": null,
        "target_name": "V4 Test",
        "orgid": "efe5bdb3-baac-5d8e-6cae57771c13"
      },
      {
        "success": true,
        "target_type": "group",
        "firmwareid": "42352cc",
        "nodeid": null,
        "target_id": "e2d53100-ff83-11e6-95c1-cd7801a1f13c",
        "status": "JOB_SENT",
        "when": "2017-03-30T12:18:00.754-07:00",
        "siteid": "1b174f90-ff83-11e6-95c1-cd7801a1f13c",
        "jobid": "abb0acb3-184e-4280-9023-f44d7bb5bf40",
        "description": "Test for sara",
        "target_name": "V4 Test",
        "orgid": "efe5bdb3-baac-5d8e-6cae57771c13"
      },
      {
        "success": true,
        "target_type": "group",
        "firmwareid": "42352cc",
        "nodeid": null,
        "target_id": "e2d53100-ff83-11e6-95c1-cd7801a1f13c",
        "status": "JOB_RECEIVED",
        "when": "2017-03-30T12:18:00.804-07:00",
        "siteid": "1b174f90-ff83-11e6-95c1-cd7801a1f13c",
        "jobid": "abb0acb3-184e-4280-9023-f44d7bb5bf40",
        "description": null,
        "target_name": "V4 Test",
        "orgid": "efe5bdb3-baac-5d8e-6cae57771c13"
      },
      {
        "success": false,
        "target_type": "group",
        "firmwareid": "42352cc",
        "nodeid": "JS_2",
        "target_id": "e2d53100-ff83-11e6-95c1-cd7801a1f13c",
        "status": "NODE_OFFLINE",
        "when": "2017-03-30T12:18:02.208-07:00",
        "siteid": "1b174f90-ff83-11e6-95c1-cd7801a1f13c",
        "jobid": "abb0acb3-184e-4280-9023-f44d7bb5bf40",
        "description": null,
        "target_name": "V4 Test",
        "orgid": "efe5bdb3-baac-5d8e-6cae57771c13"
      },
      {
        "success": true,
        "target_type": "group",
        "firmwareid": "42352cc",
        "nodeid": "N01232ea5",
        "target_id": "e2d53100-ff83-11e6-95c1-cd7801a1f13c",
        "status": "COMMAND_SENT",
        "when": "2017-03-30T12:18:02.252-07:00",
        "siteid": "1b174f90-ff83-11e6-95c1-cd7801a1f13c",
        "jobid": "abb0acb3-184e-4280-9023-f44d7bb5bf40",
        "description": null,
        "target_name": "V4 Test",
        "orgid": "efe5bdb3-baac-5d8e-6cae57771c13"
      },
      {
        "success": false,
        "target_type": "group",
        "firmwareid": "42352cc",
        "nodeid": "JS_3",
        "target_id": "e2d53100-ff83-11e6-95c1-cd7801a1f13c",
        "status": "NODE_OFFLINE",
        "when": "2017-03-30T12:18:02.263-07:00",
        "siteid": "1b174f90-ff83-11e6-95c1-cd7801a1f13c",
        "jobid": "abb0acb3-184e-4280-9023-f44d7bb5bf40",
        "description": null,
        "target_name": "V4 Test",
        "orgid": "efe5bdb3-baac-5d8e-6cae57771c13"
      },
      {
        "success": true,
        "target_type": "group",
        "firmwareid": "42352cc",
        "nodeid": "N01232ea5",
        "target_id": "e2d53100-ff83-11e6-95c1-cd7801a1f13c",
        "status": "NODE_REBOOTING",
        "when": "2017-03-30T12:18:02.546-07:00",
        "siteid": "1b174f90-ff83-11e6-95c1-cd7801a1f13c",
        "jobid": "abb0acb3-184e-4280-9023-f44d7bb5bf40",
        "description": null,
        "target_name": "V4 Test",
        "orgid": "efe5bdb3-baac-5d8e-6cae57771c13"
      },
      {
        "success": false,
        "target_type": "group",
        "firmwareid": "42352cc",
        "nodeid": "N01232ea5",
        "target_id": "e2d53100-ff83-11e6-95c1-cd7801a1f13c",
        "status": "UPDATE_FAILED",
        "when": "2017-03-30T12:18:29.643-07:00",
        "siteid": "1b174f90-ff83-11e6-95c1-cd7801a1f13c",
        "jobid": "abb0acb3-184e-4280-9023-f44d7bb5bf40",
        "description": null,
        "target_name": "V4 Test",
        "orgid": "efe5bdb3-baac-5d8e-6cae57771c13"
      },
      {
        "success": true,
        "target_type": "group",
        "firmwareid": "42352cc",
        "nodeid": "N01232ea5",
        "target_id": "e2d53100-ff83-11e6-95c1-cd7801a1f13c",
        "status": "COMMAND_SENT",
        "when": "2017-03-30T12:18:29.644-07:00",
        "siteid": "1b174f90-ff83-11e6-95c1-cd7801a1f13c",
        "jobid": "abb0acb3-184e-4280-9023-f44d7bb5bf40",
        "description": null,
        "target_name": "V4 Test",
        "orgid": "efe5bdb3-baac-5d8e-6cae57771c13"
      },
      {
        "success": true,
        "target_type": "group",
        "firmwareid": "42352cc",
        "nodeid": "N01232ea5",
        "target_id": "e2d53100-ff83-11e6-95c1-cd7801a1f13c",
        "status": "NODE_REBOOTING",
        "when": "2017-03-30T12:18:29.712-07:00",
        "siteid": "1b174f90-ff83-11e6-95c1-cd7801a1f13c",
        "jobid": "abb0acb3-184e-4280-9023-f44d7bb5bf40",
        "description": null,
        "target_name": "V4 Test",
        "orgid": "efe5bdb3-baac-5d8e-6cae57771c13"
      },
      {
        "success": false,
        "target_type": "group",
        "firmwareid": "42352cc",
        "nodeid": "N01232ea5",
        "target_id": "e2d53100-ff83-11e6-95c1-cd7801a1f13c",
        "status": "UPDATE_FAILED",
        "when": "2017-03-30T12:18:56.419-07:00",
        "siteid": "1b174f90-ff83-11e6-95c1-cd7801a1f13c",
        "jobid": "abb0acb3-184e-4280-9023-f44d7bb5bf40",
        "description": null,
        "target_name": "V4 Test",
        "orgid": "efe5bdb3-baac-5d8e-6cae57771c13"
      },
      {
        "success": true,
        "target_type": "group",
        "firmwareid": "42352cc",
        "nodeid": "N01232ea5",
        "target_id": "e2d53100-ff83-11e6-95c1-cd7801a1f13c",
        "status": "COMMAND_SENT",
        "when": "2017-03-30T12:18:56.437-07:00",
        "siteid": "1b174f90-ff83-11e6-95c1-cd7801a1f13c",
        "jobid": "abb0acb3-184e-4280-9023-f44d7bb5bf40",
        "description": null,
        "target_name": "V4 Test",
        "orgid": "efe5bdb3-baac-5d8e-6cae57771c13"
      },
      {
        "success": true,
        "target_type": "group",
        "firmwareid": "42352cc",
        "nodeid": "N01232ea5",
        "target_id": "e2d53100-ff83-11e6-95c1-cd7801a1f13c",
        "status": "NODE_REBOOTING",
        "when": "2017-03-30T12:18:56.540-07:00",
        "siteid": "1b174f90-ff83-11e6-95c1-cd7801a1f13c",
        "jobid": "abb0acb3-184e-4280-9023-f44d7bb5bf40",
        "description": null,
        "target_name": "V4 Test",
        "orgid": "efe5bdb3-baac-5d8e-6cae57771c13"
      },
      {
        "success": true,
        "target_type": "group",
        "firmwareid": "42352cc",
        "nodeid": "N01232ea5",
        "target_id": "e2d53100-ff83-11e6-95c1-cd7801a1f13c",
        "status": "COMMAND_SENT",
        "when": "2017-03-30T12:19:23.770-07:00",
        "siteid": "1b174f90-ff83-11e6-95c1-cd7801a1f13c",
        "jobid": "abb0acb3-184e-4280-9023-f44d7bb5bf40",
        "description": null,
        "target_name": "V4 Test",
        "orgid": "efe5bdb3-baac-5d8e-6cae57771c13"
      },
      {
        "success": false,
        "target_type": "group",
        "firmwareid": "42352cc",
        "nodeid": "N01232ea5",
        "target_id": "e2d53100-ff83-11e6-95c1-cd7801a1f13c",
        "status": "UPDATE_FAILED",
        "when": "2017-03-30T12:19:23.772-07:00",
        "siteid": "1b174f90-ff83-11e6-95c1-cd7801a1f13c",
        "jobid": "abb0acb3-184e-4280-9023-f44d7bb5bf40",
        "description": null,
        "target_name": "V4 Test",
        "orgid": "efe5bdb3-baac-5d8e-6cae57771c13"
      },
      {
        "success": true,
        "target_type": "group",
        "firmwareid": "42352cc",
        "nodeid": "N01232ea5",
        "target_id": "e2d53100-ff83-11e6-95c1-cd7801a1f13c",
        "status": "NODE_REBOOTING",
        "when": "2017-03-30T12:19:23.914-07:00",
        "siteid": "1b174f90-ff83-11e6-95c1-cd7801a1f13c",
        "jobid": "abb0acb3-184e-4280-9023-f44d7bb5bf40",
        "description": null,
        "target_name": "V4 Test",
        "orgid": "efe5bdb3-baac-5d8e-6cae57771c13"
      },
      {
        "success": true,
        "target_type": "group",
        "firmwareid": "42352cc",
        "nodeid": null,
        "target_id": "e2d53100-ff83-11e6-95c1-cd7801a1f13c",
        "status": "JOB_DONE",
        "when": "2017-03-30T12:19:51.281-07:00",
        "siteid": "1b174f90-ff83-11e6-95c1-cd7801a1f13c",
        "jobid": "abb0acb3-184e-4280-9023-f44d7bb5bf40",
        "description": null,
        "target_name": "V4 Test",
        "orgid": "efe5bdb3-baac-5d8e-6cae57771c13"
      },
      {
        "success": false,
        "target_type": "group",
        "firmwareid": "42352cc",
        "nodeid": "N01232ea5",
        "target_id": "e2d53100-ff83-11e6-95c1-cd7801a1f13c",
        "status": "UPDATE_FAILED",
        "when": "2017-03-30T12:19:51.295-07:00",
        "siteid": "1b174f90-ff83-11e6-95c1-cd7801a1f13c",
        "jobid": "abb0acb3-184e-4280-9023-f44d7bb5bf40",
        "description": null,
        "target_name": "V4 Test",
        "orgid": "efe5bdb3-baac-5d8e-6cae57771c13"
      }
      ]

    },

    getAuditList: function() {
        console.log("in helpers.getAuditList()");
        return [{
            "when": "2014-10-13 17:36:49",
            "userid": "uberuser",
            "targetid": "valleyfair",
            "targettype": "Site",
            "activity": "Overpower",
            "message": "Overpower: Under power limit: IO:100, i:0.63"
        }, {
            "when": "2014-10-13 17:36:49",
            "userid": "uberuser2",
            "targetid": "valleyfair",
            "targettype": "Site",
            "activity": "UnderPower",
            "message": "UnderPower: Under power limit: IO:100, i:0.63"
        }, {
            "when": "2014-10-13 17:36:49",
            "userid": "Tom",
            "targetid": "valleyfair",
            "targettype": "Site",
            "activity": "UnderPower",
            "message": "UnderPower: Under power limit: IO:100, i:0.63"
        }]
    },

    getNotificationList: function() {
        console.log("in helpers.getNotificationList()");
        return [{
            "name": "abc",
            "rules": "rules",
            "active": "true",
            "msg": "Ops critical alarm for node disconnect",
            "email": "sara@xx.com",
            "phone": "234333333",
        }]
    },

    getAlarmCategory: function (alarm) {
      // this is used in the Nodes page to adjust the colors of icons and markers
      var categories = ["Connection", "Light", "Sensor"];
      var catIndex = 0;
      switch (alarm) {
        case 'Disconnect':
        case 'CommFail':
        case 'DegradedNetwork':
        case 'NET/glassl/ethernet/cable/conn_1':
        case 'NET/glassl/ethernet/cable/conn_2':
        case 'NET/glassr/ethernet/cable/conn_1':
        case 'NET/glassr/ethernet/cable/conn_2':
        case 'NET/pebble/ethernet/cable/conn_1':
        case 'NET/pebble/ethernet/cable/conn_2':
        case 'NET/connectivity':
          break;
        case 'PMACFail':
        case 'SimFail':
        case 'SensorFail':
        case 'NotTested':
        case 'DownrevSoftware':
        case 'BadSensorData':
        case 'ConfigFail':
        case 'SoftwareUpdateFail':
        case 'PreRuninFail':
        case 'PostRuninFail':
        case 'USPFail':
        case 'FarmUSPFail':
        case 'StrangeReboot':
        case 'Assert':
        case 'HWFail_generic':
        case 'HWFail_HIH6131':
        case 'HWFail_ISL29023':
        case 'HWFail_SE95':
        case 'HWFail_ZMotion':
        case 'HWFail_MMA8451':
        case 'HWFail_TSC3414':
        case 'HWFail_UrbanUSP':
        case 'HWFail_RTC':
        case 'HWFail_EEPROM':
        case 'HWFail_NIGHTHAWK':
        case 'SWUpdateFail_SENSORPOD':
        case 'HWFail_PCT2075':
        case 'HWFAIL_SIHAWK':
        case 'HWFAIL_GPS':
        case 'HWFail_PodBus':
        case 'X509ClientFail':
        case 'X509ServerFail':
        case 'HardFault':
        case 'Epic_Fail':
        case 'HW/CPU_throttle':
        case 'HW/camera/0':
        case 'HW/camera/0/streaming':
        case 'HW/camera/0/temperature':
        case 'HW/camera/1':
        case 'HW/camera/1/streaming':
        case 'HW/camera/1/temperature':
        case 'HW/temperature':
        case 'HW/CPU_throttle':
        case 'HW/FPGA/PCIe_bus':
        case 'HW/FPGA/Serial_bus':
        case 'HW/FPGA/USB_bus':
        case 'HW/camera/0':
        case 'HW/camera/0/streaming':
        case 'HW/camera/0/temperature':
        case 'HW/camera/1':
        case 'HW/camera/1/streaming':
        case 'HW/camera/1/temperature':
        case 'HW/temperature':
        case 'HW/CPU_throttle':
        case 'HW/environmental/null/cable/conn_1':
        case 'HW/environmental/null/cable/conn_2':
        case 'HW/environmental/null/cable/conn_3':
        case 'HW/glassl/DC/null/cable_1':
        case 'HW/glassl/DC/null/cable_2':
        case 'HW/glassl/DC/null/cable_3':
        case 'HW/glassl/DC/null/cable_4':
        case 'HW/glassl/DC/null/cable_5':
        case 'HW/glassl/system/null/reboot':
        case 'HW/glassl/usb/cable/conn_1':
        case 'HW/glassl/usb/cable/conn_2':
        case 'HW/glassr/DC/null/cable_1':
        case 'HW/glassr/DC/null/cable_2':
        case 'HW/glassr/DC/null/cable_3':
        case 'HW/glassr/DC/null/cable_4':
        case 'HW/glassr/DC/null/cable_5':
        case 'HW/glassr/system/null/reboot':
        case 'HW/glassr/usb/cable/conn_1':
        case 'HW/glassr/usb/cable/conn_2':
        case 'HW/pebble/DC/null/cable_1':
        case 'HW/pebble/DC/null/cable_2':
        case 'HW/pebble/DC/null/cable_3':
        case 'HW/pebble/DC/null/cable_4':
        case 'HW/pebble/DC/null/cable_5':
        case 'HW/pebble/system/null/reboot':
        case 'HW/pebble/usb/cable/conn_1':
        case 'HW/pebble/usb/cable/conn_2':
        case 'HW/temperature':
        case 'HW/vdkmaster/door/null/intrusion_1':
        case 'HW/vdkmaster/door/null/intrusion_2':
        case 'HW/vdkmaster/door/null/intrusion_3':
        case 'SW/database':
        case 'SW/database/memory_usage':
        case 'SW/diskfree/data':
        case 'SW/diskfree/disk':
        case 'SW/diskfree/rootfs':
        case 'SW/service/NetworkManager/not_running':
        case 'SW/service/cfgsvc/not_running':
        case 'SW/service/device-ui/not_running':
        case 'SW/service/genetec_client/not_running':
        case 'SW/service/mediaserver/not_running':
        case 'SW/service/mnopenvpn/not_running':
        case 'SW/service/mnrecorder/not_running':
        case 'SW/service/nmm/not_running':
        case 'SW/service/remoteio/not_running':
        case 'SW/service/rtspserver/not_running':
        case 'SW/service/secmgmnt/not_running':
        case 'SW/service/sys_mon/not_running':
        case 'SW/storage':
        case 'SW/storage/volmissing/data':
        case 'SW/storage/volmissing/disk':
        case 'SW/storage/volmissing/rootfs':
        case 'SW/database':
        case 'SW/database/memory_usage':
        case 'SW/diskfree/data':
        case 'SW/diskfree/disk':
        case 'SW/diskfree/rootfs':
        case 'SW/service/NetworkManager/not_running':
        case 'SW/service/cfgsvc/not_running':
        case 'SW/service/device-ui/not_running':
        case 'SW/service/genetec_client/not_running':
        case 'SW/service/mediaserver/not_running':
        case 'SW/service/mnopenvpn/not_running':
        case 'SW/service/mnrecorder/not_running':
        case 'SW/service/nmm/not_running':
        case 'SW/service/remoteio/not_running':
        case 'SW/service/rtspserver/not_running':
        case 'SW/service/secmgmnt/not_running':
        case 'SW/service/sys_mon/not_running':
        case 'SW/storage':
        case 'SW/storage/volmissing/data':
        case 'SW/storage/volmissing/disk':
        case 'SW/storage/volmissing/rootfs':
        case 'SW/database':
        case 'SW/database/memory_usage':
        case 'SW/diskfree/data':
        case 'SW/diskfree/disk':
        case 'SW/diskfree/rootfs':
        case 'SW/glassl/application/null/crash':
        case 'SW/glassr/application/null/crash':
        case 'SW/pebble/application/null/crash':
        case 'SW/service/NetworkManager/not_running':
        case 'SW/service/cfgsvc/not_running':
        case 'SW/service/device-ui/not_running':
        case 'SW/service/genetec_client/not_running':
        case 'SW/service/mediaserver/not_running':
        case 'SW/service/mnopenvpn/not_running':
        case 'SW/service/mnrecorder/not_running':
        case 'SW/service/nmm/not_running':
        case 'SW/service/remoteio/not_running':
        case 'SW/service/rtspserver/not_running':
        case 'SW/service/secmgmnt/not_running':
        case 'SW/service/sys_mon/not_running':
        case 'SW/storage/volmissing/data':
        case 'SW/storage/volmissing/disk':
        case 'SW/storage/volmissing/rootfs':
          catIndex = 2;
          break;
        case 'ScheduleFail':
        case 'DriverFail':
        case 'UnderPower':
        case 'OverPower':
        case 'HWFail_STUCK_RELAY':
          catIndex = 1;
          break;
      }
      return categories[catIndex];
    },

    getAlarmTypeList: function () {
        console.log("in helpers.getAlarmTypeList()");
        return [
          {
            "category": "All Models",
            "actions": [  
              {"alarmvalue": "Disconnect", "alarmtype": "Disconnect"}
            ]
          },
          {
            "category": "Core Node",
            "actions": [
              {"alarmvalue": "CommFail", "alarmtype": "Comm Fail"},
              {"alarmvalue": "SimFail", "alarmtype": "Sim Fail"},
              {"alarmvalue": "NotTested", "alarmtype": "Not Tested"},
              {"alarmvalue": "DownrevSoftware", "alarmtype": "Down rev Software"},
              {"alarmvalue": "BadSensorData", "alarmtype": "Bad Sensor Data"},
              {"alarmvalue": "ConfigFail", "alarmtype": "Config Fail"},
              {"alarmvalue": "DegradedNetwork", "alarmtype": "Degraded Network"},
              {"alarmvalue": "SoftwareUpdateFail", "alarmtype": "Software Update Fail"},
              {"alarmvalue": "ScheduleFail", "alarmtype": "Schedule Fail"},
              {"alarmvalue": "PreRuninFail", "alarmtype": "Pre Runin Fail"},
              {"alarmvalue": "PostRuninFail", "alarmtype": "Post Runin Fail"},
              {"alarmvalue": "USPFail", "alarmtype": "USP Fail"},
              {"alarmvalue": "PMACFail", "alarmtype": "PMAC Fail"},
              {"alarmvalue": "DriverFail", "alarmtype": "Driver Fail"},
              {"alarmvalue": "FarmUSPFail", "alarmtype": "Farm USP Fail"},
              {"alarmvalue": "SensorFail", "alarmtype": "Sensor Fail"},
              {"alarmvalue": "StrangeReboot", "alarmtype": "Strange Reboot"},
              {"alarmvalue": "Assert", "alarmtype": "Assert"},
              {"alarmvalue": "X509ClientFail", "alarmtype": "X509 Client Fail"},
              {"alarmvalue": "X509ServerFail", "alarmtype": "X509 Server Fail"},
              {"alarmvalue": "UnderPower", "alarmtype": "Under Power"},
              {"alarmvalue": "OverPower", "alarmtype": "Over Power"},
              {"alarmvalue": "HardFault", "alarmtype": "Hard Fault"},
              {"alarmvalue": "HWFail_generic", "alarmtype": "HW Fail Generic"},
              {"alarmvalue": "HWFail_HIH6131", "alarmtype": "HW Fail HIH6131"},
              {"alarmvalue": "HWFail_ISL29023", "alarmtype": "HW Fail ISL29023"},
              {"alarmvalue": "HWFail_SE95", "alarmtype": "HW Fail SE95"},
              {"alarmvalue": "HWFail_ZMotion", "alarmtype": "HW Fail ZMotion"},
              {"alarmvalue": "HWFail_MMA8451", "alarmtype": "HW Fail MMA8451"},
              {"alarmvalue": "HWFail_TSC3414", "alarmtype": "HW Fail TSC3414"},
              {"alarmvalue": "HWFail_UrbanUSP", "alarmtype": "HW Fail UrbanUSP"},
              {"alarmvalue": "HWFail_RTC", "alarmtype": "HW Fail RTC"},
              {"alarmvalue": "HWFail_EEPROM", "alarmtype": "HW Fail EEPROM"},
              {"alarmvalue": "HWFail_NIGHTHAWK", "alarmtype": "HW Fail NIGHTHAWK"},
              {"alarmvalue": "SWUpdateFail_SENSORPOD", "alarmtype": "SW Update Fail SENSORPOD"},
              {"alarmvalue": "HWFail_STUCK_RELAY", "alarmtype": "HW Fail STUCK RELAY"},
              {"alarmvalue": "HWFail_PCT2075 ", "alarmtype": "HW Fail PCT2075"},
              {"alarmvalue": "HWFAIL_SIHAWK", "alarmtype": "HW Fail SIHAWK"},
              {"alarmvalue": "HWFAIL_GPS", "alarmtype": "HW Fail GPS"},
              {"alarmvalue": "HWFail_PodBus", "alarmtype": "HW Fail PodBus"},
              {"alarmvalue": "Epic_Fail", "alarmtype": "Epic Fail"},
              {"alarmvalue": "SWUpdateFail_Prod", "alarmtype": "SW Update Fail Prod"},
              {"alarmvalue": "HWFail_ProdBus", "alarmtype": "HW Fail ProdBus"},
            ]
          },
          {
            "category": "Falcon-q",
            "actions": [
              {"alarmvalue": "HW/CPU_throttle", "alarmtype": "HW/CPU_throttle"},
              {"alarmvalue": "HW/block_device", "alarmtype": "HW/block_device"},
              {"alarmvalue": "HW/camera/0", "alarmtype": "HW/camera/0"},
              {"alarmvalue": "HW/camera/0/analyzer_stream", "alarmtype": "HW/camera/0/analyzer_stream"},
              {"alarmvalue": "HW/camera/0/camera_image", "alarmtype": "HW/camera/0/camera_image"},
              {"alarmvalue": "HW/camera/0/camera_stream", "alarmtype": "HW/camera/0/camera_stream"},
              {"alarmvalue": "HW/camera/0/highres_pipeline", "alarmtype": "HW/camera/0/highres_pipeline"},
              {"alarmvalue": "HW/camera/0/lowres_pipeline", "alarmtype": "HW/camera/0/lowres_pipeline"},
              {"alarmvalue": "HW/camera/0/stitching_map", "alarmtype": "HW/camera/0/stitching_map"},
              {"alarmvalue": "HW/camera/0/streaming", "alarmtype": "HW/camera/0/streaming"},
              {"alarmvalue": "HW/camera/0/temperature", "alarmtype": "HW/camera/0/temperature"},
              {"alarmvalue": "HW/camera/1", "alarmtype": "HW/camera/1"},
              {"alarmvalue": "HW/camera/1/camera_image", "alarmtype": "HW/camera/1/camera_image"},
              {"alarmvalue": "HW/camera/1/streaming", "alarmtype": "HW/camera/1/streaming"},
              {"alarmvalue": "HW/camera/1/temperature", "alarmtype": "HW/camera/1/temperature"},
              {"alarmvalue": "HW/temperature", "alarmtype": "HW/temperature"},
              {"alarmvalue": "NET/connectivity", "alarmtype": "NET/connectivity"},
              {"alarmvalue": "SW/database", "alarmtype": "SW/database"},
              {"alarmvalue": "SW/database/memory_usage", "alarmtype": "SW/database/memory_usage"},
              {"alarmvalue": "SW/database/misconfig/eth-x/port-forwarding", "alarmtype": "SW/database/misconfig/eth-x/port-forwarding"},
              {"alarmvalue": "SW/diskfree/data", "alarmtype": "SW/diskfree/data"},
              {"alarmvalue": "SW/diskfree/disk", "alarmtype": "SW/diskfree/disk"},
              {"alarmvalue": "SW/diskfree/rootfs", "alarmtype": "SW/diskfree/rootfs"},
              {"alarmvalue": "SW/service/NetworkManager/not_running", "alarmtype": "SW/service/NetworkManager/not_running"},
              {"alarmvalue": "SW/service/cam_mgr/not_running", "alarmtype": "SW/service/cam_mgr/not_running"},
              {"alarmvalue": "SW/service/cfgsvc/not_running", "alarmtype": "SW/service/cfgsvc/not_running"},
              {"alarmvalue": "SW/service/device-ui/not_running", "alarmtype": "SW/service/device-ui/not_running"},
              {"alarmvalue": "SW/service/genetec_client/not_running", "alarmtype": "SW/service/genetec_client/not_running"},
              {"alarmvalue": "SW/service/mediaserver/not_running", "alarmtype": "SW/service/mediaserver/not_running"},
              {"alarmvalue": "SW/service/mnopenvpn/not_running", "alarmtype": "SW/service/mnopenvpn/not_running"},
              {"alarmvalue": "SW/service/mnrecorder/not_running", "alarmtype": "SW/service/mnrecorder/not_running"},
              {"alarmvalue": "SW/service/nmm/not_running", "alarmtype": "SW/service/nmm/not_running"},
              {"alarmvalue": "SW/service/remoteio/not_running", "alarmtype": "SW/service/remoteio/not_running"},
              {"alarmvalue": "SW/service/rtspserver/not_running", "alarmtype": "SW/service/rtspserver/not_running"},
              {"alarmvalue": "SW/service/secmgmnt/not_running", "alarmtype": "SW/service/secmgmnt/not_running"},
              {"alarmvalue": "SW/service/sys_mon/not_running", "alarmtype": "SW/service/sys_mon/not_running"},
              {"alarmvalue": "SW/storage", "alarmtype": "SW/storage"},
              {"alarmvalue": "SW/storage/volmissing/rootfs", "alarmtype": "SW/storage/volmissing/rootfs"},
              {"alarmvalue": "SW/storage/volmissing/data", "alarmtype": "SW/storage/volmissing/data"},
              {"alarmvalue": "SW/storage/volmissing/disk", "alarmtype": "SW/storage/volmissing/disk"},
              
            ]
          },

          {
            "category": "Merlin",
            "actions": [
              {"alarmvalue": "HW/CPU_throttle", "alarmtype": "HW/CPU_throttle"},
              {"alarmvalue": "HW/FPGA/PCIe_bus", "alarmtype": "HW/FPGA/PCIe_bus"},
              {"alarmvalue": "HW/FPGA/Serial_bus", "alarmtype": "HW/FPGA/Serial_bus"},
              {"alarmvalue": "HW/FPGA/USB_bus", "alarmtype": "HW/FPGA/USB_bus"},
              {"alarmvalue": "HW/block_device", "alarmtype": "HW/block_device"},
              {"alarmvalue": "HW/camera/0/analyzer_stream", "alarmtype": "HW/camera/0/analyzer_stream"},
              {"alarmvalue": "HW/camera/0/camera_image", "alarmtype": "HW/camera/0/camera_image"},
              {"alarmvalue": "HW/camera/0/camera_stream", "alarmtype": "HW/camera/0/camera_stream"},
              {"alarmvalue": "HW/camera/0/highres_pipeline", "alarmtype": "HW/camera/0/highres_pipeline"},
              {"alarmvalue": "HW/camera/0/lowres_pipeline", "alarmtype": "HW/camera/0/lowres_pipeline"},
              {"alarmvalue": "HW/camera/0/stitching_map", "alarmtype": "HW/camera/0/stitching_map"},
              {"alarmvalue": "HW/camera/0/streaming", "alarmtype": "HW/camera/0/streaming"},
              {"alarmvalue": "HW/camera/1/camera_image", "alarmtype": "HW/camera/1/camera_image"},
              {"alarmvalue": "HW/camera/1/streaming", "alarmtype": "HW/camera/1/streaming"},
              {"alarmvalue": "HW/temperature", "alarmtype": "HW/temperature"},
              {"alarmvalue": "NET/connectivity", "alarmtype": "NET/connectivity"},
              {"alarmvalue": "SW/database", "alarmtype": "SW/database"},
              {"alarmvalue": "SW/database/memory_usage", "alarmtype": "SW/database/memory_usage"},
              {"alarmvalue": "SW/diskfree/data", "alarmtype": "SW/diskfree/data"},
              {"alarmvalue": "SW/diskfree/disk", "alarmtype": "SW/diskfree/disk"},
              {"alarmvalue": "SW/diskfree/rootfs", "alarmtype": "SW/diskfree/rootfs"},
              {"alarmvalue": "SW/service/NetworkManager/not_running", "alarmtype": "SW/service/NetworkManager/not_running"},
              {"alarmvalue": "SW/service/cfgsvc/not_running", "alarmtype": "SW/service/cfgsvc/not_running"},
              {"alarmvalue": "SW/service/device-ui/not_running", "alarmtype": "SW/service/device-ui/not_running"},
              {"alarmvalue": "SW/service/genetec_client/not_running", "alarmtype": "SW/service/genetec_client/not_running"},
              {"alarmvalue": "SW/service/mediaserver/not_running", "alarmtype": "SW/service/mediaserver/not_running"},
              {"alarmvalue": "SW/service/mnopenvpn/not_running", "alarmtype": "SW/service/mnopenvpn/not_running"},
              {"alarmvalue": "SW/service/mnrecorder/not_running", "alarmtype": "SW/service/mnrecorder/not_running"},
              {"alarmvalue": "SW/service/nmm/not_running", "alarmtype": "SW/service/nmm/not_running"},
              {"alarmvalue": "SW/service/remoteio/not_running", "alarmtype": "SW/service/remoteio/not_running"},
              {"alarmvalue": "SW/service/rtspserver/not_running", "alarmtype": "SW/service/rtspserver/not_running"},
              {"alarmvalue": "SW/service/secmgmnt/not_running", "alarmtype": "SW/service/secmgmnt/not_running"},
              {"alarmvalue": "SW/service/sys_mon/not_running", "alarmtype": "SW/service/sys_mon/not_running"},
              {"alarmvalue": "SW/storage", "alarmtype": "SW/storage"},
              {"alarmvalue": "SW/storage/volmissing/rootfs", "alarmtype": "SW/storage/volmissing/rootfs"},
              {"alarmvalue": "SW/storage/volmissing/data", "alarmtype": "SW/storage/volmissing/data"},
              {"alarmvalue": "SW/storage/volmissing/disk", "alarmtype": "SW/storage/volmissing/disk"}, 
              {"alarmvalue": "SW/service/va-mve/not_running", "alarmtype": "SW/service/va-mve/not_running"},  
              {"alarmvalue": "SW/service/mve-control/not_running", "alarmtype": "SW/service/mve-control/not_running"},  
              {"alarmvalue": "SW/service/mve-overlay/not_running", "alarmtype": "SW/service/mve-overlay/not_running"},  
              {"alarmvalue": "SW/service/mve-pathing/not_running", "alarmtype": "SW/service/mve-pathing/not_running"},  
              {"alarmvalue": "SW/service/mve-mqtt-metadata-dispatcher/not_running", "alarmtype": "SW/service/mve-mqtt-metadata-dispatcher/not_running"},  
              {"alarmvalue": "SW/service/mve-metadata-filter/not_running", "alarmtype": "SW/service/mve-metadata-filter/not_running"},  
              {"alarmvalue": "SW/service/mve-event-generator-ch0/not_running", "alarmtype": "SW/service/mve-event-generator-ch0/not_running"},  
              {"alarmvalue": "HW/socfpga/mve/license_status", "alarmtype": "HW/socfpga/mve/license_status"},
              {"alarmvalue": "HW/socfpga/mve/pci_status", "alarmtype": "HW/socfpga/mve/pci_status"},  
            ]
          },

          {
            "category": "VDKMaster",
            "actions": [
              {"alarmvalue": "HW/block_device", "alarmtype": "HW/block_device"},
              {"alarmvalue": "HW/device/usb/cable/conn", "alarmtype": "HW/device/usb/cable/conn"},
              {"alarmvalue": "HW/environmental/null/cable/conn", "alarmtype": "HW/environmental/null/cable/conn"},
              {"alarmvalue": "HW/glassl/DC/null/cable_io", "alarmtype": "HW/glassl/DC/null/cable_io"},
              {"alarmvalue": "HW/glassl/DC/null/cable_power_off", "alarmtype": "HW/glassl/DC/null/cable_power_off"},
              {"alarmvalue": "HW/glassl/DC/null/cable_sync", "alarmtype": "HW/glassl/DC/null/cable_sync"},
              {"alarmvalue": "HW/glassl/DC/null/cable_timeout", "alarmtype": "HW/glassl/DC/null/cable_timeout"},
              {"alarmvalue": "HW/glassl/DC/null/cable_unknown", "alarmtype": "HW/glassl/DC/null/cable_unknown"},
              {"alarmvalue": "HW/glassl/HDMI/cable/unavailable", "alarmtype": "HW/glassl/HDMI/cable/unavailable"},
              {"alarmvalue": "HW/glassl/system/null/reboot", "alarmtype": "HW/glassl/system/null/reboot"},
              {"alarmvalue": "HW/glassl/usb/cable/conn", "alarmtype": "HW/glassl/usb/cable/conn"},
              {"alarmvalue": "HW/glassr/DC/null/cable_io", "alarmtype": "HW/glassr/DC/null/cable_io"},
              {"alarmvalue": "HW/glassr/DC/null/cable_power_off", "alarmtype": "HW/glassr/DC/null/cable_power_off"},
              {"alarmvalue": "HW/glassr/DC/null/cable_sync", "alarmtype": "HW/glassr/DC/null/cable_sync"},
              {"alarmvalue": "HW/glassr/DC/null/cable_timeout", "alarmtype": "HW/glassr/DC/null/cable_timeout"},
              {"alarmvalue": "HW/glassr/DC/null/cable_unknown", "alarmtype": "HW/glassr/DC/null/cable_unknown"},
              {"alarmvalue": "HW/glassr/HDMI/cable/unavailable", "alarmtype": "HW/glassr/HDMI/cable/unavailable"},
              {"alarmvalue": "HW/glassr/system/null/reboot", "alarmtype": "HW/glassr/system/null/reboot"},
              {"alarmvalue": "HW/glassr/usb/cable/conn", "alarmtype": "HW/glassr/usb/cable/conn"},
              {"alarmvalue": "HW/pebble/DC/null/cable_io", "alarmtype": "HW/pebble/DC/null/cable_io"},
              {"alarmvalue": "HW/pebble/DC/null/cable_power_off", "alarmtype": "HW/pebble/DC/null/cable_power_off"},
              {"alarmvalue": "HW/pebble/DC/null/cable_sync", "alarmtype": "HW/pebble/DC/null/cable_sync"},
              {"alarmvalue": "HW/pebble/DC/null/cable_timeout", "alarmtype": "HW/pebble/DC/null/cable_timeout"},
              {"alarmvalue": "HW/pebble/DC/null/cable_unknown", "alarmtype": "HW/pebble/DC/null/cable_unknown"},
              {"alarmvalue": "HW/pebble/HDMI/cable/unavailable", "alarmtype": "HW/pebble/HDMI/cable/unavailable"},
              {"alarmvalue": "HW/pebble/system/null/reboot", "alarmtype": "HW/pebble/system/null/reboot"},
              {"alarmvalue": "HW/pebble/tickers/left/unconfigured", "alarmtype": "HW/pebble/tickers/left/unconfigured"},
              {"alarmvalue": "HW/pebble/tickers/null/disconnected", "alarmtype": "HW/pebble/tickers/null/disconnected"},
              {"alarmvalue": "HW/pebble/tickers/right/unconfigured", "alarmtype": "HW/pebble/tickers/right/unconfigured"},              
              {"alarmvalue": "HW/pebble/usb/cable/conn", "alarmtype": "HW/pebble/usb/cable/conn"},
              {"alarmvalue": "HW/temperature", "alarmtype": "HW/temperature"},
              {"alarmvalue": "HW/vdkmaster/door/null/intr_batt", "alarmtype": "HW/vdkmaster/door/null/intr_batt"},
              {"alarmvalue": "HW/vdkmaster/door/null/intr_door", "alarmtype": "HW/vdkmaster/door/null/intr_door"},
              {"alarmvalue": "HW/vdkmaster/door/null/intr_small_cell", "alarmtype": "HW/vdkmaster/door/null/intr_small_cell"},
              {"alarmvalue": "NET/connectivity", "alarmtype": "NET/connectivity"},
              {"alarmvalue": "NET/glassl/ethernet/cable/conn_host", "alarmtype": "NET/glassl/ethernet/cable/conn_host"},
              {"alarmvalue": "NET/glassl/ethernet/cable/conn_network", "alarmtype": "NET/glassl/ethernet/cable/conn_network"},
              {"alarmvalue": "NET/glassr/ethernet/cable/conn_host", "alarmtype": "NET/glassr/ethernet/cable/conn_host"},
              {"alarmvalue": "NET/glassr/ethernet/cable/conn_network", "alarmtype": "NET/glassr/ethernet/cable/conn_network"},
              {"alarmvalue": "NET/pebble/ethernet/cable/conn_host", "alarmtype": "NET/pebble/ethernet/cable/conn_host"},
              {"alarmvalue": "NET/pebble/ethernet/cable/conn_network", "alarmtype": "NET/pebble/ethernet/cable/conn_network"},
              {"alarmvalue": "SW/database", "alarmtype": "SW/database"},
              {"alarmvalue": "SW/database/memory_usage", "alarmtype": "SW/database/memory_usage"},
              {"alarmvalue": "SW/database/misconfig/eth-x/port-forwarding", "alarmtype": "SW/database/misconfig/eth-x/port-forwarding"},
              {"alarmvalue": "SW/diskfree/data", "alarmtype": "SW/diskfree/data"},
              {"alarmvalue": "SW/diskfree/rootfs", "alarmtype": "SW/diskfree/rootfs"},
              {"alarmvalue": "SW/glassl/application/bootactivity/stuck", "alarmtype": "SW/glassl/application/bootactivity/stuck"},
              {"alarmvalue": "SW/glassl/application/null/crash", "alarmtype": "SW/glassl/application/null/crash"},
              {"alarmvalue": "SW/glassl/configuration/null/unconfigured", "alarmtype": "SW/glassl/configuration/null/unconfigured"},
              {"alarmvalue": "SW/glassl/configuration/siteid/unset", "alarmtype": "SW/glassl/configuration/siteid/unset"},
              {"alarmvalue": "SW/glassr/application/bootactivity/stuck", "alarmtype": "SW/glassr/application/bootactivity/stuck"},
              {"alarmvalue": "SW/glassr/application/null/crash", "alarmtype": "SW/glassr/application/null/crash"},
              {"alarmvalue": "SW/glassr/configuration/null/unconfigured", "alarmtype": "SW/glassr/configuration/null/unconfigured"},
              {"alarmvalue": "SW/glassr/configuration/siteid/unset", "alarmtype": "SW/glassr/configuration/siteid/unset"},
              {"alarmvalue": "SW/pebble/application/bootactivity/stuck", "alarmtype": "SW/pebble/application/bootactivity/stuck"},
              {"alarmvalue": "SW/pebble/application/null/crash", "alarmtype": "SW/pebble/application/null/crash"},
              {"alarmvalue": "SW/pebble/configuration/null/unconfigured", "alarmtype": "SW/pebble/configuration/null/unconfigured"},
              {"alarmvalue": "SW/pebble/configuration/siteid/unset", "alarmtype": "SW/pebble/configuration/siteid/unset"},
              {"alarmvalue": "SW/service/NetworkManager/not_running", "alarmtype": "SW/service/NetworkManager/not_running"},
              {"alarmvalue": "SW/service/cfgsvc/not_running", "alarmtype": "SW/service/cfgsvc/not_running"},
              {"alarmvalue": "SW/service/device-ui/not_running", "alarmtype": "SW/service/device-ui/not_running"},
              {"alarmvalue": "SW/service/genetec_client/not_running", "alarmtype": "SW/service/genetec_client/not_running"},
              {"alarmvalue": "SW/service/mediaserver/not_running", "alarmtype": "SW/service/mediaserver/not_running"},
              {"alarmvalue": "SW/service/mnopenvpn/not_running", "alarmtype": "SW/service/mnopenvpn/not_running"},
              {"alarmvalue": "SW/service/mnrecorder/not_running", "alarmtype": "SW/service/mnrecorder/not_running"},
              {"alarmvalue": "SW/service/nmm/not_running", "alarmtype": "SW/service/nmm/not_running"},
              {"alarmvalue": "SW/service/remoteio/not_running", "alarmtype": "SW/service/remoteio/not_running"},
              {"alarmvalue": "SW/service/rtspserver/not_running", "alarmtype": "SW/service/rtspserver/not_running"},
              {"alarmvalue": "SW/service/secmgmnt/not_running", "alarmtype": "SW/service/secmgmnt/not_running"},
              {"alarmvalue": "SW/service/sys_mon/not_running", "alarmtype": "SW/service/sys_mon/not_running"},
              {"alarmvalue": "SW/storage/volmissing/data", "alarmtype": "SW/storage/volmissing/data"},
              {"alarmvalue": "SW/storage/volmissing/rootfs", "alarmtype": "SW/storage/volmissing/rootfs"},
            ]
          },

          {
            "category": "Cnext",
            "actions": [
              {"alarmvalue": "HW/CPU_throttle", "alarmtype": "HW/CPU_throttle"},
              {"alarmvalue": "HW/HV/eeprom/null/fail", "alarmtype": "HW/HV/eeprom/null/fail"},
              {"alarmvalue": "HW/HV/pmac/null/fail", "alarmtype": "HW/HV/pmac/null/fail"},
              {"alarmvalue": "HW/HV/relay/main/stuck_closed", "alarmtype": "HW/HV/relay/main/stuck_closed"},
              {"alarmvalue": "HW/HV/sensor/light/fail", "alarmtype": "HW/HV/sensor/light/fail"},
              {"alarmvalue": "HW/HV/sensor/pod/fail", "alarmtype": "HW/HV/sensor/pod/fail"},
              {"alarmvalue": "HW/HV/sensor/pod/fwupdate_failed", "alarmtype": "HW/HV/sensor/pod/fwupdate_failed"},
              {"alarmvalue": "HW/HV/sensor/pod_bus/fail", "alarmtype": "HW/HV/sensor/pod_bus/fail"},
              {"alarmvalue": "HW/HV/sensor/temperature/fail", "alarmtype": "HW/HV/sensor/temperature/fail"},
              {"alarmvalue": "HW/LV/HV/config/error", "alarmtype": "HW/LV/HV/config/error"},
              {"alarmvalue": "HW/LV/HV/fw/update_failed", "alarmtype": "HW/LV/HV/fw/update_failed"},
              {"alarmvalue": "HW/LV/HV/usb/not_responding", "alarmtype": "HW/LV/HV/usb/not_responding"},
              {"alarmvalue": "HW/temperature", "alarmtype": "HW/temperature"},
              {"alarmvalue": "NET/connectivity", "alarmtype": "NET/connectivity"},
              {"alarmvalue": "SW/HV/assert/null/SW_fault", "alarmtype": "SW/HV/assert/null/SW_fault"},
              {"alarmvalue": "SW/database", "alarmtype": "SW/database"},
              {"alarmvalue": "SW/database/memory_usage", "alarmtype": "SW/database/memory_usage"},
              {"alarmvalue": "SW/database/misconfig/eth-x/port-forwarding", "alarmtype": "SW/database/misconfig/eth-x/port-forwarding"},
              {"alarmvalue": "SW/diskfree/data", "alarmtype": "SW/diskfree/data"},
              {"alarmvalue": "SW/diskfree/disk", "alarmtype": "SW/diskfree/disk"},
              {"alarmvalue": "SW/diskfree/rootfs", "alarmtype": "SW/diskfree/rootfs"},
              {"alarmvalue": "SW/service/NetworkManager/not_running", "alarmtype": "SW/service/NetworkManager/not_running"},
              {"alarmvalue": "SW/service/cfgsvc/not_running", "alarmtype": "SW/service/cfgsvc/not_running"},
              {"alarmvalue": "SW/service/cnode-pm/not_running", "alarmtype": "SW/service/cnode-pm/not_running"},
              {"alarmvalue": "SW/service/device-ui/not_running", "alarmtype": "SW/service/device-ui/not_running"},
              {"alarmvalue": "SW/service/mnopenvpn/not_running", "alarmtype": "SW/service/mnopenvpn/not_running"},
              {"alarmvalue": "SW/service/nmm/not_running", "alarmtype": "SW/service/nmm/not_running"},
              {"alarmvalue": "SW/service/secmgmnt/not_running", "alarmtype": "SW/service/secmgmnt/not_running"},
              {"alarmvalue": "SW/service/sys_mon/not_running", "alarmtype": "SW/service/sys_mon/not_running"},
              {"alarmvalue": "SW/storage/volmissing/rootfs", "alarmtype": "SW/storage/volmissing/rootfs"},
              {"alarmvalue": "SW/storage/volmissing/data", "alarmtype": "SW/storage/volmissing/data"},
              {"alarmvalue": "SW/storage/volmissing/disk", "alarmtype": "SW/storage/volmissing/disk"},
            ]
          },
                        
        ]
    },

    getAlertList: function() {
        console.log("in helpers.getAlertList()");
        return [{
            "type": "Node Disconnected",
            "severity": "major",
            "orgid": "Coke",
            "siteid": "Sunnyvale campus",
            "nodeid": "N012307af",
            "date": "Jan 2nd 2016",
            "time": "3:45pm",
        }]
    },

    getFixtureType: function() {
        console.log("in helpers.getFixtureType()");

        return [
        {
            "fixtureType": "Acorn",
            "fixtureTypeId": "acorn"
        },
        {
            "fixtureType": "Bollard",
            "fixtureTypeId": "bollard"
        },
        {
            "fixtureType": "Camera",
            "fixtureTypeId": "camera"
        },
        {
            "fixtureType": "Canopy",
            "fixtureTypeId": "canopy"
        },
        {
            "fixtureType": "Citadel 818",
            "fixtureTypeId": "citadel 818"
        },
        {
            "fixtureType": "Citadel Wallpack",
            "fixtureTypeId": "citadel wallpack"
        },
        {
            "fixtureType": "Cuscutlan",
            "fixtureTypeId": "cuscutlan"
        },
        {
            "fixtureType": "Cobra Head",
            "fixtureTypeId": "cobra head"
        },
        {
            "fixtureType": "Garage Canopy",
            "fixtureTypeId": "garage canopy"
        },
        {
            "fixtureType": "Globe",
            "fixtureTypeId": "globe"
        },
        {
            "fixtureType": "Highbay",
            "fixtureTypeId": "highbay"
        },
        {
            "fixtureType": "High Mast",
            "fixtureTypeId": "highmast"
        },
        {
            "fixtureType": "Highbay 3 petal",
            "fixtureTypeId": "highbay 3-petal"

        },
        {
            "fixtureType": "Highbay 4 petal",
            "fixtureTypeId": "highbay 4-petal"
        },
        {
            "fixtureType": "Highbay 6 petal",
            "fixtureTypeId": "highbay 6-petal"

        },
        {
            "fixtureType": "Highbay 8 petal",
            "fixtureTypeId": "highbay 8-petal"

        },
        {
            "fixtureType": "Highbay 9 petal",
            "fixtureTypeId": "highbay 9-petal"
        },
        {
            "fixtureType": "Highbay 12 petal",
            "fixtureTypeId": "highbay 12-petal"
        },
        {
            "fixtureType": "Lantern",
            "fixtureTypeId": "lantern"
        },
        {
            "fixtureType": "LMP",
            "fixtureTypeId": "lmp"
        },
        {
            "fixtureType": "Pendant",
            "fixtureTypeId": "pendant"
        },
        {
            "fixtureType": "Post Top",
            "fixtureTypeId": "post top"
        }, 
        {
            "fixtureType": "Roadway",
            "fixtureTypeId": "roadway"
        }, 
        {
            "fixtureType": "Shoebox",
            "fixtureTypeId": "shoebox"
        }, 
        {
            "fixtureType": "Shoebox 418",
            "fixtureTypeId": "shoebox 418"
        },
        {
            "fixtureType": "Spotlights",
            "fixtureTypeId": "spotlights"
        },
        {
            "fixtureType": "Streetlamp1",
            "fixtureTypeId": "streetlamp1"
        },
        {
            "fixtureType": "Wallpack",
            "fixtureTypeId": "wallpack"
        }]
    },

    getUserRoles: function() {
        console.log("in helpers.getUserRoles()");

        return [{
            "userRole": ["sensity_user", "sensity_read_only"]
        }, {
            "userRole": ["partner_admin", "partner_deployment_user", "partner_lighting_user", "partner_sensor_user", "partner_networking_user", "partner_read_only", "partner_api", "installer"]
        }, {
            "userRole": ["end_user_admin", "end_user_lighting_user", "end_user_sensor_user", "end_user_networking_user", "end_user_read_only", "end_user_api","installer",  "parking_owner_admin", "parking_manager", "policy_authority"]
        }, {
            "userRole": ["sensity_user", "sensity_admin", "sensity_read_only"]
        }, {
            "userRole": ["partner_admin", "partner_deployment_user", "partner_lighting_user", "partner_sensor_user", "partner_networking_user", "partner_read_only", "partner_api", "installer"]
        }, {
            "userRole": ["partner_admin", "partner_deployment_user", "partner_lighting_user", "partner_sensor_user", "partner_networking_user", "partner_read_only", "partner_api", "installer"]
        }, {
            "userRole": ["end_user_admin", "end_user_lighting_user", "end_user_sensor_user", "end_user_networking_user", "end_user_read_only", "end_user_api", "installer",  "parking_owner_admin", "parking_manager", "policy_authority"]
        }, {
            "userRole": ["end_user_admin", "end_user_lighting_user", "end_user_sensor_user", "end_user_networking_user", "end_user_read_only", "end_user_api", "installer",  "parking_owner_admin", "parking_manager", "policy_authority"]
        }]
    },

    getUsersList: function() {
        console.log("in helpers.getUsersList()");
        return [{
            "name": "abc"
        }]
    },

     getEnergyList: function() {
        return [{
            id: 'energy',
            name: 'Energy',
        }];
    },


    // get Next Item for the modal overlay
    // getNextItem :function(allItems, currentItem,idName, formNameRowsUpdate ){

    //     console.log("idName", idName);
    //         var selectedIndices2 = [];
    //         for( var j in allItems){
    //             if(allItems[j][idName] === currentItem[idName]){
    //                 j++;
    //                 selectedIndices2.push(j);
    //                 break;
    //             }
    //         }
    //         ReactBootstrap.Dispatcher.emit(formNameRowsUpdate,selectedIndices2 );

    // },

    // // Get Previous Item for the modal overlay
    // getPreviousItem : function(allItems, currentItem, idName,formNameRowsUpdate){
    //     var selectedIndices1 = [];
    //     for( var j in allItems){
    //         if(allItems[j][idName] === currentItem[idName]){
    //             j--;
    //             var newPreviousCustomer = allItems[j];
    //             selectedIndices1.push(j);
    //             break;
    //         }
    //     }
    //     ReactBootstrap.Dispatcher.emit(formNameRowsUpdate,selectedIndices1 );
    // }

};

module.exports = helpers;