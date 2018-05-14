/*
schedule.js - schedule editor
*/
var SunCalc = require('suncalc');

var scheduler = (function () {

	var ucdays = ["Mon","Tue","Wed","Thu","Fri","Sat","Sun"];
	var lcdays = ucdays.map(function(day,index){return day.toLowerCase();});
	var ws = 0;

	var timeToPct = function(time, sunrise, sunset){
		// converts time to percentage of a day
		if (time.match(/^\d\d:\d\d:\d\d/)) {
			var hms = time.split(":");
			return (100 * ((parseInt(hms[0]) * 60 + parseInt(hms[1])) / 1440));
		};
		if (time.indexOf("sunrise") >= 0) {
			time = time.substring(7);
			return (parseFloat(sunrise) + (100 * ((parseInt(time) || 0) / 1440)));
		};
		if (time.indexOf("sunset") >= 0) {
			time = time.substring(6);
			return (parseFloat(sunset) + (100 * ((parseInt(time) || 0) / 1440)));
		};

	};

	var setDaylightOverlay = function(elem, date, readonly) {
		var sunTimes, sunrise = 0.25, sunset = 0.75;
		if (NSN && NSN.site && NSN.site.latitude && NSN.site.longitude) {
			// get sunrise/sunset
			sunTimes = SunCalc.getTimes(date, NSN.site.latitude, NSN.site.longitude);
			// convert to site's local time
			sunTimes.sunrise = moment.tz(sunTimes.sunrise, NSN.site.time_zone).format();
			sunTimes.sunset = moment.tz(sunTimes.sunset, NSN.site.time_zone).format();
			// convert to fraction of day
			sunrise = (parseInt(sunTimes.sunrise.substr(11,2)) * 60
				     + parseInt(sunTimes.sunrise.substr(14,2))) / 1440;
			sunset = (parseInt(sunTimes.sunset.substr(11,2)) * 60
				     + parseInt(sunTimes.sunset.substr(14,2))) / 1440;
		}

		if(readonly){
			var overlay = $('<div class="sched-daylight"></div>');
			elem.css({left:(sunrise*100)+'%',right:(100-sunset*100)+'%'});
			elem.attr("data-sunrise", sunrise*100).attr("data-sunset", sunset*100);
			$('.sched-daylight').css({"border-left":"2px dotted rgba(51, 48, 48, 0.47)",
									  "border-right":"2px dotted rgba(51, 48, 48, 0.47)", "opacity":"1",
			 						  "z-index":"13", "background-color":"rgba(0, 0, 0, 0) !important"})
		}else{
			elem.css({left:(sunrise*100)+'%',right:(100-sunset*100)+'%'});
			elem.attr("data-sunrise", sunrise*100).attr("data-sunset", sunset*100);
		}
	};

	var setPDOverlay = function(elem, readOnly, pdProfileData, parentElement) {

		if(readOnly){
			if(pdProfileData){

				var sunTimes, start = 0.25 - 0.041666, end = 0.75 + 0.041666;
				var shift = 1200 / $(".sched-timeline").width();
				var maxLevel = pdProfileData.maxLevel;

				var left = timeToPct(pdProfileData.beginTime);
				var right = timeToPct(pdProfileData.endTime);

				if(right+left < 100){
					elem.css({left:(left)+'%',right:(0)+'%', height:maxLevel+'%', top: (100-maxLevel)+'%'});
					var elem2 = $('<div class="sched-pd"></div>');
					elem2.appendTo(parentElement);
					elem2.css({left:(0)+'%',right:(100-right)+'%', height:maxLevel+'%', top: (100-maxLevel)+'%'});
				}else{
					elem.css({left:(left)+'%',right:(100-right)+'%', height:maxLevel+'%', top: (100-maxLevel)+'%'});
				}

			}
		}
	}

	var setDHOverlay = function(elem,readonly, date, etDhProfileData, parentElement) {
		if(readonly){
			//***** New DH overlay Code
			if(etDhProfileData){
				if(etDhProfileData.scheduled){
					if(etDhProfileData.scheduled.length > 1) {
						var newArray = [];
						for (var a = 0; a < etDhProfileData.scheduled.length; a++) {
							var obj = {};
							obj.left = timeToPct(etDhProfileData.scheduled[a].beginTime);
							obj.right = timeToPct(etDhProfileData.scheduled[a].endTime);
							newArray.push(obj);
						}
						for (var k = 0; k < newArray.length; k++) {
							var element = $('<div class="sched-dh"></div>');
							element.css({left: (newArray[k].left) + '%', right: (100 - (newArray[k].right)) + '%'});
							element.appendTo(parentElement);
						}

					} else {
						console.log(" has only 1 scheduled  objects in the array--- needs 1 DH rectangles")
						var newArray = [];
						for (var a = 0; a < etDhProfileData.scheduled.length; a++) {
							var obj = {};
							obj.left = timeToPct(etDhProfileData.scheduled[a].beginTime);
							obj.right = timeToPct(etDhProfileData.scheduled[a].endTime);
							newArray.push(obj);
						}

						for (var k = 0; k < newArray.length; k++) {
							var element = $('<div class="sched-dh"></div>');
							element.css({left: (newArray[k].left) + '%', right: (100 - (newArray[k].right)) + '%'});
							element.appendTo(parentElement);
						}
					}
				}
			}
		}
		var sunTimes, start = 0.25 - 0.041666, end = 0.75 + 0.041666;
		if (NSN && NSN.site && NSN.site.latitude && NSN.site.longitude) {
			sunTimes = SunCalc.getTimes(date, NSN.site.latitude, NSN.site.longitude);
			// convert to site's local time
			sunTimes.sunrise = moment.tz(sunTimes.sunrise, NSN.site.time_zone).format();
			sunTimes.sunset = moment.tz(sunTimes.sunset, NSN.site.time_zone).format();
			// convert to fraction of day
			start = ((parseInt(sunTimes.sunrise.substr(11, 2)) - 1) * 60
				+ parseInt(sunTimes.sunrise.substr(14, 2))) / 1440;
			end = ((parseInt(sunTimes.sunset.substr(11, 2)) + 1) * 60
				+ parseInt(sunTimes.sunset.substr(14, 2))) / 1440;
		}
		elem.css({left: (start * 100) + '%', right: (100 - end * 100) + '%'});
	}

	var showActions = function(s, actions, ws, readonly) {
		var sunrise = s.find(".sched-daylight").attr("data-sunrise");
		var sunset = s.find(".sched-daylight").attr("data-sunset");
		var shift = 1200 / s.find(".sched-timeline").width();
		for (var i=0; i<actions.length; i++) {
			if (actions[i].time.match(/^\d\d:\d\d:\d\d/)) {
				var level = actions[i].level;
				var m = $("<div class='sched-marker sched-marker-time'><div class='sched-tooltip'></div></div>")
					.attr({"data-action":actions[i].time+","+actions[i].level,'tabindex':"0"});
				m.css({top:(250 - level * 2.5 - 12) + "px", left: (timeToPct(actions[i].time) - shift) + "%"});
				m.appendTo(s.find(".sched-timeline"));
			} else {
				if (actions[i].time.match(/^sun([rise|set])/)) {
					level = actions[i].level;
					m = $("<div class='sched-marker sched-marker-sun'><div class='sched-tooltip'></div></div>")
						.attr({"data-action":actions[i].time+","+actions[i].level,'tabindex':"0"});
					m.css({top:(250 - level * 2.5 - 12) + "px", left: (timeToPct(actions[i].time, sunrise, sunset) - shift) + "%"});
					m.appendTo(s.find(".sched-timeline"));
				}
			}
		}
		// add spares
		for (i=0; i<10; i++) {
			var m = $("<div class='sched-marker sched-marker-time sched-spare'><div class='sched-tooltip'></div></div>").attr('tabindex',"0");
			m.css({top:"-73px",left:"25px"});
			m.appendTo(s.find(".sched-timeline"));
			m = $("<div class='sched-marker sched-marker-sun sched-spare'><div class='sched-tooltip'></div></div>").attr('tabindex',"0");
			m.css({top:"-73px", left:"65px"});
			m.appendTo(s.find(".sched-timeline"));
		};

	};

	var showHeadings = function(){
		var daycount = {mon:0,tue:0,wed:0,thu:0,fri:0,sat:0,sun:0};
		var datecount = {};
		for (var i=0; i<ws; i++) {
			if (!$("#sched-ws-" + ws).hasClass("sched-deleted")) {
				var daydate = ($("#sched-ws-"+i).find("input[name='daydate"+i+"']:checked"));
				if (daydate.val() == "day") {
					$("#sched-ws-"+i+" input[name='days"+i+"']").each(function(){
						if ($(this).is(":checked")) {
							daycount[$(this).val()] += 1;
						}
					})
				} else {
					if (typeof datecount[$("#datetimepicker"+i).val()] == 'undefined') {
						datecount[$("#datetimepicker"+i).val()] = 1;
					} else {
						datecount[$("#datetimepicker"+i).val()] += 1;
					}
				}
			};
		};
		for (var i=0; i<ws; i++) {
			if (!$("#sched-ws-" + ws).hasClass("sched-deleted")) {
				var s = "", d = "";
				var daydate = ($("#sched-ws-"+i).find("input[name='daydate"+i+"']:checked"));
				if (daydate.val() == "day") {
					$("#sched-ws-"+i+" input[name='days"+i+"']").each(function(){
						if ($(this).is(":checked")) {
							if (s.length > 0) { s += ", "; } ;
							d = $(this).val();
							if (daycount[d] > 1) {
								s+= '<span style="color:red;display:inline-block" class="pulsenow duplicate">' + ucdays[lcdays.indexOf(d)] + '</span>';
							} else {
								s += ucdays[lcdays.indexOf(d)];
							};
						};
					});
				} else {
					d = $("#datetimepicker"+i).val();
					if (datecount[d] && datecount[d] > 1) {
					   s+= '<span style="color:red;display:inline-block" class="pulsenow duplicate">' + d + '</span>';
					} else {
					   s += d;
					};
				}
				if (s.length == 0) {
					s = '<i>(no days or date selected)</i>';
				};
	            $("#sched-ws-heading-" +i + " .sched-timeline-heading").html("Timeline " + (i+1) + " - "+ s);
			};
		};

	};

	var drawLines = function(ws, readonly){
		$("#sched-ws-" + ws + " .sched-line-v, #sched-ws-" + ws + " .sched-line-h").remove();
		var markers = [];
		$("#sched-ws-" + ws + " .sched-timeline .sched-marker").each(function() {
			if (parseFloat(this.style.top) >= -12) {
				markers.push({top:$(this).position().top + 12, left:(parseFloat(this.style.left) + 1.1)});
			};
		});
		markers = markers.sort(function(a, b) { return a.left - b.left });
		var prevTop = (markers.length > 0)?markers[markers.length-1].top:250, prevLeft = 0;
		if(readonly){
			// Draw only the rectangles
			for (var i=0; i<markers.length; i++) {
				var line = $("<div class='sched-line-h-view'><div>");
				var top = prevTop-3;
				var height = 250-top;
				line.css({top:top+"px",left:prevLeft+"%",width:(markers[i].left - prevLeft)+"%", height: height});
				line.appendTo($("#sched-ws-"+ws+" .sched-timeline"));
				prevTop = markers[i].top; prevLeft = markers[i].left;
			};
			if (markers.length > 0) {
				var line = $("<div class='sched-line-h-view'><div>");
				var top = prevTop-3;
				var height = 250-top;
				line.css({top:top,left:prevLeft+"%",width:(100 - prevLeft)+"%", height:height});
				line.appendTo($("#sched-ws-"+ws+" .sched-timeline"));
			}
		}else{
			for (var i=0; i<markers.length; i++) {
				var line = $("<div class='sched-line-h'><div>");
				line.css({top:prevTop-3+"px",left:prevLeft+"%",width:(markers[i].left - prevLeft)+"%"});
				line.appendTo($("#sched-ws-"+ws+" .sched-timeline"));
				line = $("<div class='sched-line-v'></div>");
				if (markers[i].top < prevTop) {
					line.css({top:markers[i].top,left:(markers[i].left-0.1)+"%",height:(prevTop - markers[i].top)});
				} else {
					line.css({top:prevTop,left:(markers[i].left-0.1)+"%",height:(markers[i].top - prevTop)});
				}
				line.appendTo($("#sched-ws-"+ws+" .sched-timeline"));
				prevTop = markers[i].top; prevLeft = markers[i].left;
			};
			if (markers.length > 0) {
				var line = $("<div class='sched-line-h'><div>");
				line.css({top:prevTop-3,left:prevLeft+"%",width:(100 - prevLeft)+"%"});
				line.appendTo($("#sched-ws-"+ws+" .sched-timeline"));
			}
		}

	}
	var deleteTimeline = function(target) {
		var heading = target.closest(".sched-ws-heading");
		var ws = parseInt(heading.attr("id").substring(17));
		heading.removeClass("ui-state-active");
		$("#sched-ws-" + ws + ", #sched-ws-heading-" + ws).addClass("sched-deleted");
		$("#sched-ws-" + ws).fadeOut('slow',function(){
			$(this).css({height:"0px"}).empty();
			$("#sched-ws-heading-" + ws + " .sched-timeline-heading").html("Timeline " + (1+ws) + " - <i>Deleted</i>");
		    showHeadings();
        });
	}

	var timeline = function(elem, event, ws, hasDH, hasPD, readonly, pdProfileData, etDhProfileData) {
		var day = typeof event.days != "undefined";
		var date = !day;

		// create the timeline
		var s = $('<div class="sched-timeline"></div>');
		s.appendTo(elem);

		// y-axis legend
		for (var i = 0; i<=100; i+=10) {
			$('<div class="sched-legend-light" style="bottom:' + (i*2.5-12) + 'px">' + i + '</div>').appendTo(s);
		}

		// x-axis legend
		var legendrow = "<tr>";
		for (i=0; i<25; i++) {
			legendrow += '<td align="center" width="4.166%">' + ((i%12==0)?12:i%12) + ((i<12||i==24)?'a':'p') + '</td>';
		}
		legendrow += '</tr>';
		var legend = $('<div class="sched-legend"><table style="width:104.1666%;cellborder:none">'
			+ legendrow
			+ '</table></div>');
		legend.appendTo(elem);

		var grid = '<table class="sched-grid"><tbody>';
		for (i=0;i<10;i++) {
			grid += '<tr>';
			for (var j=0;j<24;j++){
				grid += '<td> </td>';
			}
			grid += '</tr>';
		}
		grid += '</tbody></table>';
		$(grid).appendTo(s);

		// sunrise/sunset indicator
		var overlay = $('<div class="sched-daylight"></div>');
		overlay.appendTo(s);
		var daylight = setDaylightOverlay(overlay, day?(new Date()):new Date(event.date+"T00:00:00"), readonly);

		if (hasDH) {
			var dhoverlay = $('<div class="sched-dh"></div>');
			dhoverlay.appendTo(s);
			var daylight = setDHOverlay(dhoverlay, day?(new Date()):new Date(event.date+"T00:00:00"), readonly, etDhProfileData, s);
		}

		if(hasPD){
			var pdoverlay = $('<div class="sched-pd"></div>');
			pdoverlay.appendTo(s);
			var daylight = setPDOverlay(pdoverlay, readonly, pdProfileData, s);
		}
	};

	var today = function() {
	    var d = new Date(),
	        month = '' + (d.getMonth() + 1),
	        day = '' + d.getDate(),
	        year = d.getFullYear();

	    if (month.length < 2) month = '0' + month;
	    if (day.length < 2) day = '0' + day;

	    return [year, month, day].join('-');
	};

	var daydate = function(elem, event, ws, readonly) {
		var day = typeof event.days != "undefined";
		var date = !day;
		var checkboxes = "";
		var disabled = readonly?' disabled="disabled" ':'';
		for (var i=0; i<lcdays.length; i++){
			checkboxes += ' &nbsp; <input style="margin-right:4px" type="checkbox" ' + disabled + ' name="days' + ws + '" value="' + lcdays[i] + '" '
						+ ((day && event.days.indexOf(lcdays[i])>=0)?' checked="checked" ':'')
						+ ' /> ' + ucdays[i];
		}
		var s = $([
			'<div class="sched-daydate clearfix">',
				'<h4><input type="radio" class="mini-radio" ' + disabled + ' name="daydate' + ws + '" value="day" ' + (day?' checked="checked" ':'') +' /> <b>Days:</b> ',
					checkboxes,
				'</h4>',
				'<h4><input type="radio" class="mini-radio" ' + disabled + ' name="daydate' + ws + '" value="date" ' + (date?' checked="checked" ':'') + ' /> <b>Date:</b> ',
                    '<input type="text" ' + disabled + ' id="datetimepicker' + ws + '"',
                    ' value="' + (date?event.date:today()) + '" style="width:140px" />',
				'</h4>',
			'</div><div style="clear:both"></div>'].join(""));
		s.appendTo(elem);

		var p = $(['<div>On these Days or Date, use...'
			,		'<div style="margin-bottom:4px;margin-left:12px">'
			,     '<input type="radio" class="mini-radio" name="'
			,         ("photocell-" + ws)
			,         '" value="photocell" '
			, 				(event.photocell_enabled?' checked="checked"':'')
			,					' /> Photocell '
			,         '(<i>High:</i> <input type="text" class="form-control mini-text" id="'
			,         ("photocell-high-" + ws)
			,					'" style="width:40px;display:inline" value="' + event.photocell_highLevel + '" />'
			,     '  <i>Low:</i> <input type="text" class="form-control mini-text" id="'
			,					("photocell-low-" + ws)
			,					'" style="width:40px;display:inline" value="' + event.photocell_lowLevel + '" />)<br />'
			,     '<input type="radio" class="mini-radio" name="'
			,         ("photocell-" + ws)
			,         '" value="timeline" '
			, 				(event.photocell_enabled?'':' checked="checked"')			
			,					' /> this Timeline'
			,   '</div>'
			].join(""));
		p.css({top:"14px",left:"224px",position:"absolute",fontSize:"14px"});
		p.appendTo(elem);

		if (!readonly) {
			$('#datetimepicker' + ws).datetimepicker({format: 'YYYY-MM-DD'});
			$('#datetimepicker' + ws).on('change',function(){
				setDaylightOverlay($(this).closest(".sched-workspace").find(".sched-daylight"), new Date($(this).val()+"T00:00:00"), readonly);
			});
		};
	};

	var controls = function(elem, ws, readonly) {
		var c = $("<div class='sched-controls'>"
					+ "<div style='top:5px;left:100px' class='sched-trash pulse' title='Drag marker here to discard.'></div>"
					+ "<div style='top:24px;left:0px' class='sched-arrow sched-arrow-down'"
					+ (readonly?"":" title='Drag clock onto timeline for time-based change.'")
					+ "></div>"
					+ "<div style='top:24px;left:40px' class='sched-arrow sched-arrow-down'y"
					+ (readonly?"":" title='Drag sun onto timeline for sunrise/sunset relative change.'")
					+ "></div>"
					+ "<div style='top:24px;left:100px' class='sched-arrow sched-arrow-up'></div>"
				  + "</div>");
		c.appendTo(elem);
	};

	var calcAction = function(marker) {
		// calculate the action based on the location of the marker
		var timeline = marker.closest(".sched-timeline");
		var w = timeline.width();
		var shift = 1200 / w;
		var s = "When: ";
		var minutes = (marker[0].style.left.indexOf("px") >= 0)
			? (1440 * (parseFloat(marker[0].style.left) + shift) / w)
			: (Math.round((parseFloat(marker[0].style.left) + shift) * 14.4));
		if (marker.hasClass("sched-marker-time")) {
			var hours = Math.floor(minutes/60);
			minutes = minutes % 60;
			minutes = Math.round(minutes / 5) * 5;
			if (minutes == 60) { minutes = 0; hours++; };
			s += (hours<10?("0"+hours):hours) + ":" + (minutes<10?("0"+minutes):minutes) + ":00";
		} else {
			var sunrise = timeline.find(".sched-daylight").attr("data-sunrise") * 14.4;
			var sunset = timeline.find(".sched-daylight").attr("data-sunset") * 14.4;
			if (minutes < sunrise) {
				s += "Sunrise-" + Math.round((sunrise - minutes) / 5) * 5;
			} else {
				if (minutes > sunset) {
					s += "Sunset+" + Math.round((minutes - sunset) / 5) * 5;
				} else {
					var mid = (sunset + sunrise) / 2;
					if (minutes < mid) {
						s += "Sunrise+" + Math.round((minutes - sunrise) / 5) * 5;
					} else {
						s += "Sunset-" + Math.round((sunset - minutes) / 5) * 5;
					};
				};
			};
		};
		s = s.replace(/[\+\-]0/, "");
		return  s + "<br />Level: " + Math.round((250 - (marker.position().top + 12)) / 2.5) + "%";
	};

	var setHandlers = function(elem, ws, readonly) {
		elem.find(".sched-marker").on("mouseenter",function(){
			if ($(this).position().top >= -12) {
				$(this).find(".sched-tooltip").html(calcAction($(this))).show();
			}
		});
		elem.find(".sched-marker").on("mouseleave",function(){
			$(this).find(".sched-tooltip").hide();
		});

		if (readonly) {
			$(".sched-display").draggable();
		};

		if (!readonly) {

			//keyboard Events
			elem.find(".sched-marker").on("keydown",function(e){
			var pos = $(this).position();
			var w = elem.find(".sched-timeline").width();

			var moveTo = function(context,direction){
				var pct = 100 * (pos.left / w);
				if(direction == 'left' || direction == 'right'){
					/*
						Every hour on the time line is divided into 12 segments(each of 5 minutes 12 *5 = 60 minutes)
						whole timeline width totals to 12 * 24 hours = 288 segments
						The Width of timeLine is 100% , to make 288 segments. Each segment should be  0.3472222222222% (100/288)
						each Left/Right Arrow Key Stroke moves Icon to ± 0.3472222222222%
					*/
					var parsedVal = direction == 'left' ? Number(context.style.left.replace("%","")) - 0.3472 : Number(context.style.left.replace("%","")) + 0.3472
					context.style.left = parsedVal + "%"
				}else {
					/*
						The Height of the Timeline is 238px, to move  1% of Dimming takes icon to move 2.38px ( 238/100 )
						each top/bottom Arrow Key Stroke moves Icon to ± 2.38px
					*/
					var parsedVal = direction == 'top' ? Number(context.style.top.replace("px","")) - 2.38 : Number(context.style.top.replace("px","")) + 2.38
					context.style.top = parsedVal + "px"
				}

				var action = calcAction($(context));
				var tt = $(context).find(".sched-tooltip");
				tt.css({left:(pct>83)?"-130px":"24px"});
				$(context).css("width",(pct>83)?"10px":"154px");
				tt.html(action).show();
				$(context).attr("data-action", action.replace(/When: /,'').replace(/<br \/>Level: /,',').replace(/%/,"").toLowerCase());
				drawLines(ws);
			}

			if(pos.top >= -12 && pos.left >= -12 && pos.top <= 238 && pos.left <=  (w - 12)){
				switch(e.which) {
						case 37: // left
							moveTo(this,'left')
						break;
						case 38: // up
							moveTo(this,'top')
						break;
						case 39: // right
							moveTo(this,'right')
						break;
						case 40: // down
							moveTo(this,'down')
						break;
						default:
							//console.log("Non-arrow Key Strokes")
				}
			}
			});

			elem.find(".sched-timeline .sched-marker").draggable({
				drag: function(event, ui) {
					var pos = $(this).position();
					var w = elem.find(".sched-timeline").width();
					if (pos.top >= -12 && pos.left >= -12
						&& pos.top <= 238 && pos.left <=  (w - 12)) {
						var pct = 100 * (pos.left / w);
						this.style.left = (100 * (pos.left / w)) + "%";
						var action = calcAction($(this));
						var tt = $(this).find(".sched-tooltip");
						tt.css({left:(pct>83)?"-130px":"24px"});
						$(this).css("width",(pct>83)?"10px":"154px");
						tt.html(action).show();
						$(this).attr("data-action", action.replace(/When: /,'').replace(/<br \/>Level: /,',').replace(/%/,"").toLowerCase());
						drawLines(ws);
					} else {
						$(this).find(".sched-tooltip").html("").hide();
					}
				},
				start: function(event, ui) {
					$(this).data("zindex",$(this).css("zIndex"));
					$(this).css("zIndex", "9999");
				},
				stop: function(event, ui) {
					var pos = $(this).position();
					if (pos.top < -65 && pos.top > -95 && pos.left > 110 && pos.left < 135) {
						$(this).fadeOut(500, function() {$(this).remove();});
					}
					var w = elem.find(".sched-timeline").width();
					if (pos.left < -12) {
						$(this).css("left","-12px");						
					}
					if (pos.left > (w - 12)) {
						$(this).css("left", (w - 12) + "px");						
					}
					if (pos.top >= -12 && pos.left >= -12) {
						$(this).removeClass("sched-spare");
					} else {
						$(this).addClass("sched-spare");
					}
					if (pos.top > 238) {
						$(this).css("top","238px");
					}
					$(this).css("zIndex", $(this).data("zindex"));
					if (this.style.left.indexOf("px") >= 0) {
						this.style.left = (100 * (parseFloat(this.style.left) / $(this).closest(".sched-timeline").width())) + "%";
					};
					drawLines(ws);
				}
			});
			elem.find(".sched-timeline").droppable({accept:".sched-marker"});
			elem.find("input").on("change",function(){
				showHeadings();
			});
			elem.prev().find(".sched-timeline-delete").on("click", function(e){
				e.stopPropagation();
				e.preventDefault();
				deleteTimeline($(this));
			});
		};
	};

	var daylist = function(days) {
		return days.map(function (day, index) {
			return day.charAt(0).toUpperCase() + day.slice(1);
		});
	};

	var settingsPane = function(elem, schedule, groups, readonly) {

		var disabled = readonly?' disabled="disabled" ':'';

		var pane = $('<div class="sched-settings"></div>');

	var s = $([
			'<div class="row" style="margin:0px">'
		// name and description
			,(readonly?'<div class="col-sm-4">':'<div class="col-sm-5">')
			,   '<form class="form-horizontal">'
			,      '<div class="form-group" style="margin-top:16px">'
			,        '<input type="hidden" id="scheduleid" value="' + schedule.scheduleid + '" />'
			,        '<label for="schedname" class="control-label text-right col-sm-3">Name:</label>'
			,        '<div class="col-sm-9">'
			,           ' <input type="text" id="schedname" class="form-control" ' + disabled + ' value="' + schedule.name + '" />'
			,        '</div>'
			,      '</div>'
			,      '<div class="form-group">'
			,        '<label for="scheddescription" class="control-label text-right col-sm-3">Description:</label>'
			,        '<div class="col-sm-9">'
			,          ' <input type="text" id="scheddescription" class="form-control" ' + disabled + ' value="' + schedule.description + '" />'
			,        '</div>'
			,      '</div>'
			,   '</form>'
			,'</div>'
		// No-network options
			,(readonly?'<div class="col-sm-5">':'<div class="col-sm-7">')
			,   '<form class="form-horizontal">'
			,		'<p style="margin-bottom:4px;font-size:15px"><b>In case of Network Disconnect, use...</b></p>'
			,		'<div style="margin-bottom:4px;margin-left:12px">'
			,     '<p style="margin-bottom:0px">'
			,				'<input type="radio" class="mini-radio" name="photocell-network" value="photocell" '
			, 				(schedule.network.photocell_enabled?'checked="checked"':'')
			,					' /> '
			,     		'Photocell   (<i>High:</i> <input type="text" class="form-control mini-text" id="photocell-network-high" style="width:40px;display:inline" '
			,           'value="'+schedule.network.photocell_highLevel+'" />'
			,     		'  <i>Low:</i> <input type="text" class="form-control mini-text" id="photocell-network-low" style="width:40px;display:inline" '
			, 					'value="'+schedule.network.photocell_lowLevel+'" />)<br />'
			,     	'<input type="radio" class="mini-radio" name="photocell-network" value="dimming" '
			, 				(schedule.network.photocell_enabled?'':'checked="checked"')
			,					' /> '
			,   			'the following settings:</p>'
			,					'<div style="margin-left:28px;margin-top:0px">'
			,						'<i>High:</i> Time <input class="form-control mini-text" type="text" style="width:100px;display:inline" id="schedhitime" ' + disabled + ' value="'+schedule.network.highTime+'" />'
			,						' &nbsp; Level <input class="form-control mini-text" type="text" style="width:40px;display:inline" id="schedhilevel" ' + disabled + ' value="'+schedule.network.highLevel+'" />'
			,					'</div>'
			,					'<div style="margin-left:31px">'
			, 					'<i>Low:</i> Time <input class="form-control mini-text" type="text" style="width:100px;display:inline" id="schedlotime" ' + disabled + ' value="'
			,						((typeof schedule.network.lowTime!="undefined")?schedule.network.lowTime:"")
			,						'" />'
			,						' &nbsp; Level <input class="form-control mini-text" type="text" style="width:40px;display:inline" id="schedlolevel" ' + disabled + ' value="'
			,						((typeof schedule.network.lowLevel!="undefined")?schedule.network.lowLevel:"")
			,						'" />'
			,   			'</div>'
			,				'</div>'
			,			'</form>'
			,		'<div>'
			,	'</div>'
			,'</div>'
			,'</div>'].join(""));

		// Legend for read-only view
		var newText = $([
			 '<div class="col-sm-3">'
			,		'<ul class="schedule-colors-list">'
			,			'<li><div class="fixed-dimming"></div> <span>Schedule</span> </li>'
			,			'<li><div class="daylight-harvesting"></div><span>Daylight Harvesting </span> </li>'
			,			'<li><div class="proximity-dimming"></div> <span>Proximity Dimming </span></li>'
			,		'</ul>'
			,'</div>'
		].join(""));
		readonly? newText.appendTo(s): '';

		s.appendTo(pane);

		pane.appendTo(elem);

	};

	var workspace = function (elem, event, ws, readonly, hasDH, hasPD, pdProfileData, etDhProfileData) {
		var s = '<div class="sched-ws-heading" id="sched-ws-heading-'
					+ ws + '" style="font-size:18px;padding:4px 0px 4px 40px"><span class="sched-timeline-heading">Timeline '
					+ (ws+1) + ' - '
		if (typeof event.days != "undefined") {
			if (event.days.length==0) {
				s += '<i>(no days or date selected)</i>';
			} else {
				s += daylist(event.days).join(", ");
			}
		} else {
			s += event.date;
		};
		s += '</span>';
		if (!readonly) {
			s += '<div class="sched-timeline-delete">'
		        + '<span class="rubix-icon icon-fontello-cancel-circled-outline" style="color:#F00" title="Delete Timeline"></span>'
				+ '</div><div style="clear:both">';
		}
		s += '</div></div>';
		$(s).appendTo(elem);
		s = $('<div class="sched-workspace" id="sched-ws-' + ws + '"></div>');
		s.appendTo(elem);
		daydate(s, event, ws, readonly);
		timeline(s, event, ws, hasDH, hasPD, readonly, pdProfileData, etDhProfileData);
		controls(s, ws, readonly);
		showActions(s, event.actions, ws, readonly);
		drawLines(ws, readonly);
		setHandlers(s, ws, readonly);
	};

	var calcHeight = function(pct, extra) {
    	var h = ($(window).height() - 75) * (pct/100) + extra;
    	return h;
  	};

	return {
		// public methods

		init: function(elem, schedule, groups, readonly, hasDH, hasPD) {
			// generate the schedule editor
			//   the schedule will have multiple "workspaces"
			//   each workspace will have a timeline and controls to set days and date
			//   each timeline will contain resizeable "blocks" to specify action times and levels
			var scheduleform = $('<div class="sched-form"></div>');
			scheduleform.appendTo(elem);

			settingsPane(scheduleform, schedule, groups, readonly);
			// create a container
			var container = $('<div class="sched-container"></div>');

			container.appendTo(scheduleform);

			// create the workspaces
			ws = 0;  // workspace counter
			for (var i=0; i<schedule.events.length; i++) {
				if (typeof schedule.events[i].days != "undefined" || typeof schedule.events[i].date != "undefined") {
					workspace(container, schedule.events[i],  ws++, readonly, hasDH, hasPD,schedule.pdprofile, schedule.etDhProfile);
				}
			}

			container.accordion({collapsible:true});
			container.accordion({active: 0});

//		var shift = 1200 / $("#sched-ws-0 .sched-timeline").width();
//		$(".sched-timeline .sched-marker").not(".sched-spare").each(function() {
//			this.style.left = ((parseFloat(this.style.left) - shift)) + "%";
//		})

/*
			$(window).on("resize", function() {
				$(".sched-workspace").not(".sched-deleted").each(function(){
					var ws = parseInt($(this).attr("id").substring(9));
					showActions($(this), schedule.events[ws].actions, ws);
					drawLines(ws);
				});
			});
*/

		},

		handleAddbar: function(e) {
			e.stopPropagation();
			e.preventDefault();
			workspace($(".sched-container"),{
						photocell_enabled:false,
						photocell_highLevel:100,
						photocell_lowLevel:0,
						days:[],
	          actions:[]
	          }, ws++);
			$(".sched-container").accordion("refresh");
			$(".sched-container").accordion("option", {active: ws - 1});
		},

		validate: function(elem){
			var result = [];
			if ($("#schedname").val().trim().length == 0) {
				result.push("Schedule Name is required.");
			};
			if ($("#schedlotime").val().trim().length == 0 && $("#schedlolevel").val().trim().length > 0) {
				result.push("Cannot specify No-Network Low Level without No-Network Low Time.");
			};
			if ($("#schedlotime").val().trim().length > 0 && $("#schedlolevel").val().trim().length == 0) {
				result.push("Cannot specify No-Network Low Time without No-Network Low Level.");
			};
			if (!$("#schedhitime").val().match(/^(?:2[0-3]|[01][0-9]):[0-5][0-9]:[0-5][0-9]$/)) {
				result.push("No-Network High Time is not in the form HH:MM:SS.");
			};

			// validate No-Network Photocell values
			var photocell_network_good = true;
			var photocell_network_high = $("#photocell-network-high").val().trim();
			if (!(photocell_network_high.match(/^[0-9]?[0-9]$/) || photocell_network_high == "100")) {
				result.push("No-Network Photocell High Level is not 0-100.");
				photocell_network_good = false;
			};
			var photocell_network_low = $("#photocell-network-low").val().trim();
			if (!(photocell_network_low.match(/^[0-9]?[0-9]$/) || photocell_network_low == "100")) {
				result.push("No-Network Photocell Low Level is not 0-100.");
				photocell_network_good = false;
			};
			if (photocell_network_good) {
				if (parseInt(photocell_network_high) < parseInt(photocell_network_low)) {
				  result.push("No-Network Photocell Low Level is greater than High Level.");
				}
			}

			if (!($("#schedhilevel").val().match(/^[0-9]?[0-9]$/) || $("#schedhilevel").val() == "100")) {
				result.push("No-Network High Level is not 0-100.");
			};
			if ($("#schedlotime").val().trim().length > 0) {
				if (!$("#schedlotime").val().match(/^(?:2[0-3]|[01][0-9]):[0-5][0-9]:[0-5][0-9]$/)) {
					result.push("No-Network Low Time is not in the form HH:MM:SS.");
				};
			};
			if ($("#schedlolevel").val().trim().length > 0) {
				if (!($("#schedlolevel").val().match(/^[0-9]?[0-9]$/) || $("#schedlolevel").val() == "100")) {
					result.push("No-Network Low Level is not 0-100.");
				};
			};
			if ($(".sched-timeline").length == 0) {
				result.push("At least one timeline is required.");
			};
			if ($(".sched-ws-heading .duplicate").not(".sched-deleted").length > 0) {
				result.push("Two or more timelines exist for the same day or date.");
			};
			var days = lcdays.slice(0);

			$(".sched-workspace").not(".sched-deleted").each(function(timeLineNumber){

				var sun = { whichTimeLine:timeLineNumber, events:[] } // sample sun = { whichTimeLine:1, events: ['03:00:00','03:10:00'] }
				var time = { whichTimeLine:timeLineNumber, events:[] } // sample time = { whichTimeLine:1, events: ['03:00:00','03:10:00'] }
				var duplicateEvents = []

				$(this).find(".sched-marker-time").not(".sched-spare").each(function(){
					var EventTime = $(this).attr("data-action").split(',')[0]
					if(time.events.indexOf(EventTime) > -1){
                duplicateEvents.push(EventTime)
					}else{
                time.events.push(EventTime)
					}
				})

				$(this).find(".sched-marker-sun").not(".sched-spare").each(function(){
					var EventWhen = $(this).attr("data-action").split(',')[0]
					if(sun.events.indexOf(EventWhen) > -1){
                duplicateEvents.push(EventWhen)
					}else{
                sun.events.push(EventWhen)
					}
				})

				if(duplicateEvents.length){
					var duplicateTimeErrorMsg = "Timeline " + (timeLineNumber + 1) + " has multiple dimmings at " + JSON.stringify(duplicateEvents).slice(1,-1).replace (/"/g,'').replace (/,/g,', ')
					result.push(duplicateTimeErrorMsg)
				}

				if(sun.events.length + time.events.length > 6){
					result.push("Timeline " + (sun.whichTimeLine + 1) + " cannot have more than 6 dimming points")
				}

				var ws = parseInt($(this).attr("id").substring(9));
				if ($(this).find($(".sched-timeline .sched-marker").not(".sched-spare")).length == 0
					&& $('input[name="photocell-' + ws + '"]:checked').val() != "photocell") {
					result.push("Timeline " + (ws + 1) + " has no scheduled events.");
				};

				// validate Photocell values
				var photocell_good = true;
				var photocell_high = $(this).find("input[id='photocell-high-" + ws + "']").val().trim();
				if (!(photocell_high.match(/^[0-9]?[0-9]$/) || photocell_high == "100")) {
					result.push("Timeline " + (ws + 1) + " has Photocell High Level that is not 0-100.");
					photocell_good = false;
				};
				var photocell_low = $(this).find("input[id='photocell-low-" + ws + "']").val().trim();
				if (!(photocell_low.match(/^[0-9]?[0-9]$/) || photocell_low == "100")) {
					result.push("Timeline " + (ws + 1) + " has Photocell Low Level that is not 0-100.");
					photocell_good = false
				};
				if (photocell_good) {
					if (parseInt(photocell_high) < parseInt(photocell_low)) {
					  result.push("Timeline " + (ws + 1) + " has Photocell Low Level greater than High Level.");
					}
				};

				if (($(this).find("input[name='daydate" + ws + "']:checked").val() == "day")
				   && $(this).find("input[name='days" + ws + "']:checked").length == 0) {
					result.push("Timeline " + (ws + 1) + " is not used for any day or date.");
				};
				if ($(this).find("input[name='daydate" + ws + "']:checked").val() == "day") {
					$(this).find("input[name='days" + ws + "']:checked").each(function(){
					  var idx = days.indexOf($(this).val());
					  if (idx >= 0) {
					  	days.splice(idx,1);
					  }
					});
				}
				if ($(this).find("input[name='daydate" + ws + "']:checked").val() == "date") {
				   if (!$(this).find("input#datetimepicker" + ws).val().match(/\d{4}-\d\d-\d\d/)) {
						result.push("Date on Timeline " + (ws + 1) + " is not of the form yyyy-mm-dd.");
					} else {
						var ymd = $(this).find("input#datetimepicker" + ws).val().split("-");
						if (parseInt(ymd[1]) < 1 || parseInt(ymd[1]) > 12
							|| parseInt(ymd[2]) < 1 || parseInt(ymd[2]) > 31
							|| parseInt(ymd[0]) < 2000 || parseInt(ymd[0]) > 2100) {
							result.push("Date on Timeline " + (ws + 1) + " is invalid.");
						}
					}
				};
			});
			if (days.length > 0) {
				result.push(days.length + " day(s) of the week with no schedule defined.");
			};

			return result;
		},

		confirm: function(elem) {
			return window.confirm("Are you sure?");
		},

		encode: function(elem) {
			var schedule = {name:$("#schedname").val(),
							description:$("#scheddescription").val(),
							scheduleid:$("#scheduleid").val(),
							network: {
								highTime:$("#schedhitime").val().trim(),
								highLevel:parseInt($("#schedhilevel").val().trim()),
								lowTime:$("#schedlotime").val().trim(),
								lowLevel:parseInt($("#schedlolevel").val().trim()),
								photocell_enabled:$('input[name="photocell-network"]:checked').val() == "photocell",
								photocell_highLevel:parseInt($("#photocell-network-high").val()),
								photocell_lowLevel:parseInt($("#photocell-network-low").val())
							},
							events:[]
						};
			if (schedule.network.lowTime === "" || schedule.network.lowLevel === "") {
				delete schedule.network.lowTime;
				delete schedule.network.lowLevel;
			} else {
				schedule.network.lowLevel = parseInt(schedule.network.lowLevel);
			};
			$(".sched-workspace").not(".sched-deleted").each(function(){
				var ws = parseInt($(this).attr("id").substring(9));
				var ws_event = {};
				if ($("input[name='daydate" + ws + "']:checked").val() == "day") {
					ws_event.days = [];
					$("input[name='days" + ws + "']:checked").each(function(){
						ws_event.days.push($(this).val());
					});
	//				ws_event.days = ws_event.days.split(",");
				} else {
					ws_event.date = $("#datetimepicker" + ws).val();
				}

				ws_event.photocell_enabled = $('input[name="photocell-' + ws + '"]:checked').val() == "photocell",
				ws_event.photocell_highLevel = parseInt($("#photocell-high-" + ws).val());
				ws_event.photocell_lowLevel = parseInt($("#photocell-low-" + ws).val());
				ws_event.actions = [];

				$(this).find(".sched-timeline .sched-marker").not(".sched-spare").each(function(){
					if ($(this).position().top >= -12) {  // do not include unplaced markers
						var timeLevel = $(this).attr("data-action").split(",");
						ws_event.actions.push({time:timeLevel[0], level:parseInt(timeLevel[1])});
					}
				})
				schedule.events.push(ws_event);
			});

			return schedule;

		},

		destroy: function(elem) {
			// remove any handlers
			$(".bootstrap-datetimepicker-widget").remove();
			elem.empty();
		}
	}
})();

module.exports = scheduler;
