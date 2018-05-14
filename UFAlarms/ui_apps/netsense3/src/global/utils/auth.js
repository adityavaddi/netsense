var auth = {
	allowed: function(action, model){
		var wildcard = action.indexOf('*') === (action.length - 1);
		if (wildcard) {
			action = action.slice(0, -1);
		}
		if (NSN && NSN.userInfo && NSN.userInfo.authorization) {
		   if (model == "n/a") {
			   return true;
		   }
		   // check authorization
		   var model_matches = NSN.userInfo.authorization.filter(function(m){return m.model == model});
		   // filter returns array of matches
		   if (model_matches.length == 0) {
		      return false;
		   };

		   // now check permissions for this model
		   if (wildcard) {
			   for (var i=0, found=false; !found && i<model_matches[0].allowed.length; i++) {
			   	 found = (model_matches[0].allowed[i].indexOf(action) == 0);
			   }
		   } else {
			   for (i=0, found=false; !found && i<model_matches[0].allowed.length; i++) {
			   	 found = (model_matches[0].allowed[i] == action);
			   }
			}
		   return found;
		} else {
		   return false;
		}
	}
};

module.exports = auth;