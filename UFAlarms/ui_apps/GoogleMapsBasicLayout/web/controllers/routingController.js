
/************************************** Routing Controller **************************************/

defaultApp.controller('itemsFocusController', function($scope, $location) {
    $scope.isActive = function(route) {
        return route === $location.path();
    }
});

defaultApp.config(function($routeProvider) {
    $routeProvider.

    /******************************* Menu 1 *******************************/

      when('/', {
		templateUrl: 'templates/layout.html',
		label:'Menu 1',
		controller: 'menuOneContent'
      }).

    /******************************* End Menu 1 *******************************/


    /******************************* Menu 2 *******************************/


      when('/menuTwo', {
		templateUrl: 'templates/layout.html',
		label:'Menu 2',
		controller: 'menuTwoContent'
      }).

    /******************************* End Menu 2 *******************************/


    /******************************* Menu 3 *******************************/

      when('/menuThree', {
		templateUrl: 'templates/layout.html',
		label:'Menu 3',
		controller: 'menuThreeContent'
      }).

    /******************************* End Menu 3 *******************************/


    /******************************* Menu 4 *******************************/

      when('/menuFour', {
		templateUrl: 'templates/layout.html',
		label:'Menu 4',
		controller: 'menuFourContent'
      }).

      /******************************* End Menu 4 *******************************/


      /******************************* Menu 5 *******************************/

      when('/menuFive', {
		templateUrl: 'templates/layout.html',
		label:'Menu 5',
		controller: 'menuFiveContent'
      }).

      /******************************* End Menu 5 *******************************/


      /******************************* Menu 6 *******************************/

      when('/menuSix', {
		templateUrl: 'templates/layout.html',
		label:'Menu 6',
		controller: 'menuSixContent'
      }).

    /******************************* End Menu 6 *******************************/


    /*******************************  Menu 7 *******************************/

      when('/menuSeven', {
		templateUrl: 'templates/layout.html',
		label:'Menu 7',
		controller: 'menuSevenContent'
      }).


    /******************************* End Menu 7 *******************************/

      otherwise({
		redirectTo: 'templates/layout.html'
      });
});


/******************************* Menu 1 Controller *******************************/

defaultApp.controller('menuOneContent', function($scope) {
	
    $scope.templatetab =  {name: 'templatetab.html', url: 'templates/menu1.html'};

});

/******************************* End Menu 1 Controller *******************************/


/******************************* Menu 2 Controller *******************************/


defaultApp.controller('menuTwoContent', function($scope) {
	
	$scope.templatetab =  {name: 'templatetab.html', url: 'templates/menu2.html'};
	
});


/******************************* End Menu 2 Controller *******************************/


/******************************* Menu 3 Controller *******************************/

defaultApp.controller('menuThreeContent', function($scope) {
	
	$scope.templatetab =  {name: 'templatetab.html', url: 'templates/menu3.html'};
	
});


/******************************* End Menu 3 Controller *******************************/


/******************************* Menu 4 Controller *******************************/


defaultApp.controller('menuFourContent', function($scope) {
	
	$scope.templatetab =  {name: 'templatetab.html', url: 'templates/menu4.html'};

});

/******************************* End Menu 4 Controller *******************************/


/******************************* Menu 5 Controller  *******************************/

defaultApp.controller('menuFiveContent', function($scope) {
	
	$scope.templatetab =  {name: 'templatetab.html', url: 'templates/menu5.html'};
	
});

/******************************* End Menu 5 Controller *******************************/


/******************************* Menu 6 Controller *******************************/

defaultApp.controller('menuSixContent', function($scope) {
	
	$scope.templatetab =  {name: 'templatetab.html', url: 'templates/menu6.html'};
});

/******************************* End Menu 6 Controller *******************************/


/******************************* Menu 7 Controller *******************************/

defaultApp.controller('menuSevenContent', function($scope) {
	
	$scope.templatetab =  {name: 'templatetab.html', url: 'templates/menu7.html'};

});

/******************************* End Menu 7 Controller  *******************************/


/************************************** End Routing Controller **************************************/


